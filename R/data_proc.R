#' \code{load_bm} loads a BroodMinder SQLite database and extracts logged readings into a tibble.
#'
#' @param FilePath Path to BroodMinder SQLite database
#' @param tz String identifying time zone
#' @param ftrim String "YYYY-mm-dd" indicating front date cutoff
#' @param btrim String "YYYY-mm-dd" indicating back date cutoff
#' @param sep A string denoting the separator between the site and hive number in the HiveName field; assumes HiveName follows format site % sep % hive#
#' @return A tibble containing selected fields and trimmed to front and back cutoff dates
#' @export
load_bm <- function(FilePath, tz, ftrim, btrim, sep = "\\.") {
  data_sqlite <- RSQLite::dbConnect(RSQLite::SQLite(), FilePath) # opens db connection
  dat <- RSQLite::dbGetQuery(data_sqlite, "SELECT * FROM BroodMinderLogReading") # extracts BroodMinderLogReading sheet
  RSQLite::dbDisconnect(data_sqlite) # closes connection
  hnames <- get_names(FilePath, sep) # call get_names function (see below) to prepare site and hive# fields to be appended to data tibble
  out <- full_join(dat, hnames, by = "UUID") %>%
    select(Unix_Time, Site, Hive, ScaleID, RecordType, Weight) %>% # "Weight" is the raw, unscaled sum of left and right load cells
    as.tibble() %>%
    filter(RecordType == "Logged_Data") %>% # use only logged data readings
    mutate(TimeStamp = anytime::anytime(Unix_Time, tz = tz)) %>% # convert Unix_Time to human-readable time stamp field
    arrange(ScaleID, TimeStamp) %>% # arrange chronologically within each device
    filter(TimeStamp > ftrim & TimeStamp < btrim) %>% # trim data to desired start and end dates
    select(TimeStamp, Unix_Time, ScaleID, Site, Hive, Weight) %>%
    mutate(Site = as.character(Site)) %>% # convert Site field to character so that empty factor levels do not persist after filtering
    filter(Site != "kate")
  return(out)
}

#' \code{get_names} splits HiveName field into Site and Hive#, assuming HiveName follows format site % sep % hive#
#'
#' @param FilePath Path to BroodMinder SQLite database
#' @param sep A string denoting the separator between the site and hive number in the HiveName field; assumes HiveName follows format site % sep % hive#
#' @return A tibble consisting of fields UUID, ScaleID, Site, Hive, filtered to contain only unique rows
#' @export
get_names <- function(FilePath, sep) {
  data_sqlite <- RSQLite::dbConnect(RSQLite::SQLite(), FilePath) # open db connection
  readings <- RSQLite::dbGetQuery(data_sqlite, "SELECT * FROM BroodMinderReading") # extract BroodMinderReading sheet
  SiteNum <- data.frame(str_split(readings$HiveName, pattern = sep, simplify = TRUE)) # split HiveName field
  RSQLite::dbDisconnect(data_sqlite) # close db connection
  names <- bind_cols(select(readings, Address, HiveName), SiteNum) %>% # append split HiveName field to Address and HiveName fields
    distinct() # deduplicate
  colnames(names) <- c("UUID", "ScaleID", "Site", "Hive") # rename columns
  return(names)
}

#' Remove daily periodicity by honey bee weight data by taking a 25-h running mean
#'
#' @param x A vector of time-ordered observations
#' @return A vector of 25-h means with NA pad for first 12 and last 12 observations
#' @export
mean25h <- function(x) {
  rollmean(x, 25, na.pad = T)
}

#' \code{deartifacr} corrects artifacts by subtacting from each observation the cumulative sum of observations > delta_max.
#' Empirically, histograms of my differenced show a consistent discontinuity around 2.5, suggesting that this is an appropriate value for delta_max.
#'
#' @param x A vector of differenced time-ordered observations
#' @param delta_max A real number defining the weight chenge threshold above which a weight change is considered an artifact.
#' @return A vector of de-artifacted observations
#' @export
# deartifact <- function(x, delta_max = 4) {
#   dw <- c(0, diff(x))
#   dm <- dw * (abs(dw) > delta_max)
#   return(x - cumsum(dm))
# }
deartifact <- function(x, delta_max = 2.5) {
  for(i in seq_along(x)) {
    if(abs(x[i]) > delta_max) {
      x[i] <- 0
    }
  }
  return(x)
}



#' \code{data_proc} adds Weight_clean, Weight_decycled, and Weight_decycled_diff fields; also gathers into tidy array
#'
#'
data_proc <- function(x) {
  x %>%
    select(-Unix_Time) %>%
    group_by(ScaleID) %>%
    mutate(wt_diff = c(0, diff(Weight)),
           wt_diff_clean = deartifact(wt_diff),
           wt_diff_clean_25h = mean25h(wt_diff_clean)) %>%
    na.omit() %>%
    mutate(wt_recon = cumsum(wt_diff_clean_25h),
           TimeStamp_round = lubridate::round_date(TimeStamp, unit = "hour"),
           delta_sign = if_else(wt_diff_clean_25h >= 0, "gain", "loss")) %>%
    select(TimeStamp_round, ScaleID, Site, Hive, Weight, wt_diff, wt_diff_clean, wt_diff_clean_25h, wt_recon, delta_sign) %>%
    gather(weight_var, value, -TimeStamp_round, -ScaleID, -Site, -Hive, -delta_sign)
}


#' \code{plot_wt} is a wrapper for ggplot that allows the plotting of weight metrics by hive, by site, or pooled
#'
#' @param x a tibble output by data_proc
#' @param by a string specifying whether plot should be faceted by "site", "scale", or "none"
#' @param metric a string vector indicating which weight metrics should be plotted
#' @return a plot
plot_wt <- function(x, by = "site", metric) {
  x <- filter(x, weight_var %in% metric)

  if(by == "hive") {
    p <- ggplot(x, aes(x$TimeStamp_round, x$value)) +
      geom_point() +
      facet_wrap(~ ScaleID)
  }

  if(by == "site") {
    p <- ggplot(x, aes(TimeStamp_round, value, color = Hive)) +
      geom_point() +
      facet_wrap(~ Site)
  }

  if(by == "none") {
    p <- ggplot(x, aes(x$TimeStamp_round, x$value)) +
      geom_point(alpha = 0.025) +
      stat_smooth()
  }

  print(p)
}


plot_delta <- function(x, type = "points", by = "none", metric) {
  x <- filter(x, weight_var %in% metric)

  if(by == "hive") {
    if(type == "points") {
      p <- ggplot(x, aes(TimeStamp_round, value)) +
        geom_point(alpha = 0.1, aes(color = delta_sign)) +
        stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 25), geom = "area", alpha = 0.75) +
        geom_hline(yintercept = 0, color = "yellow") +
        coord_cartesian(ylim = c(-0.15, 0.15)) +
        facet_wrap(~ScaleID)
    }
    if(type == "hist") {
      p <- ggplot(x, aes(value)) +
        geom_histogram(binwidth = 0.05) +
        xlim(-5, 5) +
        facet_wrap(~ScaleID)
    }
  }

    if(by == "site") {
      if(type == "points") {
        p <- ggplot(x, aes(TimeStamp_round, value)) +
          geom_point(alpha = 0.1, aes(color = delta_sign)) +
          stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 25), geom = "area", alpha = 0.75) +
          geom_hline(yintercept = 0, color = "yellow") +
          coord_cartesian(ylim = c(-0.15, 0.15)) +
          facet_wrap(~Site)
      }
      if(type == "hist") {
        p <- ggplot(x, aes(value)) +
          geom_histogram(binwidth = 0.05) +
          xlim(-5, 5) +
          facet_wrap(~Site)
      }
    }

      if(by == "none") {
        if(type == "points") {
          p <- ggplot(x, aes(TimeStamp_round, value)) +
            geom_point(alpha = 0.1, aes(color = delta_sign)) +
            stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 25), geom = "area", alpha = 0.75) +
            geom_hline(yintercept = 0, color = "yellow") +
            coord_cartesian(ylim = c(-0.15, 0.15))
        }
        if(type == "hist") {
          p <- ggplot(x, aes(value)) +
            geom_histogram(binwidth = 0.05) +
            xlim(-5, 5)
        }
  }

  print(p)
}
