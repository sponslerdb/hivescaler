#' \code{data_load_anon} loads a BroodMinder SQLite database and extracts logged readings into a tibble; for dbs where scales are not named
#'
#' @param FilePath Path to BroodMinder SQLite database
#' @param tz String identifying time zone
#' @param ftrim String "YYYY-mm-dd" indicating front date cutoff
#' @param btrim String "YYYY-mm-dd" indicating back date cutoff
#' @return A tibble containing selected fields and trimmed to front and back cutoff dates
#' @export
data_load_anon <- function(FilePath, tz) {
  data_sqlite <- RSQLite::dbConnect(RSQLite::SQLite(), FilePath) # opens db connection
  dat <- RSQLite::dbGetQuery(data_sqlite, "SELECT * FROM BroodMinderLogReading") # extracts BroodMinderLogReading sheet
  RSQLite::dbDisconnect(data_sqlite) # closes connection
  out <- dat %>%
    select(Unix_Time, ScaleID = UUID, RecordType, Weight) %>% # "Weight" is the raw, unscaled sum of left and right load cells
    as.tibble() %>%
    filter(RecordType == "Logged_Data") %>% # use only logged data readings
    mutate(TimeStamp = anytime::anytime(Unix_Time, tz = tz)) %>% # convert Unix_Time to human-readable time stamp field
    arrange(ScaleID, TimeStamp) %>% # arrange chronologically within each device
    select(TimeStamp, Unix_Time, ScaleID, Weight)
  return(out)
}


#' \code{data_proc_anon} creates wt_diff (differenced weight), wt_diff_clean (deartifacted differenced weight), wt_diff_clean_25h (25-h rolling mean), wt_recon (absolute weight reconstructed by taking the cumulative sum of the deartifacted differenced weight, normalized to zero starting weight), delta_sign (indicating whether each weight change was positive or negative), and TimeStamp_round (TimeStamp field rounded to nearest hour to enable time series alignment)
#'
#' @param x a tibble created by data_load
#' @return a tibble with new fields, gathered into a tudy array
#' @export
data_proc_anon <- function(x) {
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
    select(TimeStamp_round, ScaleID, Weight, wt_diff, wt_diff_clean, wt_diff_clean_25h, wt_recon, delta_sign) %>%
    gather(weight_var, value, -TimeStamp_round, -ScaleID, -delta_sign)
}

#' \code{plot_wt_anon} is a wrapper for ggplot that allows the plotting of weight metrics by hive, by site, or pooled
#'
#' @param x a tibble output by data_proc
#' @param by a string specifying whether plot should be faceted by "site", "scale", or "none"
#' @param metric a string vector indicating which weight metrics should be plotted
#' @param omit a string indicating which ScaleIDs shoud be dropped prior to plotting
#' @param knots a real number controlling the number of knots in the GAM splines smoothing function; lower number = more smoothing
#' @return a plot
plot_wt_anon <- function(x, by = "ScaleID", metric, knots = 25, omit = NULL) {
  x <- filter(x, weight_var %in% metric & !ScaleID %in% omit)

  if(by == "ScaleID") {
    p <- ggplot(x, aes(x$TimeStamp_round, x$value)) +
      geom_point() +
      xlab("Time") +
      ylab("Weight (kg, zero-based)") +
      facet_wrap(~ ScaleID) +
      theme_gray(18)
  }

  if(by == "none") {
    p <- ggplot(x, aes(x$TimeStamp_round, x$value)) +
      geom_point(alpha = 0.05) +
      xlab("Time") +
      ylab("Weight (kg, zero-based)") +
      stat_smooth(method = "gam",
                  formula = y ~ s(x, bs = "cs", k = knots),
                  aes(color = "GAM fit"),
                  se = FALSE,
                  size = 2) +
      scale_color_manual(name='GAM fit', values=c("blue")) +
      theme_gray(18) +
      theme(legend.title=element_blank())
  }

  print(p)
}


#' \code{plot_delta_anon} is a wrapper for ggplot that allows the plotting of differenced weight metrics by hive, by site, or pooled
#'
#' @param x a tibble output by data_proc
#' @param type a string specifying the type of plot; options are "points" for scatterplot or "hist" for histogram; note that for histograms, xlim is set to c(-5, 5), which results in the trimming of extreme values but improves visualization of data. Similarly, for scatterplots, ylim is set to (-0.15, 0.15), which trims extreme values but improves visualization of the smooth fit line
#' @param by a string specifying whether plot should be faceted by "site", "scale", or "none"
#' @param metric a string vector indicating which weight metrics should be plotted
#' @param omit a string indicating which ScaleIDs shoud be dropped prior to plotting
#' @param knots a real number controlling the number of knots in the GAM splines smoothing function; lower number = more smoothing
#' @return a plot
plot_delta_anon <- function(x, type = "points", by = "none", metric, knots = 25, omit = NULL) {
  x <- filter(x, weight_var %in% metric & !ScaleID %in% omit)

  if(by == "ScaleID") {
    if(type == "points") {
      p <- ggplot(x, aes(TimeStamp_round, value)) +
        geom_point(alpha = 0.1, aes(color = delta_sign)) +
        stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = knots), geom = "area", alpha = 0.8) +
        coord_cartesian(ylim = c(-0.2, 0.2)) +
        xlab("Time") +
        ylab("Δ Weight (kg)") +
        facet_wrap(~ScaleID) +
        theme_gray() +
        theme(legend.title=element_blank())
    }
    if(type == "hist") {
      p <- ggplot(x, aes(value)) +
        geom_histogram(binwidth = 0.05) +
        xlim(-5, 5) +
        facet_wrap(~ScaleID) +
        theme_gray(18)
    }
  }

  if(by == "none") {
    if(type == "points") {
      p <- ggplot(x, aes(TimeStamp_round, value)) +
        geom_point(alpha = 0.25, aes(color = delta_sign)) +
        stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = knots), geom = "area", alpha = 0.8) +
        geom_hline(yintercept = 0, color = "black", size = 0.75) +
        xlab("Time") +
        ylab("Δ Weight (kg)") +
        coord_cartesian(ylim = c(-0.2, 0.2)) +
        theme_gray(18) +
        theme(legend.title=element_blank())
    }
    if(type == "hist") {
      p <- ggplot(x, aes(value)) +
        geom_histogram(binwidth = 0.05) +
        xlim(-5, 5) +
        theme_gray(18)
    }
  }

  print(p)
}
