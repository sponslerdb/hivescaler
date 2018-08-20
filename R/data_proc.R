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
    select(Unix_Time, Site, Hive, ScaleID, RecordType, Weight) %>%
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
#' @param x A vector of time-ordered observations
#' @param delta_max A real number defining the weight chenge threshold above which a weight change is considered an artifact.
#' @return A vector of de-artifacted observations
#' @export
deartifact <- function(x, delta_max = 2.5) {
  dw <- c(0, diff(x))
  dm <- dw * (abs(dw) > delta_max)
  return(x - cumsum(dm))
}

#' \code{data_proc} adds Weight_clean, Weight_decycled, and Weight_decycled_diff fields; also gathers into tidy array
data_proc <- function(x) {
  x %>%
    select(-Unix_Time) %>%
    group_by(ScaleID) %>%
    mutate(Weight_clean = deartifact(Weight),
           Weight_decycled = mean25h(Weight_clean),
           Weight_decycled_diff = c(0, diff(Weight_decycled))) %>%
    gather(weight_var, value, -TimeStamp, -ScaleID, -Site, -Hive)
}

plot_wt <- function(x, weight_vars = c("Weight", "Weight_clean", "Weight_decycled"), omit = NULL) {
  x <- filter(x, weight_var %in% c(weight_vars) & !ScaleID %in% omit)
  ggplot(x, aes(x = TimeStamp, y = value, color = weight_var)) +
    geom_line() +
    facet_wrap(~ ScaleID) +
    coord_cartesian(ylim = c(0, 120))
}
