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
    mutate(Weight_despiked = Reduce(c, tapply(Weight, ScaleID, clean_spikes))) %>% # call clean_spikes function (see below) to create Weight_despiked field
    filter(TimeStamp > ftrim & TimeStamp < btrim) %>% # trim data to desired start and end dates
    select(TimeStamp, Unix_Time, ScaleID, Site, Hive, Weight, Weight_despiked) %>%
    mutate(Site = as.character(Site)) # convert Site field to character so that empty factor levels do not persist after filtering
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

#' \code{clean_spikes} removes strong additive outliers, i.e. "spikes", using the \code{despike} function from package \code{oce}. Parameters are set so that strong spike values are replaced with a running median of width = 7
#'
#' @param x A vector of time-ordered observations
#' @return An adjusted vector of observations in which spieks have been replaced with running median
#' @export
clean_spikes <- function(x) {
  oce::despike(x, reference = "median", n = 1.5, k = 7, replace = "reference")
}

#' \code{data_proc} generates and appends four new fields: a 25 h running mean of the despiekd weights,
#'   a loess smoothing of the 25-h running mean,
#'   a first-order difference vector of the loess-smoothed 25-h running mean,
#'   and a z-normalization of the difference vector.
#'
#' @param x A tibble or data frame
#' @return A tibble with added fields
#' @export
data_proc <- function(x) {
  out <- x %>%
    group_by(ScaleID) %>%
    mutate(Weight_25h_mean = mean25h(Weight_adjusted)) %>%
    mutate(Weight_loess = c(rep(NA, 12), smooth_loess(Weight_25h_mean), rep(NA, 12))) %>%
    mutate(Weight_loess_diff = c(NA, diff(Weight_loess))) %>%
    mutate(Weight_loess_diff_z = scale(Weight_loess_diff))
  return(out)
}

#' \code{smooth_loess} applies loess smoothing to a time series
#'
#' @param x A vector of time-ordered observations
#' @param span A real number indicating the width (proportion of time series observations) of the loess smoothing window
#' @return A loess-smoothed vector of time-ordered observations
#' @export
smooth_loess <- function(x, span = 0.1) {
  mod <- loess(x ~ zoo::index(x), span = span)
  out <- predict(mod)
  return(out)
}

#' Remove daily periodicity by honey bee weight data by taking a 25-h running mean
#'
#' @param x A vector of time-ordered observations
#' @return A vector of 25-h means with NA pad for first 12 and last 12 observations
#' @export
mean25h <- function(x) {
  rollmean(x, 25, na.pad = T)
}

#' Use ARIMA modeling to identify and correct additive and level-shift outliers; one hive at a time
#'
#' @param x A data frame or tibble
#' @param ID A string corresponding to the ScaleID field, specifying which hive is to be processed
#' @param cval A real number, parameter of the tso function modulating sensitivity
#' @param method A string specifying the modeling method to be used to identify outliers
#' @return A tibble containing outlier-corrected vector of observations
#' @export
fix_outliers <- function(x, ID, cval = 9, method = "arima") {
  set.seed(1404)
  x_ID <-filter(x, ScaleID == ID)
  x_ts <- as.ts(x_ID$Weight_despiked)
  x_tso <- tsoutliers::tso(x_ts, types = c("AO","LS"), cval = cval, tsmethod = method)
  x_ID$Weight_adjusted <- as.numeric(x_tso$yadj)
  plot(x_tso)
  return(x_ID)
}

#' Use ARIMA modeling to identify and correct additive and level-shift outliers; batch process multiple hives
#'
#' @param x A data frame or tibble
#' @param cval A real number, parameter of the tso function modulating sensitivity
#' @param method A string specifying the modeling method to be used to identify outliers
#' @return A tibble containing outlier-corrected vector of observations
#' @export
fix_outliers_batch <- function(x, cval = 9, method = "arima") {
  set.seed(1404)
  x_ts <- as.ts(x$Weight_despiked)
  x_tso <- tsoutliers::tso(x_ts, types = c("AO","LS"), cval = cval, tsmethod = method)
  x$Weight_adjusted <- as.numeric(x_tso$yadj)
  plot(x_tso)
  return(x)
}
