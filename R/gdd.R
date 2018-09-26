#' \code{gdd_calc} calculates growing degree days using the formula ((tmax + tmin) / 2) - base, with negative results set to 0
#'
#' @param tmax maximum temperature (C)
#' @param tmin minimum temperature (C)
#' @param base base temperature (C)
#' @return a number
gdd_calc <- function(tmin, tmax, base = 10) {
  gdd <- (tmax + tmin)/2 - base
  ifelse(gdd > 0, gdd, 0)
}



#' \code{load_noaa}
#'
#' @param stationid
#' @param startdate
#' @param enddate
#' @param datasetid
#' @param limit
#' @param datatypeid
#' @param token
#' @return
load_noaa <- function(stationid, startdate, enddate,
                      datasetid, limit = 1000, datatypeid, token) {
  data <- ncdc(stationid = stationid,
               startdate = startdate,
               enddate = enddate,
               datasetid = datasetid,
               limit = limit,
               datatypeid = datatypeid,
               token = token)

  data$data %>%
    as.tibble() %>%
    select(date, datatype, value) %>%
    spread(datatype, value) %>%
    mutate(TMAX = TMAX/10,
           TMIN = TMIN/10) %>%
    mutate(gdd = gdd_calc(TMIN, TMAX),
           gdd_cum = cumsum(gdd),
           precip_bin = ifelse(PRCP > 0, TRUE, FALSE),
           date = as_datetime(date))
}


#' \code{wt_noaa_join} joins weight data to weather data by TimeStamp_round, interpolates hourly GDD, and forward-fills precip_bin
#'
#' @param wt tibble output of \code{data_proc}
#' @param noaa tibble output of \code{load_noaa}
#' @return a tibble
wt_noaa_join <- function(wt, noaa) {
  left_join(wt, noaa, by = c("TimeStamp_round" = "date"))  %>%
  mutate(gdd_int = as.integer(na.interpolation(gdd_cum))) %>%
  fill(precip_bin) %>%
  select(-PRCP, -TMAX, -TMIN, -gdd, -gdd_cum) %>%
  na.omit()
}
