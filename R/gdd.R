library(rnoaa)

token <- "pXINZDyCrPudpyRYrlHUomgipePyXrlX"

gdd_calc <- function(tmin, tmax, base = 10) {
  gdd <- (tmax + tmin)/2 - base
  ifelse(gdd > 0, gdd, 0)
}

PHL_INT <- ncdc(stationid = "GHCND:USW00013739",
                startdate = "2018-01-01",
                enddate = "2018-09-22",
                datasetid = "GHCND",
                limit = 1000,
                datatypeid = c("PRCP", "TMAX", "TMIN"),
                token = token)

PHL_INT_proc <- PHL_INT$data %>%
  as.tibble() %>%
  select(date, datatype, value) %>%
  spread(datatype, value) %>%
  mutate(TMAX = TMAX/10,
         TMIN = TMIN/10) %>%
  mutate(gdd = gdd_calc(TMIN, TMAX),
         gdd_cum = cumsum(gdd),
         date = as_datetime(date))


ggplot(PHL_INT_proc, aes(date, gdd_cum)) +
  geom_point()


gdd_2017 <- read_csv("~/hivescaler/inst/extdata/franklin_inst_GDD_2017.csv")
%>%
  mutate(gdd = as.numeric(GDD),
         gdd_int = na.interpolation(gdd),
         cum_gdd = cumsum(gdd_int),
         datetime = as_datetime(Date)) %>%
  select(datetime, cum_gdd) %>%
  right_join(dat2017_proc, by = c("datetime" = "TimeStamp_round")) %>%
  group_by(ScaleID) %>%
  arrange(datetime) %>%
  mutate(cum_gdd_int = na.interpolation(cum_gdd)) %>%
  select(datetime, ScaleID, Site, Hive, everything(), -cum_gdd)

plot_delta_gdd(gdd_2017, metric = "wt_diff_clean_25h")
plot_wt_gdd(gdd_2017, metric = "wt_recon")



gdd_2018 <- read_csv("~/hivescaler/inst/extdata/franklin_inst_GDD_2018.csv")
%>%
  mutate(gdd = as.numeric(GDD),
         gdd_int = na.interpolation(gdd),
         cum_gdd = cumsum(gdd_int),
         datetime = as_datetime(Date)) %>%
  select(datetime, cum_gdd) %>%
  right_join(dat2018_proc, by = c("datetime" = "TimeStamp_round")) %>%
  group_by(ScaleID) %>%
  arrange(datetime) %>%
  mutate(cum_gdd_int = na.interpolation(cum_gdd)) %>%
  select(datetime, ScaleID, Site, Hive, everything(), -cum_gdd)


plot_delta_gdd(gdd_2018, metric = "wt_diff_clean_25h")
plot_wt_gdd(gdd_2018, metric = "wt_recon")

