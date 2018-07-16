#' Convert data to aligned XTS object and transpose
#'
#' @param x A tibble or data frame
#' @return A data frame of row-wise time series
#' @export
clusterprep <- function(x) {
  data_in <- x %>%
    select(ScaleID, TimeStamp, weight = Weight_loess_diff_z) %>%
    group_by(ScaleID) %>%
    nest() %>%
    mutate(data_xts = map(data, timetk::tk_xts)) %>% # Convert each item in list column to xts time series object
    mutate(aligned_xts = map(data_xts, xts_align)) %>% # Align to nearest hour; this enables merge step below
    select(ScaleID, aligned_xts)
  out <- do.call(merge, data_in$aligned_xts) %>% na.omit() # Merge time series back into single xts object
  colnames(out) <- data_in$ScaleID
  return(t(out))
}

#' Align time series to nearest hour
#'
#' @param x An XTS time series object
#' @return An XTS time series object with observations aligned to nearest hour
#' @export
xts_align <- function(x) {
  xts::align.time(x, 60*60)
}

#' Cluster time series, plot dendrogram, print cluster validation indices
#'
#' @param x A data frame of row-wise time series
#' @param type See dtwclust::tso
#' @param distance See dtwclust::tso
#' @param preproc See dtwclust::tso
#' @param trace See dtwclust::tso
#' @param control See dtwclust::tso
#' @return A cluster object, dendrogram, and cluster validation indices
#' @export
cluster_func <- function(x, type = "hierarchical", distance = "gak",
                         preproc = NULL, trace = TRUE, control = "ward.D2") {
  clust <- dtwclust::tsclust(x, type = type, distance = distance,
                             preproc = preproc, seed = 1, trace = trace,
                             control = hierarchical_control(method = control))
  dtwclust::plot(clust) # Plot SBD dendrogram
  dtwclust::cvi(clust)
}

#' Convert distance matrix to long-form tibble in prep for downstream analysis
#'
#' @param x A dtwclust object containing the slot distmat
#' @return A longform tibble with pairwise distances by site
#' @export
tab_dist <- function(x) {
  out <- melt_dist(x$distmat) %>%
    separate(iso1, c("siteA", "hiveA"), sep = -1, remove = TRUE) %>%
    separate(iso2, c("siteB", "hiveB"), sep = -1, remove = TRUE) %>%
    group_by(siteA, siteB) %>%
    summarise(minGAK = min(dist), meanGAK = mean(dist)) %>%
    filter(siteA != siteB)
  return(out)
}

