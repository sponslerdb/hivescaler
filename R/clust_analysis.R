#' Convert data to aligned XTS object and transpose
#'
#' @param x A tibble or data frame
#' @param omit A vector of strings indicating ScaleIDs to be dropped
#' @return A data frame of row-wise time series
#' @export
clusterprep <- function(x, omit) {
  out <- x %>%
    filter(weight_var == "wt_diff_clean_25h") %>%
    filter(!ScaleID %in% omit) %>%
    na.omit() %>%
    select(ScaleID, TimeStamp, weight = value) %>%
    group_by(ScaleID) %>%
    nest() %>%
    mutate(data_xts = map(data, timetk::tk_xts)) %>% # Convert each item in list column to xts time series object
    mutate(aligned_xts = map(data_xts, xts_align)) %>% # Align to nearest hour; this enables merge step below
    select(ScaleID, aligned_xts)

  names(out$aligned_xts) <- out$ScaleID

  return(out)
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
                        trace = TRUE, control = "ward.D2") {

  clust <- dtwclust::tsclust(x$aligned_xts, type = type, distance = distance,
                             preproc = zscore, seed = 1, trace = trace,
                             control = hierarchical_control(method = control))

  dtwclust::plot(clust) # Plot dendrogram

  return(clust)
}

#' Convert distance matrix to long-form tibble in prep for downstream analysis
#'
#' @param x A dtwclust object containing the slot distmat
#' @return A longform tibble with pairwise distances by site
#' @export
tab_dist <- function(x) {
  out <- melt_dist(x@distmat) %>%
    separate(iso1, c("siteA", "hiveA"), sep = "\\.", remove = FALSE) %>%
    separate(iso2, c("siteB", "hiveB"), sep = "\\.", remove = FALSE) %>%
    mutate(in_out = map2_chr(siteA, siteB, inout))
    #group_by(siteA, siteB) %>%
    #summarise(minGAK = min(dist), meanGAK = mean(dist), maxGAK = max(dist)) #%>%
    #filter(siteA != siteB)
  return(out)
}

#' \code{inout} creates a new field in the tabulated distance matrix specifiying whether each pairwise comparison is of hives within a site or hives ar different sites
#'
#' @param x,y Columns in tabulated distance matrix representing each of the two hives in each pairwise comparison
inout <- function(x, y) {
  ifelse(x == y, "in", "out")
}


#' \code{site_effect} draws boxplots and runs t.test to compare pairwise distance withing and across sites
#'
#' @param x a tabulated distance matrix
site_effect <- function(x) {
  boxplot(x$dist ~ x$in_out)
  t.test(x$dist ~ x$in_out)
}




