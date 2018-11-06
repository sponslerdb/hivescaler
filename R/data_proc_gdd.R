plot_wt_gdd <- function(x, by = "site", metric, knots = 25, omit = NULL) {
  x <- filter(x, weight_var %in% metric & !ScaleID %in% omit)

  if(by == "hive") {
    p <- ggplot(x, aes(x$cum_gdd_int, x$value)) +
      geom_point() +
      xlab("Time") +
      ylab("Weight (kg, zero-based)") +
      facet_wrap(~ ScaleID) +
      theme_gray(18)
  }

  if(by == "site") {
    p <- ggplot(x, aes(cum_gdd_int, value, color = Hive)) +
      geom_point() +
      xlab("Time") +
      ylab("Weight (kg, zero-based)") +
      facet_wrap(~ Site) +
      theme_gray(18)
  }

  if(by == "none") {
    p <- ggplot(x, aes(x$cum_gdd_int, x$value)) +
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


plot_delta_gdd <- function(x, type = "points", by = "none", metric, knots = 25, omit = NULL) {
  x <- filter(x, weight_var %in% metric & !ScaleID %in% omit)

  if(by == "hive") {
    if(type == "points") {
      p <- ggplot(x, aes(cum_gdd_int, value)) +
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

  if(by == "site") {
    if(type == "points") {
      p <- ggplot(x, aes(cum_gdd_int, value)) +
        geom_point(alpha = 0.1, aes(color = delta_sign)) +
        stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = knots), geom = "area", alpha = 0.8) +
        coord_cartesian(ylim = c(-0.2, 0.2)) +
        xlab("Time") +
        ylab("Δ Weight (kg)") +
        facet_wrap(~Site) +
        theme_gray(18) +
        theme(legend.title=element_blank())
    }
    if(type == "hist") {
      p <- ggplot(x, aes(value)) +
        geom_histogram(binwidth = 0.05) +
        xlim(-5, 5) +
        facet_wrap(~Site) +
        theme_gray(18)
    }
  }

  if(by == "none") {
    if(type == "points") {
      p <- ggplot(x, aes(cum_gdd_int, value)) +
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
