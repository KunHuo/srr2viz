gg_point <- function(data, x.var, y.var, x.breaks.n = 5, y.breaks.n = 5, ...){

  x.var <- rlang::enquo(x.var)
  y.var <- rlang::enquo(y.var)

  x.var <- rlang::as_name(x.var)
  y.var <- rlang::as_name(y.var)

  x.vec <- data[[x.var]]
  y.vec <- data[[y.var]]

  x.breaks <- pretty(c(min(x.vec, na.rm = TRUE), max(x.vec, na.rm = TRUE)), x.breaks.n)
  y.breaks <- pretty(c(min(y.vec, na.rm = TRUE), max(y.vec, na.rm = TRUE)), y.breaks.n)

  x.limits <- c(min(x.breaks), max(x.breaks))
  y.limits <- c(min(y.breaks), max(y.breaks))

  x.expand <- c(0, 0)
  y.expand <- c(0, 0)

  ggplot2::ggplot(data, ggplot2::aes(x = .data[[x.var]], y = .data[[y.var]])) +
    ggplot2::geom_point(...) +
    ggplot2::scale_y_continuous(expand = x.expand, breaks = y.breaks, limits = y.limits) +
    ggplot2::scale_x_continuous(expand = y.expand, breaks = x.breaks, limits = x.limits) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_classic()

}

