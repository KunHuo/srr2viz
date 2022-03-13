gg_point <- function(data, x, y, fact, alpha = 1, colour = "black", fill = "black", shape = 16, size = 1.5, x.breaks.n = 5, y.breaks.n = 5, ...){

  x <- rlang::enquo(x)
  y <- rlang::enquo(y)

  x <- rlang::as_name(x)
  y <- rlang::as_name(y)

  opts <- list(x = x, y = y,
               alpha = alpha,
               colour = colour,
               fill = fill,
               hape = shape,
               size = size,
               x.breaks.n = x.breaks.n,
               y.breaks.n = y.breaks.n,
               ... = ...)

  plotlist <- by.data.frame(data = data, INDICES =  data[[fact]], function(d){
    opts$data <- NULL
    opts$data <- d
    # opts$x.breaks <- seq(4, 8, 1)
    # opts$y.breaks <- seq(2, 5, 1)
    do.call(gg_point_core, args = opts)
  })

  cowplot::plot_grid(plotlist = plotlist, labels = "AUTO")

}



gg_point_core <- function(data, x, y, alpha = 1, colour = "black", fill = "black", shape = 16, size = 1.5, x.breaks = NULL, y.breaks = NULL,x.breaks.n = 5, y.breaks.n = 5, ...){

  x.vec <- data[[x]]
  y.vec <- data[[y]]

  if(is.null(x.breaks)){
    x.breaks <- pretty(c(min(x.vec, na.rm = TRUE), max(x.vec, na.rm = TRUE)), x.breaks.n)
  }

  if(is.null(y.breaks)){
    y.breaks <- pretty(c(min(y.vec, na.rm = TRUE), max(y.vec, na.rm = TRUE)), y.breaks.n)
  }

  x.limits <- c(min(x.breaks), max(x.breaks))
  y.limits <- c(min(y.breaks), max(y.breaks))

  x.expand <- c(0, 0)
  y.expand <- c(0, 0)

  opts <- list(alpha = alpha, colour = colour, shape = shape, size = size, fill = fill)

  index <- sapply(opts, function(x){ as.character(x) %in% names(data)})
  aes.opt <- names(opts)[index]
  aes.str <- paste(sprintf("%s = .data[[%s]]", aes.opt, aes.opt), collapse = ", ")
  aes.str <- sprintf("ggplot2::aes(%s)", aes.str)
  aes <- eval(parse(text = aes.str))

  opts <- opts[!index]
  opts$mapping <- aes
  geom <- do.call("geom_point", args = opts)

  ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
    geom +
    ggplot2::scale_y_continuous(expand = x.expand, breaks = y.breaks, limits = y.limits) +
    ggplot2::scale_x_continuous(expand = y.expand, breaks = x.breaks, limits = x.limits) +
    ggplot2::coord_cartesian(clip = "off")
}
