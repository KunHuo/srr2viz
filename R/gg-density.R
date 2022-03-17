
add_line <- function(data, x, group, plot){
  means <- tapply(data[[x]], INDEX = data[[group]], FUN = mean, na.rm = TRUE)
  means <- data.frame(group = names(means), linetype = "mean", value = means)

  medians <- tapply(data[[x]], INDEX = data[[group]], FUN = median, na.rm = TRUE)
  medians <- data.frame(group = names(medians), linetype = "median", value = medians)

  stats <- rbind(means, medians)

  plot +
    geom_vline(aes(xintercept = .data[["value"]], color = group, linetype = linetype), data = stats)
}




gg_density <- function(data,
                       x,
                       y = "density",
                       group = NULL,
                       facet = NULL,
                       x.breaks = NULL,
                       y.breaks = NULL,
                       legend.title = "default",
                       legend.label = "default",
                       legend.pos = "default",
                       add.mean.line = FALSE,
                       add.median.line = FALSE,
                       palate = NULL,
                       theme = NULL, ...){


  if(is.null(group)){
    data$.group <- factor(rep("overall", nrow(data)))
    group <- ".group"
  }

  data[[group]] <- factor(data[[group]])

  p <- ggplot(data) +
    geom_density(aes(x = .data[[x]], color = .data[[group]], fill = .data[[group]]), alpha = 0.5)

  x.breaks <- pretty_xbreaks(p, x.breaks)
  y.breaks <- pretty_ybreaks(p, y.breaks, zero = TRUE)
  x.limits <- c(min(x.breaks), max(x.breaks))
  y.limits <- c(min(y.breaks), max(y.breaks))
  x.expand <- c(0, 0)
  y.expand <- c(0, 0)

  p <- p +
    scale_x_continuous(expand = x.expand, breaks = x.breaks, limits = x.limits) +
    scale_y_continuous(expand = y.expand, breaks = y.breaks, limits = y.limits) +
    theme_sci()

  if(add.mean.line){
    p <- add_line(data, x, group, plot = p)
  }

  if(group == ".group"){
    p <- p + legend_position("none")
  }

  if(!is.null(palate)){
    p <- p +
      scale_fill_manual(values = palate) +
      scale_color_manual(values = palate)
  }

  p
}



