gg_density <- function(data,
                       x,
                       y = "density",
                       group = NULL,
                       facet = NULL,
                       x.breaks = NULL,
                       y.breaks = NULL,
                       legend.title = waiver(),
                       legend.label = waiver(),
                       legend.position = waiver(),
                       add.mean.line = FALSE,
                       add.median.line = FALSE,
                       palate = NULL,
                       theme = theme_sci(), ...){

  # If group is NULL, the group column is added to data for the purpose of color
  # or fill mapping. Last, We will delete legend if the group levels equal to one.
  if(is.null(group)){
    data$.overall <- factor(rep("overall", nrow(data)))
    group <- ".overall"
  }

  # Factorization of group variable.
  data[[group]] <- factor(data[[group]])

  # Create density plot
  p <- ggplot(data) +
    geom_density(aes(x = .data[[x]], color = .data[[group]], fill = .data[[group]]), alpha = 0.5)

  if(add.mean.line){
    p <- add_line(data, x, group, plot = p)
  }

  # Set the X and Y axes
  x.breaks <- .pretty_xbreaks(p, x.breaks)
  y.breaks <- .pretty_ybreaks(p, y.breaks, zero = TRUE)

  x.limits <- c(min(x.breaks), max(x.breaks))
  y.limits <- c(min(y.breaks), max(y.breaks))

  x.expand <- c(0, 0)
  y.expand <- c(0, 0)

  p <- p +
    scale_x_continuous(expand = x.expand, breaks = x.breaks, limits = x.limits) +
    scale_y_continuous(expand = y.expand, breaks = y.breaks, limits = y.limits)

  # Set y labels
  p <- p + ggplot2::ylab("Density")

  # set theme
  if(!is.null(theme)){
    p <- p + theme
  }

  # set palate
  if(!is.null(palate)){
    p <- p +
      scale_fill_manual(values = palate) +
      scale_color_manual(values = palate)
  }

  # Set legend title.
  p <- .set_legend_title(plot = p, title = legend.title, data = data, group = group)

  # Set legend position.
  p <- .set_legend_position(plot = p, position = legend.position)

  # Delete legend if the group levels equal to one.
  if(length(unique(data[[group]])) == 1L){
    p <- p + legend_position("none")
  }

  p
}


add_line <- function(data, x, group, plot){
  means <- tapply(data[[x]], INDEX = data[[group]], FUN = mean, na.rm = TRUE)
  means <- data.frame(group = names(means), linetype = "mean", value = means)

  medians <- tapply(data[[x]], INDEX = data[[group]], FUN = median, na.rm = TRUE)
  medians <- data.frame(group = names(medians), linetype = "median", value = medians)

  stats <- rbind(means, medians)

  plot +
    geom_vline(aes(xintercept = .data[["value"]], color = .data[["group"]], linetype = .data[["linetype"]]), data = stats)
}



