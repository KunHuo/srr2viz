#' Density plot
#'
#' @param data data
#' @param x x
#' @param y y
#' @param group group
#' @param facet facet
#' @param x.breaks x breaks.
#' @param y.breaks y breaks.
#' @param legend.title legend title.
#' @param legend.label legend labels
#' @param legend.position legend position.
#' @param add allowed values are one of "mean" or "median" (for adding mean or
#' median line, respectively).
#' @param palate 	the color palette to be used for coloring or filling by groups.
#' Allowed values include "grey" for grey color palettes; brewer palettes e.g.
#' "RdBu", "Blues", ...; or custom color palette e.g. c("blue", "red"); and
#' scientific journal palettes from ggsci R package, e.g.: "npg", "aaas",
#' "lancet", "jco", "ucscgb", "uchicago", "simpsons" and "rickandmorty".
#' @param theme function, ggplot2 theme name. Default value is theme_sci().
#' Allowed values include ggplot2 official themes: theme_gray(), theme_bw(),
#' theme_minimal(), theme_classic(), theme_void(), ....
#' @param ... other arguments to be passed to geom_density
#'
#' @return ggplot
#' @export
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
                       add = c("mean", "median", "none"),
                       palate = NULL,
                       theme = theme_sci(), ...){

  args <- list( data = data,
                x = x,
                y = y,
                group = group,
                x.breaks = x.breaks,
                y.breaks = y.breaks,
                legend.title = legend.title,
                legend.label = legend.label,
                legend.position = legend.position,
                add = add,
                palate = palate,
                theme = theme,
                ... = ...)

  if(is.null(facet)){
    do.call(gg_density_core, args = args)

  }else{
    p <- do.call(gg_density_core, args = args)
    x.breaks <- .pretty_xbreaks(p, x.breaks, facet = facet)
    y.breaks <- .pretty_ybreaks(p, y.breaks, zero = TRUE, facet = facet)

    plotlist <- by.data.frame(data, INDICES = data[[facet]], FUN = function(d){
      args$data <- d
      args$x.breaks <- x.breaks
      args$y.breaks <- y.breaks
      do.call(gg_density_core, args = args)
    })

    patchwork::wrap_plots(plotlist) + patchwork::plot_annotation(tag_levels = "A")
  }

}

gg_density_core <- function(data,
                       x,
                       y = "density",
                       group = NULL,
                       x.breaks = NULL,
                       y.breaks = NULL,
                       legend.title = waiver(),
                       legend.label = waiver(),
                       legend.position = waiver(),
                       add = c("mean", "median", "none"),
                       palate = NULL,
                       theme = theme_sci(), ...){

  # If group is NULL, the group column is added to data for the purpose of color
  # or fill mapping. Last, We will delete legend if the group levels equal to one.
  if(is.null(group)){
    data$.overall <- factor(rep("overall", nrow(data)))
    group <- ".overall"
  }

  # Factorization of group variable.
  if(length(legend.label) == 0L){
    data[[group]] <- factor(data[[group]])
  }else{
    data[[group]] <- factor(data[[group]], labels = legend.label)
  }

  # Create density plot
  p <- suppressWarnings( ggplot(data, aes(x = .data[[x]], color = .data[[group]], fill = .data[[group]])) +
                           geom_density(alpha = 0.5, size = 0.25, ...))

  add <- match.arg(add)
  p <- .add_line(data = data, x = x, group = group, plot = p, type = add)

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


.add_line <- function(data, x, group, plot, type = "mean"){
  if(type != "none"){
    stats <- tapply(data[[x]], INDEX = data[[group]], FUN = function(x){
      do.call(type, args = list( x = x, na.rm = TRUE))
    })
    stats <- data.frame(group = names(stats), value = stats)

    plot <- plot +
      geom_vline(aes(xintercept = .data[["value"]], color = .data[["group"]]), data = stats, linetype = "dashed", size = 0.25)
  }
  plot
}



