#' ggplo2 theme for SCI
#'
#' @param font.size font size, default 12,
#' @param font.family font family, detault Times New Roman.
#' @param line.size line size, default 0.25.
#' @param legend.key.size legend key size, dsfault 0.8.
#' @param face.bold a logical, whether bold the title of axis, plot, strip, and legend, default FALSE.
#' @param panel.grid a logical, whether plot grid for the panel, default FALSE.
#' @param panel.border a logical, whether plot panel border, default FALSE.
#' @param aspect.ratio the ratio of the width to the height, default NULL.
#' @param ... further arguments pass to the [ggplot2::theme] function.
#'
#' @export
theme_sci <- function(font.size = 12,
                      font.family = "serif",
                      line.size = 0.25,
                      legend.key.size = 0.8,
                      face.bold = FALSE,
                      panel.grid = FALSE,
                      panel.border = FALSE,
                      aspect.ratio = NULL, ...) {

  face <- ifelse(face.bold, "bold", "plain")

  if(panel.grid){
    panel.grid.major = ggplot2::element_line(color = "gray90", size = line.size)
    panel.grid.minor = ggplot2::element_line(color = "gray90", size = line.size, linetype = "dashed")
  }else{
    panel.grid.major = ggplot2::element_blank()
    panel.grid.minor = ggplot2::element_blank()
  }


  if(panel.border){
    pborder = ggplot2::element_rect(color = "black", size = line.size)
  }else{
    pborder = ggplot2::element_rect(color = "NA")
  }

  ggplot2::theme_bw(
    base_size = font.size,
    base_family = font.family,
    base_line_size = line.size,
    base_rect_size = line.size) +

    ggplot2::theme(

      panel.background = ggplot2::element_rect(fill = NA),
      panel.grid = ggplot2::element_blank(),
      panel.border = pborder,
      panel.grid.major = panel.grid.major,
      panel.grid.minor = panel.grid.minor,

      axis.line = ggplot2::element_line(size = line.size, color = "black",lineend = "square"),
      axis.ticks.length = ggplot2::unit(0.15, "cm"),
      axis.ticks = ggplot2::element_line(color = "black", size = line.size),
      axis.text = ggplot2::element_text(color = "black", size = font.size),
      axis.title = ggplot2::element_text(color = "black", size = font.size, face = face),

      legend.background = ggplot2::element_rect(fill = "NA"),
      legend.text = ggplot2::element_text(color = "black", size = font.size),
      legend.title = ggplot2::element_text(face = face),
      legend.key.size = ggplot2::unit(legend.key.size, "lines"),

      plot.title = ggplot2::element_text(size = font.size + 2, face = face),
      plot.title.position = "plot",
      plot.margin = ggplot2::unit(c(0.25, 0.45, 0.25, 0.25), "cm"),

      strip.text = ggplot2::element_text(color = "black", size = font.size, face = face),
      aspect.ratio = aspect.ratio,
      complete = FALSE,
      ...
    )
}
