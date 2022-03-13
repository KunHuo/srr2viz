gg_axis_ticks_length <- function(value = 0.15){
  ggplot2::theme(axis.ticks.length = ggplot2::unit(value, "cm"))
}


gg_tag <- function(value){
  ggplot2::ggtitle(value)
}


gg_bold_axis_title <- function(value = TRUE){
  ggplot2::theme(
    axis.title = ggplot2::element_text(face = "bold")
  )
}
