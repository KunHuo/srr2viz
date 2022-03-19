#' Arrange Multiple ggplots
#'
#' Arrange multiple ggplots on the same page. Wrapper around plot_grid().
#' Can also create a common unique legend for multiple plots.
#'
#' @param ... List of plots to be arranged into the grid.
#' @param plotlist (optional) list of plots to display.
#' @param ncol (optional) number of columns in the plot grid.
#' @param nrow (optional) number of rows in the plot grid.
#' @param tag.levels (optional) list of labels to be added to the plots. You
#' can also set "A", "a", "1", "[A]", "[a]", "[1]", "(A)", "(a)"or "(1)" to
#' auto-generate tag sequence. In addition, you can also directly assign values
#' through the character vector.
#' @param tag.bold logical value, whether to make tags bold.
#' @param tag.size tags size.
#' @param align (optional) Specifies whether graphs in the grid should be
#' horizontally ("h") or vertically ("v") aligned. Options are "none" (default),
#' "hv" (align in both directions), "h", and "v".
#' @param widths (optional) numerical vector of relative columns widths. For example,
#' in a two-column grid, widths = c(2, 1) would make the first column twice as
#' wide as the second column.
#' @param heights same as widths but for column heights.
#' @param plot.margin margin around entire plot (vector with the sizes of the top,
#' right, bottom, and left margins).
#' @param collect.position the position of legends ("none", "left", "right",
#' "bottom", "top", or two-element numeric vector)
#' @param collect.legend logical value. Default is FALSE. If TRUE, a common
#' unique legend will be created for arranged plots.
#' @param legend.grob a legend grob as returned by the function get_legend().
#' If provided, it will be used as the common legend.
#'
#' @return return an object of class ggarrange, which is a ggplot or a list of ggplot.
#' @export
gg_arrange <- function(...,
                      plotlist = NULL,
                      ncol = NULL,
                      nrow = NULL,
                      tag.levels = NULL,
                      tag.bold = FALSE,
                      tag.size = NULL,
                      align = c("none", "h", "v", "hv"),
                      widths = 1, heights = 1,
                      plot.margin = NULL,
                      collect.legend = FALSE,
                      collect.position = "right",
                      legend.grob = NULL){

  plots <- c(list(...), plotlist)

  if(!is.null(tag.levels)){
    tags <- .tag_levels(tags = tag.levels, n = length(plots))
    plots <- Map(function(p, tag){
      p + ggplot2::ggtitle(tag)
    }, plots, tags)
  }

  if(tag.bold){
    plots <- lapply(plots, function(p){
       p + ggplot2::theme(
         plot.title = ggplot2::element_text(face = "bold")
       )
    })
  }

  if(!is.null(plot.margin)){
    plots <- lapply(plots, function(p){
      p + ggplot2::theme(
        plot.margin = ggplot2::unit(plot.margin, "cm"), # top, right, bottom, left
      )
    })
  }

  if(!is.null(tag.size)){
    plots <- lapply(plots, function(p){
      p + ggplot2::theme(
        plot.title = ggplot2::element_text(size = tag.size)
      )
    })
  }

  align <- match.arg(align)
  nb.plots <- length(plots)
  page.layout <- .get_layout(ncol, nrow, nb.plots)
  ncol <- page.layout$ncol
  nrow <- page.layout$nrow

  if(!is.null(legend.grob))
    collect.legend <- TRUE

  if(is.null(collect.position) & collect.legend)
    collect.position <- "top"


  collect.position <- .check_legend(collect.position)


  if(!is.null(collect.position))
    plots <- lapply(
      plots,
      function(x) {
        if(!is.null(x)){
          x
          # if(is.null(get_legend(x))){
          #   x
          # }else{
          #   x + theme(legend.position = collect.position)
          # }
        }

        else x
      }
    )

  if(collect.legend){
    if(is.null(legend.grob))
      legend.grob <- get_legend(plots)
    plots <- lapply(
      plots,
      function(x) {if(!is.null(x)) x + theme(legend.position = "none") else x}
    )
  }

  plots <- list(plots)

  res <- lapply(
    plots,
    .plot_grid,
    ncol = ncol,
    nrow = nrow,
    align = align,
    rel_widths = widths,
    rel_heights = heights,
    legend = collect.position,
    common.legend.grob = legend.grob
  )

  if(length(res) == 1) res <- res[[1]]

  class(res) <- c(class(res), "ggarrange")
  res
}



.get_layout <- function(ncol, nrow, nb.plots){
  if(!is.null(ncol) & !is.null(nrow)){}
  else if(!is.null(ncol)){
    if(ncol == 1) nrow = nb.plots
  }
  else if(!is.null(nrow)){
    if(nrow == 1) ncol = nb.plots
  }
  list(ncol = ncol, nrow = nrow)
}


.plot_grid <- function(plotlist, legend = "top", common.legend.grob = NULL,  ... ){
  res <- cowplot::plot_grid(plotlist = plotlist, ...)
  if(is.null(common.legend.grob)) return(res)
  else {
    leg <- common.legend.grob
    lheight <- sum(leg$height)
    lwidth <- sum(leg$width)
  }

  arrangeGrob <- gridExtra::arrangeGrob
  unit.c <- grid::unit.c
  .unit <- grid::unit(1, "npc")

  res <- switch(legend,
                top = arrangeGrob(leg, res, ncol = 1,
                                  heights = unit.c(lheight, .unit - lheight)),
                bottom = arrangeGrob(res, leg, ncol = 1,
                                     heights = unit.c(unit(1, "npc") - lheight, lheight)),
                left = arrangeGrob(leg, res, ncol = 2,
                                   widths = unit.c(lwidth, .unit - lwidth)),
                right = arrangeGrob(res, leg, ncol = 2,
                                    widths = unit.c(.unit - lwidth, lwidth))
  )

  p <- cowplot::ggdraw() + cowplot::draw_grob(grid::grobTree(res))
  p
}


.check_legend <- function(legend){

  allowed.values <- c("top", "bottom", "left", "right", "none")

  if(is.null(legend) | is.numeric(legend))
    return(legend)
  else if(is.logical(legend)){
    if(legend) legend <- "top"
    else legend <- "none"
  }
  else if(is.character(legend)){
    legend <- legend[1]
    if(!legend %in% allowed.values)
      stop("Argument legend should be one of ", .collapse(allowed.values, sep = ", "))
  }
  return (legend)
}

.collapse <- function(x, y = NULL, sep = "."){
  if(missing(y))
    paste(x, collapse = sep)
  else if(is.null(x) & is.null(y))
    return(NULL)
  else if(is.null(x))
    return (as.character(y))
  else if(is.null(y))
    return(as.character(x))
  else
    paste0(x, sep, y)
}

get_legend <- function(p, position = NULL){

  if(.is_list(p)){
    continue <- TRUE
    i <- 1
    while(i <= length(p) & continue){
      leg <- .get_legend(p[[i]], position = position)
      if(!is.null(leg)) continue <- FALSE
      i <- i+1
    }
  }
  else{
    leg <- .get_legend(p, position = position)
  }
  leg
}

.get_legend <- function(p, position = NULL){

  if(is.null(p)) return(NULL)
  if(!is.null(position)){
    p <- p + theme(legend.position = position)
  }
  tmp <- ggplot_gtable(ggplot_build(p))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  if(length(leg) > 0) leg <- tmp$grobs[[leg]]
  else leg <- NULL
  leg
}

.is_list <- function(x){
  inherits(x, "list")
}

.tag_levels <- function(tags, n){

  if(is.null(tags)){
    return(NULL)
  }

  if(length(tags) == 1L){
    if(tags[1] == "A"){
      tag.levels <- LETTERS[1:n]
    }else if(tags[1] == "a"){
      tag.levels <- letters[1:n]
    }else if(tags == "(A)"){
      tag.levels <- sprintf("(%s)", LETTERS[1:n])
    }else if(tags == "(a)"){
      tag.levels <- sprintf("(%s)", letters[1:n])
    }else if(tags == "[A]"){
      tag.levels <- sprintf("[%s]", LETTERS[1:n])
    }else if(tags == "[a]"){
      tag.levels <- sprintf("[%s]", letters[1:n])
    }else if(tags == "(1)"){
      tag.levels <- sprintf("(%s)", 1:n)
    }else if(tags == "[1]"){
      tag.levels <- sprintf("[%s]", 1:n)
    }else{
      tag.levels <- LETTERS[1:n]
    }
  }else{
    if(length(tags) != n){
      stop("The number of tags should match the number of figures")
    }
    tags
  }
}
