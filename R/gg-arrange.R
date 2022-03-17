
gg_arrange <- function(..., plotlist = NULL,
                      ncol = NULL,
                      nrow = NULL,
                      tag.levels = NULL,
                      tag.bold = FALSE,
                      tag.size = NULL,
                      align = c("none", "h", "v", "hv"),
                      widths = 1, heights = 1,
                      plot.margin = NULL,
                      legend.position = "right", legend.collect = FALSE, legend.grob = NULL){

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
  nb.plots.per.page <- .nbplots_per_page(ncol, nrow)

  if(!is.null(legend.grob))
    legend.collect <- TRUE
  if(is.null(legend.position) & legend.collect)
    legend.position <- "top"
  legend.position <- .check_legend(legend.position)
  if(!is.null(legend.position))
    plots <- lapply(
      plots,
      function(x) {if(!is.null(x)) x + theme(legend.position = legend.position) else x}
    )

  if(legend.collect){
    if(is.null(legend.grob))
      legend.grob <- get_legend(plots)
    plots <- lapply(
      plots,
      function(x) {if(!is.null(x)) x + theme(legend.position = "none") else x}
    )
  }

  # Split plots over multiple pages
  # if(nb.plots > nb.plots.per.page){
  #   plots <- split(plots, ceiling(seq_along(plots)/nb.plots.per.page))
  # }
  # # One unique page
  # else plots <- list(plots)

  plots <- list(plots)


  res <- lapply(
    plots,
    .plot_grid,
    ncol = ncol,
    nrow = nrow,
    align = align,
    rel_widths = widths,
    rel_heights = heights,
    legend = legend.position,
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


# Compute number of plots per page
.nbplots_per_page <- function(ncol = NULL, nrow = NULL){

  if(!is.null(ncol) & !is.null(nrow))
    ncol * nrow
  else if(!is.null(ncol))
    ncol
  else if(!is.null(nrow))
    nrow
  else Inf
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


# update label parameters for cowplot::plot_grid()
.update_label_pms <- function(font.label,
                              label.x = 0, label.y = 1, hjust = -0.5, vjust = 1.5){

  .font <- list(size = 14, color = "black", face = "bold", family = NULL)
  new.font.names <- names(font.label)
  for(i in new.font.names) .font[[i]] <- font.label[[i]]

  pms <- .font
  list(
    size = pms$size,
    family = pms$family,
    face = pms$face,
    color = pms$color,
    label.x = label.x, label.y = label.y,
    hjust = hjust, vjust = vjust
  )
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
