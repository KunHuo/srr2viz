gg_point <- function(data, x.var, y.var, g.var, fact){
  ggplot2::ggplot(data) +
    ggplot2::geom_point(ggplot2::aes_string(x = x.var, y = y.var))
}



gg_point(iris, x.var = "Sepal.Length", y.var = "Sepal.Width")
