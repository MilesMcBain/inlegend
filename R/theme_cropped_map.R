##' Remove plot axis and whitespace margins
##'
##' In a map where the legend is inset it may be convenient to have the plot
##' tightly cropped around the map extent. This theme give you a such a plot.
##' Beware that there is no space for titles, so that kind of information must
##' be conveyed in section headings or captions.
##'
##' When saving map using this theme as an image, you will see whitespace in the
##' output unless the aspect ratio of your output image matches the aspect ratio
##' of the map (plot area) exactly.
##' 
##' @title theme_cropped_map
##' @param ... arguments passed to `ggplot2::theme()`
##' @return
##' @author Miles McBain
##' @export
theme_cropped_map <- function(...) {
  list(
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      plot.margin = ggplot2::unit(
        x = c(0, 0, 0, 0),
        units = "mm"
      ),
      axis.ticks.length = ggplot2::unit(
        x = 0,
        units = "mm"
      ),
      ...
    ),
    ggplot2::scale_y_continuous(expand = c(0, 0)),
    ggplot2::scale_x_continuous(expand = c(0, 0))
  )
}
