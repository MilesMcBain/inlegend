##' Inset legend themes for ggplot2 maps
##'
##' These themes move and restyle the ggplot2 legend to be more in line with mapping nroms.
##' `ggplot2` are inset in the desired map corner. The legends are restyled to appear simpler and draw less attention when overlaid on maps.
##'
##' Convenience parameters are provided to tweak the size of legend text. The text style can be fully customised with the `ggplot2` theme engine. See examples.
##'
##' @name inset_themes
##' @title inset map legend themes
##' @param location the location to inset legends. One of "top-left", "top-right", "bottom-left", "bottom-right"
##' @param title_text_size a convenience parameter for setting the size of the legend title text. For high resolution maps the defaults tend to be too small.
##' @param text_size a convenience parameter for setting the size the legend key elements text. For high resolution maps the defaults tend to be too small.
##' @return `inlegend_dark` returns dark legend theme based on kepler.gl as an object of classes `theme` and `gg`.
##' @author Miles McBain
##' @export
##' @example inst/examples/coffee.R
inset_legend_dark <- function(location = "top-right",
                              title_text_size = NULL,
                              text_size = NULL) {
  inset_legend_custom(
    inlegend.location = location,
    legend.background = ggfx::with_shadow(
      ggplot2::element_rect(
        colour = "#242730",
        fill = "#242730",
        size = 0.6
      ),
      x_offset = -1,
      y_offset = 1,
      sigma = 4
    ),
    legend.text = ggplot2::element_text(
      colour = "#a0a7b4",
      size = text_size
    ),
    legend.title = ggplot2::element_text(
      colour = "#f0f0f0",
      margin = ggplot2::margin(0, 0, 5, 0),
      size = title_text_size
    ),
    legend.key = ggplot2::element_rect(
      fill = "#242730",
      colour = NA
    )
  )
}

##' @rdname inset_themes
##' @return `inlegend_light` returns a light white legend theme as an object of classes `theme` and `gg`.
##' @export
inset_legend_light <- function(location = "top-right",
                               title_text_size = NULL,
                               text_size = NULL) {
  inset_legend_custom(
    inlegend.location = location,
    legend.background = ggfx::with_shadow(
      x_offset = -1,
      y_offset = 1,
      sigma = 2,
      ggplot2::element_rect(
        colour = "#ffffff",
        fill = "#ffffff",
        size = 0.6
      )
    ),
    legend.text = ggplot2::element_text(
      colour = "#767d7d",
      size = text_size
    ),
    legend.title = ggplot2::element_text(
      colour = "#2b2d2d",
      margin = ggplot2::margin(0, 0, 5, 0),
      size = title_text_size
    ),
    legend.key = ggplot2::element_rect(
      fill = "#ffffff",
      colour = NA
    )
  )
}

globalVariables(".inlegend_positions", "inlegend")
.inlegend_positions <- new.env()
.inlegend_positions$`top-right` <-
  list(
    legend.box.margin = ggplot2::margin(r = 5, t = 5),
    legend.justification = c(1, 1),
    legend.position = c(1.0, 1.0)
  )
.inlegend_positions$`top-left` <-
  list(
    legend.box.margin = ggplot2::margin(l = 5, t = 5),
    legend.justification = c(0, 1),
    legend.position = c(0, 1.0)
  )
.inlegend_positions$`bottom-right` <-
  list(
    legend.box.margin = ggplot2::margin(r = 5, b = 5),
    legend.justification = c(1, 0),
    legend.position = c(1.0, 0)
  )
.inlegend_positions$`bottom-left` <-
  list(
    legend.box.margin = ggplot2::margin(l = 5, b = 5),
    legend.justification = c(0, 0),
    legend.position = c(0, 0)
  )

##' Build a custom inset map legend
##'
##' This function has defaults that will position an inset map ggplot2 legend. It is the legend constructor used by `inset_legend_light` and `inset_legend_dark`.
##'
##' @title build custom inset map legend.
##' @param inlegend.location the location of the legend. One of "top-right",
##'   "top-left", "bottom-left", "bottom-right"
##' @param legend.background the background rectangle the legend is drawn on. A
##'   `ggplot::element_rect`, where the following parameter mappings map to what
##'   you see:
##'
##'   * `fill` - background colour
##'   * `colour` - border colour
##'   * `size` -  border thickness.
##' @param legend.text text for labels, eg factor names, or scale tick labels. A
##'   `ggplot2::element_text`, where the following parameters map to what you see:
##'   * `colour` - the text colour
##'   * `size` - the text size
##'   * `face` - the text face (e.g. bold)
##'   * `family` - the text font
##'   * `margin` - `ggplot2::margin` that sets the spacing around the legend element text
##' @param legend.title text for legend title. A ggplot2::element_text, where
##'   the following parameters map to what you see:
##'
##'   * `colour` - the text colour
##'   * `size` - the text size
##'   * `face` - the text face (e.g. bold)
##'   * `family` - the text font
##'   * `margin` - `ggplot2::margin` that sets the spacing around the title text
##'   and legend.background edge.
##' @param legend.key the background rectangles that the individual legend key elements are drawn on. A `ggplot2::element_rect` where the following parameters map to what you see:
##'
##' * `fill` - the rectangle
##' @param legend.margin The spacing between the edge of the legend.background and legend elements (text, keys etc.). A `ggplot2::margin object`. Additional padding on bottom seems to be required for things to look right.
##' @param ... arguments passed to `ggplot2::theme`.
##' @return a object of classes 'theme' and 'gg'
##' @author Miles McBain
##' @importFrom magrittr %>%
##' @export
inset_legend_custom <- function(inlegend.location = c(
                                  "top-right",
                                  "top-left",
                                  "bottom-left",
                                  "bottom-right"
                                ),
                                legend.background,
                                legend.text,
                                legend.title,
                                legend.key,
                                legend.margin = ggplot2::margin(8, 8, 10, 8),
                                ...) {
  legend_location <- match.arg(inlegend.location)
  location_params <- get(legend_location, envir = .inlegend_positions)

  ggplot2::theme(
    legend.background = legend.background,
    legend.text = legend.text,
    legend.title = legend.title,
    legend.key = legend.key,
    legend.justification = location_params$legend.justification,
    legend.position = location_params$legend.position,
    legend.margin = legend.margin,
    legend.box.margin = location_params$legend.box.margin,
    ...
  )
}