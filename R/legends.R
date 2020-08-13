inlegend_dark <- function() {
  theme(
    legend.justification = c(1, 1),
    legend.position = c(1.0, 1.0),
    legend.background = element_rect(
      colour = "#29323c",
      fill = "#242730",
      size = 0.6
    ),
    legend.text = element_text(
      colour = "#a0a7b4"
    ),
    legend.title = element_text(
      colour = "#f0f0f0",
      margin = margin(0, 0, 5, 0)
    ),
    legend.margin = margin(8, 8, 10, 8),
    legend.box.margin = margin(r = 5, t = 5)
  )
}

inlegend_light <- function() {
  theme(
    legend.justification = c(1, 1),
    legend.position = c(1.0, 1.0),
    legend.background = element_rect(
      colour = "#d3d5d6",
      fill = "#ffffff",
      size = 0.6
    ),
    legend.text = element_text(
      colour = "#767d7d"
    ),
    legend.title = element_text(
      colour = "#2b2d2d",
      margin = margin(0, 0, 5, 0)
    ),
    legend.margin = margin(8, 8, 10, 8),
    legend.box.margin = margin(r = 5, t = 5)
  )
}


##' Build a custom inset map legend
##'
##' This function has defaults that will position an inset map ggplot2 legend
##' 
##' @title 
##' @param legend.background 
##' @param legend.text 
##' @param legend.titlelegend.justification 
##' @param legend.position 
##' @param legend.margin 
##' @param legend.box.margin 
##' @param ... 
##' @return 
##' @author 
inlegend_custom <- function(legend.background,
                            legend.text,
                            legend.title
                            legend.justification = c(1, 1),
                            legend.position = c(1.0, 1.0),
                            legend.margin = ggplot2::margin(8, 8, 10, 8),
                            legend.box.margin = margin(r = 5, t = 5),
                            ...){

  ggplot2::theme(legend.background = legend.background,
                 legend.text = legend.text,
                 legend.title = legend.title
                 legend.justification = legend.justification,
                 legend.position = legend.position,
                 legend.margin = legend.margin,
                 legend.box.margin = legend.box.margin,
                 ...)

}

