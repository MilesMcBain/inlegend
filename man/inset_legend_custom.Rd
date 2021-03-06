% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/legends.R
\name{inset_legend_custom}
\alias{inset_legend_custom}
\title{build custom inset map legend.}
\usage{
inset_legend_custom(
  inlegend.location = c("top-right", "top-left", "bottom-left", "bottom-right"),
  legend.background,
  legend.text,
  legend.title,
  legend.key,
  legend.margin = ggplot2::margin(8, 8, 10, 8),
  ...
)
}
\arguments{
\item{inlegend.location}{the location of the legend. One of "top-right",
"top-left", "bottom-left", "bottom-right"}

\item{legend.background}{the background rectangle the legend is drawn on. A
\code{ggplot::element_rect}, where the following parameter mappings map to what
you see:
\itemize{
\item \code{fill} - background colour
\item \code{colour} - border colour
\item \code{size} -  border thickness.
}}

\item{legend.text}{text for labels, eg factor names, or scale tick labels. A
\code{ggplot2::element_text}, where the following parameters map to what you see:
\itemize{
\item \code{colour} - the text colour
\item \code{size} - the text size
\item \code{face} - the text face (e.g. bold)
\item \code{family} - the text font
\item \code{margin} - \code{ggplot2::margin} that sets the spacing around the legend element text
}}

\item{legend.title}{text for legend title. A ggplot2::element_text, where
the following parameters map to what you see:
\itemize{
\item \code{colour} - the text colour
\item \code{size} - the text size
\item \code{face} - the text face (e.g. bold)
\item \code{family} - the text font
\item \code{margin} - \code{ggplot2::margin} that sets the spacing around the title text
and legend.background edge.
}}

\item{legend.key}{the background rectangles that the individual legend key elements are drawn on. A \code{ggplot2::element_rect} where the following parameters map to what you see:
\itemize{
\item \code{fill} - the rectangle
}}

\item{legend.margin}{The spacing between the edge of the legend.background and legend elements (text, keys etc.). A \verb{ggplot2::margin object}. Additional padding on bottom seems to be required for things to look right.}

\item{...}{arguments passed to \code{ggplot2::theme}.}
}
\value{
a object of classes 'theme' and 'gg'
}
\description{
Build a custom inset map legend
}
\details{
This function has defaults that will position an inset map ggplot2 legend. It is the legend constructor used by \code{inset_legend_light} and \code{inset_legend_dark}.
}
\author{
Miles McBain
}
