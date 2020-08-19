
<!-- README.md is generated from README.Rmd. Please edit that file -->

# inlegend

<!-- badges: start -->

<!-- badges: end -->

`ggplot2` theming helpers for making static maps with inset legends:

  - `inset_legend_dark()`
  - `inset_legend_light()`
  - `theme_cropped_map()`

## Installation

From [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("milesmcbain/inlegend")
```

## Example

``` r
library(spData)
library(snapbox) ##remotes::install_github("anthonynorth/snapbox")
library(ggplot2)
library(ggspatial)
library(sf)
library(inlegend)

ggplot() +
  layer_mapbox(spData::cycle_hire_osm) +
  layer_spatial(spData::cycle_hire_osm,
                aes(colour = capacity),
                alpha = 0.75) +
  scale_colour_viridis_b() +
  inset_legend_dark("bottom-right") +
  theme_cropped_map() 
#> Warning in showSRID(uprojargs, format = "PROJ", multiline = "NO"): Discarded
#> ellps WGS 84 in CRS definition: +proj=merc +a=6378137 +b=6378137 +lat_ts=0
#> +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +wktext +no_defs
#> Warning in showSRID(uprojargs, format = "PROJ", multiline = "NO"): Discarded
#> datum WGS_1984 in CRS definition
#> Warning in showSRID(uprojargs, format = "PROJ", multiline = "NO"): Discarded
#> ellps WGS 84 in CRS definition: +proj=merc +a=6378137 +b=6378137 +lat_ts=0
#> +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +wktext +no_defs
#> Warning in showSRID(uprojargs, format = "PROJ", multiline = "NO"): Discarded
#> datum WGS_1984 in CRS definition
```

<div class="figure">

<img src="man/figures/README-example-1.png" alt="Bike share capacity, London. Via {spData}" width="100%" />

<p class="caption">

Bike share capacity, London. Via {spData}

</p>

</div>
