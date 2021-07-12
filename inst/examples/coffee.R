library(spData)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(inlegend)

coffee_prod <-
  world %>%
  left_join(coffee_data,
    by = "name_long"
  )
ggplot() +
  layer_spatial(
    coffee_prod,
    aes(fill = coffee_production_2017)
  ) +
  theme_cropped_map() +
  inset_legend_light("bottom-left",
    title_text_size = 10,
    text_size = 8
  ) +
  theme(legend.title = element_text(face = "bold")) +
  scale_fill_viridis_b()
