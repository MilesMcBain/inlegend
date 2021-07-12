test_that("insetting a legend works", {
  coffee_prod <-
    spData::world %>%
    sf::st_as_sf() %>%
    dplyr::left_join(spData::coffee_data,
      by = "name_long"
    )
  coffee_map <-
    ggplot2::ggplot() +
    ggspatial::layer_spatial(
      coffee_prod,
      ggplot2::aes(fill = coffee_production_2017)
    ) +
    theme_cropped_map() +
    inset_legend_light("bottom-left",
      title_text_size = 10,
      text_size = 8
    ) +
    ggplot2::theme(legend.title = ggplot2::element_text(face = "bold")) +
    ggplot2::scale_fill_viridis_b()

  map_image <- tempfile(fileext = ".png")
  on.exit(unlink(map_image))
  ggplot2::ggsave(
    coffee_map,
    device = "png", 
    filename = map_image,
    height = 15,
    width = 24,
    units = "cm")

  expect_snapshot_file(map_image, "coffee.png")
})