# ---------------------------------------------------------------------------#
# Title: Points - Lizards & Snakes
# Author: Dax Kellie
# Date: 1 Nov 2024
# ---------------------------------------------------------------------------#


# packages
library(galah)
library(dplyr)
library(ggplot2)
library(tidyr)
library(sf)
library(ozmaps)
library(showtext)
library(here)
library(ggpointdensity)
library(MetBrewer)

galah_config(email = "dax.kellie@csiro.au")

# number of squamate records
galah_call() |>
  identify("squamata") |>
  filter(year > 2022) |>
  atlas_counts()

# by data resource
galah_call() |>
  identify("squamata") |>
  filter(year > 2022) |>
  group_by(dataResourceName) |>
  atlas_counts()



# download occurrences
lizards <- galah_call() |>
  identify("squamata") |>
  filter(year > 2022,
         basisOfRecord == "HumanObservation") |>
  apply_profile(ALA) |> # apply set of data quality filters
  atlas_occurrences()

# remove NAs
lizards_filtered <- lizards |>
  drop_na(decimalLongitude, decimalLatitude)

# get map of australia
aus <- ozmap_country |>
  st_transform(crs = 4326) # match ALA data projection


# MAKE MAP

# get font
font_add_google("Barlow")
font <- "barlow"

showtext_auto()

# make caption
caption <- glue::glue("
                **Dataviz by Dax Kellie**<br>
                Data from the Atlas of Living Australia<br>
                Downloaded using the galah package
                ")

met.brewer("Pillement") |> str()

ggplot() +
  geom_sf(data = aus,
          fill = "grey99",
          colour = "grey90") + 
  geom_pointdensity(data = lizards,
                    aes(x = decimalLongitude,
                        y = decimalLatitude),
                    alpha = 0.6) +
  coord_sf(xlim = c(110, 155), ylim = c(-45, -10)) + # limit to within australia
  MetBrewer::scale_colour_met_c("Pillement") +
  guides(colour = guide_coloursteps(title = "Number of overlapping\nobservations")) + 
  labs(title = "Lizard & snake observations (2023-24)",
       subtitle = glue::glue("{nrow(lizards_filtered) |> scales::comma()} records"),
       caption = caption
       ) + 
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(15, 'mm'),
    legend.text = element_text(family = "barlow", colour = "white"),
    legend.title = element_text(family = "barlow", colour = "white"),
    plot.title = ggtext::element_markdown(family = "barlow", size = "23", colour = "white"),
    plot.subtitle = ggtext::element_markdown(family = "barlow", size = "15", colour = "grey80"),
    panel.spacing = unit(c(0,0,0,0), "cm"),
    plot.background = element_rect(fill = "#0B0F1D", colour = NA),
    panel.background = element_rect(fill = "#0B0F1D", colour = NA),
    plot.margin = unit(c(1, 2, 1, 2), unit = "cm"),
    plot.caption = ggtext::element_textbox_simple(colour = "grey80",
                                                  size = 8,
                                                  halign = 1,
                                                  lineheight = 0.25,
                                                  margin = margin(t = 0.75,
                                                                  r = 0.25,
                                                                  unit = "cm"))
  )

# save
showtext_opts(dpi = 350)
ggsave(here::here("plots", "points.png"),
       dpi = 350)
