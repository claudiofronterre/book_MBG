# FIGURES FOR CHAPTER 1 --------------------------------------------------------
#
# The interactive maps of Chapter 1, saved as HTML for the website and as PNG
# for the PDF. mapshot2() needs the webshot2 package and a Chrome browser for
# the PNG files.

library(sf)
library(mapview)
library(RiskMap)
library(dplyr)

# Background layers, with the first shown by default. CARTO, the default of
# mapview, now needs an API key.
mapviewOptions(basemaps = c("OpenStreetMap", "Esri.WorldImagery",
                            "OpenTopoMap"))

# Figure 1.1, in EPSG:32629 as distributed
data("galicia")
map <- mapview(galicia, zcol = "lead", layer.name = "Lead conc.")
mapshot2(map, file = "figures/galicia_ch1.png")
mapshot2(map, url = "figures/galicia_ch1.html")

# Figure 1.2, from a data frame of longitudes and latitudes
data("liberia")
liberia_sf <- st_as_sf(liberia, coords = c("long", "lat"), crs = 4326)
liberia_sf$prev <- liberia_sf$npos / liberia_sf$ntest
map <- mapview(liberia_sf, zcol = "prev", layer.name = "Prevalence")
mapshot2(map, file = "figures/liberia_ch1.png")
mapshot2(map, url = "figures/liberia_ch1.html")

# Figure 1.3, prevalence at each household location of the community survey
data("malkenya")
malkenya_prev <- malkenya |>
  filter(Survey == "community") |>
  group_by(geometry) |>
  summarise(prev = mean(RDT))
map <- mapview(malkenya_prev, zcol = "prev", layer.name = "Prevalence")
mapshot2(map, file = "figures/malkenya_ch1.png")
mapshot2(map, url = "figures/malkenya_ch1.html")

# Figure 1.4, in EPSG:32632 as distributed
data("anopheles")
map <- mapview(anopheles, zcol = "An.gambiae", layer.name = "An. gambiae")
mapshot2(map, file = "figures/anopheles_ch1.png")
mapshot2(map, url = "figures/anopheles_ch1.html")

# Figure 1.5, in EPSG:32633 as distributed
data("italy_sim")
map <- mapview(italy_sim, zcol = "y", layer.name = "Outcome")
mapshot2(map, file = "figures/italy_sim_ch1.png")
mapshot2(map, url = "figures/italy_sim_ch1.html")
