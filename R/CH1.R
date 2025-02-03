# LOAD REQUIRED PACKAGES AND FUNCTIONS -----------------------------------------
if (!require("pacman")) install.packages("pacman")
pkgs = c("sf", "mapview", "RiskMap", "dplyr") # package names
pacman::p_load(pkgs, character.only = T)

# FIGURES FOR CHAPTER 1 --------------------------------------------------------

# Figure 1.1
data("galicia")
galicia_sf <- st_as_sf(galicia, coords = c("x", "y"), crs = 32629)
map <- mapview(galicia_sf, zcol = "lead", layer.name = "Lead conc.") 
mapshot2(map, file = "figures/galicia_ch1.png")
mapshot2(map, url = "figures/galicia_ch1.html")

# Figure 1.2
data("liberia")
liberia_sf <- st_as_sf(liberia, coords = c("long", "lat"), crs = 4326)
liberia_sf$prev <- liberia_sf$npos / liberia_sf$ntest
map <- mapview(liberia_sf, zcol = "prev", layer.name = "Prevalence") 
mapshot2(map, file = "figures/liberia_ch1.png")
mapshot2(map, url = "figures/liberia_ch1.html")

# Figure 1.3
data("malkenya")

malkenya_sf <- malkenya |> 
  filter(Survey == "community") |> 
  group_by(Long, Lat) |> 
  summarise(prev = mean(RDT)) |> 
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)

map <- mapview(malkenya_sf, zcol = "prev", layer.name = "Prevalence") 
mapshot2(map, file = "figures/malkenya_ch1.png")
mapshot2(map, url = "figures/malkenya_ch1.html")

# Figure 1.4
data("anopheles")
anopheles_sf <- st_as_sf(anopheles, coords = c("web_x", "web_y"), crs = 3857)
map <- mapview(anopheles_sf, zcol = "An.gambiae", layer.name = "An. gambiae") 
mapshot2(map, file = "figures/anopheles_ch1.png")
mapshot2(map, url = "figures/anopheles_ch1.html")

# Figure 1.5
data(italy_sim)
italy_sim_sf <- st_as_sf(italy_sim, coords = c("x1", "x2"), crs = 32634)
map <- mapview(italy_sim_sf, zcol = "y", layer.name = "Outcome") 
mapshot2(map, file = "figures/italy_sim_ch1.png")
mapshot2(map, url = "figures/italy_sim_ch1.html")
