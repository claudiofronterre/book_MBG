# PRECOMPUTED DATA FOR CHAPTER 2 ---------------------------------------------

library(rgee)
library(geobounds)
library(terra)
library(wpgpDownloadR)

# Liberia administrative boundaries ----------------------------------------
# -> data/lbr_adm0_geoboundaries.rds
# -> data/lbr_adm1_geoboundaries.rds

liberia_admin0 <- gb_get_adm0("Liberia")
liberia_admin1 <- gb_get_adm1("Liberia")

required_boundary_fields <- c("shapeName", "geometry")
if (!all(required_boundary_fields %in% names(liberia_admin0)) ||
    !all(required_boundary_fields %in% names(liberia_admin1))) {
  stop("The downloaded Liberia boundaries do not contain the expected fields.")
}

attr(liberia_admin0, "geobounds_metadata") <- gb_get_metadata(
  "Liberia", adm_lvl = "adm0"
)
attr(liberia_admin1, "geobounds_metadata") <- gb_get_metadata(
  "Liberia", adm_lvl = "adm1"
)
attr(liberia_admin0, "retrieved_on") <- Sys.Date()
attr(liberia_admin1, "retrieved_on") <- Sys.Date()

saveRDS(liberia_admin0, "data/lbr_adm0_geoboundaries.rds")
saveRDS(liberia_admin1, "data/lbr_adm1_geoboundaries.rds")

# Liberia population from WorldPop ------------------------------------------
# -> data/lbr_pop_100.tif

worldpop_file <- wpgpGetCountryDataset(ISO3 = "LBR",
                                       covariate = "ppp_2014",
                                       destDir = tempdir())
lbr_pop_100 <- rast(worldpop_file)

if (!"lbr_ppp_2014" %in% names(lbr_pop_100)) {
  stop("The downloaded WorldPop raster does not contain 'lbr_ppp_2014'.")
}

lbr_pop_100 <- lbr_pop_100[["lbr_ppp_2014"]]
metags(lbr_pop_100) <- c("provider=WorldPop",
                         "product=Global1 unconstrained population count",
                         "reference_year=2014",
                         "units=estimated people per grid cell",
                         "native_resolution=3 arc-seconds",
                         "licence=CC BY 4.0",
                         paste0("retrieved_on=", Sys.Date()))

writeRaster(lbr_pop_100,
            filename = "data/lbr_pop_100.tif",
            overwrite = TRUE)

message("Done. File written to data/lbr_pop_100.tif")

# Liberia elevation from Google Earth Engine --------------------------------
# -> data/lbr_elevation_srtm.tif

earthengine_project <- Sys.getenv("EARTHENGINE_PROJECT")

if (!nzchar(earthengine_project)) {
  stop(
    "Set EARTHENGINE_PROJECT to your registered Google Cloud project ID.",
    call. = FALSE
  )
}

ee_Initialize(
  project = earthengine_project,
  drive = TRUE,
  quiet = TRUE
)

liberia_ee <- sf_as_ee(liberia_admin0)
elev <- ee$Image("CGIAR/SRTM90_V4")$select("elevation")
elev_liberia <- elev$clip(liberia_ee)

elev_rast <- ee_as_rast(
  image = elev_liberia,
  region = liberia_ee$geometry(),
  via = "drive",
  scale = 1000, # Request an output grid of approximately 1 km
  quiet = TRUE
)

# Mask cells whose centres fall outside Liberia
elev_rast <- mask(elev_rast, liberia_admin0, touches = FALSE)
metags(elev_rast) <- c("provider=NASA/CGIAR",
                       "product=CGIAR/SRTM90_V4",
                       "reference_period=2000-02-11 to 2000-02-22",
                       "units=metres",
                       "native_resolution=90 metres",
                       "processed_resolution=approximately 1 kilometre",
                       "retrieval_source=Google Earth Engine",
                       paste0("retrieved_on=", Sys.Date()))

writeRaster(
  elev_rast,
  filename = "data/lbr_elevation_srtm.tif",
  overwrite = TRUE
)

message("Done. File written to data/lbr_elevation_srtm.tif")
