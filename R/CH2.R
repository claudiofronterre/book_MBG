# PRECOMPUTED DATA FOR CHAPTER 2 ---------------------------------------------

library(rgee)
library(rgeoboundaries)
library(terra)

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

liberia_admin0 <- gb_adm0("Liberia")
liberia_ee <- sf_as_ee(liberia_admin0)
elev <- ee$Image("CGIAR/SRTM90_V4")
elev_liberia <- elev$clip(liberia_ee)

elev_rast <- ee_as_rast(
  image = elev_liberia,
  region = liberia_ee$geometry(),
  via = "drive",
  scale = 1000,
  quiet = TRUE
)

elev_rast[elev_rast == 0] <- NA

writeRaster(
  elev_rast,
  filename = "data/lbr_elevation_srtm.tif",
  overwrite = TRUE
)

message("Done. File written to data/lbr_elevation_srtm.tif")
