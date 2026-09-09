# PRECOMPUTED DATA FOR CHAPTER 2 ---------------------------------------------

library(rgee)
library(geobounds)
library(terra)

# Liberia administrative boundaries ----------------------------------------
# -> data/lbr_adm0_geoboundaries.rds
# -> data/lbr_adm1_geoboundaries.rds

liberia_admin0 <- gb_get_adm0("Liberia")
liberia_admin1 <- gb_get_adm1("Liberia")

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
elev <- ee$Image("CGIAR/SRTM90_V4")
elev_liberia <- elev$clip(liberia_ee)

elev_rast <- ee_as_rast(
  image = elev_liberia,
  region = liberia_ee$geometry(),
  via = "drive",
  scale = 1000,
  quiet = TRUE
)

# Mask cells whose centres fall outside Liberia while preserving
# valid zero-metre elevations
liberia_mask <- rasterize(vect(liberia_admin0), elev_rast, field = 1)
elev_rast <- mask(elev_rast, liberia_mask)

writeRaster(
  elev_rast,
  filename = "data/lbr_elevation_srtm.tif",
  overwrite = TRUE
)

message("Done. File written to data/lbr_elevation_srtm.tif")
