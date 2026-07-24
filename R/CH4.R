library(sf)
library(RiskMap)
library(dplyr)

## ------------------------------------------------------------
## 0. Reuse / refit the Chapter 1 Liberia and Anopheles models
##    -> data/fit_Liberia.rds (fit_liberia)
##    -> data/an_fit.rds      (an_fit)
## ------------------------------------------------------------
data(liberia)
data(anopheles)

if (file.exists("data/fit_Liberia.rds")) {
  fit_liberia <- readRDS("data/fit_Liberia.rds")
} else {
  fit_liberia <-
    glgpm(npos ~ log(elevation) + gp(long, lat),
          den = ntest, data = liberia,
          crs = 4326,
          convert_to_crs = 32629,
          family = "binomial")
  saveRDS(fit_liberia, file = "data/fit_Liberia.rds")
}

if (file.exists("data/an_fit.rds")) {
  an_fit <- readRDS("data/an_fit.rds")
} else {
  anopheles_sf <- st_as_sf(anopheles, coords = c("web_x", "web_y"), crs = 3857)
  set.seed(1)
  an_fit <-
    glgpm(An.gambiae ~ elevation + gp(),
          data = anopheles_sf,
          family = "poisson", messages = FALSE)
  saveRDS(an_fit, file = "data/an_fit.rds")
}

## ------------------------------------------------------------
## 1. Liberia prediction grid, elevation covariate,
##    marginal & joint predictions of S(x)
##    -> data/lb_pred_S_m.rds (lb_pred_S_m)
##    -> data/lb_pred_S_j.rds (lb_pred_S_j)
## ------------------------------------------------------------
library(rgeoboundaries)
library(elevatr)

liberia_adm0 <- geoboundaries("liberia", adm_lvl = "adm0")
liberia_adm0 <- st_transform(liberia_adm0, crs = 32629)

liberia_grid <- create_grid(liberia_adm0, spat_res = 5)

liberia_elev <- get_elev_raster(locations = liberia_adm0,
                                z = 5, clip = "locations")

lb_predictors <- data.frame(
  elevation = terra::extract(liberia_elev, st_coordinates(liberia_grid))
)

lb_pred_S_m <- pred_over_grid(fit_liberia,
                              grid_pred = liberia_grid,
                              predictors = lb_predictors, messages = FALSE,
                              type = "marginal")

lb_pred_S_j <- pred_over_grid(fit_liberia,
                              grid_pred = liberia_grid,
                              predictors = lb_predictors, messages = FALSE,
                              type = "joint")

saveRDS(lb_pred_S_m, file = "data/lb_pred_S_m.rds")
saveRDS(lb_pred_S_j, file = "data/lb_pred_S_j.rds")

## ------------------------------------------------------------
## 2. Malaria (malkenya) model with age + elevation
##    -> data/fit_malkenya.rds (fit_malkenya)
## ------------------------------------------------------------
data(malkenya)
malkenya_comm <- malkenya[malkenya$Survey == "community", ]
malkenya_comm1000 <- malkenya_comm[1:1000, ]
malkenya_comm1000 <- st_as_sf(malkenya_comm1000, coords = c("Long", "Lat"),
                              crs = 4326)
malkenya_comm1000 <- st_transform(malkenya_comm1000, crs = 32736)

fit_malkenya <- glgpm(RDT ~ Age + pmax(Age - 15, 0) +
                        pmax(Age - 40, 0) + elevation +
                        gp(),
                      data = malkenya_comm1000,
                      family = "binomial")

saveRDS(fit_malkenya, file = "data/fit_malkenya.rds")

## ------------------------------------------------------------
## 3. Malaria predictions: fixed age 15 & general-population age
##    -> data/pred_age15.rds    (pred_age15)
##    -> data/pred_aver_pop.rds (pred_aver_pop)
## ------------------------------------------------------------
shp_ch <- convex_hull_sf(malkenya_comm1000)
ken_grid <- create_grid(shp_ch, spat_res = 0.5)

ken_elev <- get_elev_raster(locations = shp_ch,
                            z = 9, clip = "locations")

ken_predictors <- data.frame(
  elevation = terra::extract(ken_elev, st_coordinates(ken_grid)),
  Age = 15
)

pred_ken_S <- pred_over_grid(fit_malkenya, grid_pred = ken_grid,
                             predictors = ken_predictors)

pred_age15 <-
  pred_target_grid(pred_ken_S,
                   f_target = list(prev = function(lp) exp(lp) / (1 + exp(lp))),
                   pd_summary = list(mean = function(Tx) mean(Tx)))

saveRDS(pred_age15, file = "data/pred_age15.rds")

# General-population prediction: integrate out age via resampling
n_sim <- ncol(pred_age15$lp_samples)
n_pred <- nrow(ken_predictors)

pred_ken_S_i <- pred_ken_S
pred_ken_S_i$mu_pred <- matrix(NA, nrow = n_pred, ncol = n_sim)

for (i in 1:n_sim) {
  ken_predictors$Age <- sample(malkenya_comm1000$Age, n_pred, replace = TRUE)
  pred_ken_S_i$mu_pred[, i] <- update_predictors(pred_ken_S, ken_predictors)$mu_pred
}

pred_aver_pop <- pred_target_grid(
  pred_ken_S_i,
  f_target = list(prev = function(lp) exp(lp) / (1 + exp(lp)))
)

saveRDS(pred_aver_pop, file = "data/pred_aver_pop.rds")

## ------------------------------------------------------------
## 4. Areal-level (admin level 1) prevalence predictions for Liberia
##    -> data/pred_shp.rds   (pred_shp,   unweighted)
##    -> data/pred_shp_w.rds (pred_shp_w, population weighted)
## ------------------------------------------------------------
lb_adm1 <- geoboundaries(country = "Liberia", adm_lvl = "adm1")

pred_shp <- pred_target_shp(lb_pred_S_j, shp = lb_adm1,
                            shp_target = function(Tx) mean(Tx),
                            f_target = list(prev =
                                              function(lp) exp(lp) / (1 + exp(lp))),
                            pd_summary = list(mean = mean),
                            col_names = "shapeName")

saveRDS(pred_shp, file = "data/pred_shp.rds")

# Population-density weights from WorldPop
library(wpgpDownloadR)
library(terra)

lbr_url <- wpgpGetCountryDataset(ISO3 = "LBR", covariate = "ppp_2014")
lbr_pop <- rast(lbr_url)
lbr_pop <- project(lbr_pop, "EPSG:32629")

weights_pred <- extract(lbr_pop, st_coordinates(liberia_grid))$lbr_ppp_2014

pred_shp_w <- pred_target_shp(lb_pred_S_j, shp = lb_adm1,
                              shp_target = function(Tx) sum(Tx),
                              f_target = list(prev =
                                                function(lp) exp(lp) / (1 + exp(lp))),
                              pd_summary = list(mean = mean),
                              weights = weights_pred,
                              standardize_weights = TRUE,
                              col_names = "shapeName")

saveRDS(pred_shp_w, file = "data/pred_shp_w.rds")

## ------------------------------------------------------------
## 5. Cross-validation: fit M0/M1, run assess_pp with regularized
##    subsampling and clustering splits (AnPIT + CRPS + SCRPS)
##    -> data/regularized.rds (regularized)
##    -> data/cluster.rds     (cluster)
## ------------------------------------------------------------
set.seed(123)
M0_fit <-
  glgpm(npos ~ gp(long, lat, nugget = NULL),
        den = ntest, data = liberia,
        crs = 4326,
        convert_to_crs = 32629,
        family = "binomial", messages = FALSE)

M1_fit <-
  glgpm(npos ~ elevation + pmax(elevation - 150, 0) +
          gp(long, lat, nugget = NULL),
        den = ntest, data = liberia,
        crs = 4326,
        convert_to_crs = 32629,
        family = "binomial", messages = FALSE)

# Default which_metric = c("AnPIT", "CRSP", "SCRPS") computes all
# three diagnostics in one pass, which is what is cached here.
regularized <-
  assess_pp(list(M0 = M0_fit, M1 = M1_fit),
            method = "regularized", min_dist = 20,
            n_size = 9, iter = 10)

saveRDS(regularized, file = "data/regularized.rds")

cluster <-
  assess_pp(list(M0 = M0_fit, M1 = M1_fit),
            method = "cluster", fold = 10)

saveRDS(cluster, file = "data/cluster.rds")

## ------------------------------------------------------------
## 6. "True model" for the simulation study
##    -> data/fit_sp_lib.rds (true_model)
## ------------------------------------------------------------
set.seed(123)
true_model <- glgpm(npos ~ elevation + pmax(elevation - 150, 0) +
                      gp(long, lat),
                    den = ntest,
                    crs = 4326,
                    convert_to_crs = 32629,
                    family = "binomial",
                    data = liberia)

saveRDS(true_model, file = "data/fit_sp_lib.rds")

## ------------------------------------------------------------
## 7. Simulation study: surf_sim() + assess_sim() for both a
##    pixel-level MSE assessment and an areal-level classification
##    assessment
##    -> data/sim_grid_mse.rds (res_sim_grid)
##    -> data/sim_area_cl.rds  (res_sim_area)
## ------------------------------------------------------------
shp <- geoboundaries(country = "liberia", adm_lvl = "adm0")
shp <- st_transform(shp, crs = 32629)

sim_pred_grid <- create_grid(shp, spat_res = 5)
sim_pred_grid <- st_as_sf(sim_pred_grid)
sim_liberia_elev <- get_elev_raster(locations = shp,
                                    z = 5, clip = "locations")
sim_pred_grid$elevation <- terra::extract(sim_liberia_elev,
                                          st_coordinates(sim_pred_grid))

sampling_f_lib <- function() {
  coords_sf <- st_as_sf(liberia[, c("ntest", "long", "lat")],
                        coords = c("long", "lat"), crs = 4326)
  coords_sf <- st_transform(coords_sf, 32629)
  coords_sf$units_m <- coords_sf$ntest
  return(coords_sf)
}

lib_surf_sim <- surf_sim(n_sim = 200,
                         pred_grid = sim_pred_grid,
                         formula = ~ elevation + pmax(elevation - 150, 0) +
                           gp(long,lat),
                         sampling_f = sampling_f_lib,
                         family = "binomial",
                         par0 = coef(true_model))

# 7a. Pixel-level MSE assessment (M_T vs M_C)
res_sim_grid <- assess_sim(lib_surf_sim,
                           models = list(M_T = ~ elevation +
                                           pmax(elevation - 150, 0) +
                                           gp(long,lat),
                                         M_C = ~ gp(long, lat)),
                           f_grid_target = function(x) 1/(1+exp(-x)),
                           pred_objective = "mse",
                           spatial_scale = "grid")

saveRDS(res_sim_grid, file = "data/sim_grid_mse.rds")

# 7b. Areal-level classification assessment (admin level 1)
shp_adm <- geoboundaries(country = "liberia", adm_lvl = "adm1")
shp_adm <- st_transform(shp_adm, crs = 32629)

res_sim_area <- assess_sim(lib_surf_sim,
                           models = list(M_T = ~ elevation +
                                           pmax(elevation - 150, 0) +
                                           gp(long, lat),
                                         M_C = ~ gp(long, lat)),
                           f_grid_target = function(x) exp(x) / (1 + exp(x)),
                           f_area_target = mean,
                           pred_objective = "classify", shp = shp_adm,
                           categories = c(0, 0.2, 1),
                           spatial_scale = "area")

saveRDS(res_sim_area, file = "data/sim_area_cl.rds")

## ------------------------------------------------------------
message("Done. All files written to ./data:")
print(list.files("data", pattern = "\\.(rds|RData)$"))