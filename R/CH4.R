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
liberia <- st_as_sf(liberia, coords = c("long", "lat"), crs = 4326)

if (file.exists("data/fit_Liberia.rds")) {
  fit_liberia <- readRDS("data/fit_Liberia.rds")
} else {
  fit_liberia <- glgpm(npos ~ log(elevation) + gp(),
                       den = ntest, data = liberia,
                       convert_to_crs = 32629,
                       family = "binomial")
  saveRDS(fit_liberia, file = "data/fit_Liberia.rds")
}

if (file.exists("data/an_fit.rds")) {
  an_fit <- readRDS("data/an_fit.rds")
} else {
  set.seed(1)
  an_fit <- glgpm(An.gambiae ~ elevation + gp(),
                  data = anopheles,
                  family = "poisson", messages = FALSE)
  saveRDS(an_fit, file = "data/an_fit.rds")
}

## ------------------------------------------------------------
## Anopheles prediction grid and joint prediction
##    -> data/an_grid.rds             (an_grid)
##    -> data/an_weights.rds          (an_weights)
##    -> data/pred_an_S.rds           (pred_an_S)
##    -> data/pred_n_mosq_grid.rds    (pred_n_mosq_grid)
##    -> data/pred_n_mosq_shp.rds     (pred_n_mosq_shp)
## ------------------------------------------------------------
library(elevatr)

an_shp <- create_convex_hull(an_fit$data_sf)
an_grid <- create_grid(an_shp, spat_res = 2)
an_elev <- get_elev_raster(locations = an_shp, z = 9, clip = "locations")
an_predictors <- data.frame(
  elevation = terra::extract(an_elev, st_coordinates(an_grid))
)

pred_an_S <- setup_prediction(an_fit,
                              grid_pred = an_grid,
                              predictors = an_predictors,
                              type = "joint")

pred_n_mosq_grid <- predict_grid_target(
  pred_an_S,
  f_target = list(n_mosq = function(lp) exp(lp)),
  pd_summary = list(mean = function(Tx) mean(Tx))
)

an_weights <- 1 * (an_predictors$elevation > 390 &
                     an_predictors$elevation < 837)

pred_n_mosq_shp <- predict_areal_target(
  pred_an_S,
  shp = an_shp,
  weights = an_weights,
  shp_target = sum,
  f_target = list(n_mosq = function(lp) exp(lp)),
  pd_summary = list(
    mean = function(Tx) mean(Tx),
    q025 = function(Tx) quantile(Tx, 0.025),
    q075 = function(Tx) quantile(Tx, 0.975)
  )
)

saveRDS(an_grid, file = "data/an_grid.rds")
saveRDS(an_weights, file = "data/an_weights.rds")
saveRDS(pred_an_S, file = "data/pred_an_S.rds")
saveRDS(pred_n_mosq_grid, file = "data/pred_n_mosq_grid.rds")
saveRDS(pred_n_mosq_shp, file = "data/pred_n_mosq_shp.rds")

## ------------------------------------------------------------
## 1. Liberia prediction grid, elevation covariate,
##    marginal & joint predictions of S(x)
##    -> data/lb_pred_S_m.rds (lb_pred_S_m)
##    -> data/lb_pred_S_j.rds (lb_pred_S_j)
## ------------------------------------------------------------
library(elevatr)

liberia_adm0 <- readRDS("data/lbr_adm0_geoboundaries.rds")
liberia_adm0 <- st_transform(liberia_adm0, crs = 32629)

liberia_grid <- create_grid(liberia_adm0, spat_res = 5)

liberia_elev_path <- "data/lbr_elevation_ch4.rds"
if (file.exists(liberia_elev_path)) {
  liberia_elev <- readRDS(liberia_elev_path)
} else {
  liberia_elev <- get_elev_raster(locations = liberia_adm0,
                                  z = 5, clip = "locations")
  saveRDS(liberia_elev, file = liberia_elev_path)
}

lb_predictors <- data.frame(
  elevation = terra::extract(liberia_elev, st_coordinates(liberia_grid))
)

lb_pred_S_m <- setup_prediction(fit_liberia,
                                grid_pred = liberia_grid,
                                predictors = lb_predictors, messages = FALSE,
                                type = "marginal")

lb_pred_S_j <- setup_prediction(fit_liberia,
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
shp_ch <- create_convex_hull(malkenya_comm1000)
ken_grid <- create_grid(shp_ch, spat_res = 0.5)

ken_elev <- get_elev_raster(locations = shp_ch,
                            z = 9, clip = "locations")

ken_predictors <- data.frame(
  elevation = terra::extract(ken_elev, st_coordinates(ken_grid)),
  Age = 15
)

pred_ken_S <- setup_prediction(fit_malkenya, grid_pred = ken_grid,
                               predictors = ken_predictors)

pred_age15 <-
  predict_grid_target(pred_ken_S,
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

pred_aver_pop <- predict_grid_target(
  pred_ken_S_i,
  f_target = list(prev = function(lp) exp(lp) / (1 + exp(lp)))
)

saveRDS(pred_aver_pop, file = "data/pred_aver_pop.rds")

## ------------------------------------------------------------
## 4. Areal-level (admin level 1) prevalence predictions for Liberia
##    -> data/pred_shp.rds   (pred_shp,   unweighted)
##    -> data/pred_shp_w.rds (pred_shp_w, population weighted)
## ------------------------------------------------------------
lb_adm1 <- readRDS("data/lbr_adm1_geoboundaries.rds")

pred_shp <- predict_areal_target(lb_pred_S_j, shp = lb_adm1,
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

pred_shp_w <- predict_areal_target(lb_pred_S_j, shp = lb_adm1,
                                   shp_target = function(Tx) sum(Tx),
                                   f_target = list(prev =
                                                     function(lp) exp(lp) / (1 + exp(lp))),
                                   pd_summary = list(mean = mean),
                                   weights = weights_pred,
                                   standardize_weights = TRUE,
                                   col_names = "shapeName")

saveRDS(pred_shp_w, file = "data/pred_shp_w.rds")

## ------------------------------------------------------------
## 5. Cross-validation: fit M0/M1, run assess_prediction with regularized
##    subsampling and clustering splits (AnPIT + CRPS + SCRPS)
##    -> data/regularized.rds (regularized)
##    -> data/cluster.rds     (cluster)
## ------------------------------------------------------------
set.seed(123)
M0_fit <- glgpm(npos ~ gp(),
                den = ntest, data = liberia,
                convert_to_crs = 32629,
                family = "binomial", messages = FALSE)

M1_fit <- glgpm(npos ~ elevation + pmax(elevation - 150, 0) + gp(),
                den = ntest, data = liberia,
                convert_to_crs = 32629,
                family = "binomial", messages = FALSE)

# Default which_metric = c("AnPIT", "CRSP", "SCRPS") computes all
# three diagnostics in one pass, which is what is cached here.
regularized <-
  assess_prediction(list(M0 = M0_fit, M1 = M1_fit),
                    method = "regularized", min_dist = 20,
                    n_size = 9, iter = 10, messages = FALSE)

saveRDS(regularized, file = "data/regularized.rds")

cluster <-
  assess_prediction(list(M0 = M0_fit, M1 = M1_fit),
                    method = "cluster", fold = 10, messages = FALSE)

saveRDS(cluster, file = "data/cluster.rds")

## ------------------------------------------------------------
## 6. "True model" for the simulation study
##    -> data/fit_sp_lib.rds (true_model)
## ------------------------------------------------------------
set.seed(123)
true_model <- glgpm(npos ~ elevation + pmax(elevation - 150, 0) + gp(),
                    den = ntest,
                    convert_to_crs = 32629,
                    family = "binomial",
                    data = liberia)

saveRDS(true_model, file = "data/fit_sp_lib.rds")

## ------------------------------------------------------------
## 7. Simulation study: simulate_surface() + assess_simulation() for both a
##    pixel-level MSE assessment and an areal-level classification
##    assessment
##    -> data/lib_surf_sim.rds (lib_surf_sim)
##    -> data/sim_grid_mse.rds (res_sim_grid)
##    -> data/sim_area_cl.rds  (res_sim_area)
## ------------------------------------------------------------
shp <- readRDS("data/lbr_adm0_geoboundaries.rds")
shp <- st_transform(shp, crs = 32629)

sim_pred_grid <- create_grid(shp, spat_res = 5)
sim_pred_grid <- st_as_sf(sim_pred_grid)
sim_pred_grid$elevation <- terra::extract(liberia_elev,
                                          st_coordinates(sim_pred_grid))

sampling_f_lib <- function() {
  coords_sf <- liberia[, "ntest"]
  coords_sf <- st_transform(coords_sf, 32629)
  coords_sf$units_m <- coords_sf$ntest
  return(coords_sf)
}

set.seed(123)
lib_surf_sim <- simulate_surface(n_sim = 200,
                                 pred_grid = sim_pred_grid,
                                 formula = ~ elevation +
                                   pmax(elevation - 150, 0) + gp(kappa = 0.5),
                                 sampling_f = sampling_f_lib,
                                 family = "binomial",
                                 par0 = coef(true_model))

saveRDS(lib_surf_sim, file = "data/lib_surf_sim.rds")

# 7a. Pixel-level MSE assessment (M_T vs M_C)
res_sim_grid <- assess_simulation(lib_surf_sim,
                                  models = list(M_T = ~ elevation +
                                                  pmax(elevation - 150, 0) + gp(),
                                                M_C = ~ gp()),
                                  f_grid_target = function(x) 1 / (1 + exp(-x)),
                                  pred_objective = "mse",
                                  spatial_scale = "grid")

saveRDS(res_sim_grid, file = "data/sim_grid_mse.rds")

# 7b. Areal-level classification assessment (admin level 1)
shp_adm <- readRDS("data/lbr_adm1_geoboundaries.rds")
shp_adm <- st_transform(shp_adm, crs = 32629)

res_sim_area <- assess_simulation(lib_surf_sim,
                                  models = list(M_T = ~ elevation +
                                                  pmax(elevation - 150, 0) + gp(),
                                                M_C = ~ gp()),
                                  f_grid_target = function(x) exp(x) / (1 + exp(x)),
                                  f_area_target = mean,
                                  pred_objective = "classify", shp = shp_adm,
                                  categories = c(0, 0.2, 1),
                                  spatial_scale = "area")

saveRDS(res_sim_area, file = "data/sim_area_cl.rds")

## ------------------------------------------------------------
message("Done. All files written to ./data:")
print(list.files("data", pattern = "\\.(rds|RData)$"))
