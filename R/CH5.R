library(sf)
library(RiskMap)
library(dplyr)
library(ggplot2)
library(lme4)

dir.create("data", showWarnings = FALSE)

## ============================================================
## PART A: Ghana malnutrition (stunting / underweight) case study
## ============================================================

data(malnutrition)

malnutrition <- malnutrition %>%
  group_by(lng, lat) %>%
  mutate(loc_id = cur_group_id()) %>%
  ungroup()

# Remove missing WAZ (per the chapter's own eval=FALSE chunk)
malnutrition <- malnutrition[complete.cases(malnutrition[, "WAZ"]), ]

malnutrition_sf <- st_as_sf(malnutrition, coords = c("lng", "lat"), crs = 4326)
malnutrition_sf <- st_transform(malnutrition_sf, crs = propose_utm(malnutrition_sf))

## --- A1. Maximum likelihood fits for HAZ and WAZ ---
## (the chapter's HAZ formula chunk uses `pmax(age - 1, 0)` for
## both haz_fit and waz_fit, which looks like a copy-paste typo
## against the stated change points of 2 months / 1 month
## discussed in the text; reproduced faithfully as written)
haz_fit <-
  glgpm(HAZ ~ age + pmax(age - 1, 0) + wealth + gp(),
        data = malnutrition_sf, family = "gaussian")

waz_fit <-
  glgpm(HAZ ~ age + pmax(age - 1, 0) + wealth + gp(),
        data = malnutrition_sf, family = "gaussian")

saveRDS(haz_fit, file = "data/haz_fit.rds")
saveRDS(waz_fit, file = "data/waz_fit.rds")

## --- A2. Predictive grids for stunting / underweight prevalence ---
library(rgeoboundaries)

ghana <- geoboundaries(country = "Ghana", adm_lvl = "adm0")
ghana <- st_transform(ghana, st_crs(malnutrition_sf))

ghana_grid <- create_grid(ghana, spat_res = 10)
n_pred <- nrow(st_coordinates(ghana_grid))

haz_pred_grid <- pred_over_grid(haz_fit,
                                grid_pred = ghana_grid,
                                predictors = data.frame(
                                  age = rep(2, n_pred),      # 2 months for HAZ
                                  wealth = rep(1, n_pred)
                                ))

waz_pred_grid <- pred_over_grid(waz_fit,
                                grid_pred = ghana_grid,
                                predictors = data.frame(
                                  age = rep(1, n_pred),      # 1 month for WAZ
                                  wealth = rep(1, n_pred)
                                ))

sd_ind_haz <- sqrt(coef(haz_fit)$sigma2_me)
stunting_prev <-
  pred_target_grid(haz_pred_grid,
                   f_target = list(prev = function(x) pnorm((-2 - x) / sd_ind_haz)),
                   pd_summary = list(mean = mean,
                                     cv = function(x) sd(x) / mean(x)))

sd_ind_waz <- sqrt(coef(waz_fit)$sigma2_me)
underw_prev <-
  pred_target_grid(waz_pred_grid,
                   f_target = list(prev = function(x) pnorm((-2 - x) / sd_ind_waz)),
                   pd_summary = list(mean = mean,
                                     cv = function(x) sd(x) / mean(x)))

# NB: the chapter's own setup chunk reads this object back from
# "data/stuntin_prev.rds" (missing a "g"); saved under that exact
# filename here even though the in-memory object is spelled
# `stunting_prev`.
saveRDS(stunting_prev, file = "data/stuntin_prev.rds")
saveRDS(underw_prev, file = "data/underw_prev.rds")

## --- A3. Cross-validation (PIT) for HAZ and WAZ ---
assess_haz <-
  assess_pp(list(HAZ = haz_fit),
            n_size = 100,
            min_dist = 5,
            iter = 10,
            method = "regularized")

assess_waz <-
  assess_pp(list(WAZ = waz_fit),
            n_size = 100,
            min_dist = 5,
            iter = 10,
            method = "regularized")

saveRDS(assess_haz, file = "data/assess_haz.rds")
saveRDS(assess_waz, file = "data/assess_waz.rds")

## ============================================================
## PART B: Malawi malaria case study
## ============================================================

library(malariaAtlas)
library(tidyr)
library(rgee)
library(terra)

ee_Initialize()

## --- B1. Boundary + PfPR survey data ---
mlw_admin0_sf <- geoboundaries(country = "Malawi", adm_lvl = "adm0")
mlw_admin0_ee <- sf_as_ee(mlw_admin0_sf)

saveRDS(mlw_admin0_sf, file = "data/mlw_admin0_sf.rds")

mlw_pfpr <- getPR(country = "Malawi", species = "Pf") %>%
  filter(year_start <= 2006 & year_end >= 2006) %>%
  filter(!is.na(longitude) & !is.na(latitude))

saveRDS(mlw_pfpr, file = "data/mlw_pfpr.rds")

mlw_sf <- st_as_sf(mlw_pfpr, coords = c("longitude", "latitude"), crs = 4326)
mlw_crs <- propose_utm(mlw_sf)
mlw_sf <- st_transform(mlw_sf, crs = mlw_crs)
mlw_sf <- mlw_sf[, colSums(!is.na(mlw_sf)) > 0]

# Cache the PRE-covariate-extraction version (see assumption 1 above)
saveRDS(mlw_sf, file = "data/mlw_sf.rds")

## --- B2. Environmental covariates from Google Earth Engine ---
scale_m <- 1000

precip <- ee$ImageCollection("UCSB-CHG/CHIRPS/DAILY")$
  filterDate("2006-01-01", "2006-12-31")$
  mean()$rename("precip")$clip(mlw_admin0_ee)$toFloat()

lst <- ee$ImageCollection("MODIS/061/MOD11A2")$
  filterDate("2006-01-01", "2006-12-31")$
  select("LST_Day_1km")$mean()$
  multiply(0.02)$subtract(273.15)$rename("lst_c")$
  clip(mlw_admin0_ee)$toFloat()

ndvi <- ee$ImageCollection("MODIS/061/MOD13A2")$
  filterDate("2006-01-01", "2006-12-31")$
  select("NDVI")$mean()$
  multiply(0.0001)$rename("ndvi")$
  clip(mlw_admin0_ee)$toFloat()

elev <- ee$Image("USGS/SRTMGL1_003")$
  rename("elev")$clip(mlw_admin0_ee)$toFloat()

era5 <- ee$ImageCollection("ECMWF/ERA5_LAND/HOURLY")$
  filterDate("2006-01-01", "2006-12-31")$
  select(c("dewpoint_temperature_2m", "temperature_2m"))$
  mean()$clip(mlw_admin0_ee)

td <- era5$select("dewpoint_temperature_2m")$subtract(273.15)
t  <- era5$select("temperature_2m")$subtract(273.15)

rh <- td$expression(
  "100 * (exp((17.625 * TD)/(243.04 + TD)) / exp((17.625 * T)/(243.04 + T)))",
  list(TD = td, T = t)
)$rename("humidity")$toFloat()

urban_raw <- ee$ImageCollection("MODIS/061/MCD12Q1")$
  filterDate("2006-01-01", "2006-12-31")$
  first()$select("LC_Type1")$clip(mlw_admin0_ee)

built_up <- urban_raw$eq(13)$rename("built_up")$toFloat()

covariates <- precip$
  addBands(lst)$
  addBands(ndvi)$
  addBands(elev)$
  addBands(rh)$
  addBands(built_up)

out_file <- file.path("data", "malawi_covariates_2006.tif")
region <- mlw_admin0_ee$geometry()$bounds()

rgee::ee_as_rast(
  image = covariates,
  region = region,
  scale = scale_m,
  dsn = out_file,
  via = "drive"
)

r_covs <- rast(out_file)

## --- B3. Extract covariates + PC1, then fit both models ---
vals <- terra::extract(r_covs, vect(mlw_sf))
mlw_sf_model <- bind_cols(mlw_sf, vals[, -1])  # local, covariate-augmented copy

# PC1 loadings as reported in the chapter's loadings table
pc1_weights <- c(-0.480, 0.596, -0.338, -0.391, -0.385)

r_stack_std <- scale(r_covs[[c("precip", "lst_c", "ndvi", "elev", "humidity")]])
pc1_rast <- sum(r_stack_std * pc1_weights)
names(pc1_rast) <- "PC1"

pc1_vals <- terra::extract(pc1_rast, vect(mlw_sf_model))
mlw_sf_model$PC1 <- pc1_vals[, 2]

mod_all_cov <-
  glgpm(positive ~
          precip +
          ndvi +
          lst_c + pmax(lst_c - 33, 0) +
          elev + pmax(elev - 400, 0) +
          humidity + pmax(humidity - 65, 0) +
          built_up + gp(),
        den = examined,
        data = mlw_sf_model,
        family = "binomial")

mod_pca <-
  glgpm(positive ~
          PC1 + pmax(PC1 - 0.75, 0) +
          built_up + gp(),
        den = examined,
        data = mlw_sf_model,
        family = "binomial")

saveRDS(mod_all_cov, file = "data/mod_all_cov.rds")
saveRDS(mod_pca, file = "data/mod_pca.rds")

## --- B4. Grid predictions for both models ---
grid_mlw <- create_grid(mlw_admin0_sf, spat_res = 5)

r_covs_p   <- terra::project(r_covs, paste0("epsg:", mlw_crs))
pc1_rast_p <- terra::project(pc1_rast, paste0("epsg:", mlw_crs))

predictors <- cbind(terra::extract(r_covs_p,  st_coordinates(grid_mlw)),
                    terra::extract(pc1_rast_p, st_coordinates(grid_mlw)))

pred_all_cov <- pred_over_grid(mod_all_cov,
                               grid_pred = grid_mlw,
                               predictors = predictors)

pred_pca <- pred_over_grid(mod_pca,
                           grid_pred = grid_mlw,
                           predictors = predictors)

pred_all_cov_prev <- pred_target_grid(pred_all_cov,
                                      f_target = list(prev = function(x) exp(x) / (1 + exp(x))))

pred_pca_prev <- pred_target_grid(pred_pca,
                                  f_target = list(prev = function(x) exp(x) / (1 + exp(x))))

saveRDS(pred_all_cov_prev, file = "data/pred_all_cov_prev.rds")
saveRDS(pred_pca_prev, file = "data/pred_pca_prev.rds")

## --- B5. Geographically stratified cross-validation ---
assess_pred_mlw <-
  assess_pp(list(all_cov = mod_all_cov,
                 pca = mod_pca),
            method = "cluster",
            which_metric = c("AnPIT", "CRPS"),
            iter = 1,
            fold = 3)

saveRDS(assess_pred_mlw, file = "data/assess_pred_mlw.rds")

## ============================================================
## PART C: West Nile virus (Sacramento Metropolitan Area) case study
## ============================================================

data(abund_sma)

abund_sma <- abund_sma %>%
  mutate(trap_group = case_when(
    trap_type %in% c("BACKPACK", "LCKR", "NJLT", "OVI") ~ "low yeild",
    trap_type %in% c("GRVD", "MMT") ~ "moderate yeild",
    trap_type %in% c("CO2", "BGSENT") ~ "high yield"
  ))

abund_sma$year <- as.numeric(substr(abund_sma$date, 1, 4))
abund_sma <- st_as_sf(abund_sma, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
abund_sma$loc <- 1:nrow(abund_sma)

## --- C1. Poisson mixed model for abundance ---
glmer_wvn <- glmer(total_females ~ -1 + trap_group + scale(year) +
                     offset(log(trap_nights)) + (1 | loc),
                   data = abund_sma, family = poisson,
                   nAGQ = 100)

## --- C2. Empirical variogram of the random effects ---
wnv_summary <- abund_sma
wnv_summary$Z_hat <- ranef(glmer_wvn)$loc[, 1]

variogram_wnv <- variogram(data = wnv_summary,
                           variable = "Z_hat",
                           n_permutations = 1000,
                           scale_to_km = TRUE,
                           breaks = seq(0, 10, length = 15))

saveRDS(variogram_wnv, file = "data/variogram_wnv.rds")

## --- C3. Predictive samples of abundance (custom function from
##         @sec-sim-re, reproduced here since it is not part of
##         the RiskMap package) ---
simulate_random_effects <- function(model, nsim = 1000, grid_range = c(-10, 10), grid_length = 5000) {
  beta_hat <- fixef(model)
  sigma2_hat <- as.numeric(VarCorr(model)[[1]][1])
  
  mf <- model@frame
  group_var <- names(ranef(model))[1]
  group_ids <- mf[[group_var]]
  unique_groups <- unique(group_ids)
  ngroups <- length(unique_groups)
  
  D <- model.matrix(model)
  
  u_samples <- matrix(NA, nrow = ngroups, ncol = nsim)
  rownames(u_samples) <- unique_groups
  
  log_pd_u <- function(u, y_j, D_j, beta_hat, sigma2_hat) {
    eta <- as.vector(D_j %*% beta_hat) + u
    loglik <- sum(dpois(y_j, lambda = exp(eta), log = TRUE))
    logprior <- dnorm(u, mean = 0, sd = sqrt(sigma2_hat), log = TRUE)
    return(loglik + logprior)
  }
  
  for (j in seq_along(unique_groups)) {
    gid <- unique_groups[j]
    idx <- which(group_ids == gid)
    y_j <- model@resp$y[idx]
    D_j <- D[idx, , drop = FALSE]
    
    u_grid <- seq(grid_range[1], grid_range[2], length.out = grid_length)
    logdens <- sapply(u_grid, log_pd_u, y_j = y_j, D_j = D_j,
                      beta_hat = beta_hat, sigma2_hat = sigma2_hat)
    logdens <- logdens - max(logdens)
    dens <- exp(logdens)
    pdf_u <- dens / sum(dens)
    cdf_u <- cumsum(pdf_u)
    cdf_u <- cdf_u / max(cdf_u)
    
    keep <- !duplicated(cdf_u)
    u_samples[j, ] <- approx(cdf_u[keep], u_grid[keep], xout = runif(nsim), rule = 2)$y
  }
  
  return(u_samples)
}

n_samples <- 1000
samples_z <- simulate_random_effects(glmer_wvn, nsim = n_samples)

# Fixed part of the linear predictor (no random effects), matching
# the model.matrix/offset used in glmer_wvn; row order of
# samples_z matches abund_sma row order because `loc` is 1:n
D_ab   <- model.matrix(glmer_wvn)
beta_ab <- fixef(glmer_wvn)
eta0_ab <- as.numeric(D_ab %*% beta_ab) + log(abund_sma$trap_nights)

lambda_samples <- exp(matrix(eta0_ab, nrow = length(eta0_ab), ncol = n_samples) + samples_z)

mosq_mean  <- rowMeans(lambda_samples)
mosq_lower <- apply(lambda_samples, 1, quantile, probs = 0.025)
mosq_upper <- apply(lambda_samples, 1, quantile, probs = 0.975)

saveRDS(mosq_mean, file = "data/mosq_mean.rds")
saveRDS(mosq_lower, file = "data/mosq_lower.rds")
saveRDS(mosq_upper, file = "data/mosq_upper.rds")

## --- C4. AnPIT diagnostic for the Poisson mixed model (custom
##         function from @sec-nonspat-anpit) ---
anpit_wnv <- function(model, test_prop = 0.25, nsim = 2000,
                      u_grid = seq(0, 1, length.out = 101), seed = NULL) {
  stopifnot(inherits(model, "glmerMod"))
  if (!is.null(seed)) set.seed(seed)
  if (test_prop <= 0 || test_prop >= 1) stop("test_prop must be in (0,1)")
  
  D   <- model.matrix(model)
  off <- model@offset; if (is.null(off)) off <- rep(0, nrow(D))
  y   <- model@resp$y
  beta <- lme4::fixef(model)
  sigma <- sqrt(as.numeric(lme4::VarCorr(model)[[1]][1]))
  
  n <- nrow(D)
  test_idx  <- sort(sample(seq_len(n), size = ceiling(n * test_prop)))
  train_idx <- setdiff(seq_len(n), test_idx)
  
  eta0 <- as.numeric(D %*% beta) + off
  u_draws <- rnorm(nsim, mean = 0, sd = sigma)
  
  y_test <- y[test_idx]
  eta0_test <- eta0[test_idx]
  n_test <- length(test_idx)
  
  Fi_y      <- numeric(n_test)
  Fi_yminus <- numeric(n_test)
  y_minus <- pmax(y_test - 1L, -1L)
  
  for (i in seq_len(n_test)) {
    lam <- exp(eta0_test[i] + u_draws)
    Fi_y[i] <- mean(ppois(y_test[i], lambda = lam))
    Fi_yminus[i] <- if (y_minus[i] < 0) 0 else mean(ppois(y_minus[i], lambda = lam))
    if (Fi_y[i] < Fi_yminus[i]) { tmp <- Fi_y[i]; Fi_y[i] <- Fi_yminus[i]; Fi_yminus[i] <- tmp }
  }
  
  npit_avg <- function(u, Fy, Fym1) {
    denom <- pmax(Fy - Fym1, .Machine$double.eps)
    lt <- u <= Fym1; gt <- u >= Fy; mid <- !(lt | gt)
    out <- numeric(length(Fy))
    out[lt] <- 0; out[gt] <- 1; out[mid] <- (u - Fym1[mid]) / denom[mid]
    mean(out)
  }
  anpit <- vapply(u_grid, npit_avg, FUN.VALUE = 0.0, Fy = Fi_y, Fym1 = Fi_yminus)
  
  list(u = u_grid,
       anpit = as.numeric(anpit),
       Fi_y = Fi_y,
       Fi_yminus = Fi_yminus,
       test_index = test_idx,
       train_index = train_idx)
}

wnv_anpit_res <- anpit_wnv(glmer_wvn, test_prop = 0.30, nsim = 10000)

saveRDS(wnv_anpit_res, file = "data/wnv_anpit_res.rds")

## --- C5. WNV infection prevalence model (pooled testing) ---
data(infect_sma)

infect_sma <- st_as_sf(infect_sma, coords = c("lon", "lat"), crs = 4326)
wnv_crs <- propose_utm(infect_sma)
infect_sma <- st_transform(infect_sma, wnv_crs)

invlink_wnv <- function(x) 1 - (1 - exp(x) / (1 + exp(x)))^infect_sma$est_pool_n

inf_fit <-
  glgpm(wnv_pos ~ gp(lon, lat),
        crs = wnv_crs,
        family = "binomial",
        invlink = invlink_wnv,
        data = infect_sma)

saveRDS(inf_fit, file = "data/inf_fit.rds")

## --- C6. Prediction grid for infection prevalence ---
geom_union <- st_union(inf_fit$data_sf)
chull_sf <- st_convex_hull(geom_union)
chull_sf <- st_as_sf(data.frame(geometry = st_sfc(chull_sf)),
                     crs = st_crs(inf_fit$data_sf))

grid_pred_sac <- create_grid(chull_sf, spat_res = 0.25)

pred_S_inf <- pred_over_grid(inf_fit,
                             grid_pred = grid_pred_sac)

pred_inf_grid <- pred_target_grid(pred_S_inf,
                                  f_target = list(prev = function(x) exp(x) / (1 + exp(x))))

saveRDS(pred_inf_grid, file = "data/pred_inf_grid.rds")

## --- C7. Vector index (VI) at abund_sma locations ---
loc_pred <- st_as_sfc(st_transform(
  st_as_sf(abund_sma, coords = c("lon", "lat"), crs = 4326),
  crs = wnv_crs
))

pred_S_loc <- pred_over_grid(inf_fit, grid_pred = loc_pred)

beta_hat_inf <- coef(inf_fit)$beta
prev_inf_samples <- 1 / (1 + exp(-(beta_hat_inf + pred_S_loc$S_samples)))

# See assumption 2 above: mean_nmosq is taken to be mosq_mean
mean_nmosq <- mosq_mean
vi_samples <- mean_nmosq * prev_inf_samples

vi_mean  <- apply(vi_samples, 1, mean, na.rm = TRUE)
vi_lower <- apply(vi_samples, 1, quantile, probs = 0.025, na.rm = TRUE)
vi_upper <- apply(vi_samples, 1, quantile, probs = 0.975, na.rm = TRUE)

vi_df <- data.frame(
  id    = 1:nrow(abund_sma),
  mean  = vi_mean,
  lower = vi_lower,
  upper = vi_upper,
  trap_group = abund_sma$trap_group,
  year       = abund_sma$year
)

vi_df <- vi_df %>%
  group_by(year, trap_group) %>%
  arrange(mean, .by_group = TRUE) %>%
  mutate(x_order = dplyr::row_number()) %>%
  ungroup()

saveRDS(vi_df, file = "data/vi_df.rds")

## --- C8 (bundle for the ambiguous case_study2 load target) ---
case_study2 <- list(mean_nmosq = mean_nmosq)
saveRDS(case_study2, file = "data/case_study2.rds")

## ------------------------------------------------------------
message("Done. All files written to ./data:")
print(list.files("data", pattern = "\\.(rds|tif)$"))