library(sf)
library(RiskMap)
library(lme4)

## ------------------------------------------------------------
## 1. Italy administrative boundaries
##    -> data/ita_adm2_geoboundaries.rds
##    -> data/ita_adm3_geoboundaries.rds
## ------------------------------------------------------------
library(rgeoboundaries)

italy_regions   <- geoboundaries(country = "italy", adm_lvl = "adm2")
italy_provinces <- geoboundaries(country = "italy", adm_lvl = "adm3")

saveRDS(italy_regions,   file = "data/ita_adm2_geoboundaries.rds")
saveRDS(italy_provinces, file = "data/ita_adm3_geoboundaries.rds")

## ------------------------------------------------------------
## 2. Empirical variogram for the Italy simulated data
##    -> data/italy_sim_variog.rds
## ------------------------------------------------------------
data(italy_sim)  # from RiskMap

lmer_fit <- lmer(y ~ log(pop_dens) + (1 | ID_loc), data = italy_sim)

italy_sim$rand_eff <- ranef(lmer_fit)$ID_loc[italy_sim$ID_loc, 1]

set.seed(1)
italy_sim_variog <- variogram(
  italy_sim,
  variable = "rand_eff",
  scale_to_km = TRUE,
  n_permutations = 1000
)

saveRDS(italy_sim_variog, file = "data/italy_sim_variog.rds")

## ------------------------------------------------------------
## 3. Galicia geostatistical model and kappa profile
##    -> data/fit_galicia.rds
##    -> data/galicia_kappa_profile.rds
## ------------------------------------------------------------
data(galicia)  # from RiskMap

fit_galicia <- glgpm(log(lead) ~ gp(kappa = 1.5),
                     data = galicia, family = "gaussian",
                     scale_to_km = TRUE, messages = FALSE)

saveRDS(fit_galicia, file = "data/fit_galicia.rds")

n_kappa <- 10
kappa_values <- seq(0.5, 3.5, length = n_kappa)
llik_values <- rep(NA_real_, n_kappa)
sigma2_me_hat <- rep(NA_real_, n_kappa)
fit_galicia_list <- vector("list", n_kappa)

for (i in seq_len(n_kappa)) {
  formula_i <- as.formula(
    sprintf("log(lead) ~ gp(kappa = %s)", kappa_values[i])
  )
  fit_galicia_list[[i]] <- glgpm(formula_i,
                                 data = galicia, family = "gaussian",
                                 scale_to_km = TRUE, messages = FALSE)
  llik_values[i] <- fit_galicia_list[[i]]$log.lik
  sigma2_me_hat[i] <- coef(fit_galicia_list[[i]])["sigma2_me"]
}

galicia_kappa_profile <- list(
  kappa_values = kappa_values,
  llik_values = llik_values,
  sigma2_me_hat = sigma2_me_hat
)

saveRDS(galicia_kappa_profile, file = "data/galicia_kappa_profile.rds")

## ------------------------------------------------------------
## 4. Italy spatial mixed model
##    -> data/italy_fit.rds
## ------------------------------------------------------------
italy_fit <- glgpm(y ~ log(pop_dens) + gp(kappa = 0.5, nugget = FALSE) +
                     re(region, province),
                   data = italy_sim, scale_to_km = TRUE,
                   family = "gaussian")

saveRDS(italy_fit, file = "data/italy_fit.rds")

## ------------------------------------------------------------
## 5. Anopheles geostatistical model
##    -> data/an_fit.rds
## ------------------------------------------------------------
data(anopheles)  # from RiskMap

set.seed(1)
an_fit <- glgpm(An.gambiae ~ elevation + gp(),
                data = anopheles,
                family = "poisson", messages = FALSE)

saveRDS(an_fit, file = "data/an_fit.rds")

## ------------------------------------------------------------
## 6. Liberia river-blindness models
##    -> data/fit_Liberia.rds             (fit_liberia)
##    -> data/fit_liberia_no_nugget.rds    (fit_liberia_no_nugget)
##    -> data/fit_liberia2.rds             (fit_liberia2)
## ------------------------------------------------------------
data(liberia)  # from RiskMap
liberia <- st_as_sf(liberia, coords = c("long", "lat"), crs = 4326)

## --- 6a. Main Binomial geostatistical model (no nugget), used
##         throughout Sec. "Example: river-blindness in Liberia"
fit_liberia <-
  glgpm(npos ~ log(elevation) + gp(),
        den = ntest, data = liberia,
        convert_to_crs = 32629,
        family = "binomial")

saveRDS(fit_liberia, file = "data/fit_Liberia.rds")

## --- 6b. Same model, saved under the name used later for the
##         parametric-bootstrap section (identical specification
##         to fit_liberia; kept as a separate object/name because
##         that's the name glgpm_sim()/bootstrap code expects)
fit_liberia_no_nugget <- fit_liberia

saveRDS(fit_liberia_no_nugget, file = "data/fit_liberia_no_nugget.rds")

## --- 6c. Refit with the nugget estimated and a much larger
##         MCMC sample (110000 iterations, burn-in 10000, thin 10)
par0_liberia <- coef(fit_liberia)
par0_liberia$tau2 <- 0.1 

fit_liberia2 <-
  glgpm(npos ~ log(elevation) + gp(nugget = TRUE),
        den = ntest, data = liberia,
        convert_to_crs = 32629,
        par0 = par0_liberia,
        control_mcmc = set_control_sim(n_sim = 110000,
                                       burnin = 10000,
                                       thin = 10),
        family = "binomial", messages = FALSE)

saveRDS(fit_liberia2, file = "data/fit_liberia2.rds")

## ------------------------------------------------------------
## 7. Parametric bootstrap for fit_liberia_no_nugget
##    -> data/par_boot.rds (object: par_hat)
## ------------------------------------------------------------
n_sim <- 100  # increase to >= 1000 for production-quality CIs

liberia_boot <- glgpm_sim(n_sim = n_sim, model_fit = fit_liberia_no_nugget)

par_hat <- list()

for (i in 1:n_sim) {
  liberia_boot$data_sim$npos_sim <-
    liberia_boot$data_sim[[paste("npos_sim", i, sep = "")]]
  
  fit_sim <- glgpm(
    formula = npos_sim ~ log(elevation) + gp(),
    data = liberia_boot$data_sim,
    family = "binomial",
    par0 = coef(fit_liberia_no_nugget),
    den = ntest
  )
  
  par_hat[[i]] <- coef(fit_sim)
}

saveRDS(par_hat, file = "data/par_boot.rds")

## ------------------------------------------------------------
message("Done. All files written to ./data:")
print(list.files("data", pattern = "\\.(rds|RData)$"))
