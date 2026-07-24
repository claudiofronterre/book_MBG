library(sf)
library(RiskMap)

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
## 2. Liberia river-blindness models
##    -> data/fit_Liberia.rds             (fit_liberia)
##    -> data/fit_liberia_no_nugget.rds    (fit_liberia_no_nugget)
##    -> data/fit_liberia2.rds             (fit_liberia2)
## ------------------------------------------------------------
data(liberia)  # from RiskMap

## --- 2a. Main Binomial geostatistical model (no nugget), used
##         throughout Sec. "Example: river-blindness in Liberia"
fit_liberia <-
  glgpm(npos ~ log(elevation) + gp(long, lat),
        den = ntest, data = liberia,
        crs = 4326,
        convert_to_crs = 32629,
        family = "binomial")

saveRDS(fit_liberia, file = "data/fit_Liberia.rds")

## --- 2b. Same model, saved under the name used later for the
##         parametric-bootstrap section (identical specification
##         to fit_liberia; kept as a separate object/name because
##         that's the name glgpm_sim()/bootstrap code expects)
fit_liberia_no_nugget <- fit_liberia

saveRDS(fit_liberia_no_nugget, file = "data/fit_liberia_no_nugget.rds")

## --- 2c. Refit with the nugget estimated and a much larger
##         MCMC sample (110000 iterations, burn-in 10000, thin 10)
par0_liberia <- coef(fit_liberia)
par0_liberia$tau2 <- 0.1 

fit_liberia2 <-
  glgpm(npos ~ log(elevation) + gp(long, lat, nugget = NULL),
        den = ntest, data = liberia,
        crs = 4326,
        convert_to_crs = 32629,
        par0 = par0_liberia,
        control_mcmc = set_control_sim(n_sim = 110000,
                                       burnin = 10000,
                                       thin = 10),
        family = "binomial", messages = FALSE)

saveRDS(fit_liberia2, file = "data/fit_liberia2.rds")

## ------------------------------------------------------------
## 3. Parametric bootstrap for fit_liberia_no_nugget
##    -> data/par_boot.rds (object: par_hat)
## ------------------------------------------------------------
n_sim <- 100  # increase to >= 1000 for production-quality CIs

liberia_boot <- glgpm_sim(n_sim = n_sim, model_fit = fit_liberia_no_nugget)

par_hat <- list()

for (i in 1:n_sim) {
  liberia_boot$data_sim$npos_sim <-
    liberia_boot$data_sim[[paste("npos_sim", i, sep = "")]]
  
  fit_sim <- glgpm(
    formula = npos_sim ~ log(elevation) + gp(long, lat),
    data = liberia_boot$data_sim,
    family = "binomial",
    par0 = coef(fit_liberia_no_nugget),
    den = ntest,
    crs = 32629
  )
  
  par_hat[[i]] <- coef(fit_sim)
}

saveRDS(par_hat, file = "data/par_boot.rds")

## ------------------------------------------------------------
message("Done. All files written to ./data:")
print(list.files("data", pattern = "\\.(rds|RData)$"))