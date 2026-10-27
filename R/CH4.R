## ------------------------------------------------------------
## Explanatory figures for Sections 4.1 and 4.3
##    -> figures/joint-versus-marginal.{png,pdf}
##    -> figures/prediction-workflow.{png,pdf}
## Both are drawn at the PDF text width (28pc, about 4.65 in).
## ------------------------------------------------------------
library(ggplot2)
library(patchwork)
library(scales)

ink <- "#243746"
muted <- "#5E6E79"

# Figure 4.1: perfect dependence versus independence over 16 cells.
draw_joint_prediction_illustration <- function() {
  low <- "#E3C582"
  high <- "#347B87"
  bar <- "#9AA5AD"
  exceed <- "#BE6B2D"

  set.seed(2026)
  dependent <- list(0.1, 0.3, 0.3, 0.1)
  independent <- replicate(4, sample(c(0.1, 0.3), 16, replace = TRUE),
                           simplify = FALSE)
  draw_data <- function(values_list) {
    do.call(rbind, lapply(seq_along(values_list), function(k) {
      values <- rep_len(values_list[[k]], 16)
      data.frame(expand.grid(x = 1:4, y = 1:4), value = values,
                 draw = sprintf("Draw %d\naverage %s", k,
                                label_percent(accuracy = 1)(mean(values))))
    }))
  }
  maps <- function(values_list, title) {
    ggplot(draw_data(values_list),
           aes(x, y, fill = factor(value, levels = c(0.1, 0.3)))) +
      geom_tile(colour = "white", linewidth = 0.5) +
      annotate("point", x = 1, y = 4, shape = 21, size = 1.4,
               fill = "white", colour = ink, stroke = 0.5) +
      scale_fill_manual(values = c(low, high), drop = FALSE,
                        guide = "none") +
      facet_wrap(~ draw, nrow = 1) +
      coord_equal() +
      labs(title = title) +
      theme_void(base_size = 8) +
      theme(plot.title = element_text(face = "bold", colour = ink,
                                      margin = margin(b = 4)),
            strip.text = element_text(size = 6.5, colour = ink,
                                      margin = margin(b = 2)))
  }
  bar_theme <- theme_minimal(base_size = 8) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.title = element_text(face = "bold", colour = ink, size = 8),
          plot.subtitle = element_text(colour = ink, size = 7),
          axis.title = element_text(size = 7, colour = ink),
          axis.text = element_text(size = 6.5))
  y_scale <- scale_y_continuous(limits = c(0, 0.56),
                                breaks = c(0, 0.25, 0.5),
                                expand = expansion(mult = c(0, 0.02)))
  marginal <- ggplot(data.frame(p = c(0.1, 0.3), prob = 0.5),
                     aes(p, prob, fill = factor(p))) +
    geom_col(width = 0.05, show.legend = FALSE) +
    scale_fill_manual(values = c(low, high)) +
    scale_x_continuous(limits = c(0, 0.4), breaks = c(0.1, 0.3),
                       labels = c("10%", "30%")) +
    y_scale +
    labs(title = "Any one cell", subtitle = "Same in A and B",
         x = "Prevalence", y = "Probability") +
    bar_theme
  regional <- function(independent, title) {
    d <- if (independent) {
      data.frame(p = 0.1 + 0.2 * (0:16) / 16, prob = dbinom(0:16, 16, 0.5))
    } else {
      data.frame(p = c(0.1, 0.3), prob = 0.5)
    }
    ggplot(d, aes(p, prob, fill = p > 0.25)) +
      annotate("rect", xmin = 0.25, xmax = 0.33, ymin = 0, ymax = 0.56,
               fill = exceed, alpha = 0.08) +
      geom_col(width = if (independent) 0.0105 else 0.018,
               show.legend = FALSE) +
      geom_vline(xintercept = 0.25, linetype = "dashed", colour = exceed,
                 linewidth = 0.4) +
      annotate("label", x = 0.2, y = 0.5, size = 2.2, colour = exceed,
               fill = "white", lineheight = 0.9,
               label = sprintf("P(average > 25%%)\n= %s",
                               if (independent) "1.06%" else "50%")) +
      scale_fill_manual(values = c(`FALSE` = bar, `TRUE` = exceed)) +
      scale_x_continuous(limits = c(0.07, 0.33),
                         breaks = c(0.1, 0.2, 0.3),
                         labels = c("10%", "20%", "30%")) +
      y_scale +
      labs(title = title, x = "Average over 16 cells", y = NULL) +
      bar_theme
  }
  left <- plot_spacer() / marginal / plot_spacer() +
    plot_layout(heights = c(0.3, 1, 0.3))
  centre <- maps(dependent, "A  Cells move together") /
    maps(independent, "B  Cells vary independently")
  right <- regional(FALSE, "A: regional average") /
    regional(TRUE, "B: regional average")
  (left | centre | right) + plot_layout(widths = c(0.75, 2.6, 1.25))
}

# Figure 4.2: predict() as operations on a locations-by-draws matrix.
draw_prediction_workflow <- function() {
  blue <- "#216B91"
  teal <- "#15766D"
  orange <- "#B56B25"
  panel <- "#F3F6F8"
  mono <- "mono"
  box <- function(x, y, w, h, fill = panel, colour = NA, lty = "solid") {
    annotate("rect", xmin = x, xmax = x + w, ymin = y, ymax = y + h,
             fill = fill, colour = colour, linewidth = 0.35, linetype = lty)
  }
  txt <- function(x, y, label, size = 2.9, colour = ink, face = "plain",
                  family = "sans", hjust = 0.5, ...) {
    annotate("text", x = x, y = y, label = label, size = size,
             colour = colour, fontface = face, family = family,
             hjust = hjust, lineheight = 0.95, ...)
  }
  arr <- function(x, y, xend, yend, colour = muted, lty = "solid") {
    annotate("segment", x = x, y = y, xend = xend, yend = yend,
             colour = colour, linewidth = 0.45, linetype = lty,
             arrow = grid::arrow(length = grid::unit(0.06, "inches"),
                                 type = "closed"))
  }
  seg <- function(x, y, xend, yend, colour = muted, lty = "solid") {
    annotate("segment", x = x, y = y, xend = xend, yend = yend,
             colour = colour, linewidth = 0.45, linetype = lty)
  }
  cw <- 0.24
  ch <- 0.2
  matrix_cells <- function(x0, ytop, values, palette,
                           limits = range(values)) {
    cols <- colorRampPalette(palette)(11)
    r <- (values - limits[1]) / diff(limits)
    idx <- matrix(pmax(1, pmin(11, round(r * 10) + 1)), nrow(values))
    g <- expand.grid(i = seq_len(nrow(values)), j = seq_len(ncol(values)))
    annotate("rect",
             xmin = x0 + (g$j - 1) * cw, xmax = x0 + g$j * cw,
             ymin = ytop - g$i * ch, ymax = ytop - (g$i - 1) * ch,
             fill = cols[idx[cbind(g$i, g$j)]], colour = "white",
             linewidth = 0.3)
  }

  set.seed(42)
  trend <- c(-1.2, -0.7, -0.4, 0.3, 0.8, 1.2)
  S <- outer(trend, rep(1, 9)) + matrix(rnorm(54, 0, 0.55), 6)
  P <- plogis(S - 0.6)
  P_pal <- c("#E4F0EC", "#15766D")
  mw <- 9 * cw
  mh <- 6 * ch

  p <- ggplot() +
    coord_fixed(xlim = c(0, 9.3), ylim = c(-0.85, 13.2), expand = FALSE,
                clip = "off") +
    theme_void() + theme(plot.margin = margin(2, 2, 2, 2))
  p <- p +
    box(0.1, 11.75, 2.95, 1.35, "white", muted) +
    txt(1.575, 12.75, "object", 3, blue, family = mono) +
    txt(1.575, 12.2, "fitted model: estimates\nand conditional draws of S", 2.5, muted) +
    box(3.2, 11.75, 2.95, 1.35, "white", muted) +
    txt(4.675, 12.75, "newdata", 3, blue, family = mono) +
    txt(4.675, 12.2, "prediction locations\nand covariates", 2.5, muted) +
    box(6.3, 11.75, 2.9, 1.35, "white", orange, lty = "dashed") +
    txt(7.75, 12.75, "boundaries", 3, orange, family = mono) +
    txt(7.75, 12.2, "polygons\n(optional)", 2.5, muted) +
    arr(1.575, 11.7, 1.575, 11.2) + arr(4.675, 11.7, 4.675, 11.2)
  p <- p + box(0.55, 8.75, 8.4, 2.4) +
    txt(0.8, 10.85, "1  Draw the spatial effect", 3.1, blue, "bold",
        hjust = 0) +
    matrix_cells(1.05, 10.35, S, c("#D8E5EE", "#216B91")) +
    txt(1.05 + mw / 2, 10.6, "draws", 2.4, muted) +
    txt(0.85, 10.35 - mh / 2, "locations", 2.4, muted, angle = 90) +
    annotate("rect", xmin = 1.02, xmax = 1.08 + mw,
             ymin = 10.35 - 2 * ch - 0.02, ymax = 10.35 - ch + 0.02,
             fill = NA, colour = ink, linewidth = 0.55) +
    annotate("rect", xmin = 1.05 + 5 * cw - 0.02, xmax = 1.05 + 6 * cw + 0.02,
             ymin = 10.35 - mh - 0.03, ymax = 10.38, fill = NA,
             colour = ink, linewidth = 0.55, linetype = "22") +
    annotate("rect", xmin = 3.55, xmax = 3.85, ymin = 10.0, ymax = 10.1,
             fill = NA, colour = ink, linewidth = 0.5) +
    txt(3.95, 10.25, 'type = "marginal"', 2.55, blue, family = mono,
        hjust = 0) +
    txt(3.95, 9.97, "each row (one location) has the\nright distribution", 2.4,
        muted, hjust = 0, vjust = 0.7) +
    annotate("rect", xmin = 3.65, xmax = 3.75, ymin = 9.0, ymax = 9.42,
             fill = NA, colour = ink, linewidth = 0.5, linetype = "22") +
    txt(3.95, 9.47, 'type = "joint"', 2.55, blue, family = mono,
        hjust = 0) +
    txt(3.95, 9.19, "each column (one draw) is also\none possible map", 2.4,
        muted, hjust = 0, vjust = 0.7) +
    arr(4.675, 8.7, 4.675, 8.2)
  p <- p + box(0.55, 5.75, 8.4, 2.4) +
    txt(0.8, 7.85, "2  Build the targets in every draw", 3.1, teal, "bold",
        hjust = 0)
  for (k in 3:1) {
    off <- (k - 1) * 0.12
    p <- p + matrix_cells(1.05 + off, 7.35 + off, if (k == 1) P else S,
                          if (k == 1) P_pal else c("#E9F1EE", "#8DBDB2"))
  }
  p <- p +
    txt(3.55, 7.3, "linear predictor = fixed effects + spatial draw",
        2.45, hjust = 0) +
    txt(3.55, 7.0, "include", 2.55, teal, family = mono, hjust = 0) +
    txt(4.45, 7.0, "adds a nugget or random effects", 2.45, muted,
        hjust = 0) +
    txt(3.55, 6.6, "prevalence or rate = its inverse link", 2.45,
        hjust = 0) +
    txt(3.55, 6.25, "transform", 2.55, teal, family = mono, hjust = 0) +
    txt(4.85, 6.25, "adds further targets", 2.45, muted, hjust = 0) +
    txt(3.55, 5.95, "each target has the same rows and columns", 2.45,
        muted, hjust = 0)
  p <- p + seg(4.675, 5.7, 4.675, 5.45) + seg(2.35, 5.45, 6.95, 5.45) +
    arr(2.35, 5.45, 2.35, 5.15, teal) + arr(6.95, 5.45, 6.95, 5.15, orange) +
    seg(9.12, 11.75, 9.12, 5.3, orange, "dashed") +
    seg(9.12, 5.3, 6.95, 5.3, orange, "dashed")
  lx <- 0.1
  p <- p + box(lx, 0.1, 4.45, 5.0, "#EAF3F0") +
    txt(lx + 2.225, 4.75, "3  Summarise each location", 3.0, teal, "bold") +
    txt(lx + 2.225, 4.42, "across its draws", 2.5, teal) +
    matrix_cells(lx + 0.35, 4.05, P, P_pal) +
    annotate("rect", xmin = lx + 0.3, xmax = lx + 0.4 + mw,
             ymin = 4.05 - ch - 0.02, ymax = 4.07, fill = NA,
             colour = teal, linewidth = 0.5) +
    arr(lx + 0.45 + mw, 4.05 - mh / 2, lx + 3.05, 4.05 - mh / 2, teal) +
    matrix_cells(lx + 3.15, 4.05, matrix(rowMeans(P), ncol = 1), P_pal,
                 limits = range(P)) +
    txt(lx + 3.27, 4.05 - mh - 0.12, "one value\nper row", 2.2, teal,
        vjust = 1) +
    txt(lx + 2.225, 1.95, "mean, interval width, exceedance, ...", 2.4,
        muted) +
    txt(lx + 2.225, 1.5, "set by", 2.3, muted) +
    txt(lx + 2.225, 1.2, "summaries, thresholds, level", 2.45, teal,
        family = mono) +
    txt(lx + 2.225, 0.55, "$predictions", 2.9, teal, family = mono,
        face = "bold")
  rx <- 4.75
  G <- rbind(colMeans(P[1:3, ]), colMeans(P[4:6, ]))
  p <- p + box(rx, 0.1, 4.45, 5.0, "#FCF1E5") +
    txt(rx + 2.225, 4.75, "4  Aggregate within each draw", 3.0, orange,
        "bold") +
    txt(rx + 2.225, 4.42, "then summarise each polygon", 2.5, orange) +
    matrix_cells(rx + 0.55, 4.05, P, P_pal) +
    seg(rx + 0.47, 4.04, rx + 0.47, 4.05 - 3 * ch + 0.03, orange) +
    seg(rx + 0.47, 4.05 - 3 * ch - 0.03, rx + 0.47, 4.06 - 6 * ch, orange) +
    txt(rx + 0.35, 4.05 - 1.5 * ch, "A", 2.5, orange, "bold") +
    txt(rx + 0.35, 4.05 - 4.5 * ch, "B", 2.5, orange, "bold") +
    arr(rx + 0.55 + mw / 2, 4.05 - mh - 0.06, rx + 0.55 + mw / 2, 2.55,
        orange) +
    txt(rx + 0.7 + mw / 2, 2.67, "aggregate, weights", 2.3, orange,
        family = mono, hjust = 0) +
    matrix_cells(rx + 0.55, 2.5, G, c("#FBE9D6", "#B56B25"),
                 limits = range(P)) +
    txt(rx + 0.35, 2.5 - 0.5 * ch, "A", 2.5, orange, "bold") +
    txt(rx + 0.35, 2.5 - 1.5 * ch, "B", 2.5, orange, "bold") +
    txt(rx + 0.55 + mw / 2, 1.92, "one value per polygon in each draw",
        2.25, muted) +
    txt(rx + 2.225, 1.5, "then summarise each row as in step 3", 2.4,
        muted) +
    txt(rx + 2.225, 1.15, "needs joint draws, chosen automatically", 2.4,
        muted) +
    txt(rx + 2.225, 0.55, "$boundary_summary", 2.9, orange, family = mono,
        face = "bold")
  p <- p +
    txt(4.65, -0.3, "parameter_uncertainty = TRUE", 2.45, muted,
        family = mono) +
    txt(4.65, -0.62,
        "repeats steps 1 and 2 for each parameter draw and pools all draws",
        2.4, muted)
  p
}

save_prediction_figures <- function() {
  joint <- draw_joint_prediction_illustration()
  ggsave("figures/joint-versus-marginal.png", joint, width = 5.6,
         height = 3.3, dpi = 300, bg = "white")
  ggsave("figures/joint-versus-marginal.pdf", joint, width = 5.6,
         height = 3.3, device = cairo_pdf, bg = "white")
  workflow <- draw_prediction_workflow()
  ggsave("figures/prediction-workflow.png", workflow, width = 4.65,
         height = 7.02, dpi = 300, bg = "white")
  ggsave("figures/prediction-workflow.pdf", workflow, width = 4.65,
         height = 7.02, device = cairo_pdf, bg = "white")
}

## ------------------------------------------------------------
## Sections 4.1 to 4.3: prediction with predict()
## The displayed chapter code matches the code below. Run from the
## project root. Downloads use saved copies when they exist.
## ------------------------------------------------------------
library(sf)
library(RiskMap)
library(terra)
library(elevatr)
library(geobounds)

data(liberia)
liberia <- st_as_sf(liberia, coords = c("long", "lat"), crs = 4326)
fit_liberia <- readRDS("data/fit_Liberia.rds")
an_fit <- readRDS("data/an_fit.rds")

## Liberia prediction grid with elevation
##    -> data/liberia_grid.rds
liberia_adm0 <- readRDS("data/lbr_adm0_geoboundaries.rds") |>
  st_transform(crs = 32629)
liberia_grid <- create_grid(liberia_adm0, spacing = 5,
                            distance_units = "km", keep = "overlap")

# SRTM elevation prepared in Chapter 2, the source of the survey elevations
liberia_elev <- rast("data/lbr_elevation_srtm.tif")
liberia_elev[liberia_elev <= 0] <- NA
extraction_points <- st_transform(liberia_grid, crs(liberia_elev))
liberia_grid$elevation <- extract(liberia_elev, extraction_points,
                                  search_radius = 5000)[[2]]

saveRDS(liberia_grid, "data/liberia_grid.rds")

## 4.1 and 4.2: location-level prevalence
##    -> data/liberia_prediction.rds
liberia_prediction <- predict(fit_liberia,
                              newdata = liberia_grid,
                              type = "marginal",
                              summaries = c("mean", "interval_width",
                                            "cv", "exceedance"),
                              thresholds = 0.2,
                              keep_samples = TRUE,
                              seed = 2026,
                              messages = FALSE)
# Only the draws for the first location are shown in the chapter.
liberia_prediction$samples$spatial <-
  liberia_prediction$samples$spatial[1, , drop = FALSE]
saveRDS(liberia_prediction, "data/liberia_prediction.rds")

## 4.2: fixed versus propagated parameter uncertainty
##    -> data/liberia_parameter_uncertainty.rds
prediction_summaries <- c("mean", "lower", "upper", "interval_width")
liberia_fixed <- predict(fit_liberia,
                         newdata = liberia_grid,
                         type = "marginal",
                         summaries = prediction_summaries,
                         level = 0.95,
                         seed = 2026,
                         messages = FALSE)
liberia_uncertain <- predict(fit_liberia,
                             newdata = liberia_grid,
                             type = "marginal",
                             summaries = prediction_summaries,
                             level = 0.95,
                             parameter_uncertainty = TRUE,
                             parameter_draws = 50,
                             seed = 2026,
                             messages = FALSE)
saveRDS(list(fixed = liberia_fixed, propagated = liberia_uncertain),
        "data/liberia_parameter_uncertainty.rds")

## 4.2: malaria in Kenya, age 15 and direct age standardisation
##    -> data/fit_malkenya.rds, data/kenya_prediction.rds
data(malkenya)
malkenya_comm <- malkenya[malkenya$Survey == "community", ]
malkenya_comm1000 <- malkenya_comm[1:1000, ] |>
  st_transform(crs = 32736)

set.seed(2026)
fit_malkenya <- glgpm(RDT ~ Age + pmax(Age - 15, 0) +
                        pmax(Age - 40, 0) + elevation +
                        gp(nugget = TRUE),
                      data = malkenya_comm1000,
                      family = "binomial",
                      messages = FALSE)
saveRDS(fit_malkenya, "data/fit_malkenya.rds")

kenya_subcounties_path <- "data/ken_subcounties.rds"
if (!file.exists(kenya_subcounties_path)) {
  kenya_adm2 <- gb_get_adm2("Kenya")
  saveRDS(kenya_adm2[kenya_adm2$shapeName %in% c("Kabondo Kasipul",
                                                 "Kasipul"), ],
          kenya_subcounties_path)
}
kenya_subcounties <- readRDS(kenya_subcounties_path) |>
  st_transform(crs = 32736)
kenya_grid <- create_grid(kenya_subcounties, spacing = 0.5,
                          distance_units = "km", keep = "overlap")
kenya_elev_path <- "data/ken_elevation_ch4.rds"
if (!file.exists(kenya_elev_path)) {
  saveRDS(get_elev_raster(locations = kenya_grid, z = 9,
                          clip = "bbox", expand = 1000), kenya_elev_path)
}
kenya_elev <- rast(readRDS(kenya_elev_path))
kenya_points <- st_transform(kenya_grid, crs(kenya_elev))
kenya_grid$elevation <- extract(kenya_elev, kenya_points)[[2]]
kenya_grid$Age <- 15

kenya_age15 <- predict(fit_malkenya, newdata = kenya_grid,
                       summaries = "mean", seed = 2026,
                       messages = FALSE)
reference_ages <- data.frame(Age = malkenya_comm1000$Age)
kenya_standardised <- predict(fit_malkenya, newdata = kenya_grid,
                              standardize = reference_ages,
                              summaries = "mean",
                              seed = 2026,
                              messages = FALSE)
saveRDS(list(age15 = kenya_age15, standardised = kenya_standardised),
        "data/kenya_prediction.rds")

## 4.3: county-level prevalence, exceedance and adult burden in Liberia
##    -> data/liberia_county_grid.rds, data/liberia_county_area.rds,
##       data/liberia_county_adults.rds, data/liberia_national.rds
liberia_adm1 <- readRDS("data/lbr_adm1_geoboundaries.rds")

# Grid of 5 km cells built from WorldPop 2011 counts of adults aged 20+
liberia_adults <- get_worldpop("LBR", year = 2011, ages = c(20, Inf),
                               resolution = "1km")
adults_5km <- project(liberia_adults, "EPSG:32629", res = 1000,
                      method = "sum") |>
  aggregate(fact = 5, fun = "sum", na.rm = TRUE)
names(adults_5km) <- "adults"
county_grid <- create_grid(liberia_adm0, raster = adults_5km,
                           keep = "overlap")
county_grid$adults[is.na(county_grid$adults)] <- 0

# Adults in thousands, so that weighted sums are in thousands
county_grid$adults <- county_grid$adults / 1000
county_points <- st_transform(county_grid, crs(liberia_elev))
county_grid$elevation <- extract(liberia_elev, county_points,
                                 search_radius = 5000)[[2]]

# Drop the few coastal cells beyond the extent of the elevation raster
county_grid <- county_grid[!is.na(county_grid$elevation), ]
saveRDS(county_grid, "data/liberia_county_grid.rds")

liberia_county_area <- predict(fit_liberia,
                               newdata = county_grid,
                               boundaries = liberia_adm1,
                               aggregate = "mean",
                               summaries = "mean",
                               seed = 2026,
                               messages = FALSE)
liberia_county_adults <- predict(fit_liberia,
                                 newdata = county_grid,
                                 boundaries = liberia_adm1,
                                 aggregate = c("mean", "sum"),
                                 weights = "adults",
                                 summaries = c("mean", "lower",
                                               "upper", "exceedance"),
                                 thresholds = 0.2,
                                 seed = 2026,
                                 messages = FALSE)
liberia_national <- predict(fit_liberia,
                            newdata = county_grid,
                            boundaries = liberia_adm0,
                            aggregate = "sum",
                            weights = "adults",
                            summaries = c("mean", "lower", "upper"),
                            seed = 2026,
                            messages = FALSE)
saveRDS(liberia_county_area, "data/liberia_county_area.rds")
saveRDS(liberia_county_adults, "data/liberia_county_adults.rds")
saveRDS(liberia_national, "data/liberia_national.rds")

## 4.3: Anopheles gambiae, mean expected count over suitable elevations
##    -> data/anopheles_prediction.rds
anopheles_hull <- create_convex_hull(an_fit$data)
anopheles_grid <- create_grid(anopheles_hull, spacing = 2,
                              distance_units = "km")
anopheles_elev_path <- "data/cmr_elevation_ch4.rds"
if (!file.exists(anopheles_elev_path)) {
  saveRDS(get_elev_raster(locations = anopheles_grid, z = 9,
                          clip = "bbox", expand = 1000), anopheles_elev_path)
}
anopheles_elev <- rast(readRDS(anopheles_elev_path))
anopheles_points <- st_transform(anopheles_grid, crs(anopheles_elev))
anopheles_grid$elevation <- extract(anopheles_elev,
                                    anopheles_points)[[2]]
observed_elevation <- range(an_fit$data$elevation)
anopheles_grid$suitable <- as.numeric(
  anopheles_grid$elevation >= observed_elevation[1] &
    anopheles_grid$elevation <= observed_elevation[2])

anopheles_prediction <- predict(an_fit,
                                newdata = anopheles_grid,
                                boundaries = anopheles_hull,
                                aggregate = "mean",
                                weights = "suitable",
                                summaries = c("mean", "lower",
                                              "upper"),
                                seed = 2026,
                                messages = FALSE)
saveRDS(anopheles_prediction, "data/anopheles_prediction.rds")

library(dplyr)

## ------------------------------------------------------------
## 5. Cross-validation: fit M0/M1, run assess_prediction with regularized
##    subsampling and clustering splits (AnPIT + CRPS + SCRPS)
##    -> data/regularized.rds (regularized)
##    -> data/cluster.rds     (cluster)
## ------------------------------------------------------------
set.seed(123)
M0_fit <- glgpm(npos ~ gp(),
                denominator = ntest, data = liberia,
                model_crs = 32629,
                family = "binomial", messages = FALSE)

M1_fit <- glgpm(npos ~ elevation + pmax(elevation - 150, 0) + gp(),
                denominator = ntest, data = liberia,
                model_crs = 32629,
                family = "binomial", messages = FALSE)

# The default metrics = c("PIT", "CRPS", "SCRPS") computes all three
# diagnostics in one pass, which is what is saved here.
regularized <-
  assess_prediction(list(M0 = M0_fit, M1 = M1_fit),
                    method = "regularized", min_dist = 20,
                    size = 9, iter = 10, messages = FALSE)

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
                    denominator = ntest,
                    model_crs = 32629,
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

sim_pred_grid <- create_grid(shp, spacing = 5)
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
