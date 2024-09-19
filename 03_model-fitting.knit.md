# Model formulation and parameter estimation {#sec-estimation}

## List of the main functions used in the chapter {.unnumbered}

| Function      | R Package | Used for                                                                                     |
|-------------------|-------------------|----------------------------------|
| `lmer`        | `lme4`    | Fitting linear mixed models                                                                  |
| `glmer`       | `lme4`    | Fitting generalized linear mixed models                                                      |
| `glgm`        | `RiskMap` | Fitting generalized linear geostatistical models                                             |
| `s_variogram` | `RiskMap` | Computing the empirical variogram and carrying out permutation test for spatial independence |

## Exploratory analysis

As illustrated in @fig-stages, exploratory analysis is the first step that should be carried out in a statistical analysis. This stage is essential to inform how covariates should be introduced in the model and, in our case, whether the variation unexplained by those covariates exhibits spatial correlation.

In the exploratory analysis of count data, we will also look at how overdispersion, which is a necessary, though not sufficient, condition for residual spatial correlation.

### Exploring associations with risk factors using count data {#sec-expl-assoc}

Assessment of the association between the health outcome of interest and non-categorical (i.e. continuous) risk factors can be carried using graphical tools, such scatter plots. The graphical inspection of the empirical association between the outcome and the covariates is especially useful to identify non-linear patterns in the relationship which should then be accounted for in the model formulation.

In this section, we look more closely at the case when the observed outcome is a count which requires a different treatment from continuously measured outcomes, which are generally covered by most statistics textbooks (see, for example, Chapter 1 of @weisberg2014).

#### When the outcome is an aggregated count {#sec-agg-count-exp}

Let us first consider the example of the river-blindness data in Liberia (@sec-rb-ch1), and examine the association between prevalence and elevation. We first generate a plot of the prevalence against the measured elevation at each of the sample locations



::: {.cell}

:::

::: {.cell}

```{.r .cell-code}
liberia$prev <- liberia$npos/liberia$ntest

ggplot(liberia, aes(x = elevation, y = prev)) + geom_point() +
  labs(x="Elevation (meters)",y="Prevalence")
```

::: {.cell-output-display}
![Scatter plot of the empirical prevalence for river-blindess against elevation, measured in meters.](03_model-fitting_files/figure-pdf/fig-prev-elev-liberia-1.pdf){#fig-prev-elev-liberia fig-pos='H'}
:::
:::



The plot shown in @fig-prev-elev-liberia shows that, as elevation increases from 0 to around 150 meters, prevalence rapidly increases to around 0.25 and, for larger values in elevation than 150 meters, the relationship levels off. This begs the question of how we can account for this in a regression model. To answer this question rigorously, however, the plot in @fig-prev-elev-liberia cannot be used. This is because, when the modelled outcome is a bounded Binomial count, regression relationships are specified on the logit-transformed prevalence (log-odds) scale; see @tbl-glm in Section @sec-geostat-models . To explore regression relationships in the case of prevalence data, it is convenient to use the so-called empirical logit in place of the empirical prevalence. The empirical logit is defined as

$$
l_{i} = \log\left\{\frac{y_i + 1/2}{n_i - y_i + 1/2}\right\}
$$ {#eq-empirical-logit}

where $y_i$ are the number of individuals who tested positive for river-blindness and $n_i$ is the total number of people tested at a location. The reason for using the empirical logit, rather than the standard logit transformation applied directly to the empirical prevalence, is that it allows to generate finite values for empirical prevalence values of 0 and 1, for which the standard logit transformation is not defined.



















































































