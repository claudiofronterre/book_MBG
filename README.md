# Model-based geostatistics for global public health using R

This repository contains the source of *Model-based geostatistics for global
public health using R*, by Emanuele Giorgi and Claudio Fronterre.

Read the current online version at [www.mbgr.org](https://www.mbgr.org/).

## About the book

The book introduces the practical use of model-based geostatistics for global
public health. It covers spatial-data handling, exploratory analysis, model
fitting, prediction, validation, and applied case studies using R. The
companion RiskMap package provides the main modelling interface used in the
examples.

The intended reader has a basic understanding of linear regression and wants
to develop a reproducible workflow for analysing spatially referenced health
data, including continuous, binary, and count outcomes.

## Repository contents

- `01_intro.qmd`–`05_case-studies.qmd`: book chapters;
- `R/CH*.R`: maintenance scripts for expensive precomputed analyses;
- `data/`: datasets and single-object RDS artifacts used by the chapters;
- `_freeze/`: committed Quarto computational results used for reproducible,
  efficient publication;
- `STYLE_GUIDE.md`: writing, mathematical, R, and code-chunk conventions;
- `CONTRIBUTING.md`: contribution, rendering, and publication workflow.

Generated website files, local caches, and machine-specific settings are not
tracked.

## Reproducing the book

The project uses [Quarto](https://quarto.org/) and
[renv](https://rstudio.github.io/renv/) to record its R package environment.
From the repository root, restore the environment in R:

```r
renv::restore()
```

Then render the book from a shell:

```sh
quarto render
```

Some model fits and simulation procedures are intentionally precomputed. The
public chapters show the corresponding scientific code, while the maintenance
scripts in `R/` regenerate the saved RDS objects when required. See
[CONTRIBUTING.md](CONTRIBUTING.md) before updating these artifacts.

## Contributing

Contributions are welcome through focused branches and pull requests. Before
editing, please read:

- [the contribution guide](CONTRIBUTING.md); and
- [the book style guide](STYLE_GUIDE.md).

Please report errors or suggestions through the repository's
[GitHub issues](https://github.com/claudiofronterre/book_MBG/issues).

## Publication

Pushes to `master` trigger the GitHub Actions workflow that renders and deploys
the website to GitHub Pages. The published site uses the custom domain
[www.mbgr.org](https://www.mbgr.org/).
