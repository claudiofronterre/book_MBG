# Contributing to the MBG book

Thank you for contributing to *Model-based geostatistics for global public
health using R*. This repository contains the book source, reproducible
computational results, and the inputs needed to regenerate expensive fitted
objects.

Please read [STYLE_GUIDE.md](STYLE_GUIDE.md) before editing prose, equations,
tables, or R code.

## Branches and pull requests

- Create a short-lived branch from `master` for each coherent change.
- Keep editorial changes separate from regenerated computational output when
  practical. This makes reviews substantially easier.
- Do not use automatic Markdown reflow across a whole chapter. It can separate
  equations from identifiers and alter line breaks intentionally used by
  Quarto. Reflow only prose that you have inspected.
- Before requesting review, render every chapter changed by the pull request
  and inspect both the rendered page and the Git diff.
- Request review from another contributor before merging substantial changes.
- Do not commit editor settings, local environment paths, credentials, caches,
  or generated website files.

## Reproducible R environment

The book uses renv. After cloning or pulling changes, restore the recorded
environment from the project directory:

```r
renv::restore()
```

When a package version intentionally changes, update `renv.lock` and verify
that the relevant chapter still renders. Avoid a broad snapshot when only one
package should change; use a targeted `renv::record()` where possible.

Machine-specific settings do not belong in `.Renviron`, `.Rprofile`, or the
book source. Put them in the user's R environment instead.

## Computation, freeze, and cache

The repository uses Quarto's `freeze: auto` setting. `_freeze/` is committed
because it allows the website to be built without repeating R computations in
GitHub Actions. It is the only generated computational-output directory that
should be committed.

When changing a chapter:

1. Render the chapter locally.
2. Inspect the chapter in HTML (and PDF when the change is format-sensitive).
3. Commit the updated `.qmd` file and the corresponding `_freeze/` results.
4. Do not commit `docs/`, `*_files/`, `site_libs/`, or `*_cache/`.

Use cache only for a specific deterministic chunk that benefits materially
from it. Do not enable cache for an entire chapter by default. Cache is a local
performance aid; it is not a reproducibility mechanism and is not committed.

## Expensive computations and saved objects

Expensive model fits, simulations, and permutation procedures should be
precomputed in the appropriate `R/CH*.R` maintenance script and saved as one
object per `.rds` file in `data/`.

The public-facing chapter should:

- load the precomputed result with `readRDS()` in a hidden setup chunk;
- show the scientifically relevant computation in a separate chunk with
  `eval: false`; and
- explain the method without referring to the private maintenance script.

The displayed code and the maintenance script must remain equivalent. Record
the random seed for stochastic computations. Use descriptive, stable object
and file names.

Use the following pattern for slow computations. First, generate and save the
object in the appropriate maintenance script, such as `R/CH3.R`:

```r
fit_example <- expensive_model_fit(...)
saveRDS(fit_example, "data/fit_example.rds")
```

Load the saved object in a chapter chunk that is hidden from the reader:

````markdown
```{r}
#| label: load-fit-example
#| include: false
fit_example <- readRDS("data/fit_example.rds")
```
````

Show the complete code that produced the object, but do not execute the slow
computation during the book render:

````markdown
```{r}
#| label: fit-example
#| eval: false
fit_example <- expensive_model_fit(...)
```
````

Finally, display the relevant result from the saved object. Hide this short
presentation code when it is not itself important to the explanation:

````markdown
```{r}
#| label: tbl-fit-example
#| echo: false
to_table(fit_example)
```
````

This pattern keeps the chapter reproducible and shows readers how the result
was produced, while allowing routine renders to load the precomputed object
quietly. The visible code, hidden loading chunk, saved object, and maintenance
script must use the same model specification.

Do not add `.RData` files. They can contain multiple objects and load names
implicitly into the current environment. Existing `.RData` files may be
removed only after checking that the replacement `.rds` file preserves every
required object.

## Model output and figures

Present model results as a well-formatted, labelled table by default. Derive
the table from the fitted object rather than manually transcribing estimates.
Use consistent parameter names, numerical precision, interval labels,
alignment, and captions within and across chapters.

A complete printed `summary()` may be shown when a model-summary format is
introduced for the first time and seeing its structure is pedagogically
useful. In that first example, showing both the full summary and a formatted
table is acceptable when the text explains how to read or translate between
them. After the output structure has been introduced, use tables for model
results and discuss only the entries relevant to the argument.

Use ggplot2 for all reader-facing figures, including maps and figures based on
sf objects. Apply the established book theme, labels, scales, legends, and
caption conventions. Do not mix base R graphics with ggplot2 in the rendered
book. Base graphics may be used for private maintenance diagnostics that are
not included in a chapter.

## Rendering and publication

Render locally with:

```sh
quarto render
```

A push to `master` triggers the GitHub Pages workflow. The workflow renders
the website from committed frozen results and deploys the generated `docs/`
directory. It restores the recorded R environment because Quarto still uses
the knitr engine, but matching frozen results prevent expensive analysis from
being recomputed.

If a `.qmd` file changes without matching frozen results, update `_freeze/`
locally before merging.

The repository's GitHub Pages source must be set to **GitHub Actions**, and the
custom domain must be configured as `www.mbgr.org` in the repository Pages
settings. The tracked root `CNAME` file is copied into each deployment
artifact, but GitHub's custom-domain setting remains authoritative.

## Review checklist

- Prose and R terminology follow `STYLE_GUIDE.md`.
- Chunk labels are descriptive and unique across the book.
- Equations, equation identifiers, citations, and cross-references render.
- Displayed code agrees with the code that generated saved results.
- New stochastic results have a recorded seed.
- Slow computations use the visible-code, hidden-load, saved-object pattern.
- Model output uses a table unless a full first summary has a clear
  pedagogical purpose.
- Reader-facing figures use ggplot2 and follow the book's visual conventions.
- No local paths, credentials, caches, rendered website files, or `.RData`
  workspaces are included.
- Relevant HTML and PDF output has been inspected.
