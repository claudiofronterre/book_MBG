# Book style guide

This guide records the editorial conventions used throughout the book. Apply
them to new writing and use them when reviewing existing sections. Do not make
large mechanical replacements without checking the surrounding scientific and
mathematical context.

## R terminology in running text

| Element | Style | Example |
|---|---|---|
| R function | Code font with parentheses | `glgpm()`, `predict()`, `summary()` |
| Function call with arguments | Code font, including arguments | `glgpm(formula = ..., data = malaria)` |
| Argument or argument value | Code font, with spaces around `=` | `family = "binomial"`, `verbose = TRUE` |
| R object or variable | Code font | `fit`, `coords`, `results` |
| List or data-frame component | Code font | `fit$beta`, `liberia$elogit` |
| Dataset | Code font | `liberia`, `malaria_data` |
| Package in running text | Plain text | the ggplot2 package; the sf package |
| Package-qualified function | Code font with parentheses | `sf::st_transform()`, `stats::predict()` |
| Mathematical quantity | LaTeX math notation | $Y_i$, $\beta$, $S(x)$, $\sigma^2$ |

Always include parentheses when naming a function, even when no arguments are
shown: write `predict()`, not `predict`. Executable R expressions are written
in code font. Mathematical notation is never written as code: write $Y_i$,
not `` `Y_i` ``. Package names are neither bold nor code-formatted in running
text.

Prefer the phrase "the sf package" when discussing a package and
`sf::st_join()` when identifying a particular function. Use a qualified name
when it prevents ambiguity or makes a dependency especially useful to the
reader; otherwise use the same unqualified call shown in the surrounding code.

## Summary tables

Summary tables use a deliberately different package convention for visual
clarity:

| Column content | Style | Example |
|---|---|---|
| Function | Code font with parentheses | `glgpm()` |
| Package | Italic | *sf*, *terra*, *ggplot2* |
| Description | Plain text | Transforms a spatial object's coordinate reference system. |

Do not put backticks around package names in a table's package column.

For numerical model summaries, use the established table structure in the
relevant chapter. Report consistent parameter names, precision, interval
labels, alignment, caption style, and cross-reference placement. Derive table
values from the fitted object rather than manually transcribing them whenever
practical.

## R code

- Follow the tidyverse style guide unless the surrounding chapter has a
  deliberate local convention.
- Put spaces around `=` and binary operators.
- Use `<-` for assignment in executable R code.
- Use descriptive `snake_case` names.
- Keep displayed code scientifically complete even when an expensive call is
  marked `eval: false` and its saved result is loaded separately.
- Use `readRDS()` for `.rds` files and assign the return value explicitly.
- Do not use `load()` for `.rds` files.

## Code chunks

Every labelled chunk must have a descriptive label that is unique across the
entire book, not merely within a chapter. Use lowercase kebab-case and standard
Quarto prefixes where relevant:

- `fig-` for figures;
- `tbl-` for tables;
- `lst-` for code listings when they are cross-referenced.

Do not rename a labelled chunk without updating all references to it. Keep
chunk options consistently formatted. Use `collapse: true` only when the code
and its textual output should appear as one continuous code block; it does not
combine R objects or affect computation.

## Mathematics and cross-references

- Use `$...$` for inline mathematics.
- Use display-math blocks only for displayed equations.
- Keep an equation identifier adjacent to its equation block; do not insert a
  blank paragraph between them.
- Do not allow automatic prose wrapping to split equation syntax,
  cross-references, citations, or raw LaTeX commands.
- Define transformations and notation before first use and maintain the same
  scale throughout an example (for example, explicitly state when an outcome
  is analysed on the log scale).

## Prose and lists

Use clear sentences and British English consistently. Prefer ordinary Quarto
Markdown lists over manual line breaks. Introduce a list with a complete
sentence, leave a blank line before it, and use `1.` or `-` markers. Avoid a
trailing backslash as a layout device.

Keep one blank line between paragraphs, headings, lists, tables, equations,
and fenced code blocks. Formatting-only edits must not silently alter or
remove scientific content.
