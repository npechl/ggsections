# ggsections

<!-- badges: start -->
[![R-CMD-check](https://github.com/npechl/ggsections/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/npechl/ggsections/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
<!-- badges: end -->

ggsections colours the background of a ggplot2 panel in two or four
rectangular sections. `geom_sections()` places the boundaries
automatically from the groups in your data; `annotate_sections()` draws
them at cutoffs you choose.

## Installation

Install the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("npechl/ggsections")
```

## Example: `geom_sections()`

`geom_sections()` reads the `fill` grouping and places the section
boundaries in the gap between groups. Colours follow the usual ggplot2
fill scales.

Two groups, split along x:

```r
library(ggplot2)
library(ggsections)

two <- data.frame(
  x = c(1, 2, 3, 7, 8, 9),
  y = c(2, 4, 3, 5, 2, 4),
  group = rep(c("control", "treated"), each = 3)
)

ggplot(two, aes(x, y, fill = group)) +
  geom_sections(orientation = "x") +
  geom_point(shape = 21, size = 3) +
  scale_fill_manual(values = c(control = "steelblue", treated = "tomato"))
```

`orientation = "x"` splits left/right and `"y"` splits bottom/top. The
default, `"auto"`, picks the axis with the wider gap between the groups.

Four groups give a 2 x 2 layout, here with a stronger `alpha` than the
default of 0.2 and dividing lines (dashed by default):

```r
four <- data.frame(
  x = c(1, 2, 8, 9, 1, 2, 8, 9),
  y = c(1, 2, 1, 2, 8, 9, 8, 9),
  quadrant = rep(c("bottom-left", "bottom-right", "top-left", "top-right"), each = 2)
)

ggplot(four, aes(x, y, fill = quadrant)) +
  geom_sections(alpha = 0.3, show_lines = TRUE) +
  geom_point(shape = 21, size = 3)
```

## Example: `annotate_sections()`

`annotate_sections()` draws sections at cutoffs you choose, in data
units. One cutoff gives two sections:

```r
ggplot(mtcars, aes(wt, mpg)) +
  annotate_sections(x_cutoff = 3.5) +
  geom_point()
```

Both cutoffs give four. `fill` takes four colours in the order
bottom-left, bottom-right, top-left, top-right:

```r
ggplot(mtcars, aes(wt, mpg)) +
  annotate_sections(
    x_cutoff = 3.5,
    y_cutoff = 20,
    fill = c("grey90", "tomato", "seagreen", "grey90"),
    show_lines = TRUE,
    line_colour = "black",
    line_type = "dotted"
  ) +
  geom_point()
```
