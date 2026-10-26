
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- Knit with devtools::build_readme(), not rmarkdown::render() or the -->

<!-- "Knit" button -- those load the installed package, not the current -->

<!-- source, and can silently render stale/incorrect output. -->

<!-- badges: start -->

[![R-CMD-check](https://github.com/lhdjung/scrutiny/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/lhdjung/scrutiny/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/lhdjung/scrutiny/branch/main/graph/badge.svg)](https://app.codecov.io/gh/lhdjung/scrutiny?branch=main)

<!-- badges: end -->

# <a href="https://lhdjung.github.io/scrutiny/"><img src="man/figures/logo.png" align="left" height="150" alt="scrutiny website" /></a> scrutiny

scrutiny tests published summary statistics for consistency using
techniques like GRIM, GRIMMER, and DEBIT. It makes these methods easy to
use in a tidyverse-friendly way, hoping to help forensic meta-science go
mainstream.

You can use the most important parts in the [Error
detection](https://errors.shinyapps.io/scrutiny) Shiny app instead.

Besides ready-made tests, scrutiny has a system for implementing new
consistency tests, duplicate analysis, and data wrangling helpers. See
the *Articles* tab for vignettes.

Install the package from CRAN:

``` r
install.packages("scrutiny")
```

Alternatively, install the development version from GitHub:

``` r
remotes::install_github("lhdjung/scrutiny")
```

## Get started

GRIM-test all values in a data frame with `grim_map()`. The
`consistency` column says whether each mean (`x`) is consistent with its
sample size (`n`).

``` r
library(scrutiny)

# Example data:
pigs1
#> # A tibble: 12 × 2
#>        x     n
#>    <dbl> <dbl>
#>  1  7.22    32
#>  2  4.74    25
#>  3  5.23    29
#>  4  2.57    24
#>  5  6.77    27
#>  6  2.68    28
#>  7  7.01    29
#>  8  7.38    26
#>  9  3.14    27
#> 10  6.89    31
#> 11  5       25
#> 12  0.24    28

# GRIM-testing for data frames:
grim_map(pigs1, digits_x = 2, items_averaged = 1)
#> # A tibble: 12 × 6
#> # GRIM: default settings
#>        x     n items_averaged digits_x consistency exp_incon
#>    <dbl> <int>          <dbl>    <dbl> <lgl>           <dbl>
#>  1  7.22    32              1        2 TRUE             0.64
#>  2  4.74    25              1        2 FALSE            0.75
#>  3  5.23    29              1        2 FALSE            0.71
#>  4  2.57    24              1        2 FALSE            0.72
#>  5  6.77    27              1        2 FALSE            0.73
#>  6  2.68    28              1        2 TRUE             0.72
#>  7  7.01    29              1        2 FALSE            0.71
#>  8  7.38    26              1        2 TRUE             0.74
#>  9  3.14    27              1        2 FALSE            0.73
#> 10  6.89    31              1        2 FALSE            0.69
#> 11  5       25              1        2 TRUE             0.75
#> 12  0.24    28              1        2 FALSE            0.72
```

Test percentages instead of means:

``` r
pigs2
#> # A tibble: 6 × 2
#>       x     n
#>   <dbl> <dbl>
#> 1  67.4   150
#> 2  54.2   150
#> 3  54     150
#> 4  69.8   150
#> 5  68.1   150
#> 6  55.4   150

grim_map(pigs2, digits_x = 1, items_averaged = 1, percent = TRUE)
#> # A tibble: 6 × 6
#> # GRIM: percent = TRUE
#>       x     n items_averaged digits_x consistency exp_incon
#>   <dbl> <int>          <dbl>    <dbl> <lgl>           <dbl>
#> 1  67.4   150              1        1 FALSE            0.85
#> 2  54.2   150              1        1 FALSE            0.85
#> 3  54     150              1        1 TRUE             0.85
#> 4  69.8   150              1        1 FALSE            0.85
#> 5  68.1   150              1        1 FALSE            0.85
#> 6  55.4   150              1        1 FALSE            0.85
```

Choose how the means were rounded — below, up from 5. The plot adjusts
to the rounding method. Blue dots are consistent, red dots inconsistent:

``` r
pigs1 |>
  grim_map(rounding = "up", digits_x = 2, items_averaged = 1) |>
  grim_plot_grid()
```

<img src="man/figures/README-unnamed-chunk-5-1.png" alt="" width="75%" />

Use DEBIT to test means and standard deviations of binary data:

``` r
pigs3
#> # A tibble: 7 × 3
#>       x    sd     n
#>   <dbl> <dbl> <dbl>
#> 1  0.53  0.5   1683
#> 2  0.44  0.5   1683
#> 3  0.77  0.42  1683
#> 4  0.19  0.35  1683
#> 5  0.34  0.47  1683
#> 6  0.93  0.25  1683
#> 7  0.12  0.33  1683

pigs3 |>
  debit_map(digits_x = 2, digits_sd = 2)
#> # A tibble: 7 × 6
#> # DEBIT: default settings
#>       x    sd     n digits_x digits_sd consistency
#>   <dbl> <dbl> <int>    <dbl>     <dbl> <lgl>      
#> 1  0.53  0.5   1683        2         2 TRUE       
#> 2  0.44  0.5   1683        2         2 TRUE       
#> 3  0.77  0.42  1683        2         2 TRUE       
#> 4  0.19  0.35  1683        2         2 FALSE      
#> 5  0.34  0.47  1683        2         2 TRUE       
#> 6  0.93  0.25  1683        2         2 TRUE       
#> 7  0.12  0.33  1683        2         2 TRUE

pigs3 |>
  debit_map(digits_x = 2, digits_sd = 2) |>
  debit_plot()
```

<img src="man/figures/README-unnamed-chunk-6-1.png" alt="" width="75%" />

## Guiding ideas

> (…) a critical inspection of the published literature should not be
> mischaracterized as a hobby for the overly cynical, nor as so-called
> “methodological terrorism”. On the contrary, carefully evaluating
> presented data is a cornerstone of scientific investigation, and it is
> only logical to apply this also to the published literature. If we are
> not willing to critically assess published studies, we also cannot
> guarantee their veracity.

— van der Zee et al. (2017, pp. 8-9)

> (…) [data
> thugs](https://jamesheathers.medium.com/hugs-shrugs-and-data-thugs-663858757c4a)
> (…) demand data and if they do not receive it, they contact editors
> and universities and threaten to write blogs and tweets about the
> errors uncovered.

— Eric A. Stewart (six retractions; quoted in Pickett 2020, p. 178)

## AI usage since 1.0.0

Almost all scrutiny code before version 1.0.0 was written by me. No
version before 0.6.0 contained any appreciable amount of AI-generated
code. The first CRAN version, 0.2.2, was released even before ChatGPT
was.

However, version 1.0.0 brought vast improvements largely because I used
LLMs (mostly Claude) to write most of the new code. Not only did this
fix many bugs, iron out API quirks, enable new features, and improve
performance, but it also made testing in scrutiny much more thorough.
Unit tests are now dramatically more comprehensive than they used to be,
with grid sweeps that test key functions over entire ranges of
systematically constructed inputs. This has increased my confidence that
the code is correct.

I loved maintaining scrutiny as a hand-crafted passion project, but LLMs
now write code well enough that refusing to use them would have been a
huge missed opportunity. Even the Linux kernel [allows for AI
usage](https://docs.kernel.org/process/coding-assistants.html) now, so I
am in good company.

## References

Pickett, J. T. (2020). The Stewart Retractions: A Quantitative and
Qualitative Analysis. *Econ Journal Watch*, *17*(1), 152–190.
<https://econjwatch.org/articles/the-stewart-retractions-a-quantitative-and-qualitative-analysis>.

van der Zee, T., Anaya, J., & Brown, N. J. L. (2017). Statistical
heartburn: An attempt to digest four pizza publications from the Cornell
Food and Brand Lab. *BMC Nutrition*, *3*(1), 54.
<https://doi.org/10.1186/s40795-017-0167-x>.
