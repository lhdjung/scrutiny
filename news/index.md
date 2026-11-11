# Changelog

## scrutiny 1.0.0

CRAN release: 2026-11-10

scrutiny 1.0.0 is a major release with many bugfixes and new features,
but also many breaking changes. If you have used GRIM, GRIMMER, or DEBIT
before, you will need to adjust your code.

You can let error messages guide you – indeed, this will often be
necessary. You may read up on individual items below, but the release
notes are not intended to be read all at once.

### Key changes

Most calls to
[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md)
or [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md) need
three new arguments:

``` r

# scrutiny 0.6.1
grim_map(pigs1)
grim(x = "5.19", n = 28)

# scrutiny 1.0.0
grim_map(pigs1, digits_x = 2, items_averaged = 1, data_type = "integer")
grim(x = 5.19, n = 28, digits_x = 2, items_averaged = 1, data_type = "integer")
```

- `x` and `sd` are no longer strings but numbers. Since numbers drop
  trailing zeros, `digits_x` states the number of decimal places. For
  GRIMMER and DEBIT functions, there is also `digits_sd`.
- `items_averaged` replaces `items`, which defaulted to 1, so it
  silently tested means of multi-item scales as single items.
- `data_type` states whether the data are whole numbers (`"integer"`),
  as the tests assume, or `"continuous"`, where they don’t apply.

Each of these can also be a column of `data`. This allows for their
values to differ by row. If one is missing, the error will tell you how
to provide it.

You will also meet these changes:

- `percent = TRUE` is now `x_meaning = "percentage"`.
- The `probability` column of
  [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md)
  is now `exp_incon`, and
  [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
  renamed some columns, such as `incons_rate` to `emp_incon`. I had
  changed the name of that
  [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md)
  column before, but the new name will remain.
- Under the default settings, GRIM’s results for testable value sets are
  unchanged. GRIMMER no longer flags any consistent value sets and
  catches some inconsistent ones it used to miss. DEBIT is now exact.
  All three tests use exact integer arithmetic, and several rounding
  procedures were corrected; see “Test results” below.
- [`grim_plot()`](https://lhdjung.github.io/scrutiny/reference/grim_plot.md)
  is now
  [`grim_plot_grid()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_grid.md),
  which returns its plot instead of printing it.
- The rounding functions, such as `reround()`, moved to the
  [roundwork](https://lhdjung.github.io/roundwork/) package.
- New:
  [`grim_explain()`](https://lhdjung.github.io/scrutiny/reference/grim_explain.md),
  [`grimmer_explain()`](https://lhdjung.github.io/scrutiny/reference/grimmer_explain.md),
  and
  [`debit_explain()`](https://lhdjung.github.io/scrutiny/reference/debit_explain.md)
  use plain words to explain a result, and
  [`explain_print()`](https://lhdjung.github.io/scrutiny/reference/explain_print.md)
  does so for mapper output.

The tests, their three mapper tiers, and
[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
constitute the stable core of this release. Every function newly added
in 1.0.0 is experimental and may change in a minor or major release
later on. The same applies to the developer tools, such as
[`function_map()`](https://lhdjung.github.io/scrutiny/reference/function_map.md).

### Breaking changes

#### Numeric inputs

- `x` and `sd` are now numeric in
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md),
  [`grimmer()`](https://lhdjung.github.io/scrutiny/reference/grimmer.md),
  [`debit()`](https://lhdjung.github.io/scrutiny/reference/debit.md),
  and their mappers; strings are no longer accepted. Since numbers drop
  trailing zeros, the new `digits_x` and `digits_sd` arguments are
  required. They take one value per row in the basic and sequence
  mappers and one per group in the total-n mappers, as in
  `grim_map_total_n(df, digits_x = c(2, 1), items_averaged = 1, data_type = "integer")`,
  and must be whole numbers from 0 to 15. Alternatively, you can add
  `digits_x` and `digits_sd` columns to your input `data`. The total-n
  mappers will apply them to both groups. (See “Function interfaces”
  below on `items_averaged` and `data_type`.)

- `digits_*` arguments or columns, as well as the integer digits of `x`
  or `sd`, may not exceed about 15 significant digits. This is the
  precision of the double data type. Beyond that, GRIMMER and DEBIT get
  exact results for large means, SDs, and sample sizes. Apart from the
  limit on `x` with `n` that they share with GRIM, and one of 2^52 on
  `n` with `x` missing, GRIMMER only refuses an SD from about 5e5 to 1e6
  on, or an SE from about 3e5 to 7e5 on, for `n` from 2 to 4, and value
  sets with more than a million candidate sums; see
  [`?grimmer`](https://lhdjung.github.io/scrutiny/reference/grimmer.md)
  and [`?debit`](https://lhdjung.github.io/scrutiny/reference/debit.md).

- Mappers return `digits_x` and `digits_sd` as integer columns.
  [`grim_plot_grid()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_grid.md)
  (formerly
  [`grim_plot()`](https://lhdjung.github.io/scrutiny/reference/grim_plot.md))
  reads the number of decimal places from them instead of inferring it
  from `x`.

- The `n`, `items_averaged`, and `absolute` columns of mapper output are
  integer columns if all their values are whole numbers within the
  integer range.

#### Test results

- GRIMMER tests every sum that rounds to the reported mean. Earlier
  versions only took the one nearest to `x * n`, so tests 1 and 3 could
  fail for a consistent value set whenever several sums round to the
  mean, which can happen once `n` reaches `10^digits_x`. In a grid of
  such value sets, a fifth of the inconsistencies that scrutiny 0.6.1
  found for means with one decimal place were false positives, as were
  3% of those for means with two decimal places. The warning about this
  was removed. Thanks to Nathanael Larigaldie and
  [@takua624](https://github.com/takua624)
  ([\#80](https://github.com/lhdjung/scrutiny/issues/80),
  [\#84](https://github.com/lhdjung/scrutiny/issues/84)).

- GRIMMER computes candidate sums and sums of squares in exact integer
  arithmetic. Floating-point comparisons could lose a candidate on a
  rounding bound, so some consistent value sets were reported as
  inconsistent, also for multi-item scales. For example,
  `grimmer(x = 0.03, sd = 0.18, digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer", n = 200)`
  was `FALSE`, although seven 1s and 193 0s have exactly these
  statistics. Thanks to Ian Hussey
  ([\#86](https://github.com/lhdjung/scrutiny/issues/86)) and Aurélien
  Allard.

- GRIMMER rejects sums of squares below the minimum that whole numbers
  with the reconstructed sum can have. For example,
  `grimmer(x = 3.6, sd = 0.2, digits_x = 1, digits_sd = 1, items_averaged = 1, data_type = "integer", n = 11)`
  was `TRUE`, but 11 whole numbers that sum to 40 cannot have an SD
  below 0.5.

- For `n < 5`, GRIMMER also rejects sums of squares that are large
  enough but unattainable:
  `grimmer(x = 1.0, sd = 2.0, digits_x = 1, digits_sd = 1, items_averaged = 1, data_type = "integer", n = 2)`
  was `TRUE`, but the values would be 1 ± √2. For `n >= 5`, the minimum
  and the parity test suffice, so
  [`grimmer()`](https://lhdjung.github.io/scrutiny/reference/grimmer.md)
  is `TRUE` if and only if a sample of whole numbers has the reported
  mean and SD, except under `rounding = "even"`, which lets a tie round
  either way. The `reason` for both new rejections is
  `"GRIMMER inconsistent (test 1)"`.

- A rounding bound is exclusive whenever a value exactly on it would
  round away from the reported number. This already held for `"up"` and
  `"down"`, and now also for `"ceiling"`, `"floor"`, `"trunc"`,
  `"anti_trunc"`, and the `"*_from"` methods. Under `"even"`, both
  bounds stay inclusive, because whether
  [`base::round()`](https://rdrr.io/r/base/Round.html) sees a tie
  depends on the binary representation of the value.

- `symmetric` now affects the results of
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md) and
  [`grimmer()`](https://lhdjung.github.io/scrutiny/reference/grimmer.md),
  not only the `show_rec` values. `threshold` no longer affects `"up"`,
  `"down"`, and `"up_or_down"`, which always round from 5.

- `"down_from"` and `"up_from_or_down_from"` read `threshold` as
  `"up_from"` does: rounding goes down if the part cut off is at most
  `threshold` tenths of a rounding step. It used to switch at
  `10 - threshold`, so `"up_from_or_down_from"` could span up to 1.8
  steps, and GRIM with `threshold = 9` accepted every mean at `n = 57`.
  Results at the default `threshold = 5` are unchanged.

- `"anti_trunc"` no longer moves a value that already has the required
  number of decimal places: 8.42 rounded to two decimal places is 8.42,
  not 8.43, as with Excel’s `ROUNDUP()`. A mean of `0.00` is therefore
  consistent under `"anti_trunc"`; it used to be `FALSE`.

- DEBIT is exact by default. The mean of `n` binary values is `k / n`
  for a whole number `k`, but DEBIT treated it as continuous and
  accepted some SDs that no binary sample of size `n` can have.
  `formula` gains `"exact"`, the new default; `"mean_n"` is the old
  behavior. `"0_n"`, `"1_n"`, and `"groups"`, which always failed, were
  removed. With `show_rec = TRUE`,
  [`debit_map()`](https://lhdjung.github.io/scrutiny/reference/debit_map.md)
  also returns `sum_lower` and `sum_upper`, the smallest and largest `k`
  whose mean would be reported as `x`.

- GRIM, GRIMMER, and DEBIT return `NA` for every case that cannot be
  decided: an infinite value, an `n` or `items_averaged` that is not a
  positive whole number, or a missing value that could decide the case
  either way. A missing value that could not is no obstacle:
  [`grimmer()`](https://lhdjung.github.io/scrutiny/reference/grimmer.md)
  with a missing `sd` is `FALSE` if `x` fails GRIM, and with a missing
  `x` if no sum of whole numbers admits `sd`;
  [`debit()`](https://lhdjung.github.io/scrutiny/reference/debit.md)
  likewise. A missing `digits_*` value gives `FALSE` if the value set
  fails with the decimal places the values show, and `TRUE` if it passes
  with so many that only these values pass:
  `grim(5.2, 5, NA, 1, "integer")` is `TRUE`. A missing `n` gives `TRUE`
  if data that are all the same value fit, and `FALSE` if no data of any
  size fit: `grimmer(3.2, 0.2, NA, 1, 1, 1, "integer")` is `FALSE`,
  since whole numbers with a mean of about 3.2 have an SD of at least
  0.36. A few cases are still `NA`; see
  [`vignette("technical-notes")`](https://lhdjung.github.io/scrutiny/articles/technical-notes.md).
  GRIMMER and DEBIT also need `n >= 2`, since a sample SD divides by
  `n - 1`, unless `sd_meaning = "sd_pop"`. Such cases used to cause an
  error or return an arbitrary `TRUE` or `FALSE`. The renamed version of
  [`grim_probability()`](https://lhdjung.github.io/scrutiny/reference/grim_probability.md),
  [`grim_exp_incon()`](https://lhdjung.github.io/scrutiny/reference/grim-stats.md)
  returns `NA` for them too.

- [`grim_exp_incon()`](https://lhdjung.github.io/scrutiny/reference/grim-stats.md),
  [`grim_total()`](https://lhdjung.github.io/scrutiny/reference/grim-stats.md),
  and the `exp_incon` column of
  [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md)
  take the rounding method into account via new `rounding` and
  `threshold` arguments. The old formula, `(10^D - N * L) / 10^D`,
  assumes that each reported value comes from a single range of
  unrounded values, one step wide. Under the default `"up_or_down"`, a
  tie is accepted both ways, so the probability is lower: `0.4` instead
  of `0.6` for `n = 40` and two decimal places.

#### Function interfaces

- [`grim_probability()`](https://lhdjung.github.io/scrutiny/reference/grim_probability.md)
  is now
  [`grim_exp_incon()`](https://lhdjung.github.io/scrutiny/reference/grim-stats.md)
  (the old name is deprecated), and the `probability` column of
  [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md)
  is now `exp_incon`, the expected rate of inconsistency. In every
  [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
  method for mapper output, including
  [`audit_cols_minimal()`](https://lhdjung.github.io/scrutiny/reference/audit_cols_minimal.md),
  the first columns are now `all_cases`, `incon_cases` (formerly
  `incons_cases`), and `emp_incon` (formerly `incons_rate`), in this
  order. For
  [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md)
  output, `testable_cases` follows `all_cases`, and `exp_incon`
  (formerly `mean_grim_prob`) and `ratio` (formerly `incons_to_prob`)
  come last. `testable_rate` was removed: it is
  `testable_cases / all_cases`.

- `items` is now `items_averaged` and has no default
  ([\#94](https://github.com/lhdjung/scrutiny/issues/94)). It is the
  number of items averaged into each score; the old name was mistaken
  for the number of scale points, and the old default of 1 silently
  tested means of multi-item scales as single items. Give it wherever it
  applies, as in
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md),
  [`grimmer()`](https://lhdjung.github.io/scrutiny/reference/grimmer.md),
  their mappers, and
  [`grim_exp_incon()`](https://lhdjung.github.io/scrutiny/reference/grim-stats.md);
  use `items_averaged = 1` for single items and percentages. Mappers
  also accept an `items_averaged` column.
  [`grim_granularity()`](https://lhdjung.github.io/scrutiny/reference/grim_granularity.md)
  keeps its default of 1.

- GRIM, GRIMMER, and DEBIT have a new `data_type` argument without a
  default, since the tests only apply to whole numbers (for DEBIT, the
  binary values 0 and 1). `"integer"` data are tested as before.
  `"continuous"` data can have any mean and SD (for DEBIT, within 0 to
  1), so the result is `TRUE` unless no data can have the value set, as
  with a negative SD, the `reason` of
  [`grimmer_map()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map.md)
  is `"Continuous data"`, and `exp_incon` is 0. A `data_type` of `NA`
  gives the result for whole numbers if continuous data would get the
  same, and `NA` otherwise. Like `items_averaged`, `data_type` is
  vectorized and may be a column of `data`, and the mappers return it as
  a column. Give it in
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md),
  [`grimmer()`](https://lhdjung.github.io/scrutiny/reference/grimmer.md),
  [`debit()`](https://lhdjung.github.io/scrutiny/reference/debit.md),
  their mappers,
  [`grim_exp_incon()`](https://lhdjung.github.io/scrutiny/reference/grim-stats.md),
  and
  [`grim_total()`](https://lhdjung.github.io/scrutiny/reference/grim-stats.md),
  as in
  `grim(x = 5.19, n = 28, digits_x = 2, items_averaged = 1, data_type = "integer")`.
  [`grim_plot_grid()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_grid.md)
  and
  [`debit_plot()`](https://lhdjung.github.io/scrutiny/reference/debit_plot.md)
  leave out continuous data with a warning.

- GRIM’s `percent` argument is now `x_meaning`, which takes `"mean"`
  (the default) or `"percentage"`, as in
  `grim(x = 71, n = 43, digits_x = 0, items_averaged = 1, data_type = "integer", x_meaning = "percentage")`.
  Like `sd_meaning` in GRIMMER and DEBIT, it says what the reported
  value is rather than switching a conversion on or off. In
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md), its
  mappers,
  [`grim_exp_incon()`](https://lhdjung.github.io/scrutiny/reference/grim-stats.md),
  and
  [`grim_total()`](https://lhdjung.github.io/scrutiny/reference/grim-stats.md),
  giving `percent` is an error that points to `x_meaning`.

- In every mapper, the dots come right after the key arguments, so the
  arguments after them must be named in full. The total-n mappers gain
  an `n` argument for the column of totals, as in
  `grim_map_total_n(df, digits_x = c(2, 1), n = "total", items_averaged = 1, data_type = "integer")`.

- The settings of GRIM, GRIMMER, and DEBIT come in one order in every
  function: what the reported values mean (`x_meaning`, `sd_meaning`,
  and DEBIT’s `formula`), then `rounding`, `threshold`, and `symmetric`,
  then `show_rec`.

- [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md),
  [`grimmer_map()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map.md),
  and
  [`debit_map()`](https://lhdjung.github.io/scrutiny/reference/debit_map.md)
  are made by
  [`function_map()`](https://lhdjung.github.io/scrutiny/reference/function_map.md)
  and have an explicit argument for every argument of
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md),
  [`grimmer()`](https://lhdjung.github.io/scrutiny/reference/grimmer.md),
  and
  [`debit()`](https://lhdjung.github.io/scrutiny/reference/debit.md),
  such as `formula` in
  [`debit_map()`](https://lhdjung.github.io/scrutiny/reference/debit_map.md).
  A `digits_*` column of `data` that disagrees with the argument is an
  error. `merge_items`, `extra`, and `testables_only` were removed: use
  [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
  instead of `extra` and `dplyr::filter(exp_incon > 0)` instead of
  `testables_only`. `grim_map(x_meaning = "percentage")` keeps `x` as a
  percentage instead of converting it to a proportion with a message.

- The sequence and total-n mappers, such as
  [`grim_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grim_map_seq.md),
  have an explicit argument for every setting of their basic mapper,
  such as `rounding` or `threshold`, with the same default. These used
  to be passed through the dots, which must now be empty. Existing calls
  are unaffected.

- [`debit_map()`](https://lhdjung.github.io/scrutiny/reference/debit_map.md)’s
  `show_rec` defaults to `FALSE`, as in
  [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md);
  [`debit_plot()`](https://lhdjung.github.io/scrutiny/reference/debit_plot.md)
  reconstructs the values itself.
  [`debit_map()`](https://lhdjung.github.io/scrutiny/reference/debit_map.md)
  output no longer has a `rounding` column.
  [`grimmer_map()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map.md)
  no longer has `show_reason`: the `reason` column is always returned.
  [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md) on
  [`grimmer_map()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map.md)
  output no longer has a `fail_test2` column.

- [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md) on
  sequence or total-n mapper output returns the per-case summary of
  [`audit_seq()`](https://lhdjung.github.io/scrutiny/reference/audit_seq.md)
  or
  [`audit_total_n()`](https://lhdjung.github.io/scrutiny/reference/audit_seq.md).
  It used to apply the basic mapper’s summary, which counted the
  dispersed rows as reported cases.
  [`audit_seq()`](https://lhdjung.github.io/scrutiny/reference/audit_seq.md)
  and
  [`audit_total_n()`](https://lhdjung.github.io/scrutiny/reference/audit_seq.md)
  are deprecated in favor of
  [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md),
  which returns the same summaries and also has `.by`.
  [`audit_cols_minimal()`](https://lhdjung.github.io/scrutiny/reference/audit_cols_minimal.md)
  lost its `name_test` argument.

- Mappers accept any data frame and return a tibble. Character row
  names, such as study labels, are kept as a `rowname` column, except by
  the sequence mappers, which drop all columns that play no role in the
  test; `case` joins them back.

- [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md),
  [`grimmer()`](https://lhdjung.github.io/scrutiny/reference/grimmer.md),
  and [`debit()`](https://lhdjung.github.io/scrutiny/reference/debit.md)
  lost `show_rec` and `show_reason`, which were only meant for the
  mappers. They are vectorized over `x`, `sd`, `n`, the `digits_*`
  arguments, `items_averaged`, and `data_type`; all other arguments must
  have length 1. Recycling follows the tidyverse rules, so
  `grim(x = c(5.19, 5.18, 5.17), n = c(28, 32), digits_x = 2, items_averaged = 1, data_type = "integer")`
  is an error.

- [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md)
  and
  [`grimmer_map()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map.md)
  no longer multiply `items` into `n`: `n` is the sample size as
  reported, and `items_averaged` is a column of its own.
  [`grim_plot_grid()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_grid.md)
  places tiles at `n * items_averaged`.

- `grim_map(show_rec = TRUE)` returns the same five columns for every
  rounding method: `rec_sum`, `sum_lower`, `sum_upper`, `rec_x_upper`,
  and `rec_x_lower`. The `rec_x_*_rounded*` columns are gone.
  `sum_lower` and `sum_upper` are the smallest and largest whole-number
  sums that would be reported as `x`, and `consistency` is `TRUE`
  exactly if `sum_lower <= sum_upper`, so the columns cannot contradict
  the result. With `x_meaning = "percentage"`, `rec_x_upper` and
  `rec_x_lower` are percentages, like `x`.

- The `sd_binary_*()` functions take `sd_meaning`, as in
  [`debit()`](https://lhdjung.github.io/scrutiny/reference/debit.md),
  and return the population SD under `"sd_pop"` and the standard error
  under `"se"`.
  [`sd_binary_mean_n()`](https://lhdjung.github.io/scrutiny/reference/sd-binary.md)
  takes the mean as `x` instead of `mean`, as the tests do; `mean` still
  works, with a warning.

- [`grim_plot_grid()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_grid.md)
  takes the number of decimal places as `digits_x`, as every other
  function does, and the color of the raster as `color_raster`, like the
  `color_*` arguments of
  [`grim_plot_line()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_line.md).
  The deprecated
  [`grim_plot()`](https://lhdjung.github.io/scrutiny/reference/grim_plot.md)
  keeps its old signature, including `digits` and `raster_color`;
  `color_cons`, `color_incons`, `tile_alpha`, and `raster_alpha` have no
  effect anymore.

- [`split_by_parens()`](https://lhdjung.github.io/scrutiny/reference/split_by_parens.md),
  [`before_parens()`](https://lhdjung.github.io/scrutiny/reference/parens-extractors.md),
  and
  [`inside_parens()`](https://lhdjung.github.io/scrutiny/reference/parens-extractors.md)
  take the pair of delimiters as `delim` instead of `sep`, which is the
  decimal mark in
  [`decimal_places()`](https://lhdjung.github.io/scrutiny/reference/decimal_places.md).
  [`split_by_parens()`](https://lhdjung.github.io/scrutiny/reference/split_by_parens.md)’s
  `check_sep` is now `check_delim`. The old names still work, with a
  warning.

- S3 classes start with `scrutiny_` instead of `scr_`, as in
  `"scrutiny_grim_map"`
  ([\#39](https://github.com/lhdjung/scrutiny/issues/39)).

- [`function_map()`](https://lhdjung.github.io/scrutiny/reference/function_map.md),
  [`function_map_seq()`](https://lhdjung.github.io/scrutiny/reference/function_map_seq.md),
  and
  [`function_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/function_map_total_n.md)
  take the function to map as `.fn` instead of `.fun`.

- [`manage_helper_col()`](https://lhdjung.github.io/scrutiny/reference/manage_helper_col.md)
  takes the argument as `arg` instead of `var_arg`, and `add_col`
  instead of `affix`; the old names still work, with a warning.

- Mapper settings that other functions need later, such as `rounding`,
  are recorded in a `"scrutiny"` attribute instead of S3 classes, which
  are now used for dispatch only. The `"scr_rounding_*"`,
  `"scr_percent_true"`, `"scr_map_seq_disp_nonlinear"`, `"scr_seq_df"`,
  and `"scr_seq_test"` classes are gone.

- In `*_map_seq()` output, `case` is the row number of the value set in
  the input data; it used to count only the inconsistent rows. `n` and
  `items_averaged` are separate columns, also in `*_map_total_n()`
  output, and `n` is dispersed in steps of 1.

- In every function, `rounding` must be a string and `symmetric` a
  single `TRUE` or `FALSE`; a numeric or logical `rounding` was silently
  read as a position in the list of rounding methods.

- [`decimal_places()`](https://lhdjung.github.io/scrutiny/reference/decimal_places.md)
  and
  [`decimal_places_df()`](https://lhdjung.github.io/scrutiny/reference/decimal_places_df.md)
  match `sep` literally, as
  [`restore_zeros()`](https://lhdjung.github.io/scrutiny/reference/restore_zeros.md)
  and
  [`restore_zeros_df()`](https://lhdjung.github.io/scrutiny/reference/restore_zeros.md)
  match `sep_in`, and the defaults are `"."` instead of `"\\."`.

- [`grim_plot_grid()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_grid.md)
  and
  [`debit_plot()`](https://lhdjung.github.io/scrutiny/reference/debit_plot.md)
  return their plot instead of printing it.
  [`grim_plot_grid()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_grid.md)
  has an aspect ratio of 1, which can be undone with
  `ggplot2::theme(aspect.ratio = NULL)`. It also has a new
  `split_by_digits` argument that returns one plot per number of decimal
  places.

- The background raster in
  [`grim_plot_grid()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_grid.md)
  shows consistent cells in grey and inconsistent cells in white, the
  inverse of
  [`grim_plot()`](https://lhdjung.github.io/scrutiny/reference/grim_plot.md)’s
  former default. I acknowledge that this might confuse long-time users
  at first, and that it departs from the color scheme of [Brown and
  Heathers (2017, Fig. 1)](https://doi.org/10.1177/1948550616673876)
  that was the key inspiration for
  [`grim_plot_grid()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_grid.md).
  However, it matches the broader emerging ecosystem of forensic
  meta-science software that ties in with GRIM. In this related work,
  relatively few dark points signify possible value sets against a white
  background. See, e.g., [Hussey (2026) on combining granularity and
  bounds
  methods](https://trustworthyresear.ch/chapters/combined_granularity_and_bounds_methods.html).

- [`debit_plot()`](https://lhdjung.github.io/scrutiny/reference/debit_plot.md)
  lost `line_size`, `show_outer_boxes`, and the `tile_*` arguments.
  Instead, each rectangle is drawn over a point at the reported mean and
  SD, so that value sets with tiny rectangles stay visible. Its labels
  are placed the same way on every run.

- [`duplicate_count_colpair()`](https://lhdjung.github.io/scrutiny/reference/duplicate_count_colpair.md)
  requires at least two columns, and its `rate_y` is the proportion of
  `y` values that occur in `x`, as documented.

### New features

- [`grim_explain()`](https://lhdjung.github.io/scrutiny/reference/grim_explain.md),
  [`grimmer_explain()`](https://lhdjung.github.io/scrutiny/reference/grimmer_explain.md),
  and
  [`debit_explain()`](https://lhdjung.github.io/scrutiny/reference/debit_explain.md)
  lay out the test of a single case in plain words. They are built from
  the numbers that decide the test, so they always agree with
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md),
  [`grimmer()`](https://lhdjung.github.io/scrutiny/reference/grimmer.md),
  and
  [`debit()`](https://lhdjung.github.io/scrutiny/reference/debit.md).
  [`explain_map()`](https://lhdjung.github.io/scrutiny/reference/explain_map.md)
  adds these explanations to mapper output as an `explanation` column,
  using the settings of the mapper call, and
  [`explain_print()`](https://lhdjung.github.io/scrutiny/reference/explain_print.md)
  prints them. Each one names the rounding method it assumes, and its
  first line lists `items_averaged` and `data_type`.

- [`grimmer()`](https://lhdjung.github.io/scrutiny/reference/grimmer.md),
  [`debit()`](https://lhdjung.github.io/scrutiny/reference/debit.md),
  and their mappers, explanations, and plots have a new `sd_meaning`
  argument: what a value reported as an SD really is. `"sd_sample"`, the
  default, is a sample SD, which divides by `n - 1`. `"sd_pop"` is a
  population SD, which divides by `n`, as with NumPy’s `np.std()` or
  Excel’s `STDEV.P`. `"sd_sample_or_pop"` accepts a value set that is
  consistent under either;
  [`grimmer_map()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map.md)’s
  `reason` column names the one that passed. `"se"` is a standard error
  reported as an SD. This test is weak: from about `n = 30` on, nearly
  every value set whose mean passes GRIM is consistent under `"se"`.
  Thanks to Ian Hussey
  ([\#95](https://github.com/lhdjung/scrutiny/issues/95)).

- [`percentage_of()`](https://lhdjung.github.io/scrutiny/reference/percentage_of.md),
  [`percentage_of_map()`](https://lhdjung.github.io/scrutiny/reference/percentage_of_map.md),
  and
  [`percentage_of_map_seq()`](https://lhdjung.github.io/scrutiny/reference/percentage_of_map_seq.md)
  check whether a reported percentage matches a count out of a total.
  [`add_up_to()`](https://lhdjung.github.io/scrutiny/reference/add_up_to.md)
  and
  [`add_up_to_map()`](https://lhdjung.github.io/scrutiny/reference/add_up_to_map.md)
  check whether subgroup sizes sum to a total;
  [`add_up_to_map()`](https://lhdjung.github.io/scrutiny/reference/add_up_to_map.md)
  selects any number of columns with tidyselect.
  [`percentage_of_map()`](https://lhdjung.github.io/scrutiny/reference/percentage_of_map.md)
  and
  [`add_up_to_map()`](https://lhdjung.github.io/scrutiny/reference/add_up_to_map.md)
  have
  [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
  methods.

- [`grim_plot_line()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_line.md),
  [`grimmer_plot_line()`](https://lhdjung.github.io/scrutiny/reference/grimmer_plot_line.md),
  and
  [`debit_plot_line()`](https://lhdjung.github.io/scrutiny/reference/debit_plot_line.md)
  visualize the test of a single value set. Among all means or SDs
  within a range that could be reported with the given number of decimal
  places, they draw a square for each value that is consistent with the
  reported sample size, leave the others out, and indicate the reported
  mean or SD value. Squares are labeled with their values if there is
  space. By default, `zoom = "close"` shows the reported value and the
  nearest attainable one; `zoom = "medium"` widens to the nearest
  attainable one on either side, and `zoom = "wide"` shows a whole unit,
  which is 0% to 100% for a percentage.

- [`grim_plot_rates()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_rates.md)
  plots each study’s rate of GRIM-inconsistent value sets against the
  rate that random means would show.

- [`grim_values()`](https://lhdjung.github.io/scrutiny/reference/grim_values.md)
  returns all means of `n` integers that would be reported as `x`, with
  the whole-number sum behind each, and
  [`grim_closest()`](https://lhdjung.github.io/scrutiny/reference/grim_values.md)
  the achievable mean closest to `x`. Both return a tibble with a `case`
  column and an `items_averaged` column, as
  [`grimmer_values()`](https://lhdjung.github.io/scrutiny/reference/grimmer_values.md)
  and
  [`grimmer_closest()`](https://lhdjung.github.io/scrutiny/reference/grimmer_values.md)
  do ([\#62](https://github.com/lhdjung/scrutiny/issues/62)).

- [`grim_grid()`](https://lhdjung.github.io/scrutiny/reference/grim_grid.md)
  tests every combination of sample size and fractional part of a mean
  with GRIM. It is primarily meant for studying GRIM itself. Its
  inconsistent cells are the background raster of
  [`grim_plot_grid()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_grid.md).
  [`grim_plot_grid()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_grid.md)
  now also draws a raster for the `"up_from"` family of rounding methods
  instead of throwing an error, and has a `threshold` argument, like
  `rounding`, for the raster that `show_data = FALSE` draws on its own.
  Otherwise, the raster follows the mapper call, and a `rounding` or
  `threshold` that disagrees with it is an error.

- [`grimmer_values()`](https://lhdjung.github.io/scrutiny/reference/grimmer_values.md)
  returns every pair of a mean and an SD of `n` integers that would be
  reported as `x` and `sd`, with the sum and sum of squares behind each.
  [`debit_values()`](https://lhdjung.github.io/scrutiny/reference/debit_values.md)
  does the same for binary data.
  [`grimmer_closest()`](https://lhdjung.github.io/scrutiny/reference/grimmer_values.md)
  returns the pair whose SD is closest to `sd`, like
  [`grim_closest()`](https://lhdjung.github.io/scrutiny/reference/grim_values.md)
  for means, and
  [`debit_closest()`](https://lhdjung.github.io/scrutiny/reference/debit_values.md)
  does the same for binary data. This is related to
  [\#59](https://github.com/lhdjung/scrutiny/issues/59) but doesn’t
  conclude the questions raised there. These functions and the
  `*_plot_line()` functions require `data_type`, and `"continuous"` is
  an error there, since they list or draw the values that whole numbers
  can have.

- [`grim_binomial()`](https://lhdjung.github.io/scrutiny/reference/grim_binomial.md)
  tests whether the rate of GRIM-consistent value sets is above what
  random decimal digits would produce, and returns a Clopper-Pearson
  confidence interval.
  [`grim_binomial_power()`](https://lhdjung.github.io/scrutiny/reference/grim_binomial_power.md)
  computes the power of this test.

- [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md) has
  a `.by` argument for summaries by group, as in
  `audit(out, .by = study)`. Output grouped by
  [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
  now gets an error that explains the problem, instead of “no applicable
  method”. Methods in other packages need the dots:
  `audit.my_class <- function(data, ...)`.

- [`scrutiny_meta()`](https://lhdjung.github.io/scrutiny/reference/scrutiny_meta.md)
  returns the settings that a mapper recorded on its output, such as
  `rounding` and `sd_meaning`, so that a report can state the conditions
  of a result. Printed mapper output names the settings that differ from
  the defaults in its header, as in `# GRIM: rounding = "up"`. Combining
  the output of two calls with different settings with
  [`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html)
  is an error, since the rows would be read under the first call’s
  settings, and so is combining it with the rows of a plain data frame;
  adding columns, as with
  [`dplyr::bind_cols()`](https://dplyr.tidyverse.org/reference/bind_cols.html)
  or [`tidyr::nest()`](https://tidyr.tidyverse.org/reference/nest.html),
  works. [`rbind()`](https://rdrr.io/r/base/cbind.html) and joins such
  as
  [`dplyr::full_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
  are not covered and keep the first output’s record;
  [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md) on
  sequence and total-n output rejects the rows they add, and value sets
  that occur twice, as from binding an output to itself.

- New `rounding` strings `"ties_up"`, `"ties_down"`, `"ties_away"`,
  `"ties_zero"`, and `"ties_even"`. Each names a complete tie-breaking
  rule (`"ties_away"` is `"up"` with `symmetric = TRUE`), so combining
  one of these strings with `symmetric` is an error.

- [`function_map()`](https://lhdjung.github.io/scrutiny/reference/function_map.md)
  gains `.args_by_row`, `.args_fixed`, `.cols_helper`, `.cols_derived`,
  a working `.col_names`, and `.reported_variadic` for tests in which
  the data determine the number of key columns
  ([\#42](https://github.com/lhdjung/scrutiny/issues/42)).
  [`function_map_seq()`](https://lhdjung.github.io/scrutiny/reference/function_map_seq.md)
  gains `.var_bounds`, which sets the dispersion limits per variable, so
  that a mean can go below zero while `n` stays at least 1.

- Last but not least, the package gained a hex logo. It is a reference
  to the topos of flying pigs in [Brown and Heathers
  (2017)](https://doi.org/10.1177/1948550616673876), which the authors
  continued as a running joke in their later joint work. Like value sets
  that fail a consistency test, assertions about flying pigs may raise
  concerns about trustworthiness.

### Bug fixes

#### Consistency tests

- A count such as a sample size, item count, or decimal count within
  floating-point noise of a whole number, such as `100 * 0.29`, is used
  as that whole number, as the checks already took it to be. It used to
  be tested as is, which could flip a result, make a case untestable, or
  make
  [`disperse_total()`](https://lhdjung.github.io/scrutiny/reference/disperse.md)
  and the total-n mappers treat an even total as odd: `40 - 1e-9` was
  split into 19.5 and 20.5 instead of 20 and 20. This applies to the
  tests and their mappers,
  [`grim_exp_incon()`](https://lhdjung.github.io/scrutiny/reference/grim-stats.md),
  [`grim_total()`](https://lhdjung.github.io/scrutiny/reference/grim-stats.md),
  the `sd_binary_*()` functions,
  [`disperse()`](https://lhdjung.github.io/scrutiny/reference/disperse.md),
  [`disperse2()`](https://lhdjung.github.io/scrutiny/reference/disperse.md),
  and
  [`disperse_total()`](https://lhdjung.github.io/scrutiny/reference/disperse.md),
  including their `dispersion` steps, and the steps of
  [`seq_disperse()`](https://lhdjung.github.io/scrutiny/reference/seq_disperse.md)
  and the sequence mappers.

- [`debit()`](https://lhdjung.github.io/scrutiny/reference/debit.md) no
  longer reports consistent binary data with a mean of exactly `0.50` as
  inconsistent, and returns a result for means of exactly 0 and 1, which
  used to be `NA`. Under `formula = "mean_n"`, a bound of the mean’s
  range that the rounding method excludes no longer counts as attained:
  `debit(0.05, 0.20, 25, 2, 2, "integer", formula = "mean_n", rounding = "ceiling")`
  is `FALSE`, since every mean above 0.04 has an SD above 0.20.

- Under `rounding = "up"` or `"down"`, GRIMMER’s SD check and parity
  check now apply to the same candidate sum of squares, so
  `grimmer(x = 0.11, sd = 0.87, digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer", n = 64, rounding = "up")`
  is `FALSE`. Thanks to Ian Hussey
  ([\#85](https://github.com/lhdjung/scrutiny/issues/85)).

- [`grimmer()`](https://lhdjung.github.io/scrutiny/reference/grimmer.md)
  gives the reason `"Negative SD"` for a negative `sd`. It throws an
  error if it would have to test more than a million candidate sums,
  which takes an `n` of more than a million. Means of 1e7 and more used
  to get wrong results, since the sums of squares passed 2^53, beyond
  which a double no longer holds every whole number.

- [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md) no
  longer counts a case without a test result as inconsistent, and
  computes `exp_incon`, `ratio`, `mean_x`, and `mean_sd` over the cases
  with a result instead of returning `NA` if one has none. A rate or
  mean over no cases is `NA`, not `NaN`.

#### Sequence and total-n mappers

- An error in a row of `data` names that row instead of coming wrapped
  in purrr’s “In index” context. In the total-n mappers, the row is that
  of the reported values, not of a hypothetical scenario.

- [`reverse_map_seq()`](https://lhdjung.github.io/scrutiny/reference/reverse_map_seq.md)
  and [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
  on sequence output reconstruct the reported values from the `diff_var`
  column. They used to infer them from the gap in the sequence, which
  failed when `out_min` or `out_max` cut it short, so that
  [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
  could report the opposite result. Both now accept any `dispersion`,
  not only a linearly increasing one.

- [`reverse_map_seq()`](https://lhdjung.github.io/scrutiny/reference/reverse_map_seq.md)
  finds the tested columns by name. After `rename()`, it used to return
  a dispersed value as the reported one; a renamed or removed column is
  now an error.

- [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md) on
  sequence output reports the results that the mapper recorded instead
  of re-testing reconstructed data, which could drop `percent` (now
  `x_meaning`), `threshold`, or `symmetric` and flip the result. It
  works with a `dispersion` of length 1, with output that has no rows,
  and with cases whose sequence `out_min` or `out_max` emptied. A
  consistent reported value set included by `include_reported = TRUE`
  counts once in `hits_total`, not once per dispersed variable. A count
  or distance that depends on an `NA` result, on a variable whose
  reported value is missing and so was not dispersed, or on rows removed
  from the output, as by
  [`filter()`](https://rdrr.io/r/stats/filter.html), is `NA` rather than
  counting such value sets as misses. So does
  [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md) on
  total-n output for rows removed from a direction. A `case` column
  turned into strings is read as case numbers again, so that the rows
  come out in the order 1, 2, …, 10 rather than 1, 10, 2.

- `*_map_seq()` functions disperse means and SDs down to zero, and means
  below it; they used to stop one decimal unit above zero. With
  `include_consistent = TRUE`, they skip cases without a test result. If
  there is nothing to disperse, they return a tibble with no rows but
  the usual columns, not one without columns.

- [`seq_disperse()`](https://lhdjung.github.io/scrutiny/reference/seq_disperse.md)
  rounds its values to the decimal level of `by`, so that long sequences
  don’t accumulate floating-point error
  ([\#83](https://github.com/lhdjung/scrutiny/issues/83)). It applies
  `out_min` and `out_max` to `from` as well, and no longer includes
  `from` twice when `dispersion` contains 0. `out_max = "auto"` is no
  maximum, like the default `NULL`, so that it works as in the sequence
  mappers.

- `*_map_total_n()` functions use an `items_averaged` column, and
  [`reverse_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/reverse_map_total_n.md)
  reconstructs the correct total for any `dispersion`. Functions made by
  [`function_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/function_map_total_n.md)
  work when created in another package
  ([\#69](https://github.com/lhdjung/scrutiny/issues/69)). An even total
  split into two equal groups is tested once rather than twice, so
  [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md) on
  total-n output no longer counts one split too many. It leaves
  undecidable splits out of `scenarios_total`, its hits are `NA` where
  an undecidable split might be one, and its `hit_rate` is `NA` for a
  case with no decidable split.

- `*_map_total_n()` functions return the other columns of `data`, and
  warn about a total that leaves no pair of group sizes within `n_min`
  and `n_max` instead of failing. A helper argument such as
  `items_averaged` must be a single value there; a vector was paired
  with the wrong groups in the `"back"` direction. The `constant` values
  stay with their groups in the `"back"` direction, too. Input columns
  named like the result columns (`n_change`, `both_consistent`, `case`,
  `dir`, and `constant` if given) or the reported values (`x`, `sd`) are
  an error instead of being overwritten, and so is a `constant` named
  like a column of the output. Grouped input is tested like ungrouped
  input.

- [`grimmer_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map_seq.md)
  and
  [`debit_map_seq()`](https://lhdjung.github.io/scrutiny/reference/debit_map_seq.md)
  no longer disperse `n` below 2, and
  [`grimmer_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map_total_n.md)
  and
  [`debit_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/debit_map_total_n.md)
  default to `n_min = 2`, since a group of 1 has no sample SD. Under
  `sd_meaning = "sd_pop"` or `"sd_sample_or_pop"`, where it has one,
  both tiers go down to 1.

#### Decimal places

- [`decimal_places()`](https://lhdjung.github.io/scrutiny/reference/decimal_places.md)
  handles scientific notation, counts numeric values correctly when
  `sep` is not `"."`, and returns `NA` for `NaN`, infinite values, and
  the string `"NA"`.

- [`restore_zeros()`](https://lhdjung.github.io/scrutiny/reference/restore_zeros.md)
  handles scientific notation, no longer introduces floating-point noise
  at high widths, and returns `NA` instead of `"NA"`. It rejects a
  negative `width` and reports values that exceed a per-value `width`.
  [`restore_zeros_df()`](https://lhdjung.github.io/scrutiny/reference/restore_zeros.md)
  and
  [`decimal_places_df()`](https://lhdjung.github.io/scrutiny/reference/decimal_places_df.md)
  handle columns that contain only `NA`, and recognize columns whose
  decimal separator is `sep_in` or `sep`.
  [`restore_zeros_df()`](https://lhdjung.github.io/scrutiny/reference/restore_zeros.md)
  no longer warns about a column without decimal places, such as `n`, or
  a single row.

- [`decimal_places()`](https://lhdjung.github.io/scrutiny/reference/decimal_places.md)
  no longer depends on `options(OutDec = )`. With a decimal comma, it
  used to count no decimal places in any number, which also misled
  [`restore_zeros()`](https://lhdjung.github.io/scrutiny/reference/restore_zeros.md),
  [`seq_disperse()`](https://lhdjung.github.io/scrutiny/reference/seq_disperse.md),
  the sequence mappers, and
  [`duplicate_count()`](https://lhdjung.github.io/scrutiny/reference/duplicate_count.md).

#### Other

- [`split_by_parens()`](https://lhdjung.github.io/scrutiny/reference/split_by_parens.md),
  [`before_parens()`](https://lhdjung.github.io/scrutiny/reference/parens-extractors.md),
  and
  [`inside_parens()`](https://lhdjung.github.io/scrutiny/reference/parens-extractors.md)
  split each string separately; a string with an unusual number of
  separators used to shift the values of all later rows. Custom
  separators are matched literally.
  [`split_by_parens()`](https://lhdjung.github.io/scrutiny/reference/split_by_parens.md)
  errors instead of overwriting a column, handles `NA` and input without
  rows or columns, only warns about unsplit columns if `cols` is given,
  but always about columns split under `check_delim = FALSE`, never
  about columns with no values, and keeps the column order with
  `transform = TRUE`.

- [`row_to_colnames()`](https://lhdjung.github.io/scrutiny/reference/row_to_colnames.md)
  keeps a one-column data frame a data frame, and rejects a header that
  repeats, such as “M, SD, M, SD”. It skips blank header cells and trims
  whitespace, so that a blank cell above “Study” no longer gives the
  name ” Study”.

- [`duplicate_tally()`](https://lhdjung.github.io/scrutiny/reference/duplicate_tally.md)
  no longer counts missing values as matches of every other value, runs
  in linear rather than quadratic time, and treats `NaN` as missing.
  [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
  finds its tally columns by the names
  [`duplicate_tally()`](https://lhdjung.github.io/scrutiny/reference/duplicate_tally.md)
  gave them; it took every second column, so a selection or reordering
  summarized the wrong ones.
  [`duplicate_count()`](https://lhdjung.github.io/scrutiny/reference/duplicate_count.md)
  is linear rather than quadratic for named input, uses a named vector’s
  names as `locations`, drops unused factor levels, and works on grouped
  tibbles.
  [`duplicate_count_colpair()`](https://lhdjung.github.io/scrutiny/reference/duplicate_count_colpair.md)
  compares values as strings. All `duplicate_*()` functions write
  numbers out in full, so `100000` is the same value as `100000L` and
  `"100000"`, not `"1e+05"`. A list-column or a matrix column is an
  error.
  [`duplicate_count()`](https://lhdjung.github.io/scrutiny/reference/duplicate_count.md)
  and
  [`duplicate_tally()`](https://lhdjung.github.io/scrutiny/reference/duplicate_tally.md)
  keep the column names of a matrix, and
  [`duplicate_count_colpair()`](https://lhdjung.github.io/scrutiny/reference/duplicate_count_colpair.md)
  takes one. Blank strings, `"NA"`, and `"NaN"` are missing values
  there, as in the other string-reading functions, so the blank cells of
  a table read with
  [`read.csv()`](https://rdrr.io/r/utils/read.table.html) are not
  counted as duplicates.

- [`is_seq_linear()`](https://lhdjung.github.io/scrutiny/reference/seq-predicates.md)
  returns `FALSE` for sequences such as `c(1, 2, 1)`. The `is_seq_*()`
  functions fill gaps left by `NA` values using the step size implied by
  the known values, return `NA` for a vector with `NA`s only if the
  answer depends on the missing values (any one or two values are
  linear), and test strings and factors by their numeric values.
  Anything other than a numeric, character, or factor vector is an
  error, such as a list, a data frame, or a logical vector with values
  other than `NA`. `tolerance = 0` no longer makes every comparison
  fail, and the tolerance is never finer than the rounding error of
  arithmetic on the values, so
  [`seq_disperse()`](https://lhdjung.github.io/scrutiny/reference/seq_disperse.md)
  output around values of 1e8 and more tests as linear and dispersed. A
  missing string counts as a missing number, as does the string `"NA"`.

- [`grim_plot_grid()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_grid.md)
  plots negative means at the fractional part of their absolute value,
  plots percentages with the decimal places they were tested with, works
  with `show_raster = FALSE`, extends its x-axis to the data when `n` is
  not given, and warns about any rows it leaves out instead of
  suppressing all warnings. It only says “Also visualizing” GRIMMER
  inconsistencies if there are GRIM inconsistencies as well.

- [`debit_plot()`](https://lhdjung.github.io/scrutiny/reference/debit_plot.md)
  draws the DEBIT lines for the smallest and largest sample size and
  shades the band between them; it used to draw a single line from a
  recycled `n` column. Its x-axis is labeled as the mean, and `data`
  without rows is an error.

- A key column name such as `x = "mean"` can be passed in a variable or
  through [`lapply()`](https://rdrr.io/r/base/lapply.html) and
  [`purrr::map()`](https://purrr.tidyverse.org/reference/map.html).
  `.name_key_result` works in all three function factories.
  [`function_map_seq()`](https://lhdjung.github.io/scrutiny/reference/function_map_seq.md)
  works for a mapper with a single reported variable.
  [`is_map_df()`](https://lhdjung.github.io/scrutiny/reference/data-frame-predicates.md)
  and the related predicates recognize only scrutiny’s own classes; they
  used to accept any class ending in `_map`.

- [`reverse_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/reverse_map_total_n.md)
  rejects mapper output with rows removed or reordered.
  [`disperse()`](https://lhdjung.github.io/scrutiny/reference/disperse.md)
  rejects a missing `n_max`, and
  [`disperse2()`](https://lhdjung.github.io/scrutiny/reference/disperse.md)
  applies `n_min` and `n_max` to the group sizes rather than to their
  midpoint, and disperses from two numbers that are not consecutive as
  given rather than from their mean, which made the group sizes
  fractional.
  [`seq_disperse()`](https://lhdjung.github.io/scrutiny/reference/seq_disperse.md)
  disperses a factor by its label. The `sd_binary_*()` functions return
  `NA` for `n` below 2 and for a fractional `n` or count, reject
  non-numeric input, and recycle their arguments by the tidyverse rules,
  as do
  [`grim_granularity()`](https://lhdjung.github.io/scrutiny/reference/grim_granularity.md)
  and
  [`grim_items()`](https://lhdjung.github.io/scrutiny/reference/grim_items.md).

- [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md) and
  [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md)
  no longer fail when `n` and `items_averaged` are integers whose
  product exceeds the integer range.

- Errors about an invalid `rounding` or `symmetric` name the function
  that the user called. Many errors from base R or from internal code
  were replaced by messages that name the argument at fault. A factor
  where a number is expected, as from
  `read.csv(stringsAsFactors = TRUE)`, is an error that says how to
  convert it; it used to be read by its level codes, or gave `NA` with
  warnings.
  [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md) and
  [`grim_plot_grid()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_grid.md)
  name the columns they need if any were dropped.

- [`restore_zeros_df()`](https://lhdjung.github.io/scrutiny/reference/restore_zeros.md),
  [`decimal_places_df()`](https://lhdjung.github.io/scrutiny/reference/decimal_places_df.md),
  and
  [`split_by_parens()`](https://lhdjung.github.io/scrutiny/reference/split_by_parens.md)
  ignore grouping and return ungrouped data; grouped input used to fail
  or leave columns unchanged.
  [`is_numeric_like()`](https://lhdjung.github.io/scrutiny/reference/is_numeric_like.md)
  is `FALSE` for dates and times, which
  [`restore_zeros_df()`](https://lhdjung.github.io/scrutiny/reference/restore_zeros.md)
  used to turn into `NA`, reads a number next to a non-breaking space,
  and takes the strings `"NA"` and `"NaN"` as missing values, so that
  the `*_df()` functions no longer skip a column that has them. A blank
  string, as [`read.csv()`](https://rdrr.io/r/utils/read.table.html)
  leaves in a character column, is missing too:
  [`restore_zeros_df()`](https://lhdjung.github.io/scrutiny/reference/restore_zeros.md)
  used to skip a column with a blank cell without a word.
  [`decimal_places()`](https://lhdjung.github.io/scrutiny/reference/decimal_places.md)
  gives it `NA` decimal places, not 0, and
  [`restore_zeros()`](https://lhdjung.github.io/scrutiny/reference/restore_zeros.md)
  makes it `NA` without a warning.
  [`before_parens()`](https://lhdjung.github.io/scrutiny/reference/parens-extractors.md)
  returns `NA` for a blank part, as
  [`inside_parens()`](https://lhdjung.github.io/scrutiny/reference/parens-extractors.md)
  does, so that
  [`split_by_parens()`](https://lhdjung.github.io/scrutiny/reference/split_by_parens.md)
  splits a blank cell into two missing values. Both take a part `"NA"`
  or `"NaN"` as missing, and
  [`row_to_colnames()`](https://lhdjung.github.io/scrutiny/reference/row_to_colnames.md)
  skips it like a blank cell.

- [`decimal_places_df()`](https://lhdjung.github.io/scrutiny/reference/decimal_places_df.md)
  and
  [`split_by_parens()`](https://lhdjung.github.io/scrutiny/reference/split_by_parens.md)
  take a matrix, as
  [`restore_zeros_df()`](https://lhdjung.github.io/scrutiny/reference/restore_zeros.md)
  does, and name the function for vectors when given one.

- [`decimal_places_df()`](https://lhdjung.github.io/scrutiny/reference/decimal_places_df.md)
  and
  [`restore_zeros_df()`](https://lhdjung.github.io/scrutiny/reference/restore_zeros.md)
  warn about a column that isn’t numeric-like if `cols` is given, not
  under the default, and always if it was processed under
  `check_numeric_like = FALSE`, but never about a column with no values.

- A column named like an argument, such as `sep` or `width`, no longer
  replaces that argument in
  [`restore_zeros_df()`](https://lhdjung.github.io/scrutiny/reference/restore_zeros.md),
  [`decimal_places_df()`](https://lhdjung.github.io/scrutiny/reference/decimal_places_df.md),
  and
  [`split_by_parens()`](https://lhdjung.github.io/scrutiny/reference/split_by_parens.md).
  [`seq_disperse_df()`](https://lhdjung.github.io/scrutiny/reference/seq_disperse.md)
  finds a caller’s variable named `x` or `diff_var`. The `is_seq_*()`
  functions test integers as doubles, so that their differences don’t
  overflow.

- A 64-bit integer (`integer64`) vector, as databases return for counts,
  is an error that points to
  [`as.numeric()`](https://rdrr.io/r/base/numeric.html). It used to be
  read as numbers near zero, so that every test returned `NA`.

- The mappers test a numeric column with a class, such as one from
  [`tibble::num()`](https://tibble.tidyverse.org/reference/num.html), as
  plain numbers. The sequence mappers failed on such an `x`, and every
  mapper on such an `n`.

- [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md)
  is about 2.5 times as fast, and
  [`grimmer_map()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map.md)
  about 80 times.

### Lifecycle changes

- The rounding functions moved to the roundwork package, which scrutiny
  now imports: `reround()`, `unround()`, `reround_to_fraction()`,
  `reround_to_fraction_level()`, `rounding_bias()`, `anti_trunc()`, and
  the `round_*()` functions. Call them from roundwork, as in
  [`roundwork::round_up()`](https://lhdjung.github.io/roundwork/reference/rounding-common.html);
  see [roundwork’s
  changelog](https://lhdjung.github.io/roundwork/news/index.html) for
  their changes. The vignettes [Rounding
  options](https://lhdjung.github.io/roundwork/articles/rounding-options.html)
  and [Rounding in
  depth](https://lhdjung.github.io/roundwork/articles/rounding-in-depth.html)
  moved with them. Errors about `rounding`, `threshold`, and `symmetric`
  still name the scrutiny function that was called, and are now raised
  even if `data` has no rows.

#### Deprecated

- [`grim_plot()`](https://lhdjung.github.io/scrutiny/reference/grim_plot.md)
  is deprecated in favor of
  [`grim_plot_grid()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_grid.md),
  which tells it apart from
  [`grim_plot_line()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_line.md)
  and
  [`grim_plot_rates()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_rates.md).

- [`grim_items()`](https://lhdjung.github.io/scrutiny/reference/grim_items.md)
  is deprecated. It only computed `1 / (n * gran)`.

- [`grim_probability()`](https://lhdjung.github.io/scrutiny/reference/grim_probability.md)
  is deprecated in favor of
  [`grim_exp_incon()`](https://lhdjung.github.io/scrutiny/reference/grim-stats.md).

- [`audit_seq()`](https://lhdjung.github.io/scrutiny/reference/audit_seq.md)
  and
  [`audit_total_n()`](https://lhdjung.github.io/scrutiny/reference/audit_seq.md)
  are deprecated in favor of
  [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md),
  which returns the same summaries.

- [`restore_zeros()`](https://lhdjung.github.io/scrutiny/reference/restore_zeros.md)
  and
  [`restore_zeros_df()`](https://lhdjung.github.io/scrutiny/reference/restore_zeros.md)
  are deprecated. They put trailing zeros back into numbers as strings,
  which the tests no longer take. Give the number of decimal places as
  `digits_x` or `digits_sd` instead, and format numbers for display with
  `formatC(x, format = "f", digits = 2)`.

- [`decimal_places_df()`](https://lhdjung.github.io/scrutiny/reference/decimal_places_df.md)
  is deprecated, since it replaces the values by their counts, while the
  tests need both. Use
  [`decimal_places()`](https://lhdjung.github.io/scrutiny/reference/decimal_places.md)
  in
  [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
  instead, as in `mutate(digits_x = decimal_places(x))`.

- [`is_numeric_like()`](https://lhdjung.github.io/scrutiny/reference/is_numeric_like.md)
  is deprecated, since scrutiny no longer takes numbers as strings, and
  so are
  [`is_seq_linear()`](https://lhdjung.github.io/scrutiny/reference/seq-predicates.md),
  [`is_seq_ascending()`](https://lhdjung.github.io/scrutiny/reference/seq-predicates.md),
  [`is_seq_descending()`](https://lhdjung.github.io/scrutiny/reference/seq-predicates.md),
  and
  [`is_seq_dispersed()`](https://lhdjung.github.io/scrutiny/reference/seq-predicates.md),
  which scrutiny no longer uses: the sequence mappers record how far
  each value was moved in `diff_var`. These functions may be removed in
  a future version.

#### Superseded

- [`sd_binary_0_n()`](https://lhdjung.github.io/scrutiny/reference/sd-binary.md)
  and
  [`sd_binary_1_n()`](https://lhdjung.github.io/scrutiny/reference/sd-binary.md)
  are superseded by
  [`sd_binary_groups()`](https://lhdjung.github.io/scrutiny/reference/sd-binary.md):
  `sd_binary_0_n(group_0, n)` is
  `sd_binary_groups(group_0, n - group_0)`.

- [`absorb_key_args()`](https://lhdjung.github.io/scrutiny/reference/absorb_key_args.md),
  [`check_mapper_input_colnames()`](https://lhdjung.github.io/scrutiny/reference/check_mapper_input_colnames.md),
  and
  [`manage_helper_col()`](https://lhdjung.github.io/scrutiny/reference/manage_helper_col.md)
  are superseded by
  [`function_map()`](https://lhdjung.github.io/scrutiny/reference/function_map.md),
  whose mappers do what these helpers do for a handwritten one, such as
  managing helper columns through `.cols_helper`.

#### Removed

- Removed after their deprecation in 0.5.0:
  [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md)’s
  `show_prob`, `grim_ratio_upper()`, and the 15 functions around
  `is_subset_of()`.

- Removed: `grim_ratio()` (use
  [`grim_exp_incon()`](https://lhdjung.github.io/scrutiny/reference/grim-stats.md),
  which takes the rounding method into account), `manage_key_colnames()`
  (use
  [`absorb_key_args()`](https://lhdjung.github.io/scrutiny/reference/absorb_key_args.md)),
  `unnest_consistency_cols()`, `write_doc_factory_map_conventions()`,
  and the re-exports of rlang’s `enquo()`, `enquos()`, `as_name()`,
  `as_label()`, `.data`, and `:=`. `check_audit_special()` was removed,
  since
  [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md) now
  reaches
  [`audit_seq()`](https://lhdjung.github.io/scrutiny/reference/audit_seq.md)
  and
  [`audit_total_n()`](https://lhdjung.github.io/scrutiny/reference/audit_seq.md)
  itself.

- Removed in the sense of no longer exported: `check_factory_dots()`,
  `check_args_disabled()`, `decimal_places_scalar()`,
  `write_doc_audit()`, `write_doc_audit_seq()`, and
  `write_doc_audit_total_n()`.

- `duplicate_detect()`, superseded since 0.3.0, was removed in favor of
  [`duplicate_tally()`](https://lhdjung.github.io/scrutiny/reference/duplicate_tally.md).

- [`debit_plot()`](https://lhdjung.github.io/scrutiny/reference/debit_plot.md)
  and
  [`grim_plot_grid()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_grid.md)
  map `consistency` to one scale for `colour` and `fill`, so that
  [`ggplot2::scale_colour_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.html)
  with `aesthetics = c("colour", "fill")` restyles them. Consistent and
  inconsistent value sets are a stronger blue and red than before, as in
  the new
  [`grim_plot_line()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_line.md)
  and
  [`grimmer_plot_line()`](https://lhdjung.github.io/scrutiny/reference/grimmer_plot_line.md).
  Values that are consistent but were not reported are grey, like the
  consistent cells of
  [`grim_plot_grid()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_grid.md)’s
  raster. The arguments for fixed styles were removed:
  [`debit_plot()`](https://lhdjung.github.io/scrutiny/reference/debit_plot.md)’s
  `color_cons`, `color_incons`, `rect_alpha`, `line_*`, and `label_*`,
  and
  [`grim_plot_grid()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_grid.md)’s
  `color_cons`, `color_incons`, `tile_alpha`, and `raster_alpha`.

- [`seq_disperse()`](https://lhdjung.github.io/scrutiny/reference/seq_disperse.md)
  and
  [`seq_disperse_df()`](https://lhdjung.github.io/scrutiny/reference/seq_disperse.md)
  return numbers, not strings, and lost their `string_output` argument.
  A string `from` still sets the step size by its decimal places,
  trailing zeros included.

- `seq_length()` and `seq_length<-` were removed; their names clashed
  with [`seq_len()`](https://rdrr.io/r/base/seq.html). So were
  `seq_endpoint()`, `seq_distance()`, `seq_endpoint_df()`,
  `seq_distance_df()`, and `seq_test_ranking()`, which made decimal
  strings for mappers that no longer take strings. Use
  `seq(from, to, by = 10^-digits)`.

- Removed after their deprecation in 0.3.1:
  [`seq_disperse()`](https://lhdjung.github.io/scrutiny/reference/seq_disperse.md)’s
  `track_var_change` and
  [`seq_disperse_df()`](https://lhdjung.github.io/scrutiny/reference/seq_disperse.md)’s
  `.track_var_change`. Also,
  [`restore_zeros_df()`](https://lhdjung.github.io/scrutiny/reference/restore_zeros.md)
  and
  [`split_by_parens()`](https://lhdjung.github.io/scrutiny/reference/split_by_parens.md)
  lost `...`, which only caught argument names from before 0.2.4.

- [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md),
  [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md),
  [`grimmer()`](https://lhdjung.github.io/scrutiny/reference/grimmer.md),
  and
  [`grimmer_map()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map.md)
  lost `tolerance` as both tests now compare whole numbers exactly.
  GRIMMER’s test 2 no longer rounds reconstructed SDs, since every sum
  of squares that passes test 1 passes it, so
  `"GRIMMER inconsistent (test 2)"` is no longer a possible `reason`.

- [`function_map()`](https://lhdjung.github.io/scrutiny/reference/function_map.md)
  lost `.col_control` and `.col_filler`, which never worked.

- [`disperse()`](https://lhdjung.github.io/scrutiny/reference/disperse.md),
  [`disperse2()`](https://lhdjung.github.io/scrutiny/reference/disperse.md),
  [`disperse_total()`](https://lhdjung.github.io/scrutiny/reference/disperse.md),
  and the `*_map_total_n()` functions lost `constant_index`, and
  [`function_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/function_map_total_n.md)
  lost `.constant_index`. The `constant` columns always follow
  `n_change`; move them with
  [`dplyr::relocate()`](https://dplyr.tidyverse.org/reference/relocate.html).

- scrutiny uses the base pipe `|>`, no longer exports the magrittr pipe
  `%>%`, and requires R \>= 4.3.0.

- scrutiny requires purrr \>= 1.0.0
  ([\#87](https://github.com/lhdjung/scrutiny/issues/87)), dplyr \>=
  1.1.0, ggplot2 \>= 3.4.0, and cli \>= 3.4.0. It no longer depends on
  corrr, and it suggests ggrepel instead of importing it. When using
  ggrepel, it needs a version \>= 0.9.3.

### Documentation

- [`vignette("technical-notes")`](https://lhdjung.github.io/scrutiny/articles/technical-notes.md)
  is new. It describes how the tests treat missing values, including the
  few cases that stay `NA` although every value would give the same
  result, and the rounding, exact arithmetic, and decimal places behind
  the results.

- The GRIM and GRIMMER vignettes were each split in two.
  [`vignette("grim")`](https://lhdjung.github.io/scrutiny/articles/grim.md)
  and
  [`vignette("grimmer")`](https://lhdjung.github.io/scrutiny/articles/grimmer.md)
  cover the basics, including how to explain a result with
  [`explain_print()`](https://lhdjung.github.io/scrutiny/reference/explain_print.md);
  [`vignette("grim-in-depth")`](https://lhdjung.github.io/scrutiny/articles/grim-in-depth.md)
  and
  [`vignette("grimmer-in-depth")`](https://lhdjung.github.io/scrutiny/articles/grimmer-in-depth.md)
  cover the values behind a result, the sequence and total-n mappers,
  and, for GRIM, the GRIM statistics.

- [`vignette("consistency-tests-in-depth")`](https://lhdjung.github.io/scrutiny/articles/consistency-tests-in-depth.md)
  was updated ([\#74](https://github.com/lhdjung/scrutiny/issues/74)),
  the GRIMMER documentation was improved
  ([\#66](https://github.com/lhdjung/scrutiny/issues/66)), and “Related
  software” has new entries.

- `vignette("consistency-tests-simple")` was removed, since
  [`vignette("consistency-tests-in-depth")`](https://lhdjung.github.io/scrutiny/articles/consistency-tests-in-depth.md)
  covers the same steps. `vignette("devtools")` was split up: the SDs of
  binary data are now in
  [`vignette("debit")`](https://lhdjung.github.io/scrutiny/articles/debit.md),
  and
  [`decimal_places()`](https://lhdjung.github.io/scrutiny/reference/decimal_places.md),
  [`seq_disperse()`](https://lhdjung.github.io/scrutiny/reference/seq_disperse.md),
  and
  [`disperse_total()`](https://lhdjung.github.io/scrutiny/reference/disperse.md)
  in
  [`vignette("consistency-tests-in-depth")`](https://lhdjung.github.io/scrutiny/articles/consistency-tests-in-depth.md).

## scrutiny 0.6.1

CRAN release: 2025-12-02

This is a fix for compatibility with testthat 3.3.0. Also:

### Bugfixes

- Fixed a bug in
  [`grimmer_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map_total_n.md),
  [`debit_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/debit_map_total_n.md),
  and the underlying
  [`function_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/function_map_total_n.md)
  ([\#81](https://github.com/lhdjung/scrutiny/issues/81)). This could
  also have lead to incorrect results, but only in total-N mapper
  functions with two or more key arguments beyond `n`, so the bug never
  affected
  [`grim_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/grim_map_total_n.md).

- Added a warning whenever GRIMMER runs. I recently learned about a bug
  that can lead to false positives in test 3 of the current GRIMMER
  implementation; see
  [\#80](https://github.com/lhdjung/scrutiny/issues/80). The bug will be
  fixed in the next version of scrutiny (and the warning will be
  removed).

### Minor changes

- Updated the [“Related
  software”](https://lhdjung.github.io/scrutiny/articles/related.html)
  article to reflect recent additions to the forensic metascience
  toolkit. Now, it also attempts to roughly categorize tools by usage.

## scrutiny 0.6.0

CRAN release: 2025-08-22

### Bugfixes

- Fixed a bug in the DEBIT functions that could sometimes have resulted
  in `consistency` being `FALSE` when it should have been `TRUE` (thanks
  to [@nrposner](https://github.com/nrposner),
  [\#75](https://github.com/lhdjung/scrutiny/issues/75)). However, this
  seems to be a rare issue, and DEBIT is not widely used in any case.

- Fixed a bug that could theoretically lead
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md),
  [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md),
  [`grim_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grim_map_seq.md),
  and
  [`grim_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/grim_map_total_n.md)
  to throw a warning and possibly even return incorrect results (also
  [@nrposner](https://github.com/nrposner),
  [\#75](https://github.com/lhdjung/scrutiny/issues/75)). However, this
  is even less realistic than the previous bug.

- Compatibility with ggplot2 4.0.0 was ensured
  ([@teunbrand](https://github.com/teunbrand),
  [\#78](https://github.com/lhdjung/scrutiny/issues/78)).

- [`restore_zeros()`](https://lhdjung.github.io/scrutiny/reference/restore_zeros.md)
  now checks `width` more strictly:

  - It no longer truncates decimal numbers if `width` is specified but
    some elements of `x` have more decimal places than that. For
    example, in earlier versions,
    `restore_zeros(c(0.12, 0.123, 0.1234), width = 2)` would have
    returned `c("0.120", "0.123", "0.123")`: it silently cut off the `4`
    from the last value. An error is now thrown in such cases.
  - Also, `width` is now checked to be either a single whole number or a
    vector of whole numbers with the same length as `x`.

- [`restore_zeros_df()`](https://lhdjung.github.io/scrutiny/reference/restore_zeros.md)
  has the same fixes as above.

- [`is_seq_dispersed()`](https://lhdjung.github.io/scrutiny/reference/seq-predicates.md)
  now works correctly if `NA` values are present.

### Lifecycle updates

Currently none. However, the next major or minor version will introduce
breaking changes, removals (as announced in scrutiny 0.5.0 below), and
possibly more deprecations.

## scrutiny 0.5.0

CRAN release: 2024-09-22

The package is now released under the MIT license.

### Breaking changes

- The `ratio` column in the output of
  [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md)
  and
  [`grim_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grim_map_seq.md)
  was replaced by a `probability` column. This means:

  - Numerically, the only difference is that `probability` is zero
    whenever `ratio` was negative.

  - Conceptually, it is much easier to interpret: it is the probability
    that a reported mean or percentage of integer data that has a
    specific number of decimal places but is otherwise random is
    GRIM-inconsistent with the reported sample size.

    For example, `probability` is `0.6` for a mean of `1.23` and a
    sample size of `40`. The same is true for any other mean with two
    decimal places. Thus, a randomly chosen mean with two decimal
    places, ostensibly derived from integer data, has a 0.6 probability
    of being GRIM-inconsistent with the reported sample size.

- In the functions around `grim_ratio()`, the `x` argument must now be a
  string. This is consistent with
  [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md),
  `unround()`, etc.; and it prevents erroneous results that could
  previously occur by omitting trailing zeros.

- The GRIMMER implementation was debugged, so that
  [`grimmer_map()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map.md)
  etc. may now yield different results in a few cases. In particular,
  the `items` argument now works correctly, thanks to Aurélien Allard
  and Lukas Wallrich
  ([\#58](https://github.com/lhdjung/scrutiny/issues/58)).

- [`is_seq_dispersed()`](https://lhdjung.github.io/scrutiny/reference/seq-predicates.md)
  now correctly returns `FALSE` if different numbers of missing values
  at the start and end of `x` mean that `x` cannot be dispersed around
  `from`.

### New features

- The `probability` column (see above) is created by a new function,
  [`grim_probability()`](https://lhdjung.github.io/scrutiny/reference/grim_probability.md).

### Lifecycle updates

#### Deprecated

- As a consequence of the above, the `show_prob` argument of
  [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md)
  is now deprecated and will be removed in a future version. It no
  longer has any effect.

- `grim_ratio_upper()` is deprecated and will be removed in a future
  version. It no longer seems very interesting (and likely never was),
  especially now that the GRIM ratio in general has taken a backseat.

- All 15 (!) functions around `is_subset_of()` are deprecated and will
  be removed in a future version. In truth, they were always poorly
  written and widely out of scope for scrutiny.

#### Removed

All of these had been deprecated since scrutiny 0.3.0:

- `audit_list()` was removed.

- The `sep` argument in
  [`restore_zeros()`](https://lhdjung.github.io/scrutiny/reference/restore_zeros.md)
  and
  [`restore_zeros_df()`](https://lhdjung.github.io/scrutiny/reference/restore_zeros.md)
  was removed.

- The `numeric_only` argument in
  [`duplicate_count()`](https://lhdjung.github.io/scrutiny/reference/duplicate_count.md)
  and `duplicate_detect()` was removed.

- The `na.rm` argument in
  [`duplicate_count_colpair()`](https://lhdjung.github.io/scrutiny/reference/duplicate_count_colpair.md)
  was removed.

## scrutiny 0.4.0

CRAN release: 2024-02-23

This version brings major performance improvements. Furthermore:

### Bugfixes

- Fixed a bug in
  [`audit_seq()`](https://lhdjung.github.io/scrutiny/reference/audit_seq.md):
  If the `dispersion` argument in the preceding call to a function like
  [`grim_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grim_map_seq.md)
  was specified as something other than a linearly increasing sequence,
  the `"diff_*"` columns in the data frames returned by
  [`audit_seq()`](https://lhdjung.github.io/scrutiny/reference/audit_seq.md)
  may have contained incorrect values.
- Similarly,
  [`audit_seq()`](https://lhdjung.github.io/scrutiny/reference/audit_seq.md)
  and
  [`reverse_map_seq()`](https://lhdjung.github.io/scrutiny/reference/reverse_map_seq.md)
  used to reconstruct the reported values incorrectly if the
  `dispersion` default was overridden as described above. At least for
  now, the issue is handled by throwing an error if these functions
  operate on data frames that are the result of specifying `dispersion`
  as something other than a linearly increasing sequence.
- Fixed a bug that incorrectly threw an error in
  [`grim_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grim_map_seq.md),
  other functions made by
  [`function_map_seq()`](https://lhdjung.github.io/scrutiny/reference/function_map_seq.md),
  as well as
  [`seq_disperse()`](https://lhdjung.github.io/scrutiny/reference/seq_disperse.md)
  and
  [`seq_disperse_df()`](https://lhdjung.github.io/scrutiny/reference/seq_disperse.md)
  if an input value was so close to `out_min` or `out_max` that the
  output sequence would be shorter than implied by `dispersion` /
  `.dispersion` , and if `track_var_change` / `.track_var_change` (see
  below) was `TRUE`. Again, note that the bug only occurred if an error
  was thrown.

### New features

- A new vignette lists the options for specifying the `rounding`
  argument that many scrutiny functions have:
  `vignette("rounding-options")`.
- Another new vignette shows the minimal steps to implement a
  consistency test using scrutiny:
  `vignette("consistency-tests-simple")`.
- The output of
  [`grim_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grim_map_seq.md),
  [`grimmer_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map_seq.md),
  [`debit_map_seq()`](https://lhdjung.github.io/scrutiny/reference/debit_map_seq.md)
  and any other function made by
  [`function_map_seq()`](https://lhdjung.github.io/scrutiny/reference/function_map_seq.md)
  now has a `diff_var` column that tracks the difference between the
  dispersed variable (see the `var` column) and the reported value.
  Following the `diff_*` columns in the output of
  [`audit_seq()`](https://lhdjung.github.io/scrutiny/reference/audit_seq.md),
  this is the number of dispersion steps, not the actual numeric
  difference.
- The same `diff_*` columns are now integer, not double.
- [`function_map()`](https://lhdjung.github.io/scrutiny/reference/function_map.md),
  [`function_map_seq()`](https://lhdjung.github.io/scrutiny/reference/function_map_seq.md),
  and
  [`function_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/function_map_total_n.md)
  have a new `.name_key_result` argument that controls the name of the
  key result column in the output of the factory-made function. This is
  `"consistency"` by default, but other names will fit better for other
  kinds of tests. (The results of these tests must still be logical
  values.)

### Minor changes

- In
  [`duplicate_count()`](https://lhdjung.github.io/scrutiny/reference/duplicate_count.md),
  the `count` column in the output tibble was renamed to `frequency`.
  This makes for a more streamlined frequency table and removes an
  ambiguity with
  [`duplicate_count_colpair()`](https://lhdjung.github.io/scrutiny/reference/duplicate_count_colpair.md),
  where the `count` output column means something different.
- In
  [`seq_disperse()`](https://lhdjung.github.io/scrutiny/reference/seq_disperse.md)
  and
  [`seq_disperse_df()`](https://lhdjung.github.io/scrutiny/reference/seq_disperse.md),
  the `track_var_change` / `.track_var_change` argument was renamed to
  `track_diff_var` / `.track_diff_var`. The arguments with the old names
  are still present for now but will be removed in a future version.
  Also, the unit of these values is now dispersion steps, for
  consistency with
  [`grim_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grim_map_seq.md)
  etc. as well as
  [`audit_seq()`](https://lhdjung.github.io/scrutiny/reference/audit_seq.md).
- [`grim_total()`](https://lhdjung.github.io/scrutiny/reference/grim-stats.md),
  `grim_ratio()`, and `grim_ratio_upper()` now require `x` to have
  length 1.
- The docs now link to functions when opened in RStudio, not just on the
  website.
- Accordingly, the output of `write_doc_factory_map_conventions()` now
  renders links. The function also has a new `scrutiny_prefix` argument
  for use in another package.
- The “Infrastructure” article was renamed to “Developer tools”;
  `vignette("devtools")`.
- Some dependencies that used to be suggested are now imported.

## scrutiny 0.3.0

CRAN release: 2023-08-08

### Duplicate analysis overhaul

The `duplicate_*()` functions now present their output better and have
overall been streamlined. Read more at
[`vignette("duplicates")`](https://lhdjung.github.io/scrutiny/articles/duplicates.md).

- A new function,
  [`duplicate_tally()`](https://lhdjung.github.io/scrutiny/reference/duplicate_tally.md),
  marks each observation with its overall frequency. It is similar to
  `duplicate_detect()` but more informative.

- In
  [`duplicate_count()`](https://lhdjung.github.io/scrutiny/reference/duplicate_count.md):

  - All values are now treated like character strings, so all can be
    checked. The `numeric_only` argument is deprecated and should no
    longer be used.

  - The output tibble has two new columns, `locations` and
    `locations_n`. These hold the names of all input columns in which a
    value appears and the number of these columns. Details are
    controlled by the new `locations_type` argument.

  - New `ignore` argument for specifying one or more values that will
    not be checked for duplicates.

- In
  [`duplicate_count_colpair()`](https://lhdjung.github.io/scrutiny/reference/duplicate_count_colpair.md):

  - New `total_x` and `total_y` columns in the output show how many
    non-missing values were checked for duplicates.

  - New `ignore` argument as in
    [`duplicate_count()`](https://lhdjung.github.io/scrutiny/reference/duplicate_count.md).

  - The `na.rm` argument is deprecated. It wasn’t very useful because
    missing values are never checked for duplicates.

- `duplicate_detect()` is superseded. It is less informative than
  [`duplicate_count()`](https://lhdjung.github.io/scrutiny/reference/duplicate_count.md)
  and, in particular,
  [`duplicate_tally()`](https://lhdjung.github.io/scrutiny/reference/duplicate_tally.md).
  Still, it shares in the overhaul:

  - As in
    [`duplicate_count()`](https://lhdjung.github.io/scrutiny/reference/duplicate_count.md),
    all values are now treated like character strings, so all can be
    checked. The `numeric_only` argument is deprecated and should no
    longer be used.
  - The duplicate status of missing values is now shown as `NA`.
  - New `ignore` argument as in
    [`duplicate_count()`](https://lhdjung.github.io/scrutiny/reference/duplicate_count.md).

### Bugfixes

- Fixed a numeric precision bug in `round_up_from()` and
  `round_down_from()` that occurred when rounding numbers greater than
  circa 2100 with a part to be truncated that was equal to 5 on that
  decimal level (thanks to [@kaz462](https://github.com/kaz462),
  [\#43](https://github.com/lhdjung/scrutiny/issues/43)). These
  functions are called within `round_up()` and `round_down()`, and
  indirectly by all consistency-testing functions.

- Fixed a bug in
  [`audit_seq()`](https://lhdjung.github.io/scrutiny/reference/audit_seq.md)
  that displayed one “hit” found by varying a given reported value if
  there were no such hits. The other columns were not affected.

- Fixed a bug in
  [`function_map()`](https://lhdjung.github.io/scrutiny/reference/function_map.md)
  that displayed the wrong calling function’s name in case of an error.

### Minor improvements

- Documentation for
  [`grim_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grim_map_seq.md)
  and all other functions made by
  [`function_map()`](https://lhdjung.github.io/scrutiny/reference/function_map.md),
  [`function_map_seq()`](https://lhdjung.github.io/scrutiny/reference/function_map_seq.md),
  or
  [`function_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/function_map_total_n.md)
  now displays meaningful defaults. Printing the factory-made functions
  is more meaningful, as well. Internally, they now work with
  [`rlang::new_function()`](https://rlang.r-lib.org/reference/new_function.html),
  which allows for flexible metaprogramming.

- The experimental function `audit_list()` is deprecated. Just use
  [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
  instead.
  [`audit_seq()`](https://lhdjung.github.io/scrutiny/reference/audit_seq.md)
  and
  [`audit_total_n()`](https://lhdjung.github.io/scrutiny/reference/audit_seq.md)
  are now documented separately from
  [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md) and
  `audit_list()`.

- The lifecycle package is now imported and used in formal deprecations
  such as that of `sep` in the `restore_*()` functions. The janitor
  package is no longer suggested.

- Adjusted to new CRAN requirements for
  [`packageVersion()`](https://rdrr.io/r/utils/packageDescription.html)
  usage.

- Some performance improvements.

## scrutiny 0.2.4

CRAN release: 2023-01-20

- New
  [`decimal_places_df()`](https://lhdjung.github.io/scrutiny/reference/decimal_places_df.md)
  function that takes a data frame and counts the decimal places in all
  numeric-like columns.
- Four new predicate functions centered around
  [`is_map_df()`](https://lhdjung.github.io/scrutiny/reference/data-frame-predicates.md)
  test whether an object is the output of a scrutiny-style mapper
  function for consistency tests.
- Newly exported
  [`is_numeric_like()`](https://lhdjung.github.io/scrutiny/reference/is_numeric_like.md)
  function to test whether an object (e.g., a string vector) can be
  coerced to numeric.
- New `grim_ratio_upper()` function gives an upper bound for
  `grim_ratio()`.
- Changes in
  [`split_by_parens()`](https://lhdjung.github.io/scrutiny/reference/split_by_parens.md):
  - The function now uses a `cols` argument instead of the dots (`...`).
    This follows [tidyselect development
    guidelines](https://tidyselect.r-lib.org/articles/tidyselect.html#selections-as-dots-or-as-named-arguments).
    The default, `cols = everything()`, is to select all columns that
    contain the `sep` elements (by default, parentheses). Set the new
    `check_sep` argument to `FALSE` to select all columns regardless.

  - All other arguments were renamed: they no longer start on a dot.
    Furthermore, `.col1` and `.col2` have been renamed to `end1` and
    `end2`.

  - A warning is now issued if one or more columns can’t be split (or is
    de-selected from splitting). This occurs if a column doesn’t contain
    the `sep` elements.

  - Internal changes for compatibility with dplyr 1.1.0.
- In
  [`restore_zeros_df()`](https://lhdjung.github.io/scrutiny/reference/restore_zeros.md)
  as well, the dots (`...`) were replaced by a `cols` argument, and each
  other argument no longer has a prefix dot. This follows the changes in
  [`split_by_parens()`](https://lhdjung.github.io/scrutiny/reference/split_by_parens.md),
  but note the default selection restrictions by the new
  `check_numeric_like` argument. The optional `check_decimals` argument
  goes even further.
- Prevent false-positive warnings when printing ggplot objects (they had
  occurred since ggplot2 3.4.0).

## scrutiny 0.2.3

CRAN release: 2022-12-11

Some new features and bugfixes:

- New [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
  methods for the output of
  [`audit_seq()`](https://lhdjung.github.io/scrutiny/reference/audit_seq.md)
  and
  [`audit_total_n()`](https://lhdjung.github.io/scrutiny/reference/audit_seq.md).

- New
  [`duplicate_count_colpair()`](https://lhdjung.github.io/scrutiny/reference/duplicate_count_colpair.md)
  function that checks each combination of columns in a data frame for
  duplicates.

- New
  [`restore_zeros_df()`](https://lhdjung.github.io/scrutiny/reference/restore_zeros.md)
  function to easily restore trailing zeros in all numeric-like columns
  in a data frame.

- New `seq_length()` function to extend or shorten linear sequences.

- Bugfixes in the `is_seq_*()` functions.

- Argument evaluation is now forced in the function factories:
  [`function_map()`](https://lhdjung.github.io/scrutiny/reference/function_map.md),
  [`function_map_seq()`](https://lhdjung.github.io/scrutiny/reference/function_map_seq.md),
  and
  [`function_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/function_map_total_n.md).

- Some possible corner case issues in
  [`split_by_parens()`](https://lhdjung.github.io/scrutiny/reference/split_by_parens.md)
  are now prevented.

- Internal changes for compatibility with purrr 1.0.0 and tidyselect
  1.2.0.

## scrutiny 0.2.2

CRAN release: 2022-08-22

This is a patch for CRAN compliance.

- The package now requires R version \>= 3.4.0 and rlang version \>=
  1.0.2.

- Subtle changes to
  [`split_by_parens()`](https://lhdjung.github.io/scrutiny/reference/split_by_parens.md)
  that users generally won’t notice.

- Minor shifts in the documentation (e.g.,
  `vignette("consistency-tests")` now has instructions on exporting
  factory-made functions.).

## scrutiny 0.2.1

This is a patch.

- It reduces the scope of some examples for CRAN compliance.

- Minor vignette changes.

## scrutiny 0.2.0

This is a massive release, with many new features and improvements all
over scrutiny. Most notably, the package now includes an entirely new
system for implementing consistency tests.

- A new vignette lays out how to implement consistency tests using
  scrutiny’s infrastructure. It describes many of the features mentioned
  below.
- GRIMMER support was added, as explained in another new vignette. All
  GRIM and DEBIT functions mentioned below have GRIMMER analogues. For
  example,
  [`grimmer_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map_seq.md)
  is analogous to
  [`grim_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grim_map_seq.md).
- Because of the new, stricter rules for consistency tests, the output
  of
  [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md)
  no longer includes an `items` column by default. Instead, the numbers
  of items (1 by default) are factored into the output’s `n` column.
  This focuses the presentation on the essence of GRIM.
- GRIM and DEBIT functions are now somewhat less likely to flag value
  sets as inconsistent. That is because measures were taken to reduce
  spurious, computer-induced differences when comparing floating-point
  numbers. The same applies to the new GRIMMER functions.
- [`function_map()`](https://lhdjung.github.io/scrutiny/reference/function_map.md)
  enables users to quickly create consistency test functions for data
  frames much like
  [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md)
  or
  [`debit_map()`](https://lhdjung.github.io/scrutiny/reference/debit_map.md).
- [`grim_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grim_map_seq.md)
  checks if GRIM inconsistencies might be due to small errors, and the
  true values might be close to the reported ones. It varies the inputs
  up and down in a specified range, holding the respective other ones
  constant, and tests all those combinations. For summaries, call
  [`audit_seq()`](https://lhdjung.github.io/scrutiny/reference/audit_seq.md)
  on the results.
- [`debit_map_seq()`](https://lhdjung.github.io/scrutiny/reference/debit_map_seq.md)
  does the same for DEBIT.
- The above two are powered by
  [`function_map_seq()`](https://lhdjung.github.io/scrutiny/reference/function_map_seq.md),
  which allows users to easily create functions just like these for any
  consistency test. All that’s needed is a data-frame-level consistency
  testing function like
  [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md)
  or
  [`debit_map()`](https://lhdjung.github.io/scrutiny/reference/debit_map.md).
- [`grim_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/grim_map_total_n.md)
  applies GRIM in cases where no group sizes are reported, only total
  sample sizes. It systematically matches possible group sizes (around
  half the total) with reported mean or proportion values, GRIM-tests
  them, and counts the scenarios in which both matches are consistent.
  For summaries, call
  [`audit_total_n()`](https://lhdjung.github.io/scrutiny/reference/audit_seq.md)
  on the results.
- [`debit_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/debit_map_total_n.md)
  does the same for DEBIT.
- The above two are powered by
  [`function_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/function_map_total_n.md),
  which allows users to easily create new functions like
  [`grim_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/grim_map_total_n.md)
  or
  [`debit_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/debit_map_total_n.md),
  provided a data-frame-level consistency testing function like
  [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md)
  or
  [`debit_map()`](https://lhdjung.github.io/scrutiny/reference/debit_map.md).
- On a lower level still,
  [`disperse_total()`](https://lhdjung.github.io/scrutiny/reference/disperse.md)
  takes a total sample size (comprised of the two unknown group sizes on
  interest) and calls the appropriate group-level function:
  [`disperse()`](https://lhdjung.github.io/scrutiny/reference/disperse.md)
  for even totals,
  [`disperse2()`](https://lhdjung.github.io/scrutiny/reference/disperse.md)
  for odd ones.
- [`seq_disperse()`](https://lhdjung.github.io/scrutiny/reference/seq_disperse.md)
  and
  [`seq_disperse_df()`](https://lhdjung.github.io/scrutiny/reference/seq_disperse.md)
  extend scrutiny’s support for string decimal sequences with trailing
  zeros. They construct sequences centered around the input; a use case
  not directly covered by
  [`base::seq()`](https://rdrr.io/r/base/seq.html).
- Predicate functions around
  [`is_seq_linear()`](https://lhdjung.github.io/scrutiny/reference/seq-predicates.md)
  test whether a vector represents a certain kind of numeric sequence.
- In
  [`debit_map()`](https://lhdjung.github.io/scrutiny/reference/debit_map.md),
  the `x` column is now to the left of the `sd` column if `show_rec` is
  `FALSE`, in accordance with the `show_rec = TRUE` default.
- [`debit()`](https://lhdjung.github.io/scrutiny/reference/debit.md) is
  now vectorized.
- The functions around `is_subset_of() and is_superset_of()` functions
  now have stricter variants grouped around `is_proper_subset_of()` and
  `is_proper_superset_of()`.
- [`split_by_parens()`](https://lhdjung.github.io/scrutiny/reference/split_by_parens.md)
  now accepts any pair of separators passed to `.sep` as a length-2
  vector.

## scrutiny 0.1.1

This is a patch, mainly fixing a bug that used to affect the
presentation of input data in
[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md)’s
results. It needs to be emphasized that this bug only affected a
convenience feature, namely the presentation of certain input data in
the output, not the GRIM test itself.

- Previously, if `percent` was set to `TRUE`, the `x` values were
  converted to percentages. Because they need to be presented as
  strings, percentage conversion involves restoring the correct number
  of trailing zeros. The bug, then, was that all the `x` values
  appearing in the output (not in the internal computations!) were
  restored to the same “length” as the single longest one. This was now
  remedied, and `x` values are restored to their individually
  appropriate number of trailing zeros.

- Another bugfix concerns versioning. Previously, the package had an
  incorrect version number. It was now corrected.

- The last change was to remove an outdated and potentially misleading
  paragraph in the documentation of `reround_to_fraction()`.

## scrutiny 0.1.0

- This version includes an overhaul of
  [`grim_plot()`](https://lhdjung.github.io/scrutiny/reference/grim_plot.md):

  - It extends the function to cover cases of `decimals` values greater
    than 2, using a gradient instead of a raster.

  - It enables data-free calls to
    [`grim_plot()`](https://lhdjung.github.io/scrutiny/reference/grim_plot.md)
    with the new `show_data` argument. Resulting plots only display the
    background raster. This mirrors Figure 1 in Brown and Heathers’ GRIM
    paper. (Although
    [`grim_plot()`](https://lhdjung.github.io/scrutiny/reference/grim_plot.md)
    as a whole is modeled after this figure, the default addition of
    empirical summary data is specific to scrutiny.) Like Brown and
    Heathers, users may wish to create such raster-only plots in order
    to demonstrate some principled points. The key parameters `decimals`
    and `rounding` can be controlled directly to make up for the lack of
    information from `data`.

  - The function now checks if all input means or proportions (`x`) have
    the same number of decimal places. If they don’t, it throws an
    error. This strict criterion can be circumvented by specifying the
    `decimals` argument. However, since each raster is specific to one
    number of decimal places (and hence cannot be interpreted regarding
    `x` values with a different number), the recommended solution is to
    plot `x` values separately — once for each number of decimal places.

  - The `show_full_range` argument was removed because I now think it is
    superfluous.

  - Previously, there was some space between the raster and the y-axis.
    It has now been removed.

  - Test result data points, shown in blue and/or red by default, are
    now built on top of the raster, which makes for a more distinct
    appearance.

- Two new functions, `reround_to_fraction()`, and
  `reround_to_fraction_level()`, enable fractional rounding, inspired by
  `janitor::round_to_fraction()`. For example, they might round `0.4` to
  `0.5` for fractions of `2`. What tells the new functions apart is that
  they come with all the flexibility of `reround()`. Furthermore,
  `reround_to_fraction_level()` is closer to a conventional rounding
  function than the other two.

- The new version also fixes a bug in
  [`row_to_colnames()`](https://lhdjung.github.io/scrutiny/reference/row_to_colnames.md),
  rewriting the function’s core.

- Another bug was fixed in
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md) and
  [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md),
  concerning `show_rec`: For `rounding` strings that lead to four
  reconstructed numbers per `x` value rather than just two, it used to
  be the case that only the two values corresponding to the first of the
  two rounding procedures were displayed in the output tibble. Now, all
  four are displayed, bearing appropriate names.

- Another bugfix is for the `threshold` argument in `reround()`, which
  didn’t work properly before. This used to affect higher-level
  functions such as
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md),
  [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md),
  [`debit()`](https://lhdjung.github.io/scrutiny/reference/debit.md),
  and
  [`debit_map()`](https://lhdjung.github.io/scrutiny/reference/debit_map.md),
  as well. The default for `threshold` is now `5` in all such functions.
  Note that rounding up and down from 5 has been fully functional
  independently of it.

- Also in `reround()`, the `rec` argument has been renamed to `x` in
  accordance with general naming conventions. The `decimals` argument
  has been renamed to `digits` in accordance with naming conventions
  among rounding functions.

- In
  [`split_by_parens()`](https://lhdjung.github.io/scrutiny/reference/split_by_parens.md),
  ellipsis support was added to protect the user from silent, unexpected
  results following named arguments in tidy evaluation. The ellipsis
  package has been added to the Suggests field in `DESCRIPTION`.

- In some high-level functions, internal checks now determine if the
  lengths of multiple arguments that are factored into the same internal
  function call are mutually congruent. That is, if two such arguments
  are length \> 1, they need to have the same length (which will throw a
  warning). Otherwise, there will be an explicit and very specific error
  message.

- Finally, some minor refactoring and other small changes that users
  generally won’t notice.

## scrutiny 0.0.1

- Added vignette about other packages for error detection, called
  `Related software`.
- Exported
  [`grim_plot()`](https://lhdjung.github.io/scrutiny/reference/grim_plot.md).
- Minor refactoring.

## scrutiny 0.0.0.9000

- Added a `NEWS.md` file to track changes to the package.
