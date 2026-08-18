# scrutiny 1.0.0

## Breaking changes

- scrutiny now uses the base pipe `|>` instead of the magrittr pipe `%>%`, so `%>%` is no longer exported.
- `grim_map()`, `grimmer_map()`, and `debit_map()` are now created by `function_map()` instead of being written by hand. Their output changed as noted below, and they gained the remaining arguments of `grim()`, `grimmer()`, and `debit()` (e.g., `debit_map()` now has a `formula` argument) and lost these:
  - `grim_map()` and `grimmer_map()` no longer have `merge_items`. Setting it to `FALSE` packed `n` and `items` into a single data-frame column, which every function downstream of the mapper takes to be a numeric vector. The default behavior, multiplying `items` into `n` for the output, is now the only one.
  - `grim_map()` and `debit_map()` no longer have `extra`. It selected which of the other columns of `data` come along, and all three mappers now return all of them. `dplyr::select()` does the same job on the output, with tidyselect, and `grimmer_map()` never had the argument in the first place.
  - `grim_map()` no longer has `testables_only`. Use `dplyr::filter(probability > 0)` on the output instead.
  - `grim_map()` no longer reports "`x` converted from percentage" when `percent` is `TRUE`. The conversion is what the user asked for by setting the argument, and the message fired once per dispersed value inside `grim_map_seq()`.
  - `debit_map()` no longer accepts strings for `x` and `sd`, as `grim_map()` and `grimmer_map()` already didn't. Use numbers and state the decimal places with `digits_x` and `digits_sd`. DEBIT used to count them itself.

- `grim()`, `grimmer()`, and `debit()` no longer have `show_rec` / `show_reason`. These arguments make the underlying single-case functions return a list with the reconstructed values, or with the reason for an inconsistency, so that `grim_map()`, `grimmer_map()`, and `debit_map()` can unpack them into columns. They were documented as being for internal use only, but they were formals of the exported functions all the same: the three used to be made with `Vectorize()`, which copies every formal of the function it wraps. Setting one did not produce a usable object, either --- `grim(x = c(5.19, 5.18), n = c(28, 32), digits_x = 2, show_rec = TRUE)` returned an unnamed 6 × 2 matrix of lists rather than the logical vector `grim()` promises. Use `show_rec` and `show_reason` in the mappers, where they return named columns.

- Following from the same change, `grim()`, `grimmer()`, and `debit()` are now vectorized only over the values they test --- `x`, `sd`, `n`, the `digits_*` arguments, and `items` --- which is the same split that the mappers make between what they take from `data` and what they apply to the whole call. `rounding`, `threshold`, `symmetric`, `percent`, `formula`, `min_val`, `max_val`, and `tolerance` must now have length 1. They describe how to test rather than what to test, and `reround()` has always treated them as scalar; `Vectorize()` looped over them along with everything else, so `grim(x = c(5.19, 5.19), n = 28, digits_x = 2, rounding = c("up", "down"))` returned one verdict per rounding method. To test value sets that differ in one of these, call the mapper once for each of its values.

- The values that `grim()`, `grimmer()`, and `debit()` are vectorized over are now recycled by the tidyverse rules: a length-1 argument is recycled to the length of the others, and anything else must already have that length. `mapply()` recycled anything into anything and only warned when the longer length was not a multiple of the shorter, so `grim(x = c(5.19, 5.18, 5.17), n = c(28, 32), digits_x = 2)` returned three verdicts, the third of which paired the third `x` with the first `n` --- a value set the caller never wrote down. That is now an error. Relatedly, the three functions now return `logical(0)` for zero-length input, where `Vectorize()` returned an empty list, and they no longer carry over names from `x`.

- `grim_map(show_rec = TRUE)` now returns the same five columns for every rounding method: `rec_sum`, `sum_lower`, `sum_upper`, `rec_x_upper`, and `rec_x_lower`. The `rec_x_*_rounded*` columns are gone. (There were four of them for `rounding = "up_or_down"`, `"up_from_or_down_from"`, and `"ceiling_or_floor"`, and two for the other methods.) They displayed the two reconstructed means re-rounded, which was how GRIM decided consistency before the test moved to exact integer arithmetic in this release. Since the verdict no longer comes from those numbers, they had become a second, parallel reconstruction that could contradict the `consistency` column they were meant to explain. `grim_map(tibble::tibble(x = 2, n = 127), digits_x = 2, rounding = "anti_trunc", show_rec = TRUE)` reported `TRUE` while displaying granules that round to 2.01, and the sum that actually decided the verdict appeared nowhere in the output. The new `sum_lower` and `sum_upper` columns are that deciding range: the least and the greatest whole-number sum total that would have been reported as `x`. A value set is consistent exactly if `sum_lower` is not greater than `sum_upper`.

- With `percent = TRUE`, the `rec_x_upper` and `rec_x_lower` columns of `grim_map(show_rec = TRUE)` are now percentages, like `x` itself. They used to be the decimal numbers that GRIM converts `x` to internally, so a reported 71 was displayed alongside granules of 0.72 and 0.68. The documentation calls those the reconstructed `x` values closest to `x`, but you could not compare them to `x` at a glance. `grim_values()` and `grim_closest()` already returned their values on the scale of `x`. The `rec_sum`, `sum_lower`, and `sum_upper` columns are unchanged: they are sums of the underlying data, which the scale of the reported mean does not change.

- The `n` column returned by `grim_map()`, `grimmer_map()`, and `debit_map()` is now an integer column if all of its values are integers, which sample sizes are. `function_map_seq()` output already followed this convention.

- `debit_plot()` no longer has a `line_size` argument. The function now always draws the DEBIT line via `linewidth`.

- `grim_map()`, `grimmer_map()`, and `debit_map()` now carry the `digits_x` (and, for GRIMMER and DEBIT, `digits_sd`) they were given forward into a `digits_x` / `digits_sd` output column. The same is true of `*_map_total_n()`.

- `grim_plot()` now reads this column instead of guessing the decimal count from the numeric `x` column via `decimal_places()`, which was unreliable because a numeric value cannot carry trailing zeros: `5.00` reads back as 0 decimal places, not 2.

- `grim_plot()` now has a `split_by_digits` argument (default is `FALSE`) to optionally return a list of plots instead; one plot per distinct number of decimal places.

- `grim_plot()` returns its plot instead of printing it and returning invisibly. At the console, auto-printing draws it either way, but `p <- grim_plot(g)` used to draw a plot the caller had not asked for, `grim_plot(g) + ggplot2::labs(...)` drew two, and composing with patchwork or cowplot always left a stray canvas. `debit_plot()` has always returned its object normally. The one exception is `split_by_digits = TRUE`, which returns a list -- something auto-printing cannot draw -- so that branch still prints each plot itself.

## Bugfixes

- `debit()` and `debit_map()` no longer flag consistent binary data with a mean reported as `0.50`. DEBIT reconstructs the SD at the bounds of the mean's rounding interval and concludes from those two values that every SD in between is reachable, which needs the reconstruction to be monotonic in the mean. It is not: `sd_binary_mean_n()` is a downward parabola peaking at a mean of 0.5, so an interval containing 0.5 reaches SDs *above* both of its endpoints -- and for a mean of exactly `0.50` the interval is symmetric around the peak, both endpoints give the same SD, and the whole attainable band collapsed to a single point. `debit(x = 0.50, sd = 0.503, n = 100, digits_x = 2, digits_sd = 3)` was `FALSE` for 50 ones and 50 zeros. The peak is now evaluated as well wherever it falls inside the interval. The error only ever turned `TRUE` into `FALSE`, so no value set that used to pass now fails.

- `*_map_seq()` functions no longer cut the dispersion of a value variable short at one decimal unit above zero. `out_min = "auto"` was written for `n`, which cannot go below 1, but it applied to every dispersed variable alike. For a negative reported mean it removed the entire lower half of the sequence, and `0` -- a legal mean, SD, and proportion -- was out of reach everywhere. `"auto"` is now resolved per variable through the new `.var_bounds` argument of `function_map_seq()`: `n` keeps a minimum of 1 whether it is declared or not, a GRIMMER `sd` gets one of 0, DEBIT's `x` and `sd` are confined to `[0, 1]`, and a mean is unbounded. `.out_max` now defaults to `"auto"` as well, for the same reason.

  This also fixes `debit_map_seq()` failing outright -- with DEBIT's "values must range from 0 to 1" error -- whenever a reported `x` or `sd` sat close enough to `0` or `1` for the dispersion to cross it.

- `reverse_map_seq()`, and hence `audit_seq()`, now recover the reported values exactly. They used to infer them from the *shape* of the dispersed sequence: the midpoint of its gap, or its median if it had none. That is only valid for a complete sequence, and once `out_min` or `out_max` had truncated one side of it, the gap disappeared along with the values below it and the median branch returned a plausible but wrong number. `reverse_map_seq()` on `grim_map_seq()` output for a reported mean of `-2.51` gave `-2.48`, and `audit_seq()` re-tested that and reported the opposite consistency verdict. The reported value is now computed from the `diff_var` column, which records how many steps each row sits from it.

- `audit()` on `grim_map()` output no longer returns `NA` for `mean_grim_prob` and `incons_to_prob` when any case is undecidable. A missing `n` has no GRIM probability, and one such row used to erase those two summaries for the whole table, although `audit_cols_minimal()` had always excluded undecidable cases from its own counts. The same applies to `mean_x` and `mean_sd` in `audit()` on `debit_map()` output.

- `grim_probability()` no longer returns a "probability" greater than 1. A non-positive `n` leaves nothing to test, which `grim()` reports as `NA`; the formula returned `1.03` for `n = -3`, so `grim_map()` displayed that next to a verdict of `NA`. It now returns `NA` there. `grim_ratio()` is unchanged -- it is documented as the unclamped one.

- `grim_plot()` no longer drops value sets whose mean has a fractional portion of exactly 0, such as `5.00`. The y-axis bounds were scale limits, which discard a tile as soon as one of its edges falls outside, and a tile centered on 0 reaches half its height below the axis. They are now coordinate limits, which clip instead of discarding. `pigs1` contains such a value, so the plot in the package's own examples had been showing eleven of its twelve value sets.

- `grim_plot()` no longer suppresses every warning raised while drawing. That had hidden the dropped rows above, along with a live deprecation warning for ggplot2's `size` aesthetic.

- `grim_plot()` draws its data with a single `geom_tile()` call, using `linewidth`. It used to pick between two calls at run time on `utils::packageVersion("ggplot2") >= "3.4"`, the release that renamed `size` to `linewidth` for lines. `DESCRIPTION` has required `ggplot2 (>= 3.4.0)` ever since, so the test could only come out `TRUE` and the `size` branch was unreachable.

- `grim_plot(show_raster = FALSE)` works again. The plot object was only created inside the branch that draws the raster, so the documented argument failed with "object 'p' not found" as soon as the data layer was added.

- `grim_plot()` now plots negative means, at the fractional portion of their absolute value: a mean of `-2.51` is drawn at `0.51`, in the same place as `2.51`. GRIM's granularity is symmetric around zero, since the achievable means of `n` integers are `k / n` for every whole number `k` whatever its sign. Such a value used to have a negative fractional portion, fall outside the y-axis, and be dropped without a word. See the new `Negative means` section of `?grim_plot`, which also explains the one thing the sign still affects: for a one-directional `rounding` method, the background raster behind a negative value is the one for the mirrored method. The tile's color is unaffected -- it comes from the `consistency` column, which was computed for the value as reported.

- `grim_plot()` now explains itself instead of failing from the middle. A zero-row `data` -- an ordinary result of `dplyr::filter()` -- used to raise R's own "missing value where TRUE/FALSE needed".

- `round_up_from()` and `round_down_from()` now validate `threshold`, as `reround()`, `unround()`, and the bounds machinery already did. `round_up_from(4.28, 1, threshold = 0)` used to return `4.3`, silently rounding like `round_ceiling()`, and a negative threshold was accepted too.

- `unround()` returns a zero-row tibble for a zero-length `x`, rather than one row of missing values. Recycling now stops at zero instead of letting the length-1 defaults set the row count.

- `disperse()` rejects a fractional `dispersion` instead of mangling its output. The `n_change` column was truncated toward zero, so `disperse(n = 10, dispersion = c(0.5, 1.5))` reported changes of `0` and `1` for group sizes of `9.5` and `8.5`.

- `seq_disperse()` no longer repeats the value it disperses from when `dispersion` contains a `0`. A step of zero is that value itself, once in each direction, on top of whatever `include_reported` adds. `grim_map_seq(dispersion = c(0, 1, 2))` returned the reported case twice, and `audit_seq()` counted it twice.

- `debit()` now rejects a `formula` other than `"mean_n"` with an explanation. The other formulas need a group size that DEBIT is not given, so they used to reach `reconstruct_sd()` and fail there with R's own "argument "group_0" is missing", about an argument the user never saw. The documentation already said that only `"mean_n"` is supported.

- `*_map_seq()` functions now name the R argument, `include_consistent`, when there are no inconsistent cases to disperse from. The message named it only in interactive sessions and otherwise told the user to untick a checkbox in the scrutiny Shiny app -- so scripts, R Markdown documents, and test runs, which are exactly the contexts with no checkbox on screen, got the checkbox message.

- `*_map_total_n()` functions now swap the two groups correctly whatever the column order of `data`. The swap that produces the `"back"` direction used to be silently skipped unless the key columns appeared in the exact `x1, x2, sd1, sd2, ...` order, in which case the `"back"` half of the output was a duplicate of the `"forth"` half labeled `"back"` -- and `audit_total_n()`'s `hits_back` counts were wrong accordingly.

- `*_map_seq()` functions now apply `items` only once. The initial test multiplies `items` into the `n` column, so the internal re-tests of dispersed values receive data whose `n` is already merged; forwarding `items` to them as well used to multiply it in a second time, so dispersed values were tested against `n * items^2`.

- `audit_seq()` now re-tests the reconstructed data with the same arguments as the original `*_map_seq()` call. Arguments that change verdicts but leave no trace in the output columns -- `percent`, `threshold`, `symmetric`, GRIMMER's `min_val` and `max_val` -- used to be silently dropped, which could flip the `consistency` column of the summary. The mapper output now records the replayable arguments in an attribute; for older output without it, `audit_seq()` falls back to the `digits_*` columns and the rounding class, as before.

- `rounding_bias()` again throws an error for the compound rounding methods `"up_or_down"`, `"up_from_or_down_from"`, and `"ceiling_or_floor"`, as documented. The check had been lost, and since these methods return two rounded values per input value, the function silently returned twice as many "biases" as inputs (with `mean = FALSE`) or their meaningless average (by default).

- `function_map_seq()` now enforces its `.args_disabled` argument. It was documented to make the factory-made function throw an error when a disabled argument is specified, but no check ever ran, so such arguments were silently passed on.

- `round_anti_trunc()` and `anti_trunc()` no longer move a value that already sits on the rounding grid one step further away from zero. `round_anti_trunc(8.42, digits = 2)` is now `8.42` rather than `8.43`, and `anti_trunc(0)` is `0` rather than `1`. The old behavior had no software behind it: Excel's and Google Sheets' `ROUNDUP()`, Java's `RoundingMode.UP`, and Python's `decimal.ROUND_UP` all round away from zero in the sense implemented now, where a value on the grid stays put.

  As a consequence, `rounding = "anti_trunc"` now agrees with `ROUNDUP()` for every value rather than for every non-grid value, so consistency verdicts under it can change. Also, `unround()` at zero returns a range instead of `NA`: since every non-zero value is taken away from zero, the only value that would be reported as zero is zero itself, so the range is the single point `0 <= x <= 0`. A mean reported as `0.00` therefore pins the sum to exactly `0`, and GRIM, GRIMMER, and DEBIT are decidable there rather than `NA`. Finally, no rounding method has undefined bounds any more; the only undecidable cases left are missing values and an `n` that leaves nothing to test.

- Fixed an error in the GRIM rounding-boundary logic for `rounding = "up"` and `rounding = "down"`. For `"up"` rounding, the upper boundary is exclusive (a value exactly at the midpoint rounds *away* from the reported mean, not toward it), but the old implementation treated it as inclusive. This could cause `grim()` and `grim_map()` to return `TRUE` for value sets that are actually inconsistent.

- Fixed a bug in GRIMMER that could occur when using the non-default `rounding = "up"` or `"down"` options in cases with three or more candidate sums of squares. In these situations, the checks for the reconstructed SD matching the reported SD and for the sum of squares having the same parity as the sum could be satisfied by different candidate sums instead of the same one, producing false-negative consistency results (i.e., wrongly letting inconsistent value sets pass). Thanks to Ian Hussey (#85).

- GRIM and GRIMMER now derive the set of candidate sums in exact integer arithmetic. Both used to compare quantities against the rounding bounds in floating point, which failed whenever a candidate sat exactly on a bound. In such cases, the two sides of the comparison were different floating-point representations of the same real number. This corrupted verdicts in both directions. `grim()` could therefore pass unattainable means, and `grimmer()` could both pass values with no valid sum of squares and flag mean-SD combinations that could be produced by real data sets. For instance, `grimmer(x = 0.03, sd = 0.18, n = 200)` was reported as inconsistent although the dataset with seven 1s and 193 0s has exactly these values. Thanks to Ian Hussey (#86).

- GRIMMER now derives the bounds of the sum of squares in exact integer arithmetic as well, not just the candidate sums. The previous `round(sum_squares_lower, 12)` correction could not repair floating-point error once the sum of squares exceeded about 1000, because two neighboring doubles are more than 1e-12 apart from there on. A bound that was mathematically an exact integer could then be ceilinged to the next one up, dropping the only viable sum of squares and flagging real data. For instance, `grimmer(x = 7.67, sd = 0, n = 2, items = 3, digits_x = 2, digits_sd = 2)` was reported as inconsistent although two subjects with three items each, all summing to 23, have exactly these values.

- GRIM and GRIMMER now treat a rounding bound as exclusive whenever the rounding method in question would carry a value sitting on it away from the reported number. This was already the case for `rounding = "up"` and `"down"`; it now also holds for `"ceiling"`, `"floor"`, `"ceiling_or_floor"`, `"trunc"`, `"anti_trunc"`, `"up_from"`, `"down_from"`, and `"up_from_or_down_from"`, which used to treat both of their bounds as inclusive and so let some inconsistent value sets pass. `rounding = "even"` keeps both bounds inclusive, since `base::round()` breaks midpoint ties by the parity of the preceding digit, and whether a tie occurs at all depends on the binary representation of the value; being too permissive is the safe direction here.

- `grim()` and `grimmer()` now take `symmetric` into account more comprehensively when deciding consistency. It used to affect only the reconstructed values shown by `grim_map(show_rec = TRUE)`, so that, e.g., `grim(-0.07, n = 40, digits_x = 2, rounding = "up", symmetric = TRUE)` returned `TRUE` even though the only candidate mean, -0.075, rounds to -0.08 under symmetric rounding.

- `threshold` no longer affects `grim()` and `grimmer()` for `rounding = "up_or_down"`, `"up"`, and `"down"`. Those methods round from a fixed 5, as `round_up()` and `round_down()` do, and the documentation already described `threshold` as applying to the `"*_from"` methods only. Passing it alongside `"up"` used to silently move the bounds, producing verdicts that no rounding function in the package would agree with.

- `grimmer()` now accepts `rounding = "ceiling_or_floor"`, `"up_from"`, `"down_from"`, and `"up_from_or_down_from"`. These used to throw an error, because the SD bounds came from `unround()`, which does not know these methods, even though `grim()` and `reround()` both do.

- `round_ceiling()`, `round_floor()`, `round_trunc()`, and `round_anti_trunc()` no longer move a number a whole step because of floating-point error. Shifting a number by `digits` decimal places is inexact (e.g., `0.28 * 100` is 28.000000000000004), so `round_ceiling(0.28, 2)` returned 0.29, and `round_floor(0.29, 2)` returned 0.28. These functions now apply the same tolerance that `round_up_from()` and `round_down_from()` already used.

- `unround()` received many fixes:
  - It now handles `threshold` consistently. It used to contradict itself by implicitly using a fixed threshold of 5 in, e.g., `round_up()`.
  - It now has a `symmetric` argument, which can influence unrounding behavior.
  - It now treats `rounding = "even"` as inclusive because the bounds cannot be determined mechanically, and widening the range errs on the side of safety.
  - It now reports the correct boundary inclusion for `rounding = "anti_trunc"` and a negative `x`. The bounds were right, but they carried the inequation signs of the positive case: since anti-truncation rounds away from zero, it is the upper bound that a negative `x` can be reached from, not the lower one.

- `debit()` and `debit_map()` now reconstruct their boundary values the same way as `grim()` and `grimmer()`. DEBIT therefore inherits every fix listed above for `unround()`:
  - It now accepts `rounding = "ceiling_or_floor"`, `"up_from"`, `"down_from"`, and `"up_from_or_down_from"`. These used to throw an error although `debit_map()`'s documentation said that `rounding` is passed on to `debit()`.
  - `threshold` no longer moves the bounds for `rounding = "up_or_down"`, `"up"`, and `"down"`, which round from a fixed 5.
  - `symmetric` now also applies to the reconstruction of the bounds, not just to the re-rounding of the reconstructed SD. DEBIT used to unround asymmetrically and re-round symmetrically within the same call.
  - The reconstructed SD is now compared to the reported SD's range in exact integer arithmetic. DEBIT was the last test to compare bounds in floating point, with a fudge of ±1e-12 in either direction. That fudge also defeated the exclusive bounds of `rounding = "ceiling"`, `"floor"`, `"trunc"`, and the others listed above: a reconstructed SD sitting exactly on such a bound was accepted although the rounding method in question would have carried it away from the reported SD. Some value sets that DEBIT used to pass under these rounding methods are therefore reported as inconsistent now.

- Mapper functions made by `function_map()` no longer return a corrupt tibble when `data` has no rows. The `consistency` column was `NULL`, but the tibble still counted it among its columns.

- A missing value in a key column no longer aborts the tests. `grim()`, `grimmer()`, `debit()`, and their mappers return `NA` for such a case, as they already did for a case whose rounding bounds are undefined. They used to fail from inside an input check with `missing value where TRUE/FALSE needed`, which named neither the column nor the row and read like an internal error rather than like something about the data. Since a mapper tests a whole data frame at once, a single missing value made the other rows untestable too. DEBIT additionally reported a missing value as being outside the range from 0 to 1, because `dplyr::between()` returns `NA` for it. `grimmer_map()` gives such a case the reason `"Missing value"`.

- An undecidable case is now `NA` under every rounding method, not just under the default one. `grim()` derived its bounds from the sign of `x`, which a missing value does not have, so `rounding = "trunc"`, `rounding = "anti_trunc"`, and `symmetric = TRUE` still aborted with `missing value where TRUE/FALSE needed`, as did `grim_map()`, `grim_values()`, `grim_closest()`, and `unround()` with those settings. An unknown `rounding` string is still an input error, whether or not `x` is missing.

- `grimmer()` and `grimmer_map()` no longer abort when GRIM itself is undecidable, which happens with `rounding = "anti_trunc"` at a mean of zero and with a non-positive `n`. GRIMMER branched on the GRIM verdict without allowing for `NA`, and failed with `missing value where TRUE/FALSE needed`. It now returns `NA`, as its documentation says it does for undefined rounding bounds, and `grimmer_map(show_reason = TRUE)` gives the reason `"GRIM undecidable"`.

- `audit()` no longer counts an undecidable case as an inconsistent one. `incons_cases` is the number of rows where `consistency` is `FALSE`, as documented, but it was derived by indexing rows with `!consistency`, and indexing by `NA` returns a row of `NA`s rather than no row at all. The same phantom row could reach `*_map_seq()`, which dispersed values around a case that has no values, and `audit_seq()`, which counted it as a hit.

- `debit_map()` now returns `x` and `sd` as numeric columns, not as strings. This matches `grim_map()`.

- Fixed a pre-existing compatibility issue in `debit_plot()` where a theme element was out of date with recent ggplot2 versions.

- `digits_x` and `digits_sd` now accept one value per row, not just a single value for the whole column. `grim_map()`, `grimmer_map()`, and `debit_map()` take a vector of length 1 or `nrow(data)`; the total-n mappers take a length-2 vector stating the decimal places of the two groups, as in `grim_map_total_n(df, digits_x = c(2, 1))`. Data whose values were reported with different numbers of decimal places could not be tested correctly before: any single `digits_x` was wrong for some of the rows. This is what the Bauer and Francis (2021) case study in `vignette("grim")` needs, since it reports means of 4.71 and 5.3. Sequence mappers still require a single value per column, and now say so instead of failing further down.

- `*_map_seq()` functions now take their dispersion step size from `digits_x` / `digits_sd` rather than from the stored value. A mean reported as 5.30 is stored as `5.3`, so it used to be dispersed in steps of 0.1 while a neighboring 4.71 was dispersed in steps of 0.01, within the same call, under the same `digits_x`, and with both rows labeled `digits_x = 2` in the output. Among other things, this pushed `debit_map_seq()` values outside the range that DEBIT accepts.

- Fixed the example in the error message for a missing `digits_*` argument. It put the argument inside the data frame, as in `grim_map(tibble::tibble(x = 1.4, n = 29, digits_x = 2))`; so following it reproduced the same error. It now reads `grim_map(tibble::tibble(x = 1.4, n = 29), digits_x = 2)`.

- `seq_disperse()` and `seq_disperse_df()` now keep their sequences on the decimal level given by `by` (or, if `by` is not specified, by `from`). Each value used to be computed in plain floating-point arithmetic, so a long enough `dispersion` vector introduced spurious decimal places: `3.14 - (305 * 0.01)` is `0.0899999999999999`, not `0.09`. This made the sequence mappers fail once `dispersion` reached 305, since the dispersed values then had more decimal places than `digits_x` allowed. The same error could also drop a value that sat exactly on `out_min` or `out_max` (#83).

- `audit_seq()` now works on the output of a `*_map_seq()` function whose underlying mapper has no `digits_*` arguments, such as one built for a test of one's own. `function_map_seq()` added a `digits_*` column for every reported variable except `n`, whether or not the mapper had a matching argument, and `audit_seq()` forwards every such column back to the mapper as an argument. The mapper then rejected its own output. Only variables that the mapper accepts a `digits_*` argument for now get a column, which is unchanged for GRIM, GRIMMER, and DEBIT.

- The errors about missing or misspecified `digits_x` and `digits_sd` arguments now name the function the user actually called, in all of the functions that throw them. The name used to be derived by counting frames up the call stack, which only held for the plain mappers: `Vectorize()` inserts `do.call()` and `mapply()` frames, purrr inserts several of its own, and factory-made functions invoke `fun` as a function object, so that frame carries no name at all. In the worst cases an internal error replaced the guidance that the message exists to give: `debit_map_seq(pigs3)` reported `cannot coerce type 'closure' to vector of type 'character'`, and calling a mapper as `scrutiny::grim_map()` produced `` `what` must be a single string ``. The name is now resolved by finding the outermost consistency test function on the call stack, so sequence and total-n mappers name themselves rather than the basic mapper they call internally.

- `audit_seq()` now finds the mapper that produced its input wherever that mapper is defined. A name that cannot be resolved at all now raises a message saying so.

- Functions made by `function_map_total_n()` now work when they are created outside of scrutiny, e.g., in another package. Their bodies call scrutiny-internal helpers such as `absorb_key_args()`, but the factory used to enclose them in the caller's environment, which has no path to those helpers. They are now enclosed in an environment inheriting from scrutiny's namespace, as those made by `function_map()` and `function_map_seq()` already were (#69).

- `debit()` and `debit_map()` now decide a mean of exactly 0 or exactly 1. Both are perfectly consistent -- every value is 0, or every value is 1, and the SD is 0 either way -- but their rounding bounds reach outside of 0 and 1, where the SD of binary data is undefined. `sd_binary_mean_n()` returned `NaN` for such a bound, and the comparison against the reported SD came out as `NA`, so `debit(x = 0, sd = 0, n = 50, digits_x = 2, digits_sd = 2)` was undecidable rather than `TRUE`. The mean's bounds are now clamped to the range that a mean of binary data can occupy, which only ever narrows them. Relatedly, `debit_map()`'s `sd_lower` column no longer reports a negative standard deviation: a negative lower bound is now treated as a bound of zero, as it already was in `grimmer()`.

- `decimal_places()` and `decimal_places_scalar()` now account for scientific notation, which moves the decimal point: `1e-05` has five decimal places and `1.5e3` has none, where both used to be counted by the digits after the point alone (0 and 3). R writes numerics that way by itself -- `as.character(0.0001)` is `"1e-04"` -- so this was not only about strings the user typed. The step size of `seq_disperse()`, `seq_endpoint()`, and `seq_distance()` comes from this count, so those functions were silently wrong below `0.001`: `seq_endpoint(from = 0.0001, to = 0.0005)` returned a single value, and `seq_disperse(from = 7.22, by = 1e-4)` threw an error from `restore_zeros()`. `*_map_seq()` functions were affected through the same count, and more widely: their step size is `1 / (10^digits_x)`, which is itself written in scientific notation from `digits_x = 4` on, so the dispersed values were rounded back to the wrong decimal level and collapsed onto the reported value. `grim_map_seq(tibble::tibble(x = 5.1, n = 41), digits_x = 4)` returned ten rows for `var = "x"` that all had `x` at 5.1, rather than the sequence from 5.0995 to 5.1005; `audit_seq()` then summarized those rows as though they were a real sequence. Three decimal places and below were never affected, because `1 / (10^3)` is written as `0.001`.

- `decimal_places()` and `decimal_places_scalar()` now always agree with each other. They are separate implementations of one rule, and each was missing a part of it: `decimal_places()` counted every character after the separator rather than the digits alone, so `"5.30%"` came out as three decimal places and `"1.2.3"` as three; `decimal_places_scalar()` did not remove surrounding whitespace, and since an exponent is only recognized at the end of the string, a single trailing space hid it, making `"1.5e3 "` one decimal place instead of none. Both now trim, split off the exponent, and count the run of digits immediately after the first separator. No well-formed number changes, because every character of its mantissa is a digit; only malformed and padded input does. A generated corpus in the tests holds the two functions to the same answers.

- `unround()` now checks the lengths of all its vectorized arguments against each other, not just `x` and `rounding`. A `digits` shorter than `x` used to be recycled without a word, so the extra `x` values were unrounded at the wrong number of decimal places: `unround(c("1.0", "2.00", "3.000"), digits = c(1, 2))` reported the bounds of `3.000` as those of a value with one decimal place. One `digits` value per `x` value raises no pairing warning; that warning is still about `x` and `rounding` alone.

- `restore_zeros()` now returns `NA` for a missing value rather than the string `"NA"`. `stringr::str_split_fixed()` gives `NA` an empty mantissa, which counts as fewer decimal places than the target, so `sprintf()` formatted the missing value into the two characters that spell it out.

- `grim_total()` no longer returns `NA` when the number of possible inconsistencies exceeds the integer range, as it does from `digits_x = 10` on (or from `digits_x = 8` with `percent = TRUE`). The result was coerced to integer unconditionally, and `as.integer(1e10)` is `NA` with a warning. It is now a double in every case, where it used to be an integer in most of them. A double is exact well past anything GRIM can produce, and this keeps the return type from depending on how large the count happens to be.

- Sequence mappers such as `grim_map_seq()` now give their own error message when `data` already has a `consistency` column. The check that produces it was called without the name of the test, so cli failed on the missing argument and reported "Could not evaluate cli `{}` expression: `name_test`" instead of saying what was wrong.

- `*_map_total_n()` functions now swap the two groups correctly when the reported statistic's own name contains a digit. The `"1"` and `"2"` suffixes were swapped by replacing those characters anywhere in the column name, which hit the wrong one for a statistic called, say, `t1`, whose columns are `t11` and `t12`.

- `audit_seq()` now orders its `hits_*` and `diff_*` columns by `var`. Undoing the alphabetical order that `split()` imposes takes `rank()`, not `order()`; the two are inverses of each other and agree only up to three variables that don't form a cycle. The column *names* always tracked their values, so no summary was ever wrong -- only the order in which the columns appeared.

- The mappers no longer turn an `n` beyond `.Machine$integer.max` into `NA`. They coerce the `n` column to integer, which is the faithful representation of a sample size, but `as.integer()` discards anything that does not fit and says so only through a bare base-R warning. The verdict had been computed from the real value before that, so `grim_map(tibble::tibble(x = c(5.19, 5.19), n = c(28, 3e9)), digits_x = 2)` returned a row reading `n = NA` and `consistency = TRUE` at once -- while a missing `n` yields an `NA` verdict everywhere else in the package. The coercion now runs only where it is lossless, in all three mapper tiers and in `disperse()` and friends.

- `.name_key_result` now works in `function_map_seq()` and `function_map_total_n()`, not only in `function_map()`. It is documented for all three, but the other two hard-coded the string `"consistency"` in every place they read the test results, so a mapper created with, say, `.name_key_result = "verdict"` failed with base R's "invalid argument type" or "first argument must be a vector" as soon as it was wrapped in a sequence or total-n mapper. `audit_seq()` and `reverse_map_seq()` read the name off a new attribute of the mapper output. A mapper and its sequence or total-n mapper have to be created with the same value; a mismatch now says so instead of failing further down.

- `grim_map_total_n()`, `grimmer_map_total_n()`, and `debit_map_total_n()` now add the test-specific classes `"scrutiny_grim_map_total_n"`, `"scrutiny_grimmer_map_total_n"`, and `"scrutiny_debit_map_total_n"` that `?audit-special` documents. None of them was ever set: only the generic `"scrutiny_map_total_n"` was, so the total-n tier was the one tier of the three whose output could not be dispatched on by test. The sequence tier has always set its counterpart.

- The mappers now reject a `rounding` or `symmetric` of length greater than 1 with the same message `reround()` gives. These describe a single rounding procedure, but the mappers passed them straight down to the `*_scalar()` function, where R's own errors surfaced instead: "no such index at level 1" for `rounding`, and "'length = 2' in coercion to 'logical(1)'" for `symmetric`. The check now sits in `rounding_offsets()`, which is on every one of the three tests' paths. `unround()` is unaffected -- it is documented as vectorized over `rounding`.

- Any error about a mapper's arguments now comes out without `purrr::pmap()`'s "In index: 1." wrapper. The mappers apply the test to the first row on its own before mapping over all of them, which had been done for a missing required argument such as `digits_x` but not for anything else, so an invalid `rounding` string arrived wrapped in the indexed-error context.

- GRIM, GRIMMER, and DEBIT now agree on which value sets they cannot decide, and report all of them as `NA`. Every one of the three reasons from integer data, so a fractional or non-positive `n` or `items` describes no data set for the test to be consistent or inconsistent *with* -- and GRIMMER and DEBIT reconstruct a *sample* SD, so they divide by `n - 1` and need an `n` of at least 2. Each test used to make its own arrangements and none covered all of these: `grim(x = 5.19, n = 20.5, digits_x = 2)` was `FALSE`, `grimmer(x = 3, sd = 1, n = 20, items = 1.5, ...)` was `TRUE`, `grimmer(x = 5, sd = 0, n = 1, ...)` was `FALSE` by way of a `NaN` that `na.rm = TRUE` swallowed, and `debit()` validated `n` not at all, returning `FALSE` at `n = 1` from an `Inf` and at `n = 0` from a reconstructed SD of zero. `grim_probability()` uses the same condition, so the `probability` column can no longer report a number next to an `NA` verdict.

- `grimmer()` no longer hangs or exhausts memory on an enormous `n`. It decides a case by enumerating every integer sum the reported mean admits and, for each of those, every integer sum of squares the reported SD admits; both ranges grow linearly with `n`, and `a:b` simply allocates. `n = 1e8` took about eleven seconds and `n = 3e9` did not finish. Beyond a limit far above any published summary statistic, the function now says what it would have had to allocate and why.

- `decimal_places()` and `decimal_places_scalar()` now return `NA` for `NaN` and the infinities rather than `0`. `NaN` is a missing value everywhere else in the package -- `is.na(NaN)` is `TRUE` -- so counting it as zero decimal places while counting a literal `NA` as `NA` was inconsistent, and an infinity has no decimal places in any meaningful sense either. Both used to be coerced to the strings `"NaN"` and `"Inf"`, which have no decimal point and hence no digits after one.

- `grim_plot()` now says when it leaves value sets out, instead of letting ggplot2 drop them with "Removed N rows containing missing values", which names neither the column nor the reason. A tile is colored by its `consistency`, so an undecidable case has no color to be drawn in; those rows are dropped with a warning naming the count, and if *no* case can be decided that is an error. The same applies to the `digits_x = 0` rows that `split_by_digits = TRUE` leaves out: a mean reported with no decimal places has a fractional portion of zero, so there is no plot for it, and the success message used to count only the plots that were made.

- `grim_plot()` on `grim_map(percent = TRUE)` output now plots percentages on the grid they were tested on. The test divides `x` by 100 and raises its decimal count by 2; the plot did neither, so a percentage reported as `67.4` was drawn at a fractional portion of `0.4` against the raster for one decimal place, while the verdict coloring that tile had been reached at three. The y-axis label has said "% (as decimal)" all along.

- `grim_plot()` explains a `digits_x` of `0` instead of failing on the raster lookup. The precomputed rasters cover one and two decimal places, so a whole-number mean used to produce R's own "object 'grim_raster_0_up_or_down_n' not found". A whole-number *percentage* is unaffected, since the conversion above gives it two decimal places.

- `grim()`, `grimmer()`, `grim_map()`, `grimmer_map()`, `grim_values()`, and `grim_closest()` now return `NA` for an infinite `x` or `sd` instead of aborting with R's "missing value where TRUE/FALSE needed". No data set has an infinite mean or SD, and an infinity has no decimal places to be reported with -- `decimal_places()` returns `NA` for it, as of this release -- so the case is undecidable in exactly the way a missing value is. The abort came from `check_newly_numeric()`, which compared `digits_x` against that `NA`; `grim_values()` and `grim_closest()` got past it and failed in `seq()` instead, with "'from' must be a finite number". `grimmer_map(show_reason = TRUE)` gives the reason as `"Infinite value"`. `debit()` and `debit_map()` are unaffected: a binary mean or SD outside of `[0, 1]` is an input error there, and they have always said so.

## Minor improvements

- `reround_to_fraction(digits = "auto")` no longer errors with "non-numeric argument to mathematical function". The function validated `digits` as a integer before resolving `"auto"` into one, and `is.infinite("auto")` is `FALSE`, so the string went straight into `is_whole_number()`. `reround_to_fraction_level()` has always had the two steps in the right order.

- The internal `check_lengths_congruent()` helper no longer errors on arguments that are in fact the same length. It deduplicates the lengths greater than 1, so that two such arguments are not counted as two different lengths, but the index it used for that was computed over the lengths of *all* the arguments. It was therefore longer than the vector it subset and, whenever a length-1 argument sat between two longer ones, dropped nothing at all. `reround_to_fraction(c(0.4, 0.6), denominator = 2, digits = c(1, 2))` therefore failed with a message saying that `x` and `digits` must have the same length, which they already did. When the lengths really do disagree, the error now also names the pair that disagrees rather than the first two candidates. `unround()`, `rounding_bias()`, and the mappers made by `function_map()` use the same check.

- `reround_to_fraction()` and `reround_to_fraction_level()` now apply both procedures of a compound rounding method to every element of `x`, returning `2 * length(x)` values in `reround()`'s interleaved layout. They used to expand a method such as `"up_or_down"` into `c("up", "down")` and hand that to `reround()`, which *paired* the two procedures with the elements of `x`: `reround_to_fraction(c(0.4, 0.6), denominator = 2)` returned two values, the first rounded up and the second down, rather than all four. For an `x` of length 1, which covers every documented example, the results are unchanged. The `@return` text, which said the length is "always 2" for the compound methods, is corrected accordingly.

  In `reround_to_fraction()`, the subsequent rounding to `digits` now keeps each branch on the procedure that produced it, so the value rounded up in the first step goes on being rounded up. Both functions also gained their first tests.

- `reround()` now requires `rounding`, `threshold`, and `symmetric` to have length 1. They describe a single rounding procedure, which is then applied to all of `x`. Vectorizing them meant pairing values with procedures by position, which is confusing enough that `unround()`, where the behavior is kept for its display use case, warns about it. It also required `check_rounding_singular()` plus a length-congruence check for the ways such a call can be malformed. No consistency test ever made one. `check_rounding_singular()` is gone with it, as is the `Vectorize()` wrapper.

- `reround()` no longer dispatches on `rounding` once per element of `x`. Every `round_*()` function is natively vectorized, so a single rounding procedure, which is what every consistency test uses (once per candidate value per row), is now dispatched once for the whole vector instead of going through `Vectorize()`. On a vector of 1000 values this is about 200 times faster. The output is unchanged, including the interleaved `c(up_1, down_1, up_2, down_2, ...)` layout of the compound methods, and calls that vary `rounding`, `threshold`, or `symmetric` across elements still take the old path.

- `reround()`'s `@return` documentation now describes what the function actually returns. It said "numeric vector of length 1 or 2", which is only true for a single input value: for a vector `x` and a compound rounding method, the result is `2 * length(x)` values. It also spells out the layout and warns against pooling the pairs across elements of `x`, which was the cause of the false-pass bug #85.

- `reround()` and `unround()` now reject a `threshold` outside of the interval it has to lie in. It is the point within a step at which rounding switches direction, so at `0` or `10` one of the two directions can never be taken, which silently turns `"up_from"` or `"down_from"` into ceiling-like or floor-like behavior. Non-numeric, missing, and non-scalar values are rejected as well; fractional thresholds inside the interval keep working.

- Conversely, `threshold = 5` is no longer an error for `rounding = "up_from"`, `"down_from"`, and `"up_from_or_down_from"`. The check that fired there took a threshold of `5` to be the argument's default value showing through rather than a deliberate specification, so any caller that computed a threshold and passed it on failed spuriously at exactly the most common value. `"up_from"` with a threshold of `5` is simply `"up"`, which is a correct answer rather than an error.

- The floating-point tolerance is now expressed through the single `rounding_tolerance` constant in every rounding function. `round_up_from()` and `round_down_from()` used to subtract `.Machine$double.eps^0.5` from `threshold`, which the `/ 10` in their formula turns into the very same additive nudge that `round_ceiling()` and friends apply directly. That equality was important because `unround()`'s bounds assume one shared tolerance, but this was not stated anywhere. Results are unchanged.

- `round_trunc()` and `anti_trunc()` no longer call `dplyr::if_else()` to restore the sign of a value derived from `abs(x)`, nor do the `symmetric` branches of `round_up_from()` and `round_down_from()`. These are the package's innermost primitives, running once per candidate value inside GRIMMER's loop. The one behavioral difference is that a `NaN` input now yields `NaN` rather than `NA`, as it does in `base::round()`.

- `grDevices`, `grid`, and `utils` are now declared in `Imports`. All three are used with `::` -- `grim_plot()` builds its gradient with the first two, and `check_args_disabled()` looks up a package name with the third -- but only the packages they are used alongside were declared.

- `digits_x` and `digits_sd` are now real arguments of `grim_map_total_n()`, `grimmer_map_total_n()`, and `debit_map_total_n()`, in the same position as in the other two mapper tiers: right after `data`, ahead of the key column arguments. They used to reach the total-n mappers through the dots only. That worked, and omitting one still gave the bespoke error, but an argument with no default that is required in every call was invisible to `formals()`, to tab-completion, and to the argument list in the rendered help page.

- The `n` column is now an integer column throughout, including in `disperse()`, `disperse2()`, `disperse_total()`, and `reverse_map_total_n()`. The mappers coerce it because a sample size is a whole number; the dispersion helpers that feed them and the reverse function that reads their output back returned doubles, so the column changed type on the way in and back out again.

- 1,513 lines of fully commented-out code -- about 9% of the package's R source -- have left `R/`. Seven files in the `Collate` field had no live line in them: six moved to `special-scripts/dormant/`, which is not part of the built package, and `R/grimmer-rsprite2.R`, a verbatim copy of rsprite2 code kept as a reference for a revamp that has since happened, was removed (it is still in git history, and the fuller original is under `special-scripts/`). The unused internal helper `dustify()` is gone as well; its last caller was DEBIT's boundary comparison, which this release replaced with exact integer arithmetic.

- The two PDFs under `vignettes/` no longer go into the source tarball. `.Rbuildignore` excluded the `.tex` files they are compiled from, and their `.synctex.gz` files, but not the 266 KB of output itself -- which had no `.Rmd` to register it as a vignette.

- The internal check that `digits_x` and `digits_sd` are not smaller than the number of decimal places already in `x` and `sd` no longer counts those decimal places for the values that pass it. It asked `decimal_places_scalar()`, which renders the value as a string and runs three regular expressions over it, at about 35 microseconds per call -- paid once per key value per row, which made this check the most expensive part of a mapper call. Whether a value can be written with a given number of decimal places is a numeric question, and `x == round(x, digits)` answers it some 200 times faster. That test is one-sided, so the values it leaves undecided, such as the `0.1 + 0.2` that prints as `"0.3"` without being the double for `0.3`, still fall through to counting; the verdicts are unchanged throughout. `decimal_places_scalar()` is about a third faster on its own account, having stopped testing numeric values for the strings `"Inf"` and `"NaN"`, which only a string can be, and stopped running the exponent regexes on values with no `e` in them. Relatedly, `digits_x` and `digits_sd` are now rejected outright if they are negative, which no consistency test can act on, and the whole-number test for them no longer recomputes its tolerance on every call. `is_decidable_n_items()`, which runs once per row in all three tests, was rewritten along the same lines: the two `is_whole_number()` calls are written out, and single values now take a short-circuiting `&&` path instead of allocating a logical vector per operator. Altogether, `grim_map()` is about 2.1 times faster on 5,000 rows, `grimmer_map()` about 2.6 times on 2,000, and `debit_map()` about 1.4 times.

## New features

- New functions `round_ties_up()`, `round_ties_down()`, `round_ties_away()`, and `round_ties_zero()`, and matching `rounding` strings `"ties_up"`, `"ties_down"`, `"ties_away"`, and `"ties_zero"`. Each names a complete tie-breaking procedure, so it says by itself what `rounding` and `symmetric` say together: `"ties_up"` is `"up"` with `symmetric = FALSE`, `"ties_away"` is `"up"` with `symmetric = TRUE`, and likewise for the other two. `symmetric` is ignored for them, since the name already determines the procedure.

  Nothing is deprecated: `round_up()`, `round_down()`, and the `"up"` and `"down"` strings keep working exactly as before. The new names exist because the translation between them and `symmetric` is easy to get backwards, and because "round up" means away from zero in Excel, Java, and Python but on the number line in scrutiny. `round_ties_away()` is unambiguous where `round_up(symmetric = TRUE)` has to be worked out.

  They are deliberately not called `round_half_up()` and friends. `janitor::round_half_up()` is *roundTiesToAway* (`janitor::round_half_up(-2.5)` is `-3`), so a scrutiny function of that name meaning ties toward `+Inf` would take the same arguments and give the opposite answer, with `library()` attach order deciding silently which one ran. And janitor is not an outlier: in Java's `RoundingMode.HALF_UP`, Python's `decimal.ROUND_HALF_UP`, and .NET's `MidpointRounding.AwayFromZero`, "half up" already means away from zero.

  `reround()` and `rounding_offsets()` resolve the new strings through one shared table, so the rounding functions and the bounds cannot come to disagree about what a name means. `grim_plot()` maps them onto the background raster they share with `"up"` and `"down"`. That is exact for a non-negative mean; for a negative one the raster is the mirrored method's, as the `Negative means` section of `?grim_plot` explains.

- `function_map()` has a new `.reported_variadic` argument for tests whose number of key columns is a property of the data rather than of the factory call, as with a test that checks whether the values in any number of columns add up to the value in one specific other column. It names an argument of the `*_scalar()` function that takes a whole row's values as a single vector; the factory-made function then has an argument by that name which selects any number of columns with tidyselect syntax. The selected columns are returned as themselves, so the output stays as rectangular as any other mapper's, and `.reported` may be `NULL` if every key column is variadic. Such a mapper is basic-tier only, because `function_map_seq()` and `function_map_total_n()` derive their own arguments from `.reported` (#42).

- `grimmer()` and `grimmer_map()` have new `min_val` and `max_val` arguments for the bounds of the scale that the values were measured on, such as the endpoints of a Likert scale. If both are given, GRIMMER also tests whether integers from that range could have produced the reported `x` and `sd` at all: the range caps how far the values can spread out around their mean, which rules out standard deviations that are possible on an unbounded scale, and a mean outside of the range is inconsistent by itself. Both bounds refer to a single response, so they don't depend on `items`, and they must be specified together, because a single bound places no limit on the SD. The two new reasons, `"Mean out of scale range"` and `"GRIMMER inconsistent (scale range)"`, are counted by `audit()` in a new `fail_scale` column. This is the same idea as `min_val` and `max_val` in `rsprite2::GRIMMER_test()`, but the bound is derived in closed form and in exact integer arithmetic rather than by constructing a sample. The [strait](https://github.com/ianhussey/strait) package goes much further on the subject, with bounds sharpened by attained extremes, response granularity, or a reported Cronbach's alpha, and with an exact decision procedure (#60).

- New functions `grim_values()` and `grim_closest()` reconstruct the mean or percentage values that integer data of the reported sample size could actually have produced. `grim_values()` returns every achievable value that would have been reported as `x`, and is empty if `x` is GRIM-inconsistent with `n`; `grim_closest()` returns the single achievable value nearest to `x`, whether or not it is consistent. Like `grim()`, both are vectorized and derive their values in closed form and in exact integer arithmetic. Their goal is similar to `rsprite2::GRIM_test(return_values = TRUE)` (#62).

## Lifecycle updates

- `function_map()` can now do everything that the mappers it creates need, which is why all three of `grim_map()`, `grimmer_map()`, and `debit_map()` are made by it (see above). The factory-made function now has a real argument for every argument of the `*_scalar()` function, with the same default, instead of taking them via the dots. Along with that, the factory gained these arguments:
  - `.args_by_row`, for arguments that may have one value per row of `data`, such as `digits_x`. They become columns of the output.
  - `.args_defaults`, for arguments the mapper should have a different default for than the `*_scalar()` function itself.
  - `.cols_helper` and `.cols_helper_merge`, for arguments that may also be given as columns of `data`, such as `items`.
  - `.col_names`, which replaces the non-functional argument of the same name (see below). It names the columns that the `*_scalar()` function's values unpack into when it is asked to show them, as with `show_rec` or `show_reason`.
  - `.cols_derived`, for columns that the `*_scalar()` function does not return at all but that are computed from the same per-row input, such as `probability` in `grim_map()`, which comes from `grim_probability()`.
  - `.name_class_flags`, for logical arguments that change what the numbers in the output mean and that functions downstream of the mapper therefore need to know about, such as `percent` in `grim_map()`, which `grim_plot()` reads off the `scrutiny_percent_true` class.

- `function_map()`'s experimental `.col_control` and `.col_filler` arguments are gone, and `.col_names` works differently, as described above. The three of them were documented as a way to turn additional values from a `*_scalar()` function into columns, but the code they generated addressed variables that the manufactured function does not have, so any use of them failed. `.col_control` was checked and then never referenced at all.

- scrutiny now requires R >= 4.1.0, as do recent versions of tidyverse packages. This is because the package now uses the base pipe `|>`, but also to avoid any incompatibilities with older versions of R.

- scrutiny now requires purrr >= 1.0.0 (#87) and ggplot2 >= 3.4.0, both released in November 2022.

- `grim(tolerance = )` is deprecated, and so is the argument in `grim_map()` and the mappers built on it. GRIM decides which reconstructed means are consistent in exact integer arithmetic, so there is no floating-point comparison for a tolerance to loosen -- the documentation already said the argument has no effect. It was kept "because `grimmer()` and `debit()` inherit it and do use it", which is only half true: `grimmer()` compares reconstructed SDs with `dplyr::near()` and still takes it, but `debit()` compares exact integers, like `grim()`, and never had the argument at all.

## Documentation

- `vignette("rounding-options")` now maps each program to the `rounding` string *and* the `symmetric` setting that reconstructs it, instead of only describing what each program does and leaving the translation to the reader. It also leads with the point that was previously only implicit: for consistency testing you usually don't need to identify the software at all, because the default `"up_or_down"` spans every way of breaking a tie at 5, and committing to a single procedure only ever makes a test stricter.

- Two claims in that vignette's software section were wrong, and both are corrected:
  - Stata was described as "seemingly rounds to even, but the documentation is not very explicit". Its manual is in fact explicit, and says the opposite: "For values of `x` exactly at midpoints [...] `x` is always rounded up to the larger value. For example, `round(4.5)` is 5 and `round(-4.5)` is −4." That is rounding toward `+Inf`, i.e. scrutiny's `"up"` with the default `symmetric = FALSE`. Stata is the only program in the table for which that default is right.
  - SPSS was described as rounding to even by default. The cited source was a page for SPSS MR / Reporter, a different product. SPSS Statistics' `RND()` rounds ties away from zero, i.e. `"up"` with `symmetric = TRUE`, and exposes its rounding fuzz as a user setting (`SET FUZZBITS`).

- The vignette also notes that Excel's and Google Sheets' `ROUNDUP()` rounds *away from zero* rather than toward `+Inf`, so it is not scrutiny's `"ceiling"` for negative numbers, and that `ROUNDDOWN()` is `"trunc"` rather than `"floor"`. The footnote that discussed Excel's naming previously equated `ROUNDUP()` with ceiling.

- `symmetric` is documented as what it is: the axis that separates Excel, SPSS, SAS, and Matlab from Stata and from Java-style rounding on any data containing negative values. `round_up()` has a new `Negative numbers` section, and `vignette("rounding-in-depth")` no longer calls the argument "mostly forgettable".

- Every `round_*()` function applies a floating-point tolerance of about `1.5e-9`, so that `round_up(0.145, 2)` is `0.15` even though `0.145 * 100` is stored as `14.499999999999998`. It is now documented in a new `Floating-point tolerance` section of `round_up()`, along with the range of magnitudes over which it is guaranteed to dominate representation error. The formulas in `vignette("rounding-in-depth")` now include it, so that they match the implementation. `unround()` reports bounds that assume this tolerance, which is why a number and its reconstructed range always agree.

- `round_down_from()`'s `threshold` is documented correctly. It mirrors `round_up_from()`'s threshold along with everything else: `round_down_from()` rounds *down* when the part cut off by rounding is at most `10 - threshold` tenths of a step, so it switches direction at the same point as `round_up_from(threshold = 10 - t)` and differs from it only in sending a value sitting exactly on that point down rather than up. The two readings coincide at the threshold of 5 that `round_up()` and `round_down()` use, which is why nothing in the package depended on the difference. The Rd previously described the argument as the "threshold for rounding up or down, respectively", which suggests the other reading.

- Both vignettes and the Rd of `round_up()` now agree on `base::round()`: it is the right reconstruction of software that rounds binary doubles to even (R, Python, NumPy), and what is unreliable is predicting its output from the decimal display of a number. The three used to state this differently enough that a reader consulting only one of them came away with a different belief.

- Negative values of `digits` are documented: `round_up(1250, digits = -2)` is `1300`, which reconstructs values reported as rounded to the nearest hundred.

- The `digits_x` and `digits_sd` arguments introduced in 1.0.0 are now documented, and all examples were updated to the numeric `x` and `sd` values that the mappers have taken since then. Many of them still passed strings and omitted the `digits_*` arguments, and so failed to run.

- `grim_plot()`'s examples now pass `digits` explicitly. `grim_plot()` reads the decimal count off the `x` column unless told otherwise, and a numeric column cannot carry trailing zeros: `pigs1` contains 5.00, which reads back as zero decimal places rather than two.

- The example datasets no longer show two "See Also" paragraphs each. When `pigs5` was added, its entry was appended in a second `@seealso` block instead of the existing one, and roxygen2 merges the two, so `?pigs1`, `?pigs2`, and `?pigs3` each printed the same list twice over -- once without `pigs5` and once with it. References to `pigs1` are now links, as the references to the other four datasets already were.

# scrutiny 0.6.1

## Bugfixes

-   Fixed a floating-point bug in the GRIMMER implementation, thanks to Aurélien Allard. It could treat some consistent value sets as inconsistent because of spurious precision in decimal numbers, at least in some edge cases with multi-item scales. This affected `grimmer()`, `grimmer_map()`, etc.

## Breaking changes

-   In `grim_plot()`, the aspect ratio was set to 1, so the plot is always square now. This is more pleasant to the eye given the background raster. To get the old behavior back, add `ggplot2::theme(aspect.ratio = NULL)` to the plot. You could also choose some ratio other than `NULL`, of course.

## Minor improvements

-   "Related software" (under "Articles" in the header) now has some new entries.

-   The development-focused article "Consistency tests in depth" was updated (#74).

-   GRIMMER documentation was improved (#66).

-   Source code is now formatted using [Air](https://posit-dev.github.io/air/).

-   Internally, the `scr_` prefix used for S3-based operations such as method dispatch was replaced by `scrutiny_` throughout the package for greater clarity and better disambiguation (#39). This change will be invisible to any users who have not previously looked into scrutiny's S3 features.

## Lifecycle updates

### Removed

All of these had been deprecated since scrutiny 0.5.0:

-   The `show_prob` argument of `grim_map()` was removed.

-   `grim_ratio_upper()` was removed.

-   The 15 functions around `is_subset_of()` were removed.

# scrutiny 0.5.0

The package is now released under the MIT license.

## Breaking changes

-   The `ratio` column in the output of `grim_map()` and `grim_map_seq()` was replaced by a `probability` column. This means:

    -   Numerically, the only difference is that `probability` is zero whenever `ratio` was negative.

    -   Conceptually, it is much easier to interpret: it is the probability that a reported mean or percentage of integer data that has a specific number of decimal places but is otherwise random is GRIM-inconsistent with the reported sample size.

        For example, `probability` is `0.6` for a mean of `1.23` and a sample size of `40`. The same is true for any other mean with two decimal places. Thus, a randomly chosen mean with two decimal places, ostensibly derived from integer data, has a 0.6 probability of being GRIM-inconsistent with the reported sample size.

-   In the functions around `grim_ratio()`, the `x` argument must now be a string. This is consistent with `grim_map()`, `unround()`, etc.; and it prevents erroneous results that could previously occur by omitting trailing zeros.

-   The GRIMMER implementation was debugged, so that `grimmer_map()` etc. may now yield different results in a few cases. In particular, the `items` argument now works correctly, thanks to Aurélien Allard and Lukas Wallrich (#58).

-   `is_seq_dispersed()` now correctly returns `FALSE` if different numbers of missing values at the start and end of `x` mean that `x` cannot be dispersed around `from`.

## New features

-   The `probability` column (see above) is created by a new function, `grim_probability()`.

## Lifecycle updates

### Deprecated

-   As a consequence of the above, the `show_prob` argument of `grim_map()` is now deprecated and will be removed in a future version. It no longer has any effect.

-   `grim_ratio_upper()` is deprecated and will be removed in a future version. It no longer seems very interesting (and likely never was), especially now that the GRIM ratio in general has taken a backseat.

-   All 15 (!) functions around `is_subset_of()` are deprecated and will be removed in a future version. In truth, they were always poorly written and widely out of scope for scrutiny.

### Removed

All of these had been deprecated since scrutiny 0.3.0:

-   `audit_list()` was removed.

-   The `sep` argument in `restore_zeros()` and `restore_zeros_df()` was removed.

-   The `numeric_only` argument in `duplicate_count()` and `duplicate_detect()` was removed.

-   The `na.rm` argument in `duplicate_count_colpair()` was removed.

# scrutiny 0.4.0

This version brings major performance improvements. Furthermore:

## Bugfixes

-   Fixed a bug in `audit_seq()`: If the `dispersion` argument in the preceding call to a function like `grim_map_seq()` was specified as something other than a linearly increasing sequence, the `"diff_*"` columns in the data frames returned by `audit_seq()` may have contained incorrect values.
-   Similarly, `audit_seq()` and `reverse_map_seq()` used to reconstruct the reported values incorrectly if the `dispersion` default was overridden as described above. At least for now, the issue is handled by throwing an error if these functions operate on data frames that are the result of specifying `dispersion` as something other than a linearly increasing sequence.
-   Fixed a bug that incorrectly threw an error in `grim_map_seq()`, other functions made by `function_map_seq()`, as well as `seq_disperse()` and `seq_disperse_df()` if an input value was so close to `out_min` or `out_max` that the output sequence would be shorter than implied by `dispersion` / `.dispersion` , and if `track_var_change` / `.track_var_change` (see below) was `TRUE`. Again, note that the bug only occurred if an error was thrown.

## New features

-   A new vignette lists the options for specifying the `rounding` argument that many scrutiny functions have: `vignette("rounding-options")`.
-   Another new vignette shows the minimal steps to implement a consistency test using scrutiny: `vignette("consistency-tests-simple")`.
-   The output of `grim_map_seq()`, `grimmer_map_seq()`, `debit_map_seq()` and any other function made by `function_map_seq()` now has a `diff_var` column that tracks the difference between the dispersed variable (see the `var` column) and the reported value. Following the `diff_*` columns in the output of `audit_seq()`, this is the number of dispersion steps, not the actual numeric difference.
-   The same `diff_*` columns are now integer, not double.
-   `function_map()`, `function_map_seq()`, and `function_map_total_n()` have a new `.name_key_result` argument that controls the name of the key result column in the output of the factory-made function. This is `"consistency"` by default, but other names will fit better for other kinds of tests. (The results of these tests must still be logical values.)

## Minor changes

-   In `duplicate_count()`, the `count` column in the output tibble was renamed to `frequency`. This makes for a more streamlined frequency table and removes an ambiguity with `duplicate_count_colpair()`, where the `count` output column means something different.
-   In `seq_disperse()` and `seq_disperse_df()`, the `track_var_change` / `.track_var_change` argument was renamed to `track_diff_var` / `.track_diff_var`. The arguments with the old names are still present for now but will be removed in a future version. Also, the unit of these values is now dispersion steps, for consistency with `grim_map_seq()` etc. as well as `audit_seq()`.
-   `grim_total()`, `grim_ratio()`, and `grim_ratio_upper()` now require `x` to have length 1.
-   The docs now link to functions when opened in RStudio, not just on the website.
-   Accordingly, the output of `write_doc_factory_map_conventions()` now renders links. The function also has a new `scrutiny_prefix` argument for use in another package.
-   The "Infrastructure" article was renamed to "Developer tools"; `vignette("devtools")`.
-   Some dependencies that used to be suggested are now imported.

# scrutiny 0.3.0

## Duplicate analysis overhaul

The `duplicate_*()` functions now present their output better and have overall been streamlined. Read more at `vignette("duplicates")`.

-   A new function, `duplicate_tally()`, marks each observation with its overall frequency. It is similar to `duplicate_detect()` but more informative.

-   In `duplicate_count()`:

    -   All values are now treated like character strings, so all can be checked. The `numeric_only` argument is deprecated and should no longer be used.

    -   The output tibble has two new columns, `locations` and `locations_n`. These hold the names of all input columns in which a value appears and the number of these columns. Details are controlled by the new `locations_type` argument.

    -   New `ignore` argument for specifying one or more values that will not be checked for duplicates.

-   In `duplicate_count_colpair()`:

    -   New `total_x` and `total_y` columns in the output show how many non-missing values were checked for duplicates.

    -   New `ignore` argument as in `duplicate_count()`.

    -   The `na.rm` argument is deprecated. It wasn't very useful because missing values are never checked for duplicates.

-   `duplicate_detect()` is superseded. It is less informative than `duplicate_count()` and, in particular, `duplicate_tally()`. Still, it shares in the overhaul:

    -   As in `duplicate_count()`, all values are now treated like character strings, so all can be checked. The `numeric_only` argument is deprecated and should no longer be used.
    -   The duplicate status of missing values is now shown as `NA`.
    -   New `ignore` argument as in `duplicate_count()`.

## Bugfixes

-   Fixed a numeric precision bug in `round_up_from()` and `round_down_from()` that occurred when rounding numbers greater than circa 2100 with a part to be truncated that was equal to 5 on that decimal level (thanks to \@kaz462, #43). These functions are called within `round_up()` and `round_down()`, and indirectly by all consistency-testing functions.

-   Fixed a bug in `audit_seq()` that displayed one "hit" found by varying a given reported value if there were no such hits. The other columns were not affected.

-   Fixed a bug in `function_map()` that displayed the wrong calling function's name in case of an error.

## Minor improvements

-   Documentation for `grim_map_seq()` and all other functions made by `function_map()`, `function_map_seq()`, or `function_map_total_n()` now displays meaningful defaults. Printing the factory-made functions is more meaningful, as well. Internally, they now work with `rlang::new_function()`, which allows for flexible metaprogramming.

-   The experimental function `audit_list()` is deprecated. Just use `audit()` instead. `audit_seq()` and `audit_total_n()` are now documented separately from `audit()` and `audit_list()`.

-   The lifecycle package is now imported and used in formal deprecations such as that of `sep` in the `restore_*()` functions. The janitor package is no longer suggested.

-   Adjusted to new CRAN requirements for `packageVersion()` usage.

-   Some performance improvements.

# scrutiny 0.2.4

-   New `decimal_places_df()` function that takes a data frame and counts the decimal places in all numeric-like columns.
-   Four new predicate functions centered around `is_map_df()` test whether an object is the output of a scrutiny-style mapper function for consistency tests.
-   Newly exported `is_numeric_like()` function to test whether an object (e.g., a string vector) can be coerced to numeric.
-   New `grim_ratio_upper()` function gives an upper bound for `grim_ratio()`.
-   Changes in `split_by_parens()`:
    -   The function now uses a `cols` argument instead of the dots (`...`). This follows [tidyselect development guidelines](https://tidyselect.r-lib.org/articles/tidyselect.html#selections-as-dots-or-as-named-arguments). The default, `cols = everything()`, is to select all columns that contain the `sep` elements (by default, parentheses). Set the new `check_sep` argument to `FALSE` to select all columns regardless.

    -   All other arguments were renamed: they no longer start on a dot. Furthermore, `.col1` and `.col2` have been renamed to `end1` and `end2`.

    -   A warning is now issued if one or more columns can't be split (or is de-selected from splitting). This occurs if a column doesn't contain the `sep` elements.

    -   Internal changes for compatibility with dplyr 1.1.0.
-   In `restore_zeros_df()` as well, the dots (`...`) were replaced by a `cols` argument, and each other argument no longer has a prefix dot. This follows the changes in `split_by_parens()`, but note the default selection restrictions by the new `check_numeric_like` argument. The optional `check_decimals` argument goes even further.
-   Prevent false-positive warnings when printing ggplot objects (they had occurred since ggplot2 3.4.0).

# scrutiny 0.2.3

Some new features and bugfixes:

-   New `audit()` methods for the output of `audit_seq()` and `audit_total_n()`.

-   New `duplicate_count_colpair()` function that checks each combination of columns in a data frame for duplicates.

-   New `restore_zeros_df()` function to easily restore trailing zeros in all numeric-like columns in a data frame.

-   New `seq_length()` function to extend or shorten linear sequences.

-   Bugfixes in the `is_seq_*()` functions.

-   Argument evaluation is now forced in the function factories: `function_map()`, `function_map_seq()`, and `function_map_total_n()`.

-   Some possible corner case issues in `split_by_parens()` are now prevented.

-   Internal changes for compatibility with purrr 1.0.0 and tidyselect 1.2.0.

# scrutiny 0.2.2

This is a patch for CRAN compliance.

-   The package now requires R version \>= 3.4.0 and rlang version \>= 1.0.2.

-   Subtle changes to `split_by_parens()` that users generally won't notice.

-   Minor shifts in the documentation (e.g., `vignette("consistency-tests")` now has instructions on exporting factory-made functions.).

# scrutiny 0.2.1

This is a patch.

-   It reduces the scope of some examples for CRAN compliance.

-   Minor vignette changes.

# scrutiny 0.2.0

This is a massive release, with many new features and improvements all over scrutiny. Most notably, the package now includes an entirely new system for implementing consistency tests.

-   A new vignette lays out how to implement consistency tests using scrutiny's infrastructure. It describes many of the features mentioned below.
-   GRIMMER support was added, as explained in another new vignette. All GRIM and DEBIT functions mentioned below have GRIMMER analogues. For example, `grimmer_map_seq()` is analogous to `grim_map_seq()`.
-   Because of the new, stricter rules for consistency tests, the output of `grim_map()` no longer includes an `items` column by default. Instead, the numbers of items (1 by default) are factored into the output's `n` column. This focuses the presentation on the essence of GRIM.
-   GRIM and DEBIT functions are now somewhat less likely to flag value sets as inconsistent. That is because measures were taken to reduce spurious, computer-induced differences when comparing floating-point numbers. The same applies to the new GRIMMER functions.
-   `function_map()` enables users to quickly create consistency test functions for data frames much like `grim_map()` or `debit_map()`.
-   `grim_map_seq()` checks if GRIM inconsistencies might be due to small errors, and the true values might be close to the reported ones. It varies the inputs up and down in a specified range, holding the respective other ones constant, and tests all those combinations. For summaries, call `audit_seq()` on the results.
-   `debit_map_seq()` does the same for DEBIT.
-   The above two are powered by `function_map_seq()`, which allows users to easily create functions just like these for any consistency test. All that's needed is a data-frame-level consistency testing function like `grim_map()` or `debit_map()`.
-   `grim_map_total_n()` applies GRIM in cases where no group sizes are reported, only total sample sizes. It systematically matches possible group sizes (around half the total) with reported mean or proportion values, GRIM-tests them, and counts the scenarios in which both matches are consistent. For summaries, call `audit_total_n()` on the results.
-   `debit_map_total_n()` does the same for DEBIT.
-   The above two are powered by `function_map_total_n()`, which allows users to easily create new functions like `grim_map_total_n()` or `debit_map_total_n()`, provided a data-frame-level consistency testing function like `grim_map()` or `debit_map()`.
-   On a lower level still, `disperse_total()` takes a total sample size (comprised of the two unknown group sizes on interest) and calls the appropriate group-level function: `disperse()` for even totals, `disperse2()` for odd ones.
-   `seq_disperse()` and `seq_disperse_df()` extend scrutiny's support for string decimal sequences with trailing zeros. They construct sequences centered around the input; a use case not directly covered by `base::seq()`.
-   Predicate functions around `is_seq_linear()` test whether a vector represents a certain kind of numeric sequence.
-   In `debit_map()`, the `x` column is now to the left of the `sd` column if `show_rec` is `FALSE`, in accordance with the `show_rec = TRUE` default.
-   `debit()` is now vectorized.
-   The functions around `is_subset_of() and is_superset_of()` functions now have stricter variants grouped around `is_proper_subset_of()` and `is_proper_superset_of()`.
-   `split_by_parens()` now accepts any pair of separators passed to `.sep` as a length-2 vector.

# scrutiny 0.1.1

This is a patch, mainly fixing a bug that used to affect the presentation of input data in `grim_map()`'s results. It needs to be emphasized that this bug only affected a convenience feature, namely the presentation of certain input data in the output, not the GRIM test itself.

-   Previously, if `percent` was set to `TRUE`, the `x` values were converted to percentages. Because they need to be presented as strings, percentage conversion involves restoring the correct number of trailing zeros. The bug, then, was that all the `x` values appearing in the output (not in the internal computations!) were restored to the same "length" as the single longest one. This was now remedied, and `x` values are restored to their individually appropriate number of trailing zeros.

-   Another bugfix concerns versioning. Previously, the package had an incorrect version number. It was now corrected.

-   The last change was to remove an outdated and potentially misleading paragraph in the documentation of `reround_to_fraction()`.

# scrutiny 0.1.0

-   This version includes an overhaul of `grim_plot()`:

    -   It extends the function to cover cases of `decimals` values greater than 2, using a gradient instead of a raster.

    -   It enables data-free calls to `grim_plot()` with the new `show_data` argument. Resulting plots only display the background raster. This mirrors Figure 1 in Brown and Heathers' GRIM paper. (Although `grim_plot()` as a whole is modeled after this figure, the default addition of empirical summary data is specific to scrutiny.) Like Brown and Heathers, users may wish to create such raster-only plots in order to demonstrate some principled points. The key parameters `decimals` and `rounding` can be controlled directly to make up for the lack of information from `data`.

    -   The function now checks if all input means or proportions (`x`) have the same number of decimal places. If they don't, it throws an error. This strict criterion can be circumvented by specifying the `decimals` argument. However, since each raster is specific to one number of decimal places (and hence cannot be interpreted regarding `x` values with a different number), the recommended solution is to plot `x` values separately --- once for each number of decimal places.

    -   The `show_full_range` argument was removed because I now think it is superfluous.

    -   Previously, there was some space between the raster and the y-axis. It has now been removed.

    -   Test result data points, shown in blue and/or red by default, are now built on top of the raster, which makes for a more distinct appearance.

-   Two new functions, `reround_to_fraction()`, and `reround_to_fraction_level()`, enable fractional rounding, inspired by `janitor::round_to_fraction()`. For example, they might round `0.4` to `0.5` for fractions of `2`. What tells the new functions apart is that they come with all the flexibility of `reround()`. Furthermore, `reround_to_fraction_level()` is closer to a conventional rounding function than the other two.

-   The new version also fixes a bug in `row_to_colnames()`, rewriting the function's core.

-   Another bug was fixed in `grim()` and `grim_map()`, concerning `show_rec`: For `rounding` strings that lead to four reconstructed numbers per `x` value rather than just two, it used to be the case that only the two values corresponding to the first of the two rounding procedures were displayed in the output tibble. Now, all four are displayed, bearing appropriate names.

-   Another bugfix is for the `threshold` argument in `reround()`, which didn't work properly before. This used to affect higher-level functions such as `grim()`, `grim_map()`, `debit()`, and `debit_map()`, as well. The default for `threshold` is now `5` in all such functions. Note that rounding up and down from 5 has been fully functional independently of it.

-   Also in `reround()`, the `rec` argument has been renamed to `x` in accordance with general naming conventions. The `decimals` argument has been renamed to `digits` in accordance with naming conventions among rounding functions.

-   In `split_by_parens()`, ellipsis support was added to protect the user from silent, unexpected results following named arguments in tidy evaluation. The ellipsis package has been added to the Suggests field in `DESCRIPTION`.

-   In some high-level functions, internal checks now determine if the lengths of multiple arguments that are factored into the same internal function call are mutually congruent. That is, if two such arguments are length \> 1, they need to have the same length (which will throw a warning). Otherwise, there will be an explicit and very specific error message.

-   Finally, some minor refactoring and other small changes that users generally won't notice.

# scrutiny 0.0.1

-   Added vignette about other packages for error detection, called `Related software`.
-   Exported `grim_plot()`.
-   Minor refactoring.

# scrutiny 0.0.0.9000

-   Added a `NEWS.md` file to track changes to the package.
