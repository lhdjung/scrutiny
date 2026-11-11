# Package index

## Consistency tests

Check the numerical consistency of summary statistics

### GRIM

Test reported means or percentages for numerical consistency with
reported sample sizes

- [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md) : The
  GRIM test (granularity-related inconsistency of means)
- [`grim_explain()`](https://lhdjung.github.io/scrutiny/reference/grim_explain.md)
  **\[experimental\]** : Explain a GRIM test step by step
- [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md)
  : GRIM-test many cases at once
- [`grim_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grim_map_seq.md)
  : GRIM-test values around the reported ones
- [`grim_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/grim_map_total_n.md)
  : GRIM-test with hypothetical group sizes
- [`grim_plot_grid()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_grid.md)
  : Visualize GRIM test results
- [`grim_plot_line()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_line.md)
  **\[experimental\]** : Visualize the means that GRIM admits
- [`grim_plot_rates()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_rates.md)
  **\[experimental\]** : Compare GRIM inconsistency rates to random
  means
- [`grim_grid()`](https://lhdjung.github.io/scrutiny/reference/grim_grid.md)
  **\[experimental\]** : GRIM results on a grid of sample sizes and
  means
- [`grim_values()`](https://lhdjung.github.io/scrutiny/reference/grim_values.md)
  [`grim_closest()`](https://lhdjung.github.io/scrutiny/reference/grim_values.md)
  **\[experimental\]** : Achievable means behind a reported one
- [`grim_exp_incon()`](https://lhdjung.github.io/scrutiny/reference/grim-stats.md)
  [`grim_total()`](https://lhdjung.github.io/scrutiny/reference/grim-stats.md)
  : Possible GRIM inconsistencies
- [`grim_granularity()`](https://lhdjung.github.io/scrutiny/reference/grim_granularity.md)
  : Granularity of non-continuous scales
- [`grim_binomial()`](https://lhdjung.github.io/scrutiny/reference/grim_binomial.md)
  **\[experimental\]** : Poisson binomial test for the rate of GRIM
  consistency
- [`grim_binomial_power()`](https://lhdjung.github.io/scrutiny/reference/grim_binomial_power.md)
  **\[experimental\]** : Power of the GRIM binomial test

### GRIMMER

Test reported means and standard deviations for numerical consistency
with reported sample sizes

- [`grimmer()`](https://lhdjung.github.io/scrutiny/reference/grimmer.md)
  : The GRIMMER test (granularity-related inconsistency of means mapped
  to error repeats)
- [`grimmer_explain()`](https://lhdjung.github.io/scrutiny/reference/grimmer_explain.md)
  **\[experimental\]** : Explain a GRIMMER test step by step
- [`grimmer_map()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map.md)
  : GRIMMER-test many cases at once
- [`grimmer_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map_seq.md)
  : GRIMMER-test values around the reported ones
- [`grimmer_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map_total_n.md)
  : GRIMMER-test with hypothetical group sizes
- [`grimmer_plot_line()`](https://lhdjung.github.io/scrutiny/reference/grimmer_plot_line.md)
  **\[experimental\]** : Visualize the standard deviations that GRIMMER
  admits
- [`grimmer_values()`](https://lhdjung.github.io/scrutiny/reference/grimmer_values.md)
  [`grimmer_closest()`](https://lhdjung.github.io/scrutiny/reference/grimmer_values.md)
  **\[experimental\]** : Achievable means and SDs behind a reported pair

### DEBIT

Test reported means and standard deviations of binary data for numerical
consistency with reported sample sizes

- [`debit()`](https://lhdjung.github.io/scrutiny/reference/debit.md) :
  The DEBIT (descriptive binary) test
- [`debit_explain()`](https://lhdjung.github.io/scrutiny/reference/debit_explain.md)
  **\[experimental\]** : Explain a DEBIT test step by step
- [`debit_map()`](https://lhdjung.github.io/scrutiny/reference/debit_map.md)
  : DEBIT-test many cases at once
- [`debit_map_seq()`](https://lhdjung.github.io/scrutiny/reference/debit_map_seq.md)
  : DEBIT-test values around the reported ones
- [`debit_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/debit_map_total_n.md)
  : DEBIT-test with hypothetical group sizes
- [`debit_plot()`](https://lhdjung.github.io/scrutiny/reference/debit_plot.md)
  : Visualize DEBIT results
- [`debit_plot_line()`](https://lhdjung.github.io/scrutiny/reference/debit_plot_line.md)
  **\[experimental\]** : Visualize the standard deviations that DEBIT
  admits
- [`debit_values()`](https://lhdjung.github.io/scrutiny/reference/debit_values.md)
  [`debit_closest()`](https://lhdjung.github.io/scrutiny/reference/debit_values.md)
  **\[experimental\]** : Achievable means and SDs of binary data
- [`sd_binary_groups()`](https://lhdjung.github.io/scrutiny/reference/sd-binary.md)
  [`sd_binary_0_n()`](https://lhdjung.github.io/scrutiny/reference/sd-binary.md)
  [`sd_binary_1_n()`](https://lhdjung.github.io/scrutiny/reference/sd-binary.md)
  [`sd_binary_mean_n()`](https://lhdjung.github.io/scrutiny/reference/sd-binary.md)
  **\[superseded\]** : Standard deviation of binary data

### Percentages and sums

Test reported percentages against their counts, and subgroup sizes
against their totals

- [`percentage_of()`](https://lhdjung.github.io/scrutiny/reference/percentage_of.md)
  **\[experimental\]** : Is a reported percentage consistent with its
  count?
- [`percentage_of_map()`](https://lhdjung.github.io/scrutiny/reference/percentage_of_map.md)
  **\[experimental\]** : Test many reported percentages against their
  counts
- [`percentage_of_map_seq()`](https://lhdjung.github.io/scrutiny/reference/percentage_of_map_seq.md)
  **\[experimental\]** : Test percentages against their counts with
  values around the reported ones
- [`add_up_to()`](https://lhdjung.github.io/scrutiny/reference/add_up_to.md)
  **\[experimental\]** : Do reported counts add up to their total?
- [`add_up_to_map()`](https://lhdjung.github.io/scrutiny/reference/add_up_to_map.md)
  **\[experimental\]** : Test whether reported counts add up to their
  totals

## Duplicate detection

Blunt functions to tentatively discover and count duplicate numeric
values; interpret results with care

- [`duplicate_count()`](https://lhdjung.github.io/scrutiny/reference/duplicate_count.md)
  : Count duplicate values
- [`duplicate_count_colpair()`](https://lhdjung.github.io/scrutiny/reference/duplicate_count_colpair.md)
  : Count duplicate values by column pair
- [`duplicate_tally()`](https://lhdjung.github.io/scrutiny/reference/duplicate_tally.md)
  : Count duplicates at each observation

## Summarize and explain results

Summarize, explain, and document the output of scrutiny’s tests

- [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md) :
  Summarize scrutiny objects
- [`audit-special`](https://lhdjung.github.io/scrutiny/reference/audit-special.md)
  : Classes of sequence and total-n mapper output
- [`explain_map()`](https://lhdjung.github.io/scrutiny/reference/explain_map.md)
  **\[experimental\]** : Explain mapper results row by row
- [`explain_print()`](https://lhdjung.github.io/scrutiny/reference/explain_print.md)
  **\[experimental\]** : Print the explanations of mapper results
- [`scrutiny_meta()`](https://lhdjung.github.io/scrutiny/reference/scrutiny_meta.md)
  **\[experimental\]** : Read the settings a mapper recorded on its
  output

## Developer tools

Helpers for implementing error detection techniques. Unlike the
functions above, these may change in minor releases, with a NEWS entry.

### Function factories

Easily create new mapper functions for consistency tests

- [`function_map()`](https://lhdjung.github.io/scrutiny/reference/function_map.md)
  :

  Create new `*_map()` functions

- [`function_map_seq()`](https://lhdjung.github.io/scrutiny/reference/function_map_seq.md)
  :

  Create new `*_map_seq()` functions

- [`function_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/function_map_total_n.md)
  :

  Create new `*_map_total_n()` functions

- [`reverse_map_seq()`](https://lhdjung.github.io/scrutiny/reference/reverse_map_seq.md)
  : Reconstruct the input of a sequence mapper

- [`reverse_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/reverse_map_total_n.md)
  : Reconstruct the input of a total-n mapper

- [`absorb_key_args()`](https://lhdjung.github.io/scrutiny/reference/absorb_key_args.md)
  **\[superseded\]** : Rename key columns according to the key arguments

### Counting decimal places

- [`decimal_places()`](https://lhdjung.github.io/scrutiny/reference/decimal_places.md)
  : Count decimal places

### Sequence generation

- [`seq_disperse()`](https://lhdjung.github.io/scrutiny/reference/seq_disperse.md)
  [`seq_disperse_df()`](https://lhdjung.github.io/scrutiny/reference/seq_disperse.md)
  : Sequences dispersed around a value
- [`disperse()`](https://lhdjung.github.io/scrutiny/reference/disperse.md)
  [`disperse2()`](https://lhdjung.github.io/scrutiny/reference/disperse.md)
  [`disperse_total()`](https://lhdjung.github.io/scrutiny/reference/disperse.md)
  : Vary hypothetical group sizes

### Consistency test helpers

Call these helpers inside of your own functions that implement
consistency tests at various levels

- [`check_mapper_input_colnames()`](https://lhdjung.github.io/scrutiny/reference/check_mapper_input_colnames.md)
  **\[superseded\]** : Check that a mapper's input has the right column
  names

- [`manage_helper_col()`](https://lhdjung.github.io/scrutiny/reference/manage_helper_col.md)
  **\[superseded\]** : Manage a helper column

- [`audit_cols_minimal()`](https://lhdjung.github.io/scrutiny/reference/audit_cols_minimal.md)
  :

  Compute minimal
  [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
  summaries

### Data frame predicates

Test for mapper function output

- [`is_map_df()`](https://lhdjung.github.io/scrutiny/reference/data-frame-predicates.md)
  [`is_map_basic_df()`](https://lhdjung.github.io/scrutiny/reference/data-frame-predicates.md)
  [`is_map_seq_df()`](https://lhdjung.github.io/scrutiny/reference/data-frame-predicates.md)
  [`is_map_total_n_df()`](https://lhdjung.github.io/scrutiny/reference/data-frame-predicates.md)
  : Is an object the output of a mapper function?

## Data wrangling

Automate formatting tasks with a special relevance to error detection

- [`split_by_parens()`](https://lhdjung.github.io/scrutiny/reference/split_by_parens.md)
  : Split columns by parentheses, brackets, braces, or similar
- [`before_parens()`](https://lhdjung.github.io/scrutiny/reference/parens-extractors.md)
  [`inside_parens()`](https://lhdjung.github.io/scrutiny/reference/parens-extractors.md)
  : Extract substrings from before and inside parentheses
- [`row_to_colnames()`](https://lhdjung.github.io/scrutiny/reference/row_to_colnames.md)
  : Turn row values into column names

## Datasets

Small example datasets to demonstrate how
[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md)
and
[`debit_map()`](https://lhdjung.github.io/scrutiny/reference/debit_map.md)
work

- [`pigs1`](https://lhdjung.github.io/scrutiny/reference/pigs1.md) :
  Means and sample sizes for GRIM-testing
- [`pigs2`](https://lhdjung.github.io/scrutiny/reference/pigs2.md) :
  Percentages and sample sizes for GRIM-testing
- [`pigs3`](https://lhdjung.github.io/scrutiny/reference/pigs3.md) :
  Binary means and standard deviations for DEBIT-testing
- [`pigs4`](https://lhdjung.github.io/scrutiny/reference/pigs4.md) :
  Data with duplications
- [`pigs5`](https://lhdjung.github.io/scrutiny/reference/pigs5.md) :
  Means, SDs, and sample sizes for GRIMMER-testing
