# Package index

## Consistency tests

Check the numerical consistency of summary statistics; three tests
currently implemented —

### GRIM

Test reported means or percentages for numerical consistency with
reported sample sizes

- [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md) : The
  GRIM test (granularity-related inconsistency of means)
- [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md)
  : GRIM-test many cases at once
- [`grim_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grim_map_seq.md)
  : GRIM-testing with dispersed inputs
- [`grim_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/grim_map_total_n.md)
  : GRIM-testing with hypothetical group sizes
- [`grim_plot()`](https://lhdjung.github.io/scrutiny/reference/grim_plot.md)
  : Visualize GRIM test results
- [`grim_probability()`](https://lhdjung.github.io/scrutiny/reference/grim-stats.md)
  [`grim_ratio()`](https://lhdjung.github.io/scrutiny/reference/grim-stats.md)
  [`grim_total()`](https://lhdjung.github.io/scrutiny/reference/grim-stats.md)
  : Possible GRIM inconsistencies
- [`grim_granularity()`](https://lhdjung.github.io/scrutiny/reference/grim_granularity.md)
  [`grim_items()`](https://lhdjung.github.io/scrutiny/reference/grim_granularity.md)
  : Granularity of non-continuous scales

### GRIMMER

Test reported means and standard deviations for numerical consistency
with reported sample sizes

- [`grimmer()`](https://lhdjung.github.io/scrutiny/reference/grimmer.md)
  : The GRIMMER test (granularity-related inconsistency of means mapped
  to error repeats)
- [`grimmer_map()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map.md)
  : GRIMMER-test many cases at once
- [`grimmer_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map_seq.md)
  : GRIMMER-testing with dispersed inputs
- [`grimmer_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map_total_n.md)
  : GRIMMER-testing with hypothetical group sizes

### DEBIT

Test reported means and standard deviations of binary data for numerical
consistency with reported sample sizes

- [`debit()`](https://lhdjung.github.io/scrutiny/reference/debit.md) :
  The DEBIT (descriptive binary) test
- [`debit_map()`](https://lhdjung.github.io/scrutiny/reference/debit_map.md)
  : Apply DEBIT to many cases
- [`debit_map_seq()`](https://lhdjung.github.io/scrutiny/reference/debit_map_seq.md)
  : Using DEBIT with dispersed inputs
- [`debit_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/debit_map_total_n.md)
  : Use DEBIT with hypothetical group sizes
- [`debit_plot()`](https://lhdjung.github.io/scrutiny/reference/debit_plot.md)
  : Visualize DEBIT results
- [`sd_binary_groups()`](https://lhdjung.github.io/scrutiny/reference/sd-binary.md)
  [`sd_binary_0_n()`](https://lhdjung.github.io/scrutiny/reference/sd-binary.md)
  [`sd_binary_1_n()`](https://lhdjung.github.io/scrutiny/reference/sd-binary.md)
  [`sd_binary_mean_n()`](https://lhdjung.github.io/scrutiny/reference/sd-binary.md)
  : Standard deviation of binary data

## Duplicate detection

Blunt functions to tentatively discover and count duplicate numeric
values; interpret results with care

- [`duplicate_count()`](https://lhdjung.github.io/scrutiny/reference/duplicate_count.md)
  : Count duplicate values
- [`duplicate_count_colpair()`](https://lhdjung.github.io/scrutiny/reference/duplicate_count_colpair.md)
  : Count duplicate values by column
- [`duplicate_tally()`](https://lhdjung.github.io/scrutiny/reference/duplicate_tally.md)
  : Count duplicates at each observation

## Summarize scrutiny tests

Follow up on scrutiny’s testing functions by computing specific summary
statistics

- [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md) :
  Summarize scrutiny objects
- [`audit_seq()`](https://lhdjung.github.io/scrutiny/reference/audit-special.md)
  [`audit_total_n()`](https://lhdjung.github.io/scrutiny/reference/audit-special.md)
  : Summarize output of sequence mappers and total-n mappers

## Developer tools

Helpers for implementing error detection techniques

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
  :

  Reverse the `*_map_seq()` process

- [`reverse_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/reverse_map_total_n.md)
  :

  Reverse the `*_map_total_n()` process

- [`check_args_disabled()`](https://lhdjung.github.io/scrutiny/reference/check_args_disabled.md)
  : Check that disabled arguments are not specified

- [`check_factory_dots()`](https://lhdjung.github.io/scrutiny/reference/check_factory_dots.md)
  : Check that no dots-argument is misspelled

- [`absorb_key_args()`](https://lhdjung.github.io/scrutiny/reference/absorb_key_args.md)
  : Absorb key arguments from the user's call

### Rounding

Reconstruct rounding procedures

- [`round_up_from()`](https://lhdjung.github.io/scrutiny/reference/rounding-common.md)
  [`round_down_from()`](https://lhdjung.github.io/scrutiny/reference/rounding-common.md)
  [`round_up()`](https://lhdjung.github.io/scrutiny/reference/rounding-common.md)
  [`round_down()`](https://lhdjung.github.io/scrutiny/reference/rounding-common.md)
  : Common rounding procedures
- [`round_ceiling()`](https://lhdjung.github.io/scrutiny/reference/rounding-uncommon.md)
  [`round_floor()`](https://lhdjung.github.io/scrutiny/reference/rounding-uncommon.md)
  [`round_trunc()`](https://lhdjung.github.io/scrutiny/reference/rounding-uncommon.md)
  [`anti_trunc()`](https://lhdjung.github.io/scrutiny/reference/rounding-uncommon.md)
  [`round_anti_trunc()`](https://lhdjung.github.io/scrutiny/reference/rounding-uncommon.md)
  : Uncommon rounding procedures
- [`reround()`](https://lhdjung.github.io/scrutiny/reference/reround.md)
  : General interface to reconstructing rounded numbers
- [`reround_to_fraction()`](https://lhdjung.github.io/scrutiny/reference/fractional-rounding.md)
  [`reround_to_fraction_level()`](https://lhdjung.github.io/scrutiny/reference/fractional-rounding.md)
  : Generalized rounding to the nearest fraction of a specified
  denominator
- [`unround()`](https://lhdjung.github.io/scrutiny/reference/unround.md)
  : Reconstruct rounding bounds
- [`rounding_bias()`](https://lhdjung.github.io/scrutiny/reference/rounding_bias.md)
  : Compute rounding bias

### Counting decimal places

- [`decimal_places()`](https://lhdjung.github.io/scrutiny/reference/decimal_places.md)
  [`decimal_places_scalar()`](https://lhdjung.github.io/scrutiny/reference/decimal_places.md)
  : Count decimal places
- [`decimal_places_df()`](https://lhdjung.github.io/scrutiny/reference/decimal_places_df.md)
  : Count decimal places in a data frame

### Sequence generation

- [`seq_endpoint()`](https://lhdjung.github.io/scrutiny/reference/seq-decimal.md)
  [`seq_distance()`](https://lhdjung.github.io/scrutiny/reference/seq-decimal.md)
  [`seq_endpoint_df()`](https://lhdjung.github.io/scrutiny/reference/seq-decimal.md)
  [`seq_distance_df()`](https://lhdjung.github.io/scrutiny/reference/seq-decimal.md)
  : Sequence generation at decimal level
- [`seq_test_ranking()`](https://lhdjung.github.io/scrutiny/reference/seq_test_ranking.md)
  : Rank sequence test results
- [`seq_disperse()`](https://lhdjung.github.io/scrutiny/reference/seq_disperse.md)
  [`seq_disperse_df()`](https://lhdjung.github.io/scrutiny/reference/seq_disperse.md)
  : Sequence generation with dispersion at decimal level
- [`disperse()`](https://lhdjung.github.io/scrutiny/reference/disperse.md)
  [`disperse2()`](https://lhdjung.github.io/scrutiny/reference/disperse.md)
  [`disperse_total()`](https://lhdjung.github.io/scrutiny/reference/disperse.md)
  : Vary hypothetical group sizes
- [`seq_length()`](https://lhdjung.github.io/scrutiny/reference/seq_length.md)
  [`` `seq_length<-`() ``](https://lhdjung.github.io/scrutiny/reference/seq_length.md)
  : Set sequence length
- [`is_seq_linear()`](https://lhdjung.github.io/scrutiny/reference/seq-predicates.md)
  [`is_seq_ascending()`](https://lhdjung.github.io/scrutiny/reference/seq-predicates.md)
  [`is_seq_descending()`](https://lhdjung.github.io/scrutiny/reference/seq-predicates.md)
  [`is_seq_dispersed()`](https://lhdjung.github.io/scrutiny/reference/seq-predicates.md)
  : Is a vector a certain kind of sequence?

### Documentation templates

Return standardized building blocks for documenting specific kinds of
functions

- [`write_doc_audit()`](https://lhdjung.github.io/scrutiny/reference/write_doc_audit.md)
  :

  Documentation template for
  [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)

- [`write_doc_audit_seq()`](https://lhdjung.github.io/scrutiny/reference/write_doc_audit_seq.md)
  :

  Documentation template for
  [`audit_seq()`](https://lhdjung.github.io/scrutiny/reference/audit-special.md)

- [`write_doc_audit_total_n()`](https://lhdjung.github.io/scrutiny/reference/write_doc_audit_total_n.md)
  :

  Documentation template for
  [`audit_total_n()`](https://lhdjung.github.io/scrutiny/reference/audit-special.md)

- [`write_doc_factory_map_conventions()`](https://lhdjung.github.io/scrutiny/reference/write_doc_factory_map_conventions.md)
  : Documentation template for function factory conventions

### Consistency test helpers

Call these helpers inside of your own functions that implement
consistency tests at various levels

- [`check_mapper_input_colnames()`](https://lhdjung.github.io/scrutiny/reference/check_mapper_input_colnames.md)
  : Check that a mapper's input has correct column names

- [`manage_helper_col()`](https://lhdjung.github.io/scrutiny/reference/manage_helper_col.md)
  : Helper column operations

- [`manage_key_colnames()`](https://lhdjung.github.io/scrutiny/reference/manage_key_colnames.md)
  : Enable name-independent key column identification

- [`unnest_consistency_cols()`](https://lhdjung.github.io/scrutiny/reference/unnest_consistency_cols.md)
  : Unnest a test result column

- [`audit_cols_minimal()`](https://lhdjung.github.io/scrutiny/reference/audit_cols_minimal.md)
  :

  Compute minimal
  [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
  summaries

- [`check_audit_special()`](https://lhdjung.github.io/scrutiny/reference/check_audit_special.md)
  :

  Alert user if more specific `audit_*()` summaries are available

### Data frame predicates

Test for mapper function output

- [`is_map_df()`](https://lhdjung.github.io/scrutiny/reference/data-frame-predicates.md)
  [`is_map_basic_df()`](https://lhdjung.github.io/scrutiny/reference/data-frame-predicates.md)
  [`is_map_seq_df()`](https://lhdjung.github.io/scrutiny/reference/data-frame-predicates.md)
  [`is_map_total_n_df()`](https://lhdjung.github.io/scrutiny/reference/data-frame-predicates.md)
  : Is an object a consistency test output tibble?

### Testing for subsets / supersets

Predicate functions for logical relations between vectors, supporting
flexible data entry and tidy evaluation

- [`is_subset_of()`](https://lhdjung.github.io/scrutiny/reference/subset-superset.md)
  [`is_superset_of()`](https://lhdjung.github.io/scrutiny/reference/subset-superset.md)
  [`is_equal_set()`](https://lhdjung.github.io/scrutiny/reference/subset-superset.md)
  [`is_proper_subset_of()`](https://lhdjung.github.io/scrutiny/reference/subset-superset.md)
  [`is_proper_superset_of()`](https://lhdjung.github.io/scrutiny/reference/subset-superset.md)
  [`is_subset_of_vals()`](https://lhdjung.github.io/scrutiny/reference/subset-superset.md)
  [`is_superset_of_vals()`](https://lhdjung.github.io/scrutiny/reference/subset-superset.md)
  [`is_equal_set_vals()`](https://lhdjung.github.io/scrutiny/reference/subset-superset.md)
  [`is_proper_subset_of_vals()`](https://lhdjung.github.io/scrutiny/reference/subset-superset.md)
  [`is_proper_superset_of_vals()`](https://lhdjung.github.io/scrutiny/reference/subset-superset.md)
  [`is_subset_of_vecs()`](https://lhdjung.github.io/scrutiny/reference/subset-superset.md)
  [`is_superset_of_vecs()`](https://lhdjung.github.io/scrutiny/reference/subset-superset.md)
  [`is_equal_set_vecs()`](https://lhdjung.github.io/scrutiny/reference/subset-superset.md)
  [`is_proper_subset_of_vecs()`](https://lhdjung.github.io/scrutiny/reference/subset-superset.md)
  [`is_proper_superset_of_vecs()`](https://lhdjung.github.io/scrutiny/reference/subset-superset.md)
  **\[deprecated\]** : Test for subsets, supersets, and equal sets

### Testing for numeric-like vectors

- [`is_numeric_like()`](https://lhdjung.github.io/scrutiny/reference/is_numeric_like.md)
  : Test whether a vector is numeric or coercible to numeric

## Data wrangling

Automate formatting tasks with a special relevance to error detection

- [`restore_zeros()`](https://lhdjung.github.io/scrutiny/reference/restore_zeros.md)
  [`restore_zeros_df()`](https://lhdjung.github.io/scrutiny/reference/restore_zeros.md)
  : Restore trailing zeros
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
  Binary means and standard deviations for using DEBIT
- [`pigs4`](https://lhdjung.github.io/scrutiny/reference/pigs4.md) :
  Data with duplications
- [`pigs5`](https://lhdjung.github.io/scrutiny/reference/pigs5.md) :
  Means, SDs, and sample sizes for GRIMMER-testing

## Superseded

- [`duplicate_detect()`](https://lhdjung.github.io/scrutiny/reference/duplicate_detect.md)
  **\[superseded\]** : Detect duplicate values
