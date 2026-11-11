# Deprecated summaries of sequence and total-n mapper output

**\[deprecated\]**

`audit_seq()` and `audit_total_n()` were folded into
[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md) in
scrutiny 1.0.0. It returns the same summaries for the output of sequence
and total-n mappers, and also has `.by`.

## Usage

``` r
audit_seq(data)

audit_total_n(data)
```

## Arguments

- data:

  Output of a sequence mapper, such as
  [`grim_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grim_map_seq.md),
  or of a total-n mapper, such as
  [`grim_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/grim_map_total_n.md).
