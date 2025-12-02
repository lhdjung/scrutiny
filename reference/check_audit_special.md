# Alert user if more specific `audit_*()` summaries are available

(Note: Ignore this function if your
[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
method calls
[`audit_cols_minimal()`](https://lhdjung.github.io/scrutiny/reference/audit_cols_minimal.md).)

Call `check_audit_special()` within an
[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
method for a consistency test mapper function, such as
`audit.scr_grim_map()`. It checks if the input data frame was the
product of a function produced by
[`function_map_seq()`](https://lhdjung.github.io/scrutiny/reference/function_map_seq.md)
or
[`function_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/function_map_total_n.md).

If so, the function issues a gentle alert to the user that points to
[`audit_seq()`](https://lhdjung.github.io/scrutiny/reference/audit-special.md)
or
[`audit_total_n()`](https://lhdjung.github.io/scrutiny/reference/audit-special.md),
respectively.

## Usage

``` r
check_audit_special(data, name_test)
```

## Arguments

- data:

  The [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
  method's input data frame.

- name_test:

  String (length 1). Short, plain-text name of the consistency test,
  such as `"GRIM"`.

## Value

No return value. Might print an alert.

## See also

[`vignette("consistency-tests-in-depth")`](https://lhdjung.github.io/scrutiny/articles/consistency-tests-in-depth.md),
for context.
