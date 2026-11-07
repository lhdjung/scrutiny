# Critical assessment of the `round_*()` functions

*Assessment of scrutiny's rounding functions, their documentation, and the
rounding vignettes, focused on their stated purpose: reconstructing the
rounding procedures other researchers used to produce published summary
statistics. Written 2026-08-12, ahead of the next major version, so breaking
changes are on the table.*

Files in scope: `R/round.R` (`round_up()`, `round_down()`, `round_up_from()`,
`round_down_from()`), `R/round-ceil-floor.R` (`round_ceiling()`,
`round_floor()`, `round_trunc()`, `round_anti_trunc()`, plus the exported
helper `anti_trunc()`), and their main consumers `reround()` (`R/reround.R`)
and the bounds machinery in `R/unround.R`. Docs: the Rd pages of the above,
`vignette("rounding-options")`, `vignette("rounding-in-depth")`.

## Summary

The forward rounding functions are correct on their home turf — positive
summary statistics with a handful of decimal places — and the recent
bounds-based rework (`rounding_offsets()` et al.) has given the package a
principled inverse that the consistency tests actually run on. The problems
are concentrated elsewhere:

1. **The negative-number story is wrong for the reconstruction use case.**
   With the default `symmetric = FALSE`, `"up"` does not reconstruct what
   Excel, SAS, SPSS, or Matlab do to negative ties — and the vignette's
   software table doesn't say so.
2. **The naming is misleading**, as the rounding-options vignette itself
   concedes in a footnote. `"up"` means *half*-up; Excel's `ROUNDUP()` means
   scrutiny's `"ceiling"`. A major version is the moment to fix this.
3. **Forward and inverse disagree at one point** (`anti_trunc` at zero) and
   are property-tested for coherence at only a single value.
4. **`reround()`'s output contract is a proven bug factory** (interleaved
   length-2 blocks, see #85), its docs misstate the return length, and its
   `Vectorize()` implementation is needlessly slow for GRIMMER's inner loop.
5. **The `*_from` / `threshold` generality has no documented real-world
   target** but taxes the entire stack, from `check_threshold_specified()`
   to a denominator-rescaling loop in `bound_numerators()`.
6. **The vignette that should map "the authors used SPSS" to a `rounding`
   argument never actually states that mapping**, and some of its software
   claims are unsourced or stale.

Details and recommendations below, roughly in decreasing order of importance.

## 1. Negative numbers: the default does not reconstruct common software

`round_up()` with the default `symmetric = FALSE` rounds ties toward
**+Inf** on the number line:

```r
round_up(-2.5)                    # -2
round_up(-2.5, symmetric = TRUE)  # -3
```

That is what Java's `Math.round()` and "school math read literally on the
number line" do. But the software that actually produced most published
summary statistics rounds ties **away from zero** for negative numbers:
Excel's `ROUND(-2.5, 0)` is `-3`, as are SAS's `ROUND()` and Matlab's
`round()`. In IEEE 754 terms these are *roundTiesToAway*, which in scrutiny
is `round_up(symmetric = TRUE)` — the non-default.

So a user who reads `vignette("rounding-options")` ("In Excel, `ROUND()`
rounds up from 5"), and accordingly runs a consistency test with
`rounding = "up"` on data containing negative means — difference scores,
z-scored values, effect sizes, temperature scales — is silently testing
against the wrong procedure at every negative tie. Nothing in the vignette's
software section mentions `symmetric` at all, and the in-depth vignette
actively waves it off: "The two remaining arguments are mostly forgettable."
For the reconstruction use case, `symmetric` is not a technicality; it is
*the* axis that separates Excel/SAS/Matlab from Java-style rounding.

Mitigating factors, worth stating precisely:

- The package-wide default `"up_or_down"` includes both endpoints of the
  ±5-unit interval, so `symmetric` cannot change a consistency verdict under
  the default. The blast radius is limited to users who commit to a single
  direction (`"up"`, `"down"`, `"up_from"`, `"down_from"`) on negative
  values.
- Ties are the only values affected; non-tie rounding is direction-free.

Recommendations:

- **Fix the vignette now**, independently of any API change: the software
  table must say that Excel/SAS/Matlab behavior on negative numbers
  corresponds to `symmetric = TRUE`.
- **For the major version, consider dissolving `symmetric` into explicitly
  named procedures** instead of a modifier flag: `"half_up"` (toward +Inf),
  `"half_down"` (toward −Inf), `"half_away"` (roundTiesToAway),
  `"half_toward"` (ties toward zero). Four names replace 2 procedures × 1
  Boolean, match IEEE 754 vocabulary, and make the Excel-vs-Java distinction
  impossible to overlook. The in-depth vignette's own IEEE table (which
  needs the `symmetric = TRUE` footnote to make `round_up()` fit the
  standard) is the strongest argument that the current factoring is
  backwards.

## 2. Naming: `"up"` is half-up; Excel's "up" is scrutiny's "ceiling"

The rounding-options vignette footnote already makes the case: scrutiny's
`"up"` relies "on a shared understanding with the user that numbers will
only be rounded up from 5", whereas in Excel and in common parlance,
"rounding up" means what scrutiny calls ceiling. `janitor::round_half_up()`
is cited there as the better precedent. This is not cosmetic for an
error-detection package: a user who believes the authors "always rounded up"
(ceiling) and types `rounding = "up"` gets a *different test* than intended,
with bounds `[x−5, x+5)` instead of `(x−10, x]` units. The failure is silent
and produces plausible-looking verdicts.

Recommendation for the major version: rename the family and the `rounding`
strings together, with deprecated aliases for one release cycle:

| Current                          | Suggested                          |
|----------------------------------|------------------------------------|
| `round_up()` / `"up"`            | `round_half_up()` / `"half_up"`    |
| `round_down()` / `"down"`        | `round_half_down()` / `"half_down"`|
| `round_up(symmetric = TRUE)`     | `round_half_away()` / `"half_away"`|
| `round_ceiling()` / `"ceiling"`  | keep (already unambiguous)         |
| `round_trunc()` / `"trunc"`      | keep, or `"toward_zero"`           |
| `round_anti_trunc()`             | see §7 — consider removal          |
| `"up_or_down"`                   | `"half_either"` or keep as alias   |

However, note that the popular janitor package also has a `janitor::round_half_up()` function.

Whatever the exact names, the criterion should be: no string may mean one
thing in scrutiny and a different thing in Excel.

## 3. Two parallel systems, coherence tested at a single point

Since the bounds rework, the package contains two encodings of every
rounding procedure:

- **forward**: the `round_*()` functions, dispatched via `reround()`; still
  load-bearing inside `grimmer_scalar()` (SD reconstruction, `grimmer.R:406`)
  and `debit_scalar()` (`debit.R:178`), and the only thing the exported
  `round_*()` API offers users;
- **inverse**: `rounding_offsets()` / `bound_numerators()` in `unround.R`,
  which AGENTS.md rightly calls the single source of truth for GRIM,
  GRIMMER, and DEBIT.

If these two ever disagree — an offset table edit, a tolerance change in a
forward function — GRIMMER and DEBIT would compare an SD rounded by one
convention against bounds derived from the other, and the verdict could flip
without any test noticing. There *is* a property test tying them together
(`test-unround.R`, "bounds agree with the rounding they invert"), which is
exactly the right idea, but it currently runs at **one value** (`"0.53"`,
positive, `digits = 2`, `threshold = 6`, `symmetric = FALSE`). It cannot see
the negative-number branches of `rounding_offsets()`, the `symmetric`
mirroring, the zero special cases, or the fractional-threshold rescaling
loop.

And the two systems demonstrably *do* disagree at one point already:
`round_anti_trunc(0)` returns `1`, while `rounding_offsets()` declares
`anti_trunc` at zero undefined and returns `NA` bounds (see §7).

Recommendations:

- Extend the property test into a sweep: all rounding strings × positive /
  negative / zero `x` × `symmetric` TRUE/FALSE × a few `digits` and
  `threshold` values, asserting "just inside rounds back to `x`, just
  outside does not" plus endpoint inclusivity. This is cheap (a few hundred
  cases) and would pin the entire contract.
- Document in the code (both files) that `rounding_offsets()` is normative
  and any change to a forward function must be mirrored there — or, more
  ambitiously, derive the forward functions' tie behavior tests from the
  offsets table so there is only one encoding to maintain.

## 4. `reround()`: output contract, speed, and two API warts

**Output shape.** The docs say the return is a "numeric vector of length 1
or 2". For vector input that is false: `reround(c(1.234, 5.678), 2)` returns
a length-4 vector of per-element blocks, `c(up₁, down₁, up₂, down₂)`,
because `Vectorize()` produces a matrix whose attributes are then stripped.
Callers must reverse-engineer the block size — `grimmer.R:424` literally
computes `reps <- length(sd_rec_rounded) / length(integers_possible)` and
carries a comment explaining how pooling the blocks incorrectly caused the
false-pass bug #85. An output contract that internal callers have already
been bitten by is a poor thing to expose to users. For the major version:
return something with shape — a matrix with one column per input, a tibble,
or a pair of vectors — or split the compound methods into their own
interface so that `reround()` always returns `length(x)` values.

**Speed.** `reconstruct_rounded_numbers` is `Vectorize()` (i.e., `mapply()`)
over a scalar `switch`. Every `round_*()` function is natively vectorized;
only the dispatch is scalar. `grimmer_scalar()` calls `reround()` on a
candidate vector once per SD bound per row, and the seq mappers multiply
that by hundreds of rows. Dispatching once when `rounding` has length 1 (the
overwhelmingly common case) and falling back to the slow path otherwise is a
small change with a real payoff.

**`digits = 0L` default.** For a reconstruction interface, `digits` is the
single most consequential parameter, and a silent default of 0 turns a
forgotten argument into wrong-but-plausible results rather than an error.
`unround()` gets this right by forcing the user to either pass strings or
specify `digits`. Make `digits` required in `reround()`.

**`check_threshold_specified()` conflates "unspecified" with "= 5".**
`reround(x, d, "up_from", threshold = 5)` errors even though the threshold
was explicitly given. The intent (steer users to `"up"`) is fine
interactively, but any programmatic caller that computes a threshold and
passes it through will fail spuriously at exactly 5. Detect missingness
(`NULL` default or `rlang`) instead of testing the value.

**Silently ignored arguments.** `threshold` and `symmetric` are accepted and
ignored for methods they don't apply to (`reround(2.34, 1, "ceiling",
symmetric = TRUE)` works without comment). The docs do say "otherwise
irrelevant", but a one-time warning would catch the user who believes
`symmetric` applies to `"ceiling_or_floor"`.

Also worth a look: vectorized `rounding` (different method per element,
with the `check_rounding_singular()` machinery and length checks) is exotic
in `reround()` — its natural home is `unround()`'s display use case. Consider
requiring scalar `rounding` in `reround()` and deleting the checks.

## 5. The floating-point tolerance: sound design, undocumented and split

All eight rounding functions nudge values by ~1.5e-9 so that representation
error cannot move a number across a rounding boundary — e.g.
`round_up(0.145, 2)` correctly gives `0.15` even though `0.145 * 100` is
stored as `14.499999999999998`. This is the right call for reconstruction
(the original software also displayed `0.145` from such a double), and it
correctly matches the *inclusive* lower bound that `unround()` reports for
`"up"`. Three criticisms:

1. **It is implemented twice, differently.** `round-ceil-floor.R` uses the
   named constant `rounding_tolerance` (`eps^0.5 / 10`, `utils.R:1573`).
   `round.R` instead subtracts `eps^0.5` from `threshold`, which after the
   `/ 10` in the formula happens to equal the same additive nudge. That
   equality is load-bearing (the `unround()` property test depends on both
   families honoring the same tolerance) and nowhere stated. Express both
   through the one constant.
2. **It is absent from the user-facing docs.** The Rd for `round_up()`
   claims the functions are "less prone to floating-point number quirks
   than `base::round()`" without saying *how*: values within ~1e-9 below a
   boundary are deliberately treated as on the boundary. Users comparing
   against `janitor::round_half_up()` (sprintf-based, different tie
   behavior on non-representable values) will see discrepancies they can't
   explain from the docs. One paragraph fixes this.
3. **It is absolute, so it has a domain of validity.** Representation error
   of `x * 10^digits` scales with magnitude (~`|x|·10^digits · 2.2e-16`),
   while the nudge is fixed at ~1.5e-9. Beyond `|x·10^digits| ≈ 10^7` the
   guard is no longer guaranteed to dominate representation error, and
   boundary cases can round either way. Irrelevant for means and SDs, but
   `reround()` is advertised for general "reconstruction of statistical
   analyses" (test statistics, sums of squares can be large). Documenting
   the domain is enough; a relative tolerance would break the exactness
   assumptions `unround.R` relies on and isn't worth it.

## 6. `"even"`: the docs pull in two directions

`rounding = "even"` delegates to `base::round()`, which is correct for
reconstructing R, Python 3, and numpy output: those systems break ties by
the parity question *as measured on the binary double*, which is exactly
what post-4.0.0 `base::round()` does. The bounds side handles it well too —
both bounds inclusive, explicitly justified as erring permissive
(`unround.R:94–99`).

The documentation, however, sends mixed messages. `round.R`'s Rd praises
`base::round()` ("works fine", "not meant to replace it"), the in-depth
vignette recommends it for original work, while the options vignette warns
"Rounding to the next even number is not reliable" with no qualifier. All
three statements are defensible in context, but a reader consulting only
one of them walks away with a different belief. The needed distinction is
one sentence: *`base::round()` is the right reconstruction of software that
rounds binary doubles to even; what is unreliable is predicting its output
from the decimal display of a number.* State it once, in the options
vignette, and link the others to it.

A genuinely missing piece for reconstruction: SAS's `ROUNDE()` (half-even
*with* a fuzzing tolerance, per SAS docs) and any software that half-evens
the decimal *string* are not exactly `base::round()`. Probably not worth
implementing — the inclusive-both-bounds treatment already covers them in
consistency tests — but the vignette's SAS entry could say that `"even"` is
the closest match and why that is safe.

## 7. `anti_trunc`: internally inconsistent and of doubtful use

- **Forward/inverse disagreement**: `round_anti_trunc(0)` returns `1` (a
  documented but arbitrary sign choice), while `rounding_offsets()` treats
  `anti_trunc` at zero as undefined (`NA` bounds), as do GRIM and GRIMMER
  ("undecidable"). Two parts of the package answer the same question
  differently. If the function stays, `round_anti_trunc(0)` should return
  `NA` to match the normative bounds machinery.
- **No known real-world target.** `"trunc"` earns its place (integer
  casting, Stata's `trunc()`, floor-like display truncation). But no
  documented statistical software *always* rounds away from zero as its
  rounding mode; the Rd itself only offers "featured here in case they are
  needed for reconstruction". Meanwhile the procedure costs real
  complexity: the zero special case ripples into `grim.R`, `grimmer.R`,
  `grim-values.R`, and the docs of all of them; gotcha #11 in AGENTS.md
  (display contradicting verdicts under `"anti_trunc"`) originated here.
- `anti_trunc()` itself — a digit-less helper, described in its own source
  as an "interlude" — is exported for no evident reason.

Recommendation: in the major version, drop `round_anti_trunc()`,
`anti_trunc()`, and the `"anti_trunc"` string, or demote them clearly to
"theoretical completeness, no known software uses this" status. Removal
deletes special cases from three test implementations. `"ceiling_or_floor"`
remains as the maximally permissive fallback and strictly contains
`anti_trunc`'s bounds.

## 8. The `*_from` family and `threshold`: generality without a customer

`round_up_from()` / `round_down_from()` (and the `"up_from"`,
`"down_from"`, `"up_from_or_down_from"` strings) parameterize the tie
threshold. Neither vignette, nor any Rd, nor the software table names a
single program or publication practice that rounds from a threshold other
than 5. Against that absent benefit, the feature's costs run through the
whole stack:

- the `threshold` argument on `reround()`, `unround()`, `grim()`,
  `grimmer()`, `debit()`, and every mapper built on them;
- `check_threshold_specified()` and its "= 5 means unspecified" wart (§4);
- the fractional-threshold rescaling loop in `bound_numerators()`
  (`unround.R:246–251`), which exists only because `threshold` is
  documented as "Integer" but not validated (`4.5` works; `0` and `10`
  produce degenerate always-up/always-down behavior without comment);
- extra branches in `rounding_offsets()` with their own inclusivity logic
  (the tie-breaking union logic at `unround.R:158–171` is subtle and, per
  §3, untested).

If a forensic case ever does require an exotic threshold,
`"ceiling_or_floor"` already bounds every possible threshold's results, so
consistency testing loses nothing. Recommendation: drop the family in the
major version. If it stays, validate `threshold` (numeric, strictly between
0 and 10), fix the docs ("Integer" is wrong), and fix the `= 5` check.

## 9. The rounding-options vignette doesn't deliver its own promise

The "Rounding at 5 in other software" section is the heart of the
reconstruction use case — it is what lets a user translate "the authors
report SPSS" into a `rounding` argument. As written, it describes each
program's behavior but **never states the corresponding scrutiny setting**.
The user must infer that SPSS default → `"even"`, Excel `ROUND()` → `"up"`
*plus `symmetric = TRUE` for negatives* (§1), Matlab → `"up"` with
`symmetric = TRUE`, and so on. Concrete fixes:

- Rewrite the section as a table: software / function → `rounding` string +
  `symmetric` setting + caveat. Add R itself (relevant since most
  psychology pipelines now run R) and, ideally, numpy and Google Sheets.
- Lead with triage guidance that is currently only implicit: *for
  consistency testing you usually don't need to identify the software at
  all* — the default `"up_or_down"` covers every half-rounding variant and
  `"ceiling_or_floor"` covers everything; committing to a single procedure
  only ever makes a test stricter. That one paragraph would prevent most
  misuse the rest of this assessment worries about.
- Verify and refresh the sources. The SPSS link is a third-party Ipsos
  mirror; the Stata link points at the `trunc()` manual while the text
  hedges ("seemingly rounds to even, but the documentation is not very
  explicit"); Matlab has no link at all. For a package whose subject is
  the reliability of others' reported numbers, its own software claims
  should be verifiable.
- The SAS entry notes `ROUND()` has "a small tolerance" — that is precisely
  the behavior scrutiny's own `rounding_tolerance` mimics (§5); connecting
  the two would justify the design in one line.

In the in-depth vignette, additionally: the displayed formulas for rounding
up/down omit the epsilon adjustment the code applies (and a source comment
in `round.R:110–112` admits the implementation differs from the article's
formula), so code and math don't match as presented; and the "mostly
forgettable" line about `symmetric` should go (§1).

## 10. Tests: the threshold tests assert the wrong thing

In `tests/testthat/test-round.R`:

- `x_up_from_3`, `x_down_from_3`, `x_up_from_8`, `x_down_from_8` (lines
  30–31, 50–51) are computed and **never used**. The "threshold 3" and
  "threshold 8" test blocks actually assert that re-rounding the
  *threshold-5* output reproduces itself — true for any threshold, because
  a number with 2 decimal places is a fixed point of rounding to 2 decimal
  places. The intended assertions were evidently never written, so
  threshold behavior has no direct test.
- There are no tests of the forward functions on negative inputs or with
  `symmetric = TRUE`, and no midpoint-grid test (e.g., all of 0.005, 0.015,
  …, 0.995 against hand-computed expectations) — the one input class where
  these functions differ from every alternative and from each other.

The strongest existing test is the `unround()` round-trip property test;
extending it as in §3 plus adding a midpoint-grid oracle for the forward
functions would cover most of what this file currently misses. The
random-roundtrip tests in `test-round.R` (lines 13–25) are fine as smoke
tests but cannot fail on tie behavior, which is the functions' entire
reason to exist.

## 11. Minor points

- **Negative `digits` work** (`round_up(1250, -2)` → `1300`) but are
  undocumented. Either document or reject; for reconstruction of reported
  statistics they are plausible (values "rounded to the nearest 100").
- The Rd `@return` for the common-rounding page doesn't mention that
  `symmetric` only affects ties of negative numbers; the parameter text
  describes the mechanics but not when it matters.
- The example-section note in `round.R` ("Defining `original` as
  `seq(0.05:0.95, by = 0.1)` would lead to wrong results…") is confusing —
  `0.05:0.95` inside `seq()` is itself a bug — and undercuts the "more
  predictable" claim two paragraphs earlier. Replace with a clean example.
- `round_trunc()` and `anti_trunc()` pull in `dplyr::if_else()` for what
  base `ifelse()`-free arithmetic (`sign(x) * core`) would do without the
  dependency in a hot path. Trivial, but these are the package's innermost
  primitives.
- The doc cross-reference story is good (`@seealso` both ways, vignette
  links throughout) — worth preserving through any renaming.

## Addendum, 2026-08-13: what was done, and four corrections

### Implemented (non-breaking; on branch `polishing`)

- **§9, §1, §6** — `vignette("rounding-options")` rewritten: triage guidance
  first, a software → `rounding` + `symmetric` table with live sources, a
  negative-numbers section, and the one reconciling sentence about `"even"`.
  `vignette("rounding-in-depth")` no longer calls `symmetric` forgettable, its
  formulas now show the epsilon the code applies, and its `base::round()`
  recommendation distinguishes reconstruction from prediction.
- **§5.1, §5.2, §5.3** — one `rounding_tolerance` constant behind all eight
  functions (via a new `tie_offset()`); a `Floating-point tolerance` section in
  the `round_up()` Rd stating the mechanism, its domain of validity, and how it
  differs from `janitor::round_half_up()` and `base::round()`.
- **§4** — `reround()` dispatches once per call rather than once per element
  when `rounding`, `threshold`, and `symmetric` are scalar (~240× faster on a
  1000-value vector; the interleaved layout is preserved explicitly);
  `check_threshold_specified()` replaced by `check_threshold_valid()`, so a
  computed `threshold` of exactly 5 no longer fails and thresholds outside
  `(0, 10)` no longer pass; `@return` now describes the real output shape.
- **§3, §10** — the dead `x_up_from_3` / `x_up_from_8` blocks replaced by real
  oracles; a midpoint-grid oracle for all four `round_*_from()` variants,
  positive and negative, `symmetric` both ways; the `unround()` property test
  extended into a sweep over 12 methods × 7 values × 3 decimal counts ×
  `symmetric` × 3 thresholds, now also asserting bound *inclusivity* (a value
  exactly on an inclusive bound rounds back, one on an exclusive bound does
  not). 8,833 assertions, all passing — forward and inverse agree everywhere.
- **§11** — negative `digits` documented; the confusing `seq(0.05:0.95)` note
  replaced by worked examples; `dplyr::if_else()` dropped from `round_trunc()`,
  `anti_trunc()`, and the `symmetric` branches (a `NaN` now yields `NaN` rather
  than `NA`, as in `base::round()`).

### Correction 1: `"half_up"` is the wrong name, and not because of janitor

§2 notes the `janitor::round_half_up()` collision. It is worse than a name
clash. `janitor::round_half_up(-2.5)` is `-3`: janitor's function is
*roundTiesToAway*, i.e. scrutiny's `round_up(symmetric = TRUE)` — the
non-default. A scrutiny `round_half_up()` meaning ties-toward-`+Inf` would have
the same name, the same signature, and the opposite result on negative ties,
with attach order deciding silently which one runs.

And the deeper problem is not janitor at all. In Java's
`RoundingMode.HALF_UP`, Python's `decimal.ROUND_HALF_UP`, and .NET's
`MidpointRounding.AwayFromZero`, "half up" *already means* ties away from zero,
because "up" in that vocabulary means away from zero. So §1's proposed pair —
`"half_up"` for toward-`+Inf` and `"half_away"` for roundTiesToAway — would put
scrutiny at odds with four ecosystems at once, silently, and namespacing cannot
fix a semantic collision. The criterion §2 states ("no string may mean one thing
in scrutiny and a different thing in Excel") rules out `"half_up"` for the
toward-`+Inf` procedure.

The `ties_*` vocabulary avoids both problems and is already IEEE 754's own:
`round_ties_up()` / `round_ties_down()` / `round_ties_away()` /
`round_ties_zero()`, and matching strings. Note that roundwork already ships
`round_ties_to_away()` and `round_ties_to_even()` as thin wrappers *alongside*
the `round_up()` family — an additive scheme, not a rename, which sidesteps the
collision entirely and costs no deprecation cycle.

### Correction 2: `anti_trunc` has a customer; scrutiny's version isn't it

§7 says no documented software always rounds away from zero. Excel's and Google
Sheets' `ROUNDUP()`, Java's `RoundingMode.UP`, and Python's
`decimal.ROUND_UP` all do — `ROUNDUP(-3.14159, 1)` is `-3.2`, which is also why
the current vignette footnote is wrong to equate `ROUNDUP()` with ceiling.

But they implement `ceiling(abs(x))`, which leaves a value already on the
rounding grid where it is, whereas scrutiny's `anti_trunc()` is
`trunc(abs(x)) + 1`, which always moves: `round_anti_trunc(8.42, 2)` is `8.43`
where `ROUNDUP(8.42, 2)` is `8.42`. (A source comment claimed the two formulas
were equivalent. They agree everywhere except on whole numbers, which is exactly
where the choice lies. Comment corrected.)

So the §7 recommendation inverts. Rather than dropping the procedure, switching
`anti_trunc()` to `ceiling(abs(x))` would make it match real software *and*
delete the special cases §7 objects to: zero stops being undefined (values in
`(-1, +1)` units all round to zero, exactly as under `"trunc"`), the `NA` branch
in `rounding_offsets()` goes away, and so does the `x_num < 0 && upper == 0`
correction in `bound_numerators()`. It is a breaking change to verdicts under
`rounding = "anti_trunc"`, so it is a decision, not a cleanup.

### Correction 3: two software claims in the vignette were wrong

Both are fixed, and both were wrong in the direction that matters.

- **Stata does not round to even.** Its manual is explicit: "For values of `x`
  exactly at midpoints [...] `x` is always rounded up to the larger value. For
  example, `round(4.5)` is 5 and `round(-4.5)` is −4." That is ties toward
  `+Inf` — scrutiny's `"up"` with the *default* `symmetric = FALSE`, and the
  only program in the table for which that default is right. The old claim came
  from a search-visible misreading of `round(R, 2)` ("rounded to the closest
  even number" means the closest multiple of 2).
- **SPSS does not round to even either.** The cited source was a page for SPSS
  MR / Reporter, a different product. SPSS Statistics' `RND()` rounds ties away
  from zero and exposes its rounding fuzz as a user setting (`SET FUZZBITS`) —
  which, incidentally, is the second program after SAS whose documented fuzz
  vindicates scrutiny's own `rounding_tolerance`.

§1's framing survives but narrows: the trap is real, and `symmetric = TRUE` is
right for Excel, SPSS, SAS, and Matlab — but Stata is a genuine customer for the
current default, so the default is not simply wrong.

### Correction 4: `round_down_from()`'s `threshold` is mirrored

Not noted in the assessment, and the reason §10's dead test blocks were never
finished: `round_down_from(threshold = t)` does not round down from `t`. It
rounds *down* when the part cut off by rounding is at most `10 - t` tenths of a
step — i.e. it switches direction at the same point as
`round_up_from(threshold = 10 - t)`, differing only in sending a value exactly
on that point down rather than up. The two readings coincide at `t = 5`, which
is why nothing in the package ever depended on the difference.

This is not a bug: `unround()`'s offsets encode the same thing (verified by the
new sweep), and it is what makes `"up_from_or_down_from"` span an interval
symmetric around `x`. But the Rd described the argument as the "threshold for
rounding up or down, respectively", which states the other reading. Docs fixed;
tests now pin the behavior.

### Decided and implemented, 2026-08-13

**§1, §2 — naming: additive, not a rename.** New `round_ties_up()`,
`round_ties_down()`, `round_ties_away()`, `round_ties_zero()` and matching
`"ties_*"` strings, each naming a complete tie-breaking procedure so that
`symmetric` need not be given separately (and is ignored if it is). Nothing is
deprecated; `round_up()` and `"up"` keep working unchanged. `reround()` and
`rounding_offsets()` resolve the new strings through one shared table, so the
forward and inverse encodings cannot come to disagree about a name.
`grim_plot()` maps them onto the raster they share with `"up"` and `"down"`,
which is exact because the plot's axis is a mean's fractional part and
`symmetric` only touches negatives.

**§7 — `anti_trunc` fixed rather than dropped.** `anti_trunc()` is now
`ceiling(abs(x))`, matching Excel's and Sheets' `ROUNDUP()`, Java's
`RoundingMode.UP`, and Python's `decimal.ROUND_UP` exactly. The special cases
§7 objects to are gone with it: the `NA` branch in `rounding_offsets()` and the
`x_num < 0 && upper == 0` correction in `bound_numerators()`.

One correction to what was predicted when this was decided. The zero case does
not become the wide range `-0.5 < x < 0.5`; it becomes the single point
`0 <= x <= 0`. Every non-zero value, however small, is taken away from zero to
the next step out, so the only value that would be reported as zero is zero
itself. That is still a defined answer rather than `NA` — and a meaningful one:
a mean reported as `0.00` under this method pins the sum to exactly `0`, so
GRIM is decidable there and consistent only for all-zero data. With that, no
rounding method has undefined bounds any more; missing values and a
non-positive `n` are the only undecidable cases left.

Since `anti_trunc`-at-zero had been the package's stock example of an
undecidable case, the tests that used it as a vehicle for `NA` propagation
(`grim_values()`, `debit()`, `grimmer()`) now use a missing value or an `n` of
zero instead. The independent GRIM oracle no longer excludes the case, so the
new bounds are checked against a brute-force walk over candidate sums.

**§8 — `*_from` kept.** The re-commit §8 asks for is done: `threshold`
validated to `(0, 10)`, the "= 5 means unspecified" wart gone, the mirrored
threshold documented, direct oracle tests added.

**§4 — partly.** `reround()` now requires length-1 `rounding`, `threshold`, and
`symmetric`; `check_rounding_singular()` and the `Vectorize()` wrapper are gone.
The structured return and the ignored-argument warning were not taken.

`digits` stays optional, against §4's recommendation. `reround()` is documented
as an interface to all eight `round_*()` functions plus `base::round()`, and
every one of those takes `digits = 0`; a wrapper stricter than everything it
wraps is a wart of its own. The parallel with `unround()` also does not hold:
`unround()` accepts a *string* and counts decimals from it, because its `x` is
the reported value, whereas `reround()`'s `x` is a candidate at full precision
with no decimals to count. And the failure mode is mild — integers, which are
conspicuous — and cannot reach a verdict, since `grim()`, `grimmer()`, and
`debit()` always pass `digits` explicitly. Where the danger is real, at
`grim_map(digits_x =)`, the guardrail already exists as
`error_digits_missing()`.

### Still open

Nothing from this assessment. A structured return for `reround()`'s compound
methods (§4) and a one-time warning for silently ignored arguments (§4) were
considered and declined.

## Suggested priority for the major version

1. Rename the family and `rounding` strings (§2), folding `symmetric` into
   explicit `half_*` procedure names (§1) with deprecated aliases.
2. Fix the software table in `vignette("rounding-options")` — mapping,
   `symmetric`, triage guidance, sources (§9). This is doc-only and could
   ship before the major version.
3. Drop `anti_trunc` and the `*_from`/`threshold` machinery (§7, §8), or
   consciously re-commit to them with validation, real-world justification,
   and tests.
4. Rework `reround()`: required `digits`, structured return, fast scalar
   dispatch (§4).
5. Consolidate and document the tolerance (§5); one constant, one
   explanatory paragraph in the Rd.
6. Extend the forward/inverse property test into a sweep and fix the dead
   threshold tests (§3, §10).
