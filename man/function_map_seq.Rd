% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/function-map-seq.R
\name{function_map_seq}
\alias{function_map_seq}
\title{Create new \verb{*_map_seq()} functions}
\usage{
function_map_seq(
  .fun,
  .var = Inf,
  .reported,
  .name_test,
  .name_key_result = "consistency",
  .name_class = NULL,
  .args_disabled = NULL,
  .dispersion = 1:5,
  .out_min = "auto",
  .out_max = NULL,
  .include_reported = FALSE,
  .include_consistent = FALSE,
  ...
)
}
\arguments{
\item{.fun}{Function such as \code{grim_map()}, or one made by \code{\link[=function_map]{function_map()}}:
It will be used to test columns in a data frame for consistency. Test
results are logical and need to be contained in a column called
\code{"consistency"} that is added to the input data frame. This modified data
frame is then returned by \code{.fun}.}

\item{.var}{String. Variables that will be dispersed by the manufactured
function. Defaults to \code{.reported}.}

\item{.reported}{String. All variables the manufactured function can disperse
in principle.}

\item{.name_test}{String (length 1). The name of the consistency test, such
as \code{"GRIM"}, to be optionally shown in a message when using the
manufactured function.}

\item{.name_key_result}{(Experimental) Optionally, a single string that will
be the name of the key result column in the output. Default is
\code{"consistency"}.}

\item{.name_class}{String. If specified, the tibbles returned by the
manufactured function will inherit this string as an S3 class. Default is
\code{NULL}, i.e., no extra class.}

\item{.args_disabled}{String. Optionally, names of the basic \verb{*_map()}
function's arguments. These arguments will throw an error if specified when
calling the factory-made function.}

\item{.dispersion}{Numeric. Sequence with steps up and down from the reported
values. It will be adjusted to these values' decimal level. For example,
with a reported \code{8.34}, the step size is \code{0.01}. Default is \code{1:5}, for five
steps up and down.}

\item{.out_min, .out_max}{If specified when calling a factory-made function,
output will be restricted so that it's not below \code{.out_min} or above
\code{.out_max}. Defaults are \code{"auto"} for \code{.out_min}, i.e., a minimum of one
decimal unit above zero; and \code{NULL} for \code{.out_max}, i.e., no maximum.}

\item{.include_reported}{Logical. Should the reported values themselves be
included in the sequences originating from them? Default is \code{FALSE} because
this might be redundant and bias the results.}

\item{.include_consistent}{Logical. Should the function also process
consistent cases (from among those reported), not just inconsistent ones?
Default is \code{FALSE} because the focus should be on clarifying
inconsistencies.}

\item{...}{These dots must be empty.}
}
\value{
A function such as those below. ("Testable statistics" are variables
that can be selected via \code{var}, and are then varied. All variables except
for those in parentheses are selected by default.)\tabular{lll}{
   \strong{Manufactured function} \tab \strong{Testable statistics} \tab \strong{Test vignette} \cr
   \code{\link[=grim_map_seq]{grim_map_seq()}} \tab \code{"x"}, \code{"n"}, (\code{"items"}) \tab \code{vignette("grim")} \cr
   \code{\link[=grimmer_map_seq]{grimmer_map_seq()}} \tab \code{"x"}, \code{"sd"}, \code{"n"}, (\code{"items"}) \tab \code{vignette("grimmer")} \cr
   \code{\link[=debit_map_seq]{debit_map_seq()}} \tab \code{"x"}, \code{"sd"}, \code{"n"} \tab \code{vignette("debit")} \cr
}


The factory-made function will also have dots, \code{...}, to pass arguments
down to \code{.fun}, i.e., the basic mapper function such as \code{grim_map()}.
}
\description{
\code{function_map_seq()} is the engine that powers functions such as
\code{\link[=grim_map_seq]{grim_map_seq()}}. It creates new, "factory-made" functions that apply
consistency tests such as GRIM or GRIMMER to sequences of specified
variables. The sequences are centered around the reported values of those
variables.

By default, only inconsistent values are dispersed from and tested. This
provides an easy and powerful way to assess whether small errors in
computing or reporting may be responsible for inconsistencies in published
statistics.

For background and more examples, see the
\href{https://lhdjung.github.io/scrutiny/articles/consistency-tests.html#sequence-mapper}{sequence
mapper section} of \emph{Consistency tests in depth}.
}
\details{
All arguments of \code{function_map_seq()} set the defaults for the
arguments in the manufactured function. They can still be specified
differently when calling the latter.

If functions created this way are exported from other packages, they should
be written as if they were created with
\href{https://purrr.tidyverse.org/reference/faq-adverbs-export.html}{purrr
adverbs}; see explanations there, and examples in the
\href{https://lhdjung.github.io/scrutiny/articles/consistency-tests.html#context-and-export}{export
section} of \emph{Consistency tests in depth}.

This function is a so-called function factory: It produces other functions,
such as \code{\link[=grim_map_seq]{grim_map_seq()}}. More specifically, it is a function operator
because it also takes functions as inputs, such as \code{\link[=grim_map]{grim_map()}}. See
Wickham (2019, ch. 10-11).
}
\section{Conventions}{
 The name of a function returned by
\code{function_map_seq()} should mechanically follow from that of
the input function. For example, \code{\link[=grim_map_seq]{grim_map_seq()}} derives
from \code{\link[=grim_map]{grim_map()}}. This pattern fits best if the input function itself
is named after the test it performs on a data frame, followed by \verb{_map}:
\code{\link[=grim_map]{grim_map()}} applies GRIM, \code{\link[=grimmer_map]{grimmer_map()}} applies GRIMMER, etc.

Much the same is true for the classes of data frames returned by the
manufactured function via the \code{.name_class} argument of
\code{function_map_seq()}. It should be the function's own name preceded
by the name of the package that contains it, or by an acronym of that
package's name. Therefore, some existing classes are
\code{scr_grim_map_seq} and \code{scr_grimmer_map_seq}.
}

\examples{
# Function definition of `grim_map_seq()`:
grim_map_seq <- function_map_seq(
  .fun = grim_map,
  .reported = c("x", "n"),
  .name_test = "GRIM",
)
}
\references{
Wickham, H. (2019). \emph{Advanced R} (Second Edition). CRC
Press/Taylor and Francis Group. https://adv-r.hadley.nz/index.html
}
