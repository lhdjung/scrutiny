## R CMD check results

0 errors | 0 warnings | 1 note

## Resubmission
This is a resubmission. I now have:

* Added references to the description field of the DESCRIPTION file.

* Added a \value to the tidyeval.Rd file in which it had been missing.

* Removed if(FALSE) wrapping from the function_map.Rd file and moved the problematic example from function_map.Rd to the consistency-tests.Rmd file.

Furthermore, the CRAN check's NOTE on the description field of the DESCRIPTION file might refer to:

* The names of two authors of the references ('Allard' and 'Heathers').

* The URL <https://journals.sagepub.com/doi/10.1177/1948550616673876/>, which is only invalid because of the trailing slash. The corresponding DOI is also flagged by CRAN.
