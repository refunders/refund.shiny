## Edits for cran submission 1.0

Fixed the following: 
 The Description field should not start with the package name,
   'This package' or similar.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
  * This package was previously on CRAN but was archived on 2020-08-26 because it depends on a package (`refund`) that was also archived on that date
  * This issue has been resolved (`refund` is now backon CRAN)


## Edits for cran submission 0.4.1

* removed dontrun in documentation
* expanded on package description
* only exporting user-facing functions
* `dontrun()` is used in examples for `plot_shiny()` function because the function launches a Shiny App, and check cannot complete because app is running
