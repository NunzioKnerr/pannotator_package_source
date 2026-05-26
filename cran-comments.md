## Resubmission

This is an updated resubmission of an archived package.

The earlier archive was caused by the dependency on the archived package
`leaflet.extras`. That dependency has been removed and the related map behavior
has been reworked using supported packages.

## Current release summary

This release substantially updates the app and package internals. Main changes
include:

* Refactored the app into a modular workspace with Mapping, Image, Annotation,
  and Annotation Table panels.
* Added a persistent Settings workflow that can render below the workspace or
  in a left or right drawer.
* Reworked Main Settings into accordion sections and moved shared actions and
  notices above the tabs.
* Added editable lookup and username table workflows using `rhandsontable`.
* Added project YAML export from Settings and support for
  `run_app(projectSettingsFile = ...)`.
* Added ExifTool dependency checks and install helpers in the app.
* Added a `sourcekmz` field to annotation records and exports so annotations
  record which KMZ file each image came from.
* Replaced `magrittr`, `stringr`, and `readr` usage with native pipe and base R
  helpers where practical.
* Expanded automated tests and updated the vignette text to reflect the current
  workflow.

## Check status

Before final submission, `devtools::check()` should be rerun once more so the
exact `R CMD check` summary in this file reflects the final submission state.

At the current local state:

* `testthat::test_local('.')` passes.
* Test status: 0 failures, 1 expected non-interactive skip.
* There is 1 known local warning from a Sass cache permission issue during a UI
  render test, which does not fail the test suite.
