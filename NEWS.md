
# pannotator (version 1.1.0)

* Refactored the app shell into a modular workspace with dedicated Mapping, Image, Annotation, and Annotation Table panels.
* Added a persistent Settings workflow that can render below the workspace or in a left/right drawer, with updated layout schematics and single-scroll drawer behavior.
* Reworked Main Settings into accordion sections and moved shared actions and notices above the settings tabs.
* Added editable lookup and username table workflows in Settings using `rhandsontable`, with lookup accordions and on-demand table loading.
* Added a full-width Annotation Table panel for reviewing and editing annotation records in one place.
* Added project YAML export from Settings, including support for launching with `run_app(projectSettingsFile = ...)` and an export-only Google Maps API key for project YAML files.
* Added ExifTool dependency checks and install support in Settings, plus KMZ loading guards when ExifTool is unavailable.
* Replaced popup-driven guidance with persistent inline notices and tour-ready UI structure.
* Improved panel layout behavior, equal-height top panels, annotation card controls, drawer styling, and KMZ runtime file handling.
* Added the `sourcekmz` field to annotation records and exports so annotations record which KMZ file each image came from.
* Replaced `magrittr`, `stringr`, and `readr` usage with native pipe and base R helpers where practical.
* Expanded automated tests and updated the vignette text to reflect the current settings and panel workflow.

# pannotator (version 1.0.1)

* Replaced leaflet.extras dependency.
* Reordered new dropdowns to appear at top.
* Changed load kmz button to use different progress bar saving screen space.

# pannotator (version 1.0.0.4)

* Initial CRAN submission.
