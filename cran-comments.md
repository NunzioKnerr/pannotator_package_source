## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Comments

This is a resubmission of an archived package.

The previous version was archived because it required the archived package
'leaflet.extras'. All direct references to 'leaflet.extras' have now been
removed.

This version also adds Depends: R (>= 4.1.0), because the package uses the
native pipe operator.

Changes include:
* Replaced leaflet.extras dependency.
* Reordered new drop-downs to appear at top.
* Changed load kmz button to use different progress bar saving screen space.
