# DatawRappr 1.1

## Bug Fixes

* `dw_edit_chart()` now makes calls to Datawrapper API v1, until there's an error fixed in API v3. Datawrapper is working on it. (#29)

## Enhancements

* `dw_export_chart()` returns the published chart as png, pdf or svg (the later two only in paid plans) in R. Thanks to Bob Rudis (@hrbrmstr) for including this function.

* `dw_list_charts()` now returns a data frame (classed as a tibble)

* `dw_edit_chart()` now also includes a _byline_ argument to include the name of the chart's creator.

* The DESCRIPTION now uses the person() format.

* There is now a package-level documentation file that also includes the hex logo.

* There is now a package user-agent string added to the API-call.

* Adds a "Chart Types" section to create and edit functions to avoid the need to go to the online reference.

# DatawRappr 1.0.1

## Bug Fixes / enhancements

* `dw_data_to_chart()` now includes a `parse_dates` argument which automatically transforms all Date- or POSIX*-columns in the dataframe to character vectors, if not set to FALSE. (#25)

# DatawRappr 1.0.0

Is the first public release of DatawRappr.
