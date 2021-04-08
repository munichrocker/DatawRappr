# DatawRappr 1.2

## Bug Fixes

* Replace `list.append` to base `append` for `...` in `dw_edit_chart()`. (fixes #59)

* Add transparent export option.

## Enhancements

* Allow to switch data upload type between csv and tsv. (f9e078e2cf1632d29cd9475246e9a1e3a7c2f1de, as proposed in #57)

* Change `cat`-messages to `message` to allow standardized suppression of messages. (209c63326e256c32f4ac22e5d209bc3a33aed4ac, as proposed in #56) 

# DatawRappr 1.1.4

## Bug Fixes

* Replace `parsed` in `sprintf`-option to R with a `httr`-call in `dw_handle_errors()`.

## Enhancements

* Introducing a `dw_legend_to_string()`-function to create a mobile-friendly legend. But will already be deprecated soon, as Datawrappers now offers a similar option by default.

* `dw_publish_chart()` can now return a object which contains information about the chart (id, embed-code, url).

* Add a `theme`-argument to `dw_create_chart()`.

# DatawRappr 1.1.3

## Bug Fixes

* Fix URLs to chart types and chart properties in `dw_create_chart()` and `dw_edit_chart()`. (fixes #38)

## Enhancements

* Adds a `axes`-argument to `dw_edit_chart()` which enables users to change the axes from within {DatawRappr}.

* Includes a basemap-dump `dw_basemaps` as a data.frame (#36)

* Adds the three dots ... to `dw_edit_chart()` to allow any modification to the call-body.

# DatawRappr 1.1.2

## Bug Fixes

* Fixes `handle_reset()`-issue - hopefully permanently by including it into `dw_publish_chart()`. Thanks to @fin! (fixes #31)

* Adds a new way to `dw_data_to_chart()` to allow data which contain commas, quotes or newlines to be parsed correctly before sending it to Datawrapper. (fixes #34)

* Clearifies error messages in case something goes wrong when communicating with the API. (fixes #32)

# DatawRappr 1.1.1

## Bug Fixes

* Adds a `handle_reset()` to `dw_edit_chart()` to allow multiple edit-calls to the API during a session. Until now a 401-error was raised. (fixes #31)

# DatawRappr 1.1

## Bug Fixes

* `dw_edit_chart()` is working as expected again, after an API-error was resolved by Datawrapper. (#29)

## Enhancements

* `dw_export_chart()` returns the published chart as png, pdf or svg (the later two only in paid plans) in R. Thanks to Bob Rudis (@hrbrmstr) for including this function.

* `dw_list_charts()` now returns a data frame (classed as a tibble).

* `dw_edit_chart()` now also includes a _byline_ argument to include the name of the chart's creator. (#30)

* The DESCRIPTION now uses the person() format.

* There is now a package-level documentation file that also includes the hex logo.

* There is now a package user-agent string added to the API-call.

* Adds a "Chart Types" section to create and edit functions to avoid the need to go to the online reference.

# DatawRappr 1.0.1

## Bug Fixes / enhancements

* `dw_data_to_chart()` now includes a `parse_dates` argument which automatically transforms all Date- or POSIX*-columns in the dataframe to character vectors, if not set to FALSE. (#25)

# DatawRappr 1.0.0

Is the first public release of DatawRappr.
