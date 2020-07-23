#' Provides Access to Datawrapper's API
#'
#' Connecting R to Datawrapper, allowing the creation and edits of charts via Datawrapper's API.
#' It makes heavy use of the httr-library and tries to manage the API-token for the user in the .Renviron-file.
#' \if{html}{\figure{logo.png}{options: align="right" style="width:125px; height:145px; padding-top:16px"}}
#'
#'
#' @import httr rlist
#' @importFrom jsonlite fromJSON
#' @importFrom magick image_read
#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @importFrom lifecycle deprecate_soft
## usethis namespace: end
NULL
