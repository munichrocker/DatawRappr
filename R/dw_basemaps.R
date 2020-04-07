#' Datawrapper Basemaps
#'
#' Look up basemaps with their available keys.
#'
#' @source <https://api.datawrapper.de/plugin/basemaps>
#'
#' @format Data frame with columns
#' \describe{
#' \item{id}{id of the basemap. Required for call to \code{\link{dw_get_map_key}}.}
#' \item{level}{Geographic level of this basemap}
#' \item{value}{Value of the keys. Required for call to \code{\link{dw_get_map_key}}.}
#' \item{label}{Short name of the keys}
#' \item{description}{Description of the keys.}
#' }
#'
#' @examples
#'   dw_basemaps
"dw_basemaps"
