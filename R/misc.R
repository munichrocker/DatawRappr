# This contains all misc functions which are not exported

dw_check_chart_id <- function(chart_id) {

  if (class(chart_id) == "dw_chart") {
    chart_id <- chart_id[["content"]][["data"]][[1]][["id"]]
  } else if (class(chart_id) == "character") {
    if (!grepl("[a-zA-Z0-9]{5}", chart_id)) {
      stop("Entered chart_id is not valid!", call. = FALSE)
    }
  }
  return(chart_id)
}
