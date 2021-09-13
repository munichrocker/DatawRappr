#' Creates a new folder
#'
#' \lifecycle{maturing}
#' Creates a new folder
#'
#' @param name Required. Specify the name for the newly created folder.
#' @param organization_id Optional. ID of organization if creating a folder for a organization that you're working with. Defaults to user.
#' @param parent_id Optional. ID of parent folder if creating a subfolder.
#' @param api_key Optional. A Datawrapper-API-key as character string. Defaults to "environment" - tries to automatically retrieve the key that's stored in the .Reviron-file by \code{\link{datawrapper_auth}}.
#'
#' @return Returns a success message
#' @author Benedict Witzenberger
#' @note This function builds a body for a API-call to the Datawrapper-API, and creates a new folder for a user or a team.
#' @examples
#'
#' \dontrun{
#'
#' dw_create_folder(name = "New Folder")
#'
#' } # uses the preset API-key in the .Renviron-file
#'

#' @rdname dw_create_folder
#' @export
dw_create_folder <- function(name = "", organization_id = NULL, parent_id = NULL, api_key = "environment"){

  if (api_key == "environment") {
    api_key <- dw_get_api_key()
  }

  if (name == "") { stop("Newly created folder requires a name.") }

  # create empty body for API-call
  call_body <- list("name" = name)

  if (!is.null(organization_id)) {call_body <- c(call_body, "organizationId" = organization_id)}
  if (!is.null(parent_id)) {call_body <- c(call_body, "parentId" = parent_id)}

  parsed <- dw_call_api("POST", "https://api.datawrapper.de/v3/folders", httr::add_headers(Authorization = paste("Bearer", api_key, sep = " ")),
                 body = call_body, encode = "json", .DATAWRAPPR_UA)

  message(paste0("New Folder '", parsed$name, "' successfully created!", "\n", "Folder-ID is: ", parsed$id, "\n"))

  return(parsed)

}
