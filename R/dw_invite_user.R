#' Invite a person to a team
#'
#' \lifecycle{maturing}
#'
#' Invites a new team member.
#'
#' @param team Required. The team-id (can be found in the URL of a teams-folder) as character.
#' @param email Required. The email-address that gets an invitation as character.
#' @param role Required. Set to one of: \code{member}, \code{admin}, \code{owner}. Check this for more informations: \href{https://academy.datawrapper.de/article/212-how-to-invite-others-to-a-team}
#' @param api_key Optional. A Datawrapper-API-key as character string. Defaults to "environment" - tries to automatically retrieve the key that's stored in the .Reviron-file by \code{\link{datawrapper_auth}}.
#'
#' @return A message of failure or success, derived from the API's status code.
#' @author Benedict Witzenberger
#' @note This function invites a email-address to a team.
#' @examples
#'
#' \dontrun{dw_invite_user(team = "testTeam", email = "test_at_test.com", role = "admin") # uses the preset key in the .Renviron-file}
#' @rdname dw_invite_user
#' @export
dw_invite_user <- function(team, email, role = c("member", "admin", "owner"), api_key = "environment"){

  if (api_key == "environment") {
    api_key <- dw_get_api_key()
  }

  url <- paste0("https://api.datawrapper.de/v3/teams/", team,"/invites")

  if (!role %in% c("member", "admin", "owner")) stop("Specify a correct role: member, admin or owner.")

  call_body <- list(
    "email" = email,
    "role" = role
  )

  r <- httr::POST(url, httr::add_headers(Authorization = paste("Bearer", api_key, sep = " ")),
                 body = call_body, encode = "form", .DATAWRAPPR_UA)

  parsed <- dw_handle_errors(r)

  if (r$status_code == 201 & is.null(parsed)) message(paste0("The emailadress ", email, " has successfully been added to team ", team, ".\n"))

  return(parsed)

}

