#' Set Datawrapper-API-Key to Systemenvironment
#'
#' Adds Key to Environment to be available on Startup.
#'
#' @param api_key Required. A character string, containing the API-Key.
#'
#' @return A Message in the command line.
#' @author Benedict Witzenberger
#' @note This is a very simple function that adds the API-key to the .Renviron-file in the user's home folder. If a key already exists and the user wants to, it will get replaced.
#' @examples
#'
#' \dontrun{datawrapper_auth(api_key = "1234ABC")}
#' @rdname datawrapper_auth
#' @export
datawrapper_auth <- function(api_key) {

  # Access environment file:
  filename <- paste0(Sys.getenv("HOME"), "/.Renviron")

  # read_key-function allows input selection - and removes key if wanted:
  read_key <- function()
  {
    n <- readline(prompt = "Overwrite existing key? J/N: ")

    if (n == "J"| n == "j"){

      # delete DW_KEY from .Renviron
      command <- paste0("sed -i '' '/^DW_KEY/d' ", filename)
      system(command = command)

      # write new Key to to environment file
      new_key <- paste0('DW_KEY = ', api_key)
      write(new_key, file = filename, append = TRUE)

      warning("Existing key overwritten!", call. = FALSE)

    } else if (n == "N" | n == "n") {

      warning("Existing key remains!", call. = FALSE)

    } else {
      read_key()
    }
  }

  # check if key already exists - throw warning if yes, else, write new key
  if (Sys.getenv("DW_KEY") != "") {

    warning(paste0("A Datawrapper-key ", Sys.getenv("DW_KEY"), " already exists on this system."), immediate. = TRUE)
    read_key()

  } else {

    # write new Key to to environment file
    new_key <- paste0('DW_KEY = ', api_key)
    write(new_key, file = filename, append = TRUE)
    print("New key was saved!")

  }

}
