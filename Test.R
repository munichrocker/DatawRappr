# This is the Test file which contains all the functions

##
# Set the key
# registers API-key to R SysEnv, checks if key already exists, if yes, asks if it should be deleted
datawrapper_auth <- function(api_key) {

  # Access environment file:
  filename <- paste0(Sys.getenv("HOME"), "/.Renviron")

  # read_key-function allows input selection - and removes key if wanted:
  read_key <- function()
  {
    n <- readline(prompt = "Überschreiben? J/N: ")

    if (n == "J"| n == "j"){

      # delete DW_KEY from .Renviron
      command <- paste0("sed -i '' '/^DW_KEY/d' ", filename)
      system(command = command)

      # write new Key to to environment file
      new_key <- paste0('DW_KEY = ', api_key)
      write(new_key, file = filename, append = TRUE)

      warning("Vorhandener Key überschrieben!", call. = FALSE)

    } else if (n == "N" | n == "n") {

      warning("Vorhandener Key bleibt bestehen!", call. = FALSE)

    } else {
      read_key()
    }
  }

  # check if key already exists - throw warning if yes, else, write new key
  if (Sys.getenv("DW_KEY") != "") {

    warning(paste0("Ein Datawrapper-Key ", Sys.getenv("DW_KEY"), " existiert bereits."), immediate. = TRUE)
    read_key()

  } else {

    # write new Key to to environment file
    new_key <- paste0('DW_KEY = ', api_key)
    write(new_key, file = filename, append = TRUE)
    print("Neuer Key gespeichert!")

  }

}

##
# retrieve Datawrapper-API-Key
dw_get_api_key <- function() {

  if (Sys.getenv("DW_KEY") != "") {

    api_key <- Sys.getenv("DW_KEY")
    return(api_key)

  } else {

    warning("Kein Datawrapper-API-Key gefunden. Zuerst einen neuen Key mit datawrapper_auth() hinzufügen.", immediate. = TRUE)

  }

}

##
# Tests Connection to API, should return User-Information if Key is correct
dw_test_key <- function(api_key = "environment") {

  if (api_key == "environment") {
    api_key <- get_dw_api_key()
  }

  r <- httr::GET("https://api.datawrapper.de/account", httr::add_headers(Authorization = paste("Bearer", api_key, sep = " ")))

  try(if(httr::status_code(r) != 200) stop("Fehler bei der Verbindung. Statuscode ist nicht 200."))

  return(httr::content(r))
}

## TEST ##
test_key(api_key = "c6207ffcb97469dee1d38e7448f62518e8d098a028348c83788d6bdd633a2777")
####

###
# Calling the API

# dw_retrieve_chart(chart_id)
# retrieves the metadata of a specified chart
dw_retrieve_chart_metadata <- function(chart_id, api_key = "environment") {

  url <- paste0("https://api.datawrapper.de/charts/", chart_id)

  if (api_key == "environment") {
    api_key <- dw_get_api_key()
  }

  r <- httr::GET(url, httr::add_headers(Authorization = paste("Bearer", api_key, sep = " ")))

  try(if(httr::status_code(r) != 200) stop("Fehler bei der Verbindung. Statuscode ist nicht 200."))

  return(httr::content(r))
}

# create_chart(api_key = "environment)
# creates chart, returns chart_id

dw_create_chart <- function(api_key = "environment") {

  if (api_key == "environment") {
    api_key <- dw_get_api_key()
  }

  r <- httr::POST("https://api.datawrapper.de/charts", httr::add_headers(Authorization = paste("Bearer", api_key, sep = " ")))

  try(if(httr::status_code(r) != 200) stop("Fehler bei der Verbindung. Statuscode ist nicht 200."))

  chart_content <- httr::content(r)

  print(paste0("New chart's id: ", chart_content$data[[1]]$id))

  return(chart_content)

}

test_df <- read.csv2("/Users/witzenbergb/Downloads/45412-0001.csv", encoding = "UTF-8")


# put data to a chart xyz
dw_data_to_chart <- function(x, chart_id, api_key = "environment", display_response = TRUE) {

  if (api_key == "environment") {
    api_key <- dw_get_api_key()
  }

  try(if (class(x) != "data.frame") stop("data is not of class data.frame!"))

  df_content <- paste(t(sapply(seq(1, nrow(x), by = 1), function(i)
    paste(unlist(x[i,]), collapse = ","))), collapse = "\n")

  try(if (TRUE %in% grepl(",", names(x))) stop("The Dataframe's header contains a comma - which is used as the seperator. Remove the comma (e.g. with names()) and try again."))

  df_names <- paste(names(x), collapse = ",")

  data_body <- paste(df_names, df_content, sep = "\n")

  url <- paste0("https://api.datawrapper.de/charts/", chart_id, "/data")

  r <- httr::PUT(url, httr::add_headers(Authorization = paste("Bearer", api_key, sep = " ")),
           body = data_body)

  try(if(httr::status_code(r) != 200) stop("Fehler bei der Verbindung. Statuscode ist nicht 200."))

  if(httr::status_code(r) == 200) {
    print("Chart updated.")
  }

  if (display_response == TRUE) {
    chart_content <- httr::content(r)
    return(chart_content)
  }

}

dw_data_to_chart(test_df, "97EWn")

# request to put new metadata
dw_edit_chart <- function(chart_id, api_key = "environment", title = "", intro = "", annotate = "",
                          type = "", source_name = "", source_url = "", ...) {

  if (api_key == "environment") {
    api_key <- dw_get_api_key()
  }

  # download existing data from chart
  call_body <- list(metadata = list())

  # change only specified parts of existing data
  if (title != "") {call_body <- rlist::list.append(call_body, title = title)}
  if (type != "") {call_body <- rlist::list.append(call_body, type = type)}

  if (intro != "") {call_body$metadata$describe$intro <- intro}
  if (annotate != "") {call_body$metadata$annotate$notes <- annotate}

  if (source_name != "") {call_body$metadata$describe$`source-name` <- source_name}
  if (source_url != "") {call_body$metadata$describe$`source-url` <- source_url}

  #### HERE!
  # work in additional arguments
  # How to append additional arguments without creating many describe = list()s, visualize = list()s etc.?
  additional_arguments <- ...
  if (!is.null(additional_arguments)) {
    call_body$metadata <- rlist::list.append(call_body$metadata, visualize = list(yaxis = TRUE, transpose = TRUE), describe = list(`source-name` = "Tolle Quelle"))
  }
  ######
  print(chart_data) # helper

  # upload modified data
  url_upload <- paste0("https://api.datawrapper.de/charts/", chart_id)

  r_upload <- httr::PUT(url_upload, httr::add_headers(Authorization = paste("Bearer", api_key, sep = " ")),
           body = call_body, encode = "json")

  print(httr::content(r_upload)) # helper

  try(if(httr::status_code(r_upload) != 200) stop("Fehler bei der Verbindung. Statuscode ist nicht 200."))

  if(httr::status_code(r_upload) == 200) {
    print("Chart updated.")
  }

}

dw_edit_chart("97EWn", title = "Testtitel", intro = "Das ist die Einleitung zu den Daten", annotate = "Ich bin eine Anmerkung", source_name = "Quelle 3")

call_body <- list(
  title = "Testtitel",
  type = "d3-lines",
  metadata = list(
    describe = list(`source-url` = "www.sz.de"),
    describe = list(`source-name` = "Quelle 3")
  )
)

call_body <- list(
  title = "",
  type = "d3-lines",
  metadata = list()
)

# to publish the chart, run this POST request:

dw_publish_chart <- function(chart_id, api_key = "environment") {

  if (api_key == "environment") {
    api_key <- dw_get_api_key()
  }

  url <- paste0("https://api.datawrapper.de/charts/", chart_id, "/publish")

  r <- httr::POST(url, httr::add_headers(Authorization = paste("Bearer", api_key, sep = " ")))

  try(if(httr::status_code(r) != 200) stop("Fehler bei der Verbindung. Statuscode ist nicht 200."))

  if (httr::status_code(r) == 200) {
    print(paste0("Chart ", chart_id, " published!"))
  } else {
      warning("There has been an error.", immediate. = TRUE)
    }

}

dw_publish_chart("yPa1l")

# delete a given chart

dw_delete_chart <- function(chart_id, api_key = "environment") {

  if (api_key == "environment") {
    api_key <- dw_get_api_key()
  }

  url <- paste0("https://api.datawrapper.de/charts/", chart_id)

  r <- httr::DELETE(url, httr::add_headers(Authorization = paste("Bearer", api_key, sep = " ")))

  try(if(httr::status_code(r) != 200) stop("Fehler bei der Verbindung. Statuscode ist nicht 200."))

  response_content <- httr::content(r)

  if (response_content$data == "" & response_content$status == "ok") {
    print(paste0("Chart ", chart_id, " sucessfully deleted!"))
  }

}
dw_delete_chart("yPa1l")
