.DatawRappr_ua <- httr::user_agent(
  sprintf(
    "DatawRappr package v%s: (<%s>)",
    utils::packageVersion("DatawRappr"),
    utils::packageDescription("DatawRappr")$URL
  )
) -> .DATAWRAPPR_UA
