# This script contains a function to update the basemap-df, if needed
needs::needs(usethis, httr, dplyr, tidyr)

r <- GET("https://api.datawrapper.de/plugin/basemaps")
list_dw_maps <- content(r)$data

dw_basemaps <- tibble(maps = list_dw_maps)

dw_basemaps %<>%
  unnest_wider(maps) %>%
  rename(level = label) %>%
  unnest(keys) %>%
  unnest_wider(keys) %>%
  select(-keys)

usethis::use_data(dw_basemaps, overwrite = TRUE)
