library(sf)
library(dplyr)
library(lwgeom)

sf::sf_use_s2(FALSE)

wcmc_url <- paste0(
  "https://data-gis.unep-wcmc.org/server/rest/services/",
  "WCMC036_MEOW_PPOW_2007_2012/MapServer/0/query",
  "?where=1%3D1&outFields=*&f=geojson"
)

meow_ppow <- sf::st_read(wcmc_url)

realm_only <- meow_ppow %>%
  dplyr::filter(type == "MEOW") %>%
  dplyr::select(realm) %>%
  sf::st_make_valid()

saveRDS(realm_only, "data/realm_only.rds")