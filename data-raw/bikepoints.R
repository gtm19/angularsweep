library(httr)
library(jsonlite)
library(janitor)

endpoint <- "https://api.tfl.gov.uk/bikepoint"

req <- GET(url = endpoint,
           query = list(app_id = Sys.getenv("TFL_APP_ID"),
                        app_key = Sys.getenv("TFL_API_KEY")))

bikepoints <- fromJSON(content(req, as = "text"))

no_bikes <- sapply(bikepoints$additionalProperties, function(tbl) {
  as.integer(tbl$value[tbl$key == "NbDocks"])
})

bikepoints <- bikepoints[c("id", "commonName", "lat", "lon")]

bikepoints$no_bikes <- no_bikes

bikepoints <- clean_names(bikepoints)

usethis::use_data(bikepoints, overwrite = TRUE)
