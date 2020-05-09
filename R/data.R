#' Santander Cycle docking station locations in London
#'
#' This dataset was produced using [Transport for London's Unified API](https://api.tfl.gov.uk/swagger/ui/index.html?url=/swagger/docs/v1#!/BikePoint/BikePoint_GetAll),
#' and is contains public sector information licensed under the [Open Government Licence v2.0](http://www.nationalarchives.gov.uk/doc/open-government-licence/version/2/)
#'
#' @format A data frame with 766 rows and 5 variables:
#' \describe{
#'   \item{id}{TfL API's bike point ID}
#'   \item{common_name}{Text description of the bike point location}
#'   \item{lat}{Latitude of the bike point location}
#'   \item{lon}{Longitude of the bike point location}
#'   \item{no_bikes}{The number of docking stations available at the bike point}
#' }
#'
"bikepoints"
