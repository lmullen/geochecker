#' Sample geocoding data for cities
#'
#' This data frame contains sample geocoded data for the twenty most populous
#' cities in the world. The latitudes and longitudes of these cities have, in a
#' few cases, been deliberately modified for the purposes of demonstrating how
#' to correct their geocoding. The column \code{correct} is \code{TRUE} if the
#' coordinates are accurate and \code{FALSE} if they have been modified.
#'
#' @docType data
#' @format A data frame with 20 observations of 6 variables
#' @source
#' \href{https://en.wikipedia.org/wiki/List_of_cities_proper_by_population}{Wikipedia}
"geocoded_cities"