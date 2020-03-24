#' Travel restrictions data
#'
#' A dataset containing locations and dates of travel restrictions, as well as an
#' educated guess for the reduction in the travel.
#'
#' @format A data frame with 721 rows and 4 variables:
#' \itemize{
#'   \item \code{loc} adm1 level location name
#'   \item \code{min} the starting date of the travel restriction
#'   \item \code{max} the last date of the travel restriction
#'   \item \code{p_travel} the proportion of normal level travel due to the travel restriction (e.g. 0.1 means that there is 10% of the normal travel due to the restriction)
#' }
#' @docType data
#' @name travel_restrictions
#' @usage data(travel-restrictions)
NULL
