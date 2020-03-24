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

#' Underreporting data
#'
#' A dataset containing estimates for the reporting rate for each country based on
#'  their CFR from https://cmmid.github.io/topics/covid19/severity/global_cfr_estimates.html
#'
#' @format A data frame with 6 variables:
#' \itemize{
#'   \item \code{country} country name
#'   \item \code{total_cases} number of reported cases
#'   \item \code{total_deaths} number of reported deaths
#'   \item \code{underreporting_estimate} estimate of reporting rate using mean CFR from Verity et al.
#'   \item \code{lower} estimate of reporting rate using upper bound CFR from Verity et al.
#'   \item \code{upper} estimate of reporting rate using lower bound CFR from Verity et al.
#' }
#' @docType data
#' @references Russell, T.W. et al. Using a delay-adjusted case fatality ratio to estimate under-reporting. CMMID Repository 2020.
#' @references Verity R, Okell LC, Dorigatti I et al. Estimates of the severity of covid-19 disease. medRxiv 2020.
#' @name underreporting
#' @usage data(underreporting)
NULL
