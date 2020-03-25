#'
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


#'
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


#'
#' Population data
#'
#' A dataset containing populations for each source
#'
#' @format A data frame with 5 variables:
#' \itemize{
#'   \item \code{country_name} country name
#'   \item \code{country} country code
#'   \item \code{pop} population
#'   \item \code{source} if the source is the country, then the country code; if the source is sub-country level, then it is a code for the source
#'   \item \code{state_name} full name of US states
#' }
#' @docType data
#' @name pop_data
#' @usage data(pop_data)
NULL


#'
#' Airport data
#'
#' A dataset containing information about airports
#'
#' @format A data frame with 55,113 rows and 12 variables:
#' \itemize{
#'   \item \code{ident} airport code
#'   \item \code{type} type of airport
#'   \item \code{name} airport name
#'   \item \code{elevation_ft} airport elevation, in feet
#'   \item \code{continent} 2 letter code for continent
#'   \item \code{iso_country} 2 letter code for country
#'   \item \code{iso_region} 2 letter code for country with 2 number code for region
#'   \item \code{municipality} municipality in which airport is located
#'   \item \code{gps_code}
#'   \item \code{iata_code}
#'   \item \code{local_code}
#'   \item \code{coordinates} GPS coordinates
#' }
#' @docType data
#' @name airport_data
#' @usage data(airport_data)
NULL

#'
#' Airport attribution data for China
#'
#' A dataset containing information about which Chinese airports the people in
#' different Chinese provinces use.
#'
#' @format A data frame with 507 rows and 3 variables:
#' \itemize{
#'   \item \code{Province} the name of a Chinese province
#'   \item \code{airport_iata} airport code
#'   \item \code{attribution} the percentage of people from that province that use that airport, such that the sum of the attributions for each province equals 1
#' }
#' @docType data
#' @name airport_attribution
#' @usage data(airport_attribution)
NULL
