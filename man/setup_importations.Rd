% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_model_source.R
\name{setup_importations}
\alias{setup_importations}
\title{Set up importation sims}
\usage{
setup_importations(
  dest = "UT",
  dest_type = c("state"),
  dest_country = "USA",
  dest_aggr_level = c("airport"),
  first_date = ISOdate(2019, 12, 1),
  last_date = Sys.time(),
  output_dir = file.path("output", paste0(paste(dest, collapse = "+"), "_",
    as.Date(Sys.Date()))),
  save_case_data = TRUE,
  get_travel = TRUE,
  n_top_dests = Inf,
  travel_dispersion = 3,
  param_list = list(p_report_source = c(0.05, 0.25), shift_incid_days = -10, delta = 1),
  check_errors = TRUE
)
}
\arguments{
\item{dest}{character string, name of destination to simulate importations for}

\item{dest_type}{character string, options: "airport", "city", "state", "country"}

\item{dest_country}{optional character string, specify a higher level destination (i.e. dest_0="USA"), default NULL}

\item{dest_aggr_level}{character string, level to which travel will be aggregated for destination. Includes "airport", "city", "state", "country", "metro" (only available for CA currently)}

\item{first_date}{Date, first import date}

\item{last_date}{Date, last import date}

\item{output_dir}{where output files are saved}

\item{save_case_data}{Whether to save the JHUCSSE raw data}

\item{get_travel}{whether to load or pull travel data}

\item{n_top_dests}{Number of destinations to include, ranked by volume; default (Inf) is all.}

\item{travel_dispersion}{numeric}

\item{param_list}{list, with the following elements
\itemize{
  \item \code{p_report_source} numeric vector of length 2, currently the probability of reporting by source with the first indicating Hubei reporting and second indicating everywhere else (UPDATE WITH THESE https://cmmid.github.io/topics/covid19/severity/global_cfr_estimates.html)
  \item \code{shift_incid_days} lag from infection to report
  \item \code{delta} days per time period
}}

\item{check_errors}{logical}
}
\value{

}
\description{
Set up importation sims
}
