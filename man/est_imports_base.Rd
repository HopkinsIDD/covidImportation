% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_model_source.R
\name{est_imports_base}
\alias{est_imports_base}
\title{Estimate imports base}
\usage{
est_imports_base(
  input_data,
  tr_inf_redux = 0,
  meanD,
  u_origin,
  allow_travel_variance = FALSE
)
}
\arguments{
\item{input_data}{Full, combined input data that includes incident cases, travel volume, and population
for all sources/destination pairs}

\item{tr_inf_redux}{Default=1; Travel reduction given infected}

\item{meanD}{Vector of length nrow(input_data); mean duration of time during which an infected individual
can travel, in days.}

\item{u_origin}{Vector of length nrow(input_data); underreporting rate in the infection source location.}

\item{allow_travel_variance}{Logical, whether to sample the travel}
}
\value{

}
\description{
This is the base model for estimating importations. We will use this within the larger framework
to get importations per simulation
}
