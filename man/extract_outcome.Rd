% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract-outcome.R
\name{extract_outcome}
\alias{extract_outcome}
\alias{extract_outcome.default}
\alias{extract_outcome.formula}
\alias{extract_outcome.model_spec}
\alias{extract_outcome.workflow}
\alias{extract_outcome.model_fit}
\alias{extract_outcome.Arima}
\alias{extract_outcome.Arima_fit_impl}
\alias{extract_outcome.mdl_ts}
\title{Extracts outcome column name}
\usage{
extract_outcome(object, ...)

\method{extract_outcome}{default}(object, ...)

\method{extract_outcome}{formula}(object, ...)

\method{extract_outcome}{model_spec}(object, ...)

\method{extract_outcome}{workflow}(object, ...)

\method{extract_outcome}{model_fit}(object, ...)

\method{extract_outcome}{Arima}(object, ...)

\method{extract_outcome}{Arima_fit_impl}(object, ...)

\method{extract_outcome}{mdl_ts}(object, ...)
}
\arguments{
\item{object}{an object to extract model from.}

\item{...}{additional arguements such as \code{formula}.}
}
\description{
Generic function to extract and return outcome variable name from various
classes of objects. Currently implemented for \code{formula}, \code{model_spec},
\code{workflow}, \code{model_fit}, \code{Arima}, \code{Arima_fit_impl}, \code{mdl_defn} and \code{mdl_ts}
objects.
}
