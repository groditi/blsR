% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_series.R
\name{get_series}
\alias{get_series}
\title{Create and execute query for a single time series}
\usage{
get_series(series_id, start_year = NA, end_year = NA, ...)
}
\arguments{
\item{series_id}{BLS series ID}

\item{start_year}{numeric 4-digit year}

\item{end_year}{numeric 4-digit year}

\item{...}{additional parameters to pass to \code{\link[=bls_request]{bls_request()}}}
}
\value{
a single series result, in list form. The resulting list will have
the following items:
\itemize{
\item \code{seriesID}: a character vector of length 1 containing the \code{series_id}
\item \code{data}: a list of lists containing the payload data. Each item of the list
represents an observation. Each observation is a list with the following
named items \code{year}, \code{period}, \code{periodName}, \code{value}, \code{footnotes}.
Footnotes are a list. Additionally, the most recent observation will have
an item named \code{latest} which will be marked as 'true'.
}
}
\description{
Create and execute query for a single time series
}
\examples{
\dontrun{
series <- get_series('LNS14000001')
}
}
\seealso{
\code{\link{query_series}}

Other blsR-requests: 
\code{\link{bls_request}()},
\code{\link{get_all_surveys}()},
\code{\link{get_latest_observation}()},
\code{\link{get_n_series_table}()},
\code{\link{get_n_series}()},
\code{\link{get_popular_series}()},
\code{\link{get_series_tables}()},
\code{\link{get_series_table}()},
\code{\link{get_survey_info}()}
}
\concept{blsR-requests}
