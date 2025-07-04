% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chek_reptiledb_status.R
\name{check_data_update}
\alias{check_data_update}
\title{Check if reptile database data needs updating based on date comparison}
\usage{
check_data_update(silent = FALSE, check_connection = TRUE)
}
\arguments{
\item{silent}{Logical. If TRUE, suppresses messages and only returns results.
Default is FALSE.}

\item{check_connection}{Logical. If TRUE, checks internet connection before
attempting to access online data. Default is TRUE.}
}
\value{
A list containing the following elements:
\describe{
\item{update_needed}{Logical. TRUE if an update is needed, FALSE otherwise}
\item{local_info}{List. Information about the local dataset}
\item{remote_info}{List. Information about the remote dataset}
\item{message}{Character. Status message describing the comparison result}
\item{recommendation}{Character. Recommendation for user action}
\item{local_date}{Character. Date of local data in YYYY-MM-DD format}
\item{remote_date}{Character. Date of remote data in YYYY-MM-DD format (if available)}
\item{remote_filename}{Character. Filename of the remote file (if available)}
\item{days_difference}{Numeric. Number of days difference between local and remote data (if both dates available)}
}
If an error occurs or internet connection is not available, only the message
element will contain relevant error information.
}
\description{
This function checks if the local reptile database data is up-to-date by
comparing the date extracted from the local dataset name with the date
from the latest available file on The Reptile Database website.
}
\examples{
\donttest{
# Silent check (no messages) - requires internet connection
update_status <- check_data_update(silent = TRUE)

# Verbose check with connection verification
update_status <- check_data_update(silent = FALSE, check_connection = TRUE)

# Check without internet connection verification
update_status <- check_data_update(check_connection = FALSE)
}

}
