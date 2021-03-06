% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pin_internal.R
\name{pin_birthplace_internal}
\alias{pin_birthplace_internal}
\title{pin_birthplace_internal}
\usage{
pin_birthplace_internal(pin, birth_vector, birth_other_text)
}
\arguments{
\item{pin}{Character element with pin at standard format 'YYYYMMDDNNNC'. See \link{as.pin}.}

\item{birth_vector}{Vector mapping birth number to birthplace. See \link{pin_birthplace}.}

\item{birth_other_text}{Text to return if born >= 1990. See \link{pin_birthplace}.}
}
\value{
Character element containing birthplace
}
\description{
Internal computation of birthplace (one for each pin)
}
\keyword{internal}
