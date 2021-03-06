% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subunih.R
\name{subunih}
\alias{subunih}
\title{Establish the pressure at the start of a lateral line in a subunit in function flow rater required of emissors}
\usage{
subunih(DLL, K, x, DLD, SeLL, SeLD, nLL, ne, dec, qreq, imax = 100,
  toler = 1e-10, HfimLDa = 100, HfimLDb = 0)
}
\arguments{
\item{DLL}{Diameter lateral line in meters}

\item{K}{Emission coefficient}

\item{x}{Emission exponent}

\item{DLD}{Diameter manifold in meters}

\item{SeLL}{Emitter spacing in meters}

\item{SeLD}{Lateral line spacing in meters}

\item{nLL}{Number of lateral lines in meters}

\item{ne}{Number of emitters in meters}

\item{dec}{Slope on the lateral line}

\item{qreq}{Flow rate required at the emitters in cubic meters per second}

\item{imax}{Maximum number of iteration}

\item{toler}{Maximum tolerance}

\item{HfimLDa}{Maximum pressure head at the end of the manifold in meters}

\item{HfimLDb}{Minimum pressure head at the end of the manifold in meters}
}
\value{
HfimLD pressure head at the end of the manifold, qsistema system flow rate, iter number iterations, tempo time
}
\description{
Establish the pressure at the start of a lateral line in a subunit
}
\examples{
subunih(DLL = 0.025, K = 1.053e-6, x = 0.5, DLD = 0.04,
SeLL = 1, SeLD = 4, nLL = 10, ne = 100, dec = 0.0,
qreq = 3.333333e-6, imax = 100, toler = 1e-10, HfimLDa = 100, HfimLDb = 0)
subunih(DLL=0.025, K = 1.053e-6, x = 0.5, DLD = 0.04,
SeLL = 1, SeLD = 4, nLL = 10, ne = 100, dec = 0.01,
qreq = 3.333333e-6, imax = 100, toler = 1e-10, HfimLDa = 100, HfimLDb = 0)

}
