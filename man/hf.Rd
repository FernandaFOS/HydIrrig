% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hf.R
\name{hf}
\alias{hf}
\title{Head loss in meters overs the length of pipe}
\usage{
hf(D, Q, L, E = 1e-04, v = 1.01e-06, x1 = 4, g = 9.81)
}
\arguments{
\item{D}{Diameter in meters}

\item{Q}{Flow rate in cubic meters per second}

\item{L}{Length of pipe in meters}

\item{E}{Roughness coefficient in meters}

\item{v}{Kinematic viscosity of fluiyd in square meters per second.}

\item{x1}{Initial parameter of f for Newthon-Raphson}

\item{g}{Gravitational acceleration \code{g=9.81} meters per square second.}
}
\value{
regime flow laminar or turbulent, hf Head loss in meters, iterações iterations end tempo time
}
\description{
calculate the head loss by fricition in pipes
}
\examples{
hf(D = 0.025, Q = 0.001,  L = 100)
hf(D = 0.080, Q = 0.0001, L = 500)
}
