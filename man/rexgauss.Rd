% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rexgauss.R
\name{rexgauss}
\alias{rexgauss}
\title{Generate random deviate from ex-gaussian distribution}
\usage{
rexgauss(n, mu = 0, sigma = 1, tau = 1, positive = TRUE)
}
\arguments{
\item{n}{integer, number of deviates to generate}

\item{mu}{integer, mean of normal}

\item{sigma}{integer, sd of normal}

\item{tau}{integer, tail parameter}

\item{positive}{logical, force values to be positive}
}
\value{
a vector of random deviates from an ex-gaussian distribution
}
\description{
Generate random deviate from ex-gaussian distribution
}
\details{
This function was originally part of the retimes package, authored by Davide Massidda, which was removed from CRAN on 2021-01-27.
}
\examples{
rexgauss(n=10,mu=0,sigma=1,tau=1)

}
