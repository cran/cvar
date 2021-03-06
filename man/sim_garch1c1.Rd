% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/garch.R
\name{sim_garch1c1}
\alias{sim_garch1c1}
\title{Simulate GARCH(1,1) time series}
\usage{
sim_garch1c1(model, n, n.start = 0, seed = NULL)
}
\arguments{
\item{model}{a GARCH(1,1) model, an object obtained from \code{GarchModel}.}

\item{n}{the length of the generated time series.}

\item{n.start}{number of warm-up values, which are then dropped.}

\item{seed}{an integer to use for setting the random number generator.}
}
\value{
a list with components:
\item{eps}{the time series,}
\item{h}{the (squared) volatilities,}
\item{eta}{the standardised innovations,}
\item{model}{the GARCH(1,1) model,}
\item{.sim}{a list containing the parameters of the simulation,}
\item{call}{the call.}
}
\description{
Simulate GARCH(1,1) time series.
}
\details{
The simulated time series is in component \code{eps} of the returned value.
For exploration of algorithms and eestimation procedures, the volatilities
and the standardised innovations are also returned.

The random seed at the start of the simulations is saved in the returned
object.  A speficific seed can be requested with argument \code{seed}. In
that case the simulations are done with the specified seed and the old state
of the random number generator is restored before the function returns.
}
\note{
This function is under development and may be changed.
}
