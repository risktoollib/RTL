% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simOUJ.R
\name{simOUJ}
\alias{simOUJ}
\title{\code{simOUJ}}
\usage{
simOUJ(
  S0 = 5,
  mu = 5,
  theta = 10,
  sigma = 0.2,
  jump_prob = 0.05,
  jump_avesize = 2,
  jump_stdv = 0.05,
  T2M = 1,
  dt = 1/250
)
}
\arguments{
\item{S0}{S at t=0}

\item{mu}{Mean reversion level}

\item{theta}{Mean reversion speed}

\item{sigma}{Standard deviation}

\item{jump_prob}{Probability of jumps}

\item{jump_avesize}{Average size of jumps}

\item{jump_stdv}{Standard deviation of jump average size}

\item{T2M}{Maturity in years}

\item{dt}{Time step size e.g. 1/250 = 1 business day.}
}
\value{
A numeric vector of simulated values
}
\description{
Simulates a Ornstein–Uhlenbeck process with Jumps
}
\examples{
simOUJ(S0=5,mu=5,theta=.5,sigma=0.2,jump_prob=0.05,jump_avesize = 3,jump_stdv = 0.05,T2M=1,dt=1/12)
}
\author{
Philippe Cote
}
