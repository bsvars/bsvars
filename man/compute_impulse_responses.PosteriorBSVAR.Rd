% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_impulse_responses.R
\name{compute_impulse_responses.PosteriorBSVAR}
\alias{compute_impulse_responses.PosteriorBSVAR}
\title{Computes posterior draws of impulse responses}
\usage{
\method{compute_impulse_responses}{PosteriorBSVAR}(posterior, horizon, standardise = FALSE)
}
\arguments{
\item{posterior}{posterior estimation outcome - an object of class 
\code{PosteriorBSVAR} obtained by running the \code{estimate} function.}

\item{horizon}{a positive integer number denoting the forecast horizon for the impulse responses computations.}

\item{standardise}{a logical value. If \code{TRUE}, the impulse responses are standardised 
so that the variables' own shocks at horizon 0 are equal to 1. Otherwise, the parameter estimates 
determine this magnitude.}
}
\value{
An object of class PosteriorIR, that is, an \code{NxNx(horizon+1)xS} array with attribute PosteriorIR 
containing \code{S} draws of the impulse responses.
}
\description{
Each of the draws from the posterior estimation of models from 
packages \pkg{bsvars} or \pkg{bsvarSIGNs} is transformed into
a draw from the posterior distribution of the impulse responses.
}
\examples{
# upload data
data(us_fiscal_lsuw)

# specify the model and set seed
set.seed(123)
specification  = specify_bsvar$new(us_fiscal_lsuw, p = 1)

# run the burn-in
burn_in        = estimate(specification, 10)

# estimate the model
posterior      = estimate(burn_in, 20)

# compute impulse responses 2 years ahead
irf           = compute_impulse_responses(posterior, horizon = 8)

# workflow with the pipe |>
############################################################
set.seed(123)
us_fiscal_lsuw |>
  specify_bsvar$new(p = 1) |>
  estimate(S = 10) |> 
  estimate(S = 20) |> 
  compute_impulse_responses(horizon = 8) -> ir

}
\references{
Kilian, L., & Lütkepohl, H. (2017). Structural VAR Tools, Chapter 4, In: Structural vector autoregressive analysis. Cambridge University Press.
}
\seealso{
\code{\link{estimate}}, \code{\link{normalise_posterior}}, \code{\link{summary}}
}
\author{
Tomasz Woźniak \email{wozniak.tom@pm.me}
}
