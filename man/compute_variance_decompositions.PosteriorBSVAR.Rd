% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_variance_decompositions.R
\name{compute_variance_decompositions.PosteriorBSVAR}
\alias{compute_variance_decompositions.PosteriorBSVAR}
\title{Computes posterior draws of the forecast error variance decomposition}
\usage{
\method{compute_variance_decompositions}{PosteriorBSVAR}(posterior, horizon)
}
\arguments{
\item{posterior}{posterior estimation outcome - an object of class 
\code{PosteriorBSVAR} obtained by running the \code{estimate} function.}

\item{horizon}{a positive integer number denoting the forecast horizon for 
the forecast error variance decomposition computations.}
}
\value{
An object of class PosteriorFEVD, that is, an \code{NxNx(horizon+1)xS} array with attribute PosteriorFEVD 
containing \code{S} draws of the forecast error variance decomposition.
}
\description{
Each of the draws from the posterior estimation of the model
is transformed into a draw from the posterior distribution of the forecast 
error variance decomposition.
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

# compute forecast error variance decomposition 2 years ahead
fevd           = compute_variance_decompositions(posterior, horizon = 8)

# workflow with the pipe |>
############################################################
set.seed(123)
us_fiscal_lsuw |>
  specify_bsvar$new(p = 1) |>
  estimate(S = 10) |> 
  estimate(S = 20) |> 
  compute_variance_decompositions(horizon = 8) -> fevd

}
\references{
Kilian, L., & Lütkepohl, H. (2017). Structural VAR Tools, Chapter 4, In: Structural vector autoregressive analysis. Cambridge University Press.
}
\seealso{
\code{\link{compute_impulse_responses}}, \code{\link{estimate}}, \code{\link{normalise_posterior}}, \code{\link{summary}}
}
\author{
Tomasz Woźniak \email{wozniak.tom@pm.me}
}
