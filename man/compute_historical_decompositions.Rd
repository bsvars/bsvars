% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_historical_decompositions.R
\name{compute_historical_decompositions}
\alias{compute_historical_decompositions}
\title{Computes posterior draws of historical decompositions}
\usage{
compute_historical_decompositions(posterior, show_progress = TRUE)
}
\arguments{
\item{posterior}{posterior estimation outcome obtained by running the \code{estimate} 
function. The interpretation depends on the normalisation of the shocks
using function \code{normalise_posterior()}. Verify if the default settings 
are appropriate.}

\item{show_progress}{a logical value, if \code{TRUE} the estimation progress bar is visible}
}
\value{
An object of class \code{PosteriorHD}, that is, an \code{NxNxTxS} array 
with attribute \code{PosteriorHD} containing \code{S} draws of the historical 
decompositions.
}
\description{
Each of the draws from the posterior estimation of models from
packages \pkg{bsvars} or \pkg{bsvarSIGNs} is transformed into
a draw from the posterior distribution of the historical decompositions. 
IMPORTANT! The historical decompositions are interpreted correctly for 
covariance stationary data. Application to unit-root non-stationary data might
result in non-interpretable outcomes.
}
\examples{
# upload data
data(us_fiscal_lsuw)

# specify the model and set seed
set.seed(123)
specification  = specify_bsvar$new(diff(us_fiscal_lsuw), p = 1)

# run the burn-in
burn_in        = estimate(specification, 10)

# estimate the model
posterior      = estimate(burn_in, 20)

# compute historical decompositions
hd            = compute_historical_decompositions(posterior)

# workflow with the pipe |>
############################################################
set.seed(123)
diff(us_fiscal_lsuw) |>
  specify_bsvar$new(p = 1) |>
  estimate(S = 10) |> 
  estimate(S = 20) |> 
  compute_historical_decompositions() -> hd

}
\references{
Kilian, L., & Lütkepohl, H. (2017). Structural VAR Tools, Chapter 4, In: Structural vector autoregressive analysis. Cambridge University Press.
}
\seealso{
\code{\link{estimate}}, \code{\link{normalise_posterior}}, \code{\link{summary}}
}
\author{
Tomasz Woźniak \email{wozniak.tom@pm.me} and Xiaolei Wang \email{adamwang15@gmail.com}
}
