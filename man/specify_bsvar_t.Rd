% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/specify_bsvar_t.R
\name{specify_bsvar_t}
\alias{specify_bsvar_t}
\title{R6 Class representing the specification of the BSVAR model with t-distributed structural shocks.}
\description{
The class BSVART presents complete specification for the BSVAR model with t-distributed structural shocks.
}
\examples{
data(us_fiscal_lsuw)
spec = specify_bsvar_t$new(
   data = us_fiscal_lsuw,
   p = 4
)

}
\seealso{
\code{\link{estimate}}, \code{\link{specify_posterior_bsvar_t}}
}
\section{Super class}{
\code{bsvars::BSVAR} -> \code{BSVART}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{p}}{a non-negative integer specifying the autoregressive lag order of the model.}

\item{\code{identification}}{an object IdentificationBSVARs with the identifying restrictions.}

\item{\code{prior}}{an object PriorBSVART with the prior specification.}

\item{\code{data_matrices}}{an object DataMatricesBSVAR with the data matrices.}

\item{\code{starting_values}}{an object StartingValuesBSVART with the starting values.}

\item{\code{adaptiveMH}}{a vector of two values setting the Robust Adaptive 
Metropolis sampler for df: target acceptance rate and adaptive rate.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-BSVART-new}{\code{specify_bsvar_t$new()}}
\item \href{#method-BSVART-clone}{\code{specify_bsvar_t$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="bsvars" data-topic="BSVAR" data-id="get_data_matrices"><a href='../../bsvars/html/BSVAR.html#method-BSVAR-get_data_matrices'><code>bsvars::BSVAR$get_data_matrices()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bsvars" data-topic="BSVAR" data-id="get_identification"><a href='../../bsvars/html/BSVAR.html#method-BSVAR-get_identification'><code>bsvars::BSVAR$get_identification()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bsvars" data-topic="BSVAR" data-id="get_prior"><a href='../../bsvars/html/BSVAR.html#method-BSVAR-get_prior'><code>bsvars::BSVAR$get_prior()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bsvars" data-topic="BSVAR" data-id="get_starting_values"><a href='../../bsvars/html/BSVAR.html#method-BSVAR-get_starting_values'><code>bsvars::BSVAR$get_starting_values()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BSVART-new"></a>}}
\if{latex}{\out{\hypertarget{method-BSVART-new}{}}}
\subsection{Method \code{new()}}{
Create a new specification of the BSVAR model with t-distributed structural shocks, BSVART.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{specify_bsvar_t$new(
  data,
  p = 1L,
  B,
  A,
  exogenous = NULL,
  stationary = rep(FALSE, ncol(data))
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data}}{a \code{(T+p)xN} matrix with time series data.}

\item{\code{p}}{a positive integer providing model's autoregressive lag order.}

\item{\code{B}}{a logical \code{NxN} matrix containing value \code{TRUE} for the 
elements of the structural matrix \eqn{B} to be estimated and value 
\code{FALSE} for exclusion restrictions to be set to zero.}

\item{\code{A}}{a logical \code{NxK} matrix containing value \code{TRUE} for the elements of 
the autoregressive matrix \eqn{A} to be estimated and value \code{FALSE} for exclusion restrictions 
to be set to zero.}

\item{\code{exogenous}}{a \code{(T+p)xd} matrix of exogenous variables.}

\item{\code{stationary}}{an \code{N} logical vector - its element set to 
\code{FALSE} sets the prior mean for the autoregressive parameters of the 
\code{N}th equation to the white noise process, otherwise to random walk.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new complete specification for the bsvar model with t-distributed 
structural shocks, BSVART.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BSVART-clone"></a>}}
\if{latex}{\out{\hypertarget{method-BSVART-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{specify_bsvar_t$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
