% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/specify_bsvar.R
\name{specify_prior_bsvar}
\alias{specify_prior_bsvar}
\title{R6 Class Representing PriorBSVAR}
\description{
The class PriorBSVAR presents a prior specification for the homoskedastic bsvar model.
}
\examples{
prior = specify_prior_bsvar$new(N = 3, p = 1)  # a prior for 3-variable example with one lag
prior$A                                        # show autoregressive prior mean


## ------------------------------------------------
## Method `specify_prior_bsvar$new`
## ------------------------------------------------

# a prior for 3-variable example with one lag and stationary data
prior = specify_prior_bsvar$new(N = 3, p = 1, stationary = rep(TRUE, 3))
prior$A # show autoregressive prior mean


## ------------------------------------------------
## Method `specify_prior_bsvar$get_prior`
## ------------------------------------------------

# a prior for 3-variable example with four lags
prior = specify_prior_bsvar$new(N = 3, p = 4)
prior$get_prior() # show the prior as list

}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{A}}{an \code{NxK} matrix, the mean of the normal prior distribution for the parameter matrix \eqn{A}.}

\item{\code{A_V_inv}}{a \code{KxK} precision matrix of the normal prior distribution for each of 
the row of the parameter matrix \eqn{A}. This precision matrix is equation invariant.}

\item{\code{B_V_inv}}{an \code{NxN} precision matrix of the generalised-normal prior distribution 
for the structural matrix \eqn{B}. This precision matrix is equation invariant.}

\item{\code{B_nu}}{a positive integer greater of equal than \code{N}, a shape parameter of 
the generalised-normal prior distribution for the structural matrix \eqn{B}.}

\item{\code{hyper_nu_B}}{a positive scalar, the shape parameter of the inverted-gamma 2 prior
for the overall shrinkage parameter for matrix \eqn{B}.}

\item{\code{hyper_a_B}}{a positive scalar, the shape parameter of the gamma prior
for the second-level hierarchy for the overall shrinkage parameter for matrix \eqn{B}.}

\item{\code{hyper_s_BB}}{a positive scalar, the scale parameter of the inverted-gamma 2 prior
for the third-level of hierarchy for overall shrinkage parameter for matrix \eqn{B}.}

\item{\code{hyper_nu_BB}}{a positive scalar, the shape parameter of the inverted-gamma 2 prior
for the third-level of hierarchy for overall shrinkage parameter for matrix \eqn{B}.}

\item{\code{hyper_nu_A}}{a positive scalar, the shape parameter of the inverted-gamma 2 prior 
for the overall shrinkage parameter for matrix \eqn{A}.}

\item{\code{hyper_a_A}}{a positive scalar, the shape parameter of the gamma prior
for the second-level hierarchy for the overall shrinkage parameter for matrix \eqn{A}.}

\item{\code{hyper_s_AA}}{a positive scalar, the scale parameter of the inverted-gamma 2 prior
for the third-level of hierarchy for overall shrinkage parameter for matrix \eqn{A}.}

\item{\code{hyper_nu_AA}}{a positive scalar, the shape parameter of the inverted-gamma 2 prior
for the third-level of hierarchy for overall shrinkage parameter for matrix \eqn{A}.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-PriorBSVAR-new}{\code{specify_prior_bsvar$new()}}
\item \href{#method-PriorBSVAR-get_prior}{\code{specify_prior_bsvar$get_prior()}}
\item \href{#method-PriorBSVAR-clone}{\code{specify_prior_bsvar$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PriorBSVAR-new"></a>}}
\if{latex}{\out{\hypertarget{method-PriorBSVAR-new}{}}}
\subsection{Method \code{new()}}{
Create a new prior specification PriorBSVAR.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{specify_prior_bsvar$new(N, p, d = 0, stationary = rep(FALSE, N))}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{N}}{a positive integer - the number of dependent variables in the model.}

\item{\code{p}}{a positive integer - the autoregressive lag order of the SVAR model.}

\item{\code{d}}{a positive integer - the number of \code{exogenous} variables in the model.}

\item{\code{stationary}}{an \code{N} logical vector - its element set to \code{FALSE} sets 
the prior mean for the autoregressive parameters of the \code{N}th equation to the white noise process, 
otherwise to random walk.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new prior specification PriorBSVAR.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{# a prior for 3-variable example with one lag and stationary data
prior = specify_prior_bsvar$new(N = 3, p = 1, stationary = rep(TRUE, 3))
prior$A # show autoregressive prior mean

}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PriorBSVAR-get_prior"></a>}}
\if{latex}{\out{\hypertarget{method-PriorBSVAR-get_prior}{}}}
\subsection{Method \code{get_prior()}}{
Returns the elements of the prior specification PriorBSVAR as a \code{list}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{specify_prior_bsvar$get_prior()}\if{html}{\out{</div>}}
}

\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{# a prior for 3-variable example with four lags
prior = specify_prior_bsvar$new(N = 3, p = 4)
prior$get_prior() # show the prior as list

}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PriorBSVAR-clone"></a>}}
\if{latex}{\out{\hypertarget{method-PriorBSVAR-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{specify_prior_bsvar$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
