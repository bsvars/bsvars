
#' R6 Class Representing PriorBSVARMIX
#'
#' @description
#' The class PriorBSVARMIX presents a prior specification for the bsvar model with a zero-mean mixture of normals model for structural shocks.
#' 
#' @examples 
#' prior = specify_prior_bsvar_mix$new(N = 3, p = 1, M = 2)  # specify the prior
#' prior$A                                        # show autoregressive prior mean
#' 
#' @export
specify_prior_bsvar_mix = R6::R6Class(
  "PriorBSVARMIX",
  
  inherit = specify_prior_bsvar_msh,
  
  public = list(
    
    #' @field A an \code{NxK} matrix, the mean of the normal prior distribution for the parameter matrix \eqn{A}. 
    A          = matrix(),
    
    #' @field A_V_inv a \code{KxK} precision matrix of the normal prior distribution for each of the row of the parameter matrix \eqn{A}. This precision matrix is equation invariant.
    A_V_inv    = matrix(),
    
    #' @field B_V_inv an \code{NxN} precision matrix of the generalised-normal prior distribution for the structural matrix \eqn{B}. This precision matrix is equation invariant.
    B_V_inv    = matrix(),
    
    #' @field B_nu a positive integer greater of equal than \code{N}, a shape parameter of the generalised-normal prior distribution for the structural matrix \eqn{B}.
    B_nu       = NA,
    
    #' @field hyper_nu_B a positive scalar, the shape parameter of the inverted-gamma 2 prior
    #' for the overall shrinkage parameter for matrix \eqn{B}.
    hyper_nu_B = NA,
    
    #' @field hyper_a_B a positive scalar, the shape parameter of the gamma prior
    #' for the second-level hierarchy for the overall shrinkage parameter for matrix \eqn{B}.
    hyper_a_B  = NA,
    
    #' @field hyper_s_BB a positive scalar, the scale parameter of the inverted-gamma 2 prior
    #' for the third-level of hierarchy for overall shrinkage parameter for matrix \eqn{B}.
    hyper_s_BB  = NA,
    
    #' @field hyper_nu_BB a positive scalar, the shape parameter of the inverted-gamma 2 prior
    #' for the third-level of hierarchy for overall shrinkage parameter for matrix \eqn{B}.
    hyper_nu_BB  = NA,
    
    #' @field hyper_nu_A a positive scalar, the shape parameter of the inverted-gamma 2 prior 
    #' for the overall shrinkage parameter for matrix \eqn{A}.
    hyper_nu_A  = NA,
    
    #' @field hyper_a_A a positive scalar, the shape parameter of the gamma prior
    #' for the second-level hierarchy for the overall shrinkage parameter for matrix \eqn{A}.
    hyper_a_A  = NA,
    
    #' @field hyper_s_AA a positive scalar, the scale parameter of the inverted-gamma 2 prior
    #' for the third-level of hierarchy for overall shrinkage parameter for matrix \eqn{A}.
    hyper_s_AA  = NA,
    
    #' @field hyper_nu_AA a positive scalar, the shape parameter of the inverted-gamma 2 prior
    #' for the third-level of hierarchy for overall shrinkage parameter for matrix \eqn{A}.
    hyper_nu_AA  = NA,
    
    #' @field sigma_nu a positive scalar,  the shape parameter of the inverted-gamma 2 for mixture component-dependent variances of the structural shocks, \eqn{\sigma^2_{n.s_t}}.
    sigma_nu   = 3,
    
    #' @field sigma_s a positive scalar,  the scale parameter of the inverted-gamma 2 for mixture component-dependent variances of the structural shocks, \eqn{\sigma^2_{n.s_t}}.
    sigma_s    = 1,
    
    #' @field PR_TR an \code{MxM} matrix, the matrix of hyper-parameters of the row-specific Dirichlet prior distribution for the state probabilities the Markov process \eqn{s_t}. Its rows must be identical.
    PR_TR      = matrix()
    
  ) # END public
) # END specify_prior_bsvar_mix




#' R6 Class Representing StartingValuesBSVARMIX
#'
#' @description
#' The class StartingValuesBSVARMIX presents starting values for the bsvar model with a zero-mean mixture of normals model for structural shocks.
#' 
#' @examples 
#' # starting values for a bsvar model for a 3-variable system
#' A = matrix(TRUE, 3, 4)
#' B = matrix(TRUE, 3, 3)
#' sv = specify_starting_values_bsvar_mix$new(A = A, B = B, N = 3, p = 1, M = 2, T = 100)
#' 
#' @export
specify_starting_values_bsvar_mix = R6::R6Class(
  "StartingValuesBSVARMIX",
  
  inherit = specify_starting_values_bsvar_msh,
  
  public = list(
    
    #' @field A an \code{NxK} matrix of starting values for the parameter \eqn{A}. 
    A             = matrix(),
    
    #' @field B an \code{NxN} matrix of starting values for the parameter \eqn{B}. 
    B             = matrix(),
    
    #' @field hyper a \code{(2*N+1)x2} matrix of starting values for the shrinkage hyper-parameters of the 
    #' hierarchical prior distribution. 
    hyper         = matrix(),
    
    #' @field sigma2 an \code{NxM} matrix of starting values for the MS state-specific variances of the structural shocks. Its elements sum to value \code{M} over the rows.
    sigma2        = matrix(),
    
    #' @field PR_TR an \code{MxM} matrix of starting values for the probability matrix of the Markov process. Its rows must be identical and the elements of each row sum to 1 over the rows.
    PR_TR         = matrix(),
    
    #' @field xi an \code{MxT} matrix of starting values for the Markov process indicator. Its columns are a chosen column of an identity matrix of order \code{M}.
    xi            = matrix(),
    
    #' @field pi_0 an \code{M}-vector of starting values for mixture components state probabilities. Its elements sum to 1.
    pi_0          = numeric(),
    
    
    #' @description
    #' Create new starting values StartingValuesBSVARMIX.
    #' @param A a logical \code{NxK} matrix containing value \code{TRUE} for the elements of 
    #' the autoregressive matrix \eqn{A} to be estimated and value \code{FALSE} for exclusion restrictions 
    #' to be set to zero.
    #' @param B a logical \code{NxN} matrix containing value \code{TRUE} for the elements of 
    #' the staructural matrix \eqn{B} to be estimated and value \code{FALSE} for exclusion restrictions 
    #' to be set to zero.
    #' @param N a positive integer - the number of dependent variables in the model.
    #' @param p a positive integer - the autoregressive lag order of the SVAR model.
    #' @param M an integer greater than 1 - the number of components of the mixture of normals.
    #' @param T a positive integer - the the time series dimension of the dependent variable matrix \eqn{Y}.
    #' @param d a positive integer - the number of \code{exogenous} variables in the model.
    #' @param finiteM a logical value - if true a finite mixture model is estimated. Otherwise, a sparse mixture model is estimated in which \code{M=20} and the number of visited states is estimated.
    #' @return Starting values StartingValuesBSVARMIX.
    initialize = function(A, B, N, p, M, T, d = 0, finiteM = TRUE){
      stopifnot("Argument N must be a positive integer number." = N > 0 & N %% 1 == 0)
      stopifnot("Argument p must be a positive integer number." = p > 0 & p %% 1 == 0)
      stopifnot("Argument M must be an integer number greater than 1." = M > 1 & M %% 1 == 0)
      stopifnot("Argument T must be a positive integer number." = T > 0 & T %% 1 == 0)
      stopifnot("Argument d must be a non-negative integer number." = d >= 0 & d %% 1 == 0)
      stopifnot("Argument finiteM must be a logical value." = is.logical(finiteM) & length(finiteM) == 1)
      
      if (!finiteM) {
        M = 20
      }
      
      super$initialize(A, B, N, p, M, T, d)
    } # END initialize
    
  ) # END public
) # END specify_starting_values_bsvar_mix




#' R6 Class representing the specification of the BSVAR model with a zero-mean mixture of normals model for structural shocks.
#'
#' @description
#' The class BSVARMIX presents complete specification for the BSVAR model with a zero-mean mixture of normals model for structural shocks.
#' 
#' @seealso \code{\link{estimate}}, \code{\link{specify_posterior_bsvar_mix}}
#' 
#' @examples 
#' data(us_fiscal_lsuw)
#' spec = specify_bsvar_mix$new(
#'    data = us_fiscal_lsuw,
#'    p = 4,
#'    M = 2
#' )
#' 
#' @export
specify_bsvar_mix = R6::R6Class(
  "BSVARMIX",
  
  inherit = specify_bsvar_msh,
  
  public = list(
    
    #' @field p a non-negative integer specifying the autoregressive lag order of the model. 
    p                      = numeric(),
    
    #' @field identification an object IdentificationBSVARs with the identifying restrictions. 
    identification         = list(),
    
    #' @field prior an object PriorBSVARMIX with the prior specification. 
    prior                  = list(),
    
    #' @field data_matrices an object DataMatricesBSVAR with the data matrices.
    data_matrices          = list(),
    
    #' @field starting_values an object StartingValuesBSVARMIX with the starting values.
    starting_values        = list(),
    
    #' @field finiteM a logical value - if true a finite mixture model is estimated. Otherwise, a sparse mixture model is estimated in which \code{M=20} and the number of visited states is estimated.
    finiteM                = logical(),
    
    #' @description
    #' Create a new specification of the BSVAR model with a zero-mean mixture of normals model for structural shocks, BSVARMIX.
    #' @param data a \code{(T+p)xN} matrix with time series data.
    #' @param p a positive integer providing model's autoregressive lag order.
    #' @param M an integer greater than 1 - the number of components of the mixture of normals.
    #' @param B a logical \code{NxN} matrix containing value \code{TRUE} for the elements of the structural matrix \eqn{B} to be estimated and value \code{FALSE} for exclusion restrictions to be set to zero.
    #' @param A a logical \code{NxK} matrix containing value \code{TRUE} for the elements of 
    #' the autoregressive matrix \eqn{A} to be estimated and value \code{FALSE} for exclusion restrictions 
    #' to be set to zero.
    #' @param exogenous a \code{(T+p)xd} matrix of exogenous variables. 
    #' @param stationary an \code{N} logical vector - its element set to \code{FALSE} sets the prior mean for the autoregressive parameters of the \code{N}th equation to the white noise process, otherwise to random walk.
    #' @param finiteM a logical value - if true a finite mixture model is estimated. Otherwise, a sparse mixture model is estimated in which \code{M=20} and the number of visited states is estimated.
    #' @return A new complete specification for the bsvar model with a zero-mean mixture of normals model for structural shocks, BSVARMIX.
    initialize = function(
    data,
    p = 1L,
    M = 2L,
    B,
    A,
    exogenous = NULL,
    stationary = rep(FALSE, ncol(data)),
    finiteM = TRUE
    ) {
      stopifnot("Argument p has to be a positive integer." = ((p %% 1) == 0 & p > 0))
      self$p        = p
      
      TT            = nrow(data)
      T             = TT - self$p
      N             = ncol(data)
      d             = 0
      if (!is.null(exogenous)) {
        d           = ncol(exogenous)
      }
      K             = N * p + 1 + d
      
      if (!finiteM) {
        if ( M < 20 ) {
          M = 20L
          message("In the sparse mixture model the value of M is overwritten and set to 20.")
        }
      }
      self$finiteM  = finiteM
      
      if (missing(B)) {
        message("The identification is set to the default option of lower-triangular structural matrix.")
        B     = matrix(FALSE, N, N)
        B[lower.tri(B, diag = TRUE)] = TRUE
      }
      stopifnot("Incorrectly specified argument B." = (is.matrix(B) & is.logical(B)) | (length(B) == 1 & is.na(B)))
      if (missing(A)) {
        A     = matrix(TRUE, N, K)
      }
      stopifnot("Incorrectly specified argument A." = (is.matrix(A) & is.logical(A)))
      
      self$data_matrices   = specify_data_matrices$new(data, p, exogenous)
      self$identification  = specify_identification_bsvars$new(B, A, N, K)
      self$prior           = specify_prior_bsvar_mix$new(N, p, d, M, stationary)
      self$starting_values = specify_starting_values_bsvar_mix$new(A, B, N, self$p, M, T, d, finiteM)
    } # END initialize
  ) # END public
) # END specify_bsvar_mix




#' R6 Class Representing PosteriorBSVARMIX
#'
#' @description
#' The class PosteriorBSVARMIX contains posterior output and the specification including 
#' the last MCMC draw for the bsvar model with a zero-mean mixture of normals model for structural shocks.
#' Note that due to the thinning of the MCMC output the starting value in element \code{last_draw}
#' might not be equal to the last draw provided in element \code{posterior}.
#' 
#' @seealso \code{\link{estimate}}, \code{\link{specify_bsvar_mix}}
#' 
#' @examples 
#' # This is a function that is used within estimate()
#' data(us_fiscal_lsuw)
#' specification  = specify_bsvar_mix$new(us_fiscal_lsuw, p = 4, M = 2)
#' set.seed(123)
#' estimate       = estimate(specification, 10, thin = 1)
#' class(estimate)
#' 
#' @export
specify_posterior_bsvar_mix = R6::R6Class(
  "PosteriorBSVARMIX",
  
  private = list(
    normalised = FALSE
  ), # END private
  
  public = list(
    
    #' @field last_draw an object of class BSVARMIX with the last draw of the current MCMC run as the starting value to be passed to the continuation of the MCMC estimation using \code{estimate()}. 
    last_draw = list(),
    
    #' @field posterior a list containing Bayesian estimation output.
    posterior = list(),
    
    #' @description
    #' Create a new posterior output PosteriorBSVARMIX.
    #' @param specification_bsvar an object of class BSVARMIX with the last draw of the current MCMC run as the starting value.
    #' @param posterior_bsvar a list containing Bayesian estimation output.
    #' @return A posterior output PosteriorBSVARMIX.
    initialize = function(specification_bsvar, posterior_bsvar) {
      
      stopifnot("Argument specification_bsvar must be of class BSVARMIX." = any(class(specification_bsvar) == "BSVARMIX"))
      stopifnot("Argument posterior_bsvar must must contain MCMC output." = is.list(posterior_bsvar) & is.array(posterior_bsvar$B) & is.array(posterior_bsvar$A) & is.array(posterior_bsvar$hyper) & is.matrix(posterior_bsvar$pi_0))
      
      self$last_draw    = specification_bsvar
      self$posterior    = posterior_bsvar
    }, # END initialize
    
    #' @description
    #' Returns a list containing Bayesian estimation output.
    #' 
    #' @examples 
    #' data(us_fiscal_lsuw)
    #' specification  = specify_bsvar_mix$new(us_fiscal_lsuw, M = 2)
    #' set.seed(123)
    #' estimate       = estimate(specification, 10, thin = 1)
    #' estimate$get_posterior()
    #' 
    get_posterior       = function(){
      self$posterior
    }, # END get_posterior
    
    #' @description
    #' Returns an object of class BSVARMIX with the last draw of the current MCMC run as the starting value to be passed to the continuation of the MCMC estimation using \code{estimate()}.
    #' 
    #' @examples
    #' data(us_fiscal_lsuw)
    #' 
    #' # specify the model and set seed
    #' specification  = specify_bsvar_mix$new(us_fiscal_lsuw, p = 4, M = 2)
    #' 
    #' # run the burn-in
    #' set.seed(123)
    #' burn_in        = estimate(specification, 10, thin = 2)
    #' 
    #' # estimate the model
    #' posterior      = estimate(burn_in, 10, thin = 2)
    #' 
    get_last_draw      = function(){
      self$last_draw$clone()
    }, # END get_last_draw
    
    #' @description
    #' Returns \code{TRUE} if the posterior has been normalised using \code{normalise_posterior()} and \code{FALSE} otherwise.
    #' 
    #' @examples
    #' # upload data
    #' data(us_fiscal_lsuw)
    #' 
    #' # specify the model and set seed
    #' specification  = specify_bsvar_mix$new(us_fiscal_lsuw, p = 4, M = 2)
    #' 
    #' # estimate the model
    #' set.seed(123)
    #' posterior      = estimate(specification, 10, thin = 1)
    #' 
    #' # check normalisation status beforehand
    #' posterior$is_normalised()
    #' 
    #' # normalise the posterior
    #' BB            = posterior$last_draw$starting_values$B      # get the last draw of B
    #' B_hat         = diag((-1) * sign(diag(BB))) %*% BB         # set negative diagonal elements
    #' normalise_posterior(posterior, B_hat)                      # draws in posterior are normalised
    #' 
    #' # check normalisation status afterwards
    #' posterior$is_normalised()
    #' 
    is_normalised      = function(){
      private$normalised
    }, # END is_normalised
    
    #' @description
    #' Sets the private indicator \code{normalised} to TRUE.
    #' @param value (optional) a logical value to be passed to indicator \code{normalised}.
    #' 
    #' @examples
    #' # This is an internal function that is run while executing normalise_posterior()
    #' # Observe its working by analysing the workflow:
    #' 
    #' # upload data
    #' data(us_fiscal_lsuw)
    #' 
    #' # specify the model and set seed
    #' specification  = specify_bsvar_mix$new(us_fiscal_lsuw, p = 4, M = 2)
    #' set.seed(123)
    #' 
    #' # estimate the model
    #' posterior      = estimate(specification, 10, thin = 1)
    #' 
    #' # check normalisation status beforehand
    #' posterior$is_normalised()
    #' 
    #' # normalise the posterior
    #' BB            = posterior$last_draw$starting_values$B      # get the last draw of B
    #' B_hat         = diag(sign(diag(BB))) %*% BB                # set positive diagonal elements
    #' normalise_posterior(posterior, B_hat)                      # draws in posterior are normalised
    #' 
    #' # check normalisation status afterwards
    #' posterior$is_normalised()
    #' 
    set_normalised     = function(value){
      if (missing(value)) {
        private$normalised <- TRUE
      } else {
        private$normalised <- value
      }
    } # END set_normalised
    
  ) # END public
) # END specify_posterior_bsvar_mix
