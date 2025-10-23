
#' R6 Class Representing StartingValuesBSVAHMSH
#'
#' @description
#' The class StartingValuesBSVARHMSH presents starting values for the bsvar 
#' model with Heterogeneous Markov Switching Heteroskedasticity.
#' 
#' @examples 
#' # starting values for a bsvar model for a 3-variable system
#' A = matrix(TRUE, 3, 4)
#' B = matrix(TRUE, 3, 3)
#' sv = specify_starting_values_bsvar_hmsh$new(A = A, B = B, N = 3, p = 1, M = 2, T = 100)
#' 
#' @export
specify_starting_values_bsvar_hmsh = R6::R6Class(
  "StartingValuesBSVARHMSH",
  
  inherit = specify_starting_values_bsvar,
  
  public = list(
    
    #' @field A an \code{NxK} matrix of starting values for the parameter \eqn{A}. 
    A             = matrix(),
    
    #' @field B an \code{NxN} matrix of starting values for the parameter \eqn{B}. 
    B             = matrix(),
    
    #' @field hyper a \code{(2*N+1)x2} matrix of starting values for the shrinkage 
    #' hyper-parameters of the hierarchical prior distribution. 
    hyper         = matrix(),
    
    #' @field sigma2 an \code{NxM} matrix of starting values for the MS 
    #' state-specific variances of the structural shocks. Its elements sum to 
    #' value \code{M} over the rows.
    sigma2        = matrix(),
    
    #' @field PR_TR an \code{MxMxN} array of starting values for the transition 
    #' probability matrix of the Markov process. Its elements sum to 1 over the rows.
    PR_TR         = array(),
    
    #' @field xi an \code{MxTxN} array of starting values for the Markov process 
    #' indicator. Its columns are a chosen column of an identity matrix of order \code{M}.
    xi            = array(),
    
    #' @field pi_0 an \code{MxN} matrix of starting values for state probability at 
    #' time \code{t=0}. Its elements sum to 1 in columns.
    pi_0          = matrix(),
    
    
    #' @description
    #' Create new starting values StartingValuesBSVARHMSH.
    #' @param A a logical \code{NxK} matrix containing value \code{TRUE} for the elements of 
    #' the autoregressive matrix \eqn{A} to be estimated and value \code{FALSE} for exclusion restrictions 
    #' to be set to zero.
    #' @param B a logical \code{NxN} matrix containing value \code{TRUE} for the elements of 
    #' the staructural matrix \eqn{B} to be estimated and value \code{FALSE} for exclusion restrictions 
    #' to be set to zero.
    #' @param N a positive integer - the number of dependent variables in the model.
    #' @param p a positive integer - the autoregressive lag order of the SVAR model.
    #' @param M an integer greater than 1 - the number of Markov process' heteroskedastic regimes.
    #' @param T a positive integer - the the time series dimension of the dependent variable matrix \eqn{Y}.
    #' @param d a positive integer - the number of \code{exogenous} variables in the model.
    #' @param finiteM a logical value - if true a stationary Markov switching model 
    #' is estimated. Otherwise, a sparse Markov switching model is estimated in which 
    #' \code{M=20} and the number of visited states is estimated.
    #' @return Starting values StartingValuesBSVARHMSH.
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
      
      super$initialize(A, B, N, T, p, d)
      
      self$sigma2         = matrix(1, N, M)
      self$pi_0           = matrix(1/M, M, N)
      self$PR_TR          = array(NA, c(M, M, N))
      self$xi             = array(NA, c(M, T, N))
      for (n in 1:N) {
        self$PR_TR[,,n]   = diag(M)
        self$xi[,,n]      = diag(M)[,sample(1:M, T, replace = TRUE)]
      }
    }, # END initialize
    
    #' @description
    #' Returns the elements of the starting values StartingValuesBSVARHMSH as a \code{list}.
    #' 
    #' @examples 
    #' # starting values for a homoskedastic bsvar with 1 lag for a 3-variable system
    #' A = matrix(TRUE, 3, 4)
    #' B = matrix(TRUE, 3, 3)
    #' sv = specify_starting_values_bsvar_hmsh$new(A = A, B = B, N = 3, p = 1, M = 2, T = 100)
    #' sv$get_starting_values()   # show starting values as list
    #' 
    get_starting_values   = function(){
      list(
        B                 = self$B,
        A                 = self$A,
        hyper             = self$hyper,
        sigma2            = self$sigma2,
        PR_TR             = self$PR_TR,
        xi                = self$xi,
        pi_0              = self$pi_0
      )
    }, # END get_starting_values
    
    #' @description
    #' Returns the elements of the starting values StartingValuesBSVARHMSH as a \code{list}.
    #' @param last_draw a list containing the last draw.
    #' @return An object of class StartingValuesBSVARHMSH including the last draw 
    #' of the current MCMC as the starting value to be passed to the continuation 
    #' of the MCMC estimation using \code{estimate()}.
    #' 
    #' @examples 
    #' # starting values for a bsvar model with 1 lag for a 3-variable system
    #' A = matrix(TRUE, 3, 4)
    #' B = matrix(TRUE, 3, 3)
    #' sv = specify_starting_values_bsvar_hmsh$new(A = A, B = B, N = 3, p = 1, M = 2, T = 100)
    #' 
    #' # Modify the starting values by:
    #' sv_list = sv$get_starting_values()   # getting them as list
    #' sv_list$A <- matrix(rnorm(12), 3, 4) # modifying the entry
    #' sv$set_starting_values(sv_list)      # providing to the class object
    #' 
    set_starting_values   = function(last_draw) {
      self$B            = last_draw$B
      self$A            = last_draw$A
      self$hyper        = last_draw$hyper
      self$sigma2       = last_draw$sigma2
      self$PR_TR        = last_draw$PR_TR
      self$xi           = last_draw$xi
      self$pi_0         = last_draw$pi_0
    } # END set_starting_values
  ) # END public
) # END specify_starting_values_bsvar_msh




#' R6 Class representing the specification of the BSVARHMSH model with 
#' Heterogeneous Markov Switching Heteroskedasticity.
#'
#' @description
#' The class BSVARHMSH presents complete specification for the BSVAR model with 
#' Heterogeneous Markov Switching Heteroskedasticity.
#' 
#' @seealso \code{\link{estimate}}, \code{\link{specify_posterior_bsvar_hmsh}}
#' 
#' @examples 
#' spec = specify_bsvar_hmsh$new(
#'    data = us_fiscal_lsuw,
#'    p = 4,
#'    M = 2
#' )
#' 
#' @export
specify_bsvar_hmsh = R6::R6Class(
  "BSVARHMSH",
  
  public = list(
    
    #' @field p a non-negative integer specifying the autoregressive lag order of the model. 
    p                      = numeric(),
    
    #' @field identification an object IdentificationBSVARs with the identifying restrictions. 
    identification         = list(),
    
    #' @field prior an object PriorBSVARMSH with the prior specification. 
    prior                  = list(),
    
    #' @field data_matrices an object DataMatricesBSVAR with the data matrices.
    data_matrices          = list(),
    
    #' @field starting_values an object StartingValuesBSVARHMSH with the starting values.
    starting_values        = list(),
    
    #' @field finiteM a logical value - if true a stationary Markov switching model 
    #' is estimated. Otherwise, a sparse Markov switching model is estimated in 
    #' which \code{M=20} and the number of visited states is estimated.
    finiteM                = logical(),
    
    #' @description
    #' Create a new specification of the BSVAR model with Heterogeneous Markov 
    #' Switching Heteroskedasticity, BSVARHMSH.
    #' @param data a \code{(T+p)xN} matrix with time series data.
    #' @param p a positive integer providing model's autoregressive lag order.
    #' @param M an integer greater than 1 - the number of Markov process' heteroskedastic regimes.
    #' @param B a logical \code{NxN} matrix containing value \code{TRUE} for the 
    #' elements of the structural matrix \eqn{B} to be estimated and value \code{FALSE} 
    #' for exclusion restrictions to be set to zero.
    #' @param A a logical \code{NxK} matrix containing value \code{TRUE} for the elements of 
    #' the autoregressive matrix \eqn{A} to be estimated and value \code{FALSE} for exclusion restrictions 
    #' to be set to zero.
    #' @param exogenous a \code{(T+p)xd} matrix of exogenous variables. 
    #' @param stationary an \code{N} logical vector - its element set to \code{FALSE} 
    #' sets the prior mean for the autoregressive parameters of the \code{N}th equation 
    #' to the white noise process, otherwise to random walk.
    #' @param finiteM a logical value - if true a stationary Markov switching model 
    #' is estimated. Otherwise, a sparse Markov switching model is estimated in which 
    #' \code{M=20} and the number of visited states is estimated.
    #' @return A new complete specification for the bsvar model with Heterogeneous 
    #' Markov Switching Heteroskedasticity, BSVARHMSH.
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
          message("In the sparse Markov switching model the value of M is overwritten and set to 20.")
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
      self$prior           = specify_prior_bsvar_msh$new(N, p, d, M, stationary)
      self$starting_values = specify_starting_values_bsvar_hmsh$new(A, B, N, self$p, M, T, d, finiteM)
    }, # END initialize
    
    #' @description
    #' Returns the data matrices as the DataMatricesBSVAR object.
    #' 
    #' @examples 
    #' spec = specify_bsvar_hmsh$new(
    #'    data = us_fiscal_lsuw,
    #'    p = 4,
    #'    M = 2
    #' )
    #' spec$get_data_matrices()
    #' 
    get_data_matrices = function() {
      self$data_matrices$clone()
    }, # END get_data_matrices
    
    #' @description
    #' Returns the identifying restrictions as the IdentificationBSVARs object.
    #' 
    #' @examples 
    #' spec = specify_bsvar_hmsh$new(
    #'    data = us_fiscal_lsuw,
    #'    p = 4,
    #'    M = 2
    #' )
    #' spec$get_identification()
    #' 
    get_identification = function() {
      self$identification$clone()
    }, # END get_identification
    
    #' @description
    #' Returns the prior specification as the PriorBSVARMSH object.
    #' 
    #' @examples 
    #' spec = specify_bsvar_hmsh$new(
    #'    data = us_fiscal_lsuw,
    #'    p = 4,
    #'    M = 2
    #' )
    #' spec$get_prior()
    #' 
    get_prior = function() {
      self$prior$clone()
    }, # END get_prior
    
    #' @description
    #' Returns the starting values as the StartingValuesBSVARHMSH object.
    #' 
    #' @examples 
    #' spec = specify_bsvar_hmsh$new(
    #'    data = us_fiscal_lsuw,
    #'    p = 4,
    #'    M = 2
    #' )
    #' spec$get_starting_values()
    #' 
    get_starting_values = function() {
      self$starting_values$clone()
    } # END get_starting_values
  ) # END public
) # END specify_bsvar_hmsh




#' R6 Class Representing PosteriorBSVARHMSH
#'
#' @description
#' The class PosteriorBSVARHMSH contains posterior output and the specification including 
#' the last MCMC draw for the bsvar model with Hetrogeneous Markov Switching Heteroskedasticity. 
#' Note that due to the thinning of the MCMC output the starting value in element \code{last_draw}
#' might not be equal to the last draw provided in element \code{posterior}.
#' 
#' @seealso \code{\link{estimate}}, \code{\link{specify_bsvar_hmsh}}
#' 
#' @examples 
#' # This is a function that is used within estimate()
#' specification  = specify_bsvar_hmsh$new(us_fiscal_lsuw, p = 4, M = 2)
#' set.seed(123)
#' estimate       = estimate(specification, 10, thin = 1)
#' class(estimate)
#' 
#' @export
specify_posterior_bsvar_hmsh = R6::R6Class(
  "PosteriorBSVARHMSH",
  
  private = list(
    normalised = FALSE
  ), # END private
  
  public = list(
    
    #' @field last_draw an object of class BSVARHMSH with the last draw of the 
    #' current MCMC run as the starting value to be passed to the continuation 
    #' of the MCMC estimation using \code{estimate()}. 
    last_draw = list(),
    
    #' @field posterior a list containing Bayesian estimation output.
    posterior = list(),
    
    #' @description
    #' Create a new posterior output PosteriorBSVARHMSH.
    #' @param specification_bsvar an object of class BSVARHMSH with the last draw 
    #' of the current MCMC run as the starting value.
    #' @param posterior_bsvar a list containing Bayesian estimation output.
    #' @return A posterior output PosteriorBSVARHMSH.
    initialize = function(specification_bsvar, posterior_bsvar) {
      
      stopifnot("Argument specification_bsvar must be of class BSVARHMSH." = any(class(specification_bsvar) == "BSVARHMSH"))
      stopifnot("Argument posterior_bsvar must must contain MCMC output." = is.list(posterior_bsvar) & is.array(posterior_bsvar$B) & is.array(posterior_bsvar$PR_TR) & is.array(posterior_bsvar$hyper) & is.array(posterior_bsvar$xi))
      
      self$last_draw    = specification_bsvar
      self$posterior    = posterior_bsvar
    }, # END initialize
    
    #' @description
    #' Returns a list containing Bayesian estimation output.
    #' 
    #' @examples 
    #' specification  = specify_bsvar_hmsh$new(us_fiscal_lsuw, M = 2)
    #' set.seed(123)
    #' estimate       = estimate(specification, 10)
    #' estimate$get_posterior()
    #' 
    get_posterior       = function(){
      self$posterior
    }, # END get_posterior
    
    #' @description
    #' Returns an object of class BSVARHMSH with the last draw of the current MCMC 
    #' run as the starting value to be passed to the continuation of the MCMC 
    #' estimation using \code{estimate()}.
    #' 
    #' @examples
    #' # specify the model and set seed
    #' specification  = specify_bsvar_hmsh$new(us_fiscal_lsuw, p = 4, M = 2)
    #' 
    #' # run the burn-in
    #' set.seed(123)
    #' burn_in        = estimate(specification, 5)
    #' 
    #' # estimate the model
    #' posterior      = estimate(burn_in, 5)
    #' 
    get_last_draw      = function(){
      self$last_draw$clone()
    }, # END get_last_draw
    
    #' @description
    #' Returns \code{TRUE} if the posterior has been normalised using 
    #' \code{normalise_posterior()} and \code{FALSE} otherwise.
    #' 
    #' @examples
    #' # specify the model and set seed
    #' specification  = specify_bsvar_hmsh$new(us_fiscal_lsuw, p = 4, M = 2)
    #' 
    #' # estimate the model
    #' posterior      = estimate(specification, 5)
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
    #' # specify the model and set seed
    #' specification  = specify_bsvar_hmsh$new(us_fiscal_lsuw, p = 4, M = 2)
    #' set.seed(123)
    #' 
    #' # estimate the model
    #' posterior      = estimate(specification, 5)
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
) # END specify_posterior_bsvar_hmsh
