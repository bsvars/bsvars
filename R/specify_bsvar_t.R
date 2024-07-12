
#' R6 Class Representing PriorBSVART
#'
#' @description
#' The class PriorBSVART presents a prior specification for the bsvar model with
#' t-distributed structural shocks.
#' 
#' @examples 
#' prior = specify_prior_bsvar_t$new(N = 3, p = 1)  # specify the prior
#' prior$A                                        # show autoregressive prior mean
#' 
#' @export
specify_prior_bsvar_t = R6::R6Class(
  "PriorBSVART",
  
  inherit = specify_prior_bsvar,
  
  public = list(
    
    #' @field A an \code{NxK} matrix, the mean of the normal prior distribution 
    #' for the parameter matrix \eqn{A}. 
    A          = matrix(),
    
    #' @field A_V_inv a \code{KxK} precision matrix of the normal prior 
    #' distribution for each of the row of the parameter matrix \eqn{A}. This 
    #' precision matrix is equation invariant.
    A_V_inv    = matrix(),
    
    #' @field B_V_inv an \code{NxN} precision matrix of the generalised-normal 
    #' prior distribution for the structural matrix \eqn{B}. This precision 
    #' matrix is equation invariant.
    B_V_inv    = matrix(),
    
    #' @field B_nu a positive integer greater of equal than \code{N}, a shape 
    #' parameter of the generalised-normal prior distribution for the structural
    #'  matrix \eqn{B}.
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
    hyper_nu_AA  = NA
    
  ) # END public
) # END specify_prior_bsvar_t




#' R6 Class Representing StartingValuesBSVART
#'
#' @description
#' The class StartingValuesBSVART presents starting values for the bsvar model 
#' with t-distributed structural shocks.
#' 
#' @examples 
#' # starting values for a bsvar model for a 3-variable system
#' sv = specify_starting_values_bsvar_t$new(N = 3, p = 1, T = 100)
#' 
#' @export
specify_starting_values_bsvar_t = R6::R6Class(
  "StartingValuesBSVART",
  
  inherit = specify_starting_values_bsvar,
  
  public = list(
    
    #' @field A an \code{NxK} matrix of starting values for the parameter \eqn{A}. 
    A             = matrix(),
    
    #' @field B an \code{NxN} matrix of starting values for the parameter \eqn{B}. 
    B             = matrix(),
    
    #' @field hyper a \code{(2*N+1)x2} matrix of starting values for the shrinkage hyper-parameters of the 
    #' hierarchical prior distribution. 
    hyper         = matrix(),
    
    #' @field lambda a \code{Tx1} vector of starting values for latent variables.
    lambda        = numeric(),
    
    #' @field df a positive scalar with starting values for the degrees of 
    #' freedom parameter of the Student-t conditional distribution of structural 
    #' shock.
    df            = NA,
    
    #' @description
    #' Create new starting values StartingValuesBSVART
    #' @param N a positive integer - the number of dependent variables in the model.
    #' @param p a positive integer - the autoregressive lag order of the SVAR model.
    #' @param T a positive integer - the the time series dimension of the dependent variable matrix \eqn{Y}.
    #' @param d a positive integer - the number of \code{exogenous} variables in the model.
    #' @return Starting values StartingValuesBSVART
    initialize = function(N, p, T, d = 0){
      stopifnot("Argument N must be a positive integer number." = N > 0 & N %% 1 == 0)
      stopifnot("Argument p must be a positive integer number." = p > 0 & p %% 1 == 0)
      stopifnot("Argument T must be a positive integer number." = T > 0 & T %% 1 == 0)
      stopifnot("Argument d must be a non-negative integer number." = d >= 0 & d %% 1 == 0)
      
      super$initialize(N, p, d)
      self$lambda = rep(1, T)
      self$df     = 6
    }, # END initialize
    
    #' @description
    #' Returns the elements of the starting values StartingValuesBSVAR as a \code{list}.
    #' 
    #' @examples 
    #' # starting values for a homoskedastic bsvar with 1 lag for a 3-variable system
    #' sv = specify_starting_values_bsvar$new(N = 3, p = 1)
    #' sv$get_starting_values()   # show starting values as list
    #' 
    get_starting_values   = function(){
      list(
        B                 = self$B,
        A                 = self$A,
        hyper             = self$hyper,
        lambda            = self$lambda,
        df                = self$df
      )
    }, # END get_starting_values
    
    #' @description
    #' Returns the elements of the starting values StartingValuesBSVAR as a \code{list}.
    #' @param last_draw a list containing the last draw of elements \code{B} - an \code{NxN} matrix, 
    #' \code{A} - an \code{NxK} matrix, and \code{hyper} - a vector of 5 positive real numbers.
    #' @return An object of class StartingValuesBSVAR including the last draw of the current MCMC 
    #' as the starting value to be passed to the continuation of the MCMC estimation using \code{estimate()}.
    #' 
    #' @examples 
    #' # starting values for a homoskedastic bsvar with 1 lag for a 3-variable system
    #' sv = specify_starting_values_bsvar$new(N = 3, p = 1)
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
      self$lambda       = last_draw$lambda
      self$df           = last_draw$df
    } # END set_starting_values
  ) # END public
) # END specify_starting_values_bsvar_t




#' R6 Class representing the specification of the BSVAR model with t-distributed structural shocks.
#'
#' @description
#' The class BSVART presents complete specification for the BSVAR model with t-distributed structural shocks.
#' 
#' @seealso \code{\link{estimate}}, \code{\link{specify_posterior_bsvar_t}}
#' 
#' @examples 
#' data(us_fiscal_lsuw)
#' spec = specify_bsvar_t$new(
#'    data = us_fiscal_lsuw,
#'    p = 4
#' )
#' 
#' @export
specify_bsvar_t = R6::R6Class(
  "BSVART",
  
  inherit = specify_bsvar,
  
  public = list(
    
    #' @field p a non-negative integer specifying the autoregressive lag order of the model. 
    p                      = numeric(),
    
    #' @field identification an object IdentificationBSVARs with the identifying restrictions. 
    identification         = list(),
    
    #' @field prior an object PriorBSVART with the prior specification. 
    prior                  = list(),
    
    #' @field data_matrices an object DataMatricesBSVAR with the data matrices.
    data_matrices          = list(),
    
    #' @field starting_values an object StartingValuesBSVART with the starting values.
    starting_values        = list(),
    
    #' @field adaptiveMH a vector of two values setting the Robust Adaptive 
    #' Metropolis sampler for df: target acceptance rate and adaptive rate.
    adaptiveMH             = numeric(),
    
    #' @description
    #' Create a new specification of the BSVAR model with t-distributed structural shocks, BSVART.
    #' @param data a \code{(T+p)xN} matrix with time series data.
    #' @param p a positive integer providing model's autoregressive lag order.
    #' @param B a logical \code{NxN} matrix containing value \code{TRUE} for the 
    #' elements of the structural matrix \eqn{B} to be estimated and value 
    #' \code{FALSE} for exclusion restrictions to be set to zero.
    #' @param exogenous a \code{(T+p)xd} matrix of exogenous variables. 
    #' @param stationary an \code{N} logical vector - its element set to 
    #' \code{FALSE} sets the prior mean for the autoregressive parameters of the 
    #' \code{N}th equation to the white noise process, otherwise to random walk.
    #' @return A new complete specification for the bsvar model with t-distributed 
    #' structural shocks, BSVART.
    initialize = function(
      data,
      p = 1L,
      B,
      exogenous = NULL,
      stationary = rep(FALSE, ncol(data))
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
      
      if (missing(B)) {
        message("The identification is set to the default option of lower-triangular structural matrix.")
        B     = matrix(FALSE, N, N)
        B[lower.tri(B, diag = TRUE)] = TRUE
      }
      stopifnot("Incorrectly specified argument B." = (is.matrix(B) & is.logical(B)) | (length(B) == 1 & is.na(B)))
      
      self$data_matrices   = specify_data_matrices$new(data, p, exogenous)
      self$identification  = specify_identification_bsvars$new(N, B)
      self$prior           = specify_prior_bsvar_t$new(N, p, d, stationary)
      self$starting_values = specify_starting_values_bsvar_t$new(N, self$p, T, d)
      self$adaptiveMH      = c(0.44, 0.6)
    } # END initialize
  ) # END public
) # END specify_bsvar_t




#' R6 Class Representing PosteriorBSVART
#'
#' @description
#' The class PosteriorBSVART contains posterior output and the specification including 
#' the last MCMC draw for the bsvar model with t-distributed structural shocks.
#' Note that due to the thinning of the MCMC output the starting value in element \code{last_draw}
#' might not be equal to the last draw provided in element \code{posterior}.
#' 
#' @seealso \code{\link{estimate}}, \code{\link{specify_bsvar_t}}
#' 
#' @examples 
#' # This is a function that is used within estimate()
#' data(us_fiscal_lsuw)
#' specification  = specify_bsvar_t$new(us_fiscal_lsuw, p = 4)
#' set.seed(123)
#' estimate       = estimate(specification, 10)
#' class(estimate)
#' 
#' @export
specify_posterior_bsvar_t = R6::R6Class(
  "PosteriorBSVART",
  
  private = list(
    normalised = FALSE
  ), # END private
  
  public = list(
    
    #' @field last_draw an object of class BSVART with the last draw of the 
    #' current MCMC run as the starting value to be passed to the continuation 
    #' of the MCMC estimation using \code{estimate()}. 
    last_draw = list(),
    
    #' @field posterior a list containing Bayesian estimation output.
    posterior = list(),
    
    #' @description
    #' Create a new posterior output PosteriorBSVART.
    #' @param specification_bsvar an object of class BSVART with the last draw 
    #' of the current MCMC run as the starting value.
    #' @param posterior_bsvar a list containing Bayesian estimation output.
    #' @return A posterior output PosteriorBSVART.
    initialize = function(specification_bsvar, posterior_bsvar) {
      
      stopifnot("Argument specification_bsvar must be of class BSVART." = any(class(specification_bsvar) == "BSVART"))
      stopifnot("Argument posterior_bsvar must must contain MCMC output." = is.list(posterior_bsvar) & is.array(posterior_bsvar$B) & is.array(posterior_bsvar$A) & is.numeric(posterior_bsvar$df))
      
      self$last_draw    = specification_bsvar
      self$posterior    = posterior_bsvar
    }, # END initialize
    
    #' @description
    #' Returns a list containing Bayesian estimation output.
    #' 
    #' @examples 
    #' data(us_fiscal_lsuw)
    #' specification  = specify_bsvar_t$new(us_fiscal_lsuw)
    #' set.seed(123)
    #' estimate       = estimate(specification, 10)
    #' estimate$get_posterior()
    #' 
    get_posterior       = function(){
      self$posterior
    }, # END get_posterior
    
    #' @description
    #' Returns an object of class BSVART with the last draw of the current MCMC 
    #' run as the starting value to be passed to the continuation of the MCMC 
    #' estimation using \code{estimate()}.
    #' 
    #' @examples
    #' data(us_fiscal_lsuw)
    #' 
    #' # specify the model and set seed
    #' specification  = specify_bsvar_t$new(us_fiscal_lsuw, p = 4)
    #' 
    #' # run the burn-in
    #' set.seed(123)
    #' burn_in        = estimate(specification, 10)
    #' 
    #' # estimate the model
    #' posterior      = estimate(burn_in, 10)
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
    #' specification  = specify_bsvar_t$new(us_fiscal_lsuw, p = 4)
    #' 
    #' # estimate the model
    #' set.seed(123)
    #' posterior      = estimate(specification, 10)
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
    #' specification  = specify_bsvar_t$new(us_fiscal_lsuw, p = 4)
    #' set.seed(123)
    #' 
    #' # estimate the model
    #' posterior      = estimate(specification, 10)
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
) # END specify_posterior_bsvar_t
