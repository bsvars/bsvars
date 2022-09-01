
#' R6 Class Representing PriorBSVAR-SV
#'
#' @description
#' The class PriorBSVAR-SV presents a prior specification for the bsvar model with Stochastic Volatility heteroskedasticity.
#' 
#' @examples 
#' prior = specify_prior_bsvar_sv$new(N = 3, p = 1) # a prior for 3-variable example with one lag
#' prior$A                                          # show autoregressive prior mean
#' 
#' @export
specify_prior_bsvar_sv = R6::R6Class(
  "PriorBSVAR-SV",
  
  inherit = specify_prior_bsvar,
  
  public = list(
    
    #' @field A an \code{NxK} matrix, the mean of the normal prior distribution for the parameter matrix \eqn{A}. 
    A          = matrix(),
    
    #' @field A_V_inv a \code{KxK} precision matrix of the normal prior distribution for each of the row of the parameter matrix \eqn{A}. This precision matrix is equation invariant.
    A_V_inv    = matrix(),
    
    #' @field B_V_inv an \code{NxN} precision matrix of the generalised-normal prior distribution for the structural matrix \eqn{B}. This precision matrix is equation invariant.
    B_V_inv    = matrix(),
    
    #' @field B_nu a positive integer greater of equal than \code{N}, a shape parameter of the generalised-normal prior distribution for the structural matrix \eqn{B}.
    B_nu       = NA,
    
    #' @field hyper_nu a positive scalar, the shape parameter of the inverted-gamma 2 prior distribution for the two overall shrinkage parameters for matrices \eqn{B} and \eqn{A}.
    hyper_nu   = NA,
    
    #' @field hyper_a a positive scalar, the shape parameter of the gamma prior for the two overall shrinkage parameters.
    hyper_a    = NA,
    
    #' @field hyper_V a positive scalar,  the shape parameter of the inverted-gamma 2 for the level 3 hierarchy of shrinkage parameters.
    hyper_V    = NA,
    
    #' @field hyper_S a positive scalar,  the scale parameter of the inverted-gamma 2 for the level 3 hierarchy of shrinkage parameters.
    hyper_S    = NA,
    
    #' @field sv_a_ a positive scalar, the shape parameter of the gamma prior in the hierarchical prior for \eqn{\sigma^2_{\omega}}. 
    sv_a_      = numeric(),
    
    #' @field sv_s_ a positive scalar, the scale parameter of the gamma prior in the hierarchical prior for \eqn{\sigma^2_{\omega}}.
    sv_s_      = numeric(),
    
    #' @description
    #' Create a new prior specification PriorBSVAR-SV.
    #' @param N a positive integer - the number of dependent variables in the model.
    #' @param p a positive integer - the autoregressive lag order of the SVAR model.
    #' @param stationary an \code{N} logical vector - its element set to \code{FALSE} sets the prior mean for the autoregressive parameters of the \code{N}th equation to the white noise process, otherwise to random walk.
    #' @return A new prior specification PriorBSVAR-SV.
    initialize = function(N, p, stationary = rep(FALSE, N)){
      stopifnot("Argument N must be a positive integer number." = N > 0 & N %% 1 == 0)
      stopifnot("Argument p must be a positive integer number." = p > 0 & p %% 1 == 0)
      stopifnot("Argument stationary must be a logical vector of length N." = length(stationary) == N & is.logical(stationary))
      
      super$initialize(N, p, stationary)
      self$sv_a_             = 1
      self$sv_s_             = 0.1
    }, # END initialize

    #' @description
    #' Returns the elements of the prior specification PriorBSVAR-SV as a \code{list}.
    #' 
    #' @examples 
    #' # a prior for 3-variable example with four lags
    #' prior = specify_prior_bsvar_sv$new(N = 3, p = 4)
    #' prior$get_prior() # show the prior as list
    #' 
    get_prior           = function(){
      list(
        A        = self$A,
        A_V_inv  = self$A_V_inv,
        B_V_inv  = self$B_V_inv,
        B_nu     = self$B_nu,
        hyper_nu = self$hyper_nu,
        hyper_a  = self$hyper_a,
        hyper_V  = self$hyper_V,
        hyper_S  = self$hyper_S,
        sv_a_    = self$sv_a_,
        sv_s_    = self$sv_s_
      )
    } # END get_prior
    
  ) # END public
) # END specify_prior_bsvar_sv



#' R6 Class Representing StartingValuesBSVAR-SV
#'
#' @description
#' The class StartingValuesBSVAR-SV presents starting values for the bsvar model with Stochastic Volatility heteroskedasticity.
#' 
#' @examples 
#' # starting values for a bsvar model for a 3-variable system
#' sv = specify_starting_values_bsvar_sv$new(N = 3, p = 1, T = 100)
#' 
#' @export
specify_starting_values_bsvar_sv = R6::R6Class(
  "StartingValuesBSVAR-SV",
  
  inherit = specify_starting_values_bsvar,
  
  public = list(
    
    #' @field A an \code{NxK} matrix of starting values for the parameter \eqn{A}. 
    A             = matrix(),
    
    #' @field B an \code{NxN} matrix of starting values for the parameter \eqn{B}. 
    B             = matrix(),
    
    #' @field hyper a \code{5}-vector of starting values for the shrinkage hyper-parameters of the hierarchical prior distribution.
    hyper         = numeric(),
    
    #' @field h an \code{NxT} matrix with the starting values of the log-volatility processes.
    h             = matrix(),
    
    #' @field rho an \code{N}-vector with values of SV autoregressive parameters.
    rho           = numeric(),
    
    #' @field omega an \code{N}-vector with values of SV process conditional standard deviations.
    omega         = numeric(),
    
    #' @field S an \code{NxT} integer matrix with the auxiliary mixture component indicators.
    S             = matrix(),
    
    #' @field sigma2_omega an \code{N}-vector with variances of the zero-mean normal prior for \eqn{\omega_n}.
    sigma2_omega  = numeric(),
    
    #' @field s_ a positive scalar with the scale of the gamma prior of the hierarchical prior for \eqn{\sigma^2_{\omega}}.
    s_            = numeric(),
    
    #' @description
    #' Create new starting values StartingValuesBSVAR-SV.
    #' @param N a positive integer - the number of dependent variables in the model.
    #' @param p a positive integer - the autoregressive lag order of the SVAR model.
    #' @param T a positive integer - the the time series dimension of the dependent variable matrix \eqn{Y}.
    #' @return Starting values StartingValuesBSVAR-SV.
    initialize = function(N, p, T){
      stopifnot("Argument N must be a positive integer number." = N > 0 & N %% 1 == 0)
      stopifnot("Argument p must be a positive integer number." = p > 0 & p %% 1 == 0)
      stopifnot("Argument T must be a positive integer number." = T > 0 & T %% 1 == 0)
      
      super$initialize(N, p)
      
      self$h              = matrix(rnorm(N * T, sd = .01), N, T)
      self$rho            = rep(.5, N)
      self$omega          = rep(.1, N)
      self$S              = matrix(1, N, T)
      self$sigma2_omega   = rep(1, N)
      self$s_             = rep(0.05, N)
    }, # END initialize
    
    #' @description
    #' Returns the elements of the starting values StartingValuesBSVAR-SV as a \code{list}.
    #' 
    #' @examples 
    #' # starting values for a bsvar model with 1 lag for a 3-variable system
    #' sv = specify_starting_values_bsvar_sv$new(N = 3, p = 1, T = 100)
    #' sv$get_starting_values()   # show starting values as list
    #' 
    get_starting_values   = function(){
      list(
        B                 = self$B,
        A                 = self$A,
        hyper             = self$hyper,
        h                 = self$h,
        rho               = self$rho,
        omega             = self$omega,
        S                 = self$S,
        sigma2_omega      = self$sigma2_omega,
        s_                = self$s_
      )
    }, # END get_starting_values
    
    #' @description
    #' Returns the elements of the starting values StartingValuesBSVAR_SV as a \code{list}.
    #' @param last_draw a list containing the last draw of the current MCMC run.
    #' @return An object of class StartingValuesBSVAR including the last draw of the current MCMC as the starting value to be passed to the continuation of the MCMC estimation using \code{bsvar()}.
    #' 
    #' @examples 
    #' # starting values for a bsvar model with 1 lag for a 3-variable system
    #' sv = specify_starting_values_bsvar_sv$new(N = 3, p = 1, T = 100)
    #' 
    #' # Modify the starting values by:
    #' sv_list = sv$get_starting_values()   # getting them as list
    #' sv_list$A <- matrix(rnorm(12), 3, 4) # modifying the entry
    #' sv$set_starting_values(sv_list)      # providing to the class object
    #' 
    set_starting_values   = function(last_draw) {
      self$B              = last_draw$B
      self$A              = last_draw$A
      self$hyper          = last_draw$hyper
      self$h              = last_draw$h
      self$rho            = last_draw$rho
      self$omega          = last_draw$omega
      self$S              = last_draw$S
      self$sigma2_omega   = last_draw$sigma2_omega
      self$s_             = last_draw$s_
    } # END set_starting_values
  ) # END public
) # END specify_starting_values_bsvar_sv



#' R6 Class representing the specification of the BSVAR model with Stochastic Volatility heteroskedasticity.
#'
#' @description
#' The class BSVAR-SV presents complete specification for the BSVAR model with Stochastic Volatility heteroskedasticity.
#' 
#' @seealso \code{\link{estimate_bsvar_sv}}, \code{\link{specify_posterior_bsvar_sv}}
#' 
#' @examples 
#' data(us_fiscal_lsuw)
#' spec = specify_bsvar_sv$new(
#'    data = us_fiscal_lsuw,
#'    p = 4
#' )
#' 
#' @export
specify_bsvar_sv = R6::R6Class(
  "BSVAR-SV",
  
  public = list(
    
    #' @field p a non-negative integer specifying the autoregressive lag order of the model. 
    p                      = numeric(),
    
    #' @field identification an object IdentificationBSVARs with the identifying restrictions. 
    identification         = list(),
    
    #' @field prior an object PriorBSVAR-SV with the prior specification. 
    prior                  = list(),
    
    #' @field data_matrices an object DataMatricesBSVAR with the data matrices.
    data_matrices          = list(),
    
    #' @field starting_values an object StartingValuesBSVAR-SV with the starting values.
    starting_values        = list(),
    
    #' @description
    #' Create a new specification of the BSVAR model with Stochastic Volatility heteroskedasticity, BSVAR-SV.
    #' @param data a \code{(T+p)xN} matrix with time series data.
    #' @param p a positive integer providing model's autoregressive lag order.
    #' @param B a logical \code{NxN} matrix containing value \code{TRUE} for the elements of the structural matrix \eqn{B} to be estimated and value \code{FALSE} for exclusion restrictions to be set to zero.
    #' @param stationary an \code{N} logical vector - its element set to \code{FALSE} sets the prior mean for the autoregressive parameters of the \code{N}th equation to the white noise process, otherwise to random walk.
    #' @return A new complete specification for the bsvar model with Stochastic Volatility heteroskedasticity, BSVAR-SV.
    initialize = function(
    data,
    p = 1L,
    B,
    stationary = rep(FALSE, ncol(data))
    ) {
      stopifnot("Argument p has to be a positive integer." = ((p %% 1) == 0 & p > 0))
      self$p     = p
      
      TT            = nrow(data)
      T             = TT - self$p
      N             = ncol(data)
      
      if (missing(B)) {
        message("The identification is set to the default option of lower-triangular structural matrix.")
        B     = matrix(FALSE, N, N)
        B[lower.tri(B, diag = TRUE)] = TRUE
      }
      stopifnot("Incorrectly specified argument B." = (is.matrix(B) & is.logical(B)) | (length(B) == 1 & is.na(B)))
      
      self$data_matrices   = specify_data_matrices$new(data, p)
      self$identification  = specify_identification_bsvars$new(N, B)
      self$prior           = specify_prior_bsvar_sv$new(N, p, stationary)
      self$starting_values = specify_starting_values_bsvar_sv$new(N, self$p, T)
    }, # END initialize
    
    #' @description
    #' Returns the data matrices as the DataMatricesBSVAR object.
    #' 
    #' @examples 
    #' data(us_fiscal_lsuw)
    #' spec = specify_bsvar_sv$new(
    #'    data = us_fiscal_lsuw,
    #'    p = 4
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
    #' data(us_fiscal_lsuw)
    #' spec = specify_bsvar_sv$new(
    #'    data = us_fiscal_lsuw,
    #'    p = 4
    #' )
    #' spec$get_identification()
    #' 
    get_identification = function() {
      self$identification$clone()
    }, # END get_identification
    
    #' @description
    #' Returns the prior specification as the PriorBSVAR-SV object.
    #' 
    #' @examples 
    #' data(us_fiscal_lsuw)
    #' spec = specify_bsvar_sv$new(
    #'    data = us_fiscal_lsuw,
    #'    p = 4
    #' )
    #' spec$get_prior()
    #' 
    get_prior = function() {
      self$prior$clone()
    }, # END get_prior
    
    #' @description
    #' Returns the starting values as the StartingValuesBSVAR-SV object.
    #' 
    #' @examples 
    #' data(us_fiscal_lsuw)
    #' spec = specify_bsvar_sv$new(
    #'    data = us_fiscal_lsuw,
    #'    p = 4
    #' )
    #' spec$get_starting_values()
    #' 
    get_starting_values = function() {
      self$starting_values$clone()
    } # END get_starting_values
  ) # END public
) # END specify_bsvar_sv



#' R6 Class Representing PosteriorBSVAR-SV
#'
#' @description
#' The class PosteriorBSVAR-SV contains posterior output and the specification including 
#' the last MCMC draw for the bsvar model with Stochastic Volatility heteroskedasticity.
#' Note that due to the thinning of the MCMC output the starting value in element \code{last_draw}
#' might not be equal to the last draw provided in element \code{posterior}.
#' 
#' @seealso \code{\link{estimate_bsvar_sv}}, \code{\link{specify_bsvar_sv}}
#' 
#' @examples 
#' # This is a function that is used within estimate_bsvar()
#' data(us_fiscal_lsuw)
#' specification  = specify_bsvar_sv$new(us_fiscal_lsuw, p = 4)
#' set.seed(123)
#' estimate       = estimate_bsvar_sv(10, specification)
#' class(estimate)
#' 
#' @export
specify_posterior_bsvar_sv = R6::R6Class(
  "PosteriorBSVAR-SV",
  
  private = list(
    normalised = FALSE
  ), # END private
  
  public = list(
    
    #' @field last_draw an object of class BSVAR-SV with the last draw of the current MCMC run as the starting value to be passed to the continuation of the MCMC estimation using \code{bsvar_sv()}. 
    last_draw = list(),
    
    #' @field posterior a list containing Bayesian estimation output.
    posterior = list(),
    
    #' @description
    #' Create a new posterior output PosteriorBSVAR-SV.
    #' @param specification_bsvar an object of class BSVAR-SV with the last draw of the current MCMC run as the starting value.
    #' @param posterior_bsvar a list containing Bayesian estimation output.
    #' @return A posterior output PosteriorBSVAR-SV.
    initialize = function(specification_bsvar, posterior_bsvar) {
      
      stopifnot("Argument specification_bsvar must be of class BSVAR-SV." = any(class(specification_bsvar) == "BSVAR-SV"))
      stopifnot("Argument posterior_bsvar must must contain MCMC output." = is.list(posterior_bsvar) & is.array(posterior_bsvar$B) & is.array(posterior_bsvar$A) & is.matrix(posterior_bsvar$hyper) & is.array(posterior_bsvar$h))
      
      self$last_draw    = specification_bsvar
      self$posterior    = posterior_bsvar
    }, # END initialize
    
    #' @description
    #' Returns a list containing Bayesian estimation.
    #' 
    #' @examples 
    #' data(us_fiscal_lsuw)
    #' specification  = specify_bsvar_sv$new(us_fiscal_lsuw)
    #' set.seed(123)
    #' estimate       = estimate_bsvar_sv(10, specification)
    #' estimate$get_posterior()
    #' 
    get_posterior       = function(){
      self$posterior
    }, # END get_posterior
    
    #' @description
    #' Returns an object of class BSVAR-SV with the last draw of the current MCMC run as the starting value to be passed to the continuation of the MCMC estimation using \code{bsvar_sv()}.
    #' 
    #' @examples
    #' data(us_fiscal_lsuw)
    #' 
    #' # specify the model and set seed
    #' specification  = specify_bsvar_sv$new(us_fiscal_lsuw, p = 4)
    #' set.seed(123)
    #' 
    #' # run the burn-in
    #' burn_in        = estimate_bsvar_sv(10, specification)
    #' 
    #' # get the last draw
    #' last_draw      = burn_in$get_last_draw()
    #' 
    #' # estimate the model
    #' posterior      = estimate_bsvar_sv(10, last_draw)
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
    #' specification  = specify_bsvar_sv$new(us_fiscal_lsuw, p = 4)
    #' 
    #' # estimate the model
    #' set.seed(123)
    #' posterior      = estimate_bsvar_sv(10, specification, thin = 1)
    #' 
    #' # check normalisation status beforehand
    #' posterior$is_normalised()
    #' 
    #' # normalise the posterior
    #' BB            = posterior$last_draw$starting_values$B      # get the last draw of B
    #' B_hat         = diag(sign(diag(BB))) %*% BB                # set positive diagonal elements
    #' bsvars::normalise_posterior(posterior, B_hat)              # draws in posterior are normalised
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
    #' specification  = specify_bsvar_sv$new(us_fiscal_lsuw, p = 4)
    #' 
    #' # estimate the model
    #' set.seed(123)
    #' posterior      = estimate_bsvar_sv(10, specification, thin = 1)
    #' 
    #' # check normalisation status beforehand
    #' posterior$is_normalised()
    #' 
    #' # normalise the posterior
    #' BB            = posterior$last_draw$starting_values$B      # get the last draw of B
    #' B_hat         = diag(sign(diag(BB))) %*% BB                # set positive diagonal elements
    #' bsvars::normalise_posterior(posterior, B_hat)              # draws in posterior are normalised
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
) # END specify_posterior_bsvar_sv
