
#' R6 Class Representing PriorBSVAR-SV
#'
#' @description
#' The class PriorBSVAR-MS presents a prior specification for the bsvar model with Markov Switching Heteroskedasticity.
#' 
#' @export
specify_prior_bsvar_msh = R6::R6Class(
  "PriorBSVAR-MSH",
  
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
    
    #' @field sigma_nu a positive scalar,  the shape parameter of the inverted-gamma 2 for MS state-dependent variances of the structural shocks, \eqn{\sigma^2_{n.s_t}}.
    sigma_nu   = 3,
    
    #' @field sigma_s a positive scalar,  the scale parameter of the inverted-gamma 2 for MS state-dependent variances of the structural shocks, \eqn{\sigma^2_{n.s_t}}.
    sigma_s    = 1,
    
    #' @field PR_TR an \code{MxM} matrix, the matrix of hyper-parameters of the row-specific Dirichlet prior distribution for transition probabilities matrix \eqn{P} of the Markov process \eqn{s_t}. 
    PR_TR      = matrix(),
    
    #' @description
    #' Create a new prior specification PriorBSVAR-MSH.
    #' @param N a positive integer - the number of dependent variables in the model.
    #' @param p a positive integer - the autoregressive lag order of the SVAR model.
    #' @param M an integer greater than 1 - the number of Markov process' heteroskedastic regimes.
    #' @param stationary an \code{N} logical vector - its element set to \code{FALSE} sets the prior mean for the autoregressive parameters of the \code{N}th equation to the white noise process, otherwise to random walk.
    #' @return A new prior specification PriorBSVAR-SV.
    initialize = function(N, p, M, stationary = rep(FALSE, N)){
      stopifnot("Argument N must be a positive integer number." = N > 0 & N %% 1 == 0)
      stopifnot("Argument p must be a positive integer number." = p > 0 & p %% 1 == 0)
      stopifnot("Argument stationary must be a logical vector of length N." = length(stationary) == N & is.logical(stationary))
      stopifnot("Argument M must be an integer number greater than 1." = M > 1 & M %% 1 == 0)
      
      super$initialize(N, p, stationary)
      self$sigma_nu         = 3
      self$sigma_s          = 1
      self$PR_TR            = matrix(1, M, M)
      
    }, # END initialize
    
    #' @description
    #' Returns the elements of the prior specification PriorBSVAR-MSH as a \code{list}.
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
        sv_s_    = self$sv_s_,
        sigma_nu = self$sigma_nu,
        sigma_s  = self$sigma_s,
        PR_TR    = self$PR_TR
      )
    } # END get_prior
    
  ) # END public
) # END specify_prior_bsvar_msh




#' R6 Class Representing StartingValuesBSVAR-MSH
#'
#' @description
#' The class StartingValuesBSVAR-MSH presents starting values for the bsvar model with Markov Switching Heteroskedasticity.
#' 
#' @export
specify_starting_values_bsvar_msh = R6::R6Class(
  "StartingValuesBSVAR-MSH",
  
  inherit = specify_starting_values_bsvar,
  
  public = list(
    
    #' @field A an \code{NxK} matrix of starting values for the parameter \eqn{A}. 
    A             = matrix(),
    
    #' @field B an \code{NxN} matrix of starting values for the parameter \eqn{B}. 
    B             = matrix(),
    
    #' @field hyper a \code{5}-vector of starting values for the shrinkage hyper-parameters of the hierarchical prior distribution. 
    hyper         = numeric(),
    
    #' @field sigma2 an \code{NxM} matrix of starting values for the MS state-specific variances of the structural shocks. Its elements sum up to value \code{M} over the rows.
    sigma2        = matrix(),
    
    #' @field PR_TR an \code{MxM} matrix of starting values for the transition probability matrix of the Markov process. Its elements sum up to 1 over the rows.
    PR_TR         = matrix(),
    
    #' @field xi an \code{MxT} matrix of starting values for the Markov process indicator. Its columns are a chosen column of an identity matrix of order \code{M}.
    xi            = matrix(),
    
    #' @field pi_0 an \code{M}-vector of starting values for state probability at time \code{t=0}. Its elements sum to 1.
    pi_0          = numeric(),
    
    
    #' @description
    #' Create new starting values StartingValuesBSVAR-MS.
    #' @param N a positive integer - the number of dependent variables in the model.
    #' @param p a positive integer - the autoregressive lag order of the SVAR model.
    #' @param M an integer greater than 1 - the number of Markov process' heteroskedastic regimes.
    #' @param T a positive integer - the the time series dimension of the dependent variable matrix \eqn{Y}.
    #' @param finiteM a logical value - if true a stationary Markov switching model is estimated. Otherwise, a sparse Markov switching model is estimated in which \code{M=20} and the number of visited states is estimated.
    #' @return Starting values StartingValuesBSVAR-MS.
    initialize = function(N, p, M, T, finiteM = TRUE){
      stopifnot("Argument N must be a positive integer number." = N > 0 & N %% 1 == 0)
      stopifnot("Argument p must be a positive integer number." = p > 0 & p %% 1 == 0)
      stopifnot("Argument M must be an integer number greater than 1." = M > 1 & M %% 1 == 0)
      stopifnot("Argument T must be a positive integer number." = T > 0 & T %% 1 == 0)
      stopifnot("Argument finiteM must be a logical value." = is.logical(finiteM) & length(finiteM) == 1)
      
      if (!finiteM) {
        M = 20
      }
      
      super$initialize(N, p)
      
      self$sigma2         = matrix(1, N, M)
      self$PR_TR          = diag(M)
      self$xi             = diag(M)[,sample(1:M, T, replace = TRUE)]
      self$pi_0           = rep(1/M, M)
    }, # END initialize
    
    #' @description
    #' Returns the elements of the starting values StartingValuesBSVAR-MS as a \code{list}.
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
    #' Returns the elements of the starting values StartingValuesBSVAR-MSH as a \code{list}.
    #' @param last_draw a list containing the last draw.
    #' @return An object of class StartingValuesBSVAR-MS including the last draw of the current MCMC as the starting value to be passed to the continuation of the MCMC estimation using \code{bsvar_msh()}.
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




#' R6 Class representing the specification of the BSVAR model with Markov Switching Heteroskedasticity.
#'
#' @description
#' The class BSVAR-MSH presents complete specification for the BSVAR model with Markov Switching Heteroskedasticity.
#' 
#' @export
specify_bsvar_msh = R6::R6Class(
  "BSVAR-MSH",
  
  public = list(
    
    #' @field p a non-negative integer specifying the autoregressive lag order of the model. 
    p                      = numeric(),
    
    #' @field identification an object IdentificationBSVARs with the identifying restrictions. 
    identification         = list(),
    
    #' @field prior an object PriorBSVAR-MSH with the prior specification. 
    prior                  = list(),
    
    #' @field data_matrices an object DataMatricesBSVAR with the data matrices.
    data_matrices          = list(),
    
    #' @field starting_values an object StartingValuesBSVAR-MSH with the starting values.
    starting_values        = list(),
    
    #' @field finiteM a logical value - if true a stationary Markov switching model is estimated. Otherwise, a sparse Markov switching model is estimated in which \code{M=20} and the number of visited states is estimated.
    finiteM                = logical(),
    
    #' @description
    #' Create a new specification of the BSVAR model with Markov Switching Heteroskedasticity, BSVAR-MSH.
    #' @param data a \code{(T+p)xN} matrix with time series data.
    #' @param p a positive integer providing model's autoregressive lag order.
    #' @param M an integer greater than 1 - the number of Markov process' heteroskedastic regimes.
    #' @param B a logical \code{NxN} matrix containing value \code{TRUE} for the elements of the structural matrix \eqn{B} to be estimated and value \code{FALSE} for exclusion restrictions to be set to zero.
    #' @param stationary an \code{N} logical vector - its element set to \code{FALSE} sets the prior mean for the autoregressive parameters of the \code{N}th equation to the white noise process, otherwise to random walk.
    #' @param finiteM a logical value - if true a stationary Markov switching model is estimated. Otherwise, a sparse Markov switching model is estimated in which \code{M=20} and the number of visited states is estimated.
    #' @return A new complete specification for the bsvar model with Markov Switching Heteroskedasticity, BSVAR-MSH.
    initialize = function(
    data,
    p = 1L,
    M,
    B,
    stationary = rep(FALSE, ncol(data)),
    finiteM = TRUE
    ) {
      stopifnot("Argument p has to be a positive integer." = ((p %% 1) == 0 & p > 0))
      self$p        = p
      
      TT            = nrow(data)
      T             = TT - self$p
      N             = ncol(data)
      
      if (!finiteM) {
        M = 20
        message("In the sparse Markov switching model the value of M is overwritten and set to 20.")
      }
      self$finiteM  = finiteM
      
      if (missing(B)) {
        message("The identification is set to the default option of lower-triangular structural matrix.")
        B     = matrix(FALSE, N, N)
        B[lower.tri(B, diag = TRUE)] = TRUE
      }
      stopifnot("Incorrectly specified argument B." = (is.matrix(B) & is.logical(B)) | (length(B) == 1 & is.na(B)))
      
      self$data_matrices   = specify_data_matrices$new(data, p)
      self$identification  = specify_identification_bsvars$new(N, B)
      self$prior           = specify_prior_bsvar_msh$new(N, p, M, stationary)
      self$starting_values = specify_starting_values_bsvar_msh$new(N, self$p, M, T, finiteM)
    }, # END initialize
    
    #' @description
    #' Returns the data matrices as the DataMatricesBSVAR object.
    get_data_matrices = function() {
      self$data_matrices$clone()
    }, # END get_data_matrices
    
    #' @description
    #' Returns the identifying restrictions as the IdentificationBSVARs object.
    get_identification = function() {
      self$identification$clone()
    }, # END get_identification
    
    #' @description
    #' Returns the prior specification as the PriorBSVAR-MSH object.
    get_prior = function() {
      self$prior$clone()
    }, # END get_prior
    
    #' @description
    #' Returns the starting values as the StartingValuesBSVAR-MSH object.
    get_starting_values = function() {
      self$starting_values$clone()
    } # END get_starting_values
  ) # END public
) # END specify_bsvar_msh




#' R6 Class Representing PosteriorBSVAR-MSH
#'
#' @description
#' The class PosteriorBSVAR-MSH contains posterior output and the specification including 
#' the last MCMC draw for the bsvar model with Markov Switching Heteroskedasticity. 
#' Note that due to the thinning of the MCMC output the starting value in element \code{last_draw}
#' might not be equal to the last draw provided in element \code{posterior}.
#' 
#' @export
specify_posterior_bsvar_msh = R6::R6Class(
  "PosteriorBSVAR-MSH",
  
  public = list(
    
    #' @field last_draw an object of class BSVAR-MSH with the last draw of the current MCMC run as the starting value to be passed to the continuation of the MCMC estimation using \code{bsvar_msh()}. 
    last_draw = list(),
    
    #' @field posterior a list containing Bayesian estimation output.
    posterior = list(),
    
    #' @description
    #' Create a new posterior output PosteriorBSVAR-MSH.
    #' @param specification_bsvar an object of class BSVAR-MSH with the last draw of the current MCMC run as the starting value.
    #' @param posterior_bsvar a list containing Bayesian estimation output.
    #' @return A posterior output PosteriorBSVAR-MSH.
    initialize = function(specification_bsvar, posterior_bsvar) {
      
      stopifnot("Argument specification_bsvar must be of class BSVAR-MSH." = any(class(specification_bsvar) == "BSVAR-MSH"))
      stopifnot("Argument posterior_bsvar must must contain MCMC output." = is.list(posterior_bsvar) & is.array(posterior_bsvar$B) & is.array(posterior_bsvar$A) & is.matrix(posterior_bsvar$hyper) & is.array(posterior_bsvar$xi))
      
      self$last_draw    = specification_bsvar
      self$posterior    = posterior_bsvar
    }, # END initialize
    
    #' @description
    #' Returns a list containing Bayesian estimation output.
    get_posterior       = function(){
      self$posterior$clone()
    }, # END get_posterior
    
    #' @description
    #' Returns an object of class BSVAR-MSH with the last draw of the current MCMC run as the starting value to be passed to the continuation of the MCMC estimation using \code{bsvar_msh()}.
    get_last_draw      = function(){
      self$last_draw$clone()
    } # END get_last_draw
    
  ) # END public
) # END specify_posterior_bsvar_msh
