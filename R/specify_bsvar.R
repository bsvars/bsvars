#' R6 Class Representing PriorBSVAR
#'
#' @description
#' The class PriorBSVAR presents a prior specification for the homoskedastic bsvar model.
specify_prior_bsvar = R6::R6Class(
  "PriorBSVAR",
  
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
    
    #' @description
    #' Create a new prior specification PriorBSVAR.
    #' @param N a positive integer - the number of dependent variables in the model.
    #' @param p a positive integer - the autoregressive lag order of the SVAR model.
    #' @param stationary an \code{N} logical vector - its element set to \code{FALSE} sets the prior mean for the autoregressive parameters of the \code{N}th equation to the white noise process, otherwise to random walk.
    #' @return A new prior specification PriorBSVAR.
    initialize = function(N, p, stationary = rep(FALSE, N)){
      stopifnot("Argument N must be a positive integer number." = N > 0 & N %% 1 == 0)
      stopifnot("Argument p must be a positive integer number." = p > 0 & p %% 1 == 0)
      stopifnot("Argument stationary must be an N-vector." = length(stationary) == N)
      
      K                 = N * p + 1
      self$A            = cbind(diag(as.numeric(!stationary)), matrix(0, N, K - N))
      self$A_V_inv      = diag(c(kronecker((1:p)^2, rep(1, N) ), 1))
      self$B_V_inv      = diag(N)
      self$B_nu         = N
      self$hyper_nu     = 3
      self$hyper_a      = 1
      self$hyper_V      = 3
      self$hyper_S      = 1
    }, # END initialize
    
    #' @description
    #' Returns the elements of the prior specification PriorBSVAR as a \code{list}.
    get_prior_bsvar = function(){
      list(
        A        = self$A,
        A_V_inv  = self$A_V_inv,
        B_V_inv  = self$B_V_inv,
        B_nu     = self$B_nu,
        hyper_nu = self$hyper_nu,
        hyper_a  = self$hyper_a,
        hyper_V  = self$hyper_V,
        hyper_S  = self$hyper_S
      )
    } # END get_prior_bsvar
    
  ) # END public
) # END specify_prior_bsvar


#' R6 Class Representing StartingValuesBSVAR
#'
#' @description
#' The class StartingValuesBSVAR presents starting values for the homoskedastic bsvar model.
specify_starting_values_bsvar = R6::R6Class(
  "StartingValuesBSVAR",
  
  public = list(
    
    #' @field A an \code{NxK} matrix of starting values for the parameter \eqn{A}. 
    A             = matrix(),
    
    #' @field B an \code{NxN} matrix of starting values for the parameter \eqn{B}. 
    B             = matrix(),
    
    #' @field hyper a \code{5}-vector of starting values for the shrinkage hyper-parameters of the hierarchical prior distribution. 
    hyper         = numeric(),
    
    #' @description
    #' Create new starting values StartingValuesBSVAR.
    #' @param N a positive integer - the number of dependent variables in the model.
    #' @param p a positive integer - the autoregressive lag order of the SVAR model.
    #' @return Starting values StartingValuesBSVAR.
    initialize = function(N, p){
      stopifnot("Argument N must be a positive integer number." = N > 0 & N %% 1 == 0)
      stopifnot("Argument p must be a positive integer number." = p > 0 & p %% 1 == 0)

      K                  = N * p + 1
      self$B             = diag(N)
      self$A             = cbind(diag(runif(N)), matrix(0, N, K - N))
      self$hyper         = c(1,rep(1,4))
    }, # END initialize
    
    #' @description
    #' Returns the elements of the starting values StartingValuesBSVAR as a \code{list}.
    get_starting_values_bsvar = function(){
      list(
        B            = self$B,
        A            = self$A,
        hyper        = self$hyper
      )
    } # END get_starting_values_bsvar
  ) # END public
) # END specify_starting_values_bsvar



#' R6 Class Representing IdentificationBSVAR
#'
#' @description
#' The class IdentificationBSVAR presents the identifying restrictions for the homoskedastic bsvar model.
specify_identification_bsvar = R6::R6Class(
  "IdentificationBSVAR",
  
  public = list(
    
    #' @field VB a list of \code{N} matrices determining the unrestricted elements of matrix \eqn{B}. 
    VB    = list(),
    
    #' @description
    #' Create new identifying restrictions IdentificationBSVAR.
    #' @param N a positive integer - the number of dependent variables in the model.
    #' @param B a logical \code{NxN} matrix containing value \code{TRUE} for the elements of the structural matrix \eqn{B} to be estimated and value \code{FALSE} for exclusion restrictions to be set to zero.
    #' @return Identifying restrictions IdentificationBSVAR.
    initialize = function(N, B) {
      if (missing(B)) {
          B     = matrix(FALSE, N, N)
          B[lower.tri(B, diag = TRUE)] = TRUE
      }

      stopifnot("Argument B must be an NxN matrix with logical values." = is.logical(B) & is.matrix(B) & prod(dim(B) == N))
      
      self$VB          <- vector("list", N)
      for (n in 1:N) {
        self$VB[[n]]   <- matrix(diag(N)[B[n,],], ncol = N)
      }
    }, # END initialize
    
    #' @description
    #' Returns the elements of the identification pattern IdentificationBSVAR as a \code{list}.
    get_identification_bsvar = function() {
      as.list(self$VB)
    }, # END get_identification_bsvar
    
    #' @description
    #' Set new starting values StartingValuesBSVAR.
    #' @param N a positive integer - the number of dependent variables in the model.
    #' @param B a logical \code{NxN} matrix containing value \code{TRUE} for the elements of the structural matrix \eqn{B} to be estimated and value \code{FALSE} for exclusion restrictions to be set to zero.
    set_identification_bsvar = function(N, B) {
      if (missing(B)) {
        B     = matrix(FALSE, N, N)
        B[lower.tri(B, diag = TRUE)] = TRUE
      }
      
      stopifnot("Argument B must be an NxN matrix with logical values." = is.logical(B) & is.matrix(B) & prod(dim(B) == N))
      
      self$VB          <- vector("list", N)
      for (n in 1:N) {
        self$VB[[n]]   <- matrix(diag(N)[B[n,],], ncol = N)
      }
    } # END set_identification_bsvar
  ) # END public
) # END specify_identification_bsvar



#' R6 Class Representing DataMatricesBSVAR
#'
#' @description
#' The class DataMatricesBSVAR presents the data matrices of dependent variables, \eqn{Y}, and regressors, \eqn{X}, for the homoskedastic bsvar model.
specify_data_matrices = R6::R6Class(
  "DataMatricesBSVAR",
  
  public = list(
    
    #' @field Y an \code{NxT} matrix of dependent variables, \eqn{Y}. 
    Y     = matrix(),
    
    #' @field X an \code{KxT} matrix of regressors, \eqn{X}. 
    X     = matrix(),
    
    #' @description
    #' Create new data matrices DataMatricesBSVAR.
    #' @param data a \code{(T+p)xN} matrix with time series data.
    #' @param p a positive integer providing model's autoregressive lag order.
    #' @return New data matrices DataMatricesBSVAR.
    initialize = function(data, p = 1L) {
      if (missing(data)) {
        stop("Argument data has to be specified")
      } else {
        stopifnot("Argument data has to be a matrix." = is.matrix(data) & is.numeric(data))
        stopifnot("Argument data has to contain at least 2 columns and 3 rows." = (ncol(data) >= 2 & nrow(data) >= 3))
        stopifnot("Argument data cannot include missing values." = sum(is.na(data)) == 0 )
      }
      stopifnot("Argument p must be a positive integer number." = p > 0 & p %% 1 == 0)
      
      TT            = nrow(data)
      T             = TT - p
      
      self$Y        = t(data[(p + 1):TT,])
      X             = matrix(0, T, 0)
      for (i in 1:p) {
        X           = cbind(X, data[(p + 1):TT - i,])
      }
      self$X        = t(cbind(X, rep(1, T)))
    }, # END initialize
    
    #' @description
    #' Returns the data matrices DataMatricesBSVAR as a \code{list}.
    get_data_matrices = function() {
      list(
        Y = self$Y,
        X = self$X
      )
    } # END get_data_matrices
  ) # END public
) # END specify_data_matrices



#' R6 Class representing the specification of the homoskedastic BSVAR model
#'
#' @description
#' The class BSVAR presents complete specification for the homoskedastic bsvar model.
specify_bsvar = R6::R6Class(
  "BSVAR",
  
  public = list(
    
    #' @field p a non-negative integer specifying the autoregressive lag order of the model. 
    p                      = numeric(),
    
    #' @field identification an object IdentificationBSVAR with the identifying restrictions. 
    identification         = list(),
    
    #' @field prior an object PriorBSVAR with the prior specification. 
    prior                  = list(),
    
    #' @field data_matrices an object DataMatricesBSVAR with the data matrices.
    data_matrices          = list(),
    
    #' @field starting_values an object StartingValuesBSVAR with the starting values.
    starting_values        = list(),
    
    #' @description
    #' Create a new specification of the homoskedastic bsvar model BSVAR.
    #' @param data a \code{(T+p)xN} matrix with time series data.
    #' @param p a positive integer providing model's autoregressive lag order.
    #' @param B a logical \code{NxN} matrix containing value \code{TRUE} for the elements of the structural matrix \eqn{B} to be estimated and value \code{FALSE} for exclusion restrictions to be set to zero.
    #' @param stationary an \code{N} logical vector - its element set to \code{FALSE} sets the prior mean for the autoregressive parameters of the \code{N}th equation to the white noise process, otherwise to random walk.
    #' @return A new complete specification for the homoskedastic bsvar model BSVAR.
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
      self$identification  = specify_identification_bsvar$new(N, B)
      self$prior           = specify_prior_bsvar$new(N, p, stationary)
      self$starting_values = specify_starting_values_bsvar$new(N, self$p)
    }, # END initialize
    
    #' @description
    #' Returns the data matrices as the DataMatricesBSVAR object.
    get_data_matrices = function() {
      self$data_matrices$clone()
    }, # END get_data_matrices
    
    #' @description
    #' Returns the identifying restrictions as the IdentificationBSVAR object.
    get_identification = function() {
      self$identification$clone()
    }, # END get_identification
    
    #' @description
    #' Returns the prior specification as the PriorBSVAR object.
    get_prior = function() {
      self$prior$clone()
    }, # END get_prior
    
    #' @description
    #' Returns the starting values as the StartingValuesBSVAR object.
    get_starting_values = function() {
      self$starting_values$clone()
    } # END get_starting_values
  ) # END public
) # END specify_bsvar


# # a simple example
# ############################################################
# NN = 3
# 
# BB      = matrix(FALSE, NN, NN)
# BB[1,1] = BB[2,2] = BB[3,1] = BB[3,3] = TRUE
# 
# T = 100
# y = matrix(rnorm(T * NN), T, NN)
# y = apply(y, 2, cumsum)
# 
# sb                  = specify_bsvar$new(y, 4)
# sb_prior            = sb$prior$get_prior_bsvar()
# sb_starting_values  = sb$starting_values$get_starting_values_bsvar()
# sb_VB               = sb$identification$get_identification_bsvar()
# sb_data             = sb$data_matrices$get_data_matrices()
# 
# sb$get_data_matrices()
# sb$get_prior()
# 
# # this works
# sb$identification$VB
# sb$identification$set_identification_bsvar(NN, BB)
# sb$identification$VB
# 
# # full arguments provided
# sb                  = specify_bsvar$new(y, 1, BB, rep(FALSE, 3))
# sb_prior            = sb$prior$get_prior_bsvar()
# sb_starting_values  = sb$starting_values$get_starting_values_bsvar()
# sb_VB               = sb$identification$get_identification_bsvar()
# sb_data             = sb$data_matrices$get_data_matrices()
# 
# any(class(sb) == "BSVAR")