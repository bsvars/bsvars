

specify_prior_bsvar = R6::R6Class(
  "PriorBSVAR",
  
  private = list(
    # N = 2,
    # p = 1,
    # K = 2 * 1 + 1,
    A          = cbind(diag(2), matrix(0, 2, 1)),
    A_V_inv    = diag(c(kronecker((1)^2, rep(1, 2) ), 1)),
    B_V_inv    = diag(2),
    B_nu       = 2,
    hyper_nu   = 3,
    hyper_a    = 1,
    hyper_V    = 3,
    hyper_S    = 1
  ),
  
  public = list(
    initialize = function(N, p, stationary = rep(FALSE, N)){
      stopifnot("Argument N must be a positive integer number." = N > 0 & N %% 1 == 0)
      stopifnot("Argument p must be a positive integer number." = p > 0 & p %% 1 == 0)
      stopifnot("Argument stationary must be an N-vector." = length(stationary) == N)
      
      # private$N         = N
      # private$p         = p
      
      K                 = N * p + 1
      private$A         = cbind(diag(as.numeric(!stationary)), matrix(0, N, K - N))
      private$A_V_inv   = diag(c(kronecker((1:p)^2, rep(1, N) ), 1))
      private$B_V_inv   = diag(N)
      private$B_nu      = N
      private$hyper_nu  = 3
      private$hyper_a   = 1
      private$hyper_V   = 3
      private$hyper_S   = 1
    },
    
    get_prior_bsvar = function(){
      as.list(private)
    }#,
    # active = list()
  )
)



specify_starting_values_bsvar = R6::R6Class(
  "StartingValuesBSVAR",
  
  private = list(
    # N = 2,
    # p = 1,
    # K = 2 * 1 + 1,
    # T = 3,
    B             = diag(2),
    A             = cbind(diag(runif(2)), matrix(0, 2, 2 * 1 + 1 - 2)),
    hyper         = c(1,rep(1,4)),
    h             = matrix(rnorm(2 * 3, sd = .01), 2, 3),
    rho           = rep(.5, 2),
    omega         = rep(.1, 2),
    S             = matrix(1, 2, 3),
    sigma2_omega  = rep(1, 2),
    s_            = rep(0.05, 2)
  ),
  
  public = list(
    initialize = function(N, p, T){
      stopifnot("Argument N must be a positive integer number." = N > 0 & N %% 1 == 0)
      stopifnot("Argument p must be a positive integer number." = p > 0 & p %% 1 == 0)
      stopifnot("Argument T must be an integer number greater than 2." = T > 2 & T %% 1 == 0)
      
      K                 = N * p + 1
      
      private$B             = diag(N)
      private$A             = cbind(diag(runif(N)), matrix(0, N, K - N))
      private$hyper         = c(1,rep(1,4))
      private$h             = matrix(rnorm(N * T, sd = .01), N, T)
      private$rho           = rep(.5, N)
      private$omega         = rep(.1, N)
      private$S             = matrix(1, N, T)
      private$sigma2_omega  = rep(1, N)
      private$s_            = rep(0.05, N)
    },
    
    get_starting_values_bsvar = function(){
      as.list(private)
    }#,
    # active = list()
  )
)



specify_identification_bsvar = R6::R6Class(
  "IdentificationBSVAR",
  
  private = list(
    VB    = vector("list", 2)
  ),
  
  public = list(
    initialize = function(N, B) {
      if (missing(B)) {
          B     = matrix(FALSE, N, N)
          B[lower.tri(B, diag = TRUE)] = TRUE
      }

      stopifnot("Argument B must be an NxN matrix with logical values." = is.logical(B) & is.matrix(B) & prod(dim(B) == N))
      
      private$VB          <- vector("list", N)
      for (n in 1:N) {
        private$VB[[n]]   <- matrix(diag(N)[B[n,],], ncol = N)
      }
    },
    
    get_identification_bsvar = function() {
      as.list(private$VB)
    },
    
    set_identification_bsvar = function(N, B) {
      if (missing(B)) {
        B     = matrix(FALSE, N, N)
        B[lower.tri(B, diag = TRUE)] = TRUE
      }
      
      stopifnot("Argument B must be an NxN matrix with logical values." = is.logical(B) & is.matrix(B) & prod(dim(B) == N))
      
      private$VB          <- vector("list", N)
      for (n in 1:N) {
        private$VB[[n]]   <- matrix(diag(N)[B[n,],], ncol = N)
      }
    }#,
    # active = list()
  )
)



specify_data_matrices = R6::R6Class(
  "DataMatricesBSVAR",
  
  private = list(
    # p     = 1L,
    Y     = matrix(nrow = 2, ncol = 3),
    X     = matrix(nrow = 3, ncol = 3)
  ),
  
  public = list(
    initialize = function(data, p = 1L) {
      if (missing(data)) {
        stop("Argument data has to be specified")
      } else {
        stopifnot("Argument data has to be a matrix." = is.matrix(data) & is.numeric(data))
        stopifnot("Argument data has to contain at least 2 columns and 3 rows." = (ncol(data) >= 2 & nrow(data) >= 3))
        stopifnot("Argument data cannot include missing values." = sum(is.na(data)) == 0 )
      }
      
      TT            = nrow(data)
      T             = TT - p
      
      private$Y     = t(data[(p + 1):TT,])
      X             = matrix(0, T, 0)
      for (i in 1:p) {
        X           = cbind(X, data[(p + 1):TT - i,])
      }
      private$X     = t(cbind(X, rep(1, T)))
    },
    
    get_data_matrices = function() {as.list(private)}
  )
)


specify_bsvar = R6::R6Class(
  "BSVAR",
  
  private = list(
    # data_matrices   = list(),
    # identification  = specify_identification_bsvar$new(N = 2),
    # prior           = specify_prior_bsvar$new(N = 2, p = 1),
    # starting_values = specify_starting_values_bsvar$new(N = 2, p = 1, T = 3),
    p = 1L
  ),
  
  public = list(
    initialize = function(
      data,
      p = 1L,
      B
    ) {
      TT            = nrow(data)
      T             = TT - private$p
      N             = ncol(data)
      
      stopifnot("Argument p has to be a positive integer." = ((p %% 1) == 0 & p > 0))
      private$p     = p
      
      if (missing(B)) {
        message("The identification is set to the default option of lower-triangular structural matrix.")
        B     = matrix(FALSE, N, N)
        B[lower.tri(B, diag = TRUE)] = TRUE
      }
      stopifnot("Incorrectly specified argument B." = (is.matrix(B) & is.logical(B)) | (length(B) == 1 & is.na(B)))
      
      
      self$data_matrices   = specify_data_matrices$new(data, p)
      self$identification  = specify_identification_bsvar$new(N, B)
      self$prior           = specify_prior_bsvar$new(N, p)
      self$starting_values = specify_starting_values_bsvar$new(N, private$p, T)
    },
    
    data_matrices          = list(),
    
    identification         = list(),
    
    prior                  = list(),
    
    starting_values        = list(),
    
    get_data_matrices = function() {
      private$data_matrices$clone()
    },
    
    get_identification = function() {
      private$identification$clone()
    },
    
    get_prior = function() {
      private$prior$clone()
    },
    
    get_starting_values = function() {
      private$starting_values$clone()
    }
  )#,
  ############################################################
  # active
  ############################################################
  # active = list()
)


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
# sb = specify_bsvar$new(y, 4)
# 
# # this works
# sb$identification$get_identification_bsvar()
# sb$identification$set_identification_bsvar(NN, BB)
# sb$identification$get_identification_bsvar()
# 
# # this does NOT work
# sb_id = sb$identification$clone(deep = TRUE)
# sb_id$show_identification_bsvar()
# sb_id$set_identification_bsvar(NN, BB)
# sb_id$show_identification_bsvar()
# sb$identification$show_identification_bsvar()


