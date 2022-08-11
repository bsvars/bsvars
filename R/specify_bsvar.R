

specify_prior_bsvar = R6::R6Class(
  "PriorBSVAR",
  private = list(
    N = 2,
    p = 1,
    K = 2 * 1 + 1,
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
      
      private$N         = N
      private$p         = p
      private$K         = N * p + 1
      
      private$A         = cbind(diag(as.numeric(!stationary)), matrix(0, private$N, private$K - private$N))
      private$A_V_inv   = diag(c(kronecker((1:private$p)^2, rep(1,private$N) ), 1))
      private$B_V_inv   = diag(private$N)
      private$B_nu      = private$N
      private$hyper_nu  = 3
      private$hyper_a   = 1
      private$hyper_V   = 3
      private$hyper_S   = 1
    },
    get_prior_bsvar = function(){
      as.list(private)
    }#,
    # set_A_prior_mean = function(
    #   A_mean = NULL, 
    #   type = c("nonstationary", "stationary")
    # ) {
    #   type <- match.arg(type)
    #   
    #   if (is.null(A_mean)) {
    #     if (type == "nonstationary") {
    #       private$A     = cbind(diag(private$N), matrix(0, private$N, private$K - private$N))
    #     } else if (type == "stationary") {
    #       private$A     = matrix(0, private$N, private$K)
    #     } 
    #   } else {
    #     stopifnot("Argument A_mean must be an NxK matrix where K=N*p+1" = is.matrix(A_mean) & dim(A_mean) == c(private$N, private$K))
    #     stopifnot("Elements of argument A_mean must be finite real numbers" = sum(is.na(A_mean)) == 0 & sum(is.infinite(A_mean)) == 0 )
    #     
    #     private$A       = A_mean
    #   }
    # }
  )#,
  # active = list()
)

specify_bsvar = R6::R6Class(
  "BSVAR",
  ############################################################
  # private
  ############################################################
  private = list(
    data_matrices   = list(),
    VB              = list(),
    prior           = list(),
    starting_values = list(),
    N = 0L,
    T = 0L,
    p = 0L,
    K = 0L
  ),
  ############################################################
  # public
  ############################################################
  public = list(
    # initialize
    ############################################################
    initialize = function(
      data,
      p
    ) {
      if (missing(data)) {
        stop("Argument data has to be specified")
      } else {
        stopifnot("Argument data has to be a matrix or a data.frame." = is.matrix(data) | is.data.frame(data))
        stopifnot("Argument data has to contain at least 2 columns and 2 rows" = (ncol(data) > 2 & nrow(data) > 2))
        stopifnot("Argument data cannot include missing values" = sum(is.na(data)) == 0 )
      }
      if (missing(p)) {
        stop("Argument p denoting the lag order has to be specified")
      } else {
        stopifnot("Argument p has to be a positive integer" = (is.integer(p) & p > 0))
      }
      
      # read dimensions
      private$p     = p
      TT              = nrow(data)
      private$T     = TT - private$p
      private$N     = ncol(data)
      private$K     = private$N * private$p + 1
      
      # create data matrices Y and X
      private$data_matrices$Y = t(data[(p + 1):TT,])
      X                         = matrix(0, private$T, 0)
      for (i in 1:private$p) {
        X          = cbind(X, data[(private$p + 1):TT - i,])
      }
      private$data_matrices$X = t(cbind(X, rep(1, private$T)))
      
      # create exclusion restrictions
      # private$VB        = list()
      # for (n in 1:private$N) {
      #   private$VB[[n]] = matrix(diag(private$N)[1:n,], ncol = private$N)
      # }
      
      # create prior hyper-parameters
      private$prior     = specify_prior_bsvar(private$N, private$N)
      
      # create starting values
      # private$..starting_values = list(
      #   A          = cbind(diag(private$..N), matrix(0, private$..N, private$..K - private$..N)),
      #   B          = diag(private$..N),
      #   hyper      = rep(1, 5)
      # )
    },
    # get_data_matrices
    ############################################################
    get_data_matrices = function() {
      private$..data_matrices
    },
    # get_exlusion_restrictions
    ############################################################
    get_exlusion_restrictions = function() {
      private$..VB
    },
    # get_prior
    ############################################################
    get_prior = function() {
      private$..prior
    },
    # get_starting_values
    ############################################################
    get_starting_values = function() {
      private$..get_starting_values
    }
  )#,
  ############################################################
  # active
  ############################################################
  # active = list()
)

# data(us_monetary_wd)
# sb = specify_bsvar$new(us_monetary_wd[,1:4], 4L)
# sbp = sb$get_prior()
# sbp$hyper_S = 0.1

# pr = specify_prior_bsvar$new(2, 1)
# pr$get_prior_bsvar()
# pr$set_A_prior_mean("nonstationary") # This does not work but does not give error!!!
# pr$set_A_prior_mean(type = "nonstationary")
# pr$set_A_prior_mean(matrix(rnorm(6),2,3))
