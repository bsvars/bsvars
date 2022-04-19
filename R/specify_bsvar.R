
specify_bsvar   = R6::R6Class(
  "BSVAR",
  ############################################################
  # private
  ############################################################
  private = list(
    ..data_matrices   = list(),
    ..VB              = list(),
    ..prior           = list(),
    ..starting_values = list(),
    ..N = 0L,
    ..T = 0L,
    ..p = 0L,
    ..K = 0L
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
      private$..p     = p
      TT              = nrow(data)
      private$..T     = TT - private$..p
      private$..N     = ncol(data)
      private$..K     = private$..N * private$..p + 1
      
      # create data matrices Y and X
      private$..data_matrices$Y = t(data[(p + 1):TT,])
      X                         = matrix(0, private$..T, 0)
      for (i in 1:private$..p) {
        X          = cbind(X, data[(private$..p + 1):TT - i,])
      }
      private$..data_matrices$X = t(cbind(X, rep(1, private$..T)))
      
      # create exclusion restrictions
      private$..VB        = list()
      for (n in 1:private$..N) {
        private$..VB[[n]] = matrix(diag(private$..N)[1:n,], ncol = private$..N)
      }
      
      # create prior hyper-parameters
      private$..prior     = list(
        B_V_inv    = diag(private$..N),
        B_nu       = private$..N,
        A          = cbind(diag(private$..N), matrix(0, private$..N, private$..K - private$..N)),
        A_V_inv    = diag(c(kronecker((1:private$..p)^2, rep(1,private$..N) ), 1)),
        hyper_nu   = 3,
        hyper_a    = 1,
        hyper_V    = 3,
        hyper_S    = 1
      )
      
      # create starting values
      private$..starting_values = list(
        A          = cbind(diag(private$..N), matrix(0, private$..N, private$..K - private$..N)),
        B          = diag(private$..N),
        hyper      = rep(1, 5)
      )
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
