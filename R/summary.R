
#' @title Provides posterior summary of structural shocks' conditional standard 
#' deviations
#'
#' @description Provides posterior summary of structural shocks' conditional 
#' standard deviations including their mean, standard deviations, as well as 
#' 5 and 95 percentiles.
#' 
#' @param object an object of class PosteriorSigma obtained using the
#' \code{compute_conditional_sd()} function containing posterior draws of 
#' conditional standard deviations of structural shocks.
#' @param ... additional arguments affecting the summary produced.
#' 
#' @return A list reporting the posterior mean, standard deviations, as well as 
#' 5 and 95 percentiles of the structural shocks' conditional standard deviations
#' for each of the shocks and periods.
#' 
#' @method summary PosteriorSigma
#' 
#' @seealso \code{\link{compute_conditional_sd}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @examples
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' set.seed(123)
#' specification  = specify_bsvar_sv$new(us_fiscal_lsuw, p = 1)
#' 
#' # run the burn-in
#' burn_in        = estimate(specification, 10)
#' 
#' # estimate the model
#' posterior      = estimate(burn_in$get_last_draw(), 20, , thin = 1)
#' 
#' # compute structural shocks' conditional standard deviations
#' sigma          = compute_conditional_sd(posterior)
#' sigma_summary  = summary(sigma)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_sv$new(p = 1) |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20, thin = 1) |> 
#'   compute_conditional_sd() |>
#'   summary() -> sigma_summary
#' 
#' @export
summary.PosteriorSigma = function(
    object,
    ...
) {
  
  stopifnot("The model is homoskedastic. Conditional sd is equal to 1 for all variables and periods." = length(dim(object)) == 3)

  posterior_sigma = object
  
  N         = dim(posterior_sigma)[1]
  T         = dim(posterior_sigma)[2]
  
  out       = list()
  for (n in 1:N) {
    out[[n]]    = cbind(
      apply(posterior_sigma[n,,], 1, mean),
      apply(posterior_sigma[n,,], 1, sd),
      t(apply(posterior_sigma[n,,], 1, quantile, probs = c(0.05, 0.95)))
    )
    colnames(out[[n]]) = c("mean", "sd", "5% quantile", "95% quantile")
    rownames(out[[n]]) = 1:T
  } # END n loop
  
  names(out) = paste0("shock", 1:N)
  
  return(out)
} # END summary.PosteriorSigma






#' @title Provides posterior summary of variables' fitted values
#'
#' @description Provides posterior summary of the fitted values including their 
#' mean, standard deviations, as well as 5 and 95 percentiles.
#' 
#' @param object an object of class PosteriorFitted obtained using the
#' \code{compute_fitted_values()} function containing draws the predictive 
#' density of the sample data. 
#' @param ... additional arguments affecting the summary produced.
#' 
#' @return A list reporting the posterior mean, standard deviations, as well as 
#' 5 and 95 percentiles of the fitted values for each of the shocks and periods.
#' 
#' @method summary PosteriorFitted
#' 
#' @seealso \code{\link{compute_fitted_values}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @examples
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' set.seed(123)
#' specification  = specify_bsvar$new(us_fiscal_lsuw, p = 1)
#' 
#' # run the burn-in
#' burn_in        = estimate(specification, 10)
#' 
#' # estimate the model
#' posterior      = estimate(burn_in$get_last_draw(), 20, , thin = 1)
#' 
#' # compute fitted values
#' fitted         = compute_fitted_values(posterior)
#' fitted_summary = summary(fitted)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar$new(p = 1) |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20, thin = 1) |> 
#'   compute_fitted_values() |>
#'   summary() -> fitted_summary
#' 
#' @export
summary.PosteriorFitted = function(
    object,
    ...
) {
  
  N         = dim(object)[1]
  T         = dim(object)[2]
  
  out       = list()
  for (n in 1:N) {
    out[[n]]    = cbind(
      apply(object[n,,], 1, mean),
      apply(object[n,,], 1, sd),
      t(apply(object[n,,], 1, quantile, probs = c(0.05, 0.95)))
    )
    colnames(out[[n]]) = c("mean", "sd", "5% quantile", "95% quantile")
    rownames(out[[n]]) = 1:T
  } # END n loop
  
  names(out) = paste0("variable", 1:N)
  
  return(out)
} # END summary.PosteriorFitted











#' @title Provides posterior summary of historical decompositions
#'
#' @description Provides posterior means of the historical decompositions variable
#' by variable.
#' 
#' @param object an object of class PosteriorHD obtained using the
#' \code{compute_historical_decompositions()} function containing posterior draws
#'  of historical decompositions. 
#' @param ... additional arguments affecting the summary produced.
#' 
#' @return A list reporting the posterior means of historical decompositions for
#' each of the variables.
#' 
#' @method summary PosteriorHD
#' 
#' @seealso \code{\link{compute_historical_decompositions}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @examples
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' set.seed(123)
#' specification  = specify_bsvar$new(diff(us_fiscal_lsuw), p = 1)
#' 
#' # run the burn-in
#' burn_in        = estimate(specification, 10)
#' 
#' # estimate the model
#' posterior      = estimate(burn_in$get_last_draw(), 20, , thin = 1)
#' 
#' # compute historical decompositions
#' hds            = compute_historical_decompositions(posterior)
#' hds_summary    = summary(hds)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' diff(us_fiscal_lsuw) |>
#'   specify_bsvar$new(p = 1) |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20, thin = 1) |> 
#'   compute_historical_decompositions() |>
#'   summary() -> hds_summary
#' 
#' @export
summary.PosteriorHD = function(
    object,
    ...
) {
  
  N         = dim(object)[1]
  T         = dim(object)[3]
  
  out       = list()
  hds       = apply(object, 1:3, mean)
  for (n in 1:N) {
    out[[n]]    = t(hds[n,,])
    colnames(out[[n]]) = paste0("shock ", 1:N)
    rownames(out[[n]]) = 1:T
  } # END n loop
  
  names(out) = paste0("variable", 1:N)
  
  return(out)
} # END summary.PosteriorHD







#' @title Provides posterior summary of impulse responses
#'
#' @description Provides posterior summary of the impulse responses of each 
#' variable to each of the shocks at all horizons. Includes their posterior 
#' means, standard deviations, as well as 5 and 95 percentiles.
#' 
#' @param object an object of class PosteriorIR obtained using the
#' \code{compute_impulse_responses()} function containing draws from the posterior
#' distribution of the impulse responses. 
#' @param ... additional arguments affecting the summary produced.
#' 
#' @return A list reporting the posterior mean, standard deviations, as well as 
#' 5 and 95 percentiles of the impulse responses of each variable to each of the 
#' shocks at all horizons.
#' 
#' @method summary PosteriorIR
#' 
#' @seealso \code{\link{compute_impulse_responses}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @examples
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' set.seed(123)
#' specification  = specify_bsvar$new(us_fiscal_lsuw, p = 1)
#' 
#' # run the burn-in
#' burn_in        = estimate(specification, 10)
#' 
#' # estimate the model
#' posterior      = estimate(burn_in$get_last_draw(), 20, , thin = 1)
#' 
#' # compute impulse responses
#' irf            = compute_impulse_responses(posterior, horizon = 8)
#' irf_summary    = summary(irf)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar$new(p = 1) |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20, thin = 1) |> 
#'   compute_impulse_responses(horizon = 8) |>
#'   summary() -> irf_summary
#' 
#' @export
summary.PosteriorIR = function(
    object,
    ...
) {
  
  N         = dim(object)[1]
  H         = dim(object)[3] - 1
  
  out       = list()
  for (n in 1:N) {
    out[[n]] = list()
    for (i in 1:N) {
      out[[n]][[i]]    = cbind(
        apply(object[i,n,,], 1, mean),
        apply(object[i,n,,], 1, sd),
        t(apply(object[i,n,,], 1, quantile, probs = c(0.05, 0.95)))
      )
      colnames(out[[n]][[i]]) = c("mean", "sd", "5% quantile", "95% quantile")
      rownames(out[[n]][[i]]) = 0:H
    } # END i loop
    names(out[[n]]) = paste0("variable", 1:N)
  } # END n loop
  names(out) = paste0("shock", 1:N)
  
  return(out)
} # END summary.PosteriorIR






#' @title Provides posterior summary of structural shocks' conditional standard 
#' deviations
#'
#' @description Provides posterior summary of structural shocks' conditional 
#' standard deviations including their mean, standard deviations, as well as 
#' 5 and 95 percentiles.
#' 
#' @param object an object of class PosteriorRegimePr obtained using the
#' \code{compute_regime_probabilities()} function containing posterior draws of 
#' conditional standard deviations of structural shocks.
#' @param ... additional arguments affecting the summary produced.
#' 
#' @return A list reporting the posterior mean, standard deviations, as well as 
#' 5 and 95 percentiles of the structural shocks' conditional standard deviations
#' for each of the shocks and periods.
#' 
#' @method summary PosteriorRegimePr
#' 
#' @seealso \code{\link{compute_regime_probabilities}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @examples
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' set.seed(123)
#' specification  = specify_bsvar_msh$new(us_fiscal_lsuw, p = 1)
#' 
#' # run the burn-in
#' burn_in        = estimate(specification, 10)
#' 
#' # estimate the model
#' posterior      = estimate(burn_in$get_last_draw(), 20, , thin = 1)
#' 
#' # compute regime probabilities
#' rp             = compute_regime_probabilities(posterior)
#' rp_summary     = summary(rp)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_msh$new(p = 1) |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20, thin = 1) |> 
#'   compute_regime_probabilities() |>
#'   summary() -> rp_summary
#' 
#' @export
summary.PosteriorRegimePr = function(
    object,
    ...
) {
  
  M         = dim(object)[1]
  T         = dim(object)[2]
  
  out       = list()
  for (m in 1:M) {
    out[[m]]    = cbind(
      apply(object[m,,], 1, mean),
      apply(object[m,,], 1, sd),
      t(apply(object[m,,], 1, quantile, probs = c(0.05, 0.95)))
    )
    colnames(out[[m]]) = c("mean", "sd", "5% quantile", "95% quantile")
    rownames(out[[m]]) = 1:T
  } # END n loop
  
  names(out) = paste0("regime", 1:M)
  
  return(out)
} # END summary.PosteriorRegimePr






#' @title Provides posterior summary of structural shocks
#'
#' @description Provides posterior summary of the structural shocks including their 
#' mean, standard deviations, as well as 5 and 95 percentiles.
#' 
#' @param object an object of class PosteriorShocks obtained using the
#' \code{compute_structural_shocks()} function containing draws the posterior
#' distribution of the structural shocks. 
#' @param ... additional arguments affecting the summary produced.
#' 
#' @return A list reporting the posterior mean, standard deviations, as well as 
#' 5 and 95 percentiles of the structural shocks for each of the equations and periods.
#' 
#' @method summary PosteriorShocks
#' 
#' @seealso \code{\link{compute_structural_shocks}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @examples
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' set.seed(123)
#' specification  = specify_bsvar$new(us_fiscal_lsuw, p = 1)
#' 
#' # run the burn-in
#' burn_in        = estimate(specification, 10)
#' 
#' # estimate the model
#' posterior      = estimate(burn_in$get_last_draw(), 20, , thin = 1)
#' 
#' # compute structural shocks
#' shocks         = compute_structural_shocks(posterior)
#' shocks_summary = summary(shocks)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar$new(p = 1) |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20, thin = 1) |> 
#'   compute_structural_shocks() |>
#'   summary() -> shocks_summary
#' 
#' @export
summary.PosteriorShocks = function(
    object,
    ...
) {
  
  N         = dim(object)[1]
  T         = dim(object)[2]
  
  out       = list()
  for (n in 1:N) {
    out[[n]]    = cbind(
      apply(object[n,,], 1, mean),
      apply(object[n,,], 1, sd),
      t(apply(object[n,,], 1, quantile, probs = c(0.05, 0.95)))
    )
    colnames(out[[n]]) = c("mean", "sd", "5% quantile", "95% quantile")
    rownames(out[[n]]) = 1:T
  } # END n loop
  
  names(out) = paste0("shock", 1:N)
  
  return(out)
} # END summary.PosteriorShocks







#' @title Provides posterior summary of forecast error variance decompositions
#'
#' @description Provides posterior means of the forecast error variance 
#' decompositions of each variable at all horizons.
#' 
#' @param object an object of class PosteriorFEVD obtained using the
#' \code{compute_variance_decompositions()} function containing draws from the 
#' posterior distribution of the forecast error variance decompositions. 
#' @param ... additional arguments affecting the summary produced.
#' 
#' @return A list reporting the posterior mean of the forecast error variance 
#' decompositions of each variable at all horizons.
#' 
#' @method summary PosteriorFEVD
#' 
#' @seealso \code{\link{compute_variance_decompositions}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @examples
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' set.seed(123)
#' specification  = specify_bsvar$new(us_fiscal_lsuw, p = 1)
#' 
#' # run the burn-in
#' burn_in        = estimate(specification, 10)
#' 
#' # estimate the model
#' posterior      = estimate(burn_in$get_last_draw(), 20, , thin = 1)
#' 
#' # compute forecast error variance decompositions
#' fevd           = compute_variance_decompositions(posterior, horizon = 8)
#' fevd_summary   = summary(fevd)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar$new(p = 1) |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20, thin = 1) |> 
#'   compute_variance_decompositions(horizon = 8) |>
#'   summary() -> fevd_summary
#' 
#' @export
summary.PosteriorFEVD = function(
    object,
    ...
) {
  
  cat("Posterior means of the forecast error variance decompositions\n")
  cat("-------------------------------------------------------------\n")
  
  N         = dim(object)[1]
  H         = dim(object)[3] - 1
  
  fevd      = apply(object, 1:3, mean)
  out       = list()
  for (n in 1:N) {
    out[[n]] = t(fevd[n,,])
    colnames(out[[n]]) = paste0("shock", 1:N)
    rownames(out[[n]]) = 0:H
  } # END n loop
  names(out) = paste0("variable", 1:N)
  
  return(out)
} # END summary.PosteriorFEVD
