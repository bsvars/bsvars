# Waggoner & Zha (2003) row signs normalisation of the posterior draws for the structural matrix \\B\\

Normalises the sign of rows of matrix \\B\\ MCMC draws, relative to
matrix `B_benchmark`, provided as the second argument. The implemented
procedure proposed by Waggoner, Zha (2003) normalises the MCMC output in
an optimal way leading to the unimodal posterior. Only normalised MCMC
output is suitable for the computations of the posterior characteristics
of the \\B\\ matrix elements and their functions such as the impulse
response functions and other economically interpretable values.

## Usage

``` r
# S3 method for class 'PosteriorBSVARMIX'
normalise(posterior, B_benchmark = NULL)
```

## Arguments

- posterior:

  posterior estimation outcome of class `PosteriorBSVARMIX` generated
  using the
  [`estimate()`](https://bsvars.org/bsvars/reference/estimate.md)
  function, amongst other draws, the `S` draws from the posterior
  distribution of the `NxN` structural matrix of contemporaneous
  relationships \\B\\. These draws are to be normalised with respect to
  the matrix `B_benchmark`.

- B_benchmark:

  the benchmark `NxN` structural matrix specified by the user to have
  the desired row signs

## Value

An object of the same class as that provided as the input argument
`posterior` containing the posterior draws including the draws of the
normalised structural matrix.

## References

Waggoner, D.F., and Zha, T., (2003) Likelihood Preserving Normalization
in Multiple Equation Models. *Journal of Econometrics*, **114**(2),
329–47,
[doi:10.1016/S0304-4076(03)00087-3](https://doi.org/10.1016/S0304-4076%2803%2900087-3)
.

## See also

[`estimate`](https://bsvars.org/bsvars/reference/estimate.md)

## Author

Tomasz Woźniak <wozniak.tom@pm.me>

## Examples

``` r
specification  = specify_bsvar_mix$new(us_fiscal_lsuw)    # specify the model
#> The identification is set to the default option of lower-triangular structural matrix.
burn_in        = estimate(specification, 5)               # run the burn-in
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-finiteMIX model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
posterior      = estimate(burn_in, 5)                     # estimate the model
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-finiteMIX model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|

# normalise the posterior
BB             = posterior$last_draw$starting_values$B    # get the last draw of B
B_benchmark          = diag((-1) * sign(diag(BB))) %*% BB       # set negative diagonal elements
posterior      = normalise(posterior, B_benchmark)              # draws in posterior are normalised
```
