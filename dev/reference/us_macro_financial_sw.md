# A 10-variable US macro-financial system for the period 1974-01 – 2025-09

A system used to identify the US monetary policy shock by Shang and
Woźniak (2026). Last data update was implemented on 2026-03-02.

## Usage

``` r
data(us_macro_financial_sw)
```

## Format

A matrix and a `ts` object with over six hundred monthly observations on
10 variables:

- ip:

  industrial production (INDPRO) expressed in 12-month log differences

- p:

  PCE price index (PCEPI) expressed in 12-month log differences

- R:

  Federal Funds Effective Rate

- hhc:

  household credit (real estate and consumer loans) expressed in
  12-month log differences

- bc:

  business credit expressed in 12-month log differences

- GZ:

  BAA minus AAA corporate bond yield spread

- ES:

  spliced commercial paper rate minus 3-month Treasury bill rate

- TS:

  10-year Treasury yield minus 3-month Treasury bill rate

- m:

  M2 monetary aggregate (M2SL) expressed in 12-month log differences

- pcm:

  GFD Bloomberg Commodity Index with extension expressed in 12-month log
  differences of month-end values

## Source

FRED Economic Database, Federal Reserve Bank of St. Louis,
<https://fred.stlouisfed.org/>. Global Financial Data,
<https://globalfinancialdata.com/>.

## References

Shang, F., and Woźniak, T. (2026) Identification Verification for
Structural Vector Autoregressions with Heterogeneous Markov Switching
Heteroskedasticity. University of Melbourne Working Papers,
[doi:10.48550/arXiv.2603.16035](https://doi.org/10.48550/arXiv.2603.16035)
.

## Examples

``` r
data(us_macro_financial_sw)   # upload the data
plot(us_macro_financial_sw)   # plot the data
```
