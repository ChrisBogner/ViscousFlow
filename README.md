ViscousFlow
================
Christina Bogner
2020-10-27

<!-- README.md is generated from README.Rmd. Please edit that file -->

## Package aims

<!-- badges: start -->

<!-- badges: end -->

This package analyses laboratory irrigation experiments on soil columns.
It uses the viscous flow approach by Germann (2018).

## Installation

You can install ViscousFlow from GitHub with \`devoolts’ like so:

``` r
# install.packages("devtools")
devtools::install_github("ChrisBogner/ViscousFlow")
```

## Example

The package contains two data sets, namely `tracer` and `drainage`. Both
originate from the publication by Bogner and Germann (2019) (there
called column C1, irrigation intensity 10 mm h<sup>-1</sup>).

This is a basic example which shows you how to solve fit the viscous
flow equation to the decreasing limb of a drainage curve and to
calculate the viscous flow parameters \(F\) and \(L\).

``` r
library(ViscousFlow)

data(drainage)
```

## Refereneces

<div id="refs">

<div id="ref-Bogner2019">

Bogner, Christina, and Peter Germann. 2019. “Viscous Flow Approach to
‘Pushing Out Old Water’ from Undisturbed and Repacked Soil Columns.”
*Vadose Zone Journal* 18 (1): 180168.
<https://doi.org/10.2136/vzj2018.09.0168>.

</div>

<div id="ref-Germann2018">

Germann, Peter. 2018. *Preferential Flow Stokes Approach to Infiltration
and Drainage*. CH: Geographica Bernensia.
<https://doi.org/https://doi.org/10.4480/GB2018.G88>.

</div>

</div>
