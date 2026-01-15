
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nds3 <a href="https://sebastian-lortz.github.io/nds3/"><img src="man/figures/logo-comp.png" align="right" height="150" alt-text="nds3 logo"/></a>

<!-- badges:start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges:end -->

We introduce the NDS3 framework: Nonparametric Data Simulation from
Summary Statistics.

## Usage

The method is available as R package and comprehensive ShinyApp.

### Web App

You can use the app at <https://sebastian-lortz.shinyapps.io/nds3/>.
Expect longer computation time compared to running the app locally.

### System Requirements

The `nds3` package was build under R Version 4.4.2 using Apple clang
version 16.0.0 (clang-1600.0.26.6) and GNU Fortran (GCC) 14.2.0. To
compile R from source, install the appropriate toolchain  
- macOS: see <https://mac.r-project.org/tools/>  
- windows: see <https://cran.r-project.org/bin/windows/Rtools/>

### Installation

You can install the latest version of the R package `nds3` like so:

``` r
# install devtools if needed
if (!requireNamespace("devtools")) {install.packages("devtools")}

# install from GitHub
devtools::install_github("sebastian-lortz/nds3")
```

### Run

You can launch the ShinyApp locally by running:

``` r
nds3::run_app()
```

## Citation

Please cite `nds3` if you use it. To cite the software, use:

Lortz SAJ (2025). *nds3: Non-Parametric Data Simulation from Summary
Statistics*. R package version 0.0.1.000,
<https://sebastian-lortz.github.io/nds3/>,
<https://github.com/sebastian-lortz/nds3>.

Or copy the reference information to your BibTeX file:

``` bibtex
@Manual{nds3,
  title        = {nds3: Non-Parametric Data Simulation from Summary Statistics},
  author       = {S. A. J. Lortz},
  year         = {2026},
  note         = {R package version 0.0.1.000},
  url          = {https://github.com/sebastian-lortz/nds3}
}
```

## Code of Conduct

I am open to feedback and new ideas. Please mind the Contributor Code of
Conduct.

## About

You are reading the doc about version: 0.0.1.000

This README has been compiled on 2026-01-15 15:37:27.
