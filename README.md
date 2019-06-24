# conflictPower <a href='https://crumplab.github.io/conflictPower'><img src='man/figures/logo.png' align="right" height="139" /></a>

## Simulation-based power analysis for adaptive control designs

This package includes functions for conducting statistical power analysis for conflict designs (e.g., Stroop, Flanker, LWPC, ISPC, CSPC, etc.). The approach uses Monte-Carlo simulation. Parameters for variability in base reaction time distributions are used to sample single-trial data from an ex-Gaussian distribution, using the `rexgauss` function from the `retimes` package. The rexgauss function samples n observations from an ex-Gaussian distribution, with parameters mu (mean of a normal distribution), sigma (standard deviation of a normal distribution), and tau (exponent of an exponential distribution). See the article for examples.

## Installation from Github

Make sure you have the devtools package installed.
```
install.packages("devtools")
```

Then, run in the console:

```
devtools::install_github("CrumpLab/conflictPower")
```

## Citation

Crump, M. J. C., & Brosowsky, N. P. (2019). conflictPower: Simulation based power analysis for adaptive control designs. R package version 0.1.0

### Bibtex Entry

```
@Manual{,
    title = {conflictPower: Simulation based power analysis for adaptive control designs},
    author = {Matthew J. C. Crump and Nicholaus P. Brosowsky},
    note = {R package version 0.1.0},
    year = {2019},
  }

```
