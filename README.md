# BVAtoR

<!-- badges: start -->
  [![Travis build status](https://travis-ci.org/edbonneville/BVAtoR.svg?branch=master)](https://travis-ci.org/edbonneville/BVAtoR)
  <!-- badges: end -->

Minimal package to import electroencephalography (EEG) data from Brain Vision Analyzer (BVA, version >= 2.2) into R. It is meant as a first step before performing single trial EEG analysis. For more information, please read the [vignette](https://edbonneville.github.io/BVAtoR/articles/basic-usage.html).

## Installation

To install the package, run:

```r
devtools::install_github("edbonneville/BVAtoR", dependencies = T)
```

## Additional functionality

The package will not be developed beyond the simple import functionality - only bugs/compatibility issues will be treated. If you need extra functionality beyond data import (e.g. plotting), we highly recommend looking at the following R packages, which are both still under active development:

* [eegUtils](https://github.com/craddm/eegUtils) by [Matt Craddock](https://www.mattcraddock.com/)
* [eeguana](https://github.com/bnicenboim/eeguana/) by [Bruno Nicenboim](https://bnicenboim.github.io/)

## Contributors

| Name                                                         | Affiliation                           | Role         |
| ------------------------------------------------------------ | ------------------------------------- | ------------ |
| Edouard F. Bonneville                                        | Leiden University Medical Center (NL) | Maintainer   |
| [Sarah von Grebmer](https://www.multilingualmind.eu/sarah-von-grebmer-zu-wolfsthurn?fbclid=IwAR3eL2IgqgbKIeGFRZtdNaLHDswC9iraLNxXDrVorQh92mK16iH5NJjLjwE) | Leiden University (NL)                | Collaborator |

