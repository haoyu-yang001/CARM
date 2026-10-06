# CARM

[![CRAN status](https://www.r-pkg.org/badges/version/CARM)](https://CRAN.R-project.org/package=CARM)

**Covariate-Adjusted Adaptive Randomization via Mahalanobis-Distance**

In randomized controlled trials (RCTs), balancing covariates is often one of the most important concerns. `CARM` provides functions to balance covariates and generate allocation sequences by covariate-adjusted Adaptive Randomization via Mahalanobis distance (ARM). The package also supports randomization for multi-arm trials.

## Installation

From CRAN:

```r
install.packages("CARM")
```

Development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("haoyu-yang001/CARM")
```

## Main functions

| Function | Description |
|---|---|
| `ARM()` | Adaptive randomization via Mahalanobis distance (two arms) |
| `ARMM()` | Adaptive randomization via Mahalanobis distance for multi-arm trials |
| `stratum_ARM()` | Stratified ARM |

Example data: `covariate`.

## References

- Qin Y, Li Y, Ma W, Yang H, Hu F (2024). Adaptive randomization via Mahalanobis distance. *Statistica Sinica*. [doi:10.5705/ss.202020.0440](https://doi.org/10.5705/ss.202020.0440)
- Yang H, Qin Y, Wang F, et al. (2023). Balancing covariates in multi-arm trials via adaptive randomization. *Computational Statistics & Data Analysis*. [doi:10.1016/j.csda.2022.107642](https://doi.org/10.1016/j.csda.2022.107642)

## Authors

Fanglu Chen (maintainer), Haoyu Yang, Yichen Qin, Yang Li

## License

GPL (>= 2)
