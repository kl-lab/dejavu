`dejavu`
========

The R package ``dejavu`` provides  efficient algorithms for forecasting with Similarity of a bunch of time series.

Installation
------------

You can install  `dejavu` package from [GitHub Repository](https://github.com/kdwang1808/Dejavu) with:

``` r
devtools::install_github("kl-lab/dejavu")
```

Usage
-----

### Load the package

``` r
require("dejavu")
```

### Forecasting with Similarity

####  Using [Reference](https://github.com/kl-lab/dejavu-refdata) data from M3 Competition (Download the repository to local first)

- First download the reference data from GitHub to a folder e.g. `workspace`. Note that
  the reference git repository is [`git-lfs`](https://git-lfs.github.com/) controlled,
  please use a recent version of `git` and `git-lfs` software to clone it.

    ``` sh
    cd workspace
    git clone https://github.com/kl-lab/dejavu-refdata.git
    ```

- Now within R, you switch the work directory to `workspace`

    ```r
    setwd('workspace')
    library(dtw)
    library(robustbase)
    library(forecast)
    fc_Simi <- Similarity(AirPassengers, fh = 20, LoadData = TRUE, path = NULL)
    fcs_result <- ts(fc_Simi$fcs, start = 1961, frequency = 12)
    PIL_result <- ts(fc_Simi$PIL, start = 1961, frequency = 12)
    PIU_result <- ts(fc_Simi$PIU, start = 1961, frequency = 12)
    autoplot(AirPassengers)+autolayer(fcs_result)+autolayer(PIL_result)+autolayer(PIU_result)
  ```

<div align="center">
  <img src="https://github.com/kl-lab/dejavu/blob/master/Forecast_result.png"><br><br>
</div>

#### Using user-defined reference data

Assume you have created a user-defined reference in the folder `my-dejavu-refdata`

  ```R
  fc_Simi <- Similarity(AirPassengers, fh = 20, LoadData = FALSE, path = "my-dejavu-refdata")
  ```


References
----------

- Yanfei Kang, Evangelos Spiliotis, Fotios Petropoulos, Nikolaos Athiniotis, Feng Li, Vassilios
  Assimakopoulos (2020). [Déjà vu: A data-centric forecasting approach through time series cross-similarity](https://doi.org/10.1016/j.jbusres.2020.10.051) Journal of Business Research. [Working paper on arXiv](https://arxiv.org/abs/1909.00221).


License
-------
This package is free and open source software, licensed under GPL-3.
