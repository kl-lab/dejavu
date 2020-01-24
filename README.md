`Dejavu`
========

The R package ``Dejavu`` provides  efficient algorithms for forecasting with Similarity of a bunch of time series.

Installation
------------

You can install  `Dejavu` package from [GitHub Repository](https://github.com/kdwang1808/Dejavu) with:

``` r
devtools::install_github("kdwang1808/Dejavu")
```

Usage
-----

### Load the package

``` r
require("Dejavu")
```

### Forecasting with Similarity

* **Using [Reference](https://github.com/kdwang1808/Reference) data from M3 Competition ** (Download the repository to local first)

  ```R
  fc_Simi <- Similarity(AirPassengers, fh = 20, LoadData = TRUE, path = NULL)
  fcs_result <- ts(fc_Simi$fcs, start = 1961, frequency = 12)
  PIL_result <- ts(fc_Simi$PIL, start = 1961, frequency = 12)
  PIU_result <- ts(fc_Simi$PIU, start = 1961, frequency = 12)
  autoplot(AirPassengers)+autolayer(fcs_result)+autolayer(PIL_result)+autolayer(PIU_result)
  ```

<div align="center">
  <img src="https://github.com/kdwang1808/Dejavu/blob/master/Forecast_result.png"><br><br>
</div>

* **Using "Mydata" as Reference**

  ```R
  fc_Simi <- Similarity(AirPassengers, fh = 20, LoadData = FALSE, path = "Mydata")
  ```

  


References
----------

- Yanfei Kang, Evangelos Spiliotis, Fotios Petropoulos, Nikolaos Athiniotis, Feng Li, Vassilios
  Assimakopoulos (2019). **D´ej\`a vu: forecasting with similarity.** 

  [Working paper on arXiv](https://arxiv.org/abs/1909.00221).


License
-------
This package is free and open source software, licensed under GPL-3.
