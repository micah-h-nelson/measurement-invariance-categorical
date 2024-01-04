Measurement Invariance Testing with Categorical Indicators
================
Micah Nelson
2024-1-4

The classic measurement invariance testing approach assumes indicators
of latent variables are continuous. If indicators are ordinal, the
invariance tests and CFA results can be flawed.

Methodologists have proposed a number of different ways to conduct
categorical MI tests. This example code shows how to use a recent method
proposed by Wu and Estabrook
(<https://doi.org/10.1007/s11336-016-9506-0>) Here, nested models are
compared in the following order: threshold invariance, metric
invariance, scalar invariance, and residual variance

(Note: this method is applicable only when indicators have more than two
categories)

Here, I use the Holzinger and Swineford dataset, which comes with
lavaan. I test measurement invariance of mental ability test scores by
sex

``` r
library(lavaan)
library(semTools)
library(tidyverse)
```

For the sake of the example, split the indicators into quartiles

``` r
data <- HolzingerSwineford1939 |> 
  mutate(visual1 = ntile(x1, 4),
         visual2 = ntile(x2, 4),
         visual3 = ntile(x3, 4),
         textual1 = ntile(x4, 4),
         textual2 = ntile(x5, 4),
         textual3 = ntile(x6, 4),
         speed1 = ntile(x7, 4),
         speed2 = ntile(x8, 4),
         speed3 = ntile(x9, 4),
         )
```

Define model structure

``` r
HS.model <- '
visual =~ visual1 + visual2 + visual3
textual =~ textual1 + textual2 + textual3
speed =~ speed1 + speed2 + speed3
'
```

Create empty matrix to store fit indices from the models we will
estimate

``` r
fit.indices <- matrix(NA, nrow = 5, ncol = 7)
```

Use measEq.syntax() from semTools to generate code for configural model

``` r
configural <- as.character(measEq.syntax(configural.model = HS.model,
                              data = data,
                              ordered = c("visual1", "visual2", "visual3",
                                          "textual1", "textual2", "textual3",
                                          "speed1", "speed2", "speed3"),
                              parameterization = "theta",
                              ID.fac = "std.lv",
                              ID.cat = "Wu.Estabrook.2016",
                              group = "sex",
                              group.equal = "configural"))
```

Estimate model, see results, and add various fit indices to matrix

``` r
fit.configural <- cfa(model = configural, data = data, group = "sex",
                        ordered = c("visual1", "visual2", "visual3",
                                    "textual1", "textual2", "textual3",
                                    "speed1", "speed2", "speed3"),
                        parameterization = "theta")

summary(fit.configural, fit.measures = TRUE)
```

    ## lavaan 0.6.17 ended normally after 119 iterations
    ## 
    ##   Estimator                                       DWLS
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                        78
    ## 
    ##   Number of observations per group:                   
    ##     1                                              146
    ##     2                                              155
    ## 
    ## Model Test User Model:
    ##                                               Standard      Scaled
    ##   Test Statistic                                76.294     111.625
    ##   Degrees of freedom                                48          48
    ##   P-value (Chi-square)                           0.006       0.000
    ##   Scaling correction factor                                  0.755
    ##   Shift parameter                                           10.581
    ##     simple second-order correction                                
    ##   Test statistic for each group:
    ##     1                                           51.902      73.871
    ##     2                                           24.392      37.753
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                              2453.988    1582.732
    ##   Degrees of freedom                                72          72
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  1.577
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.988       0.958
    ##   Tucker-Lewis Index (TLI)                       0.982       0.937
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                         0.914
    ##   Robust Tucker-Lewis Index (TLI)                            0.871
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.063       0.094
    ##   90 Percent confidence interval - lower         0.034       0.071
    ##   90 Percent confidence interval - upper         0.088       0.117
    ##   P-value H_0: RMSEA <= 0.050                    0.205       0.001
    ##   P-value H_0: RMSEA >= 0.080                    0.143       0.854
    ##                                                                   
    ##   Robust RMSEA                                               0.106
    ##   90 Percent confidence interval - lower                     0.074
    ##   90 Percent confidence interval - upper                     0.138
    ##   P-value H_0: Robust RMSEA <= 0.050                         0.004
    ##   P-value H_0: Robust RMSEA >= 0.080                         0.910
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.078       0.078
    ## 
    ## Parameter Estimates:
    ## 
    ##   Parameterization                               Theta
    ##   Standard errors                           Robust.sem
    ##   Information                                 Expected
    ##   Information saturated (h1) model        Unstructured
    ## 
    ## 
    ## Group 1 [1]:
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   visual =~                                           
    ##     visual1 (l.1_)    1.259    0.535    2.355    0.019
    ##     visual2 (l.2_)    0.396    0.125    3.159    0.002
    ##     visual3 (l.3_)    0.584    0.148    3.938    0.000
    ##   textual =~                                          
    ##     textul1 (l.4_)    1.664    0.302    5.514    0.000
    ##     textul2 (l.5_)    1.907    0.427    4.471    0.000
    ##     textul3 (l.6_)    1.597    0.281    5.680    0.000
    ##   speed =~                                            
    ##     speed1  (l.7_)    0.651    0.158    4.127    0.000
    ##     speed2  (l.8_)    1.402    0.466    3.008    0.003
    ##     speed3  (l.9_)    0.822    0.185    4.432    0.000
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   visual ~~                                           
    ##     textul  (p.2_)    0.486    0.103    4.744    0.000
    ##     speed  (p.3_1)    0.418    0.114    3.661    0.000
    ##   textual ~~                                          
    ##     speed  (p.3_2)    0.134    0.110    1.215    0.225
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .visual1 (n.1.)    0.000                           
    ##    .visual2 (n.2.)    0.000                           
    ##    .visual3 (n.3.)    0.000                           
    ##    .textul1 (n.4.)    0.000                           
    ##    .textul2 (n.5.)    0.000                           
    ##    .textul3 (n.6.)    0.000                           
    ##    .speed1  (n.7.)    0.000                           
    ##    .speed2  (n.8.)    0.000                           
    ##    .speed3  (n.9.)    0.000                           
    ##     visual  (a.1.)    0.000                           
    ##     textual (a.2.)    0.000                           
    ##     speed   (a.3.)    0.000                           
    ## 
    ## Thresholds:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##     vsl1|t1 (v1.1)   -1.209    0.346   -3.493    0.000
    ##     vsl1|t2 (v1.2)    0.000    0.167    0.000    1.000
    ##     vsl1|t3 (v1.3)    0.933    0.300    3.112    0.002
    ##     vsl2|t1 (v2.1)   -0.761    0.124   -6.144    0.000
    ##     vsl2|t2 (v2.2)    0.000    0.112    0.000    1.000
    ##     vsl2|t3 (v2.3)    0.560    0.118    4.742    0.000
    ##     vsl3|t1 (v3.1)   -0.980    0.143   -6.831    0.000
    ##     vsl3|t2 (v3.2)   -0.261    0.122   -2.144    0.032
    ##     vsl3|t3 (v3.3)    0.535    0.127    4.214    0.000
    ##     txtl1|1 (t1.1)   -1.088    0.218   -4.987    0.000
    ##     txtl1|2 (t1.2)    0.167    0.205    0.813    0.416
    ##     txtl1|3 (t1.3)    1.550    0.284    5.452    0.000
    ##     txtl2|1 (t2.1)   -1.294    0.296   -4.378    0.000
    ##     txtl2|2 (t2.2)    0.074    0.226    0.328    0.743
    ##     txtl2|3 (t2.3)    1.619    0.344    4.712    0.000
    ##     txtl3|1 (t3.1)   -1.251    0.219   -5.715    0.000
    ##     txtl3|2 (t3.2)   -0.032    0.196   -0.165    0.869
    ##     txtl3|3 (t3.3)    1.251    0.246    5.091    0.000
    ##     spd1|t1 (s1.1)   -0.742    0.141   -5.265    0.000
    ##     spd1|t2 (s1.2)    0.123    0.125    0.988    0.323
    ##     spd1|t3 (s1.3)    0.981    0.151    6.488    0.000
    ##     spd2|t1 (s2.1)   -1.143    0.317   -3.608    0.000
    ##     spd2|t2 (s2.2)    0.000    0.179    0.000    1.000
    ##     spd2|t3 (s2.3)    0.930    0.265    3.506    0.000
    ##     spd3|t1 (s3.1)   -0.887    0.160   -5.534    0.000
    ##     spd3|t2 (s3.2)    0.044    0.135    0.330    0.741
    ##     spd3|t3 (s3.3)    0.973    0.164    5.951    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .visual1 (t.1_)    1.000                           
    ##    .visual2 (t.2_)    1.000                           
    ##    .visual3 (t.3_)    1.000                           
    ##    .textul1 (t.4_)    1.000                           
    ##    .textul2 (t.5_)    1.000                           
    ##    .textul3 (t.6_)    1.000                           
    ##    .speed1  (t.7_)    1.000                           
    ##    .speed2  (t.8_)    1.000                           
    ##    .speed3  (t.9_)    1.000                           
    ##     visual  (p.1_)    1.000                           
    ##     textual (p.2_)    1.000                           
    ##     speed   (p.3_)    1.000                           
    ## 
    ## Scales y*:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##     visual1           0.622                           
    ##     visual2           0.930                           
    ##     visual3           0.864                           
    ##     textual1          0.515                           
    ##     textual2          0.464                           
    ##     textual3          0.531                           
    ##     speed1            0.838                           
    ##     speed2            0.581                           
    ##     speed3            0.773                           
    ## 
    ## 
    ## Group 2 [2]:
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   visual =~                                           
    ##     visual1 (l.1_)    1.357    0.375    3.616    0.000
    ##     visual2 (l.2_)    0.719    0.152    4.735    0.000
    ##     visual3 (l.3_)    0.518    0.129    4.020    0.000
    ##   textual =~                                          
    ##     textul1 (l.4_)    1.600    0.278    5.753    0.000
    ##     textul2 (l.5_)    1.965    0.384    5.116    0.000
    ##     textul3 (l.6_)    1.955    0.384    5.092    0.000
    ##   speed =~                                            
    ##     speed1  (l.7_)    0.582    0.144    4.029    0.000
    ##     speed2  (l.8_)    0.642    0.142    4.530    0.000
    ##     speed3  (l.9_)    3.149    3.297    0.955    0.340
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   visual ~~                                           
    ##     textul  (p.2_)    0.598    0.081    7.347    0.000
    ##     speed  (p.3_1)    0.562    0.097    5.799    0.000
    ##   textual ~~                                          
    ##     speed  (p.3_2)    0.379    0.096    3.959    0.000
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .visual1 (n.1.)    0.000                           
    ##    .visual2 (n.2.)    0.000                           
    ##    .visual3 (n.3.)    0.000                           
    ##    .textul1 (n.4.)    0.000                           
    ##    .textul2 (n.5.)    0.000                           
    ##    .textul3 (n.6.)    0.000                           
    ##    .speed1  (n.7.)    0.000                           
    ##    .speed2  (n.8.)    0.000                           
    ##    .speed3  (n.9.)    0.000                           
    ##     visual  (a.1.)    0.000                           
    ##     textual (a.2.)    0.000                           
    ##     speed   (a.3.)    0.000                           
    ## 
    ## Thresholds:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##     vsl1|t1 (v1.1)   -0.995    0.230   -4.330    0.000
    ##     vsl1|t2 (v1.2)    0.014    0.170    0.080    0.936
    ##     vsl1|t3 (v1.3)    1.305    0.301    4.342    0.000
    ##     vsl2|t1 (v2.1)   -0.775    0.139   -5.586    0.000
    ##     vsl2|t2 (v2.2)    0.010    0.124    0.080    0.936
    ##     vsl2|t3 (v2.3)    1.036    0.151    6.859    0.000
    ##     vsl3|t1 (v3.1)   -0.580    0.120   -4.820    0.000
    ##     vsl3|t2 (v3.2)    0.248    0.116    2.142    0.032
    ##     vsl3|t3 (v3.3)    1.028    0.137    7.486    0.000
    ##     txtl1|1 (t1.1)   -1.461    0.255   -5.723    0.000
    ##     txtl1|2 (t1.2)   -0.137    0.191   -0.720    0.472
    ##     txtl1|3 (t1.3)    1.078    0.225    4.795    0.000
    ##     txtl2|1 (t2.1)   -1.612    0.320   -5.045    0.000
    ##     txtl2|2 (t2.2)   -0.053    0.222   -0.240    0.810
    ##     txtl2|3 (t2.3)    1.344    0.308    4.364    0.000
    ##     txtl3|1 (t3.1)   -1.470    0.310   -4.742    0.000
    ##     txtl3|2 (t3.2)    0.053    0.222    0.240    0.810
    ##     txtl3|3 (t3.3)    1.515    0.319    4.751    0.000
    ##     spd1|t1 (s1.1)   -0.822    0.134   -6.125    0.000
    ##     spd1|t2 (s1.2)   -0.103    0.117   -0.878    0.380
    ##     spd1|t3 (s1.3)    0.639    0.125    5.110    0.000
    ##     spd2|t1 (s2.1)   -0.796    0.136   -5.854    0.000
    ##     spd2|t2 (s2.2)    0.010    0.120    0.080    0.936
    ##     spd2|t3 (s2.3)    0.973    0.141    6.902    0.000
    ##     spd3|t1 (s3.1)   -2.146    2.024   -1.060    0.289
    ##     spd3|t2 (s3.2)   -0.080    0.348   -0.231    0.818
    ##     spd3|t3 (s3.3)    2.015    1.930    1.044    0.296
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .visual1 (t.1_)    1.000                           
    ##    .visual2 (t.2_)    1.000                           
    ##    .visual3 (t.3_)    1.000                           
    ##    .textul1 (t.4_)    1.000                           
    ##    .textul2 (t.5_)    1.000                           
    ##    .textul3 (t.6_)    1.000                           
    ##    .speed1  (t.7_)    1.000                           
    ##    .speed2  (t.8_)    1.000                           
    ##    .speed3  (t.9_)    1.000                           
    ##     visual  (p.1_)    1.000                           
    ##     textual (p.2_)    1.000                           
    ##     speed   (p.3_)    1.000                           
    ## 
    ## Scales y*:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##     visual1           0.593                           
    ##     visual2           0.812                           
    ##     visual3           0.888                           
    ##     textual1          0.530                           
    ##     textual2          0.454                           
    ##     textual3          0.455                           
    ##     speed1            0.864                           
    ##     speed2            0.841                           
    ##     speed3            0.303

``` r
fit.indices[1,] <- round(data.matrix(fitmeasures(fit.configural,
                                                     fit.measures = c(
                                                       "chisq.scaled", "df.scaled",
                                                       "pvalue.scaled", "rmsea.scaled",
                                                       "cfi.scaled", "tli.scaled",
                                                       "srmr"))), digits = 3)
```

Next step is to estimate the same model with thresholds in both groups
set to be equal

``` r
thresholds <- as.character(measEq.syntax(configural.model = HS.model,
                                         data = data,
                                         ordered = c("visual1", "visual2", "visual3",
                                                     "textual1", "textual2", "textual3",
                                                     "speed1", "speed2", "speed3"),
                                         parameterization = "theta",
                                         ID.fac = "std.lv",
                                         ID.cat = "Wu.Estabrook.2016",
                                         group = "sex",
                                         group.equal = "thresholds"))
```

Estimate model, see results, and add fit indices to matrix

``` r
fit.thresholds <- cfa(model = thresholds, data = data, group = "sex",
                      ordered = c("visual1", "visual2", "visual3",
                                  "textual1", "textual2", "textual3",
                                  "speed1", "speed2", "speed3"),
                      parameterization = "theta")

summary(fit.thresholds, fit.measures = TRUE)
```

    ## lavaan 0.6.17 ended normally after 116 iterations
    ## 
    ##   Estimator                                       DWLS
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                        96
    ##   Number of equality constraints                    27
    ## 
    ##   Number of observations per group:                   
    ##     1                                              146
    ##     2                                              155
    ## 
    ## Model Test User Model:
    ##                                               Standard      Scaled
    ##   Test Statistic                                79.124     124.298
    ##   Degrees of freedom                                57          57
    ##   P-value (Chi-square)                           0.028       0.000
    ##   Scaling correction factor                                  0.711
    ##   Shift parameter                                           13.065
    ##     simple second-order correction                                
    ##   Test statistic for each group:
    ##     1                                           53.510      81.562
    ##     2                                           25.614      42.736
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                              2453.988    1582.732
    ##   Degrees of freedom                                72          72
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  1.577
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.991       0.955
    ##   Tucker-Lewis Index (TLI)                       0.988       0.944
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                            NA
    ##   Robust Tucker-Lewis Index (TLI)                               NA
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.051       0.089
    ##   90 Percent confidence interval - lower         0.018       0.068
    ##   90 Percent confidence interval - upper         0.076       0.110
    ##   P-value H_0: RMSEA <= 0.050                    0.454       0.002
    ##   P-value H_0: RMSEA >= 0.080                    0.028       0.766
    ##                                                                   
    ##   Robust RMSEA                                                  NA
    ##   90 Percent confidence interval - lower                        NA
    ##   90 Percent confidence interval - upper                        NA
    ##   P-value H_0: Robust RMSEA <= 0.050                            NA
    ##   P-value H_0: Robust RMSEA >= 0.080                            NA
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.078       0.078
    ## 
    ## Parameter Estimates:
    ## 
    ##   Parameterization                               Theta
    ##   Standard errors                           Robust.sem
    ##   Information                                 Expected
    ##   Information saturated (h1) model        Unstructured
    ## 
    ## 
    ## Group 1 [1]:
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   visual =~                                           
    ##     visual1 (l.1_)    1.260    0.535    2.355    0.019
    ##     visual2 (l.2_)    0.396    0.125    3.159    0.002
    ##     visual3 (l.3_)    0.584    0.148    3.938    0.000
    ##   textual =~                                          
    ##     textul1 (l.4_)    1.664    0.302    5.514    0.000
    ##     textul2 (l.5_)    1.907    0.427    4.471    0.000
    ##     textul3 (l.6_)    1.597    0.281    5.680    0.000
    ##   speed =~                                            
    ##     speed1  (l.7_)    0.651    0.158    4.127    0.000
    ##     speed2  (l.8_)    1.401    0.466    3.009    0.003
    ##     speed3  (l.9_)    0.822    0.185    4.432    0.000
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   visual ~~                                           
    ##     textul  (p.2_)    0.486    0.103    4.744    0.000
    ##     speed  (p.3_1)    0.418    0.114    3.661    0.000
    ##   textual ~~                                          
    ##     speed  (p.3_2)    0.134    0.110    1.215    0.225
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .visual1 (n.1.)    0.000                           
    ##    .visual2 (n.2.)    0.000                           
    ##    .visual3 (n.3.)    0.000                           
    ##    .textul1 (n.4.)    0.000                           
    ##    .textul2 (n.5.)    0.000                           
    ##    .textul3 (n.6.)    0.000                           
    ##    .speed1  (n.7.)    0.000                           
    ##    .speed2  (n.8.)    0.000                           
    ##    .speed3  (n.9.)    0.000                           
    ##     visual  (a.1.)    0.000                           
    ##     textual (a.2.)    0.000                           
    ##     speed   (a.3.)    0.000                           
    ## 
    ## Thresholds:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##     vsl1|t1 (v1.1)   -1.154    0.333   -3.471    0.001
    ##     vsl1|t2 (v1.2)   -0.089    0.156   -0.571    0.568
    ##     vsl1|t3 (v1.3)    0.984    0.310    3.170    0.002
    ##     vsl2|t1 (v2.1)   -0.717    0.121   -5.924    0.000
    ##     vsl2|t2 (v2.2)   -0.072    0.104   -0.689    0.491
    ##     vsl2|t3 (v2.3)    0.598    0.119    5.041    0.000
    ##     vsl3|t1 (v3.1)   -0.994    0.142   -7.004    0.000
    ##     vsl3|t2 (v3.2)   -0.239    0.113   -2.118    0.034
    ##     vsl3|t3 (v3.3)    0.524    0.126    4.155    0.000
    ##     txtl1|1 (t1.1)   -1.109    0.218   -5.086    0.000
    ##     txtl1|2 (t1.2)    0.205    0.193    1.065    0.287
    ##     txtl1|3 (t1.3)    1.526    0.279    5.461    0.000
    ##     txtl2|1 (t2.1)   -1.325    0.296   -4.482    0.000
    ##     txtl2|2 (t2.2)    0.128    0.211    0.605    0.545
    ##     txtl2|3 (t2.3)    1.586    0.340    4.672    0.000
    ##     txtl3|1 (t3.1)   -1.262    0.219   -5.772    0.000
    ##     txtl3|2 (t3.2)   -0.013    0.182   -0.072    0.943
    ##     txtl3|3 (t3.3)    1.239    0.242    5.119    0.000
    ##     spd1|t1 (s1.1)   -0.739    0.139   -5.298    0.000
    ##     spd1|t2 (s1.2)    0.118    0.117    1.016    0.310
    ##     spd1|t3 (s1.3)    0.984    0.150    6.575    0.000
    ##     spd2|t1 (s2.1)   -1.096    0.308   -3.558    0.000
    ##     spd2|t2 (s2.2)   -0.078    0.168   -0.461    0.645
    ##     spd2|t3 (s2.3)    0.973    0.271    3.591    0.000
    ##     spd3|t1 (s3.1)   -0.886    0.159   -5.566    0.000
    ##     spd3|t2 (s3.2)    0.042    0.126    0.335    0.738
    ##     spd3|t3 (s3.3)    0.975    0.162    6.031    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .visual1 (t.1_)    1.000                           
    ##    .visual2 (t.2_)    1.000                           
    ##    .visual3 (t.3_)    1.000                           
    ##    .textul1 (t.4_)    1.000                           
    ##    .textul2 (t.5_)    1.000                           
    ##    .textul3 (t.6_)    1.000                           
    ##    .speed1  (t.7_)    1.000                           
    ##    .speed2  (t.8_)    1.000                           
    ##    .speed3  (t.9_)    1.000                           
    ##     visual  (p.1_)    1.000                           
    ##     textual (p.2_)    1.000                           
    ##     speed   (p.3_)    1.000                           
    ## 
    ## Scales y*:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##     visual1           0.622                           
    ##     visual2           0.930                           
    ##     visual3           0.864                           
    ##     textual1          0.515                           
    ##     textual2          0.464                           
    ##     textual3          0.531                           
    ##     speed1            0.838                           
    ##     speed2            0.581                           
    ##     speed3            0.773                           
    ## 
    ## 
    ## Group 2 [2]:
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   visual =~                                           
    ##     visual1 (l.1_)    1.263    0.385    3.281    0.001
    ##     visual2 (l.2_)    0.523    0.109    4.779    0.000
    ##     visual3 (l.3_)    0.488    0.125    3.918    0.000
    ##   textual =~                                          
    ##     textul1 (l.4_)    1.662    0.293    5.670    0.000
    ##     textul2 (l.5_)    1.936    0.393    4.920    0.000
    ##     textul3 (l.6_)    1.638    0.270    6.059    0.000
    ##   speed =~                                            
    ##     speed1  (l.7_)    0.686    0.172    3.993    0.000
    ##     speed2  (l.8_)    0.752    0.229    3.278    0.001
    ##     speed3  (l.9_)    1.408    0.266    5.300    0.000
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   visual ~~                                           
    ##     textul  (p.2_)    0.598    0.081    7.347    0.000
    ##     speed  (p.3_1)    0.562    0.097    5.799    0.000
    ##   textual ~~                                          
    ##     speed  (p.3_2)    0.379    0.096    3.959    0.000
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .visual1 (n.1.)   -0.182    0.202   -0.899    0.369
    ##    .visual2 (n.2.)   -0.125    0.125   -1.006    0.314
    ##    .visual3 (n.3.)   -0.456    0.144   -3.158    0.002
    ##    .textul1 (n.4.)    0.385    0.253    1.524    0.128
    ##    .textul2 (n.5.)    0.232    0.275    0.846    0.398
    ##    .textul3 (n.6.)   -0.040    0.233   -0.174    0.862
    ##    .speed1  (n.7.)    0.234    0.161    1.449    0.147
    ##    .speed2  (n.8.)   -0.137    0.201   -0.680    0.497
    ##    .speed3  (n.9.)    0.075    0.173    0.434    0.664
    ##     visual  (a.1.)    0.000                           
    ##     textual (a.2.)    0.000                           
    ##     speed   (a.3.)    0.000                           
    ## 
    ## Thresholds:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##     vsl1|t1 (v1.1)   -1.154    0.333   -3.471    0.001
    ##     vsl1|t2 (v1.2)   -0.089    0.156   -0.571    0.568
    ##     vsl1|t3 (v1.3)    0.984    0.310    3.170    0.002
    ##     vsl2|t1 (v2.1)   -0.717    0.121   -5.924    0.000
    ##     vsl2|t2 (v2.2)   -0.072    0.104   -0.689    0.491
    ##     vsl2|t3 (v2.3)    0.598    0.119    5.041    0.000
    ##     vsl3|t1 (v3.1)   -0.994    0.142   -7.004    0.000
    ##     vsl3|t2 (v3.2)   -0.239    0.113   -2.118    0.034
    ##     vsl3|t3 (v3.3)    0.524    0.126    4.155    0.000
    ##     txtl1|1 (t1.1)   -1.109    0.218   -5.086    0.000
    ##     txtl1|2 (t1.2)    0.205    0.193    1.065    0.287
    ##     txtl1|3 (t1.3)    1.526    0.279    5.461    0.000
    ##     txtl2|1 (t2.1)   -1.325    0.296   -4.482    0.000
    ##     txtl2|2 (t2.2)    0.128    0.211    0.605    0.545
    ##     txtl2|3 (t2.3)    1.586    0.340    4.672    0.000
    ##     txtl3|1 (t3.1)   -1.262    0.219   -5.772    0.000
    ##     txtl3|2 (t3.2)   -0.013    0.182   -0.072    0.943
    ##     txtl3|3 (t3.3)    1.239    0.242    5.119    0.000
    ##     spd1|t1 (s1.1)   -0.739    0.139   -5.298    0.000
    ##     spd1|t2 (s1.2)    0.118    0.117    1.016    0.310
    ##     spd1|t3 (s1.3)    0.984    0.150    6.575    0.000
    ##     spd2|t1 (s2.1)   -1.096    0.308   -3.558    0.000
    ##     spd2|t2 (s2.2)   -0.078    0.168   -0.461    0.645
    ##     spd2|t3 (s2.3)    0.973    0.271    3.591    0.000
    ##     spd3|t1 (s3.1)   -0.886    0.159   -5.566    0.000
    ##     spd3|t2 (s3.2)    0.042    0.126    0.335    0.738
    ##     spd3|t3 (s3.3)    0.975    0.162    6.031    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .visual1 (t.1_)    0.867    0.568    1.527    0.127
    ##    .visual2 (t.2_)    0.529    0.155    3.410    0.001
    ##    .visual3 (t.3_)    0.889    0.258    3.446    0.001
    ##    .textul1 (t.4_)    1.079    0.403    2.674    0.007
    ##    .textul2 (t.5_)    0.971    0.462    2.103    0.035
    ##    .textul3 (t.6_)    0.702    0.289    2.434    0.015
    ##    .speed1  (t.7_)    1.390    0.426    3.265    0.001
    ##    .speed2  (t.8_)    1.370    0.699    1.962    0.050
    ##    .speed3  (t.9_)    0.200    0.379    0.528    0.597
    ##     visual  (p.1_)    1.000                           
    ##     textual (p.2_)    1.000                           
    ##     speed   (p.3_)    1.000                           
    ## 
    ## Scales y*:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##     visual1           0.637                           
    ##     visual2           1.117                           
    ##     visual3           0.942                           
    ##     textual1          0.510                           
    ##     textual2          0.460                           
    ##     textual3          0.543                           
    ##     speed1            0.733                           
    ##     speed2            0.719                           
    ##     speed3            0.677

``` r
fit.indices[2,] <- round(data.matrix(fitmeasures(fit.thresholds,
                                                 fit.measures = c(
                                                   "chisq.scaled", "df.scaled",
                                                   "pvalue.scaled", "rmsea.scaled",
                                                   "cfi.scaled", "tli.scaled",
                                                   "srmr"))), digits = 3)
```

Next step is to constrain thresholds and factor loadings to test metric
invariance

``` r
metric <- as.character(measEq.syntax(configural.model = HS.model,
                                         data = data,
                                         ordered = c("visual1", "visual2", "visual3",
                                                     "textual1", "textual2", "textual3",
                                                     "speed1", "speed2", "speed3"),
                                         parameterization = "theta",
                                         ID.fac = "std.lv",
                                         ID.cat = "Wu.Estabrook.2016",
                                         group = "sex",
                                         group.equal = c("thresholds", "loadings")))
```

Estimate model, see results, and add fit indices to matrix

``` r
fit.metric <- cfa(model = metric, data = data, group = "sex",
                      ordered = c("visual1", "visual2", "visual3",
                                  "textual1", "textual2", "textual3",
                                  "speed1", "speed2", "speed3"),
                      parameterization = "theta")

summary(fit.metric, fit.measures = TRUE)
```

    ## lavaan 0.6.17 ended normally after 120 iterations
    ## 
    ##   Estimator                                       DWLS
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                        99
    ##   Number of equality constraints                    36
    ## 
    ##   Number of observations per group:                   
    ##     1                                              146
    ##     2                                              155
    ## 
    ## Model Test User Model:
    ##                                               Standard      Scaled
    ##   Test Statistic                                89.791     130.679
    ##   Degrees of freedom                                63          63
    ##   P-value (Chi-square)                           0.015       0.000
    ##   Scaling correction factor                                  0.776
    ##   Shift parameter                                           14.985
    ##     simple second-order correction                                
    ##   Test statistic for each group:
    ##     1                                           60.112      84.722
    ##     2                                           29.678      45.957
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                              2453.988    1582.732
    ##   Degrees of freedom                                72          72
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  1.577
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.989       0.955
    ##   Tucker-Lewis Index (TLI)                       0.987       0.949
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                            NA
    ##   Robust Tucker-Lewis Index (TLI)                               NA
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.053       0.085
    ##   90 Percent confidence interval - lower         0.024       0.064
    ##   90 Percent confidence interval - upper         0.077       0.105
    ##   P-value H_0: RMSEA <= 0.050                    0.393       0.004
    ##   P-value H_0: RMSEA >= 0.080                    0.031       0.665
    ##                                                                   
    ##   Robust RMSEA                                                  NA
    ##   90 Percent confidence interval - lower                        NA
    ##   90 Percent confidence interval - upper                        NA
    ##   P-value H_0: Robust RMSEA <= 0.050                            NA
    ##   P-value H_0: Robust RMSEA >= 0.080                            NA
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.080       0.080
    ## 
    ## Parameter Estimates:
    ## 
    ##   Parameterization                               Theta
    ##   Standard errors                           Robust.sem
    ##   Information                                 Expected
    ##   Information saturated (h1) model        Unstructured
    ## 
    ## 
    ## Group 1 [1]:
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   visual =~                                           
    ##     visual1 (l.1_)    1.185    0.365    3.249    0.001
    ##     visual2 (l.2_)    0.456    0.101    4.525    0.000
    ##     visual3 (l.3_)    0.541    0.110    4.893    0.000
    ##   textual =~                                          
    ##     textul1 (l.4_)    1.656    0.274    6.036    0.000
    ##     textul2 (l.5_)    1.915    0.384    4.991    0.000
    ##     textul3 (l.6_)    1.598    0.256    6.252    0.000
    ##   speed =~                                            
    ##     speed1  (l.7_)    0.631    0.125    5.042    0.000
    ##     speed2  (l.8_)    0.805    0.139    5.783    0.000
    ##     speed3  (l.9_)    1.379    0.413    3.334    0.001
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   visual ~~                                           
    ##     textul  (p.2_)    0.496    0.101    4.901    0.000
    ##     speed  (p.3_1)    0.455    0.113    4.040    0.000
    ##   textual ~~                                          
    ##     speed  (p.3_2)    0.145    0.109    1.332    0.183
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .visual1 (n.1.)    0.000                           
    ##    .visual2 (n.2.)    0.000                           
    ##    .visual3 (n.3.)    0.000                           
    ##    .textul1 (n.4.)    0.000                           
    ##    .textul2 (n.5.)    0.000                           
    ##    .textul3 (n.6.)    0.000                           
    ##    .speed1  (n.7.)    0.000                           
    ##    .speed2  (n.8.)    0.000                           
    ##    .speed3  (n.9.)    0.000                           
    ##     visual  (a.1.)    0.000                           
    ##     textual (a.2.)    0.000                           
    ##     speed   (a.3.)    0.000                           
    ## 
    ## Thresholds:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##     vsl1|t1 (v1.1)   -1.109    0.267   -4.159    0.000
    ##     vsl1|t2 (v1.2)   -0.086    0.150   -0.576    0.565
    ##     vsl1|t3 (v1.3)    0.945    0.257    3.671    0.000
    ##     vsl2|t1 (v2.1)   -0.692    0.125   -5.555    0.000
    ##     vsl2|t2 (v2.2)   -0.075    0.105   -0.712    0.476
    ##     vsl2|t3 (v2.3)    0.576    0.123    4.678    0.000
    ##     vsl3|t1 (v3.1)   -1.015    0.138   -7.356    0.000
    ##     vsl3|t2 (v3.2)   -0.236    0.112   -2.114    0.034
    ##     vsl3|t3 (v3.3)    0.548    0.125    4.378    0.000
    ##     txtl1|1 (t1.1)   -1.113    0.216   -5.154    0.000
    ##     txtl1|2 (t1.2)    0.205    0.192    1.065    0.287
    ##     txtl1|3 (t1.3)    1.529    0.278    5.495    0.000
    ##     txtl2|1 (t2.1)   -1.329    0.296   -4.491    0.000
    ##     txtl2|2 (t2.2)    0.128    0.211    0.607    0.544
    ##     txtl2|3 (t2.3)    1.591    0.338    4.705    0.000
    ##     txtl3|1 (t3.1)   -1.255    0.219   -5.718    0.000
    ##     txtl3|2 (t3.2)   -0.013    0.182   -0.071    0.943
    ##     txtl3|3 (t3.3)    1.232    0.243    5.070    0.000
    ##     spd1|t1 (s1.1)   -0.712    0.137   -5.200    0.000
    ##     spd1|t2 (s1.2)    0.117    0.115    1.019    0.308
    ##     spd1|t3 (s1.3)    0.953    0.146    6.538    0.000
    ##     spd2|t1 (s2.1)   -0.926    0.155   -5.973    0.000
    ##     spd2|t2 (s2.2)   -0.052    0.126   -0.412    0.680
    ##     spd2|t3 (s2.3)    0.822    0.146    5.622    0.000
    ##     spd3|t1 (s3.1)   -1.051    0.285   -3.682    0.000
    ##     spd3|t2 (s3.2)    0.055    0.163    0.337    0.736
    ##     spd3|t3 (s3.3)    1.164    0.300    3.881    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .visual1 (t.1_)    1.000                           
    ##    .visual2 (t.2_)    1.000                           
    ##    .visual3 (t.3_)    1.000                           
    ##    .textul1 (t.4_)    1.000                           
    ##    .textul2 (t.5_)    1.000                           
    ##    .textul3 (t.6_)    1.000                           
    ##    .speed1  (t.7_)    1.000                           
    ##    .speed2  (t.8_)    1.000                           
    ##    .speed3  (t.9_)    1.000                           
    ##     visual  (p.1_)    1.000                           
    ##     textual (p.2_)    1.000                           
    ##     speed   (p.3_)    1.000                           
    ## 
    ## Scales y*:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##     visual1           0.645                           
    ##     visual2           0.910                           
    ##     visual3           0.880                           
    ##     textual1          0.517                           
    ##     textual2          0.463                           
    ##     textual3          0.530                           
    ##     speed1            0.846                           
    ##     speed2            0.779                           
    ##     speed3            0.587                           
    ## 
    ## 
    ## Group 2 [2]:
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   visual =~                                           
    ##     visual1 (l.1_)    1.185    0.365    3.249    0.001
    ##     visual2 (l.2_)    0.456    0.101    4.525    0.000
    ##     visual3 (l.3_)    0.541    0.110    4.893    0.000
    ##   textual =~                                          
    ##     textul1 (l.4_)    1.656    0.274    6.036    0.000
    ##     textul2 (l.5_)    1.915    0.384    4.991    0.000
    ##     textul3 (l.6_)    1.598    0.256    6.252    0.000
    ##   speed =~                                            
    ##     speed1  (l.7_)    0.631    0.125    5.042    0.000
    ##     speed2  (l.8_)    0.805    0.139    5.783    0.000
    ##     speed3  (l.9_)    1.379    0.413    3.334    0.001
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   visual ~~                                           
    ##     textul  (p.2_)    0.619    0.164    3.783    0.000
    ##     speed  (p.3_1)    0.602    0.177    3.403    0.001
    ##   textual ~~                                          
    ##     speed  (p.3_2)    0.395    0.132    2.989    0.003
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .visual1 (n.1.)   -0.175    0.193   -0.906    0.365
    ##    .visual2 (n.2.)   -0.122    0.123   -0.995    0.320
    ##    .visual3 (n.3.)   -0.468    0.147   -3.177    0.001
    ##    .textul1 (n.4.)    0.386    0.254    1.523    0.128
    ##    .textul2 (n.5.)    0.233    0.275    0.846    0.397
    ##    .textul3 (n.6.)   -0.040    0.231   -0.173    0.863
    ##    .speed1  (n.7.)    0.226    0.155    1.453    0.146
    ##    .speed2  (n.8.)   -0.115    0.164   -0.700    0.484
    ##    .speed3  (n.9.)    0.091    0.208    0.439    0.661
    ##     visual  (a.1.)    0.000                           
    ##     textual (a.2.)    0.000                           
    ##     speed   (a.3.)    0.000                           
    ## 
    ## Thresholds:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##     vsl1|t1 (v1.1)   -1.109    0.267   -4.159    0.000
    ##     vsl1|t2 (v1.2)   -0.086    0.150   -0.576    0.565
    ##     vsl1|t3 (v1.3)    0.945    0.257    3.671    0.000
    ##     vsl2|t1 (v2.1)   -0.692    0.125   -5.555    0.000
    ##     vsl2|t2 (v2.2)   -0.075    0.105   -0.712    0.476
    ##     vsl2|t3 (v2.3)    0.576    0.123    4.678    0.000
    ##     vsl3|t1 (v3.1)   -1.015    0.138   -7.356    0.000
    ##     vsl3|t2 (v3.2)   -0.236    0.112   -2.114    0.034
    ##     vsl3|t3 (v3.3)    0.548    0.125    4.378    0.000
    ##     txtl1|1 (t1.1)   -1.113    0.216   -5.154    0.000
    ##     txtl1|2 (t1.2)    0.205    0.192    1.065    0.287
    ##     txtl1|3 (t1.3)    1.529    0.278    5.495    0.000
    ##     txtl2|1 (t2.1)   -1.329    0.296   -4.491    0.000
    ##     txtl2|2 (t2.2)    0.128    0.211    0.607    0.544
    ##     txtl2|3 (t2.3)    1.591    0.338    4.705    0.000
    ##     txtl3|1 (t3.1)   -1.255    0.219   -5.718    0.000
    ##     txtl3|2 (t3.2)   -0.013    0.182   -0.071    0.943
    ##     txtl3|3 (t3.3)    1.232    0.243    5.070    0.000
    ##     spd1|t1 (s1.1)   -0.712    0.137   -5.200    0.000
    ##     spd1|t2 (s1.2)    0.117    0.115    1.019    0.308
    ##     spd1|t3 (s1.3)    0.953    0.146    6.538    0.000
    ##     spd2|t1 (s2.1)   -0.926    0.155   -5.973    0.000
    ##     spd2|t2 (s2.2)   -0.052    0.126   -0.412    0.680
    ##     spd2|t3 (s2.3)    0.822    0.146    5.622    0.000
    ##     spd3|t1 (s3.1)   -1.051    0.285   -3.682    0.000
    ##     spd3|t2 (s3.2)    0.055    0.163    0.337    0.736
    ##     spd3|t3 (s3.3)    1.164    0.300    3.881    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .visual1 (t.1_)    0.787    0.434    1.812    0.070
    ##    .visual2 (t.2_)    0.476    0.150    3.181    0.001
    ##    .visual3 (t.3_)    0.998    0.304    3.279    0.001
    ##    .textul1 (t.4_)    1.092    0.413    2.642    0.008
    ##    .textul2 (t.5_)    0.977    0.466    2.098    0.036
    ##    .textul3 (t.6_)    0.689    0.285    2.417    0.016
    ##    .speed1  (t.7_)    1.229    0.388    3.166    0.002
    ##    .speed2  (t.8_)    1.081    0.351    3.080    0.002
    ##    .speed3  (t.9_)    0.621    0.397    1.563    0.118
    ##     visual  (p.1_)    1.050    0.358    2.932    0.003
    ##     textual (p.2_)    1.027    0.250    4.108    0.000
    ##     speed   (p.3_)    1.038    0.313    3.321    0.001
    ## 
    ## Scales y*:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##     visual1           0.665                           
    ##     visual2           1.200                           
    ##     visual3           0.875                           
    ##     textual1          0.506                           
    ##     textual2          0.459                           
    ##     textual3          0.550                           
    ##     speed1            0.780                           
    ##     speed2            0.755                           
    ##     speed3            0.621

``` r
fit.indices[3,] <- round(data.matrix(fitmeasures(fit.metric,
                                                 fit.measures = c(
                                                   "chisq.scaled", "df.scaled",
                                                   "pvalue.scaled", "rmsea.scaled",
                                                   "cfi.scaled", "tli.scaled",
                                                   "srmr"))), digits = 3)
```

Next, additionally constrain intercepts to test scalar invariance

``` r
scalar <- as.character(measEq.syntax(configural.model = HS.model,
                                     data = data,
                                     ordered = c("visual1", "visual2", "visual3",
                                                 "textual1", "textual2", "textual3",
                                                 "speed1", "speed2", "speed3"),
                                     parameterization = "theta",
                                     ID.fac = "std.lv",
                                     ID.cat = "Wu.Estabrook.2016",
                                     group = "sex",
                                     group.equal = c("thresholds", "loadings", "intercepts")))
```

Estimate model, see results, and add fit indices to matrix

``` r
fit.scalar <- cfa(model = scalar, data = data, group = "sex",
                  ordered = c("visual1", "visual2", "visual3",
                              "textual1", "textual2", "textual3",
                              "speed1", "speed2", "speed3"),
                  parameterization = "theta")

summary(fit.scalar, fit.measures = TRUE)
```

    ## lavaan 0.6.17 ended normally after 122 iterations
    ## 
    ##   Estimator                                       DWLS
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                        93
    ##   Number of equality constraints                    36
    ## 
    ##   Number of observations per group:                   
    ##     1                                              146
    ##     2                                              155
    ## 
    ## Model Test User Model:
    ##                                               Standard      Scaled
    ##   Test Statistic                               108.263     145.759
    ##   Degrees of freedom                                69          69
    ##   P-value (Chi-square)                           0.002       0.000
    ##   Scaling correction factor                                  0.837
    ##   Shift parameter                                           16.373
    ##     simple second-order correction                                
    ##   Test statistic for each group:
    ##     1                                           69.067      90.484
    ##     2                                           39.196      55.275
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                              2453.988    1582.732
    ##   Degrees of freedom                                72          72
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  1.577
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.984       0.949
    ##   Tucker-Lewis Index (TLI)                       0.983       0.947
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                            NA
    ##   Robust Tucker-Lewis Index (TLI)                               NA
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.062       0.086
    ##   90 Percent confidence interval - lower         0.038       0.067
    ##   90 Percent confidence interval - upper         0.083       0.106
    ##   P-value H_0: RMSEA <= 0.050                    0.189       0.002
    ##   P-value H_0: RMSEA >= 0.080                    0.084       0.715
    ##                                                                   
    ##   Robust RMSEA                                                  NA
    ##   90 Percent confidence interval - lower                        NA
    ##   90 Percent confidence interval - upper                        NA
    ##   P-value H_0: Robust RMSEA <= 0.050                            NA
    ##   P-value H_0: Robust RMSEA >= 0.080                            NA
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.080       0.080
    ## 
    ## Parameter Estimates:
    ## 
    ##   Parameterization                               Theta
    ##   Standard errors                           Robust.sem
    ##   Information                                 Expected
    ##   Information saturated (h1) model        Unstructured
    ## 
    ## 
    ## Group 1 [1]:
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   visual =~                                           
    ##     visual1 (l.1_)    1.099    0.308    3.574    0.000
    ##     visual2 (l.2_)    0.456    0.099    4.590    0.000
    ##     visual3 (l.3_)    0.583    0.117    4.980    0.000
    ##   textual =~                                          
    ##     textul1 (l.4_)    1.668    0.281    5.946    0.000
    ##     textul2 (l.5_)    1.917    0.387    4.951    0.000
    ##     textul3 (l.6_)    1.585    0.252    6.280    0.000
    ##   speed =~                                            
    ##     speed1  (l.7_)    0.634    0.126    5.040    0.000
    ##     speed2  (l.8_)    0.794    0.136    5.820    0.000
    ##     speed3  (l.9_)    1.392    0.420    3.314    0.001
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   visual ~~                                           
    ##     textul  (p.2_)    0.496    0.101    4.893    0.000
    ##     speed  (p.3_1)    0.462    0.113    4.073    0.000
    ##   textual ~~                                          
    ##     speed  (p.3_2)    0.145    0.108    1.333    0.182
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .visual1 (nu.1)    0.000                           
    ##    .visual2 (nu.2)    0.000                           
    ##    .visual3 (nu.3)    0.000                           
    ##    .textul1 (nu.4)    0.000                           
    ##    .textul2 (nu.5)    0.000                           
    ##    .textul3 (nu.6)    0.000                           
    ##    .speed1  (nu.7)    0.000                           
    ##    .speed2  (nu.8)    0.000                           
    ##    .speed3  (nu.9)    0.000                           
    ##     visual  (a.1.)    0.000                           
    ##     textual (a.2.)    0.000                           
    ##     speed   (a.3.)    0.000                           
    ## 
    ## Thresholds:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##     vsl1|t1 (v1.1)   -1.169    0.245   -4.761    0.000
    ##     vsl1|t2 (v1.2)   -0.191    0.137   -1.394    0.163
    ##     vsl1|t3 (v1.3)    0.797    0.212    3.767    0.000
    ##     vsl2|t1 (v2.1)   -0.712    0.107   -6.634    0.000
    ##     vsl2|t2 (v2.2)   -0.095    0.081   -1.167    0.243
    ##     vsl2|t3 (v2.3)    0.558    0.099    5.643    0.000
    ##     vsl3|t1 (v3.1)   -0.883    0.130   -6.772    0.000
    ##     vsl3|t2 (v3.2)   -0.096    0.099   -0.974    0.330
    ##     vsl3|t3 (v3.3)    0.690    0.116    5.970    0.000
    ##     txtl1|1 (t1.1)   -1.218    0.222   -5.486    0.000
    ##     txtl1|2 (t1.2)    0.101    0.182    0.557    0.578
    ##     txtl1|3 (t1.3)    1.427    0.261    5.469    0.000
    ##     txtl2|1 (t2.1)   -1.342    0.300   -4.472    0.000
    ##     txtl2|2 (t2.2)    0.116    0.204    0.567    0.571
    ##     txtl2|3 (t2.3)    1.579    0.325    4.852    0.000
    ##     txtl3|1 (t3.1)   -1.134    0.207   -5.489    0.000
    ##     txtl3|2 (t3.2)    0.100    0.171    0.587    0.558
    ##     txtl3|3 (t3.3)    1.337    0.236    5.665    0.000
    ##     spd1|t1 (s1.1)   -0.799    0.124   -6.432    0.000
    ##     spd1|t2 (s1.2)    0.029    0.100    0.293    0.769
    ##     spd1|t3 (s1.3)    0.860    0.133    6.474    0.000
    ##     spd2|t1 (s2.1)   -0.839    0.139   -6.036    0.000
    ##     spd2|t2 (s2.2)    0.031    0.110    0.280    0.780
    ##     spd2|t3 (s2.3)    0.894    0.134    6.663    0.000
    ##     spd3|t1 (s3.1)   -1.061    0.278   -3.813    0.000
    ##     spd3|t2 (s3.2)    0.054    0.158    0.344    0.731
    ##     spd3|t3 (s3.3)    1.173    0.305    3.842    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .visual1 (t.1_)    1.000                           
    ##    .visual2 (t.2_)    1.000                           
    ##    .visual3 (t.3_)    1.000                           
    ##    .textul1 (t.4_)    1.000                           
    ##    .textul2 (t.5_)    1.000                           
    ##    .textul3 (t.6_)    1.000                           
    ##    .speed1  (t.7_)    1.000                           
    ##    .speed2  (t.8_)    1.000                           
    ##    .speed3  (t.9_)    1.000                           
    ##     visual  (p.1_)    1.000                           
    ##     textual (p.2_)    1.000                           
    ##     speed   (p.3_)    1.000                           
    ## 
    ## Scales y*:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##     visual1           0.673                           
    ##     visual2           0.910                           
    ##     visual3           0.864                           
    ##     textual1          0.514                           
    ##     textual2          0.463                           
    ##     textual3          0.534                           
    ##     speed1            0.844                           
    ##     speed2            0.783                           
    ##     speed3            0.583                           
    ## 
    ## 
    ## Group 2 [2]:
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   visual =~                                           
    ##     visual1 (l.1_)    1.099    0.308    3.574    0.000
    ##     visual2 (l.2_)    0.456    0.099    4.590    0.000
    ##     visual3 (l.3_)    0.583    0.117    4.980    0.000
    ##   textual =~                                          
    ##     textul1 (l.4_)    1.668    0.281    5.946    0.000
    ##     textul2 (l.5_)    1.917    0.387    4.951    0.000
    ##     textul3 (l.6_)    1.585    0.252    6.280    0.000
    ##   speed =~                                            
    ##     speed1  (l.7_)    0.634    0.126    5.040    0.000
    ##     speed2  (l.8_)    0.794    0.136    5.820    0.000
    ##     speed3  (l.9_)    1.392    0.420    3.314    0.001
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   visual ~~                                           
    ##     textul  (p.2_)    0.620    0.163    3.802    0.000
    ##     speed  (p.3_1)    0.606    0.177    3.422    0.001
    ##   textual ~~                                          
    ##     speed  (p.3_2)    0.395    0.132    2.991    0.003
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .visual1 (nu.1)    0.000                           
    ##    .visual2 (nu.2)    0.000                           
    ##    .visual3 (nu.3)    0.000                           
    ##    .textul1 (nu.4)    0.000                           
    ##    .textul2 (nu.5)    0.000                           
    ##    .textul3 (nu.6)    0.000                           
    ##    .speed1  (nu.7)    0.000                           
    ##    .speed2  (nu.8)    0.000                           
    ##    .speed3  (nu.9)    0.000                           
    ##     visual  (a.1.)   -0.335    0.159   -2.110    0.035
    ##     textual (a.2.)    0.109    0.129    0.844    0.398
    ##     speed   (a.3.)    0.064    0.141    0.458    0.647
    ## 
    ## Thresholds:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##     vsl1|t1 (v1.1)   -1.169    0.245   -4.761    0.000
    ##     vsl1|t2 (v1.2)   -0.191    0.137   -1.394    0.163
    ##     vsl1|t3 (v1.3)    0.797    0.212    3.767    0.000
    ##     vsl2|t1 (v2.1)   -0.712    0.107   -6.634    0.000
    ##     vsl2|t2 (v2.2)   -0.095    0.081   -1.167    0.243
    ##     vsl2|t3 (v2.3)    0.558    0.099    5.643    0.000
    ##     vsl3|t1 (v3.1)   -0.883    0.130   -6.772    0.000
    ##     vsl3|t2 (v3.2)   -0.096    0.099   -0.974    0.330
    ##     vsl3|t3 (v3.3)    0.690    0.116    5.970    0.000
    ##     txtl1|1 (t1.1)   -1.218    0.222   -5.486    0.000
    ##     txtl1|2 (t1.2)    0.101    0.182    0.557    0.578
    ##     txtl1|3 (t1.3)    1.427    0.261    5.469    0.000
    ##     txtl2|1 (t2.1)   -1.342    0.300   -4.472    0.000
    ##     txtl2|2 (t2.2)    0.116    0.204    0.567    0.571
    ##     txtl2|3 (t2.3)    1.579    0.325    4.852    0.000
    ##     txtl3|1 (t3.1)   -1.134    0.207   -5.489    0.000
    ##     txtl3|2 (t3.2)    0.100    0.171    0.587    0.558
    ##     txtl3|3 (t3.3)    1.337    0.236    5.665    0.000
    ##     spd1|t1 (s1.1)   -0.799    0.124   -6.432    0.000
    ##     spd1|t2 (s1.2)    0.029    0.100    0.293    0.769
    ##     spd1|t3 (s1.3)    0.860    0.133    6.474    0.000
    ##     spd2|t1 (s2.1)   -0.839    0.139   -6.036    0.000
    ##     spd2|t2 (s2.2)    0.031    0.110    0.280    0.780
    ##     spd2|t3 (s2.3)    0.894    0.134    6.663    0.000
    ##     spd3|t1 (s3.1)   -1.061    0.278   -3.813    0.000
    ##     spd3|t2 (s3.2)    0.054    0.158    0.344    0.731
    ##     spd3|t3 (s3.3)    1.173    0.305    3.842    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .visual1 (t.1_)    0.764    0.384    1.992    0.046
    ##    .visual2 (t.2_)    0.477    0.150    3.187    0.001
    ##    .visual3 (t.3_)    1.034    0.327    3.160    0.002
    ##    .textul1 (t.4_)    1.095    0.420    2.610    0.009
    ##    .textul2 (t.5_)    0.974    0.467    2.086    0.037
    ##    .textul3 (t.6_)    0.688    0.282    2.441    0.015
    ##    .speed1  (t.7_)    1.232    0.392    3.145    0.002
    ##    .speed2  (t.8_)    1.076    0.348    3.093    0.002
    ##    .speed3  (t.9_)    0.624    0.405    1.542    0.123
    ##     visual  (p.1_)    1.046    0.354    2.951    0.003
    ##     textual (p.2_)    1.026    0.250    4.103    0.000
    ##     speed   (p.3_)    1.042    0.314    3.317    0.001
    ## 
    ## Scales y*:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##     visual1           0.702                           
    ##     visual2           1.200                           
    ##     visual3           0.848                           
    ##     textual1          0.503                           
    ##     textual2          0.459                           
    ##     textual3          0.553                           
    ##     speed1            0.778                           
    ##     speed2            0.760                           
    ##     speed3            0.615

``` r
fit.indices[4,] <- round(data.matrix(fitmeasures(fit.scalar,
                                                 fit.measures = c(
                                                   "chisq.scaled", "df.scaled",
                                                   "pvalue.scaled", "rmsea.scaled",
                                                   "cfi.scaled", "tli.scaled",
                                                   "srmr"))), digits = 3)
```

Finally, constrain residuals to test residual invariance

``` r
residual <- as.character(measEq.syntax(configural.model = HS.model,
                                     data = data,
                                     ordered = c("visual1", "visual2", "visual3",
                                                 "textual1", "textual2", "textual3",
                                                 "speed1", "speed2", "speed3"),
                                     parameterization = "theta",
                                     ID.fac = "std.lv",
                                     ID.cat = "Wu.Estabrook.2016",
                                     group = "sex",
                                     group.equal = c("thresholds", "loadings",
                                                     "intercepts", "residuals")))
```

Estimate model, see results, and add fit indices to matrix

``` r
fit.residual <- cfa(model = residual, data = data, group = "sex",
                  ordered = c("visual1", "visual2", "visual3",
                              "textual1", "textual2", "textual3",
                              "speed1", "speed2", "speed3"),
                  parameterization = "theta")

summary(fit.residual, fit.measures = TRUE)
```

    ## lavaan 0.6.17 ended normally after 82 iterations
    ## 
    ##   Estimator                                       DWLS
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                        84
    ##   Number of equality constraints                    36
    ## 
    ##   Number of observations per group:                   
    ##     1                                              146
    ##     2                                              155
    ## 
    ## Model Test User Model:
    ##                                               Standard      Scaled
    ##   Test Statistic                               114.371     153.271
    ##   Degrees of freedom                                78          78
    ##   P-value (Chi-square)                           0.005       0.000
    ##   Scaling correction factor                                  0.851
    ##   Shift parameter                                           18.945
    ##     simple second-order correction                                
    ##   Test statistic for each group:
    ##     1                                           72.179      93.962
    ##     2                                           42.191      59.308
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                              2453.988    1582.732
    ##   Degrees of freedom                                72          72
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  1.577
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.985       0.950
    ##   Tucker-Lewis Index (TLI)                       0.986       0.954
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                            NA
    ##   Robust Tucker-Lewis Index (TLI)                               NA
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.056       0.080
    ##   90 Percent confidence interval - lower         0.032       0.061
    ##   90 Percent confidence interval - upper         0.077       0.099
    ##   P-value H_0: RMSEA <= 0.050                    0.317       0.006
    ##   P-value H_0: RMSEA >= 0.080                    0.028       0.529
    ##                                                                   
    ##   Robust RMSEA                                                  NA
    ##   90 Percent confidence interval - lower                        NA
    ##   90 Percent confidence interval - upper                        NA
    ##   P-value H_0: Robust RMSEA <= 0.050                            NA
    ##   P-value H_0: Robust RMSEA >= 0.080                            NA
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.082       0.082
    ## 
    ## Parameter Estimates:
    ## 
    ##   Parameterization                               Theta
    ##   Standard errors                           Robust.sem
    ##   Information                                 Expected
    ##   Information saturated (h1) model        Unstructured
    ## 
    ## 
    ## Group 1 [1]:
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   visual =~                                           
    ##     visual1 (l.1_)    1.129    0.254    4.442    0.000
    ##     visual2 (l.2_)    0.551    0.106    5.215    0.000
    ##     visual3 (l.3_)    0.557    0.105    5.294    0.000
    ##   textual =~                                          
    ##     textul1 (l.4_)    1.614    0.218    7.406    0.000
    ##     textul2 (l.5_)    1.910    0.298    6.399    0.000
    ##     textul3 (l.6_)    1.717    0.249    6.898    0.000
    ##   speed =~                                            
    ##     speed1  (l.7_)    0.596    0.111    5.351    0.000
    ##     speed2  (l.8_)    0.770    0.129    5.975    0.000
    ##     speed3  (l.9_)    1.624    0.459    3.537    0.000
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   visual ~~                                           
    ##     textul  (p.2_)    0.485    0.095    5.088    0.000
    ##     speed  (p.3_1)    0.447    0.105    4.246    0.000
    ##   textual ~~                                          
    ##     speed  (p.3_2)    0.145    0.106    1.368    0.171
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .visual1 (nu.1)    0.000                           
    ##    .visual2 (nu.2)    0.000                           
    ##    .visual3 (nu.3)    0.000                           
    ##    .textul1 (nu.4)    0.000                           
    ##    .textul2 (nu.5)    0.000                           
    ##    .textul3 (nu.6)    0.000                           
    ##    .speed1  (nu.7)    0.000                           
    ##    .speed2  (nu.8)    0.000                           
    ##    .speed3  (nu.9)    0.000                           
    ##     visual  (a.1.)    0.000                           
    ##     textual (a.2.)    0.000                           
    ##     speed   (a.3.)    0.000                           
    ## 
    ## Thresholds:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##     vsl1|t1 (v1.1)   -1.229    0.197   -6.245    0.000
    ##     vsl1|t2 (v1.2)   -0.184    0.140   -1.309    0.190
    ##     vsl1|t3 (v1.3)    0.858    0.177    4.852    0.000
    ##     vsl2|t1 (v2.1)   -0.866    0.105   -8.221    0.000
    ##     vsl2|t2 (v2.2)   -0.090    0.095   -0.952    0.341
    ##     vsl2|t3 (v2.3)    0.686    0.098    7.007    0.000
    ##     vsl3|t1 (v3.1)   -0.872    0.109   -7.974    0.000
    ##     vsl3|t2 (v3.2)   -0.094    0.097   -0.972    0.331
    ##     vsl3|t3 (v3.3)    0.684    0.102    6.712    0.000
    ##     txtl1|1 (t1.1)   -1.192    0.193   -6.171    0.000
    ##     txtl1|2 (t1.2)    0.099    0.176    0.560    0.575
    ##     txtl1|3 (t1.3)    1.397    0.212    6.577    0.000
    ##     txtl2|1 (t2.1)   -1.354    0.245   -5.520    0.000
    ##     txtl2|2 (t2.2)    0.115    0.204    0.563    0.573
    ##     txtl2|3 (t2.3)    1.592    0.265    6.015    0.000
    ##     txtl3|1 (t3.1)   -1.250    0.203   -6.157    0.000
    ##     txtl3|2 (t3.2)    0.102    0.184    0.556    0.579
    ##     txtl3|3 (t3.3)    1.458    0.229    6.355    0.000
    ##     spd1|t1 (s1.1)   -0.757    0.103   -7.386    0.000
    ##     spd1|t2 (s1.2)    0.025    0.095    0.258    0.797
    ##     spd1|t3 (s1.3)    0.807    0.106    7.638    0.000
    ##     spd2|t1 (s2.1)   -0.818    0.120   -6.824    0.000
    ##     spd2|t2 (s2.2)    0.031    0.108    0.283    0.777
    ##     spd2|t3 (s2.3)    0.875    0.119    7.353    0.000
    ##     spd3|t1 (s3.1)   -1.223    0.287   -4.265    0.000
    ##     spd3|t2 (s3.2)    0.061    0.178    0.344    0.731
    ##     spd3|t3 (s3.3)    1.350    0.310    4.355    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .visual1 (t.1_)    1.000                           
    ##    .visual2 (t.2_)    1.000                           
    ##    .visual3 (t.3_)    1.000                           
    ##    .textul1 (t.4_)    1.000                           
    ##    .textul2 (t.5_)    1.000                           
    ##    .textul3 (t.6_)    1.000                           
    ##    .speed1  (t.7_)    1.000                           
    ##    .speed2  (t.8_)    1.000                           
    ##    .speed3  (t.9_)    1.000                           
    ##     visual  (p.1_)    1.000                           
    ##     textual (p.2_)    1.000                           
    ##     speed   (p.3_)    1.000                           
    ## 
    ## Scales y*:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##     visual1           0.663                           
    ##     visual2           0.876                           
    ##     visual3           0.874                           
    ##     textual1          0.527                           
    ##     textual2          0.464                           
    ##     textual3          0.503                           
    ##     speed1            0.859                           
    ##     speed2            0.792                           
    ##     speed3            0.524                           
    ## 
    ## 
    ## Group 2 [2]:
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   visual =~                                           
    ##     visual1 (l.1_)    1.129    0.254    4.442    0.000
    ##     visual2 (l.2_)    0.551    0.106    5.215    0.000
    ##     visual3 (l.3_)    0.557    0.105    5.294    0.000
    ##   textual =~                                          
    ##     textul1 (l.4_)    1.614    0.218    7.406    0.000
    ##     textul2 (l.5_)    1.910    0.298    6.399    0.000
    ##     textul3 (l.6_)    1.717    0.249    6.898    0.000
    ##   speed =~                                            
    ##     speed1  (l.7_)    0.596    0.111    5.351    0.000
    ##     speed2  (l.8_)    0.770    0.129    5.975    0.000
    ##     speed3  (l.9_)    1.624    0.459    3.537    0.000
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   visual ~~                                           
    ##     textul  (p.2_)    0.691    0.179    3.853    0.000
    ##     speed  (p.3_1)    0.660    0.195    3.392    0.001
    ##   textual ~~                                          
    ##     speed  (p.3_2)    0.407    0.136    2.985    0.003
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .visual1 (nu.1)    0.000                           
    ##    .visual2 (nu.2)    0.000                           
    ##    .visual3 (nu.3)    0.000                           
    ##    .textul1 (nu.4)    0.000                           
    ##    .textul2 (nu.5)    0.000                           
    ##    .textul3 (nu.6)    0.000                           
    ##    .speed1  (nu.7)    0.000                           
    ##    .speed2  (nu.8)    0.000                           
    ##    .speed3  (nu.9)    0.000                           
    ##     visual  (a.1.)   -0.342    0.162   -2.109    0.035
    ##     textual (a.2.)    0.110    0.130    0.842    0.400
    ##     speed   (a.3.)    0.064    0.140    0.458    0.647
    ## 
    ## Thresholds:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##     vsl1|t1 (v1.1)   -1.229    0.197   -6.245    0.000
    ##     vsl1|t2 (v1.2)   -0.184    0.140   -1.309    0.190
    ##     vsl1|t3 (v1.3)    0.858    0.177    4.852    0.000
    ##     vsl2|t1 (v2.1)   -0.866    0.105   -8.221    0.000
    ##     vsl2|t2 (v2.2)   -0.090    0.095   -0.952    0.341
    ##     vsl2|t3 (v2.3)    0.686    0.098    7.007    0.000
    ##     vsl3|t1 (v3.1)   -0.872    0.109   -7.974    0.000
    ##     vsl3|t2 (v3.2)   -0.094    0.097   -0.972    0.331
    ##     vsl3|t3 (v3.3)    0.684    0.102    6.712    0.000
    ##     txtl1|1 (t1.1)   -1.192    0.193   -6.171    0.000
    ##     txtl1|2 (t1.2)    0.099    0.176    0.560    0.575
    ##     txtl1|3 (t1.3)    1.397    0.212    6.577    0.000
    ##     txtl2|1 (t2.1)   -1.354    0.245   -5.520    0.000
    ##     txtl2|2 (t2.2)    0.115    0.204    0.563    0.573
    ##     txtl2|3 (t2.3)    1.592    0.265    6.015    0.000
    ##     txtl3|1 (t3.1)   -1.250    0.203   -6.157    0.000
    ##     txtl3|2 (t3.2)    0.102    0.184    0.556    0.579
    ##     txtl3|3 (t3.3)    1.458    0.229    6.355    0.000
    ##     spd1|t1 (s1.1)   -0.757    0.103   -7.386    0.000
    ##     spd1|t2 (s1.2)    0.025    0.095    0.258    0.797
    ##     spd1|t3 (s1.3)    0.807    0.106    7.638    0.000
    ##     spd2|t1 (s2.1)   -0.818    0.120   -6.824    0.000
    ##     spd2|t2 (s2.2)    0.031    0.108    0.283    0.777
    ##     spd2|t3 (s2.3)    0.875    0.119    7.353    0.000
    ##     spd3|t1 (s3.1)   -1.223    0.287   -4.265    0.000
    ##     spd3|t2 (s3.2)    0.061    0.178    0.344    0.731
    ##     spd3|t3 (s3.3)    1.350    0.310    4.355    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .visual1 (t.1_)    1.000                           
    ##    .visual2 (t.2_)    1.000                           
    ##    .visual3 (t.3_)    1.000                           
    ##    .textul1 (t.4_)    1.000                           
    ##    .textul2 (t.5_)    1.000                           
    ##    .textul3 (t.6_)    1.000                           
    ##    .speed1  (t.7_)    1.000                           
    ##    .speed2  (t.8_)    1.000                           
    ##    .speed3  (t.9_)    1.000                           
    ##     visual  (p.1_)    1.174    0.402    2.918    0.004
    ##     textual (p.2_)    1.080    0.264    4.098    0.000
    ##     speed   (p.3_)    1.021    0.308    3.308    0.001
    ## 
    ## Scales y*:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##     visual1           0.633                           
    ##     visual2           0.859                           
    ##     visual3           0.856                           
    ##     textual1          0.512                           
    ##     textual2          0.450                           
    ##     textual3          0.489                           
    ##     speed1            0.857                           
    ##     speed2            0.789                           
    ##     speed3            0.521

``` r
fit.indices[5,] <- round(data.matrix(fitmeasures(fit.residual,
                                                 fit.measures = c(
                                                   "chisq.scaled", "df.scaled",
                                                   "pvalue.scaled", "rmsea.scaled",
                                                   "cfi.scaled", "tli.scaled",
                                                   "srmr"))), digits = 3)
```

Measurement invariance is rejected if model fit declines sufficiently to
suggest that constraining model parameters to equality is inappropriate.
Likelihood ratio test is a common one to assess fit change
(statistically significant test indicates large decrease in fit)

``` r
lavTestLRT(fit.configural, fit.thresholds, fit.metric, fit.scalar, fit.residual)
```

    ## 
    ## Scaled Chi-Squared Difference Test (method = "satorra.2000")
    ## 
    ## lavaan NOTE:
    ##     The "Chisq" column contains standard test statistics, not the
    ##     robust test that should be reported per model. A robust difference
    ##     test is a function of two standard (not robust) statistics.
    ##  
    ##                Df AIC BIC   Chisq Chisq diff Df diff Pr(>Chisq)  
    ## fit.configural 48          76.294                                
    ## fit.thresholds 57          79.124     8.5005       9    0.48460  
    ## fit.metric     63          89.791    10.5401       6    0.10367  
    ## fit.scalar     69         108.263    15.5867       6    0.01615 *
    ## fit.residual   78         114.371     8.7711       9    0.45867  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Conventional wisdom is to look at changes in other fit measures, not
just the LRT. These are stored in the fit.indices matrix.

Rename rows and columns for clarity and display fit measures:

``` r
colnames(fit.indices) <- c(
  "chisq.scaled", "df.scaled",
  "pvalue.scaled", "rmsea.scaled",
  "cfi.scaled", "tli.scaled",
  "srmr")

rownames(fit.indices) <- c(
  "Configural", "Thresholds",
  "Metric", "Scalar",
  "Residual")
  
fit.indices
```

    ##            chisq.scaled df.scaled pvalue.scaled rmsea.scaled cfi.scaled
    ## Configural      111.625        48             0        0.094      0.958
    ## Thresholds      124.298        57             0        0.089      0.955
    ## Metric          130.679        63             0        0.085      0.955
    ## Scalar          145.759        69             0        0.086      0.949
    ## Residual        153.271        78             0        0.080      0.950
    ##            tli.scaled  srmr
    ## Configural      0.937 0.078
    ## Thresholds      0.944 0.078
    ## Metric          0.949 0.080
    ## Scalar          0.947 0.080
    ## Residual        0.954 0.082
