# nds3_application

Install `nds3` from Github.

``` r
# install devtools if needed
if (!requireNamespace("devtools")) {install.packages("devtools")}

# install from GitHub
devtools::install_github("sebastian-lortz/nds3")
```

Load the `nds3` package.

``` r
library(nds3)
```

## ANOVA Module

As part of the Open Science Collaboration’s large‐scale effort to
estimate the replicability of psychological findings
(Open-Science-Collaboration, 2015), many original datasets remain
unavailable. In the study by Reynolds and Besner (2008) on contextual
effects in reading aloud, participants’ response times to exception
words and nonwords were measured under predictable switch and stay
sequences to probe dynamic pathway control in skilled reading
\[<https://osf.io/hasfu/>\]. In the following steps, I applied nds3 to
demonstrate how it’s ANOVA module can generate a fully synthetic dataset
matching the reported summary estimates.

I began by extracting the relevant parameters from the article.

``` r
N = 16
levels = c(2,2)
target_group_means <- c(543, 536, 614, 618)
factor_type <- c("within", "within")
formula <- "outcome ~ Factor1 * Factor2 + Error(ID / (Factor1 * Factor2))"
integer <- FALSE
```

Factor2 and Interaction effects are reported as F \< 1, thus, I set
arbitrary values

``` r
target_f_vec <- list(effect = c("Factor1", "Factor2", "Factor1:Factor2"),
                          F = c(30.5, 0.0, 0.2))
```

I then computed a plausible response time range \[L,U\] from the pooled
`MSE = 3070` using

``` r
L <- min(target_group_means) - floor(3 * sqrt(3070))
U <- max(target_group_means) + ceiling(3 * sqrt(3070))
L
#> [1] 370
U
#> [1] 785
range <- c(370,785)
```

Next, I ran the ANOVA module with a small `max_step` to avoid early
convergence given the coarse target precision and otherwise default
hyperparameters.

``` r
result.aov <- optim_aov(N = N,
                        levels = levels,
                        target_group_means = target_group_means,
                        target_f_list = target_f_vec,
                        factor_type = factor_type,
                        range = range,
                        formula = formula,
                        integer = integer,
                        max_step = .1,
                        tolerance = 1e-8,
                        max_iter = 1e3,
                        init_temp = 1,
                        cooling_rate = NULL)
#>   |                                                                              |                                                                      |   0%  |                                                                              |=                                                                     |   1%  |                                                                              |=                                                                     |   2%  |                                                                              |==                                                                    |   3%  |                                                                              |===                                                                   |   4%  |                                                                              |====                                                                  |   5%  |                                                                              |====                                                                  |   6%  |                                                                              |=====                                                                 |   7%  |                                                                              |======                                                                |   8%  |                                                                              |======                                                                |   9%  |                                                                              |=======                                                               |  10%  |                                                                              |========                                                              |  11%  |                                                                              |========                                                              |  12%  |                                                                              |=========                                                             |  13%  |                                                                              |==========                                                            |  14%  |                                                                              |==========                                                            |  15%  |                                                                              |===========                                                           |  16%  |                                                                              |============                                                          |  17%  |                                                                              |=============                                                         |  18%  |                                                                              |=============                                                         |  19%
```

The optimization converged exactly. Inspecting

``` r
summary(result.aov)
#> nds3 Object Summary
#> -------------------------------------------------
#> 
#> Achieved Loss of Optimization:  0 
#> 
#> RMSE of F statistics:  0 
#> 
#> Factorial Model:
#> outcome ~ Factor1 * Factor2 + Error(ID/(Factor1 * Factor2)) 
#> 
#> Group Means:
#> [1] 543 536 614 618
```

confirms that the simulated data reproduce the published cell means and
F-statistics. I then visualized the error trajectory

``` r
plot_error(result.aov)
```

![Plot Error Trajectory for
AOV.](nds3_application_files/figure-html/plot%20error%20aov-1.png)

and illustrated the estimated versus target effects with

``` r
plot_summary(result.aov, standardised = FALSE)
```

![Plot Summary for
AOV.](nds3_application_files/figure-html/plot%20summary%20aov-1.png) .
Finally, I saved the RMSE and relevant statistics,

``` r
get_rmse(result.aov)
#> $rmse_F
#> [1] 0
get_stats(result.aov)
#> $model
#> Anova Table (Type 3 tests)
#> 
#> Response: outcome
#>                 num Df den Df    MSE       F     ges    Pr(>F)    
#> Factor1              1     15 3072.2 30.4789 0.44810 5.872e-05 ***
#> Factor2              1     15 1186.0  0.0304 0.00031    0.8640    
#> Factor1:Factor2      1     15 2166.8  0.2234 0.00418    0.6433    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> $F_value
#> [1] 30.4789024  0.0303541  0.2233733
#> 
#> $mean
#> [1] 543 536 614 618
```

extracted the simulated dataset and inspected its distribution.

``` r
data.aov <- result.aov$data
head(data.aov)
#>   ID Factor1 Factor2  outcome
#> 1  1       1       1 628.7339
#> 2  1       1       2 516.6337
#> 3  1       2       1 622.0609
#> 4  1       2       2 677.6714
#> 5  2       1       1 533.6981
#> 6  2       1       2 556.1095
plot_histogram(data.aov[,4, drop = FALSE])
#> $outcome
```

![Plot Histogram for
AOV.](nds3_application_files/figure-html/data%20aov-1.png)

Additionally, I executed the ANOVA module in multiple parallel runs to
quantify both the convergence variability of RMSE within each run
(compared to the target values) and the variability of RMSE across runs
(compared to the average simulated values).

``` r
result.parallel.aov <- parallel_aov(
                        parallel_start = 100,
                        return_best_solution = FALSE,
                        N = N,
                        levels =levels,
                        target_group_means = target_group_means,
                        target_f_list = target_f_vec,
                        factor_type = factor_type,
                        range = range,
                        formula = formula,
                        tolerance = 1e-8,
                        max_iter = 1e3,
                        init_temp = 1,
                        cooling_rate = NULL,
                        integer = FALSE,
                        checkGrim = FALSE,
                        max_step = .1,
                        min_decimals = 1)
```

I then plotted these RMSE distributions side-by-side to compare within
versus between run variation.

``` r
plot_rmse(result.parallel.aov)
```

![Plot RMSE for
AOV.](nds3_application_files/figure-html/plot%20rmse%20aov-1.png) Note.
The two clusters emerge due to the low decimal precision of the reported
F values. \#

## Descriptives and LM Module

In the replication attempt by Bardwell et al. (2007) patients with
obstructive sleep apnea completed both fatigue and depression scales to
examine whether mood symptoms or apnea severity better predict daytime
fatigue \[<https://doi.org/10.1016/j.jad.2006.06.013>\]. The original
study’s (Bardwell et al., 2003) raw data are not publicly available.
Here, I apply the Descriptives and LM module of the nds3 framework to
simulate a synthetic dataset that reproduces their reported summary
estimates.

#### Step 1.

I began by extracting the relevant descriptive parameters from the
article.

``` r
N = 60
target_mean <- c(48.8, 17.3, 12.6, 10.8)
names(target_mean) <- c("Apnea.1", "Apnea.2", "Depression", "Fatigue")
target_sd <- c(27.1, 20.1, 11.3, 7.3)
integer = c(FALSE, FALSE, TRUE, TRUE)
range_matrix <- matrix(c(15, 0, 0, 0, 
                         111, 80.9, 49, 28),
                       nrow = 2, byrow = TRUE)
```

I subsequently estimated the weights

``` r
weight.vec <- weights_vec(
  N = N, 
  target_mean =  target_mean,
  target_sd =  target_sd, 
  range = range_matrix,
  integer = integer
)
weight.vec
#> [[1]]
#> [1]  1.0000 30.1046
#> 
#> [[2]]
#> [1]  1.0000 16.6172
#> 
#> [[3]]
#> [1] 1.0000 3.9154
#> 
#> [[4]]
#> [1]  1.0000 10.6053
```

and I ran the Descriptives module with default hyperparameters.

``` r
result.vec <- optim_vec(
  N = N,
  target_mean = target_mean,
  target_sd = target_sd,
  range = range_matrix,
  integer = integer,
  obj_weight = weight.vec,
  tolerance = 1e-8,
  max_iter = 1e5,
  max_starts = 3,
  init_temp = 1,
  cooling_rate = NULL
)
#> 
#> PSO is running...
#> Converged 
#> 
#> 
#> PSO is running...
#> Converged 
#> 
#> 
#> Mean 12.6 passed GRIM test. The mean is plausible.
#>   |                                                                              |                                                                      |   0%  |                                                                              |=                                                                     |   1%  |                                                                              |=                                                                     |   2%  |                                                                              |==                                                                    |   3%  |                                                                              |===                                                                   |   4%  |                                                                              |====                                                                  |   5%  |                                                                              |====                                                                  |   6%  |                                                                              |=====                                                                 |   7%  |                                                                              |======                                                                |   8%  |                                                                              |======                                                                |   9%  |                                                                              |=======                                                               |  10%  |                                                                              |========                                                              |  11%  |                                                                              |========                                                              |  12%  |                                                                              |=========                                                             |  13%  |                                                                              |==========                                                            |  14%  |                                                                              |==========                                                            |  15%  |                                                                              |===========                                                           |  16%  |                                                                              |============                                                          |  17%  |                                                                              |=============                                                         |  18%  |                                                                              |=============                                                         |  19%  |                                                                              |==============                                                        |  20%  |                                                                              |===============                                                       |  21%  |                                                                              |===============                                                       |  22%  |                                                                              |================                                                      |  23%  |                                                                              |=================                                                     |  24%  |                                                                              |==================                                                    |  25%  |                                                                              |==================                                                    |  26%  |                                                                              |===================                                                   |  27%  |                                                                              |====================                                                  |  28%  |                                                                              |====================                                                  |  29%  |                                                                              |=====================                                                 |  30%  |                                                                              |======================                                                |  31%  |                                                                              |======================                                                |  32%  |                                                                              |=======================                                               |  33%
#> converged!
#> 
#> 
#> Mean 10.8 passed GRIM test. The mean is plausible.
#>   |                                                                              |                                                                      |   0%  |                                                                              |=                                                                     |   1%  |                                                                              |=                                                                     |   2%  |                                                                              |==                                                                    |   3%  |                                                                              |===                                                                   |   4%  |                                                                              |====                                                                  |   5%  |                                                                              |====                                                                  |   6%  |                                                                              |=====                                                                 |   7%  |                                                                              |======                                                                |   8%  |                                                                              |======                                                                |   9%  |                                                                              |=======                                                               |  10%  |                                                                              |========                                                              |  11%
#> converged!
```

The optimization converged exactly. Inspecting

``` r
summary(result.vec)
#> nds3 Object Summary
#> -------------------------------------------------
#> 
#> Achieved Loss of Optimization:
#>    Apnea.1    Apnea.2 Depression    Fatigue 
#>          0          0          0          0 
#> 
#> RMSE of Summary Statistics
#>   Means:  0 
#>   SDs:    0 
#> 
#> Means:
#>    Apnea.1    Apnea.2 Depression    Fatigue 
#>   48.76215   17.32129   12.63333   10.80000 
#> 
#> SDs:
#>    Apnea.1    Apnea.2 Depression    Fatigue 
#>   27.05934   20.09107   11.25357    7.30173
```

confirms that the simulated data reproduce the published means and SDs.
I then visualized the error trajectories. For example, the error
trajectory of the fatigue variable is:

``` r
plot_error(result.vec, run = 4)
```

![Plot Error for
Vec.](nds3_application_files/figure-html/plot%20error%20vec-1.png)

The estimated versus target descriptives are given by

``` r
plot_summary(result.vec, standardised = FALSE)
```

![Plot Summary for
Vec.](nds3_application_files/figure-html/plot%20summary%20vec-1.png)

Finally, I saved the RMSE and relevant statistics,

``` r
get_rmse(result.vec)
#> $rmse_mean
#> [1] 0
#> 
#> $rmse_sd
#> [1] 0
get_stats(result.vec)
#> $mean
#>    Apnea.1    Apnea.2 Depression    Fatigue 
#>   48.76215   17.32129   12.63333   10.80000 
#> 
#> $sd
#>    Apnea.1    Apnea.2 Depression    Fatigue 
#>   27.05934   20.09107   11.25357    7.30173
```

extracted the simulated dataset and inspected its distributions and
frequencies.

``` r
data.vec <- result.vec$data
head(data.vec)
#>    Apnea.1   Apnea.2 Depression Fatigue
#> 1 64.97937 12.931947          6       9
#> 2 33.60681 77.679530          9       5
#> 3 77.55359 28.297373         18      23
#> 4 23.51289 80.900000         20      12
#> 5 36.03457 10.184895         11       0
#> 6 18.21944  5.208951         17       6
gridExtra::grid.arrange(grobs = plot_histogram(data.vec), ncol = 2)
```

![Plot Histogram for
Vec.](nds3_application_files/figure-html/data%20vec-1.png)

#### Step 2.

I began by extracting the relevant correlation and regression parameters
from the article and handing off the simulated data from the
Descriptives module for further use.

``` r
sim_data <- data.vec
target_reg <- c(4.020, 0.023, 0.008, 0.438)
names(target_reg) <- c("Apnea.1", "Apnea.2", "Depression", "Fatigue")
target_se <- c(NA, 0.034, 0.048, 0.066)
target_cor <-  c(NA, NA, NA, 0.11, 0.20, 0.68)
reg_equation <- "Fatigue ~ Apnea.1 + Apnea.2 + Depression"
```

I subsequently estimated the weights

``` r
result.weight.lm <- weights_est(
  sim_runs = 1,
  sim_data = sim_data,
  reg_equation = reg_equation,
  target_cor = target_cor,
  target_reg = target_reg,
  target_se = target_se,
  tol = 1e-8,
  max_iter = 1e5,
  init_temp = 1,
  cooling_rate = NULL
)
#>   |                                                                              |                                                                      |   0%  |                                                                              |=                                                                     |   1%  |                                                                              |=                                                                     |   2%  |                                                                              |==                                                                    |   3%  |                                                                              |===                                                                   |   4%  |                                                                              |====                                                                  |   5%  |                                                                              |====                                                                  |   6%  |                                                                              |=====                                                                 |   7%  |                                                                              |======                                                                |   8%  |                                                                              |======                                                                |   9%  |                                                                              |=======                                                               |  10%  |                                                                              |========                                                              |  11%  |                                                                              |========                                                              |  12%  |                                                                              |=========                                                             |  13%  |                                                                              |==========                                                            |  14%  |                                                                              |==========                                                            |  15%  |                                                                              |===========                                                           |  16%  |                                                                              |============                                                          |  17%  |                                                                              |=============                                                         |  18%  |                                                                              |=============                                                         |  19%  |                                                                              |==============                                                        |  20%  |                                                                              |===============                                                       |  21%  |                                                                              |===============                                                       |  22%  |                                                                              |================                                                      |  23%  |                                                                              |=================                                                     |  24%  |                                                                              |==================                                                    |  25%  |                                                                              |==================                                                    |  26%  |                                                                              |===================                                                   |  27%  |                                                                              |====================                                                  |  28%  |                                                                              |====================                                                  |  29%  |                                                                              |=====================                                                 |  30%  |                                                                              |======================                                                |  31%  |                                                                              |======================                                                |  32%  |                                                                              |=======================                                               |  33%  |                                                                              |========================                                              |  34%  |                                                                              |========================                                              |  35%  |                                                                              |=========================                                             |  36%  |                                                                              |==========================                                            |  37%  |                                                                              |===========================                                           |  38%  |                                                                              |===========================                                           |  39%  |                                                                              |============================                                          |  40%  |                                                                              |=============================                                         |  41%  |                                                                              |=============================                                         |  42%  |                                                                              |==============================                                        |  43%  |                                                                              |===============================                                       |  44%  |                                                                              |================================                                      |  45%  |                                                                              |================================                                      |  46%  |                                                                              |=================================                                     |  47%  |                                                                              |==================================                                    |  48%  |                                                                              |==================================                                    |  49%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================                                  |  51%  |                                                                              |====================================                                  |  52%  |                                                                              |=====================================                                 |  53%  |                                                                              |======================================                                |  54%  |                                                                              |======================================                                |  55%  |                                                                              |=======================================                               |  56%  |                                                                              |========================================                              |  57%  |                                                                              |=========================================                             |  58%  |                                                                              |=========================================                             |  59%  |                                                                              |==========================================                            |  60%  |                                                                              |===========================================                           |  61%  |                                                                              |===========================================                           |  62%  |                                                                              |============================================                          |  63%  |                                                                              |=============================================                         |  64%  |                                                                              |==============================================                        |  65%  |                                                                              |==============================================                        |  66%  |                                                                              |===============================================                       |  67%  |                                                                              |================================================                      |  68%  |                                                                              |================================================                      |  69%  |                                                                              |=================================================                     |  70%  |                                                                              |==================================================                    |  71%  |                                                                              |==================================================                    |  72%  |                                                                              |===================================================                   |  73%  |                                                                              |====================================================                  |  74%  |                                                                              |====================================================                  |  75%  |                                                                              |=====================================================                 |  76%  |                                                                              |======================================================                |  77%  |                                                                              |=======================================================               |  78%  |                                                                              |=======================================================               |  79%  |                                                                              |========================================================              |  80%  |                                                                              |=========================================================             |  81%  |                                                                              |=========================================================             |  82%  |                                                                              |==========================================================            |  83%  |                                                                              |===========================================================           |  84%  |                                                                              |============================================================          |  85%  |                                                                              |============================================================          |  86%  |                                                                              |=============================================================         |  87%  |                                                                              |==============================================================        |  88%  |                                                                              |==============================================================        |  89%  |                                                                              |===============================================================       |  90%  |                                                                              |================================================================      |  91%  |                                                                              |================================================================      |  92%  |                                                                              |=================================================================     |  93%  |                                                                              |==================================================================    |  94%  |                                                                              |==================================================================    |  95%  |                                                                              |===================================================================   |  96%  |                                                                              |====================================================================  |  97%  |                                                                              |===================================================================== |  98%  |                                                                              |===================================================================== |  99%  |                                                                              |======================================================================| 100%
#> Best error in start 1 is 0.003585686 
#> 
#> The estimated weights are: 1 1.14
weight.lm <- result.weight.lm$weights
weight.lm
#> [1] 1.00 1.14
```

and I ran the LM module with default hyperparameters.

``` r
result.lm <- optim_lm(
  sim_data = sim_data,
  reg_equation = reg_equation,
  target_cor = target_cor,
  target_reg = target_reg,
  target_se = target_se,
  weight = weight.lm,
  tol = 1e-8,
  max_iter = 1e5,
  max_starts = 1,
  init_temp = 1,
  cooling_rate = NULL
)
#>   |                                                                              |                                                                      |   0%  |                                                                              |=                                                                     |   1%  |                                                                              |=                                                                     |   2%  |                                                                              |==                                                                    |   3%  |                                                                              |===                                                                   |   4%  |                                                                              |====                                                                  |   5%  |                                                                              |====                                                                  |   6%  |                                                                              |=====                                                                 |   7%  |                                                                              |======                                                                |   8%  |                                                                              |======                                                                |   9%  |                                                                              |=======                                                               |  10%  |                                                                              |========                                                              |  11%  |                                                                              |========                                                              |  12%  |                                                                              |=========                                                             |  13%  |                                                                              |==========                                                            |  14%  |                                                                              |==========                                                            |  15%  |                                                                              |===========                                                           |  16%  |                                                                              |============                                                          |  17%  |                                                                              |=============                                                         |  18%  |                                                                              |=============                                                         |  19%  |                                                                              |==============                                                        |  20%  |                                                                              |===============                                                       |  21%  |                                                                              |===============                                                       |  22%  |                                                                              |================                                                      |  23%  |                                                                              |=================                                                     |  24%  |                                                                              |==================                                                    |  25%  |                                                                              |==================                                                    |  26%  |                                                                              |===================                                                   |  27%  |                                                                              |====================                                                  |  28%  |                                                                              |====================                                                  |  29%  |                                                                              |=====================                                                 |  30%  |                                                                              |======================                                                |  31%  |                                                                              |======================                                                |  32%  |                                                                              |=======================                                               |  33%  |                                                                              |========================                                              |  34%  |                                                                              |========================                                              |  35%  |                                                                              |=========================                                             |  36%  |                                                                              |==========================                                            |  37%  |                                                                              |===========================                                           |  38%  |                                                                              |===========================                                           |  39%  |                                                                              |============================                                          |  40%  |                                                                              |=============================                                         |  41%  |                                                                              |=============================                                         |  42%  |                                                                              |==============================                                        |  43%  |                                                                              |===============================                                       |  44%  |                                                                              |================================                                      |  45%  |                                                                              |================================                                      |  46%  |                                                                              |=================================                                     |  47%  |                                                                              |==================================                                    |  48%  |                                                                              |==================================                                    |  49%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================                                  |  51%  |                                                                              |====================================                                  |  52%  |                                                                              |=====================================                                 |  53%  |                                                                              |======================================                                |  54%  |                                                                              |======================================                                |  55%  |                                                                              |=======================================                               |  56%  |                                                                              |========================================                              |  57%  |                                                                              |=========================================                             |  58%  |                                                                              |=========================================                             |  59%  |                                                                              |==========================================                            |  60%  |                                                                              |===========================================                           |  61%  |                                                                              |===========================================                           |  62%  |                                                                              |============================================                          |  63%  |                                                                              |=============================================                         |  64%  |                                                                              |==============================================                        |  65%  |                                                                              |==============================================                        |  66%  |                                                                              |===============================================                       |  67%  |                                                                              |================================================                      |  68%  |                                                                              |================================================                      |  69%  |                                                                              |=================================================                     |  70%  |                                                                              |==================================================                    |  71%  |                                                                              |==================================================                    |  72%  |                                                                              |===================================================                   |  73%  |                                                                              |====================================================                  |  74%  |                                                                              |====================================================                  |  75%  |                                                                              |=====================================================                 |  76%  |                                                                              |======================================================                |  77%  |                                                                              |=======================================================               |  78%  |                                                                              |=======================================================               |  79%  |                                                                              |========================================================              |  80%  |                                                                              |=========================================================             |  81%  |                                                                              |=========================================================             |  82%  |                                                                              |==========================================================            |  83%  |                                                                              |===========================================================           |  84%  |                                                                              |============================================================          |  85%  |                                                                              |============================================================          |  86%  |                                                                              |=============================================================         |  87%  |                                                                              |==============================================================        |  88%  |                                                                              |==============================================================        |  89%  |                                                                              |===============================================================       |  90%  |                                                                              |================================================================      |  91%  |                                                                              |================================================================      |  92%  |                                                                              |=================================================================     |  93%  |                                                                              |==================================================================    |  94%  |                                                                              |==================================================================    |  95%  |                                                                              |===================================================================   |  96%  |                                                                              |====================================================================  |  97%  |                                                                              |===================================================================== |  98%  |                                                                              |===================================================================== |  99%  |                                                                              |======================================================================| 100%
#> Best error in start 1 is 0.005876413
```

The optimization converged with reaching Max Iterations and a low RMSE.
Inspecting

``` r
summary(result.lm)
#> nds3 Object Summary
#> -------------------------------------------------
#> 
#> Achieved Loss of Optimization:  0.005876413 
#> 
#> RMSE of Summary Statistics
#>   Correlations:             0 
#>   Regression Coefficients:  0 
#>   Standard Errors:          0.007874008 
#> 
#> Regression Model:
#> "Fatigue ~ Apnea.1 + Apnea.2 + Depression" 
#> 
#> Simulated Data Summary:
#>   Coefficients:
#> (Intercept)     Apnea.1     Apnea.2  Depression 
#> 4.020195771 0.022764103 0.008178164 0.437582155 
#> 
#>   Std. Errors:
#> (Intercept)     Apnea.1     Apnea.2  Depression 
#>  1.70379530  0.02619971  0.03653964  0.06521655 
#> 
#>   Correlations:
#> [1] -0.05049437  0.04474935  0.26319808  0.11340425  0.19574643  0.68410829
#> 
#>   Means:
#>    Apnea.1    Apnea.2 Depression    Fatigue 
#>   48.76215   17.32129   12.63333   10.80000 
#> 
#>   SDs:
#>    Apnea.1    Apnea.2 Depression    Fatigue 
#>   27.05934   20.09107   11.25357    7.30173
```

confirms that the simulated data closely reproduce the published
regression and correlation parameters. I then visualized the error,

``` r
plot_error(result.lm)
```

![Plot Error for
LM.](nds3_application_files/figure-html/plot%20error%20lm-1.png) the
error-ratio trajectories,

``` r
plot_error_ratio(result.lm)
```

![Plot Error Ration for
LM.](nds3_application_files/figure-html/error%20ratio%20lm-1.png) and
illustrated the estimated versus target descriptives with

``` r
plot_summary(result.lm, standardised = FALSE)
```

![Plot Summary for
LM.](nds3_application_files/figure-html/plot%20summary%20lm-1.png)

Finally, I saved the RMSE and relevant statistics,

``` r
get_rmse(result.lm)
#> $rmse_cor
#> [1] 0
#> 
#> $rmse_reg
#> [1] 0
#> 
#> $rmse_se
#> [1] 0.007874008
get_stats(result.lm)
#> $model
#> 
#> Call:
#> stats::lm(formula = eq, data = data_df)
#> 
#> Coefficients:
#> (Intercept)      Apnea.1      Apnea.2   Depression  
#>    4.020196     0.022764     0.008178     0.437582  
#> 
#> 
#> $reg
#> (Intercept)     Apnea.1     Apnea.2  Depression 
#> 4.020195771 0.022764103 0.008178164 0.437582155 
#> 
#> $se
#> (Intercept)     Apnea.1     Apnea.2  Depression 
#>  1.70379530  0.02619971  0.03653964  0.06521655 
#> 
#> $cor
#> [1] -0.05049437  0.04474935  0.26319808  0.11340425  0.19574643  0.68410829
#> 
#> $mean
#>    Apnea.1    Apnea.2 Depression    Fatigue 
#>   48.76215   17.32129   12.63333   10.80000 
#> 
#> $sd
#>    Apnea.1    Apnea.2 Depression    Fatigue 
#>   27.05934   20.09107   11.25357    7.30173
```

extracted the simulated dataset and inspected its partial regression
plots.

``` r
data.lm <- result.lm$data
head(data.lm)
#>    Apnea.1    Apnea.2 Depression Fatigue
#> 1 58.36190 39.2568653         17       9
#> 2 15.00000  0.4791305         26       5
#> 3 46.53431 43.8387514         16      23
#> 4 51.52888 31.8596164         12      12
#> 5 18.84405  7.8001462          2       0
#> 6 89.73526 49.3039396          5       6
partial.plots <- plot_partial_regression(lm(reg_equation, data.lm))
gridExtra::grid.arrange(grobs = partial.plots, ncol = 2)
```

![Plot Partial Regressions for
LM.](nds3_application_files/figure-html/data%20lm-1.png)

Additionally, I executed the LM module in multiple parallel runs to
quantify both the convergence variability of RMSE within each run
(compared to the target values) and the variability of RMSE across runs
(compared to the average simulated values).

``` r
result.parallel.lm <- parallel_lm(
  parallel_start = 100,
  return_best_solution = FALSE,
  sim_data = sim_data,
  reg_equation = reg_equation,
  target_cor = target_cor,
  target_reg = target_reg,
  target_se = target_se,
  weight = weight.lm,
  tol = 1e-8,
  max_iter = 1e5,
  max_starts = 1,
  init_temp = 1,
  cooling_rate = NULL
)
```

I then plotted these RMSE distributions side-by-side to compare within
versus between run variation.

``` r
plot_rmse(result.parallel.lm)
```

![Plot RMSE for
LM.](nds3_application_files/figure-html/plot%20rmse%20lm-1.png)
