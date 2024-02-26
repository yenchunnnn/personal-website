---
title: Model selection in the caret package
author: Yen Chun Chen
date: '2024-01-28'
slug: machine-learning-with-caret
categories:
  - R
  - Mechine Learning
tags:
  - Data Science
  - R Programming
subtitle: ''
summary: 'Comparing the performance of glmnet and gradient boosting with a real-world customer churn dataset.'
authors: []
lastmod: '2024-02-08T16:29:18+08:00'
featured: no
image:
  caption: 'Friends scenes from Reddit'
  focal_point: 'smart'
  preview_only: no
projects: []
output:
  blogdown::html_page:
    toc: true
---

**Mission**

-   Data: customer churn at telecom company

-   Task: predict which customers will churn

**Approach**

-   Fit different models and choose the best

-   Models must use the same training/test splits: `createFolds()`

-   Create a shared `trainControl` object

## Load dataframe


```r
# library package
library(tidyverse)
library(caret)
```




```r
# view training data
glimpse(churn_x)
```

```
## Rows: 250
## Columns: 70
## $ stateAK                       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ stateAL                       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ stateAR                       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ stateAZ                       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ stateCA                       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ stateCO                       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ stateCT                       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ stateDC                       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0…
## $ stateDE                       <int> 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ stateFL                       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ stateGA                       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ stateHI                       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ stateIA                       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ stateID                       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ stateIL                       <int> 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0…
## $ stateIN                       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ stateKS                       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ stateKY                       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ stateLA                       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ stateMA                       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ stateMD                       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ stateME                       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ stateMI                       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ stateMN                       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ stateMO                       <int> 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ stateMS                       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ stateMT                       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1…
## $ stateNC                       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ stateND                       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ stateNE                       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ stateNH                       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ stateNJ                       <int> 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0…
## $ stateNM                       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ stateNV                       <int> 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0…
## $ stateNY                       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ stateOH                       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ stateOK                       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ stateOR                       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ statePA                       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0…
## $ stateRI                       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ stateSC                       <int> 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ stateSD                       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ stateTN                       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ stateTX                       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ stateUT                       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ stateVA                       <int> 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0…
## $ stateVT                       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ stateWA                       <int> 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ stateWI                       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ stateWV                       <int> 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0…
## $ stateWY                       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ account_length                <int> 137, 83, 48, 67, 143, 163, 100, 151, 139…
## $ area_codearea_code_415        <int> 0, 1, 1, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 1…
## $ area_codearea_code_510        <int> 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0…
## $ international_planyes         <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ voice_mail_planyes            <int> 0, 0, 1, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0…
## $ number_vmail_messages         <int> 0, 0, 34, 0, 0, 0, 39, 0, 43, 30, 0, 0, …
## $ total_day_minutes             <dbl> 109.8, 196.7, 198.0, 164.5, 133.4, 202.9…
## $ total_day_calls               <int> 112, 117, 70, 79, 107, 100, 74, 106, 85,…
## $ total_day_charge              <dbl> 18.67, 33.44, 33.66, 27.97, 22.68, 34.49…
## $ total_eve_minutes             <dbl> 223.5, 272.0, 273.7, 110.3, 223.9, 178.6…
## $ total_eve_calls               <int> 88, 89, 121, 108, 117, 46, 80, 87, 82, 8…
## $ total_eve_charge              <dbl> 19.00, 23.12, 23.26, 9.38, 19.03, 15.18,…
## $ total_night_minutes           <dbl> 247.5, 199.9, 217.9, 203.9, 180.4, 203.8…
## $ total_night_calls             <int> 96, 62, 71, 102, 85, 116, 89, 88, 105, 1…
## $ total_night_charge            <dbl> 11.14, 9.00, 9.81, 9.18, 8.12, 9.17, 7.6…
## $ total_intl_minutes            <dbl> 17.8, 10.1, 7.6, 9.8, 10.2, 12.8, 11.2, …
## $ total_intl_calls              <int> 2, 11, 4, 2, 13, 3, 4, 5, 5, 2, 5, 11, 1…
## $ total_intl_charge             <dbl> 4.81, 2.73, 2.05, 2.65, 2.75, 3.46, 3.02…
## $ number_customer_service_calls <int> 1, 3, 1, 1, 1, 5, 2, 0, 2, 3, 2, 1, 3, 6…
```


```r
# see DV
str(churn_y)
```

```
##  Factor w/ 2 levels "no","yes": 1 1 1 1 1 1 1 1 1 1 ...
```

## Create a reusable trainControl

Use `createFolds()` to create 5 CV folds on target variable. And then Pass `myFold` to `index` to create a reusable `trainControl` for model comparison.


```r
# Create custom indices
set.seed(12556)
myFolds <- createFolds(churn_y, k = 5)

# Create reusable trainControl
myControl <- trainControl(
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  verboseIter = FALSE,
  savePredictions = TRUE,
  index = myFolds    # reuse to compare model
)
```

## Fit baseline model

### glmnet model

-   Linear model with built-in variable selection

-   Advantages

    -   Fits quickly

    -   Ignores noisy variables

    -   Provides interpretable coefficients (alpha & lambda)


```r
# Fit glmnet model
model_glmnet <- train(
  x = churn_x, 
  y = churn_y,
  metric = "ROC",
  method = "glmnet",
  trControl = myControl,
  tuneLength = 5
)

model_glmnet
```

```
## glmnet 
## 
## 250 samples
##  70 predictor
##   2 classes: 'no', 'yes' 
## 
## No pre-processing
## Resampling: Bootstrapped (5 reps) 
## Summary of sample sizes: 49, 49, 51, 50, 51 
## Resampling results across tuning parameters:
## 
##   alpha  lambda        ROC        Sens       Spec      
##   0.100  8.451035e-05  0.5551537  0.9472644  0.07907692
##   0.100  3.922623e-04  0.5551537  0.9472644  0.07907692
##   0.100  1.820720e-03  0.5551537  0.9472644  0.07907692
##   0.100  8.451035e-03  0.5551537  0.9472644  0.07907692
##   0.100  3.922623e-02  0.5533247  0.9564335  0.06338462
##   0.325  8.451035e-05  0.5488541  0.9346338  0.12615385
##   0.325  3.922623e-04  0.5488541  0.9346338  0.12615385
##   0.325  1.820720e-03  0.5488541  0.9346338  0.12615385
##   0.325  8.451035e-03  0.5478156  0.9392053  0.11076923
##   0.325  3.922623e-02  0.5426710  0.9518292  0.06369231
##   0.550  8.451035e-05  0.5419063  0.9288736  0.13384615
##   0.550  3.922623e-04  0.5419063  0.9288736  0.13384615
##   0.550  1.820720e-03  0.5419063  0.9288736  0.13384615
##   0.550  8.451035e-03  0.5366929  0.9346076  0.11846154
##   0.550  3.922623e-02  0.5309822  0.9506667  0.07969231
##   0.775  8.451035e-05  0.5350062  0.9277241  0.15784615
##   0.775  3.922623e-04  0.5350062  0.9277241  0.15784615
##   0.775  1.820720e-03  0.5349622  0.9277241  0.15784615
##   0.775  8.451035e-03  0.5336324  0.9368933  0.13415385
##   0.775  3.922623e-02  0.5231876  0.9518095  0.08738462
##   1.000  8.451035e-05  0.5281786  0.9231133  0.18123077
##   1.000  3.922623e-04  0.5281786  0.9231133  0.18123077
##   1.000  1.820720e-03  0.5282203  0.9242562  0.18123077
##   1.000  8.451035e-03  0.5272346  0.9334516  0.14984615
##   1.000  3.922623e-02  0.5175190  0.9518095  0.11046154
## 
## ROC was used to select the optimal model using the largest value.
## The final values used for the model were alpha = 0.1 and lambda = 0.008451035.
```

### Visualize results


```r
# result model
ggplot(model_glmnet)
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/unnamed-chunk-7-1.png" width="672" />

## Fit comparison model

### Gradient boosting

-   A powerful ensemble techniques that combines several weak learners into strong learners.

-   Each new model is trained to minimize the error of the previous model using gradient descent (so they're sequentially).

-   Usually get a higher performance than glmnet.


```r
set.seed(234)

# Fit gradient boosting
model_sgb <- train(
  x = churn_x, 
  y = churn_y,
  metric = "ROC",
  method = "gbm",
  trControl = myControl,
  tuneGrid = expand.grid(interaction.depth = 3:8,
                         shrinkage = 0.15,
                         n.trees = c(150, 200, 250),
                         n.minobsinnode = 2:3),
  verbose = F
)

model_sgb
```

```
## Stochastic Gradient Boosting 
## 
## 250 samples
##  70 predictor
##   2 classes: 'no', 'yes' 
## 
## No pre-processing
## Resampling: Bootstrapped (5 reps) 
## Summary of sample sizes: 49, 49, 51, 50, 51 
## Resampling results across tuning parameters:
## 
##   interaction.depth  n.minobsinnode  n.trees  ROC        Sens       Spec      
##   3                  2               150      0.6464600  0.9828243  0.07015385
##   3                  2               200      0.6460422  0.9828374  0.07815385
##   3                  2               250      0.6494706  0.9782529  0.07815385
##   3                  3               150      0.6372467  0.9656158  0.11661538
##   3                  3               200      0.6381379  0.9633300  0.11661538
##   3                  3               250      0.6367682  0.9656223  0.10923077
##   4                  2               150      0.6566869  0.9748112  0.08584615
##   4                  2               200      0.6627788  0.9713760  0.10153846
##   4                  2               250      0.6607893  0.9725255  0.09353846
##   4                  3               150      0.6302620  0.9690378  0.11692308
##   4                  3               200      0.6299384  0.9633103  0.12461538
##   4                  3               250      0.6295597  0.9633169  0.12461538
##   5                  2               150      0.6472853  0.9793892  0.06246154
##   5                  2               200      0.6508228  0.9805386  0.06246154
##   5                  2               250      0.6525976  0.9816814  0.07015385
##   5                  3               150      0.6275372  0.9621806  0.10123077
##   5                  3               200      0.6294927  0.9644663  0.10123077
##   5                  3               250      0.6332641  0.9690772  0.10923077
##   6                  2               150      0.6569559  0.9691100  0.10092308
##   6                  2               200      0.6589466  0.9691232  0.09292308
##   6                  2               250      0.6576701  0.9691166  0.10123077
##   6                  3               150      0.6253932  0.9690640  0.12461538
##   6                  3               200      0.6275645  0.9724992  0.12461538
##   6                  3               250      0.6284345  0.9713366  0.11692308
##   7                  2               150      0.6371456  0.9793957  0.07015385
##   7                  2               200      0.6442498  0.9793957  0.07815385
##   7                  2               250      0.6483931  0.9805386  0.06246154
##   7                  3               150      0.6268112  0.9621675  0.10092308
##   7                  3               200      0.6273295  0.9587323  0.10092308
##   7                  3               250      0.6289073  0.9598818  0.09323077
##   8                  2               150      0.6558310  0.9816814  0.08584615
##   8                  2               200      0.6583112  0.9828243  0.09353846
##   8                  2               250      0.6513941  0.9816814  0.07784615
##   8                  3               150      0.6259293  0.9770772  0.11692308
##   8                  3               200      0.6303931  0.9759475  0.11661538
##   8                  3               250      0.6279997  0.9690706  0.11692308
## 
## Tuning parameter 'shrinkage' was held constant at a value of 0.15
## ROC was used to select the optimal model using the largest value.
## The final values used for the model were n.trees = 200, interaction.depth =
##  4, shrinkage = 0.15 and n.minobsinnode = 2.
```

### Visualize results


```r
# visualize gradient boosting
ggplot(model_sgb)
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/unnamed-chunk-9-1.png" width="672" />

## Comparing models

Selection criteria:

-   Highest average ROC

-   Lowest standard deviation in ROC

### Create a resamples object

Let's evaluate their out-of-sample predictions and see which model performs best with the dataset.

`resamples()` function takes a list of models as input and can be used to compare dozens of models at once, but you must ensure that each model has the same training data --- that is, the same CV folds --- and `trainControl`.


```r
# Create model_list
model_list <- list(glmnet = model_glmnet, sgb = model_sgb)

# Pass model_list to resamples()
resamples <- resamples(model_list)

# Summarize the results
summary(resamples)
```

```
## 
## Call:
## summary.resamples(object = resamples)
## 
## Models: glmnet, sgb 
## Number of resamples: 5 
## 
## ROC 
##             Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
## glmnet 0.3381963 0.4347126 0.6250575 0.5551537 0.6701099 0.7076923    0
## sgb    0.5517241 0.6078161 0.6765517 0.6627788 0.7021978 0.7756044    0
## 
## Sens 
##             Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
## glmnet 0.9022989 0.9085714 0.9655172 0.9472644 0.9714286 0.9885057    0
## sgb    0.9371429 0.9657143 0.9770115 0.9713760 0.9770115 1.0000000    0
## 
## Spec 
##        Min.    1st Qu.     Median       Mean 3rd Qu.      Max. NA's
## glmnet    0 0.03846154 0.07692308 0.07907692    0.12 0.1600000    0
## sgb       0 0.08000000 0.11538462 0.10153846    0.12 0.1923077    0
```

### Plot resamples

**box-and-whisker plot**

A higher median (black dot) of the ROC as well as a smaller range between the min and max of the ROC are preferred.



```r
# Create bwplot
bwplot(resamples, metric = "ROC")
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/unnamed-chunk-11-1.png" width="672" />

Gradient boosting has higher median and smaller range of ROC, so it's better than glmnet.

**Scatter plot**

A scatter plot can help us determine whether one model outperforms the other across all folds.


```r
# Create xyplot
xyplot(resamples, metric = "ROC")
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/unnamed-chunk-12-1.png" width="672" />

There's one fold of data where glmnet is better than gradient boosting.


### Ensembling models

Another approach is using the `caretEnsemble` package to construct a list of caret models (`caretList`), which will use the same resampling folds too.



```r
library(caretEnsemble)

set.seed(234)

# creat caretList object
model_carlist <- caretList(
    x = churn_x,
    y = churn_y,
    metric="ROC",
    trControl = myControl,
    tuneList = list(
        glmnet = caretModelSpec(method = "glmnet", tuneLength = 5),
        gbm = caretModelSpec(method = "gbm", 
                             tuneGrid = expand.grid(interaction.depth = 3:8,
                                        shrinkage = 0.15,
                                        n.trees = c(150, 200, 250),
                                        n.minobsinnode = 2:3),
                             verbose = F)
    )
)

model_carlist
```

```
## $glmnet
## glmnet 
## 
## 250 samples
##  70 predictor
##   2 classes: 'no', 'yes' 
## 
## No pre-processing
## Resampling: Bootstrapped (5 reps) 
## Summary of sample sizes: 49, 49, 51, 50, 51 
## Resampling results across tuning parameters:
## 
##   alpha  lambda        ROC        Sens       Spec      
##   0.100  8.451035e-05  0.5551537  0.9472644  0.07907692
##   0.100  3.922623e-04  0.5551537  0.9472644  0.07907692
##   0.100  1.820720e-03  0.5551537  0.9472644  0.07907692
##   0.100  8.451035e-03  0.5551537  0.9472644  0.07907692
##   0.100  3.922623e-02  0.5533247  0.9564335  0.06338462
##   0.325  8.451035e-05  0.5488541  0.9346338  0.12615385
##   0.325  3.922623e-04  0.5488541  0.9346338  0.12615385
##   0.325  1.820720e-03  0.5488541  0.9346338  0.12615385
##   0.325  8.451035e-03  0.5478156  0.9392053  0.11076923
##   0.325  3.922623e-02  0.5426710  0.9518292  0.06369231
##   0.550  8.451035e-05  0.5419063  0.9288736  0.13384615
##   0.550  3.922623e-04  0.5419063  0.9288736  0.13384615
##   0.550  1.820720e-03  0.5419063  0.9288736  0.13384615
##   0.550  8.451035e-03  0.5366929  0.9346076  0.11846154
##   0.550  3.922623e-02  0.5309822  0.9506667  0.07969231
##   0.775  8.451035e-05  0.5350062  0.9277241  0.15784615
##   0.775  3.922623e-04  0.5350062  0.9277241  0.15784615
##   0.775  1.820720e-03  0.5349622  0.9277241  0.15784615
##   0.775  8.451035e-03  0.5336324  0.9368933  0.13415385
##   0.775  3.922623e-02  0.5231876  0.9518095  0.08738462
##   1.000  8.451035e-05  0.5281786  0.9231133  0.18123077
##   1.000  3.922623e-04  0.5281786  0.9231133  0.18123077
##   1.000  1.820720e-03  0.5282203  0.9242562  0.18123077
##   1.000  8.451035e-03  0.5272346  0.9334516  0.14984615
##   1.000  3.922623e-02  0.5175190  0.9518095  0.11046154
## 
## ROC was used to select the optimal model using the largest value.
## The final values used for the model were alpha = 0.1 and lambda = 0.008451035.
## 
## $gbm
## Stochastic Gradient Boosting 
## 
## 250 samples
##  70 predictor
##   2 classes: 'no', 'yes' 
## 
## No pre-processing
## Resampling: Bootstrapped (5 reps) 
## Summary of sample sizes: 49, 49, 51, 50, 51 
## Resampling results across tuning parameters:
## 
##   interaction.depth  n.minobsinnode  n.trees  ROC        Sens       Spec      
##   3                  2               150      0.6449739  0.9793826  0.10153846
##   3                  2               200      0.6491699  0.9748112  0.10153846
##   3                  2               250      0.6488036  0.9702397  0.09353846
##   3                  3               150      0.6169165  0.9725123  0.10153846
##   3                  3               200      0.6236021  0.9656223  0.10923077
##   3                  3               250      0.6264433  0.9656289  0.10923077
##   4                  2               150      0.6448506  0.9805386  0.07046154
##   4                  2               200      0.6490504  0.9793957  0.08584615
##   4                  2               250      0.6499107  0.9782529  0.08584615
##   4                  3               150      0.6161869  0.9667783  0.10861538
##   4                  3               200      0.6213995  0.9679146  0.11630769
##   4                  3               250      0.6218171  0.9667586  0.10861538
##   5                  2               150      0.6440553  0.9782397  0.10123077
##   5                  2               200      0.6457337  0.9816814  0.07046154
##   5                  2               250      0.6440682  0.9794089  0.07015385
##   5                  3               150      0.6053242  0.9679015  0.10923077
##   5                  3               200      0.6111365  0.9713498  0.11723077
##   5                  3               250      0.6188259  0.9644663  0.13230769
##   6                  2               150      0.6433727  0.9793892  0.09353846
##   6                  2               200      0.6474984  0.9770969  0.10123077
##   6                  2               250      0.6475152  0.9759606  0.10123077
##   6                  3               150      0.6230388  0.9667652  0.10953846
##   6                  3               200      0.6244639  0.9713563  0.12461538
##   6                  3               250      0.6237725  0.9702135  0.10892308
##   7                  2               150      0.6395733  0.9771100  0.08584615
##   7                  2               200      0.6380159  0.9759672  0.08584615
##   7                  2               250      0.6408867  0.9782529  0.08584615
##   7                  3               150      0.6224764  0.9724860  0.13230769
##   7                  3               200      0.6173061  0.9736355  0.12461538
##   7                  3               250      0.6187027  0.9724926  0.10892308
##   8                  2               150      0.6515137  0.9805386  0.06276923
##   8                  2               200      0.6526611  0.9793957  0.08584615
##   8                  2               250      0.6563210  0.9805386  0.07815385
##   8                  3               150      0.6181430  0.9564204  0.10892308
##   8                  3               200      0.6183506  0.9667521  0.11692308
##   8                  3               250      0.6248878  0.9656158  0.12430769
## 
## Tuning parameter 'shrinkage' was held constant at a value of 0.15
## ROC was used to select the optimal model using the largest value.
## The final values used for the model were n.trees = 250, interaction.depth =
##  8, shrinkage = 0.15 and n.minobsinnode = 2.
## 
## attr(,"class")
## [1] "caretList"
```

You can see that the attrition class of the object was `caretList`.

Now let's use logistic regression to ensemble the two models.


```r
# Create ensemble model
stack <- caretStack(model_carlist, method = "glm")

# Look at summary
summary(stack)
```

```
## 
## Call:
## NULL
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)   
## (Intercept)   0.2082     0.5244   0.397  0.69139   
## glmnet       -0.9513     0.4415  -2.155  0.03118 * 
## gbm          -1.3305     0.4486  -2.966  0.00302 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 765.13  on 999  degrees of freedom
## Residual deviance: 750.43  on 997  degrees of freedom
## AIC: 756.43
## 
## Number of Fisher Scoring iterations: 4
```

Finally, based on the plots and statistical results of comparing models, gradient boosting performs marginally better than the glmnet model on this churn data.

*Notice: The dataset is from DataCamp's exercise.*
