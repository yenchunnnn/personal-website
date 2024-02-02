---
title: Machine Learning with caret - model selection
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
summary: 'Predict which customers will churn at a real-world telecom company.'
authors: []
lastmod: '2024-01-28T16:29:18+08:00'
featured: no
image:
  caption: ''
  focal_point: ''
  preview_only: no
projects: []
output:
  blogdown::html_page:
    toc: true
---

**A real-world example**

-   Data: customer churn at telecom company

-   Task: predict which customers will cancel their service (or churn).

**Approach: selecting models**

-   Fit different models and choose the best: `glmnet` & `rf`

-   Models must use the same training/test splits: `createFolds()`

-   Create a shared `trainControl` object

    -   can use the same `summaryFunction` and tuning parameters for multiple models.

    -   don't have to repeat code when fitting multiple models.

    -   can compare models on the exact same training and test data.

## 1.Reusing a trainControl

The first order of business is to create a reusable `trainControl` object you can use to reliably compare them.


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

Use `createFolds()` to create 5 CV folds on target variable.


```r
# Create custom indices: myFolds
set.seed(12556)
myFolds <- createFolds(churn_y, k = 5)
myFolds
```

```
## $Fold1
##  [1]   3   6   8  15  19  40  48  59  65  71  80  84  85  86  88  91  93  94 117
## [20] 120 140 146 152 153 156 159 161 170 173 174 179 181 193 201 202 204 205 210
## [39] 212 216 219 220 221 224 232 238 239 241 244
## 
## $Fold2
##  [1]   2   5   9  23  24  29  33  35  37  43  44  46  56  58  61  69  74  77 102
## [20] 104 108 110 111 119 121 125 127 135 137 141 142 147 148 149 151 157 165 171
## [39] 178 183 186 192 200 217 218 230 233 234 243
## 
## $Fold3
##  [1]   1   4  25  27  28  34  36  38  49  57  62  87  95  96  99 100 105 107 113
## [20] 118 126 130 132 143 144 145 150 160 162 163 164 167 169 180 188 189 196 199
## [39] 203 209 214 215 223 225 229 231 236 245 246 247 250
## 
## $Fold4
##  [1]  10  11  14  17  30  41  52  53  63  64  68  73  75  76  78  79  81  83  90
## [20]  92  97  98 106 109 114 122 123 129 133 134 136 139 155 166 168 182 184 185
## [39] 194 195 206 208 211 213 222 235 237 242 248 249
## 
## $Fold5
##  [1]   7  12  13  16  18  20  21  22  26  31  32  39  42  45  47  50  51  54  55
## [20]  60  66  67  70  72  82  89 101 103 112 115 116 124 128 131 138 154 158 172
## [39] 175 176 177 187 190 191 197 198 207 226 227 228 240
```

Pass `myFold` to `index` to create a reusable `trainControl` for comparing models.


```r
# Create reusable trainControl object: myControl
myControl <- trainControl(
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE,
  savePredictions = TRUE,
  index = myFolds    # reuse to compare model
)
```

By saving the indexes in the train control, we can fit many models using the same CV folds.

## 2.Fit glmnet model

-   Linear model with built-in variable selection

-   Great baseline model

-   Advantages

    -   Fits quickly

    -   Ignores noisy variables

    -   Provides interpretable coefficients

### Baseline model

Now that we have a reusable `trainControl` object called `myControl`, we can start fitting different predictive models to the churn dataset and evaluate their predictive accuracy.

`glmnet`, which penalizes linear and logistic regression models on the size and number of coefficients to help prevent overfitting.


```r
# Fit glmnet model: model_glmnet
model_glmnet <- train(
  x = churn_x, 
  y = churn_y,
  metric = "ROC",
  method = "glmnet",
  trControl = myControl
)
```

```
## + Fold1: alpha=0.10, lambda=0.01821 
## - Fold1: alpha=0.10, lambda=0.01821 
## + Fold1: alpha=0.55, lambda=0.01821 
## - Fold1: alpha=0.55, lambda=0.01821 
## + Fold1: alpha=1.00, lambda=0.01821 
## - Fold1: alpha=1.00, lambda=0.01821 
## + Fold2: alpha=0.10, lambda=0.01821 
## - Fold2: alpha=0.10, lambda=0.01821 
## + Fold2: alpha=0.55, lambda=0.01821 
## - Fold2: alpha=0.55, lambda=0.01821 
## + Fold2: alpha=1.00, lambda=0.01821 
## - Fold2: alpha=1.00, lambda=0.01821 
## + Fold3: alpha=0.10, lambda=0.01821 
## - Fold3: alpha=0.10, lambda=0.01821 
## + Fold3: alpha=0.55, lambda=0.01821 
## - Fold3: alpha=0.55, lambda=0.01821 
## + Fold3: alpha=1.00, lambda=0.01821 
## - Fold3: alpha=1.00, lambda=0.01821 
## + Fold4: alpha=0.10, lambda=0.01821 
## - Fold4: alpha=0.10, lambda=0.01821 
## + Fold4: alpha=0.55, lambda=0.01821 
## - Fold4: alpha=0.55, lambda=0.01821 
## + Fold4: alpha=1.00, lambda=0.01821 
## - Fold4: alpha=1.00, lambda=0.01821 
## + Fold5: alpha=0.10, lambda=0.01821 
## - Fold5: alpha=0.10, lambda=0.01821 
## + Fold5: alpha=0.55, lambda=0.01821 
## - Fold5: alpha=0.55, lambda=0.01821 
## + Fold5: alpha=1.00, lambda=0.01821 
## - Fold5: alpha=1.00, lambda=0.01821 
## Aggregating results
## Selecting tuning parameters
## Fitting alpha = 0.1, lambda = 0.00182 on full training set
```


```r
# print model
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
##   alpha  lambda       ROC        Sens       Spec      
##   0.10   0.000182072  0.5551537  0.9472644  0.07907692
##   0.10   0.001820720  0.5551537  0.9472644  0.07907692
##   0.10   0.018207203  0.5548023  0.9484072  0.07138462
##   0.55   0.000182072  0.5419063  0.9288736  0.13384615
##   0.55   0.001820720  0.5419063  0.9288736  0.13384615
##   0.55   0.018207203  0.5326096  0.9460755  0.11076923
##   1.00   0.000182072  0.5281786  0.9231133  0.18123077
##   1.00   0.001820720  0.5282203  0.9242562  0.18123077
##   1.00   0.018207203  0.5237805  0.9472184  0.11815385
## 
## ROC was used to select the optimal model using the largest value.
## The final values used for the model were alpha = 0.1 and lambda = 0.00182072.
```

### Visualize results


```r
# result model
plot(model_glmnet)
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/unnamed-chunk-9-1.png" width="672" />

Plot the coefficients


```r
# Plot coefficients
plot(model_glmnet$finalModel)
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/unnamed-chunk-10-1.png" width="672" />

See how our best model evolves as we increase or decrease the penalty on the coefficients.

## 3.Fit random forest model

-   Slower to fit than glmnet

-   Less interpretable

-   Often (but not always) more accurate than glmnet

-   Easier to tune

-   Require little preprocessing

-   Capture threshold effects and variable interactions

### Comparison model

Random forest, which combines an ensemble of non-linear decision trees into a highly flexible (and usually quite accurate) model.


```r
# Fit random forest: model_rf
model_rf <- train(
  x = churn_x, 
  y = churn_y,
  metric = "ROC",
  method = "ranger",
  trControl = myControl
)
```

```
## + Fold1: mtry= 2, min.node.size=1, splitrule=gini 
## - Fold1: mtry= 2, min.node.size=1, splitrule=gini 
## + Fold1: mtry=36, min.node.size=1, splitrule=gini 
## - Fold1: mtry=36, min.node.size=1, splitrule=gini 
## + Fold1: mtry=70, min.node.size=1, splitrule=gini 
## - Fold1: mtry=70, min.node.size=1, splitrule=gini 
## + Fold1: mtry= 2, min.node.size=1, splitrule=extratrees 
## - Fold1: mtry= 2, min.node.size=1, splitrule=extratrees 
## + Fold1: mtry=36, min.node.size=1, splitrule=extratrees 
## - Fold1: mtry=36, min.node.size=1, splitrule=extratrees 
## + Fold1: mtry=70, min.node.size=1, splitrule=extratrees 
## - Fold1: mtry=70, min.node.size=1, splitrule=extratrees 
## + Fold2: mtry= 2, min.node.size=1, splitrule=gini 
## - Fold2: mtry= 2, min.node.size=1, splitrule=gini 
## + Fold2: mtry=36, min.node.size=1, splitrule=gini 
## - Fold2: mtry=36, min.node.size=1, splitrule=gini 
## + Fold2: mtry=70, min.node.size=1, splitrule=gini 
## - Fold2: mtry=70, min.node.size=1, splitrule=gini 
## + Fold2: mtry= 2, min.node.size=1, splitrule=extratrees 
## - Fold2: mtry= 2, min.node.size=1, splitrule=extratrees 
## + Fold2: mtry=36, min.node.size=1, splitrule=extratrees 
## - Fold2: mtry=36, min.node.size=1, splitrule=extratrees 
## + Fold2: mtry=70, min.node.size=1, splitrule=extratrees 
## - Fold2: mtry=70, min.node.size=1, splitrule=extratrees 
## + Fold3: mtry= 2, min.node.size=1, splitrule=gini 
## - Fold3: mtry= 2, min.node.size=1, splitrule=gini 
## + Fold3: mtry=36, min.node.size=1, splitrule=gini 
## - Fold3: mtry=36, min.node.size=1, splitrule=gini 
## + Fold3: mtry=70, min.node.size=1, splitrule=gini 
## - Fold3: mtry=70, min.node.size=1, splitrule=gini 
## + Fold3: mtry= 2, min.node.size=1, splitrule=extratrees 
## - Fold3: mtry= 2, min.node.size=1, splitrule=extratrees 
## + Fold3: mtry=36, min.node.size=1, splitrule=extratrees 
## - Fold3: mtry=36, min.node.size=1, splitrule=extratrees 
## + Fold3: mtry=70, min.node.size=1, splitrule=extratrees 
## - Fold3: mtry=70, min.node.size=1, splitrule=extratrees 
## + Fold4: mtry= 2, min.node.size=1, splitrule=gini 
## - Fold4: mtry= 2, min.node.size=1, splitrule=gini 
## + Fold4: mtry=36, min.node.size=1, splitrule=gini 
## - Fold4: mtry=36, min.node.size=1, splitrule=gini 
## + Fold4: mtry=70, min.node.size=1, splitrule=gini 
## - Fold4: mtry=70, min.node.size=1, splitrule=gini 
## + Fold4: mtry= 2, min.node.size=1, splitrule=extratrees 
## - Fold4: mtry= 2, min.node.size=1, splitrule=extratrees 
## + Fold4: mtry=36, min.node.size=1, splitrule=extratrees 
## - Fold4: mtry=36, min.node.size=1, splitrule=extratrees 
## + Fold4: mtry=70, min.node.size=1, splitrule=extratrees 
## - Fold4: mtry=70, min.node.size=1, splitrule=extratrees 
## + Fold5: mtry= 2, min.node.size=1, splitrule=gini 
## - Fold5: mtry= 2, min.node.size=1, splitrule=gini 
## + Fold5: mtry=36, min.node.size=1, splitrule=gini 
## - Fold5: mtry=36, min.node.size=1, splitrule=gini 
## + Fold5: mtry=70, min.node.size=1, splitrule=gini 
## - Fold5: mtry=70, min.node.size=1, splitrule=gini 
## + Fold5: mtry= 2, min.node.size=1, splitrule=extratrees 
## - Fold5: mtry= 2, min.node.size=1, splitrule=extratrees 
## + Fold5: mtry=36, min.node.size=1, splitrule=extratrees 
## - Fold5: mtry=36, min.node.size=1, splitrule=extratrees 
## + Fold5: mtry=70, min.node.size=1, splitrule=extratrees 
## - Fold5: mtry=70, min.node.size=1, splitrule=extratrees 
## Aggregating results
## Selecting tuning parameters
## Fitting mtry = 36, splitrule = gini, min.node.size = 1 on full training set
```


```r
# print model
model_rf
```

```
## Random Forest 
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
##   mtry  splitrule   ROC        Sens       Spec      
##    2    gini        0.6098964  1.0000000  0.00000000
##    2    extratrees  0.5478626  1.0000000  0.00000000
##   36    gini        0.6469126  0.9828309  0.03938462
##   36    extratrees  0.6366564  0.9724598  0.06369231
##   70    gini        0.6451724  0.9759606  0.07815385
##   70    extratrees  0.6399428  0.9552578  0.09507692
## 
## Tuning parameter 'min.node.size' was held constant at a value of 1
## ROC was used to select the optimal model using the largest value.
## The final values used for the model were mtry = 36, splitrule = gini
##  and min.node.size = 1.
```

### Visualize results


```r
# visualize random forest model
plot(model_rf)
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/unnamed-chunk-13-1.png" width="672" />

This random forest uses the custom CV folds, so we can easily compare it to the baseline model.

## 4.Comparing models

-   Make sure they were fit on the same data!

-   Selection criteria

    -   Highest average AUC

    -   Lowest standard deviation in AUC

-   Use the `resamples()` function 

### Create resamples object

Now let's compare their out-of-sample predictions and choose which one is the best model for the dataset.

Using the `resamples()` function, provided they have the *same training data* and use the *same `trainControl` object* with preset cross-validation folds.

`resamples()` takes as input *a list of models* and can be used to compare dozens of models at once.


```r
# Create model_list
model_list <- list(glmnet = model_glmnet, rf = model_rf)

# Pass model_list to resamples(): resamples
resamples <- resamples(model_list)

# Summarize the results
summary(resamples)
```

```
## 
## Call:
## summary.resamples(object = resamples)
## 
## Models: glmnet, rf 
## Number of resamples: 5 
## 
## ROC 
##             Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
## glmnet 0.3381963 0.4347126 0.6250575 0.5551537 0.6701099 0.7076923    0
## rf     0.5100575 0.5479310 0.6808791 0.6469126 0.7320690 0.7636264    0
## 
## Sens 
##             Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
## glmnet 0.9022989 0.9085714 0.9655172 0.9472644 0.9714286 0.9885057    0
## rf     0.9371429 0.9827586 0.9942529 0.9828309 1.0000000 1.0000000    0
## 
## Spec 
##        Min.    1st Qu.     Median       Mean    3rd Qu. Max. NA's
## glmnet    0 0.03846154 0.07692308 0.07907692 0.12000000 0.16    0
## rf        0 0.00000000 0.04000000 0.03938462 0.07692308 0.08    0
```

### Plot resamples

**box-and-whisker plot**

In general, we want the model with the higher median AUC, as well as a smaller range between min and max AUC.

If you do not specify a metric to plot, `bwplot()` will automatically plot 3 of them (ROC, Sens, Spec).

Black dot is the median.


```r
# Create bwplot
bwplot(resamples, metric = "ROC")
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/unnamed-chunk-15-1.png" width="672" />

**Scatter plot**

It's particularly useful for identifying if one model is consistently better than the other across all folds, or if there are situations when the inferior model produces better predictions on a particular subset of the data.


```r
# Create xyplot
xyplot(resamples, metric = "ROC")
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/unnamed-chunk-16-1.png" width="672" />

**Dot plot**


```r
dotplot(resamples, metric = "ROC")
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/unnamed-chunk-17-1.png" width="672" />

**Density plot**


```r
densityplot(resamples, metric = "ROC")
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/unnamed-chunk-18-1.png" width="672" />

### Ensembling models

Below show how to fit a stacked ensemble of models using the `caretEnsemble` package.

`caretEnsemble` provides the `caretList()` function for creating multiple `caret` models at once on the same dataset, using the same resampling folds. We can also create our own lists of `caret` models.

Here, use the `caretStack()` function to make a stack of `caret` models, with the two sub-models (`glmnet` and `ranger`) feeding into another `caret` model.


```r
# creat caretList object
model_list <- caretEnsemble::caretList(
    x = churn_x,
    y = churn_y,
    methodList = c("glmnet", "ranger"),
    trControl = myControl
)
```

```
## + Fold1: alpha=0.10, lambda=0.01821 
## - Fold1: alpha=0.10, lambda=0.01821 
## + Fold1: alpha=0.55, lambda=0.01821 
## - Fold1: alpha=0.55, lambda=0.01821 
## + Fold1: alpha=1.00, lambda=0.01821 
## - Fold1: alpha=1.00, lambda=0.01821 
## + Fold2: alpha=0.10, lambda=0.01821 
## - Fold2: alpha=0.10, lambda=0.01821 
## + Fold2: alpha=0.55, lambda=0.01821 
## - Fold2: alpha=0.55, lambda=0.01821 
## + Fold2: alpha=1.00, lambda=0.01821 
## - Fold2: alpha=1.00, lambda=0.01821 
## + Fold3: alpha=0.10, lambda=0.01821 
## - Fold3: alpha=0.10, lambda=0.01821 
## + Fold3: alpha=0.55, lambda=0.01821 
## - Fold3: alpha=0.55, lambda=0.01821 
## + Fold3: alpha=1.00, lambda=0.01821 
## - Fold3: alpha=1.00, lambda=0.01821 
## + Fold4: alpha=0.10, lambda=0.01821 
## - Fold4: alpha=0.10, lambda=0.01821 
## + Fold4: alpha=0.55, lambda=0.01821 
## - Fold4: alpha=0.55, lambda=0.01821 
## + Fold4: alpha=1.00, lambda=0.01821 
## - Fold4: alpha=1.00, lambda=0.01821 
## + Fold5: alpha=0.10, lambda=0.01821 
## - Fold5: alpha=0.10, lambda=0.01821 
## + Fold5: alpha=0.55, lambda=0.01821 
## - Fold5: alpha=0.55, lambda=0.01821 
## + Fold5: alpha=1.00, lambda=0.01821 
## - Fold5: alpha=1.00, lambda=0.01821 
## Aggregating results
## Selecting tuning parameters
## Fitting alpha = 0.1, lambda = 0.00182 on full training set
## + Fold1: mtry= 2, min.node.size=1, splitrule=gini 
## - Fold1: mtry= 2, min.node.size=1, splitrule=gini 
## + Fold1: mtry=36, min.node.size=1, splitrule=gini 
## - Fold1: mtry=36, min.node.size=1, splitrule=gini 
## + Fold1: mtry=70, min.node.size=1, splitrule=gini 
## - Fold1: mtry=70, min.node.size=1, splitrule=gini 
## + Fold1: mtry= 2, min.node.size=1, splitrule=extratrees 
## - Fold1: mtry= 2, min.node.size=1, splitrule=extratrees 
## + Fold1: mtry=36, min.node.size=1, splitrule=extratrees 
## - Fold1: mtry=36, min.node.size=1, splitrule=extratrees 
## + Fold1: mtry=70, min.node.size=1, splitrule=extratrees 
## - Fold1: mtry=70, min.node.size=1, splitrule=extratrees 
## + Fold2: mtry= 2, min.node.size=1, splitrule=gini 
## - Fold2: mtry= 2, min.node.size=1, splitrule=gini 
## + Fold2: mtry=36, min.node.size=1, splitrule=gini 
## - Fold2: mtry=36, min.node.size=1, splitrule=gini 
## + Fold2: mtry=70, min.node.size=1, splitrule=gini 
## - Fold2: mtry=70, min.node.size=1, splitrule=gini 
## + Fold2: mtry= 2, min.node.size=1, splitrule=extratrees 
## - Fold2: mtry= 2, min.node.size=1, splitrule=extratrees 
## + Fold2: mtry=36, min.node.size=1, splitrule=extratrees 
## - Fold2: mtry=36, min.node.size=1, splitrule=extratrees 
## + Fold2: mtry=70, min.node.size=1, splitrule=extratrees 
## - Fold2: mtry=70, min.node.size=1, splitrule=extratrees 
## + Fold3: mtry= 2, min.node.size=1, splitrule=gini 
## - Fold3: mtry= 2, min.node.size=1, splitrule=gini 
## + Fold3: mtry=36, min.node.size=1, splitrule=gini 
## - Fold3: mtry=36, min.node.size=1, splitrule=gini 
## + Fold3: mtry=70, min.node.size=1, splitrule=gini 
## - Fold3: mtry=70, min.node.size=1, splitrule=gini 
## + Fold3: mtry= 2, min.node.size=1, splitrule=extratrees 
## - Fold3: mtry= 2, min.node.size=1, splitrule=extratrees 
## + Fold3: mtry=36, min.node.size=1, splitrule=extratrees 
## - Fold3: mtry=36, min.node.size=1, splitrule=extratrees 
## + Fold3: mtry=70, min.node.size=1, splitrule=extratrees 
## - Fold3: mtry=70, min.node.size=1, splitrule=extratrees 
## + Fold4: mtry= 2, min.node.size=1, splitrule=gini 
## - Fold4: mtry= 2, min.node.size=1, splitrule=gini 
## + Fold4: mtry=36, min.node.size=1, splitrule=gini 
## - Fold4: mtry=36, min.node.size=1, splitrule=gini 
## + Fold4: mtry=70, min.node.size=1, splitrule=gini 
## - Fold4: mtry=70, min.node.size=1, splitrule=gini 
## + Fold4: mtry= 2, min.node.size=1, splitrule=extratrees 
## - Fold4: mtry= 2, min.node.size=1, splitrule=extratrees 
## + Fold4: mtry=36, min.node.size=1, splitrule=extratrees 
## - Fold4: mtry=36, min.node.size=1, splitrule=extratrees 
## + Fold4: mtry=70, min.node.size=1, splitrule=extratrees 
## - Fold4: mtry=70, min.node.size=1, splitrule=extratrees 
## + Fold5: mtry= 2, min.node.size=1, splitrule=gini 
## - Fold5: mtry= 2, min.node.size=1, splitrule=gini 
## + Fold5: mtry=36, min.node.size=1, splitrule=gini 
## - Fold5: mtry=36, min.node.size=1, splitrule=gini 
## + Fold5: mtry=70, min.node.size=1, splitrule=gini 
## - Fold5: mtry=70, min.node.size=1, splitrule=gini 
## + Fold5: mtry= 2, min.node.size=1, splitrule=extratrees 
## - Fold5: mtry= 2, min.node.size=1, splitrule=extratrees 
## + Fold5: mtry=36, min.node.size=1, splitrule=extratrees 
## - Fold5: mtry=36, min.node.size=1, splitrule=extratrees 
## + Fold5: mtry=70, min.node.size=1, splitrule=extratrees 
## - Fold5: mtry=70, min.node.size=1, splitrule=extratrees 
## Aggregating results
## Selecting tuning parameters
## Fitting mtry = 70, splitrule = gini, min.node.size = 1 on full training set
```


```r
# print model list
model_list
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
##   alpha  lambda       ROC        Sens       Spec      
##   0.10   0.000182072  0.5551537  0.9472644  0.07907692
##   0.10   0.001820720  0.5551537  0.9472644  0.07907692
##   0.10   0.018207203  0.5548023  0.9484072  0.07138462
##   0.55   0.000182072  0.5419063  0.9288736  0.13384615
##   0.55   0.001820720  0.5419063  0.9288736  0.13384615
##   0.55   0.018207203  0.5326096  0.9460755  0.11076923
##   1.00   0.000182072  0.5281786  0.9231133  0.18123077
##   1.00   0.001820720  0.5282203  0.9242562  0.18123077
##   1.00   0.018207203  0.5237805  0.9472184  0.11815385
## 
## ROC was used to select the optimal model using the largest value.
## The final values used for the model were alpha = 0.1 and lambda = 0.00182072.
## 
## $ranger
## Random Forest 
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
##   mtry  splitrule   ROC        Sens       Spec      
##    2    gini        0.6208736  1.0000000  0.00000000
##    2    extratrees  0.5559884  1.0000000  0.00000000
##   36    gini        0.6489453  0.9793957  0.03138462
##   36    extratrees  0.6291936  0.9804729  0.04000000
##   70    gini        0.6527580  0.9759606  0.07015385
##   70    extratrees  0.6212003  0.9541149  0.09507692
## 
## Tuning parameter 'min.node.size' was held constant at a value of 1
## ROC was used to select the optimal model using the largest value.
## The final values used for the model were mtry = 70, splitrule = gini
##  and min.node.size = 1.
## 
## attr(,"class")
## [1] "caretList"
```

Ensemble the two models using a logistic regression.


```r
# Create ensemble model: stack
stack <- caretEnsemble::caretStack(model_list, method = "glm")

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
## (Intercept)   0.8637     0.4937   1.749   0.0802 .  
## glmnet        0.2854     0.5220   0.547   0.5846    
## ranger       -3.5332     0.6145  -5.750 8.94e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 765.13  on 999  degrees of freedom
## Residual deviance: 726.89  on 997  degrees of freedom
## AIC: 732.89
## 
## Number of Fisher Scoring iterations: 4
```

In conclusion, we can see from the output that **random forest model is better than glmnet model on churn data**.


*Notice: this content is based on Datacamp's exercise.*
