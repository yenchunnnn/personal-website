---
title: Classifying depressed and control groups by motor activity time series data
author: Yen-Chun Chen
date: '2024-02-16'
slug: classifying-depressed-and-healthy-controls
categories:
  - R
  - Mechine Learning
tags:
  - Data Science
  - Classification
  - R Programming
  - Time Series
  - Clinical Psychology
subtitle: ''
summary: 'Applying logistic regression, random forest, and SVM to analyze the daily motor activity patterns of participants.'
authors: []
lastmod: '2024-02-16T12:59:04+08:00'
featured: no
image:
  caption: 'Friends scenes from NBC'
  focal_point: 'smart'
  preview_only: no
projects: []
output:
  blogdown::html_page:
    toc: true
---

## Introduction

Depressive disorders is a cluster of mental disorders characterized by the presence of sad, empty, or irritable mood, accompanied by somatic and cognitive changes that significantly affect the individual's capacity to function. [^1]

[^1]: American Psychiatric Association. (2013). Diagnostic and statistical manual of mental disorders (5th ed.). <https://doi.org/10.1176/appi.books.9780890425596>

The traditional practice of assessing depressed mood states largely depends on subjective reports or observations made by others, combined with structured clinical rating scales. However, subjectiveness has the potential to lead to bias and misdiagnosis, which will affect the recovery and subsequent treatment as well as intervention. Therefore, when making a clinical diagnosis, several objective indicators may be taken into account.

Alteration of motor activity (psychomotor) is one of the diagnostic criteria in depressive disorders. The depressive state is often associated with lower mean activity, higher intraindividual variability, and less complexity in activity patterns when compared to healthy controls. [^2] [^3] And wearable technology-based ecological momentary assessment allows us to collect objective and real-time data on participants' daily motor activity.

[^2]: Burton, C., McKinstry, B., Szentagotai Tătar, A., Serrano-Blanco, A., Pagliari, C., & Wolters, M. (2013). Activity monitoring in patients with depression: a systematic review. *Journal of affective disorders, 145(1)*, 21--28. <https://doi.org/10.1016/j.jad.2012.07.001>

[^3]: Krane-Gartiser, K., Henriksen, T. E., Vaaler, A. E., Fasmer, O. B., & Morken, G. (2015). Actigraphically assessed activity in unipolar depression: a comparison of inpatients with and without motor retardation. *The Journal of clinical psychiatry, 76(9)*, 1181--1187. <https://doi.org/10.4088/JCP.14m09106>

So the aim of this project was to investigate whether objective biological metrics may improve on traditional diagnosis procedures by analyzing motor activity patterns in both healthy and depressed participants using machine learning techniques.

## Preprocessing


```r
# Library package
library(tidyverse)
library(rsample)
library(caret)
library(vip)
library(tsfeatures)
```

### Setting up dataframe

**Data description**

The Depresjon dataset is a collection of data that includes the motor activity of participants measured with a wrist-worn actigraph.[^4] The sampling frequency is 32Hz and movements greater than 0.05g are recorded. The number of counts corresponds to the intensity of the movement. Total activity counts were continually recorded at one-minute intervals. The following files contain:

[^4]: Garcia-Ceja, E., Riegler, M., Jakobsen, P., Torresen, J., Nordgreen, T., Oedegaard, K. J., & Fasmer, O. B. (2018). DEPRESJON dataset [Data set]. Zenodo. <https://doi.org/10.5281/zenodo.1219550>

``` markmap{height="200px"}
- Depresjon dataset
  - Condition: 23 depressed participants
    - timestamp (one minute intervals)
    - date (date of measurement): inconsistency for every participant.
    - activity
  - Control: 32 healthy participants
    - timestamp (one minute intervals)
    - date (date of measurement): inconsistency for every participant.
    - activity
  - Scores: all participants
    - Demographic
    - Severity of depression
```

We only used actigraph data in this analysis.




```r
# Load actigraph files

# condition files
condition_file <- list.files(path = condition_path, pattern = '*.csv', full.names = T)

# control files
control_file <- list.files(path = control_path, pattern = '*.csv', full.names = T)

# number of files
c(condition = length(condition_file), control = length(control_file))
```

```
## condition   control 
##        23        32
```

Create a condition data frame.


```r
# read condition files
condition_data <- read_csv(condition_file, id = "ID") %>%
    mutate(ID = parse_number(ID),
           timestamp = as.POSIXct(str_replace_all(timestamp, "/", "-")),
           date = as_date(str_replace_all(date, "/", "-")),
           group = 'condition')
glimpse(condition_data)
```

```
## Rows: 551,716
## Columns: 5
## $ ID        <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
## $ timestamp <dttm> 2003-05-07 12:00:00, 2003-05-07 12:01:00, 2003-05-07 12:02:…
## $ date      <date> 2003-05-07, 2003-05-07, 2003-05-07, 2003-05-07, 2003-05-07,…
## $ activity  <dbl> 0, 143, 0, 20, 166, 160, 17, 646, 978, 306, 277, 439, 130, 3…
## $ group     <chr> "condition", "condition", "condition", "condition", "conditi…
```

Create a control data frame.


```r
# read control files
control_data <- read_csv(control_file, id = "ID") %>%
    mutate(ID = parse_number(ID) + 23,
           group = 'control')
glimpse(control_data)
```

```
## Rows: 1,019,990
## Columns: 5
## $ ID        <dbl> 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, …
## $ timestamp <dttm> 2003-03-18 15:00:00, 2003-03-18 15:01:00, 2003-03-18 15:02:…
## $ date      <date> 2003-03-18, 2003-03-18, 2003-03-18, 2003-03-18, 2003-03-18,…
## $ activity  <dbl> 60, 0, 264, 662, 293, 0, 322, 1102, 1067, 239, 1174, 121, 37…
## $ group     <chr> "control", "control", "control", "control", "control", "cont…
```

Merge two data frames.


```r
# combine control and condition data
full_df <- rbind(condition_data, control_data) %>%
    mutate(group = as.factor(group))

head(full_df)
```

```
## # A tibble: 6 × 5
##      ID timestamp           date       activity group    
##   <dbl> <dttm>              <date>        <dbl> <fct>    
## 1     1 2003-05-07 12:00:00 2003-05-07        0 condition
## 2     1 2003-05-07 12:01:00 2003-05-07      143 condition
## 3     1 2003-05-07 12:02:00 2003-05-07        0 condition
## 4     1 2003-05-07 12:03:00 2003-05-07       20 condition
## 5     1 2003-05-07 12:04:00 2003-05-07      166 condition
## 6     1 2003-05-07 12:05:00 2003-05-07      160 condition
```

Notice that this is a multilevel data frame:

-   Level 5: group

-   Level 4: subject

-   Level 3: date

-   Level 2: hour

-   Level 1: minute

### Cleaning data

Visualize the data to see overall patterns.


```r
# nest by ID
nest_df <- full_df %>%
    group_by(ID) %>%
    nest() %>%
    arrange(ID)

# plot function
plot_fun <- function(df) {
    ggplot(df, aes(timestamp, activity)) +
        geom_line() +
        theme(axis.text.x = element_blank())
}

# create plot columns
nest_df <- nest_df %>%
    mutate(plot = map(data, plot_fun))

# condition plots
grid.arrange(grobs = nest_df$plot[1:23], ncol = 4)
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/unnamed-chunk-7-1.png" width="672" />

```r
# control plots, cause it's too many plots if we put all of them in one figure,
# we'll separate plotting them
grid.arrange(grobs = nest_df$plot[24:39], ncol = 4)
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/unnamed-chunk-7-2.png" width="672" />

```r
grid.arrange(grobs = nest_df$plot[40:55], ncol = 4)
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/unnamed-chunk-7-3.png" width="672" />

It appears that there were days when subjects did not record activities; thus, we must delete those days. Mean activity of a day that lower than the threshold (threshold = 25) was removed. In this sense, we should preserve within-day variability.


```r
# filter days where doesn't fit threshold
clean_df <- full_df %>%
    group_by(ID, date) %>%
    mutate(mean_perday = mean(activity)) %>%
    # mean activity per day threshold: 25
    filter(mean_perday > 25)

# see the rows difference
list(full = dim(full_df), clean_subact = dim(clean_df))
```

```
## $full
## [1] 1571706       5
## 
## $clean_subact
## [1] 1202071       6
```

There may be some days that's not a record for 24 hours, which means fewer than 24\*60 = 1440 minutes.


```r
# check if there any day that's < 1440 mins
clean_df %>% 
    group_by(ID, date) %>%
    count() %>%
    filter(n < 1440)
```

```
## # A tibble: 73 × 3
## # Groups:   ID, date [73]
##       ID date           n
##    <dbl> <date>     <int>
##  1     1 2003-05-07   720
##  2     2 2003-05-07   540
##  3     2 2003-06-03   946
##  4     3 2003-06-03   948
##  5     4 2003-06-03   721
##  6     5 2003-06-12   810
##  7     6 2003-08-19   720
##  8     7 2004-05-04   840
##  9     7 2004-05-19  1175
## 10     8 2004-05-06   840
## # ℹ 63 more rows
```


```r
# remove the rows that's not completed
clean_df <- clean_df %>%
    group_by(ID, date) %>%
    mutate(minutes = length(timestamp)) %>%
    filter(minutes == 1440)

# see the difference
list(full = dim(full_df), clean_comact = dim(clean_df))
```

```
## $full
## [1] 1571706       5
## 
## $clean_comact
## [1] 1140480       7
```

## Exploratory Data Analysis

### Day between groups


```r
# summary of the days between groups
day_df <- clean_df %>% 
    group_by(ID) %>% 
    summarise(day = length(unique(date))) %>%
    mutate(group = as.factor(ifelse(ID <= 23, 'condition', 'control')))

psych::describeBy(day_df$day, group = day_df$group)
```

```
## 
##  Descriptive statistics by group 
## group: condition
##    vars  n mean   sd median trimmed mad min max range skew kurtosis   se
## X1    1 23 14.3 1.79     14   14.16   0  10  19     9 0.64     1.53 0.37
## ------------------------------------------------------------ 
## group: control
##    vars  n  mean   sd median trimmed mad min max range skew kurtosis   se
## X1    1 32 14.47 1.41     14   14.15   0  13  21     8 3.28    11.68 0.25
```


```r
# sum of days
day_df %>% 
    group_by(group) %>%
    summarise(count = sum(day))
```

```
## # A tibble: 2 × 2
##   group     count
##   <fct>     <int>
## 1 condition   329
## 2 control     463
```

|       | Condition | Control |
|-------|-----------|---------|
| total | 329       | 463     |
| mean  | 14.30     | 14.47   |
| sd    | 1.79      | 1.41    |
| min   | 10        | 13      |
| max   | 19        | 21      |

: Descriptive statistics of collected days by group

### Activity between groups

#### Overall 24-hour span

We primarily chose three statistical features that were extracted from the *date level* in this step.[^5] [^6]

[^5]: Garcia-Ceja, E., Riegler, M., Jakobsen, P., Tørresen, J., Nordgreen, T., Oedegaard, K.J., & Fasmer, O.B. (2018). Depresjon: a motor activity database of depression episodes in unipolar and bipolar patients. *Proceedings of the 9th ACM Multimedia Systems Conference.*

[^6]: Jakobsen, P., Garcia-Ceja, E., Riegler, M., Stabell, L. A., Nordgreen, T., Torresen, J., Fasmer, O. B., & Oedegaard, K. J. (2020). Applying machine learning in motor activity time series of depressed bipolar and unipolar patients compared to healthy controls. *PloS one, 15(8),* e0231995. <https://doi.org/10.1371/journal.pone.0231995>

Which are:

-   Mean activity

-   Standard deviation of activity

-   The proportion of minutes with an activity level of zero within a day

Based on previous studies, we may hypothesize that the condition group would have *lower mean activity*, a *higher variability of activity*, and a *higher zero activity of proportion* in a day compared to the control group.

Because the activity isn't a normal distribution (e.g., condition group would have more low activity levels than high activity levels), we have to normalize it before further calculation. Here we used log transformation to adjust skewness.


```r
# calculate mean activity after log transform
clean_df <- clean_df %>% 
    group_by(ID, date) %>% 
    mutate(log_activity = log(activity + 1),
           mean_activity = mean(log_activity),
           sd_activity = sd(log_activity), 
           zero_prop = zero_proportion(log_activity))

# select needed columns and filter rows
group_act <- clean_df %>%
    select(ID, group, date, mean_activity, sd_activity, zero_prop) %>%
    distinct() 

# box plot for this three vars
reshape2::melt(group_act[, c(2, 4:6)], id.vars = "group") %>%
    # Everything on the same plot
    ggplot(., aes(group, value, col = variable)) + 
      geom_boxplot()
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/unnamed-chunk-13-1.png" width="672" />

It seems like there's still some noisy (outlier) activity, even though we've already log transformed data.


```r
# summarise group activity
group_act %>%
    group_by(group) %>%
    summarise(mean = mean(mean_activity),
              sd = mean(sd_activity),
              zero_prop = mean(zero_prop))
```

```
## # A tibble: 2 × 4
##   group      mean    sd zero_prop
##   <fct>     <dbl> <dbl>     <dbl>
## 1 condition  2.80  2.50     0.407
## 2 control    3.44  2.63     0.325
```

The descriptive statistics make sense; expect for the variability. Let's see if the differences are significant or not.


```r
# test for homogeneity in variances
car::leveneTest(mean_activity ~ group, data = group_act)
```

```
## Levene's Test for Homogeneity of Variance (center = median)
##        Df F value    Pr(>F)    
## group   1  13.867 0.0002101 ***
##       790                      
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
car::leveneTest(sd_activity ~ group, data = group_act)
```

```
## Levene's Test for Homogeneity of Variance (center = median)
##        Df F value  Pr(>F)  
## group   1  3.0753 0.07988 .
##       790                  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
car::leveneTest(zero_prop ~ group, data = group_act)
```

```
## Levene's Test for Homogeneity of Variance (center = median)
##        Df F value    Pr(>F)    
## group   1  25.158 6.526e-07 ***
##       790                      
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

Since there are inhomogeneous variances, we have to set `var.equal = FALSE` for mean and zero proportion and `var.equal = TRUE` for standard deviation.


```r
# corresponding two sample t-test

# mean: condition less than control
t.test(mean_activity ~ group, data = group_act, alternative = "less", var.equal = FALSE)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  mean_activity by group
## t = -10.62, df = 655.24, p-value < 2.2e-16
## alternative hypothesis: true difference in means between group condition and group control is less than 0
## 95 percent confidence interval:
##        -Inf -0.5355071
## sample estimates:
## mean in group condition   mean in group control 
##                2.804915                3.438724
```

```r
# sd: condition greater than control
t.test(sd_activity ~ group, data = group_act, alternative = "greater", var.equal = TRUE)
```

```
## 
## 	Two Sample t-test
## 
## data:  sd_activity by group
## t = -5.3539, df = 790, p-value = 1
## alternative hypothesis: true difference in means between group condition and group control is greater than 0
## 95 percent confidence interval:
##  -0.1671776        Inf
## sample estimates:
## mean in group condition   mean in group control 
##                2.498502                2.626354
```

```r
# zero proportion: condition greater than control
t.test(zero_prop ~ group, data = group_act, alternative = "greater", var.equal = FALSE)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  zero_prop by group
## t = 6.706, df = 577.94, p-value = 2.385e-11
## alternative hypothesis: true difference in means between group condition and group control is greater than 0
## 95 percent confidence interval:
##  0.06231984        Inf
## sample estimates:
## mean in group condition   mean in group control 
##               0.4071576               0.3245410
```

|                 | Condition | Control |             |
|-----------------|-----------|---------|-------------|
| mean            | 2.80      | 3.44    | *p \< .001* |
| sd              | 2.50      | 2.63    | *p = 1*     |
| zero proportion | 0.41      | 0.32    | *p \< .001* |

: Characteristics of the depressed patients and healthy controls in 24-hour activity

#### Under an hourly span

The activity was normalized (range: 0-1) across both groups to make them comparable. Here we'll use the heat map and line graph to compare activities between groups by hours.


```r
# add normal and hour columns
clean_df <- clean_df %>%
    mutate(normal_zerone = scales::rescale(log_activity),
           hour = hour(timestamp))

# heat map
heat_plot <- clean_df %>%
    group_by(group, hour) %>%
    summarise(activity = mean(normal_zerone)) %>%
    ggplot(., aes(group, hour, fill = activity)) + 
        geom_tile() +
        scale_fill_gradient(low = "white", high = "blue")

# line plot
line_plot <- clean_df %>%
    group_by(group, hour) %>%
    summarise(activity = mean(normal_zerone)) %>%
    ggplot(., aes(hour, activity, color = group)) +
        geom_line()

grid.arrange(heat_plot, line_plot, ncol = 1)
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/unnamed-chunk-17-1.png" width="672" />

It's interesting that the control group has an opposite circadian rhythm as we thought. But we can see from the heat map that the condition group may have slightly lower activity compared to the control group.

## Modeling

### Feature extraction

Besides the mean, standard deviation, and zero proportion of activity, we extracted other features according to previous studies.[^7] Including:

[^7]: Zanella-Calzada, L. A., Galván-Tejada, C. E., Chávez-Lamas, N. M., Gracia-Cortés, M. D. C., Magallanes-Quintanar, R., Celaya-Padilla, J. M., Galván-Tejada, J. I., & Gamboa-Rosales, H. (2019). Feature Extraction in Motor Activity Signal: Towards a Depression Episodes Detection in Unipolar and Bipolar Patients. *Diagnostics (Basel, Switzerland), 9(1),* 8. <https://doi.org/10.3390/diagnostics9010008>

-   Coefficient of variation

-   Kurtosis

-   Skewness

-   Quantile 25, 75, 95, 99

-   Median

-   Number of times a time series crosses the median line


```r
# set up final features df for ML
feature_df <- clean_df %>%
    group_by(ID, date) %>%
    summarise(
        mean_act = mean(log_activity),
        sd_act = sd(log_activity), 
        zero_prop = zero_proportion(log_activity),
        # Coefficient of variation
        cv_act = sd_act / mean_act, 
        skew_act = psych::skew(log_activity),
        kurtosi_act = psych::kurtosi(log_activity),
        q99_act = quantile(log_activity, probs = 0.99),
        q95_act = quantile(log_activity, probs = 0.95),
        q75_act = quantile(log_activity, probs = 0.75),
        q25_act = quantile(log_activity, probs = 0.25),
        med_act = median(log_activity),
        #  number of times crosses the median line
        cross_medpoint = crossing_points(log_activity)
    ) %>%
    distinct() %>%
    mutate(group = as.factor(if_else(ID < 24, "condition", "control"))) %>%
    ungroup() %>%
    select(-c("ID", "date"))

feature_df
```

```
## # A tibble: 792 × 13
##    mean_act sd_act zero_prop cv_act skew_act kurtosi_act q99_act q95_act q75_act
##       <dbl>  <dbl>     <dbl>  <dbl>    <dbl>       <dbl>   <dbl>   <dbl>   <dbl>
##  1     2.93   2.65     0.409  0.905 -0.00188       -1.74    6.79    6.44    5.53
##  2     2.59   2.58     0.462  0.995  0.180         -1.68    6.86    6.38    5.15
##  3     2.91   2.50     0.374  0.857 -0.0398        -1.60    7.00    6.38    5.22
##  4     2.56   2.41     0.420  0.942  0.147         -1.59    6.63    6.15    4.91
##  5     3.31   2.93     0.394  0.885 -0.0148        -1.69    7.63    7.23    6.22
##  6     2.48   2.50     0.465  1.01   0.232         -1.63    6.78    6.15    5.00
##  7     2.95   2.61     0.397  0.884 -0.0209        -1.67    7.08    6.50    5.43
##  8     2.97   2.67     0.4    0.896  0.00146       -1.70    7.04    6.63    5.53
##  9     3.29   2.62     0.342  0.795 -0.211         -1.60    7.06    6.63    5.67
## 10     2.94   2.67     0.408  0.910  0.0523        -1.66    7.10    6.73    5.53
## # ℹ 782 more rows
## # ℹ 4 more variables: q25_act <dbl>, med_act <dbl>, cross_medpoint <int>,
## #   group <fct>
```

### Splitting train/test set

Create a single 70/30 split of the data.


```r
set.seed(123)

# split train/test
split_data <- initial_split(feature_df, prop = 0.7, strata = 'group')
train_set <- training(split_data)
test_set <- testing(split_data)

# see prop
list(train_num = table(train_set$group),
     train_prop = table(train_set$group) %>% prop.table(), 
     test_num = table(test_set$group),
     test = table(test_set$group) %>% prop.table())
```

```
## $train_num
## 
## condition   control 
##       230       324 
## 
## $train_prop
## 
## condition   control 
## 0.4151625 0.5848375 
## 
## $test_num
## 
## condition   control 
##        99       139 
## 
## $test
## 
## condition   control 
## 0.4159664 0.5840336
```

### Training models

Baseline model: logistic regression

-   Traditional two classes classification model

Comparison model: random forest, linear SVM

-   Linear SVM: had a higher model performance in past studies

-   Random forest: ensemble method; easier to explain

The evaluation of the results was done through a ROC curve-based approach.


```r
set.seed(123)

# Create reusable trainControl
myControl <- trainControl(
  summaryFunction = twoClassSummary,
  classProbs = TRUE, 
  verboseIter = FALSE,
  savePredictions = TRUE,
  index = createFolds(train_set$group, k = 5)
)
```

#### Logistic regression


```r
# train logistic model
model_logis <- train(
    x = train_set[, -13],
    y = train_set$group,
    metric = "ROC",
    method = "glm",
    trControl = myControl,
    preProcess = c("center", "scale")
)

model_logis
```

```
## Generalized Linear Model 
## 
## 554 samples
##  12 predictor
##   2 classes: 'condition', 'control' 
## 
## Pre-processing: centered (12), scaled (12) 
## Resampling: Bootstrapped (5 reps) 
## Summary of sample sizes: 111, 110, 111, 111, 111 
## Resampling results:
## 
##   ROC       Sens       Spec    
##   0.770514  0.6173913  0.801687
```

#### Random forest


```r
set.seed(125749)

# train random forest
model_rf <- train(
    x = train_set[, -13],
    y = train_set$group,
    metric = "ROC",
    method = "ranger",
    trControl = myControl,
    preProcess = c("center", "scale"),
    tuneGrid = expand.grid(mtry = 1:6,
                           splitrule = 'extratrees',
                           min.node.size = c(10, 15, 20, 25, 30)),
    importance = 'impurity'
)

model_rf
```

```
## Random Forest 
## 
## 554 samples
##  12 predictor
##   2 classes: 'condition', 'control' 
## 
## Pre-processing: centered (12), scaled (12) 
## Resampling: Bootstrapped (5 reps) 
## Summary of sample sizes: 111, 110, 111, 111, 111 
## Resampling results across tuning parameters:
## 
##   mtry  min.node.size  ROC        Sens       Spec     
##   1     10             0.7721086  0.5695652  0.7870894
##   1     15             0.7713881  0.5543478  0.7886368
##   1     20             0.7731507  0.5467391  0.7901812
##   1     25             0.7707242  0.5489130  0.7901782
##   1     30             0.7701637  0.5369565  0.7940333
##   2     10             0.7668727  0.5847826  0.7685744
##   2     15             0.7660683  0.5847826  0.7701158
##   2     20             0.7668861  0.5836957  0.7716662
##   2     25             0.7682540  0.5826087  0.7739709
##   2     30             0.7684303  0.5782609  0.7716691
##   3     10             0.7620639  0.5847826  0.7670151
##   3     15             0.7660681  0.5804348  0.7624057
##   3     20             0.7660432  0.5891304  0.7678022
##   3     25             0.7665934  0.5978261  0.7585388
##   3     30             0.7665899  0.5836957  0.7647193
##   4     10             0.7617975  0.5739130  0.7677873
##   4     15             0.7626484  0.5913043  0.7600921
##   4     20             0.7638775  0.5945652  0.7639531
##   4     25             0.7668258  0.6010870  0.7639412
##   4     30             0.7660075  0.5967391  0.7623998
##   5     10             0.7603864  0.5739130  0.7716602
##   5     15             0.7610291  0.5891304  0.7608732
##   5     20             0.7627141  0.5923913  0.7554707
##   5     25             0.7669007  0.5923913  0.7585536
##   5     30             0.7668656  0.6065217  0.7608643
##   6     10             0.7605926  0.5858696  0.7600772
##   6     15             0.7619092  0.5923913  0.7608554
##   6     20             0.7638659  0.5891304  0.7616335
##   6     25             0.7643639  0.5934783  0.7562429
##   6     30             0.7641822  0.5956522  0.7647134
## 
## Tuning parameter 'splitrule' was held constant at a value of extratrees
## ROC was used to select the optimal model using the largest value.
## The final values used for the model were mtry = 1, splitrule = extratrees
##  and min.node.size = 20.
```

#### SVM


```r
# train linear svm
model_svm <- train(
    # change to formula, avoiding kernlab class probability calculations failed
    group ~ .,        
    data = train_set,
    metric = "ROC",
    method = "svmLinear",
    trControl = myControl,
    preProcess = c("center", "scale"),
    tuneGrid = expand.grid(C = seq(1, 5, length = 20))
)

model_svm
```

```
## Support Vector Machines with Linear Kernel 
## 
## 554 samples
##  12 predictor
##   2 classes: 'condition', 'control' 
## 
## Pre-processing: centered (12), scaled (12) 
## Resampling: Bootstrapped (5 reps) 
## Summary of sample sizes: 111, 110, 111, 111, 111 
## Resampling results across tuning parameters:
## 
##   C         ROC        Sens       Spec     
##   1.000000  0.7671221  0.4000000  0.8795961
##   1.210526  0.7690447  0.4304348  0.8850965
##   1.421053  0.7708282  0.4326087  0.8812355
##   1.631579  0.7706679  0.4010870  0.8858361
##   1.842105  0.7701435  0.4608696  0.8665399
##   2.052632  0.7698923  0.4434783  0.8773181
##   2.263158  0.7693097  0.3967391  0.8742530
##   2.473684  0.7693350  0.3902174  0.9027799
##   2.684211  0.7694950  0.3880435  0.8989011
##   2.894737  0.7698319  0.4445652  0.8773240
##   3.105263  0.7698426  0.3826087  0.8996644
##   3.315789  0.7693033  0.4239130  0.8904604
##   3.526316  0.7688119  0.4858696  0.8673151
##   3.736842  0.7688365  0.4010870  0.8781051
##   3.947368  0.7685025  0.2478261  0.9382269
##   4.157895  0.7681553  0.3815217  0.8958509
##   4.368421  0.7683899  0.3739130  0.9058747
##   4.578947  0.7686496  0.3652174  0.9051025
##   4.789474  0.7690439  0.3956522  0.8773418
##   5.000000  0.7691192  0.3793478  0.8827562
## 
## ROC was used to select the optimal model using the largest value.
## The final value used for the model was C = 1.421053.
```

Predict on training set.


```r
# Create model_list
model_list <- list(logistic = model_logis, rf = model_rf, svm = model_svm)

# predict on training set to see if it's overfitting later
for (i in 1:length(model_list)) {
    print(confusionMatrix(predict(model_list[[i]], train_set),
                          train_set$group))
}
```

```
## Confusion Matrix and Statistics
## 
##            Reference
## Prediction  condition control
##   condition       140      50
##   control          90     274
##                                          
##                Accuracy : 0.7473         
##                  95% CI : (0.7089, 0.783)
##     No Information Rate : 0.5848         
##     P-Value [Acc > NIR] : 9.529e-16      
##                                          
##                   Kappa : 0.4661         
##                                          
##  Mcnemar's Test P-Value : 0.0009804      
##                                          
##             Sensitivity : 0.6087         
##             Specificity : 0.8457         
##          Pos Pred Value : 0.7368         
##          Neg Pred Value : 0.7527         
##              Prevalence : 0.4152         
##          Detection Rate : 0.2527         
##    Detection Prevalence : 0.3430         
##       Balanced Accuracy : 0.7272         
##                                          
##        'Positive' Class : condition      
##                                          
## Confusion Matrix and Statistics
## 
##            Reference
## Prediction  condition control
##   condition       162      36
##   control          68     288
##                                          
##                Accuracy : 0.8123         
##                  95% CI : (0.7772, 0.844)
##     No Information Rate : 0.5848         
##     P-Value [Acc > NIR] : < 2.2e-16      
##                                          
##                   Kappa : 0.6055         
##                                          
##  Mcnemar's Test P-Value : 0.002367       
##                                          
##             Sensitivity : 0.7043         
##             Specificity : 0.8889         
##          Pos Pred Value : 0.8182         
##          Neg Pred Value : 0.8090         
##              Prevalence : 0.4152         
##          Detection Rate : 0.2924         
##    Detection Prevalence : 0.3574         
##       Balanced Accuracy : 0.7966         
##                                          
##        'Positive' Class : condition      
##                                          
## Confusion Matrix and Statistics
## 
##            Reference
## Prediction  condition control
##   condition       117      33
##   control         113     291
##                                           
##                Accuracy : 0.7365          
##                  95% CI : (0.6977, 0.7727)
##     No Information Rate : 0.5848          
##     P-Value [Acc > NIR] : 7.149e-14       
##                                           
##                   Kappa : 0.4285          
##                                           
##  Mcnemar's Test P-Value : 6.231e-11       
##                                           
##             Sensitivity : 0.5087          
##             Specificity : 0.8981          
##          Pos Pred Value : 0.7800          
##          Neg Pred Value : 0.7203          
##              Prevalence : 0.4152          
##          Detection Rate : 0.2112          
##    Detection Prevalence : 0.2708          
##       Balanced Accuracy : 0.7034          
##                                           
##        'Positive' Class : condition       
## 
```

### Validation

#### Metrics

-   AUC: Area Under ROC curve

-   Accuracy: the percentage of correctly classified condition and control samples.

-   Sensitivity: the fraction of correctly classified conditions related to all conditions.

-   Specificity: the fraction of controls correctly classified as controls.

-   Balanced Accuracy: the arithmetic mean of sensitivity and specificity, which is particularly useful when the two classes are imbalanced.

-   PPV: the proportion of true condition samples.

-   NPV: the proportion of true control samples.


```r
# gather metrics together
acc <- c()
bal <- c()
sen <- c()
spe <- c()
ppv <- c()
npv <- c()
auc <-c()

for (num in 1:length(model_list)) {
    prediction <- predict(model_list[[num]], test_set)
    confus <- confusionMatrix(prediction, test_set$group)
    acc <- acc %>% append(confus$overall[["Accuracy"]])
    bal <- bal %>% append(confus$byClass[["Balanced Accuracy"]])
    sen <- sen %>% append(confus$byClass[["Sensitivity"]])
    spe <- spe %>% append(confus$byClass[["Specificity"]])
    ppv <- ppv %>% append(confus$byClass[["Pos Pred Value"]])
    npv <- npv %>% append(confus$byClass[["Neg Pred Value"]])
    ROC <- pROC::roc(as.numeric(test_set$group), as.numeric(prediction))
    auc <- auc %>% append(pROC::auc(ROC))
}

metric <- data.frame(Model = c("Logistic", "Random Forest", "SVM"),
                     Accuracy = acc,
                     Balanced_ACC = bal, 
                     Sensitivity = sen,
                     Specificity = spe,
                     PPV = ppv,
                     NPV = npv,
                     AUC = auc) %>% 
    mutate(across(-1, ~ round(.x, 3)))

metric
```

```
##           Model Accuracy Balanced_ACC Sensitivity Specificity   PPV   NPV   AUC
## 1      Logistic    0.735        0.714       0.586       0.842 0.725 0.741 0.714
## 2 Random Forest    0.752        0.734       0.626       0.842 0.738 0.760 0.734
## 3           SVM    0.739        0.707       0.515       0.899 0.785 0.723 0.707
```

Based on the measurements, we discovered that random forest have the highest balanced accuracy and AUC. But regardless of the model, they share the same high specificity and low sensitivity. This indicates that while our models performed well in categorizing control cases, they were less likely to accurately categorize condition cases.

#### Variable importance

Let's see the variable importance of the random forest model.


```r
# visualize rf
vip(model_rf, num_features = 12)
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/unnamed-chunk-26-1.png" width="672" />

According to the figure, the five most important features in the random forest model were the median, zero proportion, mean, quantile 95, and skewness of daily activity.

## Conclusion

In conclusion, our investigation has demonstrated the potential of a number of machine learning algorithms to distinguish between depressed and healthy participants in time series of motor activity. Furthermore, random forest outperformed other methods in our analysis for the classification task. Additionally, the extracted statistical features indicated that the information they include describes the primary aspects of a participant's daily activities, making it possible to distinguish between depressed and healthy participants. Nevertheless, because of its limited sensitivity, we must integrate this kind of index with other indicators (e.g., clinical assessments) to get the big picture if we would like to implement it to aid in clinical diagnosis.
