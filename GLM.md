GLM.Rmd
================

**Heart Attack**

**Task: Logistic Regression model using GLM**

**Dataset link:-
<https://www.kaggle.com/rashikrahmanpritom/heart-attack-analysis-prediction-dataset>**

**Data dictionary**

age: Age of the patient; sex: Sex of the patient; cp: Chest pain type \~
0 = Typical Angina, 1 = Atypical Angina, 2 = Non-anginal Pain, 3 =
Asymptotic; trtbps: Resting blood pressure(in mm Hg); chol: Cholestoral
in mg/dl fetched via BMI sensor; fbs: (fasting blood sugar &gt; 120
mg/dl)\~1 = True, 0 = False; restecg: Resting electrocardiographic
results \~ 0 = Normal, 1 = ST-T wave normality, 2 = Left ventricular
hypertrophy; thalachh: Maximum heart rate achieved; oldpeak: Previous
peak; slp: Slope; caa: Number of major vessels; thall: Thalium Stress
Test result \~ (0,3); exng: Exercise induced angina \~ 1 = Yes, 0 = No;
output: Target variable

**Working**

Getting the working directory and dataset:-

``` r
getwd()
```

    ## [1] "C:/Users/Lenovo/OneDrive/Documents"

``` r
setwd("C:/Users/Lenovo/OneDrive/Desktop")
getwd()
```

    ## [1] "C:/Users/Lenovo/OneDrive/Desktop"

``` r
df <- read.csv("heart.csv")
dim(df)
```

    ## [1] 303  14

``` r
head(df)
```

    ##   age sex cp trtbps chol fbs restecg thalachh exng oldpeak slp caa thall output
    ## 1  63   1  3    145  233   1       0      150    0     2.3   0   0     1      1
    ## 2  37   1  2    130  250   0       1      187    0     3.5   0   0     2      1
    ## 3  41   0  1    130  204   0       0      172    0     1.4   2   0     2      1
    ## 4  56   1  1    120  236   0       1      178    0     0.8   2   0     2      1
    ## 5  57   0  0    120  354   0       1      163    1     0.6   2   0     2      1
    ## 6  57   1  0    140  192   0       1      148    0     0.4   1   0     1      1

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
sapply(df, n_distinct)
```

    ##      age      sex       cp   trtbps     chol      fbs  restecg thalachh 
    ##       41        2        4       49      152        2        3       91 
    ##     exng  oldpeak      slp      caa    thall   output 
    ##        2       40        3        5        4        2

``` r
sapply(df, class)
```

    ##       age       sex        cp    trtbps      chol       fbs   restecg  thalachh 
    ## "integer" "integer" "integer" "integer" "integer" "integer" "integer" "integer" 
    ##      exng   oldpeak       slp       caa     thall    output 
    ## "integer" "numeric" "integer" "integer" "integer" "integer"

Dataset’s logistic regression:

``` r
logHeart <- glm(output ~ ., binomial, df)
summary(logHeart)
```

    ## 
    ## Call:
    ## glm(formula = output ~ ., family = binomial, data = df)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.5849  -0.3872   0.1551   0.5863   2.6249  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  3.450472   2.571479   1.342 0.179653    
    ## age         -0.004908   0.023175  -0.212 0.832266    
    ## sex         -1.758181   0.468774  -3.751 0.000176 ***
    ## cp           0.859851   0.185397   4.638 3.52e-06 ***
    ## trtbps      -0.019477   0.010339  -1.884 0.059582 .  
    ## chol        -0.004630   0.003782  -1.224 0.220873    
    ## fbs          0.034888   0.529465   0.066 0.947464    
    ## restecg      0.466282   0.348269   1.339 0.180618    
    ## thalachh     0.023211   0.010460   2.219 0.026485 *  
    ## exng        -0.979981   0.409784  -2.391 0.016782 *  
    ## oldpeak     -0.540274   0.213849  -2.526 0.011523 *  
    ## slp          0.579288   0.349807   1.656 0.097717 .  
    ## caa         -0.773349   0.190885  -4.051 5.09e-05 ***
    ## thall       -0.900432   0.290098  -3.104 0.001910 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 417.64  on 302  degrees of freedom
    ## Residual deviance: 211.44  on 289  degrees of freedom
    ## AIC: 239.44
    ## 
    ## Number of Fisher Scoring iterations: 6

From output we can see that sex, cp, thalachh, exng, caa, thall is
significant on 5% level of significance.

Logistic regression of sex on output:

``` r
loglrs <- glm(output ~ sex, binomial, df)
summary(loglrs)
```

    ## 
    ## Call:
    ## glm(formula = output ~ sex, family = binomial, data = df)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.6651  -1.0923   0.7585   1.2650   1.2650  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)   1.0986     0.2357   4.661 3.15e-06 ***
    ## sex          -1.3022     0.2740  -4.752 2.01e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 417.64  on 302  degrees of freedom
    ## Residual deviance: 392.80  on 301  degrees of freedom
    ## AIC: 396.8
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
plot(df$sex, loglrs$fitted, pch = 19, col = "blue", xlab = "Sex", ylab = "Output")
```

![](GLM_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
exp(loglrs$coeff)
```

    ## (Intercept)         sex 
    ##   3.0000000   0.2719298

``` r
exp(confint(loglrs))
```

    ## Waiting for profiling to be done...

    ##                 2.5 %    97.5 %
    ## (Intercept) 1.9199586 4.8583219
    ## sex         0.1565237 0.4597383

From graph we can see that sex = 0 has high probability of having heart
attack than sex = 1.

Logistic regression of cp on output:

``` r
logcp <- glm(output ~ cp, binomial, df)
summary(logcp)
```

    ## 
    ## Call:
    ## glm(formula = output ~ cp, family = binomial, data = df)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.1711  -0.8999   0.4461   1.0573   1.4830  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  -0.6948     0.1685  -4.122 3.75e-05 ***
    ## cp            0.9840     0.1390   7.079 1.45e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 417.64  on 302  degrees of freedom
    ## Residual deviance: 356.06  on 301  degrees of freedom
    ## AIC: 360.06
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
plot(df$cp, logcp$fitted, pch = 19, xlab = "Cp", ylab = "Output")
```

![](GLM_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
exp(logcp$coeff)
```

    ## (Intercept)          cp 
    ##   0.4991937   2.6751366

``` r
exp(confint(logcp))
```

    ## Waiting for profiling to be done...

    ##                 2.5 %    97.5 %
    ## (Intercept) 0.3565089 0.6912092
    ## cp          2.0543150 3.5465719

From graph we can see that, people with cp = 3 have higher chance of
getting heart attack.

Logistic regression of resting blood pressure on output:

``` r
logthalachh <- glm(output ~ thalachh, binomial, df)
summary(logthalachh)
```

    ## 
    ## Call:
    ## glm(formula = output ~ thalachh, family = binomial, data = df)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.1383  -1.0780   0.6043   0.9200   2.1354  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -6.391452   0.987133  -6.475 9.50e-11 ***
    ## thalachh     0.043951   0.006531   6.729 1.71e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 417.64  on 302  degrees of freedom
    ## Residual deviance: 359.26  on 301  degrees of freedom
    ## AIC: 363.26
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
plot(df$thalachh, logthalachh$fitted, pch = 19, xlab = "Maximum heart rate received (thalachh)", ylab = "Output")
```

![](GLM_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
exp(logthalachh$coeff)
```

    ## (Intercept)    thalachh 
    ## 0.001675821 1.044931450

``` r
exp(confint(logthalachh))
```

    ## Waiting for profiling to be done...

    ##                    2.5 %     97.5 %
    ## (Intercept) 0.0002220576 0.01075145
    ## thalachh    1.0321488799 1.05898608

From graph, we can see that people receiving maximum heart rate have
high probability of getting heart attack.

Logistic regression of exercise induced angina (exng) on output.

``` r
logexng <- glm(output ~ exng, binomial, df)
summary(logexng)
```

    ## 
    ## Call:
    ## glm(formula = output ~ exng, family = binomial, data = df)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.5434  -0.7272   0.8512   0.8512   1.7086  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)   0.8287     0.1522   5.444 5.21e-08 ***
    ## exng         -2.0239     0.2825  -7.164 7.82e-13 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 417.64  on 302  degrees of freedom
    ## Residual deviance: 357.90  on 301  degrees of freedom
    ## AIC: 361.9
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
plot(df$exng, logexng$fitted, pch = 19, xlab = "Exercise induced angina (exng)", ylab = "Output")
```

![](GLM_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
exp(logexng$coeff)
```

    ## (Intercept)        exng 
    ##   2.2903226   0.1321349

``` r
exp(confint(logexng))
```

    ## Waiting for profiling to be done...

    ##                  2.5 %    97.5 %
    ## (Intercept) 1.70847246 3.1063958
    ## exng        0.07465841 0.2266843

From graph we can see that people with non-exercise induced angina have
high probability of getting heart attack.

Logistic regression of number of major vessels (caa) on output:

``` r
logcaa <- glm(output ~ caa, binomial, df)
summary(logcaa)
```

    ## 
    ## Call:
    ## glm(formula = output ~ caa, family = binomial, data = df)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.5349  -1.1355   0.8579   0.8579   2.4020  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)   0.8099     0.1544   5.247 1.54e-07 ***
    ## caa          -0.9093     0.1466  -6.201 5.60e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 417.64  on 302  degrees of freedom
    ## Residual deviance: 367.63  on 301  degrees of freedom
    ## AIC: 371.63
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
plot(df$caa, logcaa$fitted, pch = 19, xlab = "Number of major vessels (caa)", ylab = "Output")
```

![](GLM_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
exp(logcaa$coeff)
```

    ## (Intercept)         caa 
    ##   2.2477699   0.4028072

``` r
exp(confint(logcaa))
```

    ## Waiting for profiling to be done...

    ##                2.5 %    97.5 %
    ## (Intercept) 1.669264 3.0603783
    ## caa         0.298024 0.5303689

From graph we can see that people with zero vessels i.e., caa = 0 are
more prone to heart attack.

Logistic regression of thalium stress test on output:

``` r
logthall <- glm(output ~ thall, binomial, df)
summary(logthall)
```

    ## 
    ## Call:
    ## glm(formula = output ~ thall, family = binomial, data = df)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.5462  -0.9079   0.9284   0.9284   1.4733  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)   3.2016     0.5456   5.869 4.40e-09 ***
    ## thall        -1.2916     0.2249  -5.743 9.29e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 417.64  on 302  degrees of freedom
    ## Residual deviance: 379.14  on 301  degrees of freedom
    ## AIC: 383.14
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
plot(df$thall, logthall$fitted, pch = 19, xlab = "Thalium stress test (thall)", ylab = "Output")
```

![](GLM_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
exp(logthall$coeff)
```

    ## (Intercept)       thall 
    ##  24.5724453   0.2748342

``` r
exp(confint(logthall))
```

    ## Waiting for profiling to be done...

    ##                 2.5 %     97.5 %
    ## (Intercept) 8.6952310 73.8997330
    ## thall       0.1746485  0.4220742

From graph we can see that people with low Thalium stress test have high
risk of getting heart attack.

**Interpretation**

Final interpretation from all graph of dataset is that sex = 0 has high
risk of getting heart attack. People having asymptotic chest pain (cp =
3) have high risk of getting heart attack. People having maximum heart
rate (thalachh) have high risk of getting heart attack. People with
non-exercise induced angina (exng = 0) have high risk of getting heart
attack. People having low number of major vessels (caa) have high risk
of getting heart attack. People having lower Thalium stress test (thall)
have high risk of getting heart attack.
