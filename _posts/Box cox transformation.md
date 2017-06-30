---
layout: post
title: Box cox transformation
published: true
status: publish
draft: false
tags: R 
output:
  prettydoc::html_pretty:
    theme: leonids
    highlight: github
---
 
In order to perform ordinary least sqaures regression, there are certain assumptions that need to be considered before building the modeling. Assumptions regarding the residuals, such as normal distribution, constant variance i.e homoscadaticity need to checked while building a model. However, ther will be times when the assumptions of OLS are violated by the data. In such instances data transformation can be employed to transform data from non-linear distribution to linear distribution. In this post we will look at one such technique called "Box Cox" transformation.
 
 
 

{% highlight r %}
library(readr) # importing csv files
{% endhighlight %}



{% highlight text %}
## 
## Attaching package: 'readr'
{% endhighlight %}



{% highlight text %}
## The following object is masked from 'package:rvest':
## 
##     guess_encoding
{% endhighlight %}



{% highlight r %}
library(MASS) # Boxcox function
library(car) # qqPlot function
library(moments) # skeweness and kurtosis functions
{% endhighlight %}
 

{% highlight r %}
TGT <- read_csv("C:/Users/welcome/Downloads/TGT.csv")
 
attach(TGT)
{% endhighlight %}
 
## Data preparation
 
1. As the class of TAT is integer, it is converted to numeric
2. Grades "ASE1", "ASE2", "ASE3","ASE4", "ASE5" are changed to 1, 2, 3, 4 & 5 respectively, and class of Grade is converted from character to numeric.

{% highlight r %}
TGT[,"TAT"] <- as.numeric(TAT) # convert to num
 
 
TGT[which(GRADE == 'ASE1'), "GRADE"] <- 1
 
TGT[which(GRADE == 'ASE2'), "GRADE"] <- 2
 
TGT[which(GRADE == 'ASE3'), "GRADE"] <- 3
 
TGT[which(GRADE == 'ASE4'), "GRADE"] <- 4
 
TGT[which(GRADE == 'ASE5'), "GRADE"] <- 5
 
TGT[,"GRADE"] <- as.numeric(TGT$GRADE)
{% endhighlight %}
 
## Model fitting ( Before transformation)
 
TAT is regressed with GRADE with out any data tranformation.

{% highlight r %}
f <- lm(TAT ~ as.factor(GRADE))
 
plot(f$fitted.values,  rstandard(f)) # Examine raw residuals  vs fitted values
{% endhighlight %}

![plot of chunk unnamed-chunk-4](/figures/unnamed-chunk-4-1.png)
 
 
From the above plot, it appears that there is a trend in the residuals, which takes the shape of an outward funnel.

{% highlight r %}
hist(f$residuals) # examine histogram of raw residuals
{% endhighlight %}

![plot of chunk unnamed-chunk-5](/figures/unnamed-chunk-5-1.png)
 
Histogram indicate distribution of raw residuals is postively skewed. 

{% highlight r %}
qqPlot(rstandard(f)) # examine quantile-quantile plot for raw residuals
{% endhighlight %}

![plot of chunk unnamed-chunk-6](/figures/unnamed-chunk-6-1.png)
 

{% highlight r %}
skewness(f$residuals) 
{% endhighlight %}



{% highlight text %}
## [1] 1.076633
{% endhighlight %}
 

{% highlight r %}
kurtosis(f$residuals) 
{% endhighlight %}



{% highlight text %}
## [1] 5.420123
{% endhighlight %}
 
 
 
## Estimation of likelihood function

{% highlight r %}
b <- boxcox(TAT ~ GRADE)
{% endhighlight %}

![plot of chunk unnamed-chunk-9](/figures/unnamed-chunk-9-1.png)

{% highlight r %}
lambda <- b$x # lambda values
 
lik <- b$y # log likelihood values for SSE
 
bc <- cbind(lambda, lik) # combine lambda and lik
 
sorted_bc <- bc[order(-lik),] # values are sorted to identify the lambda value for the maximum log likelihood for obtaining minimized SSE
 
head(sorted_bc, n = 10)
{% endhighlight %}



{% highlight text %}
##          lambda       lik
##  [1,] 0.3838384 -10576.90
##  [2,] 0.3434343 -10576.99
##  [3,] 0.4242424 -10579.12
##  [4,] 0.3030303 -10579.44
##  [5,] 0.4646465 -10583.62
##  [6,] 0.2626263 -10584.28
##  [7,] 0.5050505 -10590.36
##  [8,] 0.2222222 -10591.55
##  [9,] 0.5454545 -10599.31
## [10,] 0.1818182 -10601.31
{% endhighlight %}
 
The lambda value for for the maximum log likeihood for obtaining minimized SSE is 0.38( also can be seen in the box cox plot above)
 
 
 
## Model fitting (After transformation)
 
After identifying the lambda value, which is 0.38, lets fit the model with the transformed data.

{% highlight r %}
f1 <- lm(TAT^(0.38) ~ as.factor(GRADE))
 
plot(f1$fitted.values,  rstandard(f1)) # Examine raw residuals vs fitted values
{% endhighlight %}

![plot of chunk unnamed-chunk-11](/figures/unnamed-chunk-11-1.png)
 
The distribution of residuals around the fitted line has improved from the previous model. Nearly close to constant variance.

{% highlight r %}
hist(f1$residuals) # examine histogram of raw residuals
{% endhighlight %}

![plot of chunk unnamed-chunk-12](/figures/unnamed-chunk-12-1.png)
 
Histogram indicate distribution of raw residuals is very close to normal distribution.
 

{% highlight r %}
qqPlot(rstandard(f1)) # examine quantile-quantile plot for raw residuals
{% endhighlight %}

![plot of chunk unnamed-chunk-13](/figures/unnamed-chunk-13-1.png)
 

{% highlight r %}
skewness(f1$residuals) 
{% endhighlight %}



{% highlight text %}
## [1] -0.00364926
{% endhighlight %}
 

{% highlight r %}
kurtosis(f1$residuals) 
{% endhighlight %}



{% highlight text %}
## [1] 3.373519
{% endhighlight %}
 
 
The adjusted R square value has improved from model fitted with untransformed data to model fitted with transformed data. 

{% highlight r %}
adj_rsq1 <- summary(f)$adj.r.squared
 
adj_rsq2 <- summary(f1)$adj.r.squared
 
cat("Adjusted R-Square (before transformation):", adj_rsq1, "Adjusted R-Square (after transformation):", adj_rsq2, sep = "\n")
{% endhighlight %}



{% highlight text %}
## Adjusted R-Square (before transformation):
## 0.04481049
## Adjusted R-Square (after transformation):
## 0.04722951
{% endhighlight %}
 
