---
layout: post
title: Web scraping with rvest
published: true
status: publish
draft: false
tags: R 
output:
  prettydoc::html_pretty:
    theme: leonids
    highlight: github
---
 
 
 
***Reading Data***

{% highlight r %}
library(ggplot2)
espn <-read.csv("H:/espn.csv")
attach(espn)
{% endhighlight %}
 
***Define variables***

{% highlight r %}
y <- cbind(Ave) #Dependent Variable
x1 <- cbind(Runs) #Independent Variable
x <- cbind(Runs,Inns,Matches) #Independent Variables
{% endhighlight %}
 
***Correlation among variables***

{% highlight r %}
cor(y,x)
{% endhighlight %}



{% highlight text %}
## Error in cor(y, x): 'y' must be numeric
{% endhighlight %}



{% highlight r %}
ggplot(espn, aes(x = x1, y = y)) + 
  geom_point() 
{% endhighlight %}



{% highlight text %}
## Error: Aesthetics must be either length 1 or the same as the data (50): x, y
{% endhighlight %}

![plot of chunk unnamed-chunk-3](/figures/unnamed-chunk-3-1.png)
 
***Simple linear regression***

{% highlight r %}
olsreg1 <- lm(y ~ x1)
{% endhighlight %}



{% highlight text %}
## Error in model.frame.default(formula = y ~ x1, drop.unused.levels = TRUE): variable lengths differ (found for 'x1')
{% endhighlight %}



{% highlight r %}
summary(olsreg1)
{% endhighlight %}



{% highlight text %}
## 
## Call:
## lm(formula = y ~ x1)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -17.144  -6.709  -1.950   7.473  34.951 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 24.740554   3.979617   6.217 1.17e-07 ***
## x1           0.036265   0.007032   5.157 4.71e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 10.59 on 48 degrees of freedom
## Multiple R-squared:  0.3565,	Adjusted R-squared:  0.3431 
## F-statistic: 26.59 on 1 and 48 DF,  p-value: 4.714e-06
{% endhighlight %}



{% highlight r %}
confint(olsreg1, level = 0.95)
{% endhighlight %}



{% highlight text %}
##                   2.5 %      97.5 %
## (Intercept) 16.73899785 32.74210955
## x1           0.02212589  0.05040452
{% endhighlight %}



{% highlight r %}
anova(olsreg1)
{% endhighlight %}



{% highlight text %}
## Analysis of Variance Table
## 
## Response: y
##           Df Sum Sq Mean Sq F value    Pr(>F)    
## x1         1 2982.9 2982.91  26.594 4.714e-06 ***
## Residuals 48 5383.8  112.16                      
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
{% endhighlight %}
 
***Plotting regression line*** 

{% highlight r %}
ggplot(espn, aes(x = x1, y = y)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")
{% endhighlight %}



{% highlight text %}
## Error: Aesthetics must be either length 1 or the same as the data (50): x, y
{% endhighlight %}

![plot of chunk unnamed-chunk-5](/figures/unnamed-chunk-5-1.png)
 
***Predicted values of dependent variable***

{% highlight r %}
y1hat <- fitted(olsreg1)
summary(olsreg1)
{% endhighlight %}



{% highlight text %}
## 
## Call:
## lm(formula = y ~ x1)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -17.144  -6.709  -1.950   7.473  34.951 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 24.740554   3.979617   6.217 1.17e-07 ***
## x1           0.036265   0.007032   5.157 4.71e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 10.59 on 48 degrees of freedom
## Multiple R-squared:  0.3565,	Adjusted R-squared:  0.3431 
## F-statistic: 26.59 on 1 and 48 DF,  p-value: 4.714e-06
{% endhighlight %}



{% highlight r %}
ggplot(espn, aes(x = y1hat, y = y)) + 
  geom_point() 
{% endhighlight %}

![plot of chunk unnamed-chunk-6](/figures/unnamed-chunk-6-1.png)
 
***Multiple linear regression***

{% highlight r %}
olsreg2 <- lm(y ~ x)
{% endhighlight %}



{% highlight text %}
## Error in model.frame.default(formula = y ~ x, drop.unused.levels = TRUE): variable lengths differ (found for 'x')
{% endhighlight %}



{% highlight r %}
summary(olsreg2)
{% endhighlight %}



{% highlight text %}
## 
## Call:
## lm(formula = y ~ x)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -10.3791  -3.5460  -0.6701   1.5736  18.9007 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 42.888191   3.032987  14.141  < 2e-16 ***
## xRuns        0.081197   0.006144  13.215  < 2e-16 ***
## xInns       -3.671144   0.525224  -6.990 9.44e-09 ***
## xMatches     0.963545   0.941429   1.023    0.311    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.875 on 46 degrees of freedom
## Multiple R-squared:  0.8102,	Adjusted R-squared:  0.7978 
## F-statistic: 65.46 on 3 and 46 DF,  p-value: < 2.2e-16
{% endhighlight %}



{% highlight r %}
confint(olsreg2, leve=0.95)
{% endhighlight %}



{% highlight text %}
##                   2.5 %      97.5 %
## (Intercept) 36.78310580 48.99327672
## xRuns        0.06882855  0.09356495
## xInns       -4.72836531 -2.61392361
## xMatches    -0.93145430  2.85854372
{% endhighlight %}



{% highlight r %}
anova(olsreg2)
{% endhighlight %}



{% highlight text %}
## Analysis of Variance Table
## 
## Response: y
##           Df Sum Sq Mean Sq F value    Pr(>F)    
## x          3 6778.9 2259.65  65.464 < 2.2e-16 ***
## Residuals 46 1587.8   34.52                      
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
{% endhighlight %}
 
***Predicted values of dependent variable***

{% highlight r %}
yhat <- fitted(olsreg2)
summary(yhat)
{% endhighlight %}



{% highlight text %}
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   23.48   36.07   42.17   43.75   48.87   74.43
{% endhighlight %}
 
***Regression residuals***

{% highlight r %}
ehat <- resid(olsreg2)
summary(ehat)
{% endhighlight %}



{% highlight text %}
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## -10.3800  -3.5460  -0.6701   0.0000   1.5740  18.9000
{% endhighlight %}
 
