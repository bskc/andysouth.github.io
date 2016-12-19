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
 
 
Cricket has always been the most popular sport in India. Some people are just obsessed with 'numbers'. From most 'number' of runs scored to most 'number' of wickets taken and the list goes on. I haven't followed cricket for a while now, might come as a surprise to many, considering that i live in a cricket frenzy nation. It wasn't until recently i started following the sport again, which left me curious to find out which players have been doing well and which weren't. Afterall, i was trying to reconnect with game after a long hiatus.
 
 
: ![](http://www.espncricinfo.com/db/PICTURES/CMS/246000/246097.jpg)
 
Here is the [webpage](http://stats.espncricinfo.com/ci/engine/records/averages/batting.html?class=1;id=2016;type=year) from a popular sports website, espncricinfo, holding data of batsmen records from the current England and Indian crickets teams in the year 2016 so far.
 
*i am going to scrap data from the webpage in two different ways.*
 
 
### Method 1
 
__Load rvest package__

{% highlight r %}
library(rvest)
{% endhighlight %}
 
__Reading url__

{% highlight r %}
cricinfo <- read_html("http://stats.espncricinfo.com/ci/engine/records/averages/batting.html?class=1;id=2016;type=year")
{% endhighlight %}
 
__Web scrapping__

{% highlight r %}
#Extract Players names
Player <- cricinfo %>% html_nodes(".data1 .left") %>% html_text()
#Extract Matches played by batsmen
Matches <- cricinfo %>% html_nodes(".left+ td") %>% html_text()
#Extract the number of runs scored by batsmen
Runs <- Average <- cricinfo%>%html_nodes("td:nth-child(5)")%>%html_text()
#Extract highest scores by batsmen
Highestscore <- cricinfo %>% html_nodes("td:nth-child(6)") %>% html_text()
#Extract batting average's of the batsmen
Average <- cricinfo%>%html_nodes("td:nth-child(7)")%>%html_text()
# Creat a data frame with the above variables
cricinfo_table <- data.frame(Player,Matches,Runs,Highestscore,Average)
#Preview dataframe
head(cricinfo_table) 
{% endhighlight %}



{% highlight text %}
##              Player Matches Runs Highestscore Average
## 1    KJ Abbott (SA)       3   36           17    9.00
## 2      MM Ali (ENG)     17* 1034         155*   47.00
## 3      HM Amla (SA)       8  661          201   50.84
## 4  CJ Anderson (NZ)       2  150           72   37.50
## 5 JM Anderson (ENG)      12   66           17    8.25
## 6   ZS Ansari (ENG)       3   49           32    9.80
{% endhighlight %}
###### Table 1.
 
### Method 2
 

{% highlight r %}
#Extract the entire table(including all the rows and columns)
cricinfo1 <- cricinfo %>% html_nodes("table") %>% .[[1]] %>% html_table()
#Preview dataframe
head(cricinfo1)
{% endhighlight %}



{% highlight text %}
##              Player Mat Inns NO Runs   HS   Ave   BF    SR 100 50 0  4s 6s
## 1    KJ Abbott (SA)   3    4  0   36   17  9.00   97 37.11   0  0 1   2  1
## 2      MM Ali (ENG) 17*   28  6 1034 155* 47.00 1907 54.22   4  5 3 112 11
## 3      HM Amla (SA)   8   13  0  661  201 50.84 1292 51.16   2  3 1  94  2
## 4  CJ Anderson (NZ)   2    4  0  150   72 37.50  323 46.43   0  1 1  19  4
## 5 JM Anderson (ENG)  12   15  7   66   17  8.25   99 66.66   0  0 3   9  0
## 6   ZS Ansari (ENG)   3    5  0   49   32  9.80  136 36.02   0  0 2   5  0
{% endhighlight %}
###### Table 2.
 
### Method 1 vs Method 2
 
In method 1, there is visibly more number of lines of code. However,this also gives more control over picking the columns that you may need to carry out your data analysis. For instance, Out Of the 14 variables or columns in the table, i have selectively picked 5 columns (Table 1).The down side about this method besides more lines of code is that it requires a few more additional steps, that includes using [SelectorGadget](http://selectorgadget.com/),an open source tool for CSS selection from a webpage. Method 2, on the other hand, has only a single line of code resulting in scraping the contents of theentire table(Table 2).
