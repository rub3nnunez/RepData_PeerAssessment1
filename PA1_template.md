---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


1. Load the data

First of all we have to unzip the CSV file and prepare our dataset for the first exploration.


```r
unzip("./activity.zip")

monitoring<-read.csv("./activity.csv",header = TRUE)
```

2. Process/transform the data (if necessary) into a format suitable for your analysis

The next step is to make a little exploration to check if it is possible to perform and analysis of the data.


```r
summary(monitoring)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

```r
str(monitoring)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

We could se that the date column is a factor and we need a date to move in the analysis.


```r
monitoring$date <- as.Date(monitoring$date, format = "%Y-%m-%d")

str(monitoring)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


## What is mean total number of steps taken per day?

#### Needed library load


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

1. Calculate the total number of steps taken per day



```r
steps_by_day <- monitoring %>% group_by(date) %>% summarize(total_steps = sum(steps, na.rm=TRUE))
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
hist(steps_by_day$total_steps, col="yellow", main = "Total steps frequency", xlab="Total steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day

The mean of the dataset is:


```r
steps_mean <- mean(steps_by_day$total_steps)

print(steps_mean)
```

```
## [1] 9354.23
```

The median of the dataset is:


```r
steps_median <- median(steps_by_day$total_steps)

print(steps_median)
```

```
## [1] 10395
```


## What is the average daily activity pattern?

#### Needed library load


```r
library(ggplot2)
library(plotly)
```

```
## Warning: package 'plotly' was built under R version 3.6.2
```

```
## 
## Attaching package: 'plotly'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     last_plot
```

```
## The following object is masked from 'package:stats':
## 
##     filter
```

```
## The following object is masked from 'package:graphics':
## 
##     layout
```


1. Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
mean_by_day <- monitoring %>% group_by(date) %>% summarize(mean_steps = mean(steps, na.rm=TRUE))

p <- ggplot(mean_by_day, aes(x = date, y = mean_steps)) + geom_line() + 
        ggtitle("Mean Steps by Date") + xlab("Date") + ylab("Mean Steps")

p <- ggplotly(p)

p
```

<!--html_preserve--><div id="htmlwidget-6f481813ee0338f09d48" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-6f481813ee0338f09d48">{"x":{"data":[{"x":[15614,15615,15616,15617,15618,15619,15620,15621,15622,15623,15624,15625,15626,15627,15628,15629,15630,15631,15632,15633,15634,15635,15636,15637,15638,15639,15640,15641,15642,15643,15644,15645,15646,15647,15648,15649,15650,15651,15652,15653,15654,15655,15656,15657,15658,15659,15660,15661,15662,15663,15664,15665,15666,15667,15668,15669,15670,15671,15672,15673,15674],"y":[null,0.4375,39.4166666666667,42.0694444444444,46.1597222222222,53.5416666666667,38.2465277777778,null,44.4826388888889,34.375,35.7777777777778,60.3541666666667,43.1458333333333,52.4236111111111,35.2048611111111,52.375,46.7083333333333,34.9166666666667,41.0729166666667,36.09375,30.6284722222222,46.7361111111111,30.9652777777778,29.0104166666667,8.65277777777778,23.5347222222222,35.1354166666667,39.7847222222222,17.4236111111111,34.09375,53.5208333333333,null,36.8055555555556,36.7048611111111,null,36.2465277777778,28.9375,44.7326388888889,11.1770833333333,null,null,43.7777777777778,37.3784722222222,25.4722222222222,null,0.142361111111111,18.8923611111111,49.7881944444444,52.4652777777778,30.6979166666667,15.5277777777778,44.3993055555556,70.9270833333333,73.5902777777778,50.2708333333333,41.0902777777778,38.7569444444444,47.3819444444444,35.3576388888889,24.46875,null],"text":["date: 2012-10-01<br />mean_steps:        NaN","date: 2012-10-02<br />mean_steps:  0.4375000","date: 2012-10-03<br />mean_steps: 39.4166667","date: 2012-10-04<br />mean_steps: 42.0694444","date: 2012-10-05<br />mean_steps: 46.1597222","date: 2012-10-06<br />mean_steps: 53.5416667","date: 2012-10-07<br />mean_steps: 38.2465278","date: 2012-10-08<br />mean_steps:        NaN","date: 2012-10-09<br />mean_steps: 44.4826389","date: 2012-10-10<br />mean_steps: 34.3750000","date: 2012-10-11<br />mean_steps: 35.7777778","date: 2012-10-12<br />mean_steps: 60.3541667","date: 2012-10-13<br />mean_steps: 43.1458333","date: 2012-10-14<br />mean_steps: 52.4236111","date: 2012-10-15<br />mean_steps: 35.2048611","date: 2012-10-16<br />mean_steps: 52.3750000","date: 2012-10-17<br />mean_steps: 46.7083333","date: 2012-10-18<br />mean_steps: 34.9166667","date: 2012-10-19<br />mean_steps: 41.0729167","date: 2012-10-20<br />mean_steps: 36.0937500","date: 2012-10-21<br />mean_steps: 30.6284722","date: 2012-10-22<br />mean_steps: 46.7361111","date: 2012-10-23<br />mean_steps: 30.9652778","date: 2012-10-24<br />mean_steps: 29.0104167","date: 2012-10-25<br />mean_steps:  8.6527778","date: 2012-10-26<br />mean_steps: 23.5347222","date: 2012-10-27<br />mean_steps: 35.1354167","date: 2012-10-28<br />mean_steps: 39.7847222","date: 2012-10-29<br />mean_steps: 17.4236111","date: 2012-10-30<br />mean_steps: 34.0937500","date: 2012-10-31<br />mean_steps: 53.5208333","date: 2012-11-01<br />mean_steps:        NaN","date: 2012-11-02<br />mean_steps: 36.8055556","date: 2012-11-03<br />mean_steps: 36.7048611","date: 2012-11-04<br />mean_steps:        NaN","date: 2012-11-05<br />mean_steps: 36.2465278","date: 2012-11-06<br />mean_steps: 28.9375000","date: 2012-11-07<br />mean_steps: 44.7326389","date: 2012-11-08<br />mean_steps: 11.1770833","date: 2012-11-09<br />mean_steps:        NaN","date: 2012-11-10<br />mean_steps:        NaN","date: 2012-11-11<br />mean_steps: 43.7777778","date: 2012-11-12<br />mean_steps: 37.3784722","date: 2012-11-13<br />mean_steps: 25.4722222","date: 2012-11-14<br />mean_steps:        NaN","date: 2012-11-15<br />mean_steps:  0.1423611","date: 2012-11-16<br />mean_steps: 18.8923611","date: 2012-11-17<br />mean_steps: 49.7881944","date: 2012-11-18<br />mean_steps: 52.4652778","date: 2012-11-19<br />mean_steps: 30.6979167","date: 2012-11-20<br />mean_steps: 15.5277778","date: 2012-11-21<br />mean_steps: 44.3993056","date: 2012-11-22<br />mean_steps: 70.9270833","date: 2012-11-23<br />mean_steps: 73.5902778","date: 2012-11-24<br />mean_steps: 50.2708333","date: 2012-11-25<br />mean_steps: 41.0902778","date: 2012-11-26<br />mean_steps: 38.7569444","date: 2012-11-27<br />mean_steps: 47.3819444","date: 2012-11-28<br />mean_steps: 35.3576389","date: 2012-11-29<br />mean_steps: 24.4687500","date: 2012-11-30<br />mean_steps:        NaN"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(0,0,0,1)","dash":"solid"},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":43.7625570776256,"r":7.30593607305936,"b":40.1826484018265,"l":37.2602739726027},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"title":{"text":"Mean Steps by Date","font":{"color":"rgba(0,0,0,1)","family":"","size":17.5342465753425},"x":0,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[15611,15677],"tickmode":"array","ticktext":["oct. 01","oct. 15","nov. 01","nov. 15","dic. 01"],"tickvals":[15614,15628,15645,15659,15675],"categoryorder":"array","categoryarray":["oct. 01","oct. 15","nov. 01","nov. 15","dic. 01"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":"Date","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-3.53003472222222,77.2626736111111],"tickmode":"array","ticktext":["0","20","40","60"],"tickvals":[0,20,40,60],"categoryorder":"array","categoryarray":["0","20","40","60"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"Mean Steps","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","showSendToCloud":false},"source":"A","attrs":{"2c7833f8684a":{"x":{},"y":{},"type":"scatter"}},"cur_data":"2c7833f8684a","visdat":{"2c7833f8684a":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->



2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
mean_by_interval <- monitoring %>% group_by(interval) %>% summarize(mean_value = mean(steps, na.rm=TRUE))

filter(mean_by_interval, mean_by_interval$mean_value == max(mean_by_interval$mean_value))
```

```
## # A tibble: 1 x 2
##   interval mean_value
##      <int>      <dbl>
## 1      835       206.
```


## Imputing missing values


1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)


```r
sum(is.na(monitoring$steps))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Frist we are goint to create a function which will get the mean of the interval of the dataset.


```r
get.mean.by.interval <-function(interval_number){
        filter(mean_by_interval, mean_by_interval$interval==interval_number)$mean_value
}
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
monitoring.full <- monitoring

for (i in 1:nrow(monitoring.full)){
        if (is.na(monitoring.full[i,]$steps)){
                monitoring.full[i,]$steps <- get.mean.by.interval(monitoring.full[i,]$interval)
        }
        
}
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
steps_by_day.full <- monitoring.full %>% group_by(date) %>% summarize(total_steps = sum(steps, na.rm=TRUE))

hist(steps_by_day.full$total_steps, col="yellow", main = "Total steps frequency", xlab="Total steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

The new values are calculated


```r
steps_median.full <- median(steps_by_day.full$total_steps)
steps_mean.full <- mean(steps_by_day.full$total_steps)
```

If we compare the **median**


```r
print(steps_median)
```

```
## [1] 10395
```

```r
print(steps_median.full)
```

```
## [1] 10766.19
```

And If we compare the **mean**


```r
print(steps_mean)
```

```
## [1] 9354.23
```

```r
print(steps_mean.full)
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?

#### Needed library load


```r
library(timeDate)
library(plyr)
```

```
## ------------------------------------------------------------------------------
```

```
## You have loaded plyr after dplyr - this is likely to cause problems.
## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
## library(plyr); library(dplyr)
```

```
## ------------------------------------------------------------------------------
```

```
## 
## Attaching package: 'plyr'
```

```
## The following objects are masked from 'package:plotly':
## 
##     arrange, mutate, rename, summarise
```

```
## The following objects are masked from 'package:dplyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
```

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
monitoring.full$typeday <- isWeekday(monitoring.full$date)

monitoring.full$typeday <- as.factor(monitoring.full$typeday)

monitoring.full$typeday <- revalue(monitoring.full$typeday, c("FALSE"="weekend", "TRUE"="weekday"))

str(monitoring.full)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ typeday : Factor w/ 2 levels "weekend","weekday": 2 2 2 2 2 2 2 2 2 2 ...
```


2. Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
steps_by_typeday.full <- aggregate(monitoring.full$steps ~ monitoring.full$interval + monitoring.full$typeday, monitoring.full, mean)

ggplot(steps_by_typeday.full, 
       aes(x = steps_by_typeday.full$`monitoring.full$interval`, y = steps_by_typeday.full$`monitoring.full$steps`))       + geom_line() + facet_wrap(~steps_by_typeday.full$`monitoring.full$typeday`) 
```

![](PA1_template_files/figure-html/unnamed-chunk-21-1.png)<!-- -->


