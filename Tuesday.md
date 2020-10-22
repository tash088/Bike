Project\_2
================
Taylor Ashby
10/16/2020

# Introduction

For this analysis, our goal is to predict the daily count of bikes used
from a Washington, D.C.-area bike sharing service. There are several
variables included that we would expect to influence how many bikes are
used. For this analysis, we will focus on the following variables:
season, yr, mnth, workingday, weathersit, atemp, and windspeed.
Descriptions of these variables can be found
[here](https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset).

We will use two methods to model the *cnt* variable: regression tree and
boosted tree model. A regression tree is a model that splits data into
regions and makes a prediction based on the mean value within a given
region. A boosted tree model is a way to enhance the prediction by
growing trees sequentially by modeling the residuals of the previous
tree.

The first thing we need to is to read in the data. Here we will also
drop the casual and registered variables, as they are classifications
that are not relevant to our analysis of the total daily bike count. We
will then filter to the weekday of interest.

``` r
#Read in bike share data
bike<-read_csv("day.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   instant = col_double(),
    ##   dteday = col_date(format = ""),
    ##   season = col_double(),
    ##   yr = col_double(),
    ##   mnth = col_double(),
    ##   holiday = col_double(),
    ##   weekday = col_double(),
    ##   workingday = col_double(),
    ##   weathersit = col_double(),
    ##   temp = col_double(),
    ##   atemp = col_double(),
    ##   hum = col_double(),
    ##   windspeed = col_double(),
    ##   casual = col_double(),
    ##   registered = col_double(),
    ##   cnt = col_double()
    ## )

``` r
#Drop casual and registered variables, then filter to desired weekday
bike<-select(bike,-casual,-registered) %>% filter(weekday==params$day)
```

Next we need to split the data into a training and test set.

``` r
#Partition into training and test sets
set.seed(123)
bikeIndex<-createDataPartition(bike$cnt,p=0.7,list=FALSE)
bikeTrain<-bike[bikeIndex,]
bikeTest<-bike[bikeIndex,]
```

Before we begin modeling the *cnt* variable, we should view summary
statistics and plot some of the variables to better understand the data
and their relationships with the variable of interest.

``` r
#View summary statistics for training data
summary(bikeTrain,digits=2)
```

    ##     instant        dteday               season          yr            mnth     
    ##  Min.   : 11   Min.   :2011-01-11   Min.   :1.0   Min.   :0.00   Min.   : 1.0  
    ##  1st Qu.:196   1st Qu.:2011-07-15   1st Qu.:2.0   1st Qu.:0.00   1st Qu.: 3.8  
    ##  Median :350   Median :2011-12-16   Median :3.0   Median :0.00   Median : 7.0  
    ##  Mean   :360   Mean   :2011-12-26   Mean   :2.6   Mean   :0.49   Mean   : 6.5  
    ##  3rd Qu.:531   3rd Qu.:2012-06-13   3rd Qu.:4.0   3rd Qu.:1.00   3rd Qu.:10.0  
    ##  Max.   :718   Max.   :2012-12-18   Max.   :4.0   Max.   :1.00   Max.   :12.0  
    ##     holiday     weekday    workingday   weathersit       temp          atemp     
    ##  Min.   :0   Min.   :2   Min.   :1    Min.   :1.0   Min.   :0.15   Min.   :0.13  
    ##  1st Qu.:0   1st Qu.:2   1st Qu.:1    1st Qu.:1.0   1st Qu.:0.37   1st Qu.:0.36  
    ##  Median :0   Median :2   Median :1    Median :1.0   Median :0.49   Median :0.48  
    ##  Mean   :0   Mean   :2   Mean   :1    Mean   :1.4   Mean   :0.50   Mean   :0.48  
    ##  3rd Qu.:0   3rd Qu.:2   3rd Qu.:1    3rd Qu.:2.0   3rd Qu.:0.65   3rd Qu.:0.60  
    ##  Max.   :0   Max.   :2   Max.   :1    Max.   :3.0   Max.   :0.82   Max.   :0.76  
    ##       hum         windspeed          cnt      
    ##  Min.   :0.31   Min.   :0.053   Min.   : 683  
    ##  1st Qu.:0.57   1st Qu.:0.126   1st Qu.:3579  
    ##  Median :0.66   Median :0.174   Median :4576  
    ##  Mean   :0.65   Mean   :0.185   Mean   :4487  
    ##  3rd Qu.:0.74   3rd Qu.:0.226   3rd Qu.:5758  
    ##  Max.   :0.96   Max.   :0.388   Max.   :7538

``` r
#view the distribution of the variable of interest
g<-ggplot(bikeTrain,aes(x=cnt,y=..density..))
g+geom_histogram()+labs(title="Histogram of Bike Counts")#how to fix y-axis %s
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Tuesday_files/figure-gfm/EDA-1.png)<!-- -->

``` r
#Create scatterplots for variables that are likely to be important predictors
g<-ggplot(bikeTrain,aes(y=cnt,fill=as.factor(workingday)))

g+geom_boxplot()+ylab("Bike Count")+xlab("Is it a Working Day?")+ 
  labs(title="Relationship of workingday to cnt") #why is x-axis -0.2 to 0.2?
```

![](Tuesday_files/figure-gfm/EDA-2.png)<!-- -->

``` r
g<-ggplot(bikeTrain,aes(y=cnt,fill=as.factor(weathersit)))

g+geom_boxplot()+ylab("Bike Count")+xlab("What is the weather like")+ 
  labs(title="Relationship of weathersit to cnt") #why is x-axis -0.2 to 0.2?
```

![](Tuesday_files/figure-gfm/EDA-3.png)<!-- -->

``` r
g<-ggplot(bikeTrain,aes(y=cnt,fill=as.factor(season)))

g+geom_boxplot()+ylab("Bike Count")+xlab("What season is it?")+ 
  labs(title="Relationship of season to cnt") #why is x-axis -0.2 to 0.2?
```

![](Tuesday_files/figure-gfm/EDA-4.png)<!-- -->

It looks like there are a few variables we will not need for modeling.
It will be easier to work with if we simply remove those from the
training data set.

``` r
#first remove variables that are not applicable for prediction of cnt
bikeTrain<-bikeTrain %>% select(-instant,-dteday)

#we can also remove variables where their information is captured by other variables
#i.e., we do not need to include the weekday and workingday indicators since we are
#going to create separate reports for each weekday.
bikeTrain<-bikeTrain %>% select(-holiday,-workingday,-temp, -hum)
```

Now we are ready to fit the models. First we will fit a tree model,
which separates the data into regions and uses the mean of the a given
region to predict the responses. Then we will try a boosted tree model.
This approach grows many trees sequentially, modeling residuals as the
responses. By aggregating over many trees, we lose some
interpretability, but the predictions should be better.

As we fit the models, we are also using “leave one out” cross-validation
to determine the optimal values of the tuning parameters so that we can
get the optimal model fit. The output will automatically use the
bestTune values of these parameters when we go to make predictions.

``` r
#now we are ready to fit the tree
train_control<-trainControl(method="LOOCV")
treeFit<-train(cnt ~ ., data=bikeTrain, method="rpart", 
               preProcess=c("center","scale"),
               trControl=train_control, 
               tuneGrid=NULL)
```

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

``` r
treeFit$results
treeFit$bestTune

#boosted tree model
boostFit<-train(cnt ~ ., data=bikeTrain, method="gbm", 
                preProcess=c("center","scale"),
                trControl=train_control, 
                tuneGrid=NULL, verbose=FALSE) #why does it produce all those different iterations, with no apparent variation?
```

    ## Warning in preProcess.default(method = c("center", "scale"), x = structure(c(1, : These
    ## variables have zero variances: weekday

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

    ## Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10, :
    ## These variables have zero variances: weekday

    ## Warning in (function (x, y, offset = NULL, misc = NULL, distribution = "bernoulli", :
    ## variable 4: weekday has no variation.

``` r
boostFit$results
boostFit$bestTune
```

Finally, we will make predictions using our best model fits and data
from the test set. We can compare RMSE to determine which one is best
(smaller RMSE is better).

``` r
#predict values on test set and compare RMSEs
treePred<-predict(treeFit,newdata=dplyr::select(bikeTest,-cnt))
a<- postResample(treePred,bikeTest$cnt)

boostPred<-predict(boostFit,newdata=dplyr::select(bikeTest,-cnt))
b<- postResample(boostPred,bikeTest$cnt)
```

As expected, the boosted tree tends to have lower RMSE when applied to
the test data. By fitting multiple trees sequentially, rather than just
a single tree, the boosting method provides a better prediction.

# Second Part

## Linear model fit

``` r
#Secondary Analysis of the tree fit

Fit2<- lm(cnt ~  season + weathersit + windspeed + atemp, data=bikeTrain)
Fit2
```

    ## 
    ## Call:
    ## lm(formula = cnt ~ season + weathersit + windspeed + atemp, data = bikeTrain)
    ## 
    ## Coefficients:
    ## (Intercept)       season   weathersit    windspeed        atemp  
    ##      2286.0        218.1       -889.3       -701.2       6315.1

``` r
#predict values on test set and compare RMSEs
Pred2<-predict(Fit2,newdata=dplyr::select(bikeTest,-cnt, season, weathersit, windspeed, atemp))
c<- postResample(Pred2,bikeTest$cnt)
```

\#table the RMSE from both of the model fits

``` r
d<- c(a[1], b[1], c[1])
names(d)<- c("Tree_RMSE","Boosted_RMSE", "LM_RMSE" )
d
```

    ##    Tree_RMSE Boosted_RMSE      LM_RMSE 
    ##    1167.7218     554.3124    1206.9999

From the above table the model having lowest value of RMSE is chosen to
be appropriate to fit the data set.
