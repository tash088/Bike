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
    ##  Min.   : 13   Min.   :2011-01-13   Min.   :1.0   Min.   :0.00   Min.   : 1.0  
    ##  1st Qu.:207   1st Qu.:2011-07-26   1st Qu.:2.0   1st Qu.:0.00   1st Qu.: 4.0  
    ##  Median :346   Median :2011-12-11   Median :3.0   Median :0.00   Median : 7.0  
    ##  Mean   :360   Mean   :2011-12-25   Mean   :2.6   Mean   :0.47   Mean   : 6.7  
    ##  3rd Qu.:519   3rd Qu.:2012-06-01   3rd Qu.:4.0   3rd Qu.:1.00   3rd Qu.: 9.2  
    ##  Max.   :720   Max.   :2012-12-20   Max.   :4.0   Max.   :1.00   Max.   :12.0  
    ##     holiday         weekday    workingday     weathersit       temp     
    ##  Min.   :0.000   Min.   :4   Min.   :0.00   Min.   :1.0   Min.   :0.17  
    ##  1st Qu.:0.000   1st Qu.:4   1st Qu.:1.00   1st Qu.:1.0   1st Qu.:0.36  
    ##  Median :0.000   Median :4   Median :1.00   Median :1.0   Median :0.50  
    ##  Mean   :0.026   Mean   :4   Mean   :0.97   Mean   :1.4   Mean   :0.51  
    ##  3rd Qu.:0.000   3rd Qu.:4   3rd Qu.:1.00   3rd Qu.:2.0   3rd Qu.:0.66  
    ##  Max.   :1.000   Max.   :4   Max.   :1.00   Max.   :3.0   Max.   :0.81  
    ##      atemp           hum         windspeed          cnt      
    ##  Min.   :0.15   Min.   :0.00   Min.   :0.053   Min.   : 623  
    ##  1st Qu.:0.37   1st Qu.:0.54   1st Qu.:0.136   1st Qu.:3271  
    ##  Median :0.49   Median :0.60   Median :0.183   Median :4670  
    ##  Mean   :0.48   Mean   :0.61   Mean   :0.190   Mean   :4627  
    ##  3rd Qu.:0.62   3rd Qu.:0.70   3rd Qu.:0.226   3rd Qu.:6249  
    ##  Max.   :0.83   Max.   :0.94   Max.   :0.442   Max.   :7765

``` r
#view the distribution of the variable of interest
g<-ggplot(bikeTrain,aes(x=cnt,y=..density..))
g+geom_histogram()+labs(title="Histogram of Bike Counts")#how to fix y-axis %s
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Thursday_files/figure-gfm/EDA-1.png)<!-- -->

``` r
#Create scatterplots for variables that are likely to be important predictors
g<-ggplot(bikeTrain,aes(y=cnt,fill=as.factor(workingday)))

g+geom_boxplot()+ylab("Bike Count")+xlab("Is it a Working Day?")+ 
  labs(title="Relationship of workingday to cnt") #why is x-axis -0.2 to 0.2?
```

![](Thursday_files/figure-gfm/EDA-2.png)<!-- -->

``` r
g<-ggplot(bikeTrain,aes(y=cnt,fill=as.factor(weathersit)))

g+geom_boxplot()+ylab("Bike Count")+xlab("What is the weather like")+ 
  labs(title="Relationship of weathersit to cnt") #why is x-axis -0.2 to 0.2?
```

![](Thursday_files/figure-gfm/EDA-3.png)<!-- -->

``` r
g<-ggplot(bikeTrain,aes(y=cnt,fill=as.factor(season)))

g+geom_boxplot()+ylab("Bike Count")+xlab("What season is it?")+ 
  labs(title="Relationship of season to cnt") #why is x-axis -0.2 to 0.2?
```

![](Thursday_files/figure-gfm/EDA-4.png)<!-- -->

It looks like there are a few variables we will not need for modeling.
It will be easier to work with if we simply remove those from the
training data set.

``` r
#first remove variables that are not applicable for prediction of cnt
bikeTrain<-bikeTrain %>% select(-instant,-dteday)

#we can also remove variables where their information is captured by other variables
#i.e., we do not need to include the weekday and workingday indicators since we are
#going to create separate reports for each weekday.
bikeTrain<-bikeTrain %>% select(-holiday,-weekday,-workingday,-temp, -hum)
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
treeFit$results
```

    ##          cp     RMSE   Rsquared      MAE
    ## 1 0.1352870 1289.153 0.54636622 1122.810
    ## 2 0.1875221 1460.027 0.40936541 1261.016
    ## 3 0.4667418 2119.234 0.01210054 1937.674

``` r
treeFit$bestTune
```

    ##         cp
    ## 1 0.135287

``` r
#boosted tree model
boostFit<-train(cnt ~ ., data=bikeTrain, method="gbm", 
                preProcess=c("center","scale"),
                trControl=train_control, 
                tuneGrid=NULL, verbose=FALSE) #why does it produce all those different iterations, with no apparent variation?

boostFit$results
```

    ##   n.trees interaction.depth shrinkage n.minobsinnode     RMSE  Rsquared
    ## 1      50                 1       0.1             10 812.5838 0.8163856
    ## 2      50                 2       0.1             10 825.2998 0.8087028
    ## 3      50                 3       0.1             10 790.6724 0.8248495
    ## 4     100                 1       0.1             10 803.5476 0.8177617
    ## 5     100                 2       0.1             10 777.4192 0.8295261
    ## 6     100                 3       0.1             10 759.2488 0.8374275
    ## 7     150                 1       0.1             10 808.8668 0.8153416
    ## 8     150                 2       0.1             10 778.5537 0.8294458
    ## 9     150                 3       0.1             10 766.5425 0.8341938
    ##        MAE
    ## 1 622.9556
    ## 2 632.3371
    ## 3 612.0813
    ## 4 615.6472
    ## 5 580.2611
    ## 6 577.7925
    ## 7 603.0952
    ## 8 577.1669
    ## 9 578.3788

``` r
boostFit$bestTune
```

    ##   n.trees interaction.depth shrinkage n.minobsinnode
    ## 6     100                 3       0.1             10

Finally, we will make predictions using our best model fits and data
from the test set. We can compare RMSE to determine which one is best
(smaller RMSE is better).

``` r
#predict values on test set and compare RMSEs
treePred<-predict(treeFit,newdata=dplyr::select(bikeTest,-cnt))
postResample(treePred,bikeTest$cnt)
```

    ##         RMSE     Rsquared          MAE 
    ## 1106.5819589    0.6542639  921.9167920

``` r
boostPred<-predict(boostFit,newdata=dplyr::select(bikeTest,-cnt))
postResample(boostPred,bikeTest$cnt)
```

    ##        RMSE    Rsquared         MAE 
    ## 544.0830043   0.9174626 383.7867219

As expected, the boosted tree tends to have lower RMSE when applied to
the test data. By fitting multiple trees sequentially, rather than just
a single tree, the boosting method provides a better prediction.
