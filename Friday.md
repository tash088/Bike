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
    ##  Min.   : 14   Min.   :2011-01-14   Min.   :1.0   Min.   :0.00   Min.   : 1.0  
    ##  1st Qu.:187   1st Qu.:2011-07-06   1st Qu.:2.0   1st Qu.:0.00   1st Qu.: 4.0  
    ##  Median :354   Median :2011-12-19   Median :2.0   Median :0.00   Median : 6.0  
    ##  Mean   :360   Mean   :2011-12-25   Mean   :2.5   Mean   :0.49   Mean   : 6.5  
    ##  3rd Qu.:513   3rd Qu.:2012-05-26   3rd Qu.:3.2   3rd Qu.:1.00   3rd Qu.: 9.0  
    ##  Max.   :721   Max.   :2012-12-21   Max.   :4.0   Max.   :1.00   Max.   :12.0  
    ##     holiday         weekday    workingday     weathersit       temp     
    ##  Min.   :0.000   Min.   :5   Min.   :0.00   Min.   :1.0   Min.   :0.16  
    ##  1st Qu.:0.000   1st Qu.:5   1st Qu.:1.00   1st Qu.:1.0   1st Qu.:0.33  
    ##  Median :0.000   Median :5   Median :1.00   Median :1.0   Median :0.44  
    ##  Mean   :0.026   Mean   :5   Mean   :0.97   Mean   :1.4   Mean   :0.48  
    ##  3rd Qu.:0.000   3rd Qu.:5   3rd Qu.:1.00   3rd Qu.:2.0   3rd Qu.:0.65  
    ##  Max.   :1.000   Max.   :5   Max.   :1.00   Max.   :2.0   Max.   :0.83  
    ##      atemp           hum         windspeed          cnt      
    ##  Min.   :0.16   Min.   :0.35   Min.   :0.058   Min.   :1421  
    ##  1st Qu.:0.32   1st Qu.:0.53   1st Qu.:0.138   1st Qu.:3386  
    ##  Median :0.43   Median :0.60   Median :0.174   Median :4597  
    ##  Mean   :0.46   Mean   :0.60   Mean   :0.190   Mean   :4602  
    ##  3rd Qu.:0.60   3rd Qu.:0.69   3rd Qu.:0.231   3rd Qu.:5772  
    ##  Max.   :0.79   Max.   :0.97   Max.   :0.378   Max.   :8156

``` r
#view the distribution of the variable of interest
g<-ggplot(bikeTrain,aes(x=cnt,y=..density..))
g+geom_histogram()+labs(title="Histogram of Bike Counts")#how to fix y-axis %s
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Friday_files/figure-gfm/EDA-1.png)<!-- -->

``` r
#Create scatterplots for variables that are likely to be important predictors
g<-ggplot(bikeTrain,aes(y=cnt,fill=as.factor(workingday)))

g+geom_boxplot()+ylab("Bike Count")+xlab("Is it a Working Day?")+ 
  labs(title="Relationship of workingday to cnt") #why is x-axis -0.2 to 0.2?
```

![](Friday_files/figure-gfm/EDA-2.png)<!-- -->

``` r
g<-ggplot(bikeTrain,aes(y=cnt,fill=as.factor(weathersit)))

g+geom_boxplot()+ylab("Bike Count")+xlab("What is the weather like")+ 
  labs(title="Relationship of weathersit to cnt") #why is x-axis -0.2 to 0.2?
```

![](Friday_files/figure-gfm/EDA-3.png)<!-- -->

``` r
g<-ggplot(bikeTrain,aes(y=cnt,fill=as.factor(season)))

g+geom_boxplot()+ylab("Bike Count")+xlab("What season is it?")+ 
  labs(title="Relationship of season to cnt") #why is x-axis -0.2 to 0.2?
```

![](Friday_files/figure-gfm/EDA-4.png)<!-- -->

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
    ## 1 0.1674123 1439.440 0.37704560 1234.692
    ## 2 0.2399066 1750.890 0.15184439 1541.974
    ## 3 0.3624279 2119.489 0.07328424 1949.157

``` r
treeFit$bestTune
```

    ##          cp
    ## 1 0.1674123

``` r
#boosted tree model
boostFit<-train(cnt ~ ., data=bikeTrain, method="gbm", 
                preProcess=c("center","scale"),
                trControl=train_control, 
                tuneGrid=NULL, verbose=FALSE) #why does it produce all those different iterations, with no apparent variation?

boostFit$results
```

    ##   n.trees interaction.depth shrinkage n.minobsinnode     RMSE  Rsquared
    ## 1      50                 1       0.1             10 899.5037 0.7561420
    ## 2      50                 2       0.1             10 907.4732 0.7470486
    ## 3      50                 3       0.1             10 868.0506 0.7698268
    ## 4     100                 1       0.1             10 863.1176 0.7709535
    ## 5     100                 2       0.1             10 911.6087 0.7456746
    ## 6     100                 3       0.1             10 865.4107 0.7695817
    ## 7     150                 1       0.1             10 878.0346 0.7627715
    ## 8     150                 2       0.1             10 915.7235 0.7445643
    ## 9     150                 3       0.1             10 866.9761 0.7688037
    ##        MAE
    ## 1 707.9989
    ## 2 682.6004
    ## 3 666.7494
    ## 4 675.1431
    ## 5 686.2991
    ## 6 657.2850
    ## 7 688.1120
    ## 8 698.4532
    ## 9 665.6807

``` r
boostFit$bestTune
```

    ##   n.trees interaction.depth shrinkage n.minobsinnode
    ## 4     100                 1       0.1             10

Finally, we will make predictions using our best model fits and data
from the test set. We can compare RMSE to determine which one is best
(smaller RMSE is better).

``` r
#predict values on test set and compare RMSEs
treePred<-predict(treeFit,newdata=dplyr::select(bikeTest,-cnt))
postResample(treePred,bikeTest$cnt)
```

    ##         RMSE     Rsquared          MAE 
    ## 1136.7145627    0.6023345  938.2367371

``` r
boostPred<-predict(boostFit,newdata=dplyr::select(bikeTest,-cnt))
postResample(boostPred,bikeTest$cnt)
```

    ##        RMSE    Rsquared         MAE 
    ## 691.6329057   0.8528442 526.2074319

As expected, the boosted tree tends to have lower RMSE when applied to
the test data. By fitting multiple trees sequentially, rather than just
a single tree, the boosting method provides a better prediction.
