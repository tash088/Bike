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
    ##  Min.   : 12   Min.   :2011-01-12   Min.   :1.0   Min.   :0.00   Min.   : 1.0  
    ##  1st Qu.:206   1st Qu.:2011-07-25   1st Qu.:2.0   1st Qu.:0.00   1st Qu.: 3.8  
    ##  Median :344   Median :2011-12-10   Median :3.0   Median :0.00   Median : 7.0  
    ##  Mean   :356   Mean   :2011-12-21   Mean   :2.5   Mean   :0.47   Mean   : 6.5  
    ##  3rd Qu.:518   3rd Qu.:2012-05-31   3rd Qu.:3.0   3rd Qu.:1.00   3rd Qu.: 9.0  
    ##  Max.   :719   Max.   :2012-12-19   Max.   :4.0   Max.   :1.00   Max.   :12.0  
    ##     holiday         weekday    workingday     weathersit       temp     
    ##  Min.   :0.000   Min.   :3   Min.   :0.00   Min.   :1.0   Min.   :0.11  
    ##  1st Qu.:0.000   1st Qu.:3   1st Qu.:1.00   1st Qu.:1.0   1st Qu.:0.35  
    ##  Median :0.000   Median :3   Median :1.00   Median :1.0   Median :0.54  
    ##  Mean   :0.013   Mean   :3   Mean   :0.99   Mean   :1.4   Mean   :0.52  
    ##  3rd Qu.:0.000   3rd Qu.:3   3rd Qu.:1.00   3rd Qu.:2.0   3rd Qu.:0.67  
    ##  Max.   :1.000   Max.   :3   Max.   :1.00   Max.   :3.0   Max.   :0.79  
    ##      atemp           hum         windspeed          cnt      
    ##  Min.   :0.12   Min.   :0.40   Min.   :0.061   Min.   : 705  
    ##  1st Qu.:0.35   1st Qu.:0.56   1st Qu.:0.129   1st Qu.:2653  
    ##  Median :0.52   Median :0.64   Median :0.169   Median :4642  
    ##  Mean   :0.49   Mean   :0.65   Mean   :0.183   Mean   :4534  
    ##  3rd Qu.:0.62   3rd Qu.:0.75   3rd Qu.:0.234   3rd Qu.:5846  
    ##  Max.   :0.75   Max.   :0.97   Max.   :0.415   Max.   :8173

``` r
#view the distribution of the variable of interest
g<-ggplot(bikeTrain,aes(x=cnt,y=..density..))
g+geom_histogram()+labs(title="Histogram of Bike Counts")#how to fix y-axis %s
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Wednesday_files/figure-gfm/EDA-1.png)<!-- -->

``` r
#Create scatterplots for variables that are likely to be important predictors
g<-ggplot(bikeTrain,aes(y=cnt,fill=as.factor(workingday)))

g+geom_boxplot()+ylab("Bike Count")+xlab("Is it a Working Day?")+ 
  labs(title="Relationship of workingday to cnt") #why is x-axis -0.2 to 0.2?
```

![](Wednesday_files/figure-gfm/EDA-2.png)<!-- -->

``` r
g<-ggplot(bikeTrain,aes(y=cnt,fill=as.factor(weathersit)))

g+geom_boxplot()+ylab("Bike Count")+xlab("What is the weather like")+ 
  labs(title="Relationship of weathersit to cnt") #why is x-axis -0.2 to 0.2?
```

![](Wednesday_files/figure-gfm/EDA-3.png)<!-- -->

``` r
g<-ggplot(bikeTrain,aes(y=cnt,fill=as.factor(season)))

g+geom_boxplot()+ylab("Bike Count")+xlab("What season is it?")+ 
  labs(title="Relationship of season to cnt") #why is x-axis -0.2 to 0.2?
```

![](Wednesday_files/figure-gfm/EDA-4.png)<!-- -->

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
    ## 1 0.1007572 1328.191 0.54735171 1088.520
    ## 2 0.2437851 1727.282 0.26296344 1496.171
    ## 3 0.4198293 2321.965 0.03985709 2120.212

``` r
treeFit$bestTune
```

    ##          cp
    ## 1 0.1007572

``` r
#boosted tree model
boostFit<-train(cnt ~ ., data=bikeTrain, method="gbm", 
                preProcess=c("center","scale"),
                trControl=train_control, 
                tuneGrid=NULL, verbose=FALSE) #why does it produce all those different iterations, with no apparent variation?

boostFit$results
```

    ##   n.trees interaction.depth shrinkage n.minobsinnode     RMSE  Rsquared
    ## 1      50                 1       0.1             10 827.4874 0.8291514
    ## 2      50                 2       0.1             10 779.1219 0.8460801
    ## 3      50                 3       0.1             10 779.1762 0.8447390
    ## 4     100                 1       0.1             10 789.2118 0.8384853
    ## 5     100                 2       0.1             10 777.9493 0.8433157
    ## 6     100                 3       0.1             10 767.0247 0.8475462
    ## 7     150                 1       0.1             10 789.7774 0.8381914
    ## 8     150                 2       0.1             10 777.2049 0.8433648
    ## 9     150                 3       0.1             10 765.4033 0.8481815
    ##        MAE
    ## 1 656.2293
    ## 2 586.3445
    ## 3 593.9482
    ## 4 603.3537
    ## 5 563.2886
    ## 6 568.0358
    ## 7 599.7169
    ## 8 559.7228
    ## 9 561.4640

``` r
boostFit$bestTune
```

    ##   n.trees interaction.depth shrinkage n.minobsinnode
    ## 9     150                 3       0.1             10

Finally, we will make predictions using our best model fits and data
from the test set. We can compare RMSE to determine which one is best
(smaller RMSE is better).

``` r
#predict values on test set and compare RMSEs
treePred<-predict(treeFit,newdata=dplyr::select(bikeTest,-cnt))
postResample(treePred,bikeTest$cnt)
```

    ##         RMSE     Rsquared          MAE 
    ## 1138.6610904    0.6636144  926.7108600

``` r
boostPred<-predict(boostFit,newdata=dplyr::select(bikeTest,-cnt))
postResample(boostPred,bikeTest$cnt)
```

    ##        RMSE    Rsquared         MAE 
    ## 486.8989927   0.9390605 364.7775668

As expected, the boosted tree tends to have lower RMSE when applied to
the test data. By fitting multiple trees sequentially, rather than just
a single tree, the boosting method provides a better prediction.
