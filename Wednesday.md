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
    ##     holiday         weekday    workingday     weathersit       temp          atemp     
    ##  Min.   :0.000   Min.   :3   Min.   :0.00   Min.   :1.0   Min.   :0.11   Min.   :0.12  
    ##  1st Qu.:0.000   1st Qu.:3   1st Qu.:1.00   1st Qu.:1.0   1st Qu.:0.35   1st Qu.:0.35  
    ##  Median :0.000   Median :3   Median :1.00   Median :1.0   Median :0.54   Median :0.52  
    ##  Mean   :0.013   Mean   :3   Mean   :0.99   Mean   :1.4   Mean   :0.52   Mean   :0.49  
    ##  3rd Qu.:0.000   3rd Qu.:3   3rd Qu.:1.00   3rd Qu.:2.0   3rd Qu.:0.67   3rd Qu.:0.62  
    ##  Max.   :1.000   Max.   :3   Max.   :1.00   Max.   :3.0   Max.   :0.79   Max.   :0.75  
    ##       hum         windspeed          cnt      
    ##  Min.   :0.40   Min.   :0.061   Min.   : 705  
    ##  1st Qu.:0.56   1st Qu.:0.129   1st Qu.:2653  
    ##  Median :0.64   Median :0.169   Median :4642  
    ##  Mean   :0.65   Mean   :0.183   Mean   :4534  
    ##  3rd Qu.:0.75   3rd Qu.:0.234   3rd Qu.:5846  
    ##  Max.   :0.97   Max.   :0.415   Max.   :8173

``` r
#view the distribution of the variable of interest
g<-ggplot(bikeTrain,aes(x=cnt,y=..density..))
g+geom_histogram()+labs(title="Histogram of Bike Counts")#how to fix y-axis %s
```

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
treeFit$results
treeFit$bestTune

#boosted tree model
boostFit<-train(cnt ~ ., data=bikeTrain, method="gbm", 
                preProcess=c("center","scale"),
                trControl=train_control, 
                tuneGrid=NULL, verbose=FALSE) #why does it produce all those different iterations, with no apparent variation?

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
    ##      3795.3        241.5      -1475.3      -2489.6       5500.9

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
    ##     1138.661      486.899     1233.929

From the above table the model having lowest value of RMSE is chosen to
be appropriate to fit the data set.
