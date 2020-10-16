---
title: "Project 2 README"
author: "Taylor Ashby"
date: "2020-10-15"
output: rmarkdown::github_document
---
The purpose of this repo is to store relevant files and code for predicting the utilization (i.e., count) of bikes bikes in a D.C.-area bike sharing program. We conduct EDA and model the counts using tree/boosted tree modeling techniques.

The following packages are used in this analysis
```{r packages}
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(caret)
library(GGally)
library(scales)
library(corrplot)
library(rmarkdown)
```

The code below will create six different reports, one for each day of the week (excluding Sundays, since there is no data for Sundays). This is accomplished by knitting with parameters. 


```{r automate}
dayID<-c(1:6)
output_file<-c("Monday.md","Tuesday.md","Wednesday.md","Thursday.md",
                    "Friday.md","Saturday.md")
#create a list for each team with just the team name parameter
params = lapply(dayID, FUN = function(x){list(day = x)})

#put into a data frame 
reports <- dplyr::tibble(output_file, params)


library(rmarkdown)
#need to use x[[1]] to get at elements since tibble doesn't simplify
apply(reports, MARGIN = 1, 
            FUN = function(x){
                render(input = "Project_2.Rmd", output_file = x[[1]], params = x[[2]])
                })
```

The output files can be found at the links below:

[Monday is available here](Monday.md)
[Tuesday is available here](Tuesday.md)
[Wednesday is available here](Wednesday.md)
[Thursday is available here](Thursday.md)
[Friday is available here](Friday.md)
[Saturday is available here](Saturday.md)
