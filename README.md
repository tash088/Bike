---
title: "Automation"
author: "Taylor Ashby"
date: "2020-10-15"
output: rmarkdown::github_document
---

This file creates the six different reports, one for each day of the week (excluding Sundays, since there is no data for Sundays). This is accomplished by knitting with parameters. 

```{r automate}
dayID<-c(1:6)
output_file<-c("Monday.html","Tuesday.html","Wednesday.html","Thursday.html",
                    "Friday.html","Saturday.html")
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

