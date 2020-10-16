Project 2 README
================
Taylor Ashby
10/15/2020

The purpose of this repo is to store relevant files and code for
predicting the utilization (i.e., count) of bikes bikes in a D.C.-area
bike sharing program. We conduct EDA and model the counts using
tree/boosted tree modeling techniques.

The following packages are used in this analysis

``` r
library(tidyverse)
```

    ## ── Attaching packages ────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.3     ✓ dplyr   1.0.2
    ## ✓ tidyr   1.1.1     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ───────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(dplyr)
library(ggplot2)
library(readr)
library(caret)
```

    ## Loading required package: lattice

    ## 
    ## Attaching package: 'caret'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
library(GGally)
```

    ## Registered S3 method overwritten by 'GGally':
    ##   method from   
    ##   +.gg   ggplot2

``` r
library(scales)
```

    ## 
    ## Attaching package: 'scales'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     discard

    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

``` r
library(corrplot)
```

    ## corrplot 0.84 loaded

``` r
library(rmarkdown)
```

The code below will create six different reports, one for each day of
the week (excluding Sundays, since there is no data for Sundays). This
is accomplished by knitting with parameters.

``` r
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

    ## 
    ## 
    ## processing file: Project_2.Rmd

    ##   |                                                                              |                                                                      |   0%  |                                                                              |.....                                                                 |   7%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.........                                                             |  13%
    ## label: setup (with options) 
    ## List of 1
    ##  $ include: logi FALSE
    ## 
    ##   |                                                                              |..............                                                        |  20%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |...................                                                   |  27%
    ## label: read
    ##   |                                                                              |.......................                                               |  33%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |............................                                          |  40%
    ## label: split
    ##   |                                                                              |.................................                                     |  47%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.....................................                                 |  53%
    ## label: EDA

    ##   |                                                                              |..........................................                            |  60%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |...............................................                       |  67%
    ## label: trimvars
    ##   |                                                                              |...................................................                   |  73%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |........................................................              |  80%
    ## label: fits
    ##   |                                                                              |.............................................................         |  87%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.................................................................     |  93%
    ## label: predict
    ##   |                                                                              |......................................................................| 100%
    ##   ordinary text without R code

    ## output file: Project_2.knit.md

    ## /Applications/RStudio.app/Contents/MacOS/pandoc/pandoc +RTS -K512m -RTS Project_2.utf8.md --to gfm --from markdown+autolink_bare_uris+tex_math_single_backslash --output Monday.md --standalone --template /Library/Frameworks/R.framework/Versions/4.0/Resources/library/rmarkdown/rmarkdown/templates/github_document/resources/default.md 
    ## /Applications/RStudio.app/Contents/MacOS/pandoc/pandoc +RTS -K512m -RTS Monday.md --to html4 --from gfm --output Monday.html --standalone --self-contained --highlight-style pygments --template /Library/Frameworks/R.framework/Versions/4.0/Resources/library/rmarkdown/rmarkdown/templates/github_document/resources/preview.html --variable 'github-markdown-css:/Library/Frameworks/R.framework/Versions/4.0/Resources/library/rmarkdown/rmarkdown/templates/github_document/resources/github.css' --email-obfuscation none --metadata pagetitle=PREVIEW

    ## 
    ## Preview created: /var/folders/h7/s_z1dr090z5376xv535833rh0000gn/T//Rtmp3IDzZB/preview-b7a82c059296.html

    ## 
    ## Output created: Monday.md

    ## 
    ## 
    ## processing file: Project_2.Rmd

    ##   |                                                                              |                                                                      |   0%  |                                                                              |.....                                                                 |   7%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.........                                                             |  13%
    ## label: setup (with options) 
    ## List of 1
    ##  $ include: logi FALSE
    ## 
    ##   |                                                                              |..............                                                        |  20%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |...................                                                   |  27%
    ## label: read
    ##   |                                                                              |.......................                                               |  33%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |............................                                          |  40%
    ## label: split
    ##   |                                                                              |.................................                                     |  47%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.....................................                                 |  53%
    ## label: EDA

    ##   |                                                                              |..........................................                            |  60%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |...............................................                       |  67%
    ## label: trimvars
    ##   |                                                                              |...................................................                   |  73%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |........................................................              |  80%
    ## label: fits
    ##   |                                                                              |.............................................................         |  87%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.................................................................     |  93%
    ## label: predict
    ##   |                                                                              |......................................................................| 100%
    ##   ordinary text without R code

    ## output file: Project_2.knit.md

    ## /Applications/RStudio.app/Contents/MacOS/pandoc/pandoc +RTS -K512m -RTS Project_2.utf8.md --to gfm --from markdown+autolink_bare_uris+tex_math_single_backslash --output Tuesday.md --standalone --template /Library/Frameworks/R.framework/Versions/4.0/Resources/library/rmarkdown/rmarkdown/templates/github_document/resources/default.md 
    ## /Applications/RStudio.app/Contents/MacOS/pandoc/pandoc +RTS -K512m -RTS Tuesday.md --to html4 --from gfm --output Tuesday.html --standalone --self-contained --highlight-style pygments --template /Library/Frameworks/R.framework/Versions/4.0/Resources/library/rmarkdown/rmarkdown/templates/github_document/resources/preview.html --variable 'github-markdown-css:/Library/Frameworks/R.framework/Versions/4.0/Resources/library/rmarkdown/rmarkdown/templates/github_document/resources/github.css' --email-obfuscation none --metadata pagetitle=PREVIEW

    ## 
    ## Preview created: /var/folders/h7/s_z1dr090z5376xv535833rh0000gn/T//Rtmp3IDzZB/preview-b7a81bf3ab9.html

    ## 
    ## Output created: Tuesday.md

    ## 
    ## 
    ## processing file: Project_2.Rmd

    ##   |                                                                              |                                                                      |   0%  |                                                                              |.....                                                                 |   7%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.........                                                             |  13%
    ## label: setup (with options) 
    ## List of 1
    ##  $ include: logi FALSE
    ## 
    ##   |                                                                              |..............                                                        |  20%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |...................                                                   |  27%
    ## label: read
    ##   |                                                                              |.......................                                               |  33%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |............................                                          |  40%
    ## label: split
    ##   |                                                                              |.................................                                     |  47%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.....................................                                 |  53%
    ## label: EDA

    ##   |                                                                              |..........................................                            |  60%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |...............................................                       |  67%
    ## label: trimvars
    ##   |                                                                              |...................................................                   |  73%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |........................................................              |  80%
    ## label: fits
    ##   |                                                                              |.............................................................         |  87%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.................................................................     |  93%
    ## label: predict
    ##   |                                                                              |......................................................................| 100%
    ##   ordinary text without R code

    ## output file: Project_2.knit.md

    ## /Applications/RStudio.app/Contents/MacOS/pandoc/pandoc +RTS -K512m -RTS Project_2.utf8.md --to gfm --from markdown+autolink_bare_uris+tex_math_single_backslash --output Wednesday.md --standalone --template /Library/Frameworks/R.framework/Versions/4.0/Resources/library/rmarkdown/rmarkdown/templates/github_document/resources/default.md 
    ## /Applications/RStudio.app/Contents/MacOS/pandoc/pandoc +RTS -K512m -RTS Wednesday.md --to html4 --from gfm --output Wednesday.html --standalone --self-contained --highlight-style pygments --template /Library/Frameworks/R.framework/Versions/4.0/Resources/library/rmarkdown/rmarkdown/templates/github_document/resources/preview.html --variable 'github-markdown-css:/Library/Frameworks/R.framework/Versions/4.0/Resources/library/rmarkdown/rmarkdown/templates/github_document/resources/github.css' --email-obfuscation none --metadata pagetitle=PREVIEW

    ## 
    ## Preview created: /var/folders/h7/s_z1dr090z5376xv535833rh0000gn/T//Rtmp3IDzZB/preview-b7a823796044.html

    ## 
    ## Output created: Wednesday.md

    ## 
    ## 
    ## processing file: Project_2.Rmd

    ##   |                                                                              |                                                                      |   0%  |                                                                              |.....                                                                 |   7%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.........                                                             |  13%
    ## label: setup (with options) 
    ## List of 1
    ##  $ include: logi FALSE
    ## 
    ##   |                                                                              |..............                                                        |  20%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |...................                                                   |  27%
    ## label: read
    ##   |                                                                              |.......................                                               |  33%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |............................                                          |  40%
    ## label: split
    ##   |                                                                              |.................................                                     |  47%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.....................................                                 |  53%
    ## label: EDA

    ##   |                                                                              |..........................................                            |  60%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |...............................................                       |  67%
    ## label: trimvars
    ##   |                                                                              |...................................................                   |  73%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |........................................................              |  80%
    ## label: fits
    ##   |                                                                              |.............................................................         |  87%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.................................................................     |  93%
    ## label: predict
    ##   |                                                                              |......................................................................| 100%
    ##   ordinary text without R code

    ## output file: Project_2.knit.md

    ## /Applications/RStudio.app/Contents/MacOS/pandoc/pandoc +RTS -K512m -RTS Project_2.utf8.md --to gfm --from markdown+autolink_bare_uris+tex_math_single_backslash --output Thursday.md --standalone --template /Library/Frameworks/R.framework/Versions/4.0/Resources/library/rmarkdown/rmarkdown/templates/github_document/resources/default.md 
    ## /Applications/RStudio.app/Contents/MacOS/pandoc/pandoc +RTS -K512m -RTS Thursday.md --to html4 --from gfm --output Thursday.html --standalone --self-contained --highlight-style pygments --template /Library/Frameworks/R.framework/Versions/4.0/Resources/library/rmarkdown/rmarkdown/templates/github_document/resources/preview.html --variable 'github-markdown-css:/Library/Frameworks/R.framework/Versions/4.0/Resources/library/rmarkdown/rmarkdown/templates/github_document/resources/github.css' --email-obfuscation none --metadata pagetitle=PREVIEW

    ## 
    ## Preview created: /var/folders/h7/s_z1dr090z5376xv535833rh0000gn/T//Rtmp3IDzZB/preview-b7a83b8ee11.html

    ## 
    ## Output created: Thursday.md

    ## 
    ## 
    ## processing file: Project_2.Rmd

    ##   |                                                                              |                                                                      |   0%  |                                                                              |.....                                                                 |   7%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.........                                                             |  13%
    ## label: setup (with options) 
    ## List of 1
    ##  $ include: logi FALSE
    ## 
    ##   |                                                                              |..............                                                        |  20%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |...................                                                   |  27%
    ## label: read
    ##   |                                                                              |.......................                                               |  33%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |............................                                          |  40%
    ## label: split
    ##   |                                                                              |.................................                                     |  47%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.....................................                                 |  53%
    ## label: EDA

    ##   |                                                                              |..........................................                            |  60%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |...............................................                       |  67%
    ## label: trimvars
    ##   |                                                                              |...................................................                   |  73%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |........................................................              |  80%
    ## label: fits
    ##   |                                                                              |.............................................................         |  87%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.................................................................     |  93%
    ## label: predict
    ##   |                                                                              |......................................................................| 100%
    ##   ordinary text without R code

    ## output file: Project_2.knit.md

    ## /Applications/RStudio.app/Contents/MacOS/pandoc/pandoc +RTS -K512m -RTS Project_2.utf8.md --to gfm --from markdown+autolink_bare_uris+tex_math_single_backslash --output Friday.md --standalone --template /Library/Frameworks/R.framework/Versions/4.0/Resources/library/rmarkdown/rmarkdown/templates/github_document/resources/default.md 
    ## /Applications/RStudio.app/Contents/MacOS/pandoc/pandoc +RTS -K512m -RTS Friday.md --to html4 --from gfm --output Friday.html --standalone --self-contained --highlight-style pygments --template /Library/Frameworks/R.framework/Versions/4.0/Resources/library/rmarkdown/rmarkdown/templates/github_document/resources/preview.html --variable 'github-markdown-css:/Library/Frameworks/R.framework/Versions/4.0/Resources/library/rmarkdown/rmarkdown/templates/github_document/resources/github.css' --email-obfuscation none --metadata pagetitle=PREVIEW

    ## 
    ## Preview created: /var/folders/h7/s_z1dr090z5376xv535833rh0000gn/T//Rtmp3IDzZB/preview-b7a85f96b6f5.html

    ## 
    ## Output created: Friday.md

    ## 
    ## 
    ## processing file: Project_2.Rmd

    ##   |                                                                              |                                                                      |   0%  |                                                                              |.....                                                                 |   7%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.........                                                             |  13%
    ## label: setup (with options) 
    ## List of 1
    ##  $ include: logi FALSE
    ## 
    ##   |                                                                              |..............                                                        |  20%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |...................                                                   |  27%
    ## label: read
    ##   |                                                                              |.......................                                               |  33%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |............................                                          |  40%
    ## label: split
    ##   |                                                                              |.................................                                     |  47%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.....................................                                 |  53%
    ## label: EDA

    ##   |                                                                              |..........................................                            |  60%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |...............................................                       |  67%
    ## label: trimvars
    ##   |                                                                              |...................................................                   |  73%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |........................................................              |  80%
    ## label: fits
    ##   |                                                                              |.............................................................         |  87%
    ##   ordinary text without R code
    ## 
    ##   |                                                                              |.................................................................     |  93%
    ## label: predict
    ##   |                                                                              |......................................................................| 100%
    ##   ordinary text without R code

    ## output file: Project_2.knit.md

    ## /Applications/RStudio.app/Contents/MacOS/pandoc/pandoc +RTS -K512m -RTS Project_2.utf8.md --to gfm --from markdown+autolink_bare_uris+tex_math_single_backslash --output Saturday.md --standalone --template /Library/Frameworks/R.framework/Versions/4.0/Resources/library/rmarkdown/rmarkdown/templates/github_document/resources/default.md 
    ## /Applications/RStudio.app/Contents/MacOS/pandoc/pandoc +RTS -K512m -RTS Saturday.md --to html4 --from gfm --output Saturday.html --standalone --self-contained --highlight-style pygments --template /Library/Frameworks/R.framework/Versions/4.0/Resources/library/rmarkdown/rmarkdown/templates/github_document/resources/preview.html --variable 'github-markdown-css:/Library/Frameworks/R.framework/Versions/4.0/Resources/library/rmarkdown/rmarkdown/templates/github_document/resources/github.css' --email-obfuscation none --metadata pagetitle=PREVIEW

    ## 
    ## Preview created: /var/folders/h7/s_z1dr090z5376xv535833rh0000gn/T//Rtmp3IDzZB/preview-b7a85c05721c.html

    ## 
    ## Output created: Saturday.md

    ## [1] "/Users/samanthaashby/Documents/Taylor Docs/NCSU/ST558 (Fall_2020)/Project_2/Monday.md"   
    ## [2] "/Users/samanthaashby/Documents/Taylor Docs/NCSU/ST558 (Fall_2020)/Project_2/Tuesday.md"  
    ## [3] "/Users/samanthaashby/Documents/Taylor Docs/NCSU/ST558 (Fall_2020)/Project_2/Wednesday.md"
    ## [4] "/Users/samanthaashby/Documents/Taylor Docs/NCSU/ST558 (Fall_2020)/Project_2/Thursday.md" 
    ## [5] "/Users/samanthaashby/Documents/Taylor Docs/NCSU/ST558 (Fall_2020)/Project_2/Friday.md"   
    ## [6] "/Users/samanthaashby/Documents/Taylor Docs/NCSU/ST558 (Fall_2020)/Project_2/Saturday.md"

The output files can be found at the links below:

[Monday is available here](Monday.md)

[Tuesday is available here](Tuesday.md)

[Wednesday is available here](Wednesday.md)

[Thursday is available here](Thursday.md)

[Friday is available here](Friday.md)

[Saturday is available here](Saturday.md)
