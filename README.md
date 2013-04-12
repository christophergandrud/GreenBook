# Does Partisanship Affect Fed Inflation Forecasts?

## Cassandra Grafström and Christopher Gandrud

---

These files comprise the working paper ''Does Partisanship Affect Fed Inflation Forecasts?''

### Reproduce the paper

The paper can be entirely reproduced using the `knitr` package in **R** (we used `knitr` version 1.2 and **R** version 3.0.0). The `knitr` package is by [Yihui Xie](http://yihui.name/). 

To reproduce the paper first install the necessary R packages:

```{r}
    install.packages("apsrtable",
                    "knitr", 
                    "ggplot2", 
                    "gridExtra",
                    "devtools", 
                    "MatchIt", 
                    "plyr",
                    "reshape", 
                    "reshape2", 
                    "RCurl",
                    "stats", 
                    "stringr",
                    "xtable", 
                    "Zelig"
                    )
```
              
Then download the *Paper* folder from this repository.

Finally, 'knit' the main paper file `main_GreenBook.Rnw` in this folder.

### Reproduce the analyses, tables and empirical graphs

The **R** source code for the analyses, tables, and empirical graphs are in this repository's *Analysis* folder. 

> Note: these source code files must be run in the sequence in which they appear in the paper.

