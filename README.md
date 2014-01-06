# Inflated Expectations: How government partisanship shapes bureaucrat's inflation expectations

## Cassandra Grafström and Christopher Gandrud

---

These files comprise the working paper ''Inflated Expectations: How government partisanship shapes bureaucrat's inflation expectations''

### Reproduce the paper

The paper can be entirely reproduced using the `knitr` package in **R** (we used `knitr` version 1.5 and **R** version 3.0.2). The `knitr` package is by [Yihui Xie](http://yihui.name/). 

To reproduce the paper first install the necessary R packages:

```{r}
    install.packages("apsrtable",
                    "knitr", 
                    "ggplot2", 
                    "gridExtra",
                    "devtools", 
                    "MatchIt", 
                    "plyr",
                    "repmis",
                    "reshape", 
                    "reshape2", 
                    "RCurl",
                    "stats", 
                    "stringr",
                    "xtable", 
                    "Zelig"
                    )
```
              
Download this repository

Finally, 'knit' the main paper file `main_GreenBook.Rnw` in the *Paper* folder.

### Reproduce the analyses, tables and empirical graphs

The **R** source code for the analyses, tables, and empirical graphs are in this repository's *Analysis* folder. 

> Note: these source code files must be run in the sequence in which they appear in the paper.

