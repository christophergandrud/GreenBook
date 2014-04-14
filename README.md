## Inflated Expectations: How government partisanship shapes bureaucrats'
inflation expectations

**Christopher Gandrud and Cassandra Grafström**

These files comprise the paper ''Inflated Expectations: How government
partisanship shapes bureaucrats' inflation expectations''

### Reproduce the paper

The paper can be entirely reproduced using the `knitr` package in **R**
(we used `knitr` version 1.5 and **R** version 3.1.0). The `knitr` package
is by [Yihui Xie](http://yihui.name/). You will also need to have a LaTeX
distribution installed.

To reproduce the analyses and plots in the paper first install the necessary
R packages:

```{S}
    install.packages("apsrtable", "knitr", "ggplot2", "gridExtra", "devtools",
                    "MatchIt", "plyr", "repmis", "reshape", "reshape2",
                    "stringr", "xtable", "Zelig")
```

Download this repository.

Finally, 'knit' the main paper file `main_GreenBook.Rnw` in the *Paper* folder.

### Reproduce the analyses, tables, and plots

The **R** source code for the analyses, tables, and plots are in this
repository's *Analysis* folder.

1. The source code files must be run in the sequence in which they appear in the
paper. Start with the file called *Greenbook1.R*. This will load the data set.

2. Set the working directory to the *Greenbook* level. You will also need to
manually change the file path in `write.csv(ModelParty.evPer2, '/git_repositories/Greenbook/Paper/cache/SimQrt2.csv')` in file
*Analysis/Greenbook6.R*.

The main data set we used in the paper is in the *Data* folder and is called
*GB_FRED_cpi_2007.csv*.
