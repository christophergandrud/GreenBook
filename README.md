# Inflated Expectations

**Christopher Gandrud and Cassandra Grafström**

These files comprise the paper ''[Inflated Expectations:
How government
partisanship shapes bureaucrats' inflation
expectations](http://ssrn.com/abstract=2125283)''.

The paper has been conditionally accepted at [Political Science
Research and Methods]().

## About

Governments' party identifications can indicate the types of economic
policies they are likely to pursue. A common rule of thumb is that
left-party governments are expected to pursue policies for lower
unemployment, but which may cause inflation. Right-party governments
are expected to pursue lower inflation policies. How do these
expectations shape the inflation forecasts of monetary policy
bureaucrats? If there is a mismatch between the policies bureaucrats
*expect* governments to implement and those that they *actually* do,
forecasts will be systematically biased. Using US Federal Reserve
Staff’s forecasts we test for executive partisan biases. We find that
irrespective of actual policy and economic conditions forecasters
systematically overestimate future inflation during left-party presidencies
and underestimate future inflation during right-party ones. Our findings
suggest that partisan heuristics play an important part in monetary
policy bureaucrats' inflation expectations.

## Reproduce the paper (including analyses, plots, and tables)

The paper can be entirely reproduced using the `knitr` package in **R**
(we used `knitr` version 1.5 and **R** version 3.1.0). You will also need
to have a LaTeX
distribution installed.

To reproduce the analyses and plots in the paper first install the necessary
R packages:

```{S}
install.packages("apsrtable", "knitr", "ggplot2", "gridExtra",
                "devtools", "MatchIt", "MCMCpack", "plotrix", "plyr",
                "repmis", "reshape", "reshape2", "stringr", "xtable",
                "Zelig")
```

Download this repository.

Finally, 'knit' the main paper file *main_GreenBook.Rnw* in the *Paper* folder.

**Important:** You will need to change the file paths as discussed in the next section.

## Reproduce only the analyses and plots

The **R** source code for the analyses, tables, and plots are in this
repository's *Analysis* folder. To reproduce the figures and all of the
analyses:

1. Change the working directories and file paths.
You will need to  You will need to do this twice:

    - In `Analysis/Greenbook1.R` set the working directory as appropriate on
    line 10.
    - Manually change the file path on line 169:
    `write.csv(ModelParty.evPer2, '/git_repositories/Greenbook/Paper/cache/SimQrt2.csv')`
    in the file *Analysis/Greenbook6.R*.

2. The source code files must be run in the sequence in which they appear in the
paper. Start with the file called *Greenbook1.R*. This will load the data set
and create the first plot in the paper.

The main data set we used in the paper is in the *Data* folder and is called
*GB_FRED_cpi_2007.csv*.
