# Inflated Expectations

**Christopher Gandrud and Cassandra Grafström**

[![DOI](https://zenodo.org/badge/5350/christophergandrud/GreenBook.png)](http://dx.doi.org/10.5281/zenodo.11320)

These files comprise the paper ''[Inflated Expectations:
How government
partisanship shapes bureaucrats' inflation
expectations](http://ssrn.com/abstract=2125283)''.

The paper is forthcoming at [Political Science
Research and Methods](http://journals.cambridge.org/action/displayJournal?jid=RAM).

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

The **R** and knitr source code and data for reproducing the analyses, plots, tables
and text is in this DVN repository. We used R version 3.1.1.

To reproduce the figures and all of the analyses:

1. Install the necessary R packages using the following code:

```{S}
install.packages(c("apsrtable", "ggplot2", "gridExtra", "DataCombine",
                "devtools", "knitr", "Matching", "MatchIt", "MCMCpack",
                "plotrix", "plyr", "repmis", "reshape", "reshape2", 
                "rgenoud", "stringr", "xtable", "Zelig"))
```

2. Knit the *main_GreenBook.Rnw* and *supplemental_GreenBook.Rnw* files in the
*PSRM_dvn* folder. This reproduces all of the analyses, tables, figures, and
text for both the Main Paper and Supplementary Materials:

```{S}
# Remember to set the working directory to the PSRM_dvn folder.

library(knitr)

knit2pdf('main_GreenBook.Rnw')
knit2pdf('supplemental_GreenBook.Rnw')
```

## Data

The data set we used in the paper is called
*PSRM_dvn/Data/GB_FRED_cpi_2007.csv*. The *README_data.html* file contains
variable descriptions.
