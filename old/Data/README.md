# Data Description

#### Christopher Gandrud and Cassandra Grafstrom

The data set *GB_FRED_cpi_2007.csv* was used for all analyses in the paper ''Inflated Expectations: How
government partisanship shapes monetary policy bureaucrats’ inflation forecasts''.

The table below gives a brief description of each variable and its source.
Please see the paper's text for detailed descriptions of transformations
that were made to the variables.

The data set was amalgamated and variable transformations were made using source code available at: <https://github.com/christophergandrud/GreenBook/tree/master/Data/DataCreation>.

All data is for the United States.

## Variable descriptions and sources

### Notes

- ''Authors'' means that the study's authors created the variable using generally available information.

- ''FRED'' refers to the St. Louis Federal Reserve's FRED statistical service. See <http://research.stlouisfed.org/fred2/>. Most recently accessed May 2014.

- ''infoplease'' data is available at: <http://www.infoplease.com/ipa/A0774721.html>. Most recently accessed May 2014.

|  Variable ID      | Description                | Source |
| ----------------- | -------------------------- | ------ |
| `Quarter`           | Quarter with `.1` indicating the first quarter. | Authors |
| `year`              | Year                       | Authors |
| `GP_CPI_QTR*`       | Greenbook inflation forecast. Suffix indicates how many quarters in advance it was made. `0` indicates forecasts made in the present quarter. | US Federal Reserve |
| `deflator`          | Quarterly % change in the GNP deflator | FRED |
| `cpi.change`        | Quarterly % change in the consumer price index | FRED |
| `president`         | Surname of the sitting president. | Authors |
| `term` | President's term number | Authors |
| `presTerm` | Combination of `president` and `term` | Authors |
| `EligibleReElect` | Dummy indicating if the president is eligible for re-election (`1`) or not (`0`). | Authors |
| `pres_party` | Dummy indicating the president's party. `0` is Republican. `1` is Democrat. | Authors |
| `pres_election` | Dummy indicating if the quarter has a presidential election (`1`) or not (`0`). | Authors |
| `time_to_election` | Quarters until the next presidential election. `0` is an election quarter. | Authors |
| `ElectionPeriod` | Dummy indicating (`1`) whether or not (`0`) the quarter is in the election period defined as the election quarter and the quarter before. See also `ElectionPeriod4`. | Authors |
| `senate_dems` | Number of Senate Democrats. | infoplease |
| `senate_rep`  | Number of Senate Republicans, | infoplease |
| `house_dems` | Number of House Democrats. | infoplease |
| `house_rep`  | Number of House Republicans. | infoplease |
| `senate_dem_rep` | Senate Democrats as a percentage of Senate Republicans. | Authors' calculations from infoplease data |
| `house_dem_rep` | House Democrats as a percentage of House Republicans. | Authors' calculations from infoplease data |
| `manufoutput` |  Manufacturing output % change from same quarter in previous year. | FRED |
| `recession` | US recession dummy (`1` is a recession). | FRED |
| `sales` | US real retail & food sales % change from quarter in previous year. | FRED |
| `DebtGDP` | Quarterly US Federal government debt as a % of GDP. | Authors' calculations based on FRED data |
| `ExpenditureGDP` | Quarterly US Federal government expenditures as a $ of GDP. | Authors' calculations based on FRED data |
| `PotentialGDP` | Potential GDP as a % of GDP. | Authors' calculations based on FRED data |
| `UNRATE` | Unemployment rate. | FRED |
|`GB_Unemp*` | Greenbook unemployment rate forecast. Suffix indicates how many quarters in advance it was made. For example,`0` indicates forecasts made in the present quarter. | US Federal Reserve |
| `FedFunds` | Federal funds rate. | FRED |
| `FedFunds2qChange` | Federal funds rate change between quarter 0 and quarter 2. | Authors' calculations based on FRED data |
| `DiscountRate` | Discount rate. | Compiled from FRED data by Authors |
| `DiscountTrate*` | Change in the discount rate between quarter 0 and some other quarter specified in the suffix. For example, the suffix `1qchange` indicates a change from the previous quarter. | Authors' calculations based on FRED data |
| `DeficitGDP` | Annual US Federal government deficit as a % of GDP. | FRED |
| `pres_party_name` | President's party name. | Authors |
| `error.prop.deflator.*` | Inflation forecasting error. The suffix indicates when the forecast was made. For example, `q0` means that the forecast was made in the quarter being forecasted for. | Author's calculations based on US Federal Reserve and FRED data |
| `GlobalModel` | Dummy indicating that the quarter was from or after 1996 (`1`) when the Global Forecasting model was introduced or before (`0`). | Authors |
| `Chair` | Surname of the Federal Reserve Board of Governors chairperson. | Authors |
| `ElectionPeriod4` | Dummy indicating that the quarter was in the presidential election quarter or the three quarters before (`1`) or otherwise (`0`). | Authors |
| `productivity_change` | Change % of US non-farm labour productivity from the previous quarter at the annual rate. | United States Bureau of Labor Statistics |
| `WTI_crude_price` | Change in the West Texas Crude oil price from the same quarter in the previous year. | FRED |
| `num_conflicts` | Number of armed conflicts per year which the United States participated. | Uppsala Conflict Data Program/Peach Research Institute Oslo |
| `DemAppointPerc` | Percentage of Federal Reserve Board of Governors members appointed by a democratic president. | Authors |
| `DemAppontPerc_Lag3` | `DemAppointPerc` lagged by 3 quarters. | Authors |
