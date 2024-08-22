# Long Run: Patents and Exhibitions
library(fixest); library(tidyverse); library(did)
## Estimator: Staggered DiD - Use the Borusyak-Jaravel estimator or Callaway-Sant'Anna
## DV: Log(PatentCount_{Italy, Austria}), 
## Part 1: Patents: {1855-1867-1878-1890-1902-1911}
## Robustness: Extensive vs. Intensive Margin; Unit of Measurement


## Part 2: Exhibitions: {1855-1867-1878-1889-1900-1911}
## DV: Log(ExhibitionCount), AvgComplexity, TopComplexity
## Weights: Stuff by Population
## Robustness: Extensive vs. Intensive Margin; Unit of Measurement

