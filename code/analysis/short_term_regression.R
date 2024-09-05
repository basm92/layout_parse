# Patents 1855-1866 
library(fixest); library(tidyverse)
## Event-study: unit of analysis: Municipality-year
## DV: {Sum_patents, Patents_Piedmonte, Patents_Austria}
## Treatment: Lombardia
## Robustness: Bandwidth around the border (DiD-RDD)