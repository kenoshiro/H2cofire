# Hydrogen co-firing analysis by AIM/Technology

## Introduction

- This repository includes source code for data analysis and figure production for the hydrogen co-firing analysis by AIM/Technology.
- Details on the analysis can be found in the following manuscript:  
Oshiro, K., Fujimori, S. (2024). Limited impact of hydrogen co-firing on prolonging fossil-based power generation under low emissions scenarios. *Nature Communications*, 15(1), 1778. https://doi.org/10.1038/s41467-024-46101-5

## How to use

- To run this script, scenario data file `scenario_data.csv` needs to be downloaded and copied to `./data/`. The instruction for the data file download can be found in the Data Availability statement in the paper.
- The IPCC AR6 scenario data also needs to be downloaded from [here](https://data.ene.iiasa.ac.at/ar6/) and copied to `./data/AR6/`.
- Execute `./prog/main.R` on the command line or main console on RStudio. The figures are generated in `./output/`.
- Required R packages: tidyverse (specifically,dplyr,tidyr,readr,stringr,magrittr,ggplot2,readxl), cowplot.
- This program was tested on the following environment.
  - OS: Ubuntu 22.04
  - R: 4.2.3
  - R packages
    - dplyr: 1.1.4, tidyr: 1.3.0, readr: 2.1.4, stringr: 1.5.1, magrittr: 2.0.3, ggplot2: 3.4.4, readxl: 1.4.3, cowplot: 1.1.2
