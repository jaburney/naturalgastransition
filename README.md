## Coal-to-Natural Gas Transition Impacts Analysis

This repository contains code to replicate analysis in the paper: "The downstream air pollution impacts of the transition from coal to natural gas in the United States," J. Burney, *Nature Sustainability*, January 2020 (https://doi.org/10.1038/s41893-019-0453-5). Because the underlying data are large and assembly of final data sets is slow, this replication starts from the compiled final data sets and reproduces analysis, tables, figures, and other summary output files from the main manuscript, Extended Data, and Supplementary Information.

### Setup & Organization

Due to size considerations :thinking: data for replication are hosted elsewhere. Please see the accompanying website for the paper (www.jaburney.net/coal-to-natural-gas-transition) to download a zipped file of these final data sets. (This file is almost identical to data published along with the paper ([https://doi.org/10.7910/DVN/RIZQUN](here)), but has a few additional intermediate data products included for some descriptive figures.) Unzip the data file within the main repository directory.

This repository should then have the following structure:
* **Scripts/** contains 13 individual scripts to load data, conduct analysis, and output the figures and tables in the main manuscript, extended data, and supplementary information
* **Data/** contains 8 .Rdata files
* **Basic Replication.R** calls all of the relevant scripts to load data, conduct analysis, and produce figures & tables. I strongly suggest reading through it first and running piecemeal, as opposed to in one go: a couple of the analyis files take a long time to run and may not be necessary, depending on your objectives. The script begins by creating **Analysis/** **Plots/** and **Tables/** to hold output.

### Requirements

Code was written in R version 3.6.1, and requires a number of additional packages (latest versions can be installed with the following):

```R
install.packages(c('tidyverse','sp','rgdal','raster','spdep','rgeos','GISTools','rworldmap','lfe','stargazer','RColorBrewer','ggpubr'), dependencies = T)
``` 

### Contact
Jen Burney, jburney@ucsd.edu
More information about this project can be found at (www.jaburney.net/coal-to-natural-gas-transition)
