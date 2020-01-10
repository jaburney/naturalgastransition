## Coal-to-Natural Gas Transition Impacts Analysis

This repository contains code to replicate analysis in the paper: "The downstream air pollution impacts of the transition from coal to natural gas in the United States," J. Burney, *Nature Sustainability*, January 2020 (https://doi.org/10.1038/s41893-019-0453-5). Because the underlying data are large and assembly of final data sets is slow, this replication starts from the compiled final data sets and reproduces analysis, tables, figures, and other summary output files from the main manuscript, Extended Data, and Supplementary Information.

### Setup & Organization

Due to size considerations :thinking: data for replication are hosted elsewhere. Please see the accompanying website for the paper (www.jaburney.net/coal-to-natural-gas-transition) to download a zipped file of these final data sets. (This file is almost identical to data published along with the paper ([https://doi.org/10.7910/DVN/RIZQUN](here)), but has a few additional intermediate data products included for some descriptive figures, and is organized for replication.) Unzip the data file within the main repository directory.

This repository should then have the following structure:
* **Scripts/** contains 13 individual scripts to load data, conduct analysis, and output the figures and tables in the main manuscript, extended data, and supplementary information:

   Analysis Functions.R  
   Environmental Rasters.R  
   Fleet Summary.R  
   Main Analysis - County.R  
   Main Analysis - Plant Location.R  
   Model Comparison Plot.R  
   Mortality and Crop Impacts.R  
   Mortality Comparison to Other Studies.R  
   Neighbors and Examples.R  
   Plot Pollution Impacts.R  
   Radiative Forcing.R  
   Tables - County.R  
   Tables - Plant Location.R  
   
* **Data/** contains 8 .Rdata files

   Cleaned Power Plant and Spatial Data 2005-2016.RData  
   CropAreas.Rdata  
   FullPanel_AllPoints_Annual.Rdata  
   FullPanel_Counties_Annual.Rdata  
   FullPanel_CountiesWithPlantNeighbors_Annual.Rdata  
   FullPanel_PlantLocations_Annual.Rdata  
   RF Calculations Data.Rdata  
   Satellite and Surface Rasters.Rdata  

* **Basic Replication.R** calls all of the relevant scripts to load data, conduct analysis, and produce figures & tables. I strongly suggest reading through it first and running piecemeal, as opposed to in one go: a couple of the analyis files take a long time to run and may not be necessary, depending on your objectives. The script begins by creating **Analysis/** **Plots/** and **Tables/** to hold output.

### Requirements

Code was written in R version 3.6.1, and requires a number of additional packages (latest versions can be installed with the following):

```R
install.packages(c('tidyverse','sp','rgdal','raster','spdep','rgeos','GISTools','rworldmap','lfe','stargazer','RColorBrewer','Cairo','ggpubr'), dependencies = T)
``` 

### Contact
Jen Burney, jburney@ucsd.edu

More information about this project can be found at www.jaburney.net/coal-to-natural-gas-transition
