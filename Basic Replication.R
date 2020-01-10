# --------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------
# Basic Replication Code for:
# "The downstream air pollution impacts of the transition from 
#        coal to natural gas in the United States"
# Nature Sustainability, January 2020 
# doi: 10.1038/s41893-019-0453-5
# --------------------------------------------------------------------------------------------
# Jen Burney (jburney@ucsd.edu)
# --------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------
# Note: Full Replication Data/Code is large (>50G) and time consuming to run.
# It starts with daily observations, and then cleans, aggregates, and merges the final 
# data sets for analysis. Here for ease I include the final data sets (and some intermediate
# sets) that can be used to replicate the main findings, and reproduce all of the figures
# and tables in the paper. This does not include some smaller illustrative flourishes that
# were produced elsewhere (e.g., Supplementary Figure 1). Please see the Readme file for 
# information on obtaining the underlying data in native form and processing code to create 
# the intermediate and final data sets used here (all publicly available).
# --------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------

# This file should be located in and run from within the folder also houses Scripts/ and Data/
# This script only references down the tree of included files and should be self-contained.

# Create directory structure. Reminder, downloaded data needs to be unzipped in the the 
# "Basic Replication/" folder.
dir.create("Analysis/")
dir.create("Plots/")
dir.create("Tables/")

# --------------------------------------------------------------------------------------------
# Required Packages
# --------------------------------------------------------------------------------------------

# Basic Data Handling
library(tidyverse)

# Spatial Analysis
library(sp)
library(rgdal)
library(raster)
library(spdep)
library(rgeos)
library(GISTools)
library(rworldmap)

# Regressions
library(lfe)
library(stargazer)

# Plotting
library(RColorBrewer)
library(Cairo)
library(ggpubr)

# --------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------
# Load Compiled Data Sets
# --------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------

# Main Files
# ------------------------------------------------
load(file="Data/Cleaned Power Plant and Spatial Data 2005-2016.RData")
load(file="Data/FullPanel_PlantLocations_Annual.Rdata")
load(file="Data/FullPanel_CountiesWithPlantNeighbors_Annual.Rdata")
load(file="Data/RF Calculations Data.Rdata")

# Files used in Robustness checks
# ------------------------------------------------
load(file="Data/FullPanel_AllPoints_Annual.Rdata")
load(file="Data/FullPanel_Counties_Annual.Rdata")

# Underlying Rasters 
# ------------------------------------------------
load(file="Data/Satellite and Surface Rasters.Rdata")
load(file="Data/CropAreas.Rdata")

# Borders for mapping
# ------------------------------------------------
data(countriesLow)
  usaborders=countriesLow[countriesLow$NAME=="United States" & !is.na(countriesLow$NAME),]
  usaborders=spTransform(usaborders,crs(states48))
  usaborders=crop(usaborders,extent(states48))
  
# --------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------
# Main Analysis
# --------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------

# Read notes below and toggle this depending on what you need
do.county.level = FALSE
do.plant.level = FALSE
  
# Conducts main county-level analyses and some of the plant location-level analyises
# (this may allow you to skip the more time-consuming step below)
# Takes ~30 minutes to run on a nothing-special laptop
# Outputs "County Level Results.Rdata" and "Plant Level Results.Rdata) to Analysis/
# Outputs various model results to Tables/ for examination.
# ------------------------------------------------
if (do.county.level == TRUE) {
  source("Scripts/Main Analysis - County.R")
}

# Conduct analysis (many specifications) at plant location level - warning: takes a while (~hr)!
# These are mainly robustness to what is included above, and may be optional depending on your goals.
# Unless you want to directly reproduce some of the figures below (see next section), you do not
# need to run this and can simply run the county-level script above.
# ------------------------------------------------
# Creates a bunch of directories into Analysis/ with results from different specifications
# Loops over experiments, models, and outcomes for both reduced form binary (on/off) 
# and intensity, as well as IV specifications for PM. Numbers show outcome for each expt/model
# as the analysis proceeds; you will see some specs/models do not have sufficient data.
# (Typically outcomes like Cardio-Respiratory deaths for a given age group, or SSA observations
# which have some missing data; not used here.) Table with main coeff results saved and
# printed with results from each model for all outcomes; nice tables for paper, however, 
# are built and output from a separate script, admittedly redundant, but if you just want to
# reproduce tables, you do not have to run this whole thing -- see below. You can also edit
# to only run certain specifications, subsets, outcomes, etc.
# Saves results within created directories that are accessed elsewhere to make figures.
# ------------------------------------------------
if (do.plant.level == TRUE) {
  source("Scripts/Analysis Functions.R")
  source("Scripts/Main Analysis - Plant Location.R")
}

# --------------------------------------------------------------------------------------------
# Figures, Tables, Outputs, Basic Numbers
# --------------------------------------------------------------------------------------------

  # This section is divided in three, for ease for those who only want to replicate or access
  # parts of the data/analysis. The first part contains scripts that do NOT require the above 
  # "Main Analysis - County" or "Main Analysis - Plant Location" scripts to be run beforehand.
  # The second section requires outputs from the "Main Analysis - County" script, and the
  # third section requires outputs from the "Main Analysis - Plant Location" script.


## (a) This section DOES NOT require any of the analysis outputs from above ##

# Summarize Electric Power Plant Information
#   Figs 1, ED1, ED2, and Supp Fig 2 in Plots/
#   3 Fleet summaries as .csv files in Analysis/
# ------------------------------------------------
source("Scripts/Fleet Summary.R")

# Environmental Data Plots (Rasters)
#   Fig ED3 and Supp Fig 3 in Plots/
# ------------------------------------------------
source("Scripts/Environmental Rasters.R")

# Examples to Explain Analysis Framework
#   Fig ED6 and Supp Fig 4 in Plots/
# ------------------------------------------------
source("Scripts/Neighbors and Examples.R")

# Radiative Forcing Calculations
#   Fig 4 and Supp Fig 5 in Plots/
#   Table RFRegs in Tables/
# ------------------------------------------------
source("Scripts/Radiative Forcing.R")
  
# Tables for Plant-level analyses
#   Tables S1 - S6 in Tables/
#   (Note some of these combined in final publication)
# ------------------------------------------------
source("Scripts/Tables - Plant Location.R")


## (b) This section ONLY requires outputs from the "Main Analysis - County" script above ##

# Mortality and Crop Impacts
#   Figs 3, ED7, and ED9 in Plots/
#   # Note, these are named by what they represent to avoid confusion.
#   County and Total impacts summary tables as .csv files in Analysis/
# ------------------------------------------------
source("Scripts/Mortality and Crop Impacts.R")

# Comparison to Other Studies
#    Fig ED10 in Plots/
# ------------------------------------------------
source("Scripts/Mortality Comparison to Other Studies.R")

# Model Comparison Figure 
#    Fig ED8 -- creates FigED8/ folder in Plots/
#    Leaves individual panel names as variable names for clarity
# ------------------------------------------------
source("Scripts/Model Comparison Plot.R")

# Tables for County-level analyses
#   Tables S1 - S6 in Tables/
#   (Note some of these combined in final publication)
# ------------------------------------------------
source("Scripts/Tables - County.R")

  
## (c) This section ALSO requires outputs from "Main Analysis - Plant Location" script above ##

# Pollution Impacts Analysis and Plots
#   Figs 2, ED4, ED5 in Plots/
# If you have just run the above analysis scripts, data are in your workspace and
# you do not need to re-load the results data. If you ran these prior and have since
# emptied workspace, you need to load up results -- toggle this to true.
# ------------------------------------------------
load.pollution.data = FALSE
source("Scripts/Plot Pollution Impacts.R")
