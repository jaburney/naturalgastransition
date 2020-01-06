# --------------------------------------------------------------------------------------------
# Full Replication Code for:
# "The downstream air pollution impacts of the United States coal-to-natural gas transition"
# J. Burney (jburney@ucsd.edu)
# Nature Sustainability, 2019
# doi: XXX
# --------------------------------------------------------------------------------------------

# The unzipped replication file contains XXX
# Assumption is that this file is run from within the main replication directory.
# This file references only down the directory tree and produces output 

# --------------------------------------------------------------------------------------------
# Required packages
# --------------------------------------------------------------------------------------------

# basic data handling
library(foreign)
library(tidyverse)
library(zoo)
library(data.table)

# spatial stuff
library(sp)
library(rgdal)
library(raster)
library(spdep)
library(rgeos)
library(GISTools)
library(rworldmap)
library(Matrix.utils)
library(velox)
library(ncdf4)

# regressions
library(lfe)
library(stargazer)

# plotting
library(RColorBrewer)
library(Cairo)
library(ggpubr)

# library(plyr)

# --------------------------------------------------------------------------------------------
# Level 1 Data Processing
# --------------------------------------------------------------------------------------------
# These files take all data in native formats from their original sources and do a few things:
# a. for rasters - read in, trim to common extent, align
# b. for plant data - read in, concatenate, clean
# c. for county data - read in, clean county names
#
# Note - these can be extremely slow in R on a personal machine, so only re-do if necessary
# --------------------------------------------------------------------------------------------

# The following scripts...
# "L1 Data Processing Code/Global Processing Info.R"
# "L1 Data Processing Code/Process EPA Power Plant Data.R"
# "L1 Data Processing Code/Process MODIS AOD Data.R"
# "L1 Data Processing Code/Process OMI SSA Data.R"
# "L1 Data Processing Code/Process NO2 Data.R"
# "L1 Data Processing Code/Process OMI O3 Data.R"
# "L1 Data Processing Code/Process OMI SO2 Data.R"
# "L1 Data Processing Code/Process PM2p5 Data.R"
# "L1 Data Processing Code/Process EPA AQ Report Data.R"
# "L1 Data Processing Code/Process USDA NASS Crop Data.R"
# "L1 Data Processing Code/Process NASS Area Data.R"
# "L1 Data Processing Code/Process NVSS Mortality Data.R"

# Take input (raw) data and produce the following data files:
# "L1 Processed Data/CropAreas.Rdata"
# "L1 Processed Data/EPA AMPD Cleaned 2005-2016.Rdata"
# "L1 Processed Data/EPA Annual Surface Ozone 0.25deg 2005-2016.Rdata"
# "L1 Processed Data/EPA Annual Surface PM 0.25deg 2005-2016.Rdata"
# "L1 Processed Data/Spatial Info.Rdata"
# "L1 Processed Data/USA County Crop Yields 2005-2016.RData"
# "L1 Processed Data/USA County Mortality 2005-2016.RData"
# "L1 Processed Data/USA MODIS AOD 2005-2016.Rdata"
# "L1 Processed Data/USA NO2 2005-2016.Rdata"
# "L1 Processed Data/USA OMI HCHO 2005-2016.Rdata"
# "L1 Processed Data/USA OMI O3 2005-2016.Rdata"
# "L1 Processed Data/USA OMI SO2 2005-2016.Rdata"
# "L1 Processed Data/USA OMI SSA 2005-2016.Rdata"
# "L1 Processed Data/USA PM2.5 2005-2016.Rdata"

# --------------------------------------------------------------------------------------------
# Level 2 Data Processing
# --------------------------------------------------------------------------------------------
# These files take Level 1 processed files and merge/clean them to generate the final data
# sets used for analysis.
# --------------------------------------------------------------------------------------------

## First - at the plant location

# (1) Create power plant panel, code fuel types and switches, get station locations, etc.
# Output: Data/L2 Processed Data/Cleaned Power Plant and Spatial Data 2005-2016.Rdata
source("Data/L2 Data Processing Code/Clean EPA Power Plant Data.R")

# (2) Extract PM and atmospheric data around power plants (for plant location level analysis)
# Output: Data/L2 Processed Data/Extracted Monthly and Annual Atmospheric Data.Rdata
#         Data/L2 Processed Data/Extracted All Points Atmospheric Data.Rdata
#         Data/L2 Processed Data/Satellite and Surface Rasters.Rdata
source("Data/L2 Data Processing Code/Extract Raster Data.R")

# (3) Merge plant level data with surrounding county-level information
# Output: Data/Final Data Sets for Analysis/FullPanel_AllPoints_Annual.Rdata
#         Data/Final Data Sets for Analysis/FullPanel_Counties_Annual.Rdata
#         Data/Final Data Sets for Analysis/FullPanel_PlantLocations_Annual.Rdata
#         Data/Final Data Sets for Analysis/FullPanel_PlantLocations_Monthly.Rdata
source("Data/L2 Data Processing Code/Reshape and Merge all Data.R")


## Second - at the county level

# 4. Aggregate raster environmental data to county level averages
# Output: Data/L2 Processed Data/County Environmental Data Extracted from Rasters.Rdata
source("Data/L2 Data Processing Code/Aggregate Rasters to Counties.R")

# 5. Calculate and summarize information from plant units within distance bands of each county
# Output: Data/L2 Processed Data/County Data with Plant Neighbors.Rdata
source("Data/L2 Data Processing Code/Generate Neighbors.R")

# 6. Merge neighbor set with county and power plant data
# Output: Data/Final Data Sets for Analysis/FullPanel_CountiesWithPlantNeighbors_Annual.Rdata
source("Data/L2 Data Processing Code/Generate Full County Data.R")

# --------------------------------------------------------------------------------------------
# Impacts Analysis
# --------------------------------------------------------------------------------------------

# (0) Load functions for location-based analysis (no spillovers)
# Also includes a function to get these data later (used in plotting)
source("Analysis/Analysis - Functions.R")

# (1) Location-based analysis (no spillovers) - 
# Runs many specifications (basic panel, tau, FDL) over outcomes with different controls (FEs) 
# and different universes of observations (e.g., only old coal plants, all coal plants, all plants, 
# all counties, all locations)
# Output: Many folders based on specifications/universes, each containing dataset and coarse
# table for later use (varies slightly) [output in Analysis]
# [Takes time! Might want to trim...]
source("Analysis/Analysis - Location no Spillovers.R")

# (2) County-based analysis (including spillovers) -
# Runs reduced form and IVs for county-level impacts, including the influence of plants within
# a given buffer distance. Also includes a small summary location-level analysis for comparison
# to avoid having to deal with the above =). Also produces a ton of tables in Tables directory
# Output: Analysis/County Level Results.Rdata
#         Analysis/Plant Level Results.Rdata
# [Takes some time, but not terrible.]
source("Analysis/Analysis - County with Neighbors.R")

# (3) Do Plots of Chemistry and Human Impacts, along with Total Impact Calculations
# Need to be really sure that the model numbers from above are transferred correctly for impacts
source("Analysis/Plots - Pollution Impacts.R")
source("Analysis/Analysis and Plots - Mortality and Crop Impacts.R")

# (4) Sulfate Mask Analysis - Includes Figures and #s
source("Analysis and Plots - Sulfate Mask.R")

# --------------------------------------------------------------------------------------------
# More Figures and Tables
# --------------------------------------------------------------------------------------------

# More/Main Tables
source("Analysis/Tables - Main Analysis.R")
source("Analysis/Tables - County Analysis.R")

# (b) Fleet Summary Information
# Writes out a few csv files summarizing the AMPD fleet
source("Analysis/Plots - Fleet Plots.R")

# (c) Plot out environmental rasters
source("Analysis/Plots - Satellite Rasters.R")

# (d) Some ancillary stuff:
# Example of what spillovers/neighbors looks like
source("Analysis/Plots - Neighbors and Locations.R")

# Comparison to other studies
source("Analysis/Plots - Comparison to Other Studies.R")
source("Analysis/Plots - Big Results Comparison Plot.R")



