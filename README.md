# Coal-to-Natural Gas Transition Impacts Analysis

This repository contains code to replicate analysis in the paper: "The downstream air pollution impacts of the transition from coal to natural gas in the United States" Nature Sustainability, January 2020 (https://doi.org/10.1038/s41893-019-0453-5). Because the underlying data are large and assembly of final data sets is slow, this replication starts from the compiled final data sets and reproduces analysis, tables, figures, and other summary output files.

* Organization of Repository

Please see <href>www.jaburney.net/coal-to-natural-gas-transition</href> to download a zipped file of these final data sets. (This file is almost identical to data published along with the paper (https://doi.org/10.7910/DVN/RIZQUN), but has a few additional intermediate data products included for plots included in the paper and Supplementary Information.) Unzip the data file within the main directory (Basic Replication/). Within the main director (Basic Replication/) You should have a folder Scripts/ that contains all of the code called by the main replication file (see below), and the Data/ folder that contains the necessary datasets.

The script <b>Basic Replication.R</b> calls all of the relevant scripts to load data, conduct analysis, and produce figures & tables. I strongly suggest reading through it first and running piecemeal, as opposed to in one go: a couple of the analyis files take a long time to run and may not be necessary, depending on your objectives. 

