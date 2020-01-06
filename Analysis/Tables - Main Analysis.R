# -------------------------------------------------------------------------------------------------------------------
# Impacts Tables
# -------------------------------------------------------------------------------------------------------------------

load("Data/Final Data Sets for Analysis/FullPanel_PlantLocations_Annual.Rdata")
load("Data/Final Data Sets for Analysis/FullPanel_Counties_Annual.Rdata")
load("Data/Final Data Sets for Analysis/FullPanel_AllPoints_Annual.Rdata")
load("Data/Final Data Sets for Analysis/FullPanel_CountiesWithPlantNeighbors_Annual.Rdata")

setwd("Tables/")

# -----------------------------------------------------------------
# Data subsets for plant-level analysis
# -----------------------------------------------------------------

df = fullpanel.plants.annual
df_oldcoal = subset(fullpanel.plants.annual,fullpanel.plants.annual$oldcoalplant==1,select=c(uniton,grossload,facunitid,locationid,County,year,pm.0km,omi.o3.0km,so2.0km,no2.0km,modis.aod.0km,ssa.0km,epa.pm.0km,epa.o3.0km))
df_newcoal = subset(fullpanel.plants.annual,fullpanel.plants.annual$newcoalplant==1,select=c(uniton,grossload,facunitid,locationid,County,year,pm.0km,omi.o3.0km,so2.0km,no2.0km,modis.aod.0km,ssa.0km,epa.pm.0km,epa.o3.0km))
df_allcoal = subset(fullpanel.plants.annual,fullpanel.plants.annual$fueltype=="Coal",select=c(uniton,grossload,facunitid,locationid,County,year,pm.0km,omi.o3.0km,so2.0km,no2.0km,modis.aod.0km,ssa.0km,epa.pm.0km,epa.o3.0km))
df_newng = subset(fullpanel.plants.annual,fullpanel.plants.annual$newngplant==1,select=c(uniton,grossload,facunitid,locationid,County,year,pm.0km,omi.o3.0km,so2.0km,no2.0km,modis.aod.0km,ssa.0km,epa.pm.0km,epa.o3.0km))
df_allng = subset(fullpanel.plants.annual,fullpanel.plants.annual$fueltype=="Natural Gas",select=c(uniton,grossload,facunitid,locationid,County,year,pm.0km,omi.o3.0km,so2.0km,no2.0km,modis.aod.0km,ssa.0km,epa.pm.0km,epa.o3.0km))
df_ap = subset(fullpanel.allpoints.annual.0p25,select=c(uniton,grossload,facunitid,locationid,County,year,pm.0km,omi.o3.0km,so2.0km,locationid2,fueltype,epa.pm.0km,epa.o3.0km))
df_ap2 = subset(fullpanel.allpoints.annual.0p125,select=c(uniton,grossload,facunitid,locationid,County,year,no2.0km,locationid2,fueltype))

# -----------------------------------------------------------------
# Tables of Coal Impacts on different parameters, RD, unit FE
# -----------------------------------------------------------------

mod.pm.oc.fy = felm(pm.0km ~ uniton | facunitid + year | 0 | facunitid + year,data=df_oldcoal)
mod.pm.oc.f = felm(pm.0km ~ uniton | facunitid + year | 0 | facunitid,data=df_oldcoal)
mod.pm.nc.fy = felm(pm.0km ~ uniton | facunitid + year | 0 | facunitid + year,data=df_newcoal)
mod.pm.nc.f = felm(pm.0km ~ uniton | facunitid + year | 0 | facunitid,data=df_newcoal)
mod.pm.ac.fy = felm(pm.0km ~ uniton | facunitid + year | 0 | facunitid + year,data=df_allcoal)
mod.pm.ac.f = felm(pm.0km ~ uniton | facunitid + year | 0 | facunitid,data=df_allcoal)
mod.pm.a.fy = felm(pm.0km ~ uniton:fueltype | facunitid + year | 0 | facunitid + year,data=df)
mod.pm.a.f = felm(pm.0km ~ uniton:fueltype | facunitid + year | 0 | facunitid,data=df)
mod.pm.ap.ly = felm(pm.0km ~ uniton:fueltype | locationid2 + year | 0 | locationid2 + year,data=df_ap)
mod.pm.ap.l = felm(pm.0km ~ uniton:fueltype | locationid2 + year | 0 | locationid2,data=df_ap)

stargazer(mod.pm.oc.fy,mod.pm.oc.f,mod.pm.nc.fy,mod.pm.nc.f,mod.pm.ac.fy,mod.pm.ac.f,mod.pm.a.fy,mod.pm.a.f,mod.pm.ap.ly,mod.pm.ap.l,
          out="Coal_PM_subsets_table.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
          out.header=T,dep.var.caption="Particulate Matter Concentration [ug/m3]",dep.var.labels="",
          covariate.labels=c("Unit On","Unit On * Coal","Unit On * Natural Gas","Unit On * Other"),
          column.labels=c("Old Coal Units","New Coal Units","All Coal Units","All Units","All Locations (0.25x0.25)"),column.separate=c(2,2,2,2,2),
          notes=c("Odd-numbered models include standard errors clustered by location and year;","Even-numbered models have standard errors clustered by location.","For models 1-8, location is the unit, for models 9-10, location is grid cell."),
          notes.align="l"
          )

mod.pm.oc.fy = felm(epa.pm.0km ~ uniton | facunitid + year | 0 | facunitid + year,data=df_oldcoal)
mod.pm.oc.f = felm(epa.pm.0km ~ uniton | facunitid + year | 0 | facunitid,data=df_oldcoal)
mod.pm.nc.fy = felm(epa.pm.0km ~ uniton | facunitid + year | 0 | facunitid + year,data=df_newcoal)
mod.pm.nc.f = felm(epa.pm.0km ~ uniton | facunitid + year | 0 | facunitid,data=df_newcoal)
mod.pm.ac.fy = felm(epa.pm.0km ~ uniton | facunitid + year | 0 | facunitid + year,data=df_allcoal)
mod.pm.ac.f = felm(epa.pm.0km ~ uniton | facunitid + year | 0 | facunitid,data=df_allcoal)
mod.pm.a.fy = felm(epa.pm.0km ~ uniton:fueltype | facunitid + year | 0 | facunitid + year,data=df)
mod.pm.a.f = felm(epa.pm.0km ~ uniton:fueltype | facunitid + year | 0 | facunitid,data=df)
mod.pm.ap.ly = felm(epa.pm.0km ~ uniton:fueltype | locationid2 + year | 0 | locationid2 + year,data=df_ap)
mod.pm.ap.l = felm(epa.pm.0km ~ uniton:fueltype | locationid2 + year | 0 | locationid2,data=df_ap)

stargazer(mod.pm.oc.fy,mod.pm.oc.f,mod.pm.nc.fy,mod.pm.nc.f,mod.pm.ac.fy,mod.pm.ac.f,mod.pm.a.fy,mod.pm.a.f,mod.pm.ap.ly,mod.pm.ap.l,
          out="Coal_EPA_PM_subsets_table.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
          out.header=T,dep.var.caption="Particulate Matter Concentration [ug/m3]",dep.var.labels="",
          covariate.labels=c("Unit On","Unit On * Coal","Unit On * Natural Gas","Unit On * Other"),
          column.labels=c("Old Coal Units","New Coal Units","All Coal Units","All Units","All Locations (0.25x0.25)"),column.separate=c(2,2,2,2,2),
          notes=c("Odd-numbered models include standard errors clustered by location and year;","Even-numbered models have standard errors clustered by location.","For models 1-8, location is the unit, for models 9-10, location is grid cell."),
          notes.align="l"
)

mod.o3.oc.fy = felm(omi.o3.0km ~ uniton | facunitid + year | 0 | facunitid + year,data=df_oldcoal)
mod.o3.oc.f = felm(omi.o3.0km ~ uniton | facunitid + year | 0 | facunitid,data=df_oldcoal)
mod.o3.nc.fy = felm(omi.o3.0km ~ uniton | facunitid + year | 0 | facunitid + year,data=df_newcoal)
mod.o3.nc.f = felm(omi.o3.0km ~ uniton | facunitid + year | 0 | facunitid,data=df_newcoal)
mod.o3.ac.fy = felm(omi.o3.0km ~ uniton | facunitid + year | 0 | facunitid + year,data=df_allcoal)
mod.o3.ac.f = felm(omi.o3.0km ~ uniton | facunitid + year | 0 | facunitid,data=df_allcoal)
mod.o3.a.fy = felm(omi.o3.0km ~ uniton:fueltype | facunitid + year | 0 | facunitid + year,data=df)
mod.o3.a.f = felm(omi.o3.0km ~ uniton:fueltype | facunitid + year | 0 | facunitid,data=df)
mod.o3.ap.ly = felm(omi.o3.0km ~ uniton:fueltype | locationid2 + year | 0 | locationid2 + year,data=df_ap)
mod.o3.ap.l = felm(omi.o3.0km ~ uniton:fueltype | locationid2 + year | 0 | locationid2,data=df_ap)

stargazer(mod.o3.oc.fy,mod.o3.oc.f,mod.o3.nc.fy,mod.o3.nc.f,mod.o3.ac.fy,mod.o3.ac.f,mod.o3.a.fy,mod.o3.a.f,mod.o3.ap.ly,mod.o3.ap.l,
          out="Coal_O3_subsets_table.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
          out.header=T,dep.var.caption="Ozone Thickness [DU]",dep.var.labels="",
          covariate.labels=c("Unit On","Unit On * Coal","Unit On * Natural Gas","Unit On * Other"),
          column.labels=c("Old Coal Units","New Coal Units","All Coal Units","All Units","All Locations (0.25x0.25)"),column.separate=c(2,2,2,2,2),
          notes=c("Odd-numbered models include standard errors clustered by location and year;","Even-numbered models have standard errors clustered by location.","For models 1-8, location is the unit, for models 9-10, location is grid cell."),
          notes.align="l"
)

mod.o3.oc.fy = felm(epa.o3.0km ~ uniton | facunitid + year | 0 | facunitid + year,data=df_oldcoal)
mod.o3.oc.f = felm(epa.o3.0km ~ uniton | facunitid + year | 0 | facunitid,data=df_oldcoal)
mod.o3.nc.fy = felm(epa.o3.0km ~ uniton | facunitid + year | 0 | facunitid + year,data=df_newcoal)
mod.o3.nc.f = felm(epa.o3.0km ~ uniton | facunitid + year | 0 | facunitid,data=df_newcoal)
mod.o3.ac.fy = felm(epa.o3.0km ~ uniton | facunitid + year | 0 | facunitid + year,data=df_allcoal)
mod.o3.ac.f = felm(epa.o3.0km ~ uniton | facunitid + year | 0 | facunitid,data=df_allcoal)
mod.o3.a.fy = felm(epa.o3.0km ~ uniton:fueltype | facunitid + year | 0 | facunitid + year,data=df)
mod.o3.a.f = felm(epa.o3.0km ~ uniton:fueltype | facunitid + year | 0 | facunitid,data=df)
mod.o3.ap.ly = felm(epa.o3.0km ~ uniton:fueltype | locationid2 + year | 0 | locationid2 + year,data=df_ap)
mod.o3.ap.l = felm(epa.o3.0km ~ uniton:fueltype | locationid2 + year | 0 | locationid2,data=df_ap)

stargazer(mod.o3.oc.fy,mod.o3.oc.f,mod.o3.nc.fy,mod.o3.nc.f,mod.o3.ac.fy,mod.o3.ac.f,mod.o3.a.fy,mod.o3.a.f,mod.o3.ap.ly,mod.o3.ap.l,
          out="Coal_EPA_O3_subsets_table.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
          out.header=T,dep.var.caption="Ozone Thickness [DU]",dep.var.labels="",
          covariate.labels=c("Unit On","Unit On * Coal","Unit On * Natural Gas","Unit On * Other"),
          column.labels=c("Old Coal Units","New Coal Units","All Coal Units","All Units","All Locations (0.25x0.25)"),column.separate=c(2,2,2,2,2),
          notes=c("Odd-numbered models include standard errors clustered by location and year;","Even-numbered models have standard errors clustered by location.","For models 1-8, location is the unit, for models 9-10, location is grid cell."),
          notes.align="l"
)


mod.no2.oc.fy = felm(no2.0km ~ uniton | facunitid + year | 0 | facunitid + year,data=df_oldcoal)
mod.no2.oc.f = felm(no2.0km ~ uniton | facunitid + year | 0 | facunitid,data=df_oldcoal)
mod.no2.nc.fy = felm(no2.0km ~ uniton | facunitid + year | 0 | facunitid + year,data=df_newcoal)
mod.no2.nc.f = felm(no2.0km ~ uniton | facunitid + year | 0 | facunitid,data=df_newcoal)
mod.no2.ac.fy = felm(no2.0km ~ uniton | facunitid + year | 0 | facunitid + year,data=df_allcoal)
mod.no2.ac.f = felm(no2.0km ~ uniton | facunitid + year | 0 | facunitid,data=df_allcoal)
mod.no2.a.fy = felm(no2.0km ~ uniton:fueltype | facunitid + year | 0 | facunitid + year,data=df)
mod.no2.a.f = felm(no2.0km ~ uniton:fueltype | facunitid + year | 0 | facunitid,data=df)
mod.no2.ap.ly = felm(no2.0km ~ uniton:fueltype | locationid2 + year | 0 | locationid2 + year,data=df_ap2)
mod.no2.ap.l = felm(no2.0km ~ uniton:fueltype | locationid2 + year | 0 | locationid2,data=df_ap2)

stargazer(mod.no2.oc.fy,mod.no2.oc.f,mod.no2.nc.fy,mod.no2.nc.f,mod.no2.ac.fy,mod.no2.ac.f,mod.no2.a.fy,mod.no2.a.f,mod.no2.ap.ly,mod.no2.ap.l,
          out="Coal_NO2_subsets_table.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
          out.header=T,dep.var.caption="NO2 Thickness [DU]",dep.var.labels="",
          covariate.labels=c("Unit On","Unit On * Coal","Unit On * Natural Gas","Unit On * Other"),
          column.labels=c("Old Coal Units","New Coal Units","All Coal Units","All Units","All Locations (0.25x0.25)"),column.separate=c(2,2,2,2,2),
          notes=c("Odd-numbered models include standard errors clustered by location and year;","Even-numbered models have standard errors clustered by location.","For models 1-8, location is the unit, for models 9-10, location is grid cell."),
          notes.align="l"
)

mod.so2.oc.fy = felm(so2.0km ~ uniton | facunitid + year | 0 | facunitid + year,data=df_oldcoal)
mod.so2.oc.f = felm(so2.0km ~ uniton | facunitid + year | 0 | facunitid,data=df_oldcoal)
mod.so2.nc.fy = felm(so2.0km ~ uniton | facunitid + year | 0 | facunitid + year,data=df_newcoal)
mod.so2.nc.f = felm(so2.0km ~ uniton | facunitid + year | 0 | facunitid,data=df_newcoal)
mod.so2.ac.fy = felm(so2.0km ~ uniton | facunitid + year | 0 | facunitid + year,data=df_allcoal)
mod.so2.ac.f = felm(so2.0km ~ uniton | facunitid + year | 0 | facunitid,data=df_allcoal)
mod.so2.a.fy = felm(so2.0km ~ uniton:fueltype | facunitid + year | 0 | facunitid + year,data=df)
mod.so2.a.f = felm(so2.0km ~ uniton:fueltype | facunitid + year | 0 | facunitid,data=df)
mod.so2.ap.ly = felm(so2.0km ~ uniton:fueltype | locationid2 + year | 0 | locationid2 + year,data=df_ap)
mod.so2.ap.l = felm(so2.0km ~ uniton:fueltype | locationid2 + year | 0 | locationid2,data=df_ap)

stargazer(mod.so2.oc.fy,mod.so2.oc.f,mod.so2.nc.fy,mod.so2.nc.f,mod.so2.ac.fy,mod.so2.ac.f,mod.so2.a.fy,mod.so2.a.f,mod.so2.ap.ly,mod.so2.ap.l,
          out="Coal_SO2_subsets_table.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
          out.header=T,dep.var.caption="SO2 Thickness [DU]",dep.var.labels="",
          covariate.labels=c("Unit On","Unit On * Coal","Unit On * Natural Gas","Unit On * Other"),
          column.labels=c("Old Coal Units","New Coal Units","All Coal Units","All Units","All Locations (0.25x0.25)"),column.separate=c(2,2,2,2,2),
          notes=c("Odd-numbered models include standard errors clustered by location and year;","Even-numbered models have standard errors clustered by location.","For models 1-8, location is the unit, for models 9-10, location is grid cell."),
          notes.align="l"
)

# -----------------------------------------------------------------
# Tables of NG on Impacts on different parameters, RD, unit FE
# -----------------------------------------------------------------
mod.pm.ng.fy = felm(pm.0km ~ uniton | facunitid + year | 0 | facunitid + year,data=df_newng)
mod.pm.ng.f = felm(pm.0km ~ uniton | facunitid + year | 0 | facunitid,data=df_newng)
mod.pm.ang.fy = felm(pm.0km ~ uniton | facunitid + year | 0 | facunitid + year,data=df_allng)
mod.pm.ang.f = felm(pm.0km ~ uniton | facunitid + year | 0 | facunitid,data=df_allng)
mod.pm.a.fy = felm(pm.0km ~ uniton:fueltype | facunitid + year | 0 | facunitid + year,data=df)
mod.pm.a.f = felm(pm.0km ~ uniton:fueltype | facunitid + year | 0 | facunitid,data=df)
mod.pm.ap.ly = felm(pm.0km ~ uniton:fueltype | locationid2 + year | 0 | locationid2 + year,data=df_ap)
mod.pm.ap.l = felm(pm.0km ~ uniton:fueltype | locationid2 + year | 0 | locationid2,data=df_ap)

stargazer(mod.pm.ng.fy,mod.pm.ng.f,mod.pm.ang.fy,mod.pm.ang.f,mod.pm.a.fy,mod.pm.a.f,mod.pm.ap.ly,mod.pm.ap.l,
          out="NG_PM_subsets_table.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
          out.header=T,dep.var.caption="Particulate Matter Concentration [ug/m3]",dep.var.labels="",
          covariate.labels=c("Unit On","Unit On * Coal","Unit On * Natural Gas","Unit On * Other"),
          column.labels=c("New NG Units","All NG Units","All Units","All Locations (0.25x0.25)"),column.separate=c(2,2,2,2),
          notes=c("Odd-numbered models include standard errors clustered by location and year;","Even-numbered models have standard errors clustered by location.","For models 1-6, location is the unit, for models 7-8, location is grid cell."),
          notes.align="l"
)

mod.pm.ng.fy = felm(epa.pm.0km ~ uniton | facunitid + year | 0 | facunitid + year,data=df_newng)
mod.pm.ng.f = felm(epa.pm.0km ~ uniton | facunitid + year | 0 | facunitid,data=df_newng)
mod.pm.ang.fy = felm(epa.pm.0km ~ uniton | facunitid + year | 0 | facunitid + year,data=df_allng)
mod.pm.ang.f = felm(epa.pm.0km ~ uniton | facunitid + year | 0 | facunitid,data=df_allng)
mod.pm.a.fy = felm(epa.pm.0km ~ uniton:fueltype | facunitid + year | 0 | facunitid + year,data=df)
mod.pm.a.f = felm(epa.pm.0km ~ uniton:fueltype | facunitid + year | 0 | facunitid,data=df)
mod.pm.ap.ly = felm(epa.pm.0km ~ uniton:fueltype | locationid2 + year | 0 | locationid2 + year,data=df_ap)
mod.pm.ap.l = felm(epa.pm.0km ~ uniton:fueltype | locationid2 + year | 0 | locationid2,data=df_ap)

stargazer(mod.pm.ng.fy,mod.pm.ng.f,mod.pm.ang.fy,mod.pm.ang.f,mod.pm.a.fy,mod.pm.a.f,mod.pm.ap.ly,mod.pm.ap.l,
          out="NG_EPA_PM_subsets_table.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
          out.header=T,dep.var.caption="Particulate Matter Concentration [ug/m3]",dep.var.labels="",
          covariate.labels=c("Unit On","Unit On * Coal","Unit On * Natural Gas","Unit On * Other"),
          column.labels=c("New NG Units","All NG Units","All Units","All Locations (0.25x0.25)"),column.separate=c(2,2,2,2),
          notes=c("Odd-numbered models include standard errors clustered by location and year;","Even-numbered models have standard errors clustered by location.","For models 1-6, location is the unit, for models 7-8, location is grid cell."),
          notes.align="l"
)

mod.o3.ng.fy = felm(omi.o3.0km ~ uniton | facunitid + year | 0 | facunitid + year,data=df_newng)
mod.o3.ng.f = felm(omi.o3.0km ~ uniton | facunitid + year | 0 | facunitid,data=df_newng)
mod.o3.ang.fy = felm(omi.o3.0km ~ uniton | facunitid + year | 0 | facunitid + year,data=df_allng)
mod.o3.ang.f = felm(omi.o3.0km ~ uniton | facunitid + year | 0 | facunitid,data=df_allng)
mod.o3.a.fy = felm(omi.o3.0km ~ uniton:fueltype | facunitid + year | 0 | facunitid + year,data=df)
mod.o3.a.f = felm(omi.o3.0km ~ uniton:fueltype | facunitid + year | 0 | facunitid,data=df)
mod.o3.ap.ly = felm(omi.o3.0km ~ uniton:fueltype | locationid2 + year | 0 | locationid2 + year,data=df_ap)
mod.o3.ap.l = felm(omi.o3.0km ~ uniton:fueltype | locationid2 + year | 0 | locationid2,data=df_ap)

stargazer(mod.o3.ng.fy,mod.o3.ng.f,mod.o3.ang.fy,mod.o3.ang.f,mod.o3.a.fy,mod.o3.a.f,mod.o3.ap.ly,mod.o3.ap.l,
          out="NG_O3_subsets_table.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
          out.header=T,dep.var.caption="Ozone Thickness [DU]",dep.var.labels="",
          covariate.labels=c("Unit On","Unit On * Coal","Unit On * Natural Gas","Unit On * Other"),
          column.labels=c("New NG Units","All NG Units","All Units","All Locations (0.25x0.25)"),column.separate=c(2,2,2,2),
          notes=c("Odd-numbered models include standard errors clustered by location and year;","Even-numbered models have standard errors clustered by location.","For models 1-6, location is the unit, for models 7-8, location is grid cell."),
          notes.align="l"
)

mod.o3.ng.fy = felm(epa.o3.0km ~ uniton | facunitid + year | 0 | facunitid + year,data=df_newng)
mod.o3.ng.f = felm(epa.o3.0km ~ uniton | facunitid + year | 0 | facunitid,data=df_newng)
mod.o3.ang.fy = felm(epa.o3.0km ~ uniton | facunitid + year | 0 | facunitid + year,data=df_allng)
mod.o3.ang.f = felm(epa.o3.0km ~ uniton | facunitid + year | 0 | facunitid,data=df_allng)
mod.o3.a.fy = felm(epa.o3.0km ~ uniton:fueltype | facunitid + year | 0 | facunitid + year,data=df)
mod.o3.a.f = felm(epa.o3.0km ~ uniton:fueltype | facunitid + year | 0 | facunitid,data=df)
mod.o3.ap.ly = felm(epa.o3.0km ~ uniton:fueltype | locationid2 + year | 0 | locationid2 + year,data=df_ap)
mod.o3.ap.l = felm(epa.o3.0km ~ uniton:fueltype | locationid2 + year | 0 | locationid2,data=df_ap)

stargazer(mod.o3.ng.fy,mod.o3.ng.f,mod.o3.ang.fy,mod.o3.ang.f,mod.o3.a.fy,mod.o3.a.f,mod.o3.ap.ly,mod.o3.ap.l,
          out="NG_EPA_O3_subsets_table.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
          out.header=T,dep.var.caption="Ozone Thickness [DU]",dep.var.labels="",
          covariate.labels=c("Unit On","Unit On * Coal","Unit On * Natural Gas","Unit On * Other"),
          column.labels=c("New NG Units","All NG Units","All Units","All Locations (0.25x0.25)"),column.separate=c(2,2,2,2),
          notes=c("Odd-numbered models include standard errors clustered by location and year;","Even-numbered models have standard errors clustered by location.","For models 1-6, location is the unit, for models 7-8, location is grid cell."),
          notes.align="l"
)

mod.no2.ng.fy = felm(no2.0km ~ uniton | facunitid + year | 0 | facunitid + year,data=df_newng)
mod.no2.ng.f = felm(no2.0km ~ uniton | facunitid + year | 0 | facunitid,data=df_newng)
mod.no2.ang.fy = felm(no2.0km ~ uniton | facunitid + year | 0 | facunitid + year,data=df_allng)
mod.no2.ang.f = felm(no2.0km ~ uniton | facunitid + year | 0 | facunitid,data=df_allng)
mod.no2.a.fy = felm(no2.0km ~ uniton:fueltype | facunitid + year | 0 | facunitid + year,data=df)
mod.no2.a.f = felm(no2.0km ~ uniton:fueltype | facunitid + year | 0 | facunitid,data=df)
mod.no2.ap.ly = felm(no2.0km ~ uniton:fueltype | locationid2 + year | 0 | locationid2 + year,data=df_ap2)
mod.no2.ap.l = felm(no2.0km ~ uniton:fueltype | locationid2 + year | 0 | locationid2,data=df_ap2)

stargazer(mod.no2.ng.fy,mod.no2.ng.f,mod.no2.ang.fy,mod.no2.ang.f,mod.no2.a.fy,mod.no2.a.f,mod.no2.ap.ly,mod.no2.ap.l,
          out="NG_NO2_subsets_table.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
          out.header=T,dep.var.caption="NO2 Thickness [DU]",dep.var.labels="",
          covariate.labels=c("Unit On","Unit On * Coal","Unit On * Natural Gas","Unit On * Other"),
          column.labels=c("New NG Units","All NG Units","All Units","All Locations (0.25x0.25)"),column.separate=c(2,2,2,2),
          notes=c("Odd-numbered models include standard errors clustered by location and year;","Even-numbered models have standard errors clustered by location.","For models 1-6, location is the unit, for models 7-8, location is grid cell."),
          notes.align="l"
)

mod.so2.ng.fy = felm(so2.0km ~ uniton | facunitid + year | 0 | facunitid + year,data=df_newng)
mod.so2.ng.f = felm(so2.0km ~ uniton | facunitid + year | 0 | facunitid,data=df_newng)
mod.so2.ang.fy = felm(so2.0km ~ uniton | facunitid + year | 0 | facunitid + year,data=df_allng)
mod.so2.ang.f = felm(so2.0km ~ uniton | facunitid + year | 0 | facunitid,data=df_allng)
mod.so2.a.fy = felm(so2.0km ~ uniton:fueltype | facunitid + year | 0 | facunitid + year,data=df)
mod.so2.a.f = felm(so2.0km ~ uniton:fueltype | facunitid + year | 0 | facunitid,data=df)
mod.so2.ap.ly = felm(so2.0km ~ uniton:fueltype | locationid2 + year | 0 | locationid2 + year,data=df_ap)
mod.so2.ap.l = felm(so2.0km ~ uniton:fueltype | locationid2 + year | 0 | locationid2,data=df_ap)

stargazer(mod.so2.ng.fy,mod.so2.ng.f,mod.so2.ang.fy,mod.so2.ang.f,mod.so2.a.fy,mod.so2.a.f,mod.so2.ap.ly,mod.so2.ap.l,
          out="NG_SO2_subsets_table.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
          out.header=T,dep.var.caption="SO2 Thickness [DU]",dep.var.labels="",
          covariate.labels=c("Unit On","Unit On * Coal","Unit On * Natural Gas","Unit On * Other"),
          column.labels=c("New NG Units","All NG Units","All Units","All Locations (0.25x0.25)"),column.separate=c(2,2,2,2),
          notes=c("Odd-numbered models include standard errors clustered by location and year;","Even-numbered models have standard errors clustered by location.","For models 1-6, location is the unit, for models 7-8, location is grid cell."),
          notes.align="l"
)

# Special NG Table - all

stargazer(mod.pm.ng.fy,mod.pm.ng.f,mod.pm.ang.fy,mod.pm.ang.f,
          mod.no2.ng.fy,mod.no2.ng.f,mod.no2.ang.fy,mod.no2.ang.f,
          mod.so2.ng.fy,mod.so2.ng.f,mod.so2.ang.fy,mod.so2.ang.f,
          out="NG_All_subsets_table.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
          out.header=T,dep.var.caption="EPA Particulate Matter Concentration [ug/m3], NO2 [DU], SO2 [DU]",dep.var.labels="",
          covariate.labels=c("Unit On"),
          column.labels=c("New NG Units","All NG Units","New NG Units","All NG Units","New NG Units","All NG Units"),column.separate=c(2,2,2,2),
          notes=c("Odd-numbered models include standard errors clustered by location and year;","Even-numbered models have standard errors clustered by location.","For models 1-6, location is the unit, for models 7-8, location is grid cell."),
          notes.align="l"
)


# -----------------------------------------------------------------
# INTENSITY
# -----------------------------------------------------------------
# -----------------------------------------------------------------
# Tables of Coal Impacts on different parameters, RD, unit FE
# -----------------------------------------------------------------

mega <- function(x) (x * 1e6)

mod.pm.oc.fy = felm(pm.0km ~ grossload | facunitid + year | 0 | facunitid + year,data=df_oldcoal)
mod.pm.oc.f = felm(pm.0km ~ grossload | facunitid + year | 0 | facunitid,data=df_oldcoal)
mod.pm.nc.fy = felm(pm.0km ~ grossload | facunitid + year | 0 | facunitid + year,data=df_newcoal)
mod.pm.nc.f = felm(pm.0km ~ grossload | facunitid + year | 0 | facunitid,data=df_newcoal)
mod.pm.ac.fy = felm(pm.0km ~ grossload | facunitid + year | 0 | facunitid + year,data=df_allcoal)
mod.pm.ac.f = felm(pm.0km ~ grossload | facunitid + year | 0 | facunitid,data=df_allcoal)
mod.pm.a.fy = felm(pm.0km ~ grossload:fueltype | facunitid + year | 0 | facunitid + year,data=df)
mod.pm.a.f = felm(pm.0km ~ grossload:fueltype | facunitid + year | 0 | facunitid,data=df)
mod.pm.ap.ly = felm(pm.0km ~ grossload:fueltype | locationid2 + year | 0 | locationid2 + year,data=df_ap)
mod.pm.ap.l = felm(pm.0km ~ grossload:fueltype | locationid2 + year | 0 | locationid2,data=df_ap)

stargazer(mod.pm.oc.fy,mod.pm.oc.f,mod.pm.nc.fy,mod.pm.nc.f,mod.pm.ac.fy,mod.pm.ac.f,mod.pm.a.fy,mod.pm.a.f,mod.pm.ap.ly,mod.pm.ap.l,
          out="Coal_PM_Intensity_subsets_table.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
          out.header=T,dep.var.caption="Particulate Matter Concentration [ug/m3]",dep.var.labels="",
          covariate.labels=c("Gross Load","Gross Load * Coal","Gross Load * Natural Gas","Gross Load * Other"),
          column.labels=c("Old Coal Units","New Coal Units","All Coal Units","All Units","All Locations (0.25x0.25)"),column.separate=c(2,2,2,2,2),
          notes=c("Odd-numbered models include standard errors clustered by location and year;","Even-numbered models have standard errors clustered by location.","For models 1-8, location is the unit, for models 9-10, location is grid cell."),
          notes.align="l",apply.coef=mega,apply.se=mega
)

mod.o3.oc.fy = felm(omi.o3.0km ~ grossload | facunitid + year | 0 | facunitid + year,data=df_oldcoal)
mod.o3.oc.f = felm(omi.o3.0km ~ grossload | facunitid + year | 0 | facunitid,data=df_oldcoal)
mod.o3.nc.fy = felm(omi.o3.0km ~ grossload | facunitid + year | 0 | facunitid + year,data=df_newcoal)
mod.o3.nc.f = felm(omi.o3.0km ~ grossload | facunitid + year | 0 | facunitid,data=df_newcoal)
mod.o3.ac.fy = felm(omi.o3.0km ~ grossload | facunitid + year | 0 | facunitid + year,data=df_allcoal)
mod.o3.ac.f = felm(omi.o3.0km ~ grossload | facunitid + year | 0 | facunitid,data=df_allcoal)
mod.o3.a.fy = felm(omi.o3.0km ~ grossload:fueltype | facunitid + year | 0 | facunitid + year,data=df)
mod.o3.a.f = felm(omi.o3.0km ~ grossload:fueltype | facunitid + year | 0 | facunitid,data=df)
mod.o3.ap.ly = felm(omi.o3.0km ~ grossload:fueltype | locationid2 + year | 0 | locationid2 + year,data=df_ap)
mod.o3.ap.l = felm(omi.o3.0km ~ grossload:fueltype | locationid2 + year | 0 | locationid2,data=df_ap)

stargazer(mod.o3.oc.fy,mod.o3.oc.f,mod.o3.nc.fy,mod.o3.nc.f,mod.o3.ac.fy,mod.o3.ac.f,mod.o3.a.fy,mod.o3.a.f,mod.o3.ap.ly,mod.o3.ap.l,
          out="Coal_O3_Intensity_subsets_table.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
          out.header=T,dep.var.caption="Ozone Thickness [DU]",dep.var.labels="",
          covariate.labels=c("Gross Load","Gross Load * Coal","Gross Load * Natural Gas","Gross Load * Other"),
          column.labels=c("Old Coal Units","New Coal Units","All Coal Units","All Units","All Locations (0.25x0.25)"),column.separate=c(2,2,2,2,2),
          notes=c("Odd-numbered models include standard errors clustered by location and year;","Even-numbered models have standard errors clustered by location.","For models 1-8, location is the unit, for models 9-10, location is grid cell."),
          notes.align="l",apply.coef=mega,apply.se=mega
)

mod.o3.oc.fy = felm(epa.o3.0km ~ grossload | facunitid + year | 0 | facunitid + year,data=df_oldcoal)
mod.o3.oc.f = felm(epa.o3.0km ~ grossload | facunitid + year | 0 | facunitid,data=df_oldcoal)
mod.o3.nc.fy = felm(epa.o3.0km ~ grossload | facunitid + year | 0 | facunitid + year,data=df_newcoal)
mod.o3.nc.f = felm(epa.o3.0km ~ grossload | facunitid + year | 0 | facunitid,data=df_newcoal)
mod.o3.ac.fy = felm(epa.o3.0km ~ grossload | facunitid + year | 0 | facunitid + year,data=df_allcoal)
mod.o3.ac.f = felm(epa.o3.0km ~ grossload | facunitid + year | 0 | facunitid,data=df_allcoal)
mod.o3.a.fy = felm(epa.o3.0km ~ grossload:fueltype | facunitid + year | 0 | facunitid + year,data=df)
mod.o3.a.f = felm(epa.o3.0km ~ grossload:fueltype | facunitid + year | 0 | facunitid,data=df)
mod.o3.ap.ly = felm(epa.o3.0km ~ grossload:fueltype | locationid2 + year | 0 | locationid2 + year,data=df_ap)
mod.o3.ap.l = felm(epa.o3.0km ~ grossload:fueltype | locationid2 + year | 0 | locationid2,data=df_ap)

stargazer(mod.o3.oc.fy,mod.o3.oc.f,mod.o3.nc.fy,mod.o3.nc.f,mod.o3.ac.fy,mod.o3.ac.f,mod.o3.a.fy,mod.o3.a.f,mod.o3.ap.ly,mod.o3.ap.l,
          out="Coal_EPA_O3_Intensity_subsets_table.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
          out.header=T,dep.var.caption="Surface Ozone Concentration [ppb]",dep.var.labels="",
          covariate.labels=c("Gross Load","Gross Load * Coal","Gross Load * Natural Gas","Gross Load * Other"),
          column.labels=c("Old Coal Units","New Coal Units","All Coal Units","All Units","All Locations (0.25x0.25)"),column.separate=c(2,2,2,2,2),
          notes=c("Odd-numbered models include standard errors clustered by location and year;","Even-numbered models have standard errors clustered by location.","For models 1-8, location is the unit, for models 9-10, location is grid cell."),
          notes.align="l",apply.coef=mega,apply.se=mega
)


mod.no2.oc.fy = felm(no2.0km ~ grossload | facunitid + year | 0 | facunitid + year,data=df_oldcoal)
mod.no2.oc.f = felm(no2.0km ~ grossload | facunitid + year | 0 | facunitid,data=df_oldcoal)
mod.no2.nc.fy = felm(no2.0km ~ grossload | facunitid + year | 0 | facunitid + year,data=df_newcoal)
mod.no2.nc.f = felm(no2.0km ~ grossload | facunitid + year | 0 | facunitid,data=df_newcoal)
mod.no2.ac.fy = felm(no2.0km ~ grossload | facunitid + year | 0 | facunitid + year,data=df_allcoal)
mod.no2.ac.f = felm(no2.0km ~ grossload | facunitid + year | 0 | facunitid,data=df_allcoal)
mod.no2.a.fy = felm(no2.0km ~ grossload:fueltype | facunitid + year | 0 | facunitid + year,data=df)
mod.no2.a.f = felm(no2.0km ~ grossload:fueltype | facunitid + year | 0 | facunitid,data=df)
mod.no2.ap.ly = felm(no2.0km ~ grossload:fueltype | locationid2 + year | 0 | locationid2 + year,data=df_ap2)
mod.no2.ap.l = felm(no2.0km ~ grossload:fueltype | locationid2 + year | 0 | locationid2,data=df_ap2)

stargazer(mod.no2.oc.fy,mod.no2.oc.f,mod.no2.nc.fy,mod.no2.nc.f,mod.no2.ac.fy,mod.no2.ac.f,mod.no2.a.fy,mod.no2.a.f,mod.no2.ap.ly,mod.no2.ap.l,
          out="Coal_NO2_Intensity_subsets_table.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
          out.header=T,dep.var.caption="NO2 Thickness [DU]",dep.var.labels="",
          covariate.labels=c("Gross Load","Gross Load * Coal","Gross Load * Natural Gas","Gross Load * Other"),
          column.labels=c("Old Coal Units","New Coal Units","All Coal Units","All Units","All Locations (0.25x0.25)"),column.separate=c(2,2,2,2,2),
          notes=c("Odd-numbered models include standard errors clustered by location and year;","Even-numbered models have standard errors clustered by location.","For models 1-8, location is the unit, for models 9-10, location is grid cell."),
          notes.align="l",apply.coef=mega,apply.se=mega
)

mod.so2.oc.fy = felm(so2.0km ~ grossload | facunitid + year | 0 | facunitid + year,data=df_oldcoal)
mod.so2.oc.f = felm(so2.0km ~ grossload | facunitid + year | 0 | facunitid,data=df_oldcoal)
mod.so2.nc.fy = felm(so2.0km ~ grossload | facunitid + year | 0 | facunitid + year,data=df_newcoal)
mod.so2.nc.f = felm(so2.0km ~ grossload | facunitid + year | 0 | facunitid,data=df_newcoal)
mod.so2.ac.fy = felm(so2.0km ~ grossload | facunitid + year | 0 | facunitid + year,data=df_allcoal)
mod.so2.ac.f = felm(so2.0km ~ grossload | facunitid + year | 0 | facunitid,data=df_allcoal)
mod.so2.a.fy = felm(so2.0km ~ grossload:fueltype | facunitid + year | 0 | facunitid + year,data=df)
mod.so2.a.f = felm(so2.0km ~ grossload:fueltype | facunitid + year | 0 | facunitid,data=df)
mod.so2.ap.ly = felm(so2.0km ~ grossload:fueltype | locationid2 + year | 0 | locationid2 + year,data=df_ap)
mod.so2.ap.l = felm(so2.0km ~ grossload:fueltype | locationid2 + year | 0 | locationid2,data=df_ap)

stargazer(mod.so2.oc.fy,mod.so2.oc.f,mod.so2.nc.fy,mod.so2.nc.f,mod.so2.ac.fy,mod.so2.ac.f,mod.so2.a.fy,mod.so2.a.f,mod.so2.ap.ly,mod.so2.ap.l,
          out="Coal_SO2_Intensity_subsets_table.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
          out.header=T,dep.var.caption="SO2 Thickness [DU]",dep.var.labels="",
          covariate.labels=c("Gross Load","Gross Load * Coal","Gross Load * Natural Gas","Gross Load * Other"),
          column.labels=c("Old Coal Units","New Coal Units","All Coal Units","All Units","All Locations (0.25x0.25)"),column.separate=c(2,2,2,2,2),
          notes=c("Odd-numbered models include standard errors clustered by location and year;","Even-numbered models have standard errors clustered by location.","For models 1-8, location is the unit, for models 9-10, location is grid cell."),
          notes.align="l",apply.coef=mega,apply.se=mega
)

# -----------------------------------------------------------------
# Tables of NG on Impacts on different parameters, RD, unit FE
# -----------------------------------------------------------------
mod.pm.ng.fy = felm(pm.0km ~ grossload | facunitid + year | 0 | facunitid + year,data=df_newng)
mod.pm.ng.f = felm(pm.0km ~ grossload | facunitid + year | 0 | facunitid,data=df_newng)
mod.pm.ang.fy = felm(pm.0km ~ grossload | facunitid + year | 0 | facunitid + year,data=df_allng)
mod.pm.ang.f = felm(pm.0km ~ grossload | facunitid + year | 0 | facunitid,data=df_allng)
mod.pm.a.fy = felm(pm.0km ~ grossload:fueltype | facunitid + year | 0 | facunitid + year,data=df)
mod.pm.a.f = felm(pm.0km ~ grossload:fueltype | facunitid + year | 0 | facunitid,data=df)
mod.pm.ap.ly = felm(pm.0km ~ grossload:fueltype | locationid2 + year | 0 | locationid2 + year,data=df_ap)
mod.pm.ap.l = felm(pm.0km ~ grossload:fueltype | locationid2 + year | 0 | locationid2,data=df_ap)

stargazer(mod.pm.ng.fy,mod.pm.ng.f,mod.pm.ang.fy,mod.pm.ang.f,mod.pm.a.fy,mod.pm.a.f,mod.pm.ap.ly,mod.pm.ap.l,
          out="NG_PM_Intensity_subsets_table.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
          out.header=T,dep.var.caption="Particulate Matter Concentration [ug/m3]",dep.var.labels="",
          covariate.labels=c("Gross Load","Gross Load * Coal","Gross Load * Natural Gas","Gross Load * Other"),
          column.labels=c("New NG Units","All NG Units","All Units","All Locations (0.25x0.25)"),column.separate=c(2,2,2,2),
          notes=c("Odd-numbered models include standard errors clustered by location and year;","Even-numbered models have standard errors clustered by location.","For models 1-6, location is the unit, for models 7-8, location is grid cell."),
          notes.align="l",apply.coef=mega,apply.se=mega
)

mod.o3.ng.fy = felm(omi.o3.0km ~ grossload | facunitid + year | 0 | facunitid + year,data=df_newng)
mod.o3.ng.f = felm(omi.o3.0km ~ grossload | facunitid + year | 0 | facunitid,data=df_newng)
mod.o3.ang.fy = felm(omi.o3.0km ~ grossload | facunitid + year | 0 | facunitid + year,data=df_allng)
mod.o3.ang.f = felm(omi.o3.0km ~ grossload | facunitid + year | 0 | facunitid,data=df_allng)
mod.o3.a.fy = felm(omi.o3.0km ~ grossload:fueltype | facunitid + year | 0 | facunitid + year,data=df)
mod.o3.a.f = felm(omi.o3.0km ~ grossload:fueltype | facunitid + year | 0 | facunitid,data=df)
mod.o3.ap.ly = felm(omi.o3.0km ~ grossload:fueltype | locationid2 + year | 0 | locationid2 + year,data=df_ap)
mod.o3.ap.l = felm(omi.o3.0km ~ grossload:fueltype | locationid2 + year | 0 | locationid2,data=df_ap)

stargazer(mod.o3.ng.fy,mod.o3.ng.f,mod.o3.ang.fy,mod.o3.ang.f,mod.o3.a.fy,mod.o3.a.f,mod.o3.ap.ly,mod.o3.ap.l,
          out="NG_O3_Intensity_subsets_table.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
          out.header=T,dep.var.caption="Ozone Thickness [DU]",dep.var.labels="",
          covariate.labels=c("Gross Load","Gross Load * Coal","Gross Load * Natural Gas","Gross Load * Other"),
          column.labels=c("New NG Units","All NG Units","All Units","All Locations (0.25x0.25)"),column.separate=c(2,2,2,2),
          notes=c("Odd-numbered models include standard errors clustered by location and year;","Even-numbered models have standard errors clustered by location.","For models 1-6, location is the unit, for models 7-8, location is grid cell."),
          notes.align="l",apply.coef=mega,apply.se=mega
)

mod.no2.ng.fy = felm(no2.0km ~ grossload | facunitid + year | 0 | facunitid + year,data=df_newng)
mod.no2.ng.f = felm(no2.0km ~ grossload | facunitid + year | 0 | facunitid,data=df_newng)
mod.no2.ang.fy = felm(no2.0km ~ grossload | facunitid + year | 0 | facunitid + year,data=df_allng)
mod.no2.ang.f = felm(no2.0km ~ grossload | facunitid + year | 0 | facunitid,data=df_allng)
mod.no2.a.fy = felm(no2.0km ~ grossload:fueltype | facunitid + year | 0 | facunitid + year,data=df)
mod.no2.a.f = felm(no2.0km ~ grossload:fueltype | facunitid + year | 0 | facunitid,data=df)
mod.no2.ap.ly = felm(no2.0km ~ grossload:fueltype | locationid2 + year | 0 | locationid2 + year,data=df_ap2)
mod.no2.ap.l = felm(no2.0km ~ grossload:fueltype | locationid2 + year | 0 | locationid2,data=df_ap2)

stargazer(mod.no2.ng.fy,mod.no2.ng.f,mod.no2.ang.fy,mod.no2.ang.f,mod.no2.a.fy,mod.no2.a.f,mod.no2.ap.ly,mod.no2.ap.l,
          out="NG_NO2_Intensity_subsets_table.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
          out.header=T,dep.var.caption="NO2 Thickness [DU]",dep.var.labels="",
          covariate.labels=c("Gross Load","Gross Load * Coal","Gross Load * Natural Gas","Gross Load * Other"),
          column.labels=c("New NG Units","All NG Units","All Units","All Locations (0.25x0.25)"),column.separate=c(2,2,2,2),
          notes=c("Odd-numbered models include standard errors clustered by location and year;","Even-numbered models have standard errors clustered by location.","For models 1-6, location is the unit, for models 7-8, location is grid cell."),
          notes.align="l",apply.coef=mega,apply.se=mega
)

mod.so2.ng.fy = felm(so2.0km ~ grossload | facunitid + year | 0 | facunitid + year,data=df_newng)
mod.so2.ng.f = felm(so2.0km ~ grossload | facunitid + year | 0 | facunitid,data=df_newng)
mod.so2.ang.fy = felm(so2.0km ~ grossload | facunitid + year | 0 | facunitid + year,data=df_allng)
mod.so2.ang.f = felm(so2.0km ~ grossload | facunitid + year | 0 | facunitid,data=df_allng)
mod.so2.a.fy = felm(so2.0km ~ grossload:fueltype | facunitid + year | 0 | facunitid + year,data=df)
mod.so2.a.f = felm(so2.0km ~ grossload:fueltype | facunitid + year | 0 | facunitid,data=df)
mod.so2.ap.ly = felm(so2.0km ~ grossload:fueltype | locationid2 + year | 0 | locationid2 + year,data=df_ap)
mod.so2.ap.l = felm(so2.0km ~ grossload:fueltype | locationid2 + year | 0 | locationid2,data=df_ap)

stargazer(mod.so2.ng.fy,mod.so2.ng.f,mod.so2.ang.fy,mod.so2.ang.f,mod.so2.a.fy,mod.so2.a.f,mod.so2.ap.ly,mod.so2.ap.l,
          out="NG_SO2_Intensity_subsets_table.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
          out.header=T,dep.var.caption="SO2 Thickness [DU]",dep.var.labels="",
          covariate.labels=c("Gross Load","Gross Load * Coal","Gross Load * Natural Gas","Gross Load * Other"),
          column.labels=c("New NG Units","All NG Units","All Units","All Locations (0.25x0.25)"),column.separate=c(2,2,2,2),
          notes=c("Odd-numbered models include standard errors clustered by location and year;","Even-numbered models have standard errors clustered by location.","For models 1-6, location is the unit, for models 7-8, location is grid cell."),
          notes.align="l",apply.coef=mega,apply.se=mega
)

# Special NG tables putting all together.

stargazer(mod.pm.ng.fy,mod.pm.ng.f,mod.pm.ang.fy,mod.pm.ang.f,
          mod.no2.ng.fy,mod.no2.ng.f,mod.no2.ang.fy,mod.no2.ang.f,
          mod.so2.ng.fy,mod.so2.ng.f,mod.so2.ang.fy,mod.so2.ang.f,
          out="NG_All_Intensity_subsets_table.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
          out.header=T,dep.var.caption="Particulate Matter Concentration [ug/m3], NO2 [DU], SO2 [DU]",dep.var.labels="",
          covariate.labels=c("Gross Load"),
          column.labels=c("New NG Units","All NG Units","New NG Units","All NG Units","New NG Units","All NG Units"),column.separate=c(2,2,2,2),
          notes=c("Odd-numbered models include standard errors clustered by location and year;","Even-numbered models have standard errors clustered by location.","For models 1-6, location is the unit, for models 7-8, location is grid cell."),
          notes.align="l",apply.coef=mega,apply.se=mega
)

# -----------------------------------------------------------------
# Tables of Impacts on outcomes, different RD, IV specs
# -----------------------------------------------------------------
df = fullpanel.plants.annual
  df$yearfac = as.factor(df$year)
  df$locfac = as.factor(df$locationid)
  df$ufac = as.factor(df$facunitid)

df_oldcoal = subset(df,df$oldcoalplant==1,select=c(uniton,tau_off,facunitid,locationid,yearfac,locfac,ufac,County,year,pm.0km,pm.100km,epa.o3.0km,so2.0km,no2.0km,l.Total.Crude.Rate,l.Crude.Rate.Under1,l.w.wheat,l.soy,l.corn,uniton_lead1,uniton_lead2,uniton_lead3,uniton_lag1,uniton_lag2,uniton_lag3))
df_newcoal = subset(df,df$newcoalplant==1,select=c(uniton,tau_on,facunitid,locationid,yearfac,locfac,ufac,County,year,pm.0km,pm.100km,epa.o3.0km,so2.0km,no2.0km,l.Total.Crude.Rate,l.Crude.Rate.Under1,l.w.wheat,l.soy,l.corn,uniton_lead1,uniton_lead2,uniton_lead3,uniton_lag1,uniton_lag2,uniton_lag3))
df_allcoal = subset(df,df$fueltype=="Coal",select=c(uniton,tau_off,tau_on,facunitid,locationid,yearfac,locfac,ufac,County,year,pm.0km,pm.100km,epa.o3.0km,so2.0km,no2.0km,l.Total.Crude.Rate,l.Crude.Rate.Under1,l.w.wheat,l.soy,l.corn,uniton_lead1,uniton_lead2,uniton_lead3,uniton_lag1,uniton_lag2,uniton_lag3))
df_newng = subset(df,df$newngplant==1,select=c(uniton,tau_on,facunitid,locationid,yearfac,locfac,ufac,County,year,pm.0km,pm.100km,epa.o3.0km,so2.0km,no2.0km,l.Total.Crude.Rate,l.Crude.Rate.Under1,l.w.wheat,l.soy,l.corn,uniton_lead1,uniton_lead2,uniton_lead3,uniton_lag1,uniton_lag2,uniton_lag3))
df_allng = subset(df,df$fueltype=="Natural Gas",select=c(uniton,tau_off,tau_on,facunitid,locationid,yearfac,locfac,ufac,County,year,pm.0km,pm.100km,epa.o3.0km,so2.0km,no2.0km,l.Total.Crude.Rate,l.Crude.Rate.Under1,l.w.wheat,l.soy,l.corn,uniton_lead1,uniton_lead2,uniton_lead3,uniton_lag1,uniton_lag2,uniton_lag3))

mod.pm.oc.f = felm(pm.0km ~ factor(uniton) | ufac + yearfac | 0 | ufac,data=df_oldcoal[is.finite(df_oldcoal$pm.0km),],na.action=na.omit)
mod.pm.nc.f = felm(pm.0km ~ uniton | ufac + yearfac | 0 | ufac,data=df_newcoal[is.finite(df_newcoal$pm.0km),],na.action=na.omit)
mod.pm.ac.f = felm(pm.0km ~ uniton | ufac + yearfac | 0 | ufac,data=df_allcoal[is.finite(df_allcoal$pm.0km),],na.action=na.omit)
mod.pm.ng.f = felm(pm.0km ~ uniton | ufac + yearfac | 0 | ufac,data=df_newng[is.finite(df_newng$pm.0km),],na.action=na.omit)
mod.pm.ang.f = felm(pm.0km ~ uniton | ufac + yearfac | 0 | ufac,data=df_allng[is.finite(df_allng$pm.0km),],na.action=na.omit)
mod.pm.a.f = felm(pm.0km ~ uniton:fueltype | ufac + yearfac | 0 | ufac,data=df[is.finite(df$pm.0km),],na.action=na.omit)

mod.pm100.oc.f = felm(pm.100km ~ factor(uniton) | ufac + yearfac | 0 | ufac,data=df_oldcoal[is.finite(df_oldcoal$pm.100km),],na.action=na.omit)
mod.pm100.nc.f = felm(pm.100km ~ uniton | ufac + yearfac | 0 | ufac,data=df_newcoal[is.finite(df_newcoal$pm.100km),],na.action=na.omit)
mod.pm100.ac.f = felm(pm.100km ~ uniton | ufac + yearfac | 0 | ufac,data=df_allcoal[is.finite(df_allcoal$pm.100km),],na.action=na.omit)
mod.pm100.ng.f = felm(pm.100km ~ uniton | ufac + yearfac | 0 | ufac,data=df_newng[is.finite(df_newng$pm.100km),],na.action=na.omit)
mod.pm100.ang.f = felm(pm.100km ~ uniton | ufac + yearfac | 0 | ufac,data=df_allng[is.finite(df_allng$pm.100km),],na.action=na.omit)
mod.pm100.a.f = felm(pm.100km ~ uniton:fueltype | ufac + yearfac | 0 | ufac,data=df[is.finite(df$pm.100km),],na.action=na.omit)

mod.mort.oc.f = felm(l.Total.Crude.Rate ~ factor(uniton) | ufac + yearfac | 0 | ufac,data=df_oldcoal[is.finite(df_oldcoal$l.Total.Crude.Rate),],na.action=na.omit)
mod.mort.nc.f = felm(l.Total.Crude.Rate ~ uniton | ufac + yearfac | 0 | ufac,data=df_newcoal[is.finite(df_newcoal$l.Total.Crude.Rate),],na.action=na.omit)
mod.mort.ac.f = felm(l.Total.Crude.Rate ~ uniton | ufac + yearfac | 0 | ufac,data=df_allcoal[is.finite(df_allcoal$l.Total.Crude.Rate),],na.action=na.omit)
mod.mort.ng.f = felm(l.Total.Crude.Rate ~ uniton | ufac + yearfac | 0 | ufac,data=df_newng[is.finite(df_newng$l.Total.Crude.Rate),],na.action=na.omit)
mod.mort.ang.f = felm(l.Total.Crude.Rate ~ uniton | ufac + yearfac | 0 | ufac,data=df_allng[is.finite(df_allng$l.Total.Crude.Rate),],na.action=na.omit)
mod.mort.a.f = felm(l.Total.Crude.Rate ~ uniton:fueltype | ufac + yearfac | 0 | ufac,data=df[is.finite(df$l.Total.Crude.Rate),],na.action=na.omit)

mod.mort.oc.iv.f = felm(l.Total.Crude.Rate ~ 0 | ufac + yearfac | (pm.0km~uniton) | ufac,data=df_oldcoal[is.finite(df_oldcoal$l.Total.Crude.Rate),],na.action=na.omit)
mod.mort.nc.iv.f = felm(l.Total.Crude.Rate ~ 0 | ufac + yearfac | (pm.0km~uniton) | ufac,data=df_newcoal[is.finite(df_newcoal$l.Total.Crude.Rate),],na.action=na.omit)
mod.mort.ac.iv.f = felm(l.Total.Crude.Rate ~ 0 | ufac + yearfac | (pm.0km~uniton) | ufac,data=df_allcoal[is.finite(df_allcoal$l.Total.Crude.Rate),],na.action=na.omit)
mod.mort.ng.iv.f = felm(l.Total.Crude.Rate ~ 0 | ufac + yearfac | (pm.0km~uniton) | ufac,data=df_newng[is.finite(df_newng$l.Total.Crude.Rate),],na.action=na.omit)
mod.mort.ang.iv.f = felm(l.Total.Crude.Rate ~ 0 | ufac + yearfac | (pm.0km~uniton) | ufac,data=df_allng[is.finite(df_allng$l.Total.Crude.Rate),],na.action=na.omit)
mod.mort.a.iv.f = felm(l.Total.Crude.Rate ~ 0 | ufac + yearfac | (pm.0km~uniton:fueltype) | ufac,data=df[is.finite(df$l.Total.Crude.Rate),],na.action=na.omit)

mod.wheat.oc.f = felm(l.w.wheat ~ factor(uniton) | ufac + yearfac | 0 | ufac,data=df_oldcoal[is.finite(df_oldcoal$l.w.wheat),],na.action=na.omit)
mod.wheat.nc.f = felm(l.w.wheat ~ uniton | ufac + yearfac | 0 | ufac,data=df_newcoal[is.finite(df_newcoal$l.w.wheat),],na.action=na.omit)
mod.wheat.ac.f = felm(l.w.wheat ~ uniton | ufac + yearfac | 0 | ufac,data=df_allcoal[is.finite(df_allcoal$l.w.wheat),],na.action=na.omit)
mod.wheat.ng.f = felm(l.w.wheat ~ uniton | ufac + yearfac | 0 | ufac,data=df_newng[is.finite(df_newng$l.w.wheat),],na.action=na.omit)
mod.wheat.ang.f = felm(l.w.wheat ~ uniton | ufac + yearfac | 0 | ufac,data=df_allng[is.finite(df_allng$l.w.wheat),],na.action=na.omit)
mod.wheat.a.f = felm(l.w.wheat ~ uniton:fueltype | ufac + yearfac | 0 | ufac,data=df[is.finite(df$l.w.wheat),],na.action=na.omit)

mod.wheat.oc.iv.f = felm(l.w.wheat ~ 0 | ufac + yearfac | (pm.0km~uniton) | ufac,data=df_oldcoal[is.finite(df_oldcoal$l.w.wheat),],na.action=na.omit)
mod.wheat.nc.iv.f = felm(l.w.wheat ~ 0 | ufac + yearfac | (pm.0km~uniton) | ufac,data=df_newcoal[is.finite(df_newcoal$l.w.wheat),],na.action=na.omit)
mod.wheat.ac.iv.f = felm(l.w.wheat ~ 0 | ufac + yearfac | (pm.0km~uniton) | ufac,data=df_allcoal[is.finite(df_allcoal$l.w.wheat),],na.action=na.omit)
mod.wheat.ng.iv.f = felm(l.w.wheat ~ 0 | ufac + yearfac | (pm.0km~uniton) | ufac,data=df_newng[is.finite(df_newng$l.w.wheat),],na.action=na.omit)
mod.wheat.ang.iv.f = felm(l.w.wheat ~ 0 | ufac + yearfac | (pm.0km~uniton) | ufac,data=df_allng[is.finite(df_allng$l.w.wheat),],na.action=na.omit)
mod.wheat.a.iv.f = felm(l.w.wheat ~ 0 | ufac + yearfac | (pm.0km~uniton:fueltype) | ufac,data=df[is.finite(df$l.w.wheat),],na.action=na.omit)

mod.soy.oc.f = felm(l.soy ~ factor(uniton) | ufac + yearfac | 0 | ufac,data=df_oldcoal[is.finite(df_oldcoal$l.soy),],na.action=na.omit)
mod.soy.nc.f = felm(l.soy ~ uniton | ufac + yearfac | 0 | ufac,data=df_newcoal[is.finite(df_newcoal$l.soy),],na.action=na.omit)
mod.soy.ac.f = felm(l.soy ~ uniton | ufac + yearfac | 0 | ufac,data=df_allcoal[is.finite(df_allcoal$l.soy),],na.action=na.omit)
mod.soy.ng.f = felm(l.soy ~ uniton | ufac + yearfac | 0 | ufac,data=df_newng[is.finite(df_newng$l.soy),],na.action=na.omit)
mod.soy.ang.f = felm(l.soy ~ uniton | ufac + yearfac | 0 | ufac,data=df_allng[is.finite(df_allng$l.soy),],na.action=na.omit)
mod.soy.a.f = felm(l.soy ~ uniton:fueltype | ufac + yearfac | 0 | ufac,data=df[is.finite(df$l.soy),],na.action=na.omit)

mod.soy.oc.iv.f = felm(l.soy ~ 0 | ufac + yearfac | (pm.0km~uniton) | ufac,data=df_oldcoal[is.finite(df_oldcoal$l.soy),],na.action=na.omit)
mod.soy.nc.iv.f = felm(l.soy ~ 0 | ufac + yearfac | (pm.0km~uniton) | ufac,data=df_newcoal[is.finite(df_newcoal$l.soy),],na.action=na.omit)
mod.soy.ac.iv.f = felm(l.soy ~ 0 | ufac + yearfac | (pm.0km~uniton) | ufac,data=df_allcoal[is.finite(df_allcoal$l.soy),],na.action=na.omit)
mod.soy.ng.iv.f = felm(l.soy ~ 0 | ufac + yearfac | (pm.0km~uniton) | ufac,data=df_newng[is.finite(df_newng$l.soy),],na.action=na.omit)
mod.soy.ang.iv.f = felm(l.soy ~ 0 | ufac + yearfac | (pm.0km~uniton) | ufac,data=df_allng[is.finite(df_allng$l.soy),],na.action=na.omit)
mod.soy.a.iv.f = felm(l.soy ~ 0 | ufac + yearfac | (pm.0km~uniton:fueltype) | ufac,data=df[is.finite(df$l.soy),],na.action=na.omit)

mod.corn.oc.f = felm(l.corn ~ factor(uniton) | ufac + yearfac | 0 | ufac,data=df_oldcoal[is.finite(df_oldcoal$l.corn),],na.action=na.omit)
mod.corn.nc.f = felm(l.corn ~ uniton | ufac + yearfac | 0 | ufac,data=df_newcoal[is.finite(df_newcoal$l.corn),],na.action=na.omit)
mod.corn.ac.f = felm(l.corn ~ uniton | ufac + yearfac | 0 | ufac,data=df_allcoal[is.finite(df_allcoal$l.corn),],na.action=na.omit)
mod.corn.ng.f = felm(l.corn ~ uniton | ufac + yearfac | 0 | ufac,data=df_newng[is.finite(df_newng$l.corn),],na.action=na.omit)
mod.corn.ang.f = felm(l.corn ~ uniton | ufac + yearfac | 0 | ufac,data=df_allng[is.finite(df_allng$l.corn),],na.action=na.omit)
mod.corn.a.f = felm(l.corn ~ uniton:fueltype | ufac + yearfac | 0 | ufac,data=df[is.finite(df$l.corn),],na.action=na.omit)

mod.corn.oc.iv.f = felm(l.corn ~ 0 | ufac + yearfac | (pm.0km~uniton) | ufac,data=df_oldcoal[is.finite(df_oldcoal$l.corn),],na.action=na.omit)
mod.corn.nc.iv.f = felm(l.corn ~ 0 | ufac + yearfac | (pm.0km~uniton) | ufac,data=df_newcoal[is.finite(df_newcoal$l.corn),],na.action=na.omit)
mod.corn.ac.iv.f = felm(l.corn ~ 0 | ufac + yearfac | (pm.0km~uniton) | ufac,data=df_allcoal[is.finite(df_allcoal$l.corn),],na.action=na.omit)
mod.corn.ng.iv.f = felm(l.corn ~ 0 | ufac + yearfac | (pm.0km~uniton) | ufac,data=df_newng[is.finite(df_newng$l.corn),],na.action=na.omit)
mod.corn.ang.iv.f = felm(l.corn ~ 0 | ufac + yearfac | (pm.0km~uniton) | ufac,data=df_allng[is.finite(df_allng$l.corn),],na.action=na.omit)
mod.corn.a.iv.f = felm(l.corn ~ 0 | ufac + yearfac | (pm.0km~uniton:fueltype) | ufac,data=df[is.finite(df$l.corn),],na.action=na.omit)

# PM, Mort, Yields, and IV
stargazer(mod.pm.oc.f,mod.pm100.oc.f,mod.mort.oc.f,mod.mort.oc.iv.f,mod.corn.oc.f,mod.corn.oc.iv.f,mod.wheat.oc.f,mod.wheat.oc.iv.f,mod.soy.oc.f,mod.soy.oc.iv.f,
          out="Coaloff_Impacts_table.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
          out.header=T,dep.var.labels=rep("",10),
          covariate.labels=c("Unit On","Predicted PM$_{2.5}$"),
          column.labels=c("PM$_{2.5}$","PM$_{2.5}$ @ 100km","(log) Mortality","IV","(log) Corn Yield","IV","(log) Wheat Yield","IV","(log) Soy Yield","IV"),
          #notes=c("Odd-numbered models include standard errors clustered by location and year;","Even-numbered models have standard errors clustered by location.","For models 1-6, location is the unit, for models 7-8, location is grid cell."),
          notes.align="l"
)

stargazer(mod.pm.ng.f,mod.pm100.ng.f,mod.mort.ng.f,mod.mort.ng.iv.f,mod.corn.ng.f,mod.corn.ng.iv.f,mod.wheat.ng.f,mod.wheat.ng.iv.f,mod.soy.ng.f,mod.soy.ng.iv.f,
          out="NGon_Impacts_table.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
          out.header=T,dep.var.labels=rep("",10),
          covariate.labels=c("Unit On","Predicted PM$_{2.5}$"),
          column.labels=c("PM$_{2.5}$","PM$_{2.5}$ @ 100km","(log) Mortality","IV","(log) Corn Yield","IV","(log) Wheat Yield","IV","(log) Soy Yield","IV"),
          #notes=c("Odd-numbered models include standard errors clustered by location and year;","Even-numbered models have standard errors clustered by location.","For models 1-6, location is the unit, for models 7-8, location is grid cell."),
          notes.align="l"
)

stargazer(mod.pm.nc.f,mod.pm100.nc.f,mod.mort.nc.f,mod.mort.nc.iv.f,mod.corn.nc.f,mod.corn.nc.iv.f,mod.wheat.nc.f,mod.wheat.nc.iv.f,mod.soy.nc.f,mod.soy.nc.iv.f,
          out="Coalon_Impacts_table.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
          out.header=T,dep.var.labels=rep("",10),
          covariate.labels=c("Unit On","Predicted PM$_{2.5}$"),
          column.labels=c("PM$_{2.5}$","PM$_{2.5}$ @ 100km","(log) Mortality","IV","(log) Corn Yield","IV","(log) Wheat Yield","IV","(log) Soy Yield","IV"),
          #notes=c("Odd-numbered models include standard errors clustered by location and year;","Even-numbered models have standard errors clustered by location.","For models 1-6, location is the unit, for models 7-8, location is grid cell."),
          notes.align="l"
)

######################
### Location FE now ##
######################

mod.pm.oc.f = felm(pm.0km ~ factor(uniton) | locfac + yearfac | 0 | locfac,data=df_oldcoal[is.finite(df_oldcoal$pm.0km),],na.action=na.omit)
mod.pm.nc.f = felm(pm.0km ~ uniton | locfac + yearfac | 0 | locfac,data=df_newcoal[is.finite(df_newcoal$pm.0km),],na.action=na.omit)
mod.pm.ac.f = felm(pm.0km ~ uniton | locfac + yearfac | 0 | locfac,data=df_allcoal[is.finite(df_allcoal$pm.0km),],na.action=na.omit)
mod.pm.ng.f = felm(pm.0km ~ uniton | locfac + yearfac | 0 | locfac,data=df_newng[is.finite(df_newng$pm.0km),],na.action=na.omit)
mod.pm.ang.f = felm(pm.0km ~ uniton | locfac + yearfac | 0 | locfac,data=df_allng[is.finite(df_allng$pm.0km),],na.action=na.omit)
mod.pm.a.f = felm(pm.0km ~ uniton:fueltype | locfac + yearfac | 0 | locfac,data=df[is.finite(df$pm.0km),],na.action=na.omit)

mod.pm100.oc.f = felm(pm.100km ~ factor(uniton) | locfac + yearfac | 0 | locfac,data=df_oldcoal[is.finite(df_oldcoal$pm.100km),],na.action=na.omit)
mod.pm100.nc.f = felm(pm.100km ~ uniton | locfac + yearfac | 0 | locfac,data=df_newcoal[is.finite(df_newcoal$pm.100km),],na.action=na.omit)
mod.pm100.ac.f = felm(pm.100km ~ uniton | locfac + yearfac | 0 | locfac,data=df_allcoal[is.finite(df_allcoal$pm.100km),],na.action=na.omit)
mod.pm100.ng.f = felm(pm.100km ~ uniton | locfac + yearfac | 0 | locfac,data=df_newng[is.finite(df_newng$pm.100km),],na.action=na.omit)
mod.pm100.ang.f = felm(pm.100km ~ uniton | locfac + yearfac | 0 | locfac,data=df_allng[is.finite(df_allng$pm.100km),],na.action=na.omit)
mod.pm100.a.f = felm(pm.100km ~ uniton:fueltype | locfac + yearfac | 0 | locfac,data=df[is.finite(df$pm.100km),],na.action=na.omit)

mod.mort.oc.f = felm(l.Total.Crude.Rate ~ factor(uniton) | locfac + yearfac | 0 | locfac,data=df_oldcoal[is.finite(df_oldcoal$l.Total.Crude.Rate),],na.action=na.omit)
mod.mort.nc.f = felm(l.Total.Crude.Rate ~ uniton | locfac + yearfac | 0 | locfac,data=df_newcoal[is.finite(df_newcoal$l.Total.Crude.Rate),],na.action=na.omit)
mod.mort.ac.f = felm(l.Total.Crude.Rate ~ uniton | locfac + yearfac | 0 | locfac,data=df_allcoal[is.finite(df_allcoal$l.Total.Crude.Rate),],na.action=na.omit)
mod.mort.ng.f = felm(l.Total.Crude.Rate ~ uniton | locfac + yearfac | 0 | locfac,data=df_newng[is.finite(df_newng$l.Total.Crude.Rate),],na.action=na.omit)
mod.mort.ang.f = felm(l.Total.Crude.Rate ~ uniton | locfac + yearfac | 0 | locfac,data=df_allng[is.finite(df_allng$l.Total.Crude.Rate),],na.action=na.omit)
mod.mort.a.f = felm(l.Total.Crude.Rate ~ uniton:fueltype | locfac + yearfac | 0 | locfac,data=df[is.finite(df$l.Total.Crude.Rate),],na.action=na.omit)

mod.mort.oc.iv.f = felm(l.Total.Crude.Rate ~ 0 | locfac + yearfac | (pm.0km~uniton) | locfac,data=df_oldcoal[is.finite(df_oldcoal$l.Total.Crude.Rate),],na.action=na.omit)
mod.mort.nc.iv.f = felm(l.Total.Crude.Rate ~ 0 | locfac + yearfac | (pm.0km~uniton) | locfac,data=df_newcoal[is.finite(df_newcoal$l.Total.Crude.Rate),],na.action=na.omit)
mod.mort.ac.iv.f = felm(l.Total.Crude.Rate ~ 0 | locfac + yearfac | (pm.0km~uniton) | locfac,data=df_allcoal[is.finite(df_allcoal$l.Total.Crude.Rate),],na.action=na.omit)
mod.mort.ng.iv.f = felm(l.Total.Crude.Rate ~ 0 | locfac + yearfac | (pm.0km~uniton) | locfac,data=df_newng[is.finite(df_newng$l.Total.Crude.Rate),],na.action=na.omit)
mod.mort.ang.iv.f = felm(l.Total.Crude.Rate ~ 0 | locfac + yearfac | (pm.0km~uniton) | locfac,data=df_allng[is.finite(df_allng$l.Total.Crude.Rate),],na.action=na.omit)
mod.mort.a.iv.f = felm(l.Total.Crude.Rate ~ 0 | locfac + yearfac | (pm.0km~uniton:fueltype) | locfac,data=df[is.finite(df$l.Total.Crude.Rate),],na.action=na.omit)

mod.wheat.oc.f = felm(l.w.wheat ~ factor(uniton) | locfac + yearfac | 0 | locfac,data=df_oldcoal[is.finite(df_oldcoal$l.w.wheat),],na.action=na.omit)
mod.wheat.nc.f = felm(l.w.wheat ~ uniton | locfac + yearfac | 0 | locfac,data=df_newcoal[is.finite(df_newcoal$l.w.wheat),],na.action=na.omit)
mod.wheat.ac.f = felm(l.w.wheat ~ uniton | locfac + yearfac | 0 | locfac,data=df_allcoal[is.finite(df_allcoal$l.w.wheat),],na.action=na.omit)
mod.wheat.ng.f = felm(l.w.wheat ~ uniton | locfac + yearfac | 0 | locfac,data=df_newng[is.finite(df_newng$l.w.wheat),],na.action=na.omit)
mod.wheat.ang.f = felm(l.w.wheat ~ uniton | locfac + yearfac | 0 | locfac,data=df_allng[is.finite(df_allng$l.w.wheat),],na.action=na.omit)
mod.wheat.a.f = felm(l.w.wheat ~ uniton:fueltype | locfac + yearfac | 0 | locfac,data=df[is.finite(df$l.w.wheat),],na.action=na.omit)

mod.wheat.oc.iv.f = felm(l.w.wheat ~ 0 | locfac + yearfac | (pm.0km~uniton) | locfac,data=df_oldcoal[is.finite(df_oldcoal$l.w.wheat),],na.action=na.omit)
mod.wheat.nc.iv.f = felm(l.w.wheat ~ 0 | locfac + yearfac | (pm.0km~uniton) | locfac,data=df_newcoal[is.finite(df_newcoal$l.w.wheat),],na.action=na.omit)
mod.wheat.ac.iv.f = felm(l.w.wheat ~ 0 | locfac + yearfac | (pm.0km~uniton) | locfac,data=df_allcoal[is.finite(df_allcoal$l.w.wheat),],na.action=na.omit)
mod.wheat.ng.iv.f = felm(l.w.wheat ~ 0 | locfac + yearfac | (pm.0km~uniton) | locfac,data=df_newng[is.finite(df_newng$l.w.wheat),],na.action=na.omit)
mod.wheat.ang.iv.f = felm(l.w.wheat ~ 0 | locfac + yearfac | (pm.0km~uniton) | locfac,data=df_allng[is.finite(df_allng$l.w.wheat),],na.action=na.omit)
mod.wheat.a.iv.f = felm(l.w.wheat ~ 0 | locfac + yearfac | (pm.0km~uniton:fueltype) | locfac,data=df[is.finite(df$l.w.wheat),],na.action=na.omit)

mod.soy.oc.f = felm(l.soy ~ factor(uniton) | locfac + yearfac | 0 | locfac,data=df_oldcoal[is.finite(df_oldcoal$l.soy),],na.action=na.omit)
mod.soy.nc.f = felm(l.soy ~ uniton | locfac + yearfac | 0 | locfac,data=df_newcoal[is.finite(df_newcoal$l.soy),],na.action=na.omit)
mod.soy.ac.f = felm(l.soy ~ uniton | locfac + yearfac | 0 | locfac,data=df_allcoal[is.finite(df_allcoal$l.soy),],na.action=na.omit)
mod.soy.ng.f = felm(l.soy ~ uniton | locfac + yearfac | 0 | locfac,data=df_newng[is.finite(df_newng$l.soy),],na.action=na.omit)
mod.soy.ang.f = felm(l.soy ~ uniton | locfac + yearfac | 0 | locfac,data=df_allng[is.finite(df_allng$l.soy),],na.action=na.omit)
mod.soy.a.f = felm(l.soy ~ uniton:fueltype | locfac + yearfac | 0 | locfac,data=df[is.finite(df$l.soy),],na.action=na.omit)

mod.soy.oc.iv.f = felm(l.soy ~ 0 | locfac + yearfac | (pm.0km~uniton) | locfac,data=df_oldcoal[is.finite(df_oldcoal$l.soy),],na.action=na.omit)
mod.soy.nc.iv.f = felm(l.soy ~ 0 | locfac + yearfac | (pm.0km~uniton) | locfac,data=df_newcoal[is.finite(df_newcoal$l.soy),],na.action=na.omit)
mod.soy.ac.iv.f = felm(l.soy ~ 0 | locfac + yearfac | (pm.0km~uniton) | locfac,data=df_allcoal[is.finite(df_allcoal$l.soy),],na.action=na.omit)
mod.soy.ng.iv.f = felm(l.soy ~ 0 | locfac + yearfac | (pm.0km~uniton) | locfac,data=df_newng[is.finite(df_newng$l.soy),],na.action=na.omit)
mod.soy.ang.iv.f = felm(l.soy ~ 0 | locfac + yearfac | (pm.0km~uniton) | locfac,data=df_allng[is.finite(df_allng$l.soy),],na.action=na.omit)
mod.soy.a.iv.f = felm(l.soy ~ 0 | locfac + yearfac | (pm.0km~uniton:fueltype) | locfac,data=df[is.finite(df$l.soy),],na.action=na.omit)

mod.corn.oc.f = felm(l.corn ~ factor(uniton) | locfac + yearfac | 0 | locfac,data=df_oldcoal[is.finite(df_oldcoal$l.corn),],na.action=na.omit)
mod.corn.nc.f = felm(l.corn ~ uniton | locfac + yearfac | 0 | locfac,data=df_newcoal[is.finite(df_newcoal$l.corn),],na.action=na.omit)
mod.corn.ac.f = felm(l.corn ~ uniton | locfac + yearfac | 0 | locfac,data=df_allcoal[is.finite(df_allcoal$l.corn),],na.action=na.omit)
mod.corn.ng.f = felm(l.corn ~ uniton | locfac + yearfac | 0 | locfac,data=df_newng[is.finite(df_newng$l.corn),],na.action=na.omit)
mod.corn.ang.f = felm(l.corn ~ uniton | locfac + yearfac | 0 | locfac,data=df_allng[is.finite(df_allng$l.corn),],na.action=na.omit)
mod.corn.a.f = felm(l.corn ~ uniton:fueltype | locfac + yearfac | 0 | locfac,data=df[is.finite(df$l.corn),],na.action=na.omit)

mod.corn.oc.iv.f = felm(l.corn ~ 0 | locfac + yearfac | (pm.0km~uniton) | locfac,data=df_oldcoal[is.finite(df_oldcoal$l.corn),],na.action=na.omit)
mod.corn.nc.iv.f = felm(l.corn ~ 0 | locfac + yearfac | (pm.0km~uniton) | locfac,data=df_newcoal[is.finite(df_newcoal$l.corn),],na.action=na.omit)
mod.corn.ac.iv.f = felm(l.corn ~ 0 | locfac + yearfac | (pm.0km~uniton) | locfac,data=df_allcoal[is.finite(df_allcoal$l.corn),],na.action=na.omit)
mod.corn.ng.iv.f = felm(l.corn ~ 0 | locfac + yearfac | (pm.0km~uniton) | locfac,data=df_newng[is.finite(df_newng$l.corn),],na.action=na.omit)
mod.corn.ang.iv.f = felm(l.corn ~ 0 | locfac + yearfac | (pm.0km~uniton) | locfac,data=df_allng[is.finite(df_allng$l.corn),],na.action=na.omit)
mod.corn.a.iv.f = felm(l.corn ~ 0 | locfac + yearfac | (pm.0km~uniton:fueltype) | locfac,data=df[is.finite(df$l.corn),],na.action=na.omit)

# PM, Mort, Yields, and IV
stargazer(mod.pm.oc.f,mod.pm100.oc.f,mod.mort.oc.f,mod.mort.oc.iv.f,mod.corn.oc.f,mod.corn.oc.iv.f,mod.wheat.oc.f,mod.wheat.oc.iv.f,mod.soy.oc.f,mod.soy.oc.iv.f,
          out="Coaloff_Impacts_table_LOC.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
          out.header=T,dep.var.labels=rep("",10),
          covariate.labels=c("Unit On","Predicted PM$_{2.5}$"),
          column.labels=c("PM$_{2.5}$","PM$_{2.5}$ @ 100km","(log) Mortality","IV","(log) Corn Yield","IV","(log) Wheat Yield","IV","(log) Soy Yield","IV"),
          #notes=c("Odd-numbered models include standard errors clustered by location and year;","Even-numbered models have standard errors clustered by location.","For models 1-6, location is the unit, for models 7-8, location is grid cell."),
          notes.align="l"
)

stargazer(mod.pm.ng.f,mod.pm100.ng.f,mod.mort.ng.f,mod.mort.ng.iv.f,mod.corn.ng.f,mod.corn.ng.iv.f,mod.wheat.ng.f,mod.wheat.ng.iv.f,mod.soy.ng.f,mod.soy.ng.iv.f,
          out="NGon_Impacts_table_LOC.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
          out.header=T,dep.var.labels=rep("",10),
          covariate.labels=c("Unit On","Predicted PM$_{2.5}$"),
          column.labels=c("PM$_{2.5}$","PM$_{2.5}$ @ 100km","(log) Mortality","IV","(log) Corn Yield","IV","(log) Wheat Yield","IV","(log) Soy Yield","IV"),
          #notes=c("Odd-numbered models include standard errors clustered by location and year;","Even-numbered models have standard errors clustered by location.","For models 1-6, location is the unit, for models 7-8, location is grid cell."),
          notes.align="l"
)

stargazer(mod.pm.nc.f,mod.pm100.nc.f,mod.mort.nc.f,mod.mort.nc.iv.f,mod.corn.nc.f,mod.corn.nc.iv.f,mod.wheat.nc.f,mod.wheat.nc.iv.f,mod.soy.nc.f,mod.soy.nc.iv.f,
          out="Coalon_Impacts_table_LOC.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
          out.header=T,dep.var.labels=rep("",10),
          covariate.labels=c("Unit On","Predicted PM$_{2.5}$"),
          column.labels=c("PM$_{2.5}$","PM$_{2.5}$ @ 100km","(log) Mortality","IV","(log) Corn Yield","IV","(log) Wheat Yield","IV","(log) Soy Yield","IV"),
          #notes=c("Odd-numbered models include standard errors clustered by location and year;","Even-numbered models have standard errors clustered by location.","For models 1-6, location is the unit, for models 7-8, location is grid cell."),
          notes.align="l"
)

###################################################################
# -----------------------------------------------------------------
# Tables of impacts, Tau / FDL (not included for now)
# -----------------------------------------------------------------
mod.pm.tau.oc = felm(pm.0km ~ factor(tau_off) | ufac+yearfac | 0 | ufac,data=df_oldcoal[abs(df_oldcoal$tau_off)<4,],na.action=na.omit)
mod.pm.tau.oc2 = felm(pm.0km ~ factor(tau_off) | ufac+yearfac | 0 | ufac,data=df_oldcoal[abs(df_oldcoal$tau_off)<5,],na.action=na.omit)

# felm giving error with clustering so have to use lm for unit level
mod.pm.tau.oc = lm(pm.0km ~ factor(tau_off)+ufac+yearfac,data=df_oldcoal[abs(df_oldcoal$tau_off)<4,],na.action=na.omit)
mod.pm.tau.oc2 = lm(pm.0km ~ factor(tau_off)+ufac+yearfac,data=df_oldcoal[abs(df_oldcoal$tau_off)<5,],na.action=na.omit)
mod.pm.tau.oc.l = felm(pm.0km ~ factor(tau_off) | locfac+yearfac | 0 | locfac,data=df_oldcoal[abs(df_oldcoal$tau_off)<4,],na.action=na.omit)
mod.pm.tau.oc.l2 = felm(pm.0km ~ factor(tau_off) | locfac+yearfac | 0 | locfac,data=df_oldcoal[abs(df_oldcoal$tau_off)<5,],na.action=na.omit)
mod.pm.tau.oc.fonly = felm(pm.0km ~ factor(tau_off) | ufac | 0 | ufac,data=df_oldcoal[abs(df_oldcoal$tau_off)<4,],na.action=na.omit)
mod.pm.tau.oc.fonly2 = felm(pm.0km ~ factor(tau_off) | ufac | 0 | ufac,data=df_oldcoal[abs(df_oldcoal$tau_off)<5,],na.action=na.omit)
mod.pm.tau.oc.lonly = felm(pm.0km ~ factor(tau_off) | locfac | 0 | locfac,data=df_oldcoal[abs(df_oldcoal$tau_off)<4,],na.action=na.omit)
mod.pm.tau.oc.lonly2 = felm(pm.0km ~ factor(tau_off) | locfac | 0 | locfac,data=df_oldcoal[abs(df_oldcoal$tau_off)<5,],na.action=na.omit)

stargazer(mod.pm.tau.oc,mod.pm.tau.oc2,mod.pm.tau.oc.l,mod.pm.tau.oc.l2,mod.pm.tau.oc.fonly,mod.pm.tau.oc.fonly2,mod.pm.tau.oc.lonly,mod.pm.tau.oc.lonly2,
          out="Coaloff_TAU_pm_table.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
          out.header=T,dep.var.labels="",
          covariate.labels=paste(-3:4),
          notes.align="l"
)

# felm giving error with clustering so have to use lm for unit level
mod.o3.tau.oc = lm(epa.o3.0km ~ factor(tau_off)+ufac+yearfac,data=df_oldcoal[abs(df_oldcoal$tau_off)<4,],na.action=na.omit)
mod.o3.tau.oc2 = lm(epa.o3.0km ~ factor(tau_off)+ufac+yearfac,data=df_oldcoal[abs(df_oldcoal$tau_off)<5,],na.action=na.omit)
mod.o3.tau.oc.l = felm(epa.o3.0km ~ factor(tau_off) | locfac+yearfac | 0 | locfac,data=df_oldcoal[abs(df_oldcoal$tau_off)<4,],na.action=na.omit)
mod.o3.tau.oc.l2 = felm(epa.o3.0km ~ factor(tau_off) | locfac+yearfac | 0 | locfac,data=df_oldcoal[abs(df_oldcoal$tau_off)<5,],na.action=na.omit)
mod.o3.tau.oc.fonly = felm(epa.o3.0km ~ factor(tau_off) | ufac | 0 | ufac,data=df_oldcoal[abs(df_oldcoal$tau_off)<4,],na.action=na.omit)
mod.o3.tau.oc.fonly2 = felm(epa.o3.0km ~ factor(tau_off) | ufac | 0 | ufac,data=df_oldcoal[abs(df_oldcoal$tau_off)<5,],na.action=na.omit)
mod.o3.tau.oc.lonly = felm(epa.o3.0km ~ factor(tau_off) | locfac | 0 | locfac,data=df_oldcoal[abs(df_oldcoal$tau_off)<4,],na.action=na.omit)
mod.o3.tau.oc.lonly2 = felm(epa.o3.0km ~ factor(tau_off) | locfac | 0 | locfac,data=df_oldcoal[abs(df_oldcoal$tau_off)<5,],na.action=na.omit)

stargazer(mod.o3.tau.oc,mod.o3.tau.oc2,mod.o3.tau.oc.l,mod.o3.tau.oc.l2,mod.o3.tau.oc.fonly,mod.o3.tau.oc.fonly2,mod.o3.tau.oc.lonly,mod.o3.tau.oc.lonly2,
          out="Coaloff_TAU_O3_table.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
          out.header=T,dep.var.labels="",
          covariate.labels=paste(-3:4),
          notes.align="l"
)

##### NG ##### 
mod.pm.tau.ng = lm(pm.0km ~ factor(tau_on)+ufac+yearfac,data=df_newng[abs(df_newng$tau_on)<4,],na.action=na.omit)
mod.pm.tau.ng2 = lm(pm.0km ~ factor(tau_on)+ufac+yearfac,data=df_newng[abs(df_newng$tau_on)<5,],na.action=na.omit)
mod.pm.tau.ng.l = felm(pm.0km ~ factor(tau_on) | locfac+yearfac | 0 | locfac,data=df_newng[abs(df_newng$tau_on)<4,],na.action=na.omit)
mod.pm.tau.ng.l2 = felm(pm.0km ~ factor(tau_on) | locfac+yearfac | 0 | locfac,data=df_newng[abs(df_newng$tau_on)<5,],na.action=na.omit)
mod.pm.tau.ng.fonly = felm(pm.0km ~ factor(tau_on) | ufac | 0 | ufac,data=df_newng[abs(df_newng$tau_on)<4,],na.action=na.omit)
mod.pm.tau.ng.fonly2 = felm(pm.0km ~ factor(tau_on) | ufac | 0 | ufac,data=df_newng[abs(df_newng$tau_on)<5,],na.action=na.omit)
mod.pm.tau.ng.lonly = felm(pm.0km ~ factor(tau_on) | locfac | 0 | locfac,data=df_newng[abs(df_newng$tau_on)<4,],na.action=na.omit)
mod.pm.tau.ng.lonly2 = felm(pm.0km ~ factor(tau_on) | locfac | 0 | locfac,data=df_newng[abs(df_newng$tau_on)<5,],na.action=na.omit)

stargazer(mod.pm.tau.ng,mod.pm.tau.ng2,mod.pm.tau.ng.l,mod.pm.tau.ng.l2,mod.pm.tau.ng.fonly,mod.pm.tau.ng.fonly2,mod.pm.tau.ng.lonly,mod.pm.tau.ng.lonly2,
          out="NGon_TAU_pm_table.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
          out.header=T,dep.var.labels="",
          covariate.labels=paste(-3:4),
          notes.align="l"
)

# felm giving error with clustering so have to use lm for unit level
mod.o3.tau.ng = lm(omi.o3.0km ~ factor(tau_on)+ufac+yearfac,data=df_newng[abs(df_newng$tau_on)<4,],na.action=na.omit)
mod.o3.tau.ng2 = lm(omi.o3.0km ~ factor(tau_on)+ufac+yearfac,data=df_newng[abs(df_newng$tau_on)<5,],na.action=na.omit)
mod.o3.tau.ng.l = felm(omi.o3.0km ~ factor(tau_on) | locfac+yearfac | 0 | locfac,data=df_newng[abs(df_newng$tau_on)<4,],na.action=na.omit)
mod.o3.tau.ng.l2 = felm(omi.o3.0km ~ factor(tau_on) | locfac+yearfac | 0 | locfac,data=df_newng[abs(df_newng$tau_on)<5,],na.action=na.omit)
mod.o3.tau.ng.fonly = felm(omi.o3.0km ~ factor(tau_on) | ufac | 0 | ufac,data=df_newng[abs(df_newng$tau_on)<4,],na.action=na.omit)
mod.o3.tau.ng.fonly2 = felm(omi.o3.0km ~ factor(tau_on) | ufac | 0 | ufac,data=df_newng[abs(df_newng$tau_on)<5,],na.action=na.omit)
mod.o3.tau.ng.lonly = felm(omi.o3.0km ~ factor(tau_on) | locfac | 0 | locfac,data=df_newng[abs(df_newng$tau_on)<4,],na.action=na.omit)
mod.o3.tau.ng.lonly2 = felm(omi.o3.0km ~ factor(tau_on) | locfac | 0 | locfac,data=df_newng[abs(df_newng$tau_on)<5,],na.action=na.omit)

stargazer(mod.o3.tau.ng,mod.o3.tau.ng2,mod.o3.tau.ng.l,mod.o3.tau.ng.l2,mod.o3.tau.ng.fonly,mod.o3.tau.ng.fonly2,mod.o3.tau.ng.lonly,mod.o3.tau.ng.lonly2,
          out="NGon_TAU_O3_table.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
          out.header=T,dep.var.labels="",
          covariate.labels=paste(-3:4),
          notes.align="l"
)


# ############# Finite Distributed Lag Models ################
# # felm giving error with clustering so have to use lm for unit level
# mod.pm.fdl.oc = lm(pm.0km ~ uniton_lead3+uniton_lead2+uniton_lead1+uniton+uniton_lag1+uniton_lag2+uniton_lag3+ufac+yearfac,data=df_oldcoal,na.action=na.omit)
# mod.pm.fdl.oc.l = felm(pm.0km ~ uniton_lead3+uniton_lead2+uniton_lead1+uniton+uniton_lag1+uniton_lag2+uniton_lag3 | locfac+yearfac | 0 | locfac,data=df_oldcoal,na.action=na.omit)
# mod.pm.fdl.oc.fonly = felm(pm.0km ~ uniton_lead3+uniton_lead2+uniton_lead1+uniton+uniton_lag1+uniton_lag2+uniton_lag3 | ufac | 0 | ufac,data=df_oldcoal,na.action=na.omit)
# mod.pm.fdl.oc.lonly = felm(pm.0km ~ uniton_lead3+uniton_lead2+uniton_lead1+uniton+uniton_lag1+uniton_lag2+uniton_lag3 | locfac | 0 | locfac,data=df_oldcoal,na.action=na.omit)
# 
# mod.o3.fdl.oc = lm(omi.o3.0km ~ uniton_lead3+uniton_lead2+uniton_lead1+uniton+uniton_lag1+uniton_lag2+uniton_lag3+ufac+yearfac,data=df_oldcoal,na.action=na.omit)
# mod.o3.fdl.oc.l = felm(omi.o3.0km ~ uniton_lead3+uniton_lead2+uniton_lead1+uniton+uniton_lag1+uniton_lag2+uniton_lag3 | locfac+yearfac | 0 | locfac,data=df_oldcoal,na.action=na.omit)
# mod.o3.fdl.oc.fonly = felm(omi.o3.0km ~ uniton_lead3+uniton_lead2+uniton_lead1+uniton+uniton_lag1+uniton_lag2+uniton_lag3 | ufac | 0 | ufac,data=df_oldcoal,na.action=na.omit)
# mod.o3.fdl.oc.lonly = felm(omi.o3.0km ~ uniton_lead3+uniton_lead2+uniton_lead1+uniton+uniton_lag1+uniton_lag2+uniton_lag3 | locfac | 0 | locfac,data=df_oldcoal,na.action=na.omit)
# 
# stargazer(mod.pm.fdl.oc,mod.pm.fdl.oc.l,mod.pm.fdl.oc.fonly,mod.pm.fdl.oc.lonly,mod.o3.fdl.oc,mod.o3.fdl.oc.l,mod.o3.fdl.oc.fonly,mod.o3.fdl.oc.lonly,
#           out="Coaloff_FDL_pm_and_o3_table.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
#           out.header=T,dep.var.labels="",
#           covariate.labels=paste(-3:4),
#           notes.align="l"
# )
# 

setwd("../")


