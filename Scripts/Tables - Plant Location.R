# -------------------------------------------------------------------------------------------------------------------
# Impacts Tables - Plant Location
# -------------------------------------------------------------------------------------------------------------------

setwd("Tables/")

# -----------------------------------------------------------------
# Data subsets for plant-level analysis
# -----------------------------------------------------------------

df = fullpanel.plants.annual
df$yearfac = as.factor(df$year)
df$locfac = as.factor(df$locationid)
df$ufac = as.factor(df$facunitid)

df_oldcoal = subset(df,df$oldcoalplant==1,select=c(uniton,grossload,facunitid,locationid,County,year,yearfac,locfac,ufac,pm.0km,omi.o3.0km,so2.0km,no2.0km,modis.aod.0km,ssa.0km,epa.pm.0km,epa.o3.0km))
df_newcoal = subset(df,df$newcoalplant==1,select=c(uniton,grossload,facunitid,locationid,County,year,yearfac,locfac,ufac,pm.0km,omi.o3.0km,so2.0km,no2.0km,modis.aod.0km,ssa.0km,epa.pm.0km,epa.o3.0km))
df_allcoal = subset(df,df$fueltype=="Coal",select=c(uniton,grossload,facunitid,locationid,County,year,yearfac,locfac,ufac,pm.0km,omi.o3.0km,so2.0km,no2.0km,modis.aod.0km,ssa.0km,epa.pm.0km,epa.o3.0km))
df_newng = subset(df,df$newngplant==1,select=c(uniton,grossload,facunitid,locationid,County,year,yearfac,locfac,ufac,pm.0km,omi.o3.0km,so2.0km,no2.0km,modis.aod.0km,ssa.0km,epa.pm.0km,epa.o3.0km))
df_allng = subset(df,df$fueltype=="Natural Gas",select=c(uniton,grossload,facunitid,locationid,County,year,yearfac,locfac,ufac,pm.0km,omi.o3.0km,so2.0km,no2.0km,modis.aod.0km,ssa.0km,epa.pm.0km,epa.o3.0km))
df_ap = subset(fullpanel.allpoints.annual.0p25,select=c(uniton,grossload,facunitid,locationid,County,year,pm.0km,omi.o3.0km,so2.0km,locationid2,fueltype,epa.pm.0km,epa.o3.0km))
df_ap2 = subset(fullpanel.allpoints.annual.0p125,select=c(uniton,grossload,facunitid,locationid,County,year,no2.0km,locationid2,fueltype))

# -----------------------------------------------------------------
# Tables of Coal Impacts on different parameters, RD, unit FE
# -----------------------------------------------------------------

# Particulate Matter, Coal off
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
          out="TableS3a_PM_CoalOff.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
          out.header=T,dep.var.caption="Particulate Matter Concentration [ug/m3]",dep.var.labels="",
          covariate.labels=c("Unit On","Unit On * Coal","Unit On * Natural Gas","Unit On * Other"),
          column.labels=c("Old Coal Units","New Coal Units","All Coal Units","All Units","All Locations (0.25x0.25)"),column.separate=c(2,2,2,2,2),
          notes=c("Odd-numbered models include standard errors clustered by location and year;","Even-numbered models have standard errors clustered by location.","For models 1-8, location is the unit, for models 9-10, location is grid cell."),
          notes.align="l"
)

# Ozone, Coal off
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
          out="TableS4a_O3_CoalOff.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
          out.header=T,dep.var.caption="Ozone Thickness [DU]",dep.var.labels="",
          covariate.labels=c("Unit On","Unit On * Coal","Unit On * Natural Gas","Unit On * Other"),
          column.labels=c("Old Coal Units","New Coal Units","All Coal Units","All Units","All Locations (0.25x0.25)"),column.separate=c(2,2,2,2,2),
          notes=c("Odd-numbered models include standard errors clustered by location and year;","Even-numbered models have standard errors clustered by location.","For models 1-8, location is the unit, for models 9-10, location is grid cell."),
          notes.align="l"
)

# NO2, Coal off
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
          out="TableS6a_NO2_CoalOff.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
          out.header=T,dep.var.caption="NO2 Thickness [DU]",dep.var.labels="",
          covariate.labels=c("Unit On","Unit On * Coal","Unit On * Natural Gas","Unit On * Other"),
          column.labels=c("Old Coal Units","New Coal Units","All Coal Units","All Units","All Locations (0.25x0.25)"),column.separate=c(2,2,2,2,2),
          notes=c("Odd-numbered models include standard errors clustered by location and year;","Even-numbered models have standard errors clustered by location.","For models 1-8, location is the unit, for models 9-10, location is grid cell."),
          notes.align="l"
)

# SO2, Coal off
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
          out="TableS5a_SO2_CoalOff.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
          out.header=T,dep.var.caption="SO2 Thickness [DU]",dep.var.labels="",
          covariate.labels=c("Unit On","Unit On * Coal","Unit On * Natural Gas","Unit On * Other"),
          column.labels=c("Old Coal Units","New Coal Units","All Coal Units","All Units","All Locations (0.25x0.25)"),column.separate=c(2,2,2,2,2),
          notes=c("Odd-numbered models include standard errors clustered by location and year;","Even-numbered models have standard errors clustered by location.","For models 1-8, location is the unit, for models 9-10, location is grid cell."),
          notes.align="l"
)

# -----------------------------------------------------------------
# INTENSITY
# -----------------------------------------------------------------
# -----------------------------------------------------------------
# Tables of Coal Impacts on different parameters, RD, unit FE
# -----------------------------------------------------------------

mega <- function(x) (x * 1e6)

# PM, Coal off
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
          out="TableS3b_PM_CoalOffIntensity.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
          out.header=T,dep.var.caption="Particulate Matter Concentration [ug/m3]",dep.var.labels="",
          covariate.labels=c("Gross Load","Gross Load * Coal","Gross Load * Natural Gas","Gross Load * Other"),
          column.labels=c("Old Coal Units","New Coal Units","All Coal Units","All Units","All Locations (0.25x0.25)"),column.separate=c(2,2,2,2,2),
          notes=c("Odd-numbered models include standard errors clustered by location and year;","Even-numbered models have standard errors clustered by location.","For models 1-8, location is the unit, for models 9-10, location is grid cell."),
          notes.align="l",apply.coef=mega,apply.se=mega
)

# O3, Coal off
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
          out="TableS4b_O3_CoalOffIntensity.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
          out.header=T,dep.var.caption="Surface Ozone Concentration [ppb]",dep.var.labels="",
          covariate.labels=c("Gross Load","Gross Load * Coal","Gross Load * Natural Gas","Gross Load * Other"),
          column.labels=c("Old Coal Units","New Coal Units","All Coal Units","All Units","All Locations (0.25x0.25)"),column.separate=c(2,2,2,2,2),
          notes=c("Odd-numbered models include standard errors clustered by location and year;","Even-numbered models have standard errors clustered by location.","For models 1-8, location is the unit, for models 9-10, location is grid cell."),
          notes.align="l",apply.coef=mega,apply.se=mega
)

# NO2, Coal off
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
          out="TableS6b_NO2_CoalOffIntensity.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
          out.header=T,dep.var.caption="NO2 Thickness [DU]",dep.var.labels="",
          covariate.labels=c("Gross Load","Gross Load * Coal","Gross Load * Natural Gas","Gross Load * Other"),
          column.labels=c("Old Coal Units","New Coal Units","All Coal Units","All Units","All Locations (0.25x0.25)"),column.separate=c(2,2,2,2,2),
          notes=c("Odd-numbered models include standard errors clustered by location and year;","Even-numbered models have standard errors clustered by location.","For models 1-8, location is the unit, for models 9-10, location is grid cell."),
          notes.align="l",apply.coef=mega,apply.se=mega
)

# SO2, Coal off
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
          out="TableS5b_SO2_CoalOffIntensity.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
          out.header=T,dep.var.caption="SO2 Thickness [DU]",dep.var.labels="",
          covariate.labels=c("Gross Load","Gross Load * Coal","Gross Load * Natural Gas","Gross Load * Other"),
          column.labels=c("Old Coal Units","New Coal Units","All Coal Units","All Units","All Locations (0.25x0.25)"),column.separate=c(2,2,2,2,2),
          notes=c("Odd-numbered models include standard errors clustered by location and year;","Even-numbered models have standard errors clustered by location.","For models 1-8, location is the unit, for models 9-10, location is grid cell."),
          notes.align="l",apply.coef=mega,apply.se=mega
)

# -----------------------------------------------------------------
# Tables of Impacts on outcomes, different RD, IV specs - Table S2
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

mod.pm.oc.f = felm(pm.0km ~ uniton | locfac + yearfac | 0 | locfac,data=df_oldcoal[is.finite(df_oldcoal$pm.0km),],na.action=na.omit)
mod.pm.nc.f = felm(pm.0km ~ uniton | locfac + yearfac | 0 | locfac,data=df_newcoal[is.finite(df_newcoal$pm.0km),],na.action=na.omit)
mod.pm.ac.f = felm(pm.0km ~ uniton | locfac + yearfac | 0 | locfac,data=df_allcoal[is.finite(df_allcoal$pm.0km),],na.action=na.omit)
mod.pm.ng.f = felm(pm.0km ~ uniton | locfac + yearfac | 0 | locfac,data=df_newng[is.finite(df_newng$pm.0km),],na.action=na.omit)
mod.pm.ang.f = felm(pm.0km ~ uniton | locfac + yearfac | 0 | locfac,data=df_allng[is.finite(df_allng$pm.0km),],na.action=na.omit)
mod.pm.a.f = felm(pm.0km ~ uniton:fueltype | locfac + yearfac | 0 | locfac,data=df[is.finite(df$pm.0km),],na.action=na.omit)

mod.pm100.oc.f = felm(pm.100km ~ uniton | locfac + yearfac | 0 | locfac,data=df_oldcoal[is.finite(df_oldcoal$pm.100km),],na.action=na.omit)
mod.pm100.nc.f = felm(pm.100km ~ uniton | locfac + yearfac | 0 | locfac,data=df_newcoal[is.finite(df_newcoal$pm.100km),],na.action=na.omit)
mod.pm100.ac.f = felm(pm.100km ~ uniton | locfac + yearfac | 0 | locfac,data=df_allcoal[is.finite(df_allcoal$pm.100km),],na.action=na.omit)
mod.pm100.ng.f = felm(pm.100km ~ uniton | locfac + yearfac | 0 | locfac,data=df_newng[is.finite(df_newng$pm.100km),],na.action=na.omit)
mod.pm100.ang.f = felm(pm.100km ~ uniton | locfac + yearfac | 0 | locfac,data=df_allng[is.finite(df_allng$pm.100km),],na.action=na.omit)
mod.pm100.a.f = felm(pm.100km ~ uniton:fueltype | locfac + yearfac | 0 | locfac,data=df[is.finite(df$pm.100km),],na.action=na.omit)

mod.mort.oc.f = felm(l.Total.Crude.Rate ~ uniton | locfac + yearfac | 0 | locfac,data=df_oldcoal[is.finite(df_oldcoal$l.Total.Crude.Rate),],na.action=na.omit)
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

mod.wheat.oc.f = felm(l.w.wheat ~ uniton | locfac + yearfac | 0 | locfac,data=df_oldcoal[is.finite(df_oldcoal$l.w.wheat),],na.action=na.omit)
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

mod.soy.oc.f = felm(l.soy ~ uniton | locfac + yearfac | 0 | locfac,data=df_oldcoal[is.finite(df_oldcoal$l.soy),],na.action=na.omit)
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

mod.corn.oc.f = felm(l.corn ~ uniton | locfac + yearfac | 0 | locfac,data=df_oldcoal[is.finite(df_oldcoal$l.corn),],na.action=na.omit)
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
          out="TableS2a_CoalOff.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
          out.header=T,dep.var.labels=rep("",10),
          covariate.labels=c("Unit On","Predicted PM$_{2.5}$"),
          column.labels=c("PM$_{2.5}$","PM$_{2.5}$ @ 100km","(log) Mortality","IV","(log) Corn Yield","IV","(log) Wheat Yield","IV","(log) Soy Yield","IV"),
          #notes=c("Odd-numbered models include standard errors clustered by location and year;","Even-numbered models have standard errors clustered by location.","For models 1-6, location is the unit, for models 7-8, location is grid cell."),
          notes.align="l"
)

stargazer(mod.pm.ng.f,mod.pm100.ng.f,mod.mort.ng.f,mod.mort.ng.iv.f,mod.corn.ng.f,mod.corn.ng.iv.f,mod.wheat.ng.f,mod.wheat.ng.iv.f,mod.soy.ng.f,mod.soy.ng.iv.f,
          out="TableS2b_NgOn.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
          out.header=T,dep.var.labels=rep("",10),
          covariate.labels=c("Unit On","Predicted PM$_{2.5}$"),
          column.labels=c("PM$_{2.5}$","PM$_{2.5}$ @ 100km","(log) Mortality","IV","(log) Corn Yield","IV","(log) Wheat Yield","IV","(log) Soy Yield","IV"),
          #notes=c("Odd-numbered models include standard errors clustered by location and year;","Even-numbered models have standard errors clustered by location.","For models 1-6, location is the unit, for models 7-8, location is grid cell."),
          notes.align="l"
)

stargazer(mod.pm.nc.f,mod.pm100.nc.f,mod.mort.nc.f,mod.mort.nc.iv.f,mod.corn.nc.f,mod.corn.nc.iv.f,mod.wheat.nc.f,mod.wheat.nc.iv.f,mod.soy.nc.f,mod.soy.nc.iv.f,
          out="TableS2c_CoalOn.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
          out.header=T,dep.var.labels=rep("",10),
          covariate.labels=c("Unit On","Predicted PM$_{2.5}$"),
          column.labels=c("PM$_{2.5}$","PM$_{2.5}$ @ 100km","(log) Mortality","IV","(log) Corn Yield","IV","(log) Wheat Yield","IV","(log) Soy Yield","IV"),
          #notes=c("Odd-numbered models include standard errors clustered by location and year;","Even-numbered models have standard errors clustered by location.","For models 1-6, location is the unit, for models 7-8, location is grid cell."),
          notes.align="l"
)

# -----------------------------------------------------------------
# Tables of impacts, Tau - Table S1
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
          out="TableS1_CoalOff_Tau.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
          out.header=T,dep.var.labels="",
          covariate.labels=paste(-3:4),
          notes.align="l"
)

setwd("../")


