# -----------------------------------------------------------------------
# Plots - Pollution Impacts.R
# -----------------------------------------------------------------------

# -----------------------------------------------------------------------
# Function to Load Plant-Level Results
# -----------------------------------------------------------------------

yrs = 2005:2016
nyears=3
sig = 1.64

# Function
source("Analysis/Analysis - Functions.R")

# -----------------------------------------------------------------------
# Load Results for Tau plots - simple demeaned before/after event
# -----------------------------------------------------------------------

# Data
load("Data/L2 Processed Data/County Environmental Data Extracted from Rasters.Rdata")

# Results sets (hold coefficients)
load("Analysis/Plant Level Results.Rdata")
load("Analysis/County Level Results.Rdata")

# Load Location-level data
load("Analysis/Tau Analysis LOCATION - Annual - unit FE/PlantType/offcoal/Analysis_Tau_Annual_Location_PlantType_offcoal.Rdata")
  pm5.coal = getdf2plot(pm.5km,nyears,1,sig)
  pm100.coal = getdf2plot(pm.100km,nyears,1,sig)
  pmsurf25.coal = getdf2plot(epa.pm.25km,nyears,1,sig)
  pmsurf100.coal = getdf2plot(epa.pm.100km,nyears,1,sig)
  ozone25.coal = getdf2plot(epa.o3.25km,nyears,1,sig)
  ozone100.coal = getdf2plot(epa.o3.100km,nyears,1,sig)
  nox25.coal = getdf2plot(no2.25km,nyears,1,sig)
  nox100.coal = getdf2plot(no2.100km,nyears,1,sig)
  sox25.coal = getdf2plot(so2.25km,nyears,1,sig)
  sox100.coal = getdf2plot(so2.100km,nyears,1,sig)

load("Analysis/Tau Analysis LOCATION - Annual - unit FE/PlantType/onng/Analysis_Tau_Annual_Location_PlantType_onng.Rdata")
  pm5.ng = getdf2plot(pm.5km,nyears,1,sig)
  pm100.ng = getdf2plot(pm.100km,nyears,1,sig)
  pmsurf25.coal = getdf2plot(epa.pm.25km,nyears,1,sig)
  pmsurf100.coal = getdf2plot(epa.pm.100km,nyears,1,sig)
  ozone25.ng = getdf2plot(epa.o3.25km,nyears,1,sig)
  ozone100.ng = getdf2plot(epa.o3.100km,nyears,1,sig)
  nox25.ng = getdf2plot(no2.25km,nyears,1,sig)
  nox100.ng = getdf2plot(no2.100km,nyears,1,sig)
  sox25.ng = getdf2plot(so2.25km,nyears,1,sig)
  sox100.ng = getdf2plot(so2.100km,nyears,1,sig)

load("Analysis/Tau Analysis LOCATION - Annual - unit FE/PlantType/oncoal/Analysis_Tau_Annual_Location_PlantType_oncoal.Rdata")
  pm5.newcoal = getdf2plot(pm.5km,nyears,1,sig)
  pm100.newcoal = getdf2plot(pm.100km,nyears,1,sig)
  pmsurf25.coal = getdf2plot(epa.pm.25km,nyears,1,sig)
  pmsurf100.coal = getdf2plot(epa.pm.100km,nyears,1,sig)
  ozone25.newcoal = getdf2plot(epa.o3.25km,nyears,1,sig)
  ozone100.newcoal = getdf2plot(epa.o3.100km,nyears,1,sig)
  nox25.newcoal = getdf2plot(no2.25km,nyears,1,sig)
  nox100.newcoal = getdf2plot(no2.100km,nyears,1,sig)
  sox25.newcoal = getdf2plot(so2.25km,nyears,1,sig)
  sox100.newcoal = getdf2plot(so2.100km,nyears,1,sig)


# -----------------------------------------------------------------------
# PM2.5 Tau Plots
# -----------------------------------------------------------------------
  
ylims = pmylims = c(-1,1)
nudge = 0.05

pdf(file="Plots/Chemistry/CoalOff_PM2.5_Impacts_LocationFE.pdf",width=10,height=6)
  par(mar=c(5,8,1,1),las=1)
  plot(-10,-10,xlim=c(-nyears,nyears-1),ylim=pmylims,xlab="Leads / Lags [years]",ylab="",cex.lab=2,cex.axis=2)
  title(ylab=expression(paste(Delta," ",PM[2.5]," Concentration [",mu,"g/",m^3,"]")),line=5,las=0,cex.lab=2)
  polygon(x=c(-5.5,-5.5,-0.5,-0.5),y=c(-200,1.2*ylims[2],1.2*ylims[2],-200),col="grey80")
  grid(col="darkgrey")
  abline(h=0,col="black",lwd=2,lty=2)
  segments(x0=pm5.coal$tt-nudge,y0=pm5.coal$lb,y1=pm5.coal$ub,lty=1,lwd=1,col="black")
  segments(x0=pm5.coal$tt-nudge-0.05,x1=pm5.coal$tt-nudge+0.05,y0=pm5.coal$lb,lty=1,lwd=1,col="black")
  segments(x0=pm5.coal$tt-nudge-0.05,x1=pm5.coal$tt-nudge+0.05,y0=pm5.coal$ub,lty=1,lwd=1,col="black")
  segments(x0=pm100.coal$tt+nudge,y0=pm100.coal$lb,y1=pm100.coal$ub,lty=1,col="black")
  segments(x0=pm100.coal$tt+nudge-0.05,x1=pm100.coal$tt+nudge+0.05,y0=pm100.coal$lb,lty=1,col="black")
  segments(x0=pm100.coal$tt+nudge-0.05,x1=pm100.coal$tt+nudge+0.05,y0=pm100.coal$ub,lty=1,col="black")
  points(pm100.coal$tt+nudge,pm100.coal$pt,pch=21,cex=2.5,bg="yellow4")
  points(pm5.coal$tt-nudge,pm5.coal$pt,pch=21,cex=1,bg="yellow4")
  text(-1,0.9,"Ramp",col="black",cex=2)
  text(-1,0.75,"Down",col="black",cex=2)
  text(-0.5,0.9,"Old Unit Off",pos=4,col="black",cex=2)
  arrows(x0=-0.5,y0=0.825,x1=1.5,y1=0.825,col="black",lwd=2,angle=30,length=0.1,code=2)
  box(which="plot",lty=1,lwd=2,col="black")
dev.off()

pdf(file="Plots/Chemistry/NGOn_PM2.5_Impacts_LocationFE.pdf",width=10,height=6)
  par(mar=c(5,8,1,1),las=1)
  plot(-10,-10,xlim=c(-nyears,nyears-1),ylim=pmylims,xlab="Leads / Lags [years]",ylab="",cex.lab=2,cex.axis=2)
  title(ylab=expression(paste(Delta," ",PM[2.5]," Concentration [",mu,"g/",m^3,"]")),line=5,las=0,cex.lab=2)
  polygon(x=c(-5.5,-5.5,-0.5,-0.5),y=c(-200,1.2*ylims[2],1.2*ylims[2],-200),col="grey80")
  grid(col="darkgrey")
  abline(h=0,col="black",lwd=2,lty=2)
  segments(x0=pm5.ng$tt-nudge,y0=pm5.ng$lb,y1=pm5.ng$ub,lty=1,lwd=1,col="black")
  segments(x0=pm5.ng$tt-nudge-0.05,x1=pm5.ng$tt-nudge+0.05,y0=pm5.ng$lb,lty=1,lwd=1,col="black")
  segments(x0=pm5.ng$tt-nudge-0.05,x1=pm5.ng$tt-nudge+0.05,y0=pm5.ng$ub,lty=1,lwd=1,col="black")
  segments(x0=pm100.ng$tt+nudge,y0=pm100.ng$lb,y1=pm100.ng$ub,lty=1,col="black")
  segments(x0=pm100.ng$tt+nudge-0.05,x1=pm100.ng$tt+nudge+0.05,y0=pm100.ng$lb,lty=1,col="black")
  segments(x0=pm100.ng$tt+nudge-0.05,x1=pm100.ng$tt+nudge+0.05,y0=pm100.ng$ub,lty=1,col="black")
  points(pm100.ng$tt+nudge,pm100.ng$pt,pch=21,cex=2.5,bg="yellow4")
  points(pm5.ng$tt-nudge,pm5.ng$pt,pch=21,cex=1,bg="yellow4")
  #text(-1,0.9,"Ramp",col="black",cex=2)
  #text(-1,0.75,"Down",col="black",cex=2)
  text(-0.5,0.9,"New Unit On",pos=4,col="black",cex=2)
  arrows(x0=-0.5,y0=0.825,x1=1.5,y1=0.825,col="black",lwd=2,angle=30,length=0.1,code=2)
  box(which="plot",lty=1,lwd=2,col="black")
dev.off()

# -----------------------------------------------------------------------
# O3 Surf Tau Plots
# -----------------------------------------------------------------------

ylims = c(-2,2)

pdf(file="Plots/Chemistry/CoalOff_O3_Impacts_LocationFE.pdf",width=10,height=6)
  par(mar=c(5,8,1,1),las=1)
  plot(-10,-10,xlim=c(-nyears,nyears-1),ylim=ylims,xlab="Leads / Lags [years]",ylab="",cex.lab=2,cex.axis=2)
  title(ylab=expression(paste(Delta," ",O[3]," Concentration [ppb]")),line=5,las=0,cex.lab=2)
  polygon(x=c(-5.5,-5.5,-0.5,-0.5),y=c(-200,2*ylims[2],2*ylims[2],-200),col="grey80")
  grid(col="darkgrey")
  abline(h=0,col="black",lwd=2,lty=2)
  segments(x0=ozone25.coal$tt-nudge,y0=ozone25.coal$lb,y1=ozone25.coal$ub,lty=1,lwd=1,col="black")
  segments(x0=ozone25.coal$tt-nudge-0.05,x1=ozone25.coal$tt-nudge+0.05,y0=ozone25.coal$lb,lty=1,lwd=1,col="black")
  segments(x0=ozone25.coal$tt-nudge-0.05,x1=ozone25.coal$tt-nudge+0.05,y0=ozone25.coal$ub,lty=1,lwd=1,col="black")
  segments(x0=ozone100.coal$tt+nudge,y0=ozone100.coal$lb,y1=ozone100.coal$ub,lty=1,col="black")
  segments(x0=ozone100.coal$tt+nudge-0.05,x1=ozone100.coal$tt+nudge+0.05,y0=ozone100.coal$lb,lty=1,col="black")
  segments(x0=ozone100.coal$tt+nudge-0.05,x1=ozone100.coal$tt+nudge+0.05,y0=ozone100.coal$ub,lty=1,col="black")
  points(ozone100.coal$tt+nudge,ozone100.coal$pt,pch=21,cex=2.5,bg="tan")
  points(ozone25.coal$tt-nudge,ozone25.coal$pt,pch=21,cex=1,bg="tan")
  text(-1,1.9,"Ramp",col="black",cex=2)
  text(-1,1.7,"Down",col="black",cex=2)
  text(-0.5,1.9,"Old Unit Off",pos=4,col="black",cex=2)
  arrows(x0=-0.5,y0=1.7,x1=1.5,y1=1.7,col="black",lwd=2,angle=30,length=0.1,code=2)
  box(which="plot",lty=1,lwd=3,col="black")
dev.off()

pdf(file="Plots/Chemistry/NGOn_O3_Impacts_LocationFE.pdf",width=10,height=6)
  par(mar=c(5,8,1,1),las=1)
  plot(-10,-10,xlim=c(-nyears,nyears-1),ylim=ylims,xlab="Leads / Lags [years]",ylab="",cex.lab=2,cex.axis=2)
  title(ylab=expression(paste(Delta," ",O[3]," Concentration [ppb]")),line=5,las=0,cex.lab=2)
  grid(col="darkgrey")
  polygon(x=c(-1.5,-1.5,-0.5,-0.5),y=c(-200,1.2*ylims[2],1.2*ylims[2],-200),col="grey80")
  abline(h=0,col="black",lwd=2,lty=2)
  segments(x0=ozone25.ng$tt-nudge,y0=ozone25.ng$lb,y1=ozone25.ng$ub,lty=1,lwd=1,col="black")
  segments(x0=ozone25.ng$tt-nudge-0.05,x1=ozone25.ng$tt-nudge+0.05,y0=ozone25.ng$lb,lty=1,lwd=1,col="black")
  segments(x0=ozone25.ng$tt-nudge-0.05,x1=ozone25.ng$tt-nudge+0.05,y0=ozone25.ng$ub,lty=1,lwd=1,col="black")
  segments(x0=ozone100.ng$tt+nudge,y0=ozone100.ng$lb,y1=ozone100.ng$ub,lty=1,col="black")
  segments(x0=ozone100.ng$tt+nudge-0.05,x1=ozone100.ng$tt+nudge+0.05,y0=ozone100.ng$lb,lty=1,col="black")
  segments(x0=ozone100.ng$tt+nudge-0.05,x1=ozone100.ng$tt+nudge+0.05,y0=ozone100.ng$ub,lty=1,col="black")
  points(ozone100.ng$tt+nudge,ozone100.ng$pt,pch=21,cex=2.5,bg="tan")
  points(ozone25.ng$tt-nudge,ozone25.ng$pt,pch=21,cex=1,bg="tan")
  text(-0.5,1,"New Unit On",pos=4,col="black",cex=2)
  arrows(x0=-0.5,y0=1.7,x1=1.5,y1=1.7,col="black",lwd=2,angle=30,length=0.1,code=2)
  box(which="plot",lty=1,lwd=3,col="black")
dev.off()

# -----------------------------------------------------------------------
# SO2 PBL Tau Plots
# -----------------------------------------------------------------------

ylims = c(-0.05,0.05)

pdf(file="Plots/Chemistry/CoalOff_SO2_Impacts_LocationFE.pdf",width=10,height=6)
  par(mar=c(5,8,1,1),las=1)
  plot(-10,-10,xlim=c(-nyears,nyears-1),ylim=ylims,xlab="Leads / Lags [years]",ylab="",cex.lab=2,cex.axis=2)
  title(ylab=expression(paste(Delta," ",SO[2]," [DU]")),line=5,las=0,cex.lab=2)
  polygon(x=c(-5.5,-5.5,-0.5,-0.5),y=c(-200,2*ylims[2],2*ylims[2],-200),col="grey80")
  grid(col="darkgrey")
  abline(h=0,col="black",lwd=2,lty=2)
  segments(x0=sox25.coal$tt-nudge,y0=sox25.coal$lb,y1=sox25.coal$ub,lty=1,lwd=1,col="black")
  segments(x0=sox25.coal$tt-nudge-0.05,x1=sox25.coal$tt-nudge+0.05,y0=sox25.coal$lb,lty=1,lwd=1,col="black")
  segments(x0=sox25.coal$tt-nudge-0.05,x1=sox25.coal$tt-nudge+0.05,y0=sox25.coal$ub,lty=1,lwd=1,col="black")
  segments(x0=sox100.coal$tt+nudge,y0=sox100.coal$lb,y1=sox100.coal$ub,lty=1,col="black")
  segments(x0=sox100.coal$tt+nudge-0.05,x1=sox100.coal$tt+nudge+0.05,y0=sox100.coal$lb,lty=1,col="black")
  segments(x0=sox100.coal$tt+nudge-0.05,x1=sox100.coal$tt+nudge+0.05,y0=sox100.coal$ub,lty=1,col="black")
  points(sox100.coal$tt+nudge,sox100.coal$pt,pch=21,cex=2.5,bg="darkorange")
  points(sox25.coal$tt-nudge,sox25.coal$pt,pch=21,cex=1,bg="darkorange")
  box(which="plot",lty=1,lwd=3,col="black")
dev.off()

pdf(file="Plots/Chemistry/NGOn_SO2_Impacts_LocationFE.pdf",width=10,height=6)
  par(mar=c(5,8,1,1),las=1)
  plot(-10,-10,xlim=c(-nyears,nyears-1),ylim=ylims,xlab="Leads / Lags [years]",ylab="",cex.lab=2,cex.axis=2)
  title(ylab=expression(paste(Delta," ",SO[2]," [DU]")),line=5,las=0,cex.lab=2)
  grid(col="darkgrey")
  polygon(x=c(-1.5,-1.5,-0.5,-0.5),y=c(-200,1.2*ylims[2],1.2*ylims[2],-200),col="grey80")
  abline(h=0,col="black",lwd=2,lty=2)
  segments(x0=sox25.ng$tt-nudge,y0=sox25.ng$lb,y1=sox25.ng$ub,lty=1,lwd=1,col="black")
  segments(x0=sox25.ng$tt-nudge-0.05,x1=sox25.ng$tt-nudge+0.05,y0=sox25.ng$lb,lty=1,lwd=1,col="black")
  segments(x0=sox25.ng$tt-nudge-0.05,x1=sox25.ng$tt-nudge+0.05,y0=sox25.ng$ub,lty=1,lwd=1,col="black")
  segments(x0=sox100.ng$tt+nudge,y0=sox100.ng$lb,y1=sox100.ng$ub,lty=1,col="black")
  segments(x0=sox100.ng$tt+nudge-0.05,x1=sox100.ng$tt+nudge+0.05,y0=sox100.ng$lb,lty=1,col="black")
  segments(x0=sox100.ng$tt+nudge-0.05,x1=sox100.ng$tt+nudge+0.05,y0=sox100.ng$ub,lty=1,col="black")
  points(sox100.ng$tt+nudge,sox100.ng$pt,pch=21,cex=2.5,bg="darkorange")
  points(sox25.ng$tt-nudge,sox25.ng$pt,pch=21,cex=1,bg="darkorange")
  box(which="plot",lty=1,lwd=3,col="black")
dev.off()

# -----------------------------------------------------------------------
# NO2 Tau Plots
# -----------------------------------------------------------------------

ylims = c(-100,100)

pdf(file="Plots/Chemistry/CoalOff_NO2_Impacts_LocationFE.pdf",width=10,height=6)
  par(mar=c(5,8,1,1),las=1)
  plot(-10,-10,xlim=c(-nyears,nyears-1),ylim=ylims,xlab="Leads / Lags [years]",ylab="",cex.lab=2,cex.axis=2)
  title(ylab=expression(paste(Delta," ",NO[2]," [DU]")),line=5,las=0,cex.lab=2)
  polygon(x=c(-5.5,-5.5,-0.5,-0.5),y=c(-200,2*ylims[2],2*ylims[2],-200),col="grey80")
  grid(col="darkgrey")
  abline(h=0,col="black",lwd=2,lty=2)
  segments(x0=nox25.coal$tt-nudge,y0=nox25.coal$lb,y1=nox25.coal$ub,lty=1,lwd=1,col="black")
  segments(x0=nox25.coal$tt-nudge-0.05,x1=nox25.coal$tt-nudge+0.05,y0=nox25.coal$lb,lty=1,lwd=1,col="black")
  segments(x0=nox25.coal$tt-nudge-0.05,x1=nox25.coal$tt-nudge+0.05,y0=nox25.coal$ub,lty=1,lwd=1,col="black")
  segments(x0=nox100.coal$tt+nudge,y0=nox100.coal$lb,y1=nox100.coal$ub,lty=1,col="black")
  segments(x0=nox100.coal$tt+nudge-0.05,x1=nox100.coal$tt+nudge+0.05,y0=nox100.coal$lb,lty=1,col="black")
  segments(x0=nox100.coal$tt+nudge-0.05,x1=nox100.coal$tt+nudge+0.05,y0=nox100.coal$ub,lty=1,col="black")
  points(nox100.coal$tt+nudge,nox100.coal$pt,pch=21,cex=2.5,bg="brown")
  points(nox25.coal$tt-nudge,nox25.coal$pt,pch=21,cex=1,bg="brown")
  box(which="plot",lty=1,lwd=3,col="black")
dev.off()

pdf(file="Plots/Chemistry/NGOn_NO2_Impacts_LocationFE.pdf",width=10,height=6)
  par(mar=c(5,8,1,1),las=1)
  plot(-10,-10,xlim=c(-nyears,nyears-1),ylim=ylims,xlab="Leads / Lags [years]",ylab="",cex.lab=2,cex.axis=2)
  title(ylab=expression(paste(Delta," ",NO[2]," [DU]")),line=5,las=0,cex.lab=2)
  grid(col="darkgrey")
  polygon(x=c(-1.5,-1.5,-0.5,-0.5),y=c(-200,1.2*ylims[2],1.2*ylims[2],-200),col="grey80")
  abline(h=0,col="black",lwd=2,lty=2)
  segments(x0=nox25.ng$tt-nudge,y0=nox25.ng$lb,y1=nox25.ng$ub,lty=1,lwd=1,col="black")
  segments(x0=nox25.ng$tt-nudge-0.05,x1=nox25.ng$tt-nudge+0.05,y0=nox25.ng$lb,lty=1,lwd=1,col="black")
  segments(x0=nox25.ng$tt-nudge-0.05,x1=nox25.ng$tt-nudge+0.05,y0=nox25.ng$ub,lty=1,lwd=1,col="black")
  segments(x0=nox100.ng$tt+nudge,y0=nox100.ng$lb,y1=nox100.ng$ub,lty=1,col="black")
  segments(x0=nox100.ng$tt+nudge-0.05,x1=nox100.ng$tt+nudge+0.05,y0=nox100.ng$lb,lty=1,col="black")
  segments(x0=nox100.ng$tt+nudge-0.05,x1=nox100.ng$tt+nudge+0.05,y0=nox100.ng$ub,lty=1,col="black")
  points(nox100.ng$tt+nudge,nox100.ng$pt,pch=21,cex=2.5,bg="brown")
  points(nox25.ng$tt-nudge,nox25.ng$pt,pch=21,cex=1,bg="brown")
  box(which="plot",lty=1,lwd=3,col="black")
dev.off()

# -----------------------------------------------------------------------
# Distance Plots
# -----------------------------------------------------------------------

load(file="Data/Final Data Sets for Analysis/FullPanel_PlantLocations_Annual.Rdata")

names(fullpanel.plants.annual)
mean_conc = fullpanel.plants.annual %>% group_by(fueltype,uniton) %>% summarise(pm.0km=mean(pm.0km,na.rm=T),
                                                                                pm.1km=mean(pm.1km,na.rm=T),
                                                                                pm.5km=mean(pm.5km,na.rm=T),
                                                                                pm.10km=mean(pm.10km,na.rm=T),
                                                                                pm.25km=mean(pm.25km,na.rm=T),
                                                                                pm.50km=mean(pm.50km,na.rm=T),
                                                                                pm.100km=mean(pm.100km,na.rm=T),
                                                                                epa.o3.0km=mean(epa.o3.0km,na.rm=T),
                                                                                epa.o3.25km=mean(epa.o3.25km,na.rm=T),
                                                                                epa.o3.50km=mean(epa.o3.50km,na.rm=T),
                                                                                epa.o3.75km=mean(epa.o3.75km,na.rm=T),
                                                                                epa.o3.100km=mean(epa.o3.100km,na.rm=T),
                                                                                no2.0km=mean(no2.0km,na.rm=T),
                                                                                no2.25km=mean(no2.25km,na.rm=T),
                                                                                no2.50km=mean(no2.50km,na.rm=T),
                                                                                no2.75km=mean(no2.75km,na.rm=T),
                                                                                no2.100km=mean(no2.100km,na.rm=T),
                                                                                so2.0km=mean(so2.0km,na.rm=T),
                                                                                so2.25km=mean(so2.25km,na.rm=T),
                                                                                so2.50km=mean(so2.50km,na.rm=T),
                                                                                so2.75km=mean(so2.75km,na.rm=T),
                                                                                so2.100km=mean(so2.100km,na.rm=T),
                                                                                pmsurf.0km=mean(epa.pm.0km,na.rm=T),
                                                                                pmsurf.25km=mean(epa.pm.0km,na.rm=T),
                                                                                pmsurf.50km=mean(epa.pm.0km,na.rm=T),
                                                                                pmsurf.75km=mean(epa.pm.0km,na.rm=T),
                                                                                pmsurf.100km=mean(epa.pm.0km,na.rm=T)
)                                                                                


sd_conc = fullpanel.plants.annual %>% group_by(fueltype,uniton) %>% summarise(sd.pm.0km=sd(pm.0km,na.rm=T),
                                                                              sd.pm.1km=sd(pm.1km,na.rm=T),
                                                                              sd.pm.5km=sd(pm.5km,na.rm=T),
                                                                              sd.pm.10km=sd(pm.10km,na.rm=T),
                                                                              sd.pm.25km=sd(pm.25km,na.rm=T),
                                                                              sd.pm.50km=sd(pm.50km,na.rm=T),
                                                                              sd.pm.100km=sd(pm.100km,na.rm=T),
                                                                              sd.epa.o3.0km=sd(epa.o3.0km,na.rm=T),
                                                                              sd.epa.o3.25km=sd(epa.o3.25km,na.rm=T),
                                                                              sd.epa.o3.50km=sd(epa.o3.50km,na.rm=T),
                                                                              sd.epa.o3.75km=sd(epa.o3.75km,na.rm=T),
                                                                              sd.epa.o3.100km=sd(epa.o3.100km,na.rm=T),
                                                                              sd.no2.0km=sd(no2.0km,na.rm=T),
                                                                              sd.no2.25km=sd(no2.25km,na.rm=T),
                                                                              sd.no2.50km=sd(no2.50km,na.rm=T),
                                                                              sd.no2.75km=sd(no2.75km,na.rm=T),
                                                                              sd.no2.100km=sd(no2.100km,na.rm=T),
                                                                              sd.so2.0km=sd(so2.0km,na.rm=T),
                                                                              sd.so2.25km=sd(so2.25km,na.rm=T),
                                                                              sd.so2.50km=sd(so2.50km,na.rm=T),
                                                                              sd.so2.75km=sd(so2.75km,na.rm=T),
                                                                              sd.so2.100km=sd(so2.100km,na.rm=T),
                                                                              sd.pmsurf.0km=mean(epa.pm.0km,na.rm=T),
                                                                              sd.pmsurf.25km=mean(epa.pm.0km,na.rm=T),
                                                                              sd.pmsurf.50km=mean(epa.pm.0km,na.rm=T),
                                                                              sd.pmsurf.75km=mean(epa.pm.0km,na.rm=T),
                                                                              sd.pmsurf.100km=mean(epa.pm.0km,na.rm=T)
                                                                              )

n_conc = fullpanel.plants.annual %>% group_by(fueltype,uniton) %>% summarise(n())

# convert to annuli
radii = c(1,5,10,25,50,100)
as = pi*radii^2
d_as = as[2:6]-as[1:5]

xx1a = c(1,5,10,25,50,100) - 0.5
xx1 = c(1,5,10,25,50,100)
xx1b = c(1,5,10,25,50,100) + 0.5

xx2a = c(0,25,50,75,100) - 0.5
xx2 = c(0,25,50,75,100)
xx2b = c(0,25,50,75,100) + 0.5

# PM over distance
#png(width=6,height=6,units="in",res=300,pointsize=12,file="Plots/Chemistry/PM_Distance.png")
pdf(width=6,height=6,pointsize=12,file="Plots/Chemistry/PM_Distance.pdf")
  par(mar=c(6,8,1,1),las=1)
  plot(xx1,xx1-1000,ylim=c(8,12),ylab="",xlab="Distance [km]",cex.lab=2,cex.axis=1.5)
  mtext(side=2,text=expression(paste(PM[2.5]," Concentration [",mu,"g/",m^3,"]")),cex=2,las=0,line=4)
  grid(col="darkgrey")
  
  # coal
  yy = mean_conc[2,4:9]
  yy = c(yy[1],(yy[2:6]*radii[2:6]^2-yy[1:5]*radii[1:5]^2)/(radii[2:6]^2-radii[1:5]^2))
  yy_u = as.matrix(yy+1.96*sd_conc[2,4:9]/sqrt(12))
  yy_l = as.matrix(yy-1.96*sd_conc[2,4:9]/sqrt(12))
  segments(x0=xx1a,y0=yy_l,x1=xx1a,y1=yy_u,col="black")
  points(xx1a,yy,col="black",bg="red",pch=21,cex=2)
  lines(xx1a,yy,col="red",lty=1)
  
  # ng
  yy = mean_conc[4,4:9]
  yy = c(yy[1],(yy[2:6]*radii[2:6]^2-yy[1:5]*radii[1:5]^2)/(radii[2:6]^2-radii[1:5]^2))
  yy_u = as.matrix(yy+1.96*sd_conc[4,4:9]/sqrt(12))
  yy_l = as.matrix(yy-1.96*sd_conc[4,4:9]/sqrt(12))
  segments(x0=xx1,y0=yy_l,x1=xx1,y1=yy_u,col="black")
  points(xx1,yy,col="black",bg="blue",pch=21,cex=2)
  lines(xx1,yy,col="blue",lty=1)
  
  # other
  yy = mean_conc[6,4:9]
  yy = c(yy[1],(yy[2:6]*radii[2:6]^2-yy[1:5]*radii[1:5]^2)/(radii[2:6]^2-radii[1:5]^2))
  yy_u = as.matrix(yy+1.96*sd_conc[6,4:9]/sqrt(12))
  yy_l = as.matrix(yy-1.96*sd_conc[6,4:9]/sqrt(12))
  segments(x0=xx1b,y0=yy_l,x1=xx1b,y1=yy_u,col="black")
  points(xx1b,yy,col="black",bg="orange",pch=21,cex=2)
  lines(xx1b,yy,col="orange",lty=1)
  
  legend("topright",inset=0.01,bty="n",pch=21,pt.bg=c("red","blue","orange"),legend=c("Coal","Natural Gas","Other"),pt.cex=2)
  box(which="plot",lwd=2)
dev.off()

# SurfacePM over distance
#png(width=6,height=6,units="in",res=300,pointsize=12,file="Plots/Chemistry/PMSurf_Distance.png")
pdf(width=6,height=6,pointsize=12,file="Plots/Chemistry/PMSurf_Distance.pdf")
  par(mar=c(6,8,1,1),las=1)
  plot(xx1,xx1-1000,ylim=c(4,18),ylab="",xlab="Distance [km]",cex.lab=2,cex.axis=1.5)
  mtext(side=2,text=expression(paste(PM[2.5]," Concentration [",mu,"g/",m^3,"]")),cex=2,las=0,line=4)
  grid(col="darkgrey")
  
  # coal
  yy = mean_conc[2,25:29]
  yy = c(yy[1],(yy[2:5]*radii[2:5]^2-yy[1:4]*radii[1:4]^2)/(radii[2:5]^2-radii[1:4]^2))
  yy_u = as.matrix(yy+1.96*sd_conc[2,25:29]/sqrt(12))
  yy_l = as.matrix(yy-1.96*sd_conc[2,25:29]/sqrt(12))
  segments(x0=xx2a,y0=yy_l,x1=xx2a,y1=yy_u,col="black")
  points(xx2a,yy,col="black",bg="red",pch=22,cex=2)
  lines(xx2a,yy,col="red",lty=1,lwd=2)
  
  # ng
  yy = mean_conc[4,25:29]
  yy = c(yy[1],(yy[2:5]*radii[2:5]^2-yy[1:4]*radii[1:4]^2)/(radii[2:5]^2-radii[1:4]^2))
  yy_u = as.matrix(yy+1.96*sd_conc[4,25:29]/sqrt(12))
  yy_l = as.matrix(yy-1.96*sd_conc[4,25:29]/sqrt(12))
  segments(x0=xx2,y0=yy_l,x1=xx2,y1=yy_u,col="black")
  points(xx2,yy,col="black",bg="blue",pch=22,cex=2)
  lines(xx2,yy,col="blue",lty=1,lwd=2)
  
  # other
  yy = mean_conc[6,25:29]
  yy = c(yy[1],(yy[2:5]*radii[2:5]^2-yy[1:4]*radii[1:4]^2)/(radii[2:5]^2-radii[1:4]^2))
  yy_u = as.matrix(yy+1.96*sd_conc[6,25:29]/sqrt(12))
  yy_l = as.matrix(yy-1.96*sd_conc[6,25:29]/sqrt(12))
  segments(x0=xx2b,y0=yy_l,x1=xx2b,y1=yy_u,col="black")
  points(xx2b,yy,col="black",bg="orange",pch=22,cex=2)
  lines(xx2b,yy,col="orange",lty=1,lwd=2)
  
  legend("topright",inset=0.01,bty="n",pch=21,pt.bg=c("red","blue","orange"),legend=c("Coal","Natural Gas","Other"),pt.cex=2)
  box(which="plot",lwd=2)
dev.off()

# O3 over distance
# ylim=c(290,320)
png(width=8,height=6,units="in",res=300,pointsize=12,file="Plots/Chemistry/O3_Distance.png",type="cairo")
  par(mar=c(6,8,1,1),las=1)
  plot(xx2,xx2-1000,xlim=c(0,100),ylim=c(35,45),ylab="",xlab="Distance [km]",cex.lab=2,cex.axis=1.5)
  #mtext(side=2,text=expression(paste(O[3]," Concentration [DU]")),cex=2,las=0,line=4)
  mtext(side=2,text=expression(paste(O[3]," Concentration [ppb]")),cex=2,las=0,line=4)
  grid(col="darkgrey")
  
  # coal
  yy = mean_conc[2,10:14]
  yy = c(yy[1],(yy[2:5]*radii[2:5]^2-yy[1:4]*radii[1:4]^2)/(radii[2:5]^2-radii[1:4]^2))
  yy_u = as.matrix(yy+1.96*sd_conc[2,10:14]/sqrt(12))
  yy_l = as.matrix(yy-1.96*sd_conc[2,10:14]/sqrt(12))
  segments(x0=xx2a,y0=yy_l,x1=xx2a,y1=yy_u,col="black")
  points(xx2a,yy,col="black",bg="red",pch=22,cex=2)
  lines(xx2a,yy,col="red",lty=1,lwd=2)
  
  # ng
  yy = mean_conc[4,10:14]
  yy = c(yy[1],(yy[2:5]*radii[2:5]^2-yy[1:4]*radii[1:4]^2)/(radii[2:5]^2-radii[1:4]^2))
  yy_u = as.matrix(yy+1.96*sd_conc[4,10:14]/sqrt(12))
  yy_l = as.matrix(yy-1.96*sd_conc[4,10:14]/sqrt(12))
  segments(x0=xx2,y0=yy_l,x1=xx2,y1=yy_u,col="black")
  points(xx2,yy,col="black",bg="blue",pch=22,cex=2)
  lines(xx2,yy,col="blue",lty=1,lwd=2)
  
  # other
  yy = mean_conc[6,10:14]
  yy = c(yy[1],(yy[2:5]*radii[2:5]^2-yy[1:4]*radii[1:4]^2)/(radii[2:5]^2-radii[1:4]^2))
  yy_u = as.matrix(yy+1.96*sd_conc[6,10:14]/sqrt(12))
  yy_l = as.matrix(yy-1.96*sd_conc[6,10:14]/sqrt(12))
  segments(x0=xx2b,y0=yy_l,x1=xx2b,y1=yy_u,col="black")
  points(xx2b,yy,col="black",bg="orange",pch=22,cex=2)
  lines(xx2b,yy,col="orange",lty=1,lwd=2)
  
  legend("topright",inset=0.01,bty="n",fill=c("red","blue","orange"),legend=c("Coal","Natural Gas","Other"))
  box(which="plot",lwd=2)
dev.off()

# NO2 over distance
png(width=8,height=6,units="in",res=300,pointsize=12,file="Plots/Chemistry/NO2_Distance.png",type="cairo")
  par(mar=c(6,8,1,1),las=1)
  plot(xx2,xx2-1000,xlim=c(0,100),ylim=c(200,700),ylab="",xlab="Distance [km]",cex.lab=2,cex.axis=1.5)
  mtext(side=2,text=expression(paste(NO[2]," Concentration [DU]")),cex=2,las=0,line=4)
  grid(col="darkgrey")
  
  # coal
  yy = mean_conc[2,15:19]
  yy = c(yy[1],(yy[2:5]*radii[2:5]^2-yy[1:4]*radii[1:4]^2)/(radii[2:5]^2-radii[1:4]^2))
  yy_u = as.matrix(yy+1.96*sd_conc[2,15:19]/sqrt(12))
  yy_l = as.matrix(yy-1.96*sd_conc[2,15:19]/sqrt(12))
  segments(x0=xx2a,y0=yy_l,x1=xx2a,y1=yy_u,col="black")
  points(xx2a,yy,col="black",bg="red",pch=22,cex=2)
  lines(xx2a,yy,col="red",lty=1)
  
  # ng
  yy = mean_conc[4,15:19]
  yy = c(yy[1],(yy[2:5]*radii[2:5]^2-yy[1:4]*radii[1:4]^2)/(radii[2:5]^2-radii[1:4]^2))
  yy_u = as.matrix(yy+1.96*sd_conc[4,15:19]/sqrt(12))
  yy_l = as.matrix(yy-1.96*sd_conc[4,15:19]/sqrt(12))
  segments(x0=xx2,y0=yy_l,x1=xx2,y1=yy_u,col="black")
  points(xx2,yy,col="black",bg="blue",pch=22,cex=2)
  lines(xx2,yy,col="blue",lty=1)
  
  # other
  yy = mean_conc[6,15:19]
  yy = c(yy[1],(yy[2:5]*radii[2:5]^2-yy[1:4]*radii[1:4]^2)/(radii[2:5]^2-radii[1:4]^2))
  yy_u = as.matrix(yy+1.96*sd_conc[6,15:19]/sqrt(12))
  yy_l = as.matrix(yy-1.96*sd_conc[6,15:19]/sqrt(12))
  segments(x0=xx2b,y0=yy_l,x1=xx2b,y1=yy_u,col="black")
  points(xx2b,yy,col="black",bg="orange",pch=22,cex=2)
  lines(xx2b,yy,col="orange",lty=1)
  
  legend("topright",inset=0.01,bty="n",fill=c("red","blue","orange"),legend=c("Coal","Natural Gas","Other"))
  box(which="plot",lwd=2)
dev.off()

# SO2 over distance
png(width=8,height=6,units="in",res=300,pointsize=12,file="Plots/Chemistry/SO2_Distance.png",type="cairo")
  par(mar=c(6,8,1,1),las=1)
  plot(xx2,xx2-1000,xlim=c(0,100),ylim=c(0,0.2),ylab="",xlab="Distance [km]",cex.lab=2,cex.axis=1.5)
  mtext(side=2,text=expression(paste(SO[2]," PBL Concentration [DU]")),cex=2,las=0,line=4)
  grid(col="darkgrey")
  
  # coal
  yy = mean_conc[2,20:24]
  yy = c(yy[1],(yy[2:5]*radii[2:5]^2-yy[1:4]*radii[1:4]^2)/(radii[2:5]^2-radii[1:4]^2))
  yy_u = as.matrix(yy+1.96*sd_conc[2,20:24]/sqrt(12))
  yy_l = as.matrix(yy-1.96*sd_conc[2,20:24]/sqrt(12))
  segments(x0=xx2a,y0=yy_l,x1=xx2a,y1=yy_u,col="black")
  points(xx2a,yy,col="black",bg="red",pch=22,cex=2)
  lines(xx2a,yy,col="red",lty=1)
  
  # ng
  yy = mean_conc[4,20:24]
  yy = c(yy[1],(yy[2:5]*radii[2:5]^2-yy[1:4]*radii[1:4]^2)/(radii[2:5]^2-radii[1:4]^2))
  yy_u = as.matrix(yy+1.96*sd_conc[4,20:24]/sqrt(12))
  yy_l = as.matrix(yy-1.96*sd_conc[4,20:24]/sqrt(12))
  segments(x0=xx2,y0=yy_l,x1=xx2,y1=yy_u,col="black")
  points(xx2,yy,col="black",bg="blue",pch=22,cex=2)
  lines(xx2,yy,col="blue",lty=1)
  
  # other
  yy = mean_conc[6,20:24]
  yy = c(yy[1],(yy[2:5]*radii[2:5]^2-yy[1:4]*radii[1:4]^2)/(radii[2:5]^2-radii[1:4]^2))
  yy_u = as.matrix(yy+1.96*sd_conc[6,20:24]/sqrt(12))
  yy_l = as.matrix(yy-1.96*sd_conc[6,20:24]/sqrt(12))
  segments(x0=xx2b,y0=yy_l,x1=xx2b,y1=yy_u,col="black")
  points(xx2b,yy,col="black",bg="orange",pch=22,cex=2)
  lines(xx2b,yy,col="orange",lty=1)
  
  legend("topright",inset=0.01,bty="n",fill=c("red","blue","orange"),legend=c("Coal","Natural Gas","Other"))
  box(which="plot",lwd=2)
dev.off()

# -----------------------------------------------------------------------
# Coefficients Plots (Different Models)
# -----------------------------------------------------------------------

# Better labels
labs=c("# Coal-Fired Units, 25km","# Coal-Fired Units, 50km","# Coal-Fired Units, 100km","# Coal-Fired Units, 200km",
       "# Coal and NG Units, 25km","# Coal and NG Units, 50km","# Coal and NG Units, 100km","# Coal and NG Units, 200km")
labs2=c("Area Surrounding Old Coal Unit","Area Surrounding New NG Unit")

Fstat = c(Fstat_m_tot_1,Fstat_m_tot_2,Fstat_m_tot_3,Fstat_m_tot_4,Fstat_m_tot_5,Fstat_m_tot_6,
          Fstat_m_tot_7,Fstat_m_tot_8,Fstat_m_tot_9,Fstat_m_tot_10,Fstat_m_tot_11,Fstat_m_tot_12)

m.pm = rbind(summary(m_tot_1$pm)$coefficients[1,],summary(m_tot_2$pm)$coefficients[1,],summary(m_tot_3$pm)$coefficients[1,],summary(m_tot_4$pm)$coefficients[1,],
             summary(m_tot_9$pm)$coefficients[1,],summary(m_tot_10$pm)$coefficients[1,],summary(m_tot_11$pm)$coefficients[1,],summary(m_tot_12$pm)$coefficients[1,])
m.ng = rbind(summary(m_tot_9$pm)$coefficients[2,],summary(m_tot_10$pm)$coefficients[2,],summary(m_tot_11$pm)$coefficients[2,],summary(m_tot_11$pm)$coefficients[2,])

# From main impacts plots files
m.loc = rbind(pm5.coal,pm100.coal,pm5.ng,pm100.ng)

# Reproduce basic local for ease:
fmla = pm.100km ~ uniton*fueltype-fueltype | factor(Year) + factor(locationid) | 0 |  locationid
  m = felm(fmla,data=fullpanel.plants.annual)
  aa = summary(m)$coefficients

# png(width=10,height=6,units="in",res=300,file="Plots/Chemistry/PM_Models.png")
pdf(width=10,height=6,file="Plots/Chemistry/PM_Models.pdf")
  par(mar=c(6,15,1,1),las=1)
  xx=seq(-0.3,0.3,0.01)  
  yy=rev(1:(dim(m.pm)[1]))
  plot(xx,xx-100,ylim=c(0.5,max(yy)+2+0.5),yaxt="n",ylab="",xlab="",cex.axis=1.5,cex.lab=2)
  mtext(side=1,text=expression(paste(Delta,PM[2.5]," [",mu,"g ",m^-3,"]",sep="")),cex=2,line=4)
  abline(v=seq(-0.3,0.3,0.05),col="darkgrey",lty=2)
  abline(h=max(yy)+1)
  abline(v=0,col="darkgrey",lty=1,lwd=2)
  segments(y0=yy[5:8]-0.1,x0=m.ng[,1]-sig*m.ng[,2],x1=m.ng[,1]+sig*m.ng[,2],col="black",lty=1)
  points(m.ng[,1],yy[5:8]-0.1,bg="blue",pch=21,col="black",cex=2)
  segments(y0=yy,x0=m.pm[,1]-sig*m.pm[,2],x1=m.pm[,1]+sig*m.pm[,2],col="black",lty=1)
  points(m.pm[,1],yy,bg="red",pch=21,col="black",cex=2)
  axis(side=2,at=yy,labels=labs,cex.axis=1.2)
  segments(y0=max(yy)+2,x0=aa[1,1]-1.96*aa[1,2],x1=aa[1,1]+1.96*aa[1,2])
  segments(y0=max(yy)+1.9,x0=aa[1,1]+aa[2,1]-1.96*aa[2,2],x1=aa[1,1]+aa[2,1]+1.96*aa[1,2])
  points(x=c(aa[1,1],aa[1,1]+aa[2,1]),y=max(yy)+c(2,1.9),bg=c("red","blue"),col="black",cex=2,pch=21)
  axis(side=2,at=max(yy)+2,labels="Region Around Unit, 100km",cex.axis=1.2)
  text(0*yy-0.3,yy,paste("(",round(Fstat[c(1:4,9:12)],1),")",sep=""),col="black",pos=4,cex=1)
  text(-0.3,max(yy)+2,"(15.8)",pos=4,cex=1,col="black")
  box(which="plot",lwd=2)
dev.off()


# Surface Ozone
m.coal = rbind(summary(m_tot_1$o3surf)$coefficients[1,],summary(m_tot_2$o3surf)$coefficients[1,],summary(m_tot_3$o3surf)$coefficients[1,],summary(m_tot_4$o3surf)$coefficients[1,],
             summary(m_tot_9$o3surf)$coefficients[1,],summary(m_tot_10$o3surf)$coefficients[1,],summary(m_tot_11$o3surf)$coefficients[1,],summary(m_tot_12$o3surf)$coefficients[1,])
m.ng = rbind(summary(m_tot_9$o3surf)$coefficients[2,],summary(m_tot_10$o3surf)$coefficients[2,],summary(m_tot_11$o3surf)$coefficients[2,],summary(m_tot_11$o3surf)$coefficients[2,])

# From main impacts plots files
m.loc = rbind(ozone25.coal,ozone100.coal,ozone25.ng,ozone100.ng)

# Reproduce basic local for ease:
fmla = epa.o3.100km ~ uniton*fueltype-fueltype | factor(Year) + factor(locationid) | 0 |  locationid
m = felm(fmla,data=fullpanel.plants.annual)
aa = summary(m)$coefficients

png(width=10,height=6,units="in",res=300,file="Plots/Chemistry/O3_Models.png")
  par(mar=c(6,15,1,1),las=1)
  xx=seq(-1.2,1.2,0.2)  
  yy=rev(1:(dim(m.o3)[1]))
  plot(xx,xx-100,ylim=c(0.5,max(yy)+2+0.5),yaxt="n",ylab="",xlab="",cex.axis=1.5,cex.lab=2)
  mtext(side=1,text=expression(paste(Delta,O[3]," [ppb]",sep="")),cex=2,line=4)
  abline(v=seq(-1.2,1.2,0.2),col="darkgrey",lty=2)
  abline(h=max(yy)+1)
  abline(v=0,col="darkgrey",lty=1,lwd=2)
  segments(y0=yy[5:8]-0.1,x0=m.ng[,1]-sig*m.ng[,2],x1=m.ng[,1]+sig*m.ng[,2],col="black",lty=1)
  points(m.ng[,1],yy[5:8]-0.1,bg="blue",pch=21,col="black",cex=2)
  segments(y0=yy,x0=m.coal[,1]-sig*m.coal[,2],x1=m.coal[,1]+sig*m.coal[,2],col="black",lty=1)
  points(m.coal[,1],yy,bg="red",pch=21,col="black",cex=2)
  axis(side=2,at=yy,labels=labs,cex.axis=1.2)
  segments(y0=max(yy)+2,x0=aa[1,1]-1.96*aa[1,2],x1=aa[1,1]+1.96*aa[1,2])
  segments(y0=max(yy)+1.9,x0=aa[1,1]+aa[2,1]-1.96*aa[2,2],x1=aa[1,1]+aa[2,1]+1.96*aa[1,2])
  points(x=c(aa[1,1],aa[1,1]+aa[2,1]),y=max(yy)+c(2,1.9),bg=c("red","blue"),col="black",cex=2,pch=21)
  axis(side=2,at=max(yy)+2,labels="Region Around Unit, 100km",cex.axis=1.2)
  #text(0*yy-0.3,yy,paste("(",round(Fstat[c(1:4,9:12)],1),")",sep=""),col="black",pos=4,cex=1)
  #text(-0.3,max(yy)+2,"(15.8)",pos=4,cex=1,col="black")
  box(which="plot",lwd=2)
dev.off()

# -----------------------------------------------------------------------
# Satellite and Surface Comparison
# -----------------------------------------------------------------------

cols = rainbow(length(yrs))

png(width=6,height=5,units="in",res=300,file="Plots/Chemistry/Ozone Comparison.png")
  par(mar=c(5,5,1,1),las=1)
  plot(-20,-20,xlab="Surface Ozone [ppb]",ylab="Column Ozone [DU]",xlim=c(20,60),ylim=c(260,360),cex.axis=1.2,cex.lab=1.5)
  grid(col="darkgrey")
  for (i in 1:length(yrs)) {
    d=county.env.data[county.env.data$Year==yrs[i],]
    points(d$o3surf,d$o3,col=cols[i],pch=16,cex=0.4)
  }
  box(lwd=2)
dev.off()

xx = data.frame(pmsurf=seq(0,20,0.1))

png(width=6,height=5,units="in",res=300,file="Plots/Chemistry/PM Comparison.png")
  par(mar=c(5,5,1,1),las=1)
  plot(-20,-20,xlab=expression(paste("Surface ",PM[2.5]," [",mu,"g ",m^-3,"]")),ylab=expression(paste("Satellite ",PM[2.5]," [",mu,"g ",m^-3,"]")),xlim=c(0,20),ylim=c(0,20),cex.axis=1.2,cex.lab=1.5)
  grid(col="darkgrey")
  for (i in 1:length(yrs)) {
    d=county.env.data[county.env.data$Year==yrs[i],]
    points(d$pmsurf,d$pm,col=cols[i],pch=16,cex=0.4)
    lines(xx$pmsurf,predict(lm(pm~pmsurf,data=d),newdata=xx),col=cols[i],lwd=2)
  }
  legend("bottomright",bty="n",pch=16,pt.cex=0.6,legend=yrs,col=cols)
  lines(xx$pmsurf,predict(lm(pm~pmsurf,data=county.env.data),newdata=xx),col="black",lwd=3,lty=2)
  box(lwd=2)
dev.off()
