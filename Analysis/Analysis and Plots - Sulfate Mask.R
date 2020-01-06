# ------------------------------------------------------------------
# Analysis - Sulfate Mask.R
# ------------------------------------------------------------------

# ------------------------------------------------------------------
# Load Data / Get Projections Correct
# ------------------------------------------------------------------

# MODIS surface albedo
surf_albedo_beg = raster("Data/L0 Input Data/Aerosols (MODIS and OMI)/g4.timeAvgMap.M2TMNXRAD_5_12_4_ALBEDO.20050101-20061231.125W_24N_66W_50N.nc")
surf_albedo_end = raster("Data/L0 Input Data/Aerosols (MODIS and OMI)/g4.timeAvgMap.M2TMNXRAD_5_12_4_ALBEDO.20150101-20161231.125W_24N_66W_50N.nc")

# AIRS clouds average 2005-2015
cloudsday = raster("Data/L0 Input Data/Aerosols (MODIS and OMI)/GIOVANNI-g4.timeAvgMap.AIRX3STM_006_CloudFrc_A.20050101-20151231.125W_24N_66W_50N.tif")
cloudsnight = raster("Data/L0 Input Data/Aerosols (MODIS and OMI)/GIOVANNI-g4.timeAvgMap.AIRX3STM_006_CloudFrc_D.20050101-20151231.125W_24N_66W_50N.tif")

# Aerosols and plant data
load("Data/L1 Processed Data/USA MODIS AOD 2005-2016.Rdata")
load("Data/L1 Processed Data/USA OMI SSA 2005-2016.Rdata")
load("Data/L2 Processed Data/Cleaned Power Plant and Spatial Data 2005-2016.RData")
load("Data/Final Data Sets for Analysis/FullPanel_AllPoints_Annual.Rdata")
load("Data/Final Data Sets for Analysis/FullPanel_PlantLocations_Annual.Rdata")

data("countriesCoarse")
usabounds=countriesCoarse[countriesCoarse$NAME=="United States" & !is.na(countriesCoarse$NAME),]
  usabounds=spTransform(usabounds,crs(modis.aod.usa.avg))
  usabounds=crop(usabounds,modis.aod.usa.avg)

alb_beg = projectRaster(surf_albedo_beg,modis.aod.usa.avg)
  alb_beg = mask(alb_beg,usabounds)
alb_end = projectRaster(surf_albedo_end,modis.aod.usa.avg)
  alb_end = mask(alb_end,usabounds)

alb_beg_pts = getValues(alb_beg)
alb_end_pts = getValues(alb_end)
  
# ------------------------------------------------------------------
# Aggregate and Average AOD and SSA
# ------------------------------------------------------------------

modis_aod_beg = calc(modis.aod.usa[[1:24]],fun=mean,na.rm=TRUE)
modis_aod_end = calc(modis.aod.usa[[121:144]],fun=mean,na.rm=TRUE)
modis_aod_diff = modis_aod_end - modis_aod_beg
modis_aod_ratio = modis_aod_end/modis_aod_beg

aod_beg_pts = getValues(modis_aod_beg)
aod_end_pts = getValues(modis_aod_end)

omi_ssa_beg = calc(omi.ssa.usa.monthly[[1:24]],fun=median,na.rm=TRUE)
omi_ssa_end = calc(omi.ssa.usa.monthly[[121:144]],fun=median,na.rm=TRUE)
omi_ssa_diff = omi_ssa_end - omi_ssa_beg
omi_ssa_ratio = omi_ssa_end/omi_ssa_beg

ssa_beg_pts = getValues(omi_ssa_beg)
ssa_end_pts = getValues(omi_ssa_end)

# some colors for plotting
greys=colorRampPalette(c("black","white"))
cyans=colorRampPalette(c("cyan","blue"))
reds=colorRampPalette(c("white","darkred"))

bks = seq(-0.1,0.0,0.01)
bks2 = seq(0.01,0.04,0.01)
png(width=8,height=6,units="in",res=300,file="Plots/Sulfate Mask Plots/Modis_AOD_Change.png")
  par(mar=c(0,0,0,0))
  plot(modis_aod_diff,breaks=c(bks,bks2),col=c(greys(length(bks)),cyans(length(bks2))),bty="n",box=FALSE,xaxt="n",yaxt="n")
  lines(usabounds,col="red",lwd=3)
dev.off()

bks = seq(-0.045,-0.005,0.005)
bks2 = seq(0,0.01,0.005)
png(width=8,height=6,units="in",res=300,file="Plots/Sulfate Mask Plots/OMI_SSA_Change.png")
  par(mar=c(0,0,0,0))
  plot(omi_ssa_diff,breaks=c(bks,bks2),col=c(greys(length(bks)),cyans(length(bks2))),bty="n",box=FALSE,xaxt="n",yaxt="n")
  lines(usabounds,col="red",lwd=3)
dev.off()

bks = seq(0.4,1,0.1)
bks2 = seq(1.1,1.5,0.1)
png(width=8,height=6,units="in",res=300,file="Plots/Sulfate Mask Plots/Modis_AOD_Ratio.png")
  par(mar=c(0,0,0,0))
  plot(modis_aod_ratio,breaks=c(bks,bks2),col=c(greys(length(bks)),cyans(length(bks2))),bty="n",box=FALSE,xaxt="n",yaxt="n")
  lines(usabounds,col="red",lwd=3)
dev.off()

bks = seq(0.9,1,0.02)
bks2 = seq(1.02,1.1,0.02)
png(width=8,height=6,units="in",res=300,file="Plots/Sulfate Mask Plots/OMI_SSA_Ratio.png")
  par(mar=c(0,0,0,0))
  plot(omi_ssa_ratio,breaks=c(bks,bks2),col=c(greys(length(bks)),cyans(length(bks2))),bty="n",box=FALSE,xaxt="n",yaxt="n")
  lines(usabounds,col="red",lwd=3)
dev.off()

# ------------------------------------------------------------------
# Calculate Radiative Forcing Changes
# ------------------------------------------------------------------

# Get theoretical critical value for pos/neg forcing change, with different backscatter and albedo (Rs)
Rs = seq(0,1,0.01)
backscatter = seq(0.1,0.4,0.1)
crit = matrix(NA,nrow=length(backscatter),ncol=length(Rs))
for (i in 1:length(backscatter)) {
  crit[i,] = 2*Rs/(backscatter[i]*(1-Rs)^2 + 2*Rs)
}

# SSA Threshold - use 0.21 for backscatter - all still net cooling?
# Russell et al JGR 1997 // Haywood and Shine GRL 1995
# (1-SSA)/(SSA*Beta) > (1-Rs^2)/2Rs
# w > 2*Rs/(beta*(1-Rs^2)+2*Rs)
thresh_modis_beg = 2*alb_beg/(0.21*(1-alb_beg^2)+2*alb_beg)
thresh_modis_end = 2*alb_end/(0.21*(1-alb_end^2)+2*alb_end)
net_warm_beg = omi_ssa_beg < thresh_modis_beg
net_warm_end = omi_ssa_end < thresh_modis_end

# Threshold plot
#png(width=6,height=4,units="in",res=300,pointsize=12,file="Plots/Sulfate Mask Plots/Threshold_SSA.png")
pdf(width=6,height=4,pointsize=12,file="Plots/Sulfate Mask Plots/Threshold_SSA.pdf")
  par(mar=c(4,5,1,1),las=1)
  plot(Rs,-10*Rs-10,ylim=c(0,1),xlab="Surface Reflectance (Albedo)",ylab="Single Scattering Albedo")
  grid(col="darkgrey")
  lines(Rs,crit[1,],lty=1,col="red",lwd=2)
  lines(Rs,crit[2,],lty=2,col="red",lwd=2)
  lines(Rs,crit[3,],lty=3,col="red",lwd=2)
  lines(Rs,crit[4,],lty=4,col="red",lwd=2)
  #hist(alb_pts,breaks=Rs,add=TRUE,freq=TRUE)
  points(alb_beg_pts,ssa_beg_pts,col="blue",cex=2)
  points(alb_end_pts,ssa_end_pts,col="orange",cex=2)
  text(0.5,0.5,"Net Warming",col="darkgreen")
  text(0.1,1.0,"Net Cooling",col="darkgreen")
  lgtxt=c(expression(paste(beta," = 0.1")),expression(paste(beta," = 0.2")),expression(paste(beta," = 0.3")),expression(paste(beta," = 0.4")))
  legend("bottomright",inset=0.01,lty=c(1,2,3,4),lwd=2,col="red",legend=lgtxt,bty="n")
  legend("topright",inset=0.05,col=c("blue","orange"),pch=21,pt.cex=2,legend=c("2005-2006","2015-2016"),bty="n")
dev.off()

# Pre-post SSA distributions
df = data.frame(Rs=c(alb_beg_pts,alb_end_pts),SSA=c(ssa_beg_pts,ssa_end_pts),Time=c(rep("2005-2006",length(ssa_beg_pts)),rep("2015-2016",length(ssa_end_pts))))
my_comparisons=list(c("2005-2006","2015-2016"))
p=ggviolin(data=df,x="Time",y="SSA",color="Time",fill="Time",add="boxplot",palette="jco",add.params=list(fill="white"))+stat_compare_means(comparisons = my_comparisons,label="p.signif")
png(width=6,height=4,units="in",res=300,pointsize=12,file="Plots/Sulfate Mask Plots/SSA_pre_post.png")
  ggpar(p,legend="none",xlab="")
dev.off()

# calculate change in radiative forcing
S = 1361 # Wm -2 
Tat = 0.76 # transmissivity of atmosph
D = 0.5 # fractional daylength

# cloud fraction - 
clouds = stack(cloudsday,cloudsnight)
clavg = calc(clouds,fun=mean,na.rm=TRUE)
Ac = mask(projectRaster(clavg,modis.aod.usa),usabounds)

rf_beg1 = -D*S*Tat^2*(1-Ac)*backscatter[1]*modis_aod_beg*omi_ssa_beg*((1-alb_beg)^2-2*alb_beg/backscatter[1]*(1/omi_ssa_beg -1))
rf_end1 = -D*S*Tat^2*(1-Ac)*backscatter[1]*modis_aod_end*omi_ssa_end*((1-alb_end)^2-2*alb_end/backscatter[1]*(1/omi_ssa_end -1))
rf_diff1 = rf_end1 - rf_beg1

rf_beg2 = -D*S*Tat^2*(1-Ac)*backscatter[2]*modis_aod_beg*omi_ssa_beg*((1-alb_beg)^2-2*alb_beg/backscatter[2]*(1/omi_ssa_beg -1))
rf_end2 = -D*S*Tat^2*(1-Ac)*backscatter[2]*modis_aod_end*omi_ssa_end*((1-alb_end)^2-2*alb_end/backscatter[2]*(1/omi_ssa_end -1))
rf_diff2 = rf_end2 - rf_beg2

rf_beg3 = -D*S*Tat^2*(1-Ac)*backscatter[3]*modis_aod_beg*omi_ssa_beg*((1-alb_beg)^2-2*alb_beg/backscatter[3]*(1/omi_ssa_beg -1))
rf_end3 = -D*S*Tat^2*(1-Ac)*backscatter[3]*modis_aod_end*omi_ssa_end*((1-alb_end)^2-2*alb_end/backscatter[3]*(1/omi_ssa_end -1))
rf_diff3 = rf_end3 - rf_beg3

rf_beg4 = -D*S*Tat^2*(1-Ac)*backscatter[4]*modis_aod_beg*omi_ssa_beg*((1-alb_beg)^2-2*alb_beg/backscatter[4]*(1/omi_ssa_beg -1))
rf_end4 = -D*S*Tat^2*(1-Ac)*backscatter[4]*modis_aod_end*omi_ssa_end*((1-alb_end)^2-2*alb_end/backscatter[4]*(1/omi_ssa_end -1))
rf_diff4 = rf_end4 - rf_beg4

bs=0.13
rf_beg = -D*S*Tat^2*(1-Ac)*bs*modis_aod_beg*omi_ssa_beg*((1-alb_beg)^2-2*alb_beg/bs*(1/omi_ssa_beg -1))
rf_end = -D*S*Tat^2*(1-Ac)*bs*modis_aod_end*omi_ssa_end*((1-alb_end)^2-2*alb_end/bs*(1/omi_ssa_end -1))
rf_diff = rf_end - rf_beg

cellStats(rf_diff,mean,na.rm=TRUE)

cellStats(rf_diff1,mean,na.rm=TRUE)
cellStats(rf_diff2,mean,na.rm=TRUE)
cellStats(rf_diff3,mean,na.rm=TRUE)
cellStats(rf_diff4,mean,na.rm=TRUE)

# Plots (use 0.2)
bks = seq(-0.5,0,0.25)
bks2 = seq(0.25,3,0.25)
png(width=8,height=6,units="in",res=300,file="Plots/Sulfate Mask Plots/RF_Diff_Beta2.png")
  par(mar=c(0,0,0,0))
  plot(rf_diff2,breaks=c(bks,bks2),col=c(greys(length(bks)),cyans(length(bks2))),bty="n",box=FALSE,xaxt="n",yaxt="n")
  lines(usabounds,col="red",lwd=3)
dev.off()

png(width=8,height=6,units="in",res=300,file="Plots/Sulfate Mask Plots/RF_Diff_Beta1.png")
  par(mar=c(0,0,0,0))
  plot(rf_diff1,breaks=c(bks,bks2),col=c(greys(length(bks)),cyans(length(bks2))),bty="n",box=FALSE,xaxt="n",yaxt="n")
  lines(usabounds,col="red",lwd=3)
dev.off()

png(width=8,height=6,units="in",res=300,file="Plots/Sulfate Mask Plots/RF_Diff_Beta3.png")
  par(mar=c(0,0,0,0))
  plot(rf_diff3,breaks=c(bks,bks2),col=c(greys(length(bks)),cyans(length(bks2))),bty="n",box=FALSE,xaxt="n",yaxt="n")
  lines(usabounds,col="red",lwd=3)
dev.off()

bks = seq(-0.4,0,0.2)
bks2 = seq(0.2,1.6,0.2)
png(width=8,height=6,units="in",res=300,file="Plots/Sulfate Mask Plots/RF_Diff.png")
  par(mar=c(0,0,0,0))
  plot(rf_diff,breaks=c(bks,bks2),col=c(greys(length(bks)),cyans(length(bks2))),bty="n",box=FALSE,xaxt="n",yaxt="n")
  lines(usabounds,col="red",lwd=3)
dev.off()

bks = seq(-4.5,-0.5,0.5)
bks2 = seq(0,2,0.5)
png(width=8,height=6,units="in",res=300,file="Plots/Sulfate Mask Plots/RF_Beg.png")
  par(mar=c(0,0,0,0))
  plot(rf_beg,breaks=c(bks,bks2),col=c(rev(cyans(length(bks))),reds(length(bks2+1))[2:length(bks2)]),bty="n",box=FALSE,xaxt="n",yaxt="n")
  lines(usabounds,col="red",lwd=3)
dev.off()

png(width=8,height=6,units="in",res=300,file="Plots/Sulfate Mask Plots/RF_End.png")
  par(mar=c(0,0,0,0))
  plot(rf_end,breaks=c(bks,bks2),col=c(rev(cyans(length(bks))),reds(length(bks2+1))[2:length(bks2)]),bty="n",box=FALSE,xaxt="n",yaxt="n")
  lines(usabounds,col="red",lwd=3)
dev.off()

# ------------------------------------------------------------------
# Merge with Unit Data
# ------------------------------------------------------------------

unitsbeg = fullpanel.allpoints.annual.1[fullpanel.allpoints.annual.1$year %in% c(2005,2006),] %>% group_by(locationid) %>% summarize(n_oldcoal=sum(oldcoalplant,na.rm=TRUE)/2,n_newng=sum(newngplant,na.rm=TRUE)/2,n_newcoal=sum(newcoalplant,na.rm=TRUE)/2,fuel=first(fueltype2),grossload05=mean(grossload,na.rm=TRUE),steamload05=mean(steamload,na.rm=TRUE),so205=sum(so2emissions,na.rm=TRUE)/2,nox05=sum(noxemissions,na.rm=TRUE)/2,co205=sum(co2emissions,na.rm=TRUE)/2)
unitsend = fullpanel.allpoints.annual.1[fullpanel.allpoints.annual.1$year %in% c(2015,2016),] %>% group_by(locationid) %>% summarize(grossload16=mean(grossload,na.rm=TRUE),steamload16=mean(steamload,na.rm=TRUE),so216=sum(so2emissions,na.rm=TRUE)/2,nox16=sum(noxemissions,na.rm=TRUE)/2,co216=sum(co2emissions,na.rm=TRUE)/2)
units = merge(unitsbeg,unitsend,by="locationid")

units$diff_grossload = units$grossload16-units$grossload05
units$diff_steamload = units$steamload16-units$steamload05
units$diff_so2 = units$so216-units$so205
units$diff_nox = units$nox16-units$nox05
units$diff_co2 = units$co216-units$co205

rf_diff_pts = rasterToPoints(rf_diff)
  colnames(rf_diff_pts) = c("longitude","latitude","rf_diff")

aod_diff_pts = rasterToPoints(modis_aod_diff)
  colnames(aod_diff_pts) = c("longitude","latitude","aod_diff")

ssa_diff_pts = rasterToPoints(omi_ssa_diff)
  colnames(ssa_diff_pts) = c("longitude","latitude","ssa_diff")
  
rf = merge(usa.allpoints.1,units,by="locationid")
rf = merge(rf,rf_diff_pts,by=c("longitude","latitude"))
rf = merge(rf,aod_diff_pts,by=c("longitude","latitude"))
rf = merge(rf,ssa_diff_pts,by=c("longitude","latitude"))

rf$anyoldcoal=(rf$n_oldcoal>0)
rf$anynewng=(rf$n_newng>0)
rf$anynewcoal=(rf$n_newcoal>0)

rf$category[rf$plant==0] = "No Plants"
rf$category[rf$plant==1 & rf$anyoldcoal==0 & rf$fuel=="Coal+Other"] = "Coal Entire Time"
rf$category[rf$plant==1 & rf$anyoldcoal==1] = "Coal Unit(s) Retired"
rf$category[rf$plant==1 & rf$anynewng==0 & rf$fuel=="Natural Gas"] = "NG Entire Time"
rf$category[rf$plant==1 & rf$anynewng==1] = "New NG Unit(s) Commissioned"

rf$category2[rf$plant==0] = "No Plants"
rf$category2[rf$plant==1 & rf$anyoldcoal==0 & rf$anynewng==0 & rf$anynewcoal==0] = "No Change"
rf$category2[rf$plant==1 & rf$anyoldcoal==1] = "Old Coal Unit(s)"
rf$category2[rf$plant==1 & rf$anynewng==1] = "New NG Unit(s)"
rf$category2[rf$plant==1 & rf$anynewcoal==1] = "New Coal Unit(s)"

rf$id = row.names(rf)

#my_comparisons=list(c("No Plants","Coal Unit(s) Retired"),c("NG Entire Time","New NG Unit(s) Commissioned"),c("No Plants","New NG Unit(s) Commissioned"),c("Coal Entire Time","Coal Unit(s) Retired"))
# ggviolin(rf,x="category",y="rf_diff2",fill="category",palette="jco",add="boxplot",add.params=list(fill="white"))+stat_compare_means(comparisons = my_comparisons,label="p.signif")+stat_compare_means(label.y=10)
# p = ggboxplot(rf,x="category2",y="rf_diff2",fill="category2",palette="jco")+stat_compare_means(ref.group="No Change",label="p.format",method="t.test",symnum.args=list(cutpoints=c(0,0.001,0.01,0.05,0.1,1),symbols=c("****","***","**","*","ns")))

png(width=10,height=4,units="in",res=300,file="Plots/Sulfate Mask Plots/RF_Change_by_Group.png")
  par(mar=c(0,0,0,0),las=1)
  my_comparisons2=list(c("No Change","Old Coal Unit(s)"),c("No Change","New NG Unit(s)"),c("No Plants","No Change"),c("No Change","Old Coal Unit(s)"),c("No Plants","Old Coal Unit(s)"))
  p = ggboxplot(rf,x="category2",y="rf_diff",fill="category2",palette="jco")+stat_compare_means(comparisons=my_comparisons2,label="p.signif",method="t.test",symnum.args=list(cutpoints=c(0,0.001,0.01,0.05,0.1,1),symbols=c("****","***","**","*","ns")))+geom_hline(yintercept=0,col="darkgrey",lty=2)
  ggpar(p,legend="none",xlab="",ylab=bquote("Change in TOA Radiative Forcing [W/"~m^-2~"]"))
dev.off()

png(width=8,height=5,units="in",res=300,file="Plots/Sulfate Mask Plots/RF_Change_by_Group_Violin.png")
  par(mar=c(0,0,0,0),las=1)
  #my_comparisons2=list(c("No Change","New NG Unit(s)"),c("No Plants","No Change"),c("No Change","Old Coal Unit(s)"),c("New NG Unit(s)","Old Coal Unit(s)"),c("No Plants","Old Coal Unit(s)"))
  my_comparisons2=list(c("No Change","Old Coal Unit(s)"),c("New NG Unit(s)","Old Coal Unit(s)"),c("No Plants","Old Coal Unit(s)"))
  p = ggviolin(rf,x="category2",y="rf_diff",fill="category2",palette="jco",add="boxplot",add.params=list(fill="white"))+stat_compare_means(comparisons=my_comparisons2,label="p.signif",method="t.test",symnum.args=list(cutpoints=c(0,0.001,0.01,0.05,0.1,1),symbols=c("****","***","**","*","ns")))+geom_hline(yintercept=0,col="darkgrey",lty=2)
  ggpar(p,legend="none",xlab="",ylab=bquote("Change in TOA Radiative Forcing [W/"~m^-2~"]"),font.xtickslab=c(16,"plain","black"),font.ytickslab=c(16,"plain","black"),font.y=c(16,"plain","black"))
dev.off()

# Emissions Changes
names(usa.allpoints.sp.0p125) = "locationid"
emissions = merge(usa.allpoints.sp.0p125,units,by=c("locationid"))

greys=colorRampPalette(c("grey80","grey90"))
cyans=colorRampPalette(c("white","cyan","blue","purple"))
bks = seq(-8500,0,500)
bks2 = seq(500,1000,500)

png(width=8,height=6,units="in",res=300,file="Plots/Sulfate Mask Plots/SO2Emissions_Change.png")
  par(mar=c(0,0,0,0))
  #spplot(emissions[emissions$diff_so2!=0,],zcol="diff_so2",pch=21,cex=1.2,cuts=c(bks,bks2),col.regions=c(rev(cyans(length(bks)+1))[1:length(bks)],rev(greys(length(bks2)))),xlab="",ylab="",par.settings=list(axis.line=list(col='transparent')),colorkey=TRUE,sp.layout=list(list("sp.points",emissions[emissions$diff_so2!=0,],pch=21,fill="transparent",col="black",lwd=0.5,cex=1.2),list("sp.polygons",usabounds,col="red",lwd=3)))
  spplot(emissions[emissions$diff_so2!=0,],zcol="diff_so2",pch=21,cex=1.2,cuts=c(bks,bks2),col.regions=c(rev(cyans(length(bks)+1))[1:length(bks)],rev(greys(length(bks2)))),xlab="",ylab="",par.settings=list(axis.line=list(col='transparent')),colorkey=TRUE,sp.layout=list("sp.polygons",usabounds,col="red",fill="lightyellow",alpha=0.5,lwd=3))
dev.off()

bks = seq(-3000,0,500)
bks2 = seq(500,1000,500)
png(width=8,height=6,units="in",res=300,file="Plots/Sulfate Mask Plots/NOxEmissions_Change.png")
  par(mar=c(0,0,0,0))
  spplot(emissions[emissions$diff_nox!=0,],zcol="diff_nox",pch=21,cex=1.2,cuts=c(bks,bks2),col.regions=c(rev(cyans(length(bks)+1))[1:length(bks)],rev(greys(length(bks2)))),xlab="",ylab="",par.settings=list(axis.line=list(col='transparent')),colorkey=TRUE,sp.layout=list("sp.polygons",usabounds,col="red",fill="lightyellow",alpha=0.5,lwd=3))
dev.off()

emissions$diff_poll=emissions$diff_nox+emissions$diff_so2
bks = seq(-18000,0,1000)
bks2 = seq(1000,2000,100)
png(width=8,height=6,units="in",res=300,file="Plots/Sulfate Mask Plots/NOx_plus_SO2_Emissions_Change.png")
  par(mar=c(0,0,0,0))
  spplot(emissions[emissions$diff_poll!=0,],zcol="diff_poll",pch=21,cex=1.2,cuts=c(bks,bks2),col.regions=c(rev(cyans(length(bks)+1))[1:length(bks)],rev(greys(length(bks2)))),xlab="",ylab="",par.settings=list(axis.line=list(col='transparent')),colorkey=TRUE,sp.layout=list("sp.polygons",usabounds,col="red",fill="lightyellow",alpha=0.5,lwd=3))
dev.off()

# ------------------------------------------------------------------
# Regressions
# ------------------------------------------------------------------

t1 = lm(rf_diff~diff_so2+diff_grossload,data=rf)
t2 = lm(aod_diff~diff_so2+diff_grossload,data=rf)
t3 = lm(ssa_diff~diff_so2*diff_grossload,data=rf)

summary(t1)
summary(t2)
summary(t3)

stargazer(t1,t2,t3,out="Tables/RFRegs.tex",header=TRUE)

