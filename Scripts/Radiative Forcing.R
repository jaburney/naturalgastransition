# --------------------------------------------------------------------------------------------
# Radiative Forcing.R
# --------------------------------------------------------------------------------------------
# Instantaneous top-of-atmosphere radiative forcing calculations
# --------------------------------------------------------------------------------------------

# Aggregate and Average AOD and SSA
# ----------------------------------------

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


# Calculate Radiative Forcing Changes
# ----------------------------------------

# Get theoretical critical value for pos/neg forcing change, with different backscatter and albedo (Rs)
Rs = seq(0,1,0.01)
backscatter = seq(0.1,0.4,0.1)
crit = matrix(NA,nrow=length(backscatter),ncol=length(Rs))
for (i in 1:length(backscatter)) {
  crit[i,] = 2*Rs/(backscatter[i]*(1-Rs)^2 + 2*Rs)
}

# SSA Threshold - use 0.21 for backscatter - (almost all still net cooling)
# Russell et al JGR 1997 // Haywood and Shine GRL 1995
# (1-SSA)/(SSA*Beta) > (1-Rs^2)/2Rs
# w > 2*Rs/(beta*(1-Rs^2)+2*Rs)
thresh_modis_beg = 2*alb_beg/(0.21*(1-alb_beg^2)+2*alb_beg)
thresh_modis_end = 2*alb_end/(0.21*(1-alb_end^2)+2*alb_end)
net_warm_beg = omi_ssa_beg < thresh_modis_beg
net_warm_end = omi_ssa_end < thresh_modis_end

# Calculate change in radiative forcing
S = 1361 # Wm -2 
Tat = 0.76 # transmissivity of atmosph
D = 0.5 # fractional daylength

# Cloud Fraction 
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


# Merge with Unit Data
# ----------------------------------------

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

# ------------------------------------------------------------------
# Plots (Main Manuscript Figure 4, Supplementary Figure 5)
# ------------------------------------------------------------------

# Emissions Changes - Main Manuscript Figure 4a
names(usa.allpoints.sp.0p125) = "locationid"
emissions = merge(usa.allpoints.sp.0p125,units,by=c("locationid"))

greys=colorRampPalette(c("grey80","grey90"))
cyans=colorRampPalette(c("white","cyan","blue","purple"))
bks = seq(-8500,0,500)
bks2 = seq(500,1000,500)

png(width=8,height=6,units="in",res=300,file="Plots/Fig4a.png")
  par(mar=c(0,0,0,0))
  spplot(emissions[emissions$diff_so2!=0,],zcol="diff_so2",pch=21,cex=1.2,cuts=c(bks,bks2),col.regions=c(rev(cyans(length(bks)+1))[1:length(bks)],rev(greys(length(bks2)))),xlab="",ylab="",par.settings=list(axis.line=list(col='transparent')),colorkey=TRUE,sp.layout=list("sp.polygons",usabounds,col="red",fill="lightyellow",alpha=0.5,lwd=3))
dev.off()

# AOD Changes - Main Manuscript Figure 4b
greys=colorRampPalette(c("black","white"))
cyans=colorRampPalette(c("cyan","blue"))
reds=colorRampPalette(c("white","darkred"))

bks = seq(-0.1,0.0,0.01)
bks2 = seq(0.01,0.04,0.01)
png(width=8,height=6,units="in",res=300,file="Plots/Fig4b.png")
  par(mar=c(0,0,0,0))
  plot(modis_aod_diff,breaks=c(bks,bks2),col=c(greys(length(bks)),cyans(length(bks2))),bty="n",box=FALSE,xaxt="n",yaxt="n")
  lines(usabounds,col="red",lwd=3)
dev.off()

# SSA Changes - Main Manuscript Figure 4c
bks = seq(-0.045,-0.005,0.005)
bks2 = seq(0,0.01,0.005)
png(width=8,height=6,units="in",res=300,file="Plots/Fig4c.png")
  par(mar=c(0,0,0,0))
  plot(omi_ssa_diff,breaks=c(bks,bks2),col=c(greys(length(bks)),cyans(length(bks2))),bty="n",box=FALSE,xaxt="n",yaxt="n")
  lines(usabounds,col="red",lwd=3)
dev.off()

# TOA IRF Changes - Main Manuscript Figure 4d (use 0.2)
bks = seq(-0.4,0,0.2)
bks2 = seq(0.2,1.6,0.2)
png(width=8,height=6,units="in",res=300,file="Plots/Fig4d.png")
  par(mar=c(0,0,0,0))
  plot(rf_diff,breaks=c(bks,bks2),col=c(greys(length(bks)),cyans(length(bks2))),bty="n",box=FALSE,xaxt="n",yaxt="n")
  lines(usabounds,col="red",lwd=3)
dev.off()

# Threshold Cooling-Warming - Main Manuscript Figure 4e
png(width=6,height=4,units="in",res=300,pointsize=12,file="Plots/Fig4e.png")
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

# RF Change by Plant/Location Type - Main Manuscript Figure 4f
png(width=8,height=5,units="in",res=300,file="Plots/Fig4f.png")
  par(mar=c(0,0,0,0),las=1)
  my_comparisons2=list(c("No Change","Old Coal Unit(s)"),c("New NG Unit(s)","Old Coal Unit(s)"),c("No Plants","Old Coal Unit(s)"))
  p = ggviolin(rf,x="category2",y="rf_diff",fill="category2",palette="jco",add="boxplot",add.params=list(fill="white"))+stat_compare_means(comparisons=my_comparisons2,label="p.signif",method="t.test",symnum.args=list(cutpoints=c(0,0.001,0.01,0.05,0.1,1),symbols=c("****","***","**","*","ns")))+geom_hline(yintercept=0,col="darkgrey",lty=2)
  ggpar(p,legend="none",xlab="",ylab=bquote("Change in TOA Radiative Forcing [W/"~m^-2~"]"),font.xtickslab=c(16,"plain","black"),font.ytickslab=c(16,"plain","black"),font.y=c(16,"plain","black"))
dev.off()

# RF Beginning and End - Supplementary Figure 5
bks = seq(-4.5,-0.5,0.5)
bks2 = seq(0,2,0.5)
png(width=8,height=6,units="in",res=300,file="Plots/SuppFig5a.png")
  par(mar=c(0,0,0,0))
  plot(rf_beg,breaks=c(bks,bks2),col=c(rev(cyans(length(bks))),reds(length(bks2+1))[2:length(bks2)]),bty="n",box=FALSE,xaxt="n",yaxt="n")
  lines(usabounds,col="red",lwd=3)
dev.off()

png(width=8,height=6,units="in",res=300,file="Plots/SuppFig5b.png")
  par(mar=c(0,0,0,0))
  plot(rf_end,breaks=c(bks,bks2),col=c(rev(cyans(length(bks))),reds(length(bks2+1))[2:length(bks2)]),bty="n",box=FALSE,xaxt="n",yaxt="n")
  lines(usabounds,col="red",lwd=3)
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

