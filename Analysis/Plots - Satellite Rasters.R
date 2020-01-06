# -----------------------------------------------------------------------
# Plots - Satellite Rasters
# -----------------------------------------------------------------------

# -----------------------------------------------------------------------
# Load data
# -----------------------------------------------------------------------

load(file="Data/L2 Processed Data/Satellite and Surface Rasters.Rdata")
load(file="Data/L2 Processed Data/Extracted Monthly and Annual Atmospheric Data.Rdata")
load("Data/L2 Processed Data/Cleaned Power Plant and Spatial Data 2005-2016.Rdata")

data(countriesLow)
usaborders=countriesLow[countriesLow$NAME=="United States" & !is.na(countriesLow$NAME),]
usaborders=spTransform(usaborders,crs(states48))
usaborders=crop(usaborders,extent(states48))

# -----------------------------------------------------------------------
# Aggregate NO2 and SO2 to annual and fit
# -----------------------------------------------------------------------

indices = rep(c(1:12),each=12)
no2.annual = stackApply(no2.usa,indices,fun=mean,na.rm=T)
so2.annual = stackApply(omi.so2.usa.monthly,indices,fun=mean,na.rm=T)

# Get trends
time = 1:nlayers(no2.annual)
X = cbind(1,time)
invXtX = solve(t(X) %*% X) %*% t(X)
quickfun = function(i) { (invXtX %*% i) }

no2.trend=calc(no2.annual,fun=quickfun)
so2.trend=calc(so2.annual,fun=quickfun)

# -----------------------------------------------------------------------
# Some Basic Plots
# -----------------------------------------------------------------------

png(width=8,height=5,res=300,units="in",file="Plots/Rasters/NO2_Avg.png")
  mycols = colorRampPalette(c("grey90","blue","green","yellow","orange","red","darkred"))
  plot(usaborders,border="darkgrey",lwd=3)
  plot(no2.usa.avg,col=mycols(200),bty="n",box=FALSE,xaxt="n",yaxt="n",add=T)
dev.off()  

png(width=8,height=5,res=300,units="in",file="Plots/Rasters/SO2_Avg.png")
  mycols = colorRampPalette(c("grey90","blue","green","yellow","orange","red","darkred"))
  plot(usaborders,border="darkgrey",lwd=3)
  plot(omi.so2.usa.avg,col=mycols(200),bty="n",box=FALSE,xaxt="n",yaxt="n",add=T)
dev.off()  

png(width=8,height=5,res=300,units="in",file="Plots/Rasters/O3_Avg.png")
  mycols = colorRampPalette(c("grey90","blue","green","yellow","orange","red","darkred"))
  plot(usaborders,border="darkgrey",lwd=3)
  plot(omi.o3.usa.avg,col=mycols(200),bty="n",box=FALSE,xaxt="n",yaxt="n",add=T)
dev.off()  

png(width=8,height=5,res=300,units="in",file="Plots/Rasters/EPA_O3_Avg.png")
  mycols = colorRampPalette(c("grey90","blue","green","yellow","orange","red","darkred"))
  plot(usaborders,border="darkgrey",lwd=3)
  plot(epa.o3.avg,col=mycols(200),bty="n",box=FALSE,xaxt="n",yaxt="n",add=T)
dev.off()  

png(width=8,height=5,res=300,units="in",file="Plots/Rasters/PM_Avg.png")
  mycols = colorRampPalette(c("grey90","blue","green","yellow","orange","red","darkred"))
  plot(usaborders,border="darkgrey",lwd=3)
  plot(vd.usa.avg,col=mycols(200),bty="n",box=FALSE,xaxt="n",yaxt="n",add=T)
dev.off()  

png(width=8,height=5,res=300,units="in",file="Plots/Rasters/EPA_PM_Avg.png")
  mycols = colorRampPalette(c("grey90","blue","green","yellow","orange","red","darkred"))
  plot(usaborders,border="darkgrey",lwd=3)
  plot(epa.pm.avg,col=mycols(200),bty="n",box=FALSE,xaxt="n",yaxt="n",add=T)
dev.off()  

png(width=8,height=5,res=300,units="in",file="Plots/Rasters/PM_2005.png")
  mycols = colorRampPalette(c("grey90","blue","green","yellow","orange","red","darkred"))
  plot(usaborders,border="darkgrey",lwd=3)
  plot(vd.usa.trend[[1]],col=mycols(200),bty="n",box=FALSE,xaxt="n",yaxt="n",add=T)
dev.off()  

png(width=8,height=5,res=300,units="in",file="Plots/Rasters/EPA_PM_2005.png")
  mycols = colorRampPalette(c("grey90","blue","green","yellow","orange","red","darkred"))
  plot(usaborders,border="darkgrey",lwd=3)
  plot(epa.pm[[1]],col=mycols(200),bty="n",box=FALSE,xaxt="n",yaxt="n",add=T)
dev.off()  

png(width=8,height=5,res=300,units="in",file="Plots/Rasters/NO2_2005.png")
  mycols = colorRampPalette(c("grey90","blue","green","yellow","orange","red","darkred"))
  plot(usaborders,border="darkgrey",lwd=3)
  plot(no2.trend[[1]],col=mycols(200),bty="n",box=FALSE,xaxt="n",yaxt="n",add=T)
dev.off()  

png(width=8,height=5,res=300,units="in",file="Plots/Rasters/SO2_2005.png")
  mycols = colorRampPalette(c("grey90","blue","green","yellow","orange","red","darkred"))
  plot(usaborders,border="darkgrey",lwd=3)
  plot(so2.trend[[1]],col=mycols(200),bty="n",box=FALSE,xaxt="n",yaxt="n",add=T)
dev.off()  

png(width=8,height=5,res=300,units="in",file="Plots/Rasters/EPA_PM_Trend.png")
  mycols = colorRampPalette(c("darkgreen","white","brown4"))
  plot(usaborders,border="darkgrey",lwd=3)
  plot(epa.pm[[2]],col=mycols(200),bty="n",box=FALSE,xaxt="n",yaxt="n",add=T)
dev.off()  


# -----------------------------------------------------------------------------------------------
# Improved versions - trends and locations - for paper
# -----------------------------------------------------------------------------------------------

png(width=8,height=5,res=300,units="in",file="Plots/Rasters/PM_Trend.png")
  mycols = colorRampPalette(c("darkgreen","white","brown4"))
  bks = seq(-0.8,0.8,0.01)
  cols = mycols(length(bks))
  bks2 = seq(-0.8,0.8,0.1)
  cols2 = mycols(length(bks2))
  plot(usaborders,border="darkgrey",lwd=3)
  plot(vd.usa.trend[[2]],col=cols,breaks=bks,bty="n",box=FALSE,xaxt="n",yaxt="n",add=T,legend=FALSE)
  plot(vd.usa.trend[[2]],col=cols2,breaks=bks2,bty="n",box=FALSE,xaxt="n",yaxt="n",add=T,legend.only=TRUE)
  points(plant.locations2[plant.locations2$oldcoalplant==1,],col="red",pch=16,cex=0.35)
  points(plant.locations2[plant.locations2$newngplant==1,],col="blue",pch=16,cex=0.25)
dev.off()  


fill.na <- function(x,i=25) {
  if( is.na(x)[i] ) {
    return(round(mean(x,na.rm=TRUE),0) )
  } else {
    return(round(x[i],2))
  }
}  

rr = focal(epa.o3.trend[[1]],w=matrix(1,7,7),fun=fill.na,pad=FALSE,na.rm=FALSE)
test = cover(epa.o3.trend[[1]],rr)
o3fill = mask(test,omi.o3.usa.trend[[1]])

rr = focal(epa.o3.trend[[2]],w=matrix(1,7,7),fun=fill.na,pad=FALSE,na.rm=FALSE)
test = cover(epa.o3.trend[[2]],rr)
o3tfill = mask(test,omi.o3.usa.trend[[1]])

png(width=8,height=5,res=300,units="in",file="Plots/Rasters/O3surf_2005.png")
  mycols = colorRampPalette(c("grey90","blue","green","yellow","orange","red","darkred"))
  plot(usaborders,border="darkgrey",lwd=3)
  plot(o3fill,col=mycols(200),bty="n",box=FALSE,xaxt="n",yaxt="n",add=T)
dev.off()  

png(width=8,height=5,res=300,units="in",file="Plots/Rasters/O3surf_NoFill_2005.png")
  mycols = colorRampPalette(c("grey90","blue","green","yellow","orange","red","darkred"))
  plot(usaborders,border="darkgrey",lwd=3)
  plot(epa.o3.trend[[1]],col=mycols(200),bty="n",box=FALSE,xaxt="n",yaxt="n",add=T)
dev.off()  

png(width=8,height=5,res=300,units="in",file="Plots/Rasters/O3surf_Trend.png")
  mycols = colorRampPalette(c("darkgreen","white","brown4"))
  bks = seq(-1,1,0.01)
  cols = mycols(length(bks))
  bks2 = seq(-1,1,0.25)
  cols2 = mycols(length(bks2))
  plot(usaborders,border="darkgrey",lwd=3)
  plot(o3tfill,col=cols,breaks=bks,bty="n",box=FALSE,xaxt="n",yaxt="n",add=T,legend=FALSE)
  plot(o3tfill,col=cols2,breaks=bks2,legend.only=TRUE,add=TRUE)
  points(plant.locations2[plant.locations2$oldcoalplant==1,],col="red",pch=16,cex=0.35)
  points(plant.locations2[plant.locations2$newngplant==1,],col="blue",pch=16,cex=0.25)
dev.off() 

png(width=8,height=5,res=300,units="in",file="Plots/Rasters/NO2_Trend.png")
  mycols = colorRampPalette(c("darkgreen","white","brown4"))
  bks = seq(-160,160,1)
  cols = mycols(length(bks))
  bks2 = seq(-160,160,40)
  cols2 = mycols(length(bks2))
  plot(usaborders,border="darkgrey",lwd=3)
  plot(no2.trend[[2]],col=cols,breaks=bks,bty="n",box=FALSE,xaxt="n",yaxt="n",add=T,legend=FALSE)
  plot(no2.trend[[2]],col=cols2,breaks=bks2,legend.only=TRUE,add=TRUE)
  points(plant.locations2[plant.locations2$oldcoalplant==1,],col="red",pch=16,cex=0.35)
  points(plant.locations2[plant.locations2$newngplant==1,],col="blue",pch=16,cex=0.25)
dev.off()  

png(width=8,height=5,res=300,units="in",file="Plots/Rasters/SO2_Trend.png")
  mycols = colorRampPalette(c("darkgreen","white","brown4"))
  bks = seq(-0.05,0.05,0.001)
  cols = mycols(length(bks))
  bks2 = seq(-0.05,0.05,0.01)
  cols2 = mycols(length(bks2))
  plot(usaborders,border="darkgrey",lwd=3)
  plot(so2.trend[[2]],col=cols,breaks=bks,bty="n",box=FALSE,xaxt="n",yaxt="n",add=T,legend=FALSE)
  plot(so2.trend[[2]],col=cols2,breaks=bks2,legend.only=TRUE,add=TRUE)
  points(plant.locations2[plant.locations2$oldcoalplant==1,],col="red",pch=16,cex=0.35)
  points(plant.locations2[plant.locations2$newngplant==1,],col="blue",pch=16,cex=0.25)
dev.off()  


# -----------------------------------------------------------------------------------------------
# Plot Annual sets
# -----------------------------------------------------------------------------------------------

rlist.annual = ls(pattern=".a.pp")

plot.a.pollvars = function(varstr) {
  var = get(varstr)
  xx = dim(var)[2]
  png(width=5,height=3,units="in",pointsize=8,res=300,file=paste("Plots/Parameter Trends - Annual/",varstr,".png",sep=""))
  par(mar=c(4,5,1,1),las=1)
  plot(-100,-100,xlim=c(1,xx),ylim=0.8*c(min(var[,1],na.rm=TRUE),1.2*max(var[,1],na.rm=TRUE)),xlab="Date",xaxt="n",ylab="",cex.lab=2,cex.axis=2)
  title(main=varstr)
  grid(col="darkgrey")
  for (i in 1:dim(var)[1]) {
    lines(1:xx,var[i,],col="red",lwd=0.5,lty=3)
  }
  lines(1:xx,colMeans(var,na.rm=TRUE),col="blue",lwd=3)
  axis(side=1,at=1:12,labels=yrs)
  dev.off()
}

lapply(rlist.annual,plot.a.pollvars)

# -----------------------------------------------------------------------------------------------
# Plot Monthly sets
# -----------------------------------------------------------------------------------------------

rlist = ls(pattern="pp")
rlist = rlist[!grepl(rlist,pattern=".data")]
rlist = rlist[!grepl(rlist,pattern=".a.")]

rlist.monthly = rlist[!grepl(rlist,pattern="vdpm2p5")]
rlist.monthly = rlist.monthly[!grepl(rlist.monthly,pattern="raster")]

plot.pollvars = function(varstr) {
  var = get(varstr)
  xx = dim(var)[2]
  png(width=5,height=3,units="in",pointsize=8,res=300,file=paste("Plots/Parameter Trends - Monthly/",varstr,".png",sep=""))
  par(mar=c(4,5,1,1),las=1)
  plot(-100,-100,xlim=c(1,xx),ylim=c(min(var[,1],na.rm=TRUE),1.5*max(var[,1],na.rm=TRUE)),xlab="Date",xaxt="n",ylab="",cex.lab=2,cex.axis=2)
  title(main=varstr)
  grid(col="darkgrey")
  for (i in 1:dim(var)[1]) {
    lines(1:xx,var[i,],col="red",lwd=0.5,lty=3)
  }
  lines(1:xx,colMeans(var,na.rm=TRUE),col="blue",lwd=3)
  axis(side=1,at=(1:12)*12,labels=yrs)
  dev.off()
}

lapply(rlist.monthly,plot.pollvars)
