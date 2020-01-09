# --------------------------------------------------------------------------------------------
# Environmental Rasters.R
# --------------------------------------------------------------------------------------------
# Plot various data sets
# --------------------------------------------------------------------------------------------

# Aggregate NO2 and SO2 to annual and fit 
# (Somehow didn't happen earlier)
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

# Fill EPA data for prettier plots
# -----------------------------------------------------------------------

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

# Figure ED3 
# -----------------------------------------------------------------------

# PM2.5
png(width=8,height=5,res=300,units="in",file="Plots/FigED3a.png")
  mycols = colorRampPalette(c("grey90","blue","green","yellow","orange","red","darkred"))
  plot(usaborders,border="darkgrey",lwd=3)
  plot(vd.usa.trend[[1]],col=mycols(200),bty="n",box=FALSE,xaxt="n",yaxt="n",add=T)
dev.off()  

png(width=8,height=5,res=300,units="in",file="Plots/FigED3b.png")
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

# O3
png(width=8,height=5,res=300,units="in",file="Plots/FigED3c.png")
  mycols = colorRampPalette(c("grey90","blue","green","yellow","orange","red","darkred"))
  plot(usaborders,border="darkgrey",lwd=3)
  plot(o3fill,col=mycols(200),bty="n",box=FALSE,xaxt="n",yaxt="n",add=T)
dev.off()  

png(width=8,height=5,res=300,units="in",file="Plots/FigED3d.png")
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

# NO2
png(width=8,height=5,res=300,units="in",file="Plots/FigED3e.png")
  mycols = colorRampPalette(c("grey90","blue","green","yellow","orange","red","darkred"))
  plot(usaborders,border="darkgrey",lwd=3)
  plot(no2.trend[[1]],col=mycols(200),bty="n",box=FALSE,xaxt="n",yaxt="n",add=T)
dev.off()  

png(width=8,height=5,res=300,units="in",file="Plots/FigED3f.png")
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

# SO2
png(width=8,height=5,res=300,units="in",file="Plots/FigED3g.png")
  mycols = colorRampPalette(c("grey90","blue","green","yellow","orange","red","darkred"))
  plot(usaborders,border="darkgrey",lwd=3)
  plot(so2.trend[[1]],col=mycols(200),bty="n",box=FALSE,xaxt="n",yaxt="n",add=T)
dev.off()  

png(width=8,height=5,res=300,units="in",file="Plots/FigED3h.png")
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


# Supplemental Figure 3 (maps)
# -----------------------------------------------------------------------

png(width=8,height=5,res=300,units="in",file="Plots/SuppFig3a.png")
  mycols = colorRampPalette(c("grey90","blue","green","yellow","orange","red","darkred"))
  plot(usaborders,border="darkgrey",lwd=3)
  plot(epa.pm.avg,col=mycols(200),bty="n",box=FALSE,xaxt="n",yaxt="n",add=T)
dev.off()  

png(width=8,height=5,res=300,units="in",file="Plots/SuppFig3b.png")
  mycols = colorRampPalette(c("grey90","blue","green","yellow","orange","red","darkred"))
  plot(usaborders,border="darkgrey",lwd=3)
  plot(epa.o3.avg,col=mycols(200),bty="n",box=FALSE,xaxt="n",yaxt="n",add=T)
dev.off()  

# Satellite and Surface Comparison
# -----------------------------------------------------------------------

cols = rainbow(length(yrs))

xx = data.frame(pmsurf=seq(0,20,0.1))

png(width=6,height=5,units="in",res=300,file="Plots/SuppFig3c.png")
  par(mar=c(5,5,1,1),las=1)
  plot(-20,-20,xlab=expression(paste("Surface ",PM[2.5]," [",mu,"g ",m^-3,"]")),ylab=expression(paste("Satellite ",PM[2.5]," [",mu,"g ",m^-3,"]")),xlim=c(0,20),ylim=c(0,20),cex.axis=1.2,cex.lab=1.5)
  grid(col="darkgrey")
  for (i in 1:length(yrs)) {
    d=final.county.data[final.county.data$Year==yrs[i],]
    points(d$pmsurf,d$pm,col=cols[i],pch=16,cex=0.4)
    lines(xx$pmsurf,predict(lm(pm~pmsurf,data=d),newdata=xx),col=cols[i],lwd=2)
  }
  legend("bottomright",bty="n",pch=16,pt.cex=0.6,legend=yrs,col=cols)
  lines(xx$pmsurf,predict(lm(pm~pmsurf,data=final.county.data),newdata=xx),col="black",lwd=3,lty=2)
  box(lwd=2)
dev.off()

png(width=6,height=5,units="in",res=300,file="Plots/SuppFig3d.png")
  par(mar=c(5,5,1,1),las=1)
  plot(-20,-20,xlab="Surface Ozone [ppb]",ylab="Column Ozone [DU]",xlim=c(20,60),ylim=c(260,360),cex.axis=1.2,cex.lab=1.5)
  grid(col="darkgrey")
  for (i in 1:length(yrs)) {
    d=final.county.data[final.county.data$Year==yrs[i],]
    points(d$o3surf,d$o3,col=cols[i],pch=16,cex=0.4)
  }
  box(lwd=2)
dev.off()
