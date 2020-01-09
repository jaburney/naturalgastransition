# --------------------------------------------------------------------------------------------
# Neighbors and Examples.R
# --------------------------------------------------------------------------------------------
# Produces figures showing spatial setup of project and examples
# --------------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------------------
# Compare radii to county areas to justify distance choices (Supplementary Figure 4)
# ------------------------------------------------------------------------------------------

# Calculate county areas
for (i in 1:length(counties48)) {
  counties48$area[i] = counties48@polygons[[i]]@area*1e-6
}

png(width=8,height=6,units="in",res=300,file="Plots/SuppFig4.png")
  par(mar=c(5,5,1,1),las=1)
  hist(counties48$area,breaks=100,xlab=expression(paste("Area [",km^2,"]",sep="")),ylab="# of Counties",main="",col="grey",cex.lab=1.5,cex.axis=1.2)
  abline(v=c(pi*25^2,pi*50^2,pi*100^2,pi*200^2),col="orange",lty=2,lwd=2)
  text(x=c(pi*25^2,pi*50^2,pi*100^2),y=c(800,600,400),c("r = 25km","r = 50km","r = 100km"),col="blue",cex=2,pos=4)
dev.off()

# ------------------------------------------------------------------------------------------
# Plot Example Location (Extended Data Figure 6)
# ------------------------------------------------------------------------------------------

# Generate simplified set
coaloff = fullpanel.plants.annual[fullpanel.plants.annual$oldcoalplant==1,]
  coaloff$post = "post"
  coaloff$post[coaloff$tau_off<0] = "pre"

delta.pm = coaloff %>% group_by(facunitid,post) %>% summarise(pm1km=mean(pm.1km),pm10km=mean(pm.10km),pm50km=mean(pm.50km),pds=max(abs(tau_off),na.rm=T))
  delta.pm = arrange(delta.pm,facunitid,post)
  delta.pm = as.data.frame(delta.pm[delta.pm$pds>3,])


# Pick Example Unit
# Cobb County, Georgia
# Shut down 2012
id = 427

GA=states48[states48$NAME_1=="Georgia",]
  GA_C=counties48[counties48$State=="GEORGIA",]
  GA_COBB=counties48[counties48$State=="GEORGIA" & counties48$NAME_2=="Cobb",]

exam = coaloff[coaloff$facunitid==id,]
smry = exam %>% group_by(post) %>% summarise(mnpm=mean(pm.1km,na.rm=T),mnnox=mean(no2.0km,na.rm=T),mnsox=mean(so2.0km,na.rm=T),mno3=mean(epa.o3.0km))
xx = min(exam$tau_off):max(exam$tau_off)

# Get annular differences for example location
exam$pm.center=exam$pm.1km
exam$pm.ring1=(exam$pm.10km*pi*10^2-exam$pm.center*pi*1^2)/(pi*(10^2-1^2)) 
exam$pm.ring2=(exam$pm.50km*pi*50^2-exam$pm.10km*pi*10^2)/(pi*(50^2-10^2)) 
exam$pm.ring3=(exam$pm.100km*pi*100^2-exam$pm.50km*pi*50^2)/(pi*(100^2-50^2)) 

exam$no2.center=exam$no2.0km
exam$no2.ring1=(exam$no2.25km*pi*10^2-exam$no2.center*pi*1^2)/(pi*(10^2-1^2)) 
exam$no2.ring2=(exam$no2.50km*pi*50^2-exam$no2.25km*pi*10^2)/(pi*(50^2-10^2)) 
exam$no2.ring3=(exam$no2.100km*pi*100^2-exam$no2.50km*pi*50^2)/(pi*(100^2-50^2)) 

exam$so2.center=exam$so2.0km
exam$so2.ring1=(exam$so2.25km*pi*10^2-exam$so2.center*pi*1^2)/(pi*(10^2-1^2)) 
exam$so2.ring2=(exam$so2.50km*pi*50^2-exam$so2.25km*pi*10^2)/(pi*(50^2-10^2)) 
exam$so2.ring3=(exam$so2.100km*pi*100^2-exam$so2.50km*pi*50^2)/(pi*(100^2-50^2)) 

exam$o3.center=exam$epa.o3.0km
exam$o3.ring1=(exam$epa.o3.25km*pi*10^2-exam$o3.center*pi*1^2)/(pi*(10^2-1^2)) 
exam$o3.ring2=(exam$epa.o3.50km*pi*50^2-exam$epa.o3.25km*pi*10^2)/(pi*(50^2-10^2)) 
exam$o3.ring3=(exam$epa.o3.100km*pi*100^2-exam$epa.o3.50km*pi*50^2)/(pi*(100^2-50^2)) 

# Create Maps
png(width=10,height=8,units="in",res=300,file="Plots/FigED6_map1.png")
  plot(usaborders)
  lines(GA)
  points(plant.locations2[plant.locations2$facunitid==id,],pch=16,col="red",cex=2)
dev.off()

png(width=10,height=8,units="in",res=300,file="Plots/FigED6_map2.png")
  plot(GA)
  lines(GA_C,col="blue")
  lines(GA)
  points(plant.locations2[plant.locations2$facunitid==id,],pch=16,col="red",cex=2)
dev.off()

# Plot ambient changes - PM2.5
png(width=10,height=7,units="in",res=300,file="Plots/FigED6a.png")
  yy = c(0,15)
  par(mar=c(5,6,3,6),las=1)
  plot(xx,-100+xx,xlim=c(-7,4),ylim=yy,xlab="Time Before/After Shutdown [years]",ylab="",cex.axis=1.5,cex.lab=2)
  polygon(x=c(-1,-1,0,0,-1),y=c(-10,25,25,-10,-10),col="darkgrey",border=NA)
  grid(col="darkgrey")
  lines(xx,exam$pm.1km,col="black",lwd=4)
  lines(xx,exam$pm.10km,col="black",lty=2)
  lines(xx,exam$pm.50km,col="black",lty=3)
  lines(xx,exam$pm.100km,col="black",lty=4)
  lines(xx,exam$grossload/2e5,col="blue",lwd=5)
  axis(side=4,at=seq(yy[1],yy[2],5),labels=seq(yy[1]*0.2,yy[2]*0.2,1),col.axis="blue",cex.axis=1.5)
  mtext(side=4,"Total Gross Load [1e6 MWh]",col="blue",las=0,line=4,cex=2)
  mtext(side=2,expression(paste(PM[2.5]," [",mu,"g ",m^-3,"]",sep="")),las=0,line=3.5,cex=2)
  segments(x0=c(xx[1],0),x1=c(0,max(xx)),y0=rev(smry$mnpm),col="red",lty=2,lwd=2)
  text(x=c(-3.5,2),y=c(14,12.5),paste(signif(rev(smry$mnpm),3)),col="red",cex=1.5)
dev.off()

# Plot ambient changes - O3
png(width=10,height=7,units="in",res=300,file="Plots/FigED6b.png")
  yy = c(0,15)
  par(mar=c(5,6,3,6),las=1)
  plot(xx,-100+xx,xlim=c(-7,4),ylim=yy,xlab="Time Before/After Shutdown [years]",ylab="",cex.axis=1.5,cex.lab=2,yaxt="n")
  polygon(x=c(-1,-1,0,0,-1),y=c(-10,25,25,-10,-10),col="darkgrey",border=NA)
  grid(col="darkgrey")
  lines(xx,exam$epa.o3.0km/4,col="black",lwd=3)
  lines(xx,exam$epa.o3.50km/4,col="black",lty=2)
  lines(xx,exam$epa.o3.100km/4,col="black",lty=3)
  lines(xx,exam$grossload/2e5,col="blue",lwd=5)
  axis(side=2,at=seq(yy[1],yy[2],5),labels=seq(yy[1]*4,yy[2]*4,20),cex.axis=1.5)
  axis(side=4,at=seq(yy[1],yy[2],5),labels=seq(yy[1]*0.2,yy[2]*0.2,1),col.axis="blue",cex.axis=1.5)
  mtext(side=2,expression(paste(O[3]," [ppb]",sep="")),las=0,line=3.5,cex=2)
  mtext(side=4,"Total Gross Load [1e6 MWh]",col="blue",las=0,line=4,cex=2)
  segments(x0=c(xx[1],0),x1=c(0,max(xx)),y0=rev(smry$mno3)/4,col="red",lty=2,lwd=2)
  text(x=c(-3.5,2),y=c(12,11),paste(signif(rev(smry$mno3),3)/4),col="red",cex=1.5)
dev.off()

# Plot ambient changes - SO2
png(width=10,height=7,units="in",res=300,file="Plots/FigED6c.png")
  yy = c(0,15)
  par(mar=c(5,6,3,6),las=1)
  plot(xx,-100+xx,xlim=c(-7,4),ylim=yy,xlab="Time Before/After Shutdown [years]",ylab="",cex.axis=1.5,cex.lab=2,yaxt="n")
  polygon(x=c(-1,-1,0,0,-1),y=c(-10,25,25,-10,-10),col="darkgrey",border=NA)
  grid(col="darkgrey")
  lines(xx,exam$so2.0km*50,col="black",lwd=3)
  lines(xx,exam$so2.50km*50,col="black",lty=2)
  lines(xx,exam$so2.100km*50,col="black",lty=3)
  lines(xx,exam$grossload/2e5,col="blue",lwd=5)
  axis(side=2,at=seq(yy[1],yy[2],5),labels=seq(yy[1]/50,yy[2]/50,0.1),cex.axis=1.5)
  axis(side=4,at=seq(yy[1],yy[2],5),labels=seq(yy[1]*0.2,yy[2]*0.2,1),col.axis="blue",cex.axis=1.5)
  mtext(side=2,expression(paste(SO[2]," [DU]",sep="")),las=0,line=3.5,cex=2)
  mtext(side=4,"Total Gross Load [1e6 MWh]",col="blue",las=0,line=4,cex=2)
  segments(x0=c(xx[1],0),x1=c(0,max(xx)),y0=rev(smry$mnsox)*50,col="red",lty=2,lwd=2)
  text(x=c(-3.5,2),y=c(12,8),paste(signif(rev(smry$mnsox),3)),col="red",cex=1.5)
dev.off()

# Plot ambient changes - NO2
png(width=10,height=7,units="in",res=300,file="Plots/FigED6d.png")
  yy = c(0,15)
  par(mar=c(5,6,3,6),las=1)
  plot(xx,-100+xx,xlim=c(-7,4),ylim=yy,xlab="Time Before/After Shutdown [years]",ylab="",cex.axis=1.5,cex.lab=2,yaxt="n")
  polygon(x=c(-1,-1,0,0,-1),y=c(-10,25,25,-10,-10),col="darkgrey",border=NA)
  grid(col="darkgrey")
  lines(xx,exam$no2.0km/50,col="black",lwd=3)
  lines(xx,exam$no2.50km/50,col="black",lty=2)
  lines(xx,exam$no2.100km/50,col="black",lty=3)
  lines(xx,exam$grossload/2e5,col="blue",lwd=5)
  axis(side=2,at=seq(yy[1],yy[2],5),labels=seq(yy[1]*50,yy[2]*50,200),cex.axis=1.5)
  axis(side=4,at=seq(yy[1],yy[2],5),labels=seq(yy[1]*0.2,yy[2]*0.2,1),col.axis="blue",cex.axis=1.5)
  mtext(side=2,expression(paste(NO[2]," [DU]",sep="")),las=0,line=3.5,cex=2)
  mtext(side=4,"Total Gross Load [1e6 MWh]",col="blue",las=0,line=4,cex=2)
  segments(x0=c(xx[1],0),x1=c(0,max(xx)),y0=rev(smry$mnnox/50),col="red",lty=2,lwd=2)
  text(x=c(-3.5,2),y=c(14,10),paste(signif(rev(smry$mnnox),3)),col="red",cex=1.5)
dev.off()


