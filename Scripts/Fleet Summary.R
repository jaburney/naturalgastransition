# -----------------------------------------------------------------------
# Fleet Summary.R
# -----------------------------------------------------------------------
# Produces summaries (outputs csv files) and plots maps of facilities
# and emissions from continental US electric power production
# -----------------------------------------------------------------------

# -----------------------------------------------------------------------
# Summarize and Produce Basic Fleet Tables
# -----------------------------------------------------------------------

pp.data$fueltype[is.na(pp.data$fueltype)] = "Other"
pp.data$fueltype2[is.na(pp.data$fueltype2)] = "Coal+Other"

# Load and Emissions Summary Tables
fleet_by_year = pp.data %>% group_by(year,fueltype) %>% summarize(co2=sum(co2emissions,na.rm=TRUE),
                                                                  nox=sum(noxemissions,na.rm=TRUE),
                                                                  so2=sum(so2emissions,na.rm=TRUE),
                                                                  load=sum(grossload,na.rm=TRUE),
                                                                  steam=sum(steamload,na.rm=TRUE),
                                                                  units=sum(operating,na.rm=TRUE))

gen = reshape(data.frame(fleet_by_year[,c("fueltype","year","load")]),idvar="fueltype",v.names="load",timevar="year",direction="wide")
  gen[4,2:13] = colSums(gen[,2:13])
  gen[4,1] = "Total"

emiss = fleet_by_year %>% group_by(year) %>% summarize(co2=sum(co2),so2=sum(so2),nox=sum(nox))

# Write Out Tables
write.csv(fleet_by_year,"Analysis/AMPD Fleet Summary.csv",row.names=FALSE)
write.csv(gen,"Analysis/AMPD Generation by Year.csv",row.names=FALSE)
write.csv(emiss,"Analysis/AMPD Emissions by Year.csv",row.names=FALSE)

# -----------------------------------------------------------------------
# Fleet Maps (Main Manuscript Figure 1a,b)
# -----------------------------------------------------------------------

# Fleet maps
fleet05 = subset(pp.data,pp.data$year==2005 & pp.data$operating==1)
  coordinates(fleet05) = ~longitude+latitude
  crs(fleet05) = latlon.crs
  fleet05 = spTransform(fleet05,crs(usaborders))

fleet16 = subset(pp.data,pp.data$year==2016 & pp.data$operating==1)
  coordinates(fleet16) = ~longitude+latitude
  crs(fleet16) = latlon.crs
  fleet16 = spTransform(fleet16,crs(usaborders))

numc05=dim(fleet05[fleet05$fueltype=="Coal" & !is.na(fleet05$fueltype),])[1]
numc16=dim(fleet16[fleet16$fueltype=="Coal" & !is.na(fleet16$fueltype),])[1]
numng05=dim(fleet05[fleet05$fueltype=="Natural Gas" & !is.na(fleet05$fueltype),])[1]
numng16=dim(fleet16[fleet16$fueltype=="Natural Gas" & !is.na(fleet16$fueltype),])[1]
numo05=dim(fleet05[fleet05$fueltype=="Other" & !is.na(fleet05$fueltype),])[1]
numo16=dim(fleet16[fleet16$fueltype=="Other" & !is.na(fleet16$fueltype),])[1]

lg05 = paste(c("Coal (","Natural Gas (","Other ("),c(numc05,numng05,numo05),c(")",")",")"),sep="")
lg16 = paste(c("Coal (","Natural Gas (","Other ("),c(numc16,numng16,numo16),c(")",")",")"),sep="")

png(units="in",res=300,width=8,height=6,file="Plots/Fig1a.png")
  plot(usaborders,border="darkgrey",lwd=3)
  plot(fleet05[fleet05$fueltype=="Coal" & !is.na(fleet05$fueltype),],pch=16,cex=0.8,col="red",add=TRUE)
  plot(fleet05[fleet05$fueltype=="Natural Gas" & !is.na(fleet05$fueltype),],pch=16,cex=0.8,col="blue",add=TRUE)
  plot(fleet05[fleet05$fueltype=="Other" & !is.na(fleet05$fueltype),],pch=16,cex=0.8,col="orange",add=TRUE)
  legend("bottomleft",inset=0.05,col=c("red","blue","orange"),pch=16,legend=lg05,bty="n")
dev.off()

png(units="in",res=300,width=8,height=6,file="Plots/Fig1b.png")
  plot(usaborders,border="darkgrey",lwd=3)
  plot(fleet16[fleet16$fueltype=="Coal" & !is.na(fleet16$fueltype),],pch=16,cex=0.8,col="red",add=TRUE)
  plot(fleet16[fleet16$fueltype=="Natural Gas" & !is.na(fleet16$fueltype),],pch=16,cex=0.8,col="blue",add=TRUE)
  plot(fleet16[fleet16$fueltype=="Other" & !is.na(fleet16$fueltype),],pch=16,cex=0.8,col="orange",add=TRUE)
  legend("bottomleft",inset=0.05,col=c("red","blue","orange"),pch=16,legend=lg16,bty="n")
dev.off()

# -----------------------------------------------------------------------
# Total Generation and Emissions (Main Manuscript Figure 1c,d)
# -----------------------------------------------------------------------

# Generation
png(width=8,height=6,units="in",res=300,pointsize=12,file="Plots/Fig1c.png")
  par(mar=c(5,6,1,1),las=1)
  plot(2005:2016,-200*2005:2016,ylim=c(0,3),xlab="",ylab="Gross Load (Billion MWh)",cex.lab=1.5,cex.axis=1.5)
  grid(col="darkgrey")
  polygon(x=c(2005:2016,rev(2005:2016)),y=c(gen[1,2:13]*1e-9,0*(2005:2016)),col="red")
  polygon(x=c(2005:2016,rev(2005:2016)),y=c(gen[1,2:13]*1e-9+gen[3,2:13]*1e-9,rev(gen[1,2:13]*1e-9)),col="orange")
  polygon(x=c(2005:2016,rev(2005:2016)),y=c((gen[2,2:13]+gen[1,2:13]+gen[3,2:13])*1e-9,rev((gen[3,2:13]+gen[1,2:13])*1e-9)),col="blue")
  text(2010.5,1,"Coal",cex=3)
  text(2010.5,2.4,"Natural Gas",cex=3)
  box(which="plot",lwd=3,col="black")
dev.off()

# Emissions
png(width=8,height=6,units="in",res=300,pointsize=12,file="Plots/Fig1d.png")
  par(mar=c(5,6,1,5),las=1)
  plot(2005:2016,-200*2005:2016,ylim=c(0,3),xlab="",ylab=expression(paste("Billion Tons ",CO[2])),cex.lab=1.5,cex.axis=1.5)
  axis(side=4,at=seq(0,3,0.5),labels=seq(0,30,5),cex.lab=1.5,cex.axis=1.5)
  mtext(side=4,las=0,text=expression(paste("Million Tons ",SO[2]," or NOx")),cex.lab=1.5,line=3.5,cex=1.5)
  grid(col="darkgrey")
  lines(2005:2016,y=emiss$co2*1e-9,col="brown4",lwd=10)
  lines(2005:2016,y=emiss$so2*1e-7,col="darkorange",lwd=10)
  lines(2005:2016,y=emiss$nox*1e-7,col="yellow3",lwd=10)
  text(2006,2.0,expression(CO[2]),col="brown4",cex=2)
  text(2006,0.7,expression(SO[2]),col="darkorange",cex=2)
  text(2006,0.1,expression(NO[x]),col="yellow3",cex=2)
  box(which="plot",lwd=3,col="black")
dev.off()

# -----------------------------------------------------------------------
# Maps of Unit Subsets (Extended Data Figure 1, upper panels)
# -----------------------------------------------------------------------

png(width=6,height=5,res=300,units="in",file="Plots/FigED1a_map.png")
  plot(usaborders)
  test = plant.locations2[plant.locations2$oldcoalplant==1 & !is.na(plant.locations2$oldcoalplant),]
  nu = sum(!is.na(pp.data$year[pp.data$tau_off==0 & pp.data$oldcoalplant==1]))
  nf = length(unique(pp.data$locationid[pp.data$tau_off==0 & pp.data$oldcoalplant==1]))
  points(test,col="red",pch=16)
  title(sub=paste(nu, " Units at ",nf," Facilities",sep=""),cex.sub=2)
dev.off()  

png(width=6,height=5,res=300,units="in",file="Plots/FigED1b_map.png")
  plot(usaborders)
  test = plant.locations2[plant.locations2$newngplant==1 & !is.na(plant.locations2$newngplant),]
  nu = sum(!is.na(pp.data$year[pp.data$tau_on==0 & pp.data$newngplant==1]))
  nf = length(unique(pp.data$locationid[pp.data$tau_on==0 & pp.data$newngplant==1]))
  points(test,col="blue",pch=16)
  title(sub=paste(nu, " Units at ",nf," Facilities",sep=""),cex.sub=2)
dev.off()

png(width=6,height=5,res=300,units="in",file="Plots/FigED1c_map.png")
  plot(usaborders)
  test = plant.locations2[plant.locations2$newcoalplant==1 & !is.na(plant.locations2$newcoalplant),]
  nu = sum(!is.na(pp.data$year[pp.data$tau_on==0 & pp.data$newcoalplant==1]))
  nf = length(unique(pp.data$locationid[pp.data$tau_on==0 & pp.data$newcoalplant==1]))
  points(test,col="red",pch=16)
  title(sub=paste(nu, " Units at ",nf," Facilities",sep=""),cex.sub=2)
dev.off()  

# -----------------------------------------------------------------------
# Histograms of Plant Subests (Extended Data Figure 1, lower panels)
# -----------------------------------------------------------------------

png(width=6,height=5,res=300,units="in",file="Plots/FigED1a_hist.png")
  hist(pp.data$year[pp.data$tau_off==0 & pp.data$oldcoalplant==1],xlab="",ylab="",main="",las=1,breaks=(2005:2016)+0.5,labels=2005:2016,axes=TRUE,col="red")
  title(main="Coal Units Off")
dev.off()  

png(width=6,height=5,res=300,units="in",file="Plots/FigED1b_hist.png")
  hist(pp.data$year[pp.data$tau_on==0 & pp.data$newngplant==1],xlab="",ylab="",main="",las=1,breaks=(2005:2016)+0.5,labels=2005:2016,axes=TRUE,col="blue")
  title(main="Natural Gas Units On")
dev.off()  

png(width=6,height=5,res=300,units="in",file="Plots/FigED1c_hist.png")
  hist(pp.data$year[pp.data$tau_on==0 & pp.data$newcoalplant==1],xlab="",ylab="",main="",las=1,breaks=(2005:2016)+0.5,labels=2005:2016,axes=TRUE,col="red",density=20)
  title(main="Coal Units On")
dev.off()  

# -----------------------------------------------------------------------
# Emissions Changes at Shut-down and Start-Up (Extended Data Figure 2)
# -----------------------------------------------------------------------

# Old Coal
ind = (pp.data$oldcoalplant==1 & !is.na(pp.data$oldcoalplant) & abs(pp.data$tau_off) < 5)

png(width=16,height=12,res=300,units="cm",file="Plots/FigED2a_load.png")
  par(mar=c(5,8,1,1),las=1)
  boxplot(grossload~tau_off,data=pp.data,subset=ind,range=1,outline=FALSE,boxwex=0.5,xlab="Leads / Lags of Switch [Years]",ylab="",border="red",cex.axis=1.2,cex.lab=1.5)
  abline(h=0,v=5,col="darkgrey",lwd=2)
  grid(col="darkgrey")
  boxplot(grossload~tau_off,data=pp.data,subset=ind,range=1,outline=FALSE,boxwex=0.5,xlab="",ylab="",xaxt="n",yaxt="n",border="black",col="red",lwd=2,add=T)
  mtext(expression(paste("Annual Generation [MWh]")),side=2,line=5,las=0,cex=1.5)
dev.off()

png(width=16,height=12,res=300,units="cm",file="Plots/FigED2a_co2.png")
  par(mar=c(5,8,1,1),las=1)
  boxplot(co2emissions~tau_off,data=pp.data,subset=ind,range=1,outline=FALSE,boxwex=0.5,xlab="Leads / Lags of Switch [Years]",ylab="",border="red",cex.axis=1.2,cex.lab=1.5)
  abline(h=0,v=5,col="darkgrey",lwd=2)
  grid(col="darkgrey")
  boxplot(co2emissions~tau_off,data=pp.data,subset=ind,range=1,outline=FALSE,boxwex=0.5,xlab="",ylab="",xaxt="n",yaxt="n",border="black",col="red",lwd=2,add=T)
  mtext(expression(paste("Annual ",CO[2]," Emissions [tons]")),side=2,line=5,las=0,cex=1.5)
dev.off()

png(width=16,height=12,res=300,units="cm",file="Plots/FigED2a_nox.png")
  par(mar=c(5,8,1,1),las=1)
  boxplot(noxemissions~tau_off,data=pp.data,subset=ind,range=1,outline=FALSE,boxwex=0.5,xlab="Leads / Lags of Switch [Years]",ylab="",border="red",cex.axis=1.2,cex.lab=1.5)
  abline(h=0,v=5,col="darkgrey",lwd=2)
  grid(col="darkgrey")
  boxplot(noxemissions~tau_off,data=pp.data,subset=ind,range=1,outline=FALSE,boxwex=0.5,xlab="",ylab="",xaxt="n",yaxt="n",border="black",col="red",lwd=2,add=T)
  mtext(expression(paste("Annual ",NO[x]," Emissions [tons]")),side=2,line=5,las=0,cex=1.5)
dev.off()

png(width=16,height=12,res=300,units="cm",file="Plots/FigED2a_so2.png")
  par(mar=c(5,8,1,1),las=1)
  boxplot(so2emissions~tau_off,data=pp.data,subset=ind,range=1,outline=FALSE,boxwex=0.5,ylim=c(0,7000),xlab="Leads / Lags of Switch [Years]",ylab="",border="red",cex.axis=1.2,cex.lab=1.5)
  abline(h=0,v=5,col="darkgrey",lwd=2)
  grid(col="darkgrey")
  boxplot(so2emissions~tau_off,data=pp.data,subset=ind,range=1,outline=FALSE,boxwex=0.5,xlab="",ylab="",xaxt="n",yaxt="n",border="black",col="red",lwd=2,add=T)
  mtext(expression(paste("Annual ",SO[2]," Emissions [tons]")),side=2,line=5,las=0,cex=1.5)
dev.off()


# New NG
ind = (pp.data$newngplant==1 & !is.na(pp.data$newngplant) & abs(pp.data$tau_on) < 5)

png(width=16,height=12,res=300,units="cm",file="Plots/FigED2b_load.png")
  par(mar=c(5,8,1,1),las=1)
  boxplot(grossload~tau_on,data=pp.data,subset=ind,range=1,outline=FALSE,boxwex=0.5,xlab="Leads / Lags of Switch [Years]",ylab="",border="blue",cex.axis=1.2,cex.lab=1.5)
  abline(h=0,v=5,col="darkgrey",lwd=2)
  grid(col="darkgrey")
  boxplot(grossload~tau_on,data=pp.data,subset=ind,range=1,outline=FALSE,boxwex=0.5,xlab="",ylab="",xaxt="n",yaxt="n",border="black",col="blue",lwd=2,add=T)
  mtext(expression(paste("Annual Generation [MWh]")),side=2,line=5,las=0,cex=1.5)
  text(1,1.8e6,"*",cex=3,col="blue")
dev.off()

png(width=16,height=12,res=300,units="cm",file="Plots/FigED2b_co2.png")
  par(mar=c(5,8,1,1),las=1)
  boxplot(co2emissions~tau_on,data=pp.data,subset=ind,range=1,outline=FALSE,boxwex=0.5,xlab="Leads / Lags of Switch [Years]",ylab="",border="blue",cex.axis=1.2,cex.lab=1.5)
  abline(h=0,v=5,col="darkgrey",lwd=2)
  grid(col="darkgrey")
  boxplot(co2emissions~tau_on,data=pp.data,subset=ind,range=1,outline=FALSE,boxwex=0.5,xlab="",ylab="",xaxt="n",yaxt="n",border="black",col="blue",lwd=2,add=T)
  mtext(expression(paste("Annual ",CO[2]," Emissions [tons]")),side=2,line=5,las=0,cex=1.5)
  text(1,1.2e6,"*",cex=3,col="blue")
dev.off()

png(width=16,height=12,res=300,units="cm",file="Plots/FigED2b_nox.png")
  par(mar=c(5,8,1,1),las=1)
  boxplot(noxemissions~tau_on,data=pp.data,subset=ind,range=1,outline=FALSE,boxwex=0.5,ylim=c(0,100),xlab="Leads / Lags of Switch [Years]",ylab="",border="blue",cex.axis=1.2,cex.lab=1.5)
  abline(h=0,v=5,col="darkgrey",lwd=2)
  grid(col="darkgrey")
  boxplot(noxemissions~tau_on,data=pp.data,subset=ind,range=1,outline=FALSE,boxwex=0.5,xlab="",ylab="",xaxt="n",yaxt="n",border="black",col="blue",lwd=2,add=T)
  mtext(expression(paste("Annual ",NO[x]," Emissions [tons]")),side=2,line=5,las=0,cex=1.5)
  text(1,100,"*",cex=3,col="blue")
dev.off()

png(width=16,height=12,res=300,units="cm",file="Plots/FigED2b_so2.png")
  par(mar=c(5,8,1,1),las=1)
  boxplot(so2emissions~tau_on,data=pp.data,subset=ind,range=1,outline=FALSE,boxwex=0.5,ylim=c(0,10),xlab="Leads / Lags of Switch [Years]",ylab="",border="blue",cex.axis=1.2,cex.lab=1.5)
  abline(h=0,v=5,col="darkgrey",lwd=2)
  grid(col="darkgrey")
  boxplot(so2emissions~tau_on,data=pp.data,subset=ind,range=1,outline=FALSE,boxwex=0.5,xlab="",ylab="",xaxt="n",yaxt="n",border="black",col="blue",lwd=2,add=T)
  mtext(expression(paste("Annual ",SO[2]," Emissions [tons]")),side=2,line=5,las=0,cex=1.5)
  text(1,10,"*",cex=3,col="blue")
dev.off()

# New Coal
ind = (pp.data$newcoalplant==1 & !is.na(pp.data$newcoalplant) & abs(pp.data$tau_on) < 5)

png(width=16,height=12,res=300,units="cm",file="Plots/FigED2c_load.png")
  par(mar=c(5,8,1,1),las=1)
  boxplot(grossload~tau_on,data=pp.data,subset=ind,range=1,outline=FALSE,boxwex=0.5,xlab="Leads / Lags of Switch [Years]",ylab="",border="red",cex.axis=1.2,cex.lab=1.5)
  abline(h=0,v=5,col="darkgrey",lwd=2)
  grid(col="darkgrey")
  boxplot(grossload~tau_on,data=pp.data,subset=ind,range=1,outline=FALSE,boxwex=0.5,xlab="",ylab="",xaxt="n",yaxt="n",border="red",col="white",lwd=2,add=T)
  mtext(expression(paste("Annual Generation [MWh]")),side=2,line=5,las=0,cex=1.5)
  text(1,7e6,"*",cex=3,col="red")
dev.off()

png(width=16,height=12,res=300,units="cm",file="Plots/FigED2c_co2.png")
  par(mar=c(5,8,1,1),las=1)
  boxplot(co2emissions~tau_on,data=pp.data,subset=ind,range=1,outline=FALSE,boxwex=0.5,xlab="Leads / Lags of Switch [Years]",ylab="",border="red",cex.axis=1.2,cex.lab=1.5)
  abline(h=0,v=5,col="darkgrey",lwd=2)
  grid(col="darkgrey")
  boxplot(co2emissions~tau_on,data=pp.data,subset=ind,range=1,outline=FALSE,boxwex=0.5,xlab="",ylab="",xaxt="n",yaxt="n",border="red",col="white",lwd=2,add=T)
  mtext(expression(paste("Annual ",CO[2]," Emissions [tons]")),side=2,line=5,las=0,cex=1.5)
  text(1,6.4e6,"*",cex=3,col="red")
dev.off()

png(width=16,height=12,res=300,units="cm",file="Plots/FigED2c_nox.png")
  par(mar=c(5,8,1,1),las=1)
  boxplot(noxemissions~tau_on,data=pp.data,subset=ind,range=1,outline=FALSE,boxwex=0.5,xlab="Leads / Lags of Switch [Years]",ylab="",border="red",cex.axis=1.2,cex.lab=1.5)
  abline(h=0,v=5,col="darkgrey",lwd=2)
  grid(col="darkgrey")
  boxplot(noxemissions~tau_on,data=pp.data,subset=ind,range=1,outline=FALSE,boxwex=0.5,xlab="",ylab="",xaxt="n",yaxt="n",border="red",col="white",lwd=2,add=T)
  mtext(expression(paste("Annual ",NO[x]," Emissions [tons]")),side=2,line=5,las=0,cex=1.5)
dev.off()

png(width=16,height=12,res=300,units="cm",file="Plots/FigED2c_so2.png")
  par(mar=c(5,8,1,1),las=1)
  boxplot(so2emissions~tau_on,data=pp.data,subset=ind,range=1,outline=FALSE,boxwex=0.5,ylim=c(0,7000),xlab="Leads / Lags of Switch [Years]",ylab="",border="red",cex.axis=1.2,cex.lab=1.5)
  abline(h=0,v=5,col="darkgrey",lwd=2)
  grid(col="darkgrey")
  boxplot(so2emissions~tau_on,data=pp.data,subset=ind,range=1,outline=FALSE,boxwex=0.5,xlab="",ylab="",xaxt="n",yaxt="n",border="red",col="white",lwd=2,add=T)
  mtext(expression(paste("Annual ",SO[2]," Emissions [tons]")),side=2,line=5,las=0,cex=1.5)
dev.off()

# -----------------------------------------------------------------------
# Emissions Changes (Supplementary Information Figure 2)
# -----------------------------------------------------------------------

df = fullpanel.plants.annual

co2emissions.delta = so2emissions.delta = noxemissions.delta = data.frame(matrix(NA,nrow=6,ncol=6))
row.names(co2emissions.delta) = row.names(so2emissions.delta) = row.names(noxemissions.delta) = c("on","onng","oncoal","off","offcoal","offother")
names(co2emissions.delta) = names(so2emissions.delta) = names(noxemissions.delta) = c("min","q1","med","mean","q3","max")

co2emissions.delta[1,]=summary(fullpanel.plants.annual$co2emissions[fullpanel.plants.annual$tau_on>=0 & is.finite(fullpanel.plants.annual$tau_on)])
so2emissions.delta[1,]=summary(fullpanel.plants.annual$so2emissions[fullpanel.plants.annual$tau_on>=0 & is.finite(fullpanel.plants.annual$tau_on)])
noxemissions.delta[1,]=summary(fullpanel.plants.annual$noxemissions[fullpanel.plants.annual$tau_on>=0 & is.finite(fullpanel.plants.annual$tau_on)])

co2emissions.delta[2,]=summary(fullpanel.plants.annual$co2emissions[fullpanel.plants.annual$tau_on>=0 & fullpanel.plants.annual$newngplant==1 & is.finite(fullpanel.plants.annual$tau_on)])
so2emissions.delta[2,]=summary(fullpanel.plants.annual$so2emissions[fullpanel.plants.annual$tau_on>=0 & fullpanel.plants.annual$newngplant==1 & is.finite(fullpanel.plants.annual$tau_on)])
noxemissions.delta[2,]=summary(fullpanel.plants.annual$noxemissions[fullpanel.plants.annual$tau_on>=0 & fullpanel.plants.annual$newngplant==1 & is.finite(fullpanel.plants.annual$tau_on)])

co2emissions.delta[3,]=summary(fullpanel.plants.annual$co2emissions[fullpanel.plants.annual$tau_on>=0 & fullpanel.plants.annual$newcoalplant==1 & is.finite(fullpanel.plants.annual$tau_on)])
so2emissions.delta[3,]=summary(fullpanel.plants.annual$so2emissions[fullpanel.plants.annual$tau_on>=0 & fullpanel.plants.annual$newcoalplant==1 & is.finite(fullpanel.plants.annual$tau_on)])
noxemissions.delta[3,]=summary(fullpanel.plants.annual$noxemissions[fullpanel.plants.annual$tau_on>=0 & fullpanel.plants.annual$newcoalplant==1 & is.finite(fullpanel.plants.annual$tau_on)])

co2emissions.delta[4,]=summary(fullpanel.plants.annual$co2emissions[fullpanel.plants.annual$tau_off<0 & !is.na(fullpanel.plants.annual$tau_off)])
so2emissions.delta[4,]=summary(fullpanel.plants.annual$so2emissions[fullpanel.plants.annual$tau_off<0 & !is.na(fullpanel.plants.annual$tau_off)])
noxemissions.delta[4,]=summary(fullpanel.plants.annual$noxemissions[fullpanel.plants.annual$tau_off<0 & !is.na(fullpanel.plants.annual$tau_off)])

co2emissions.delta[5,]=summary(fullpanel.plants.annual$co2emissions[fullpanel.plants.annual$tau_off<0 & fullpanel.plants.annual$oldcoalplant==1 & !is.na(fullpanel.plants.annual$oldcoalplant) & is.finite(fullpanel.plants.annual$tau_off)])
so2emissions.delta[5,]=summary(fullpanel.plants.annual$so2emissions[fullpanel.plants.annual$tau_off<0 & fullpanel.plants.annual$oldcoalplant==1 & !is.na(fullpanel.plants.annual$oldcoalplant) & is.finite(fullpanel.plants.annual$tau_off)])
noxemissions.delta[5,]=summary(fullpanel.plants.annual$noxemissions[fullpanel.plants.annual$tau_off<0 & fullpanel.plants.annual$oldcoalplant==1 & !is.na(fullpanel.plants.annual$oldcoalplant) & is.finite(fullpanel.plants.annual$tau_off)])

co2emissions.delta[6,]=summary(fullpanel.plants.annual$co2emissions[fullpanel.plants.annual$tau_off<0 & !is.na(fullpanel.plants.annual$oldcoalplant) & fullpanel.plants.annual$oldcoalplant==0 & is.finite(fullpanel.plants.annual$tau_off)])
so2emissions.delta[6,]=summary(fullpanel.plants.annual$so2emissions[fullpanel.plants.annual$tau_off<0 & !is.na(fullpanel.plants.annual$oldcoalplant) & fullpanel.plants.annual$oldcoalplant==0 & is.finite(fullpanel.plants.annual$tau_off)])
noxemissions.delta[6,]=summary(fullpanel.plants.annual$noxemissions[fullpanel.plants.annual$tau_off<0 & !is.na(fullpanel.plants.annual$oldcoalplant) & fullpanel.plants.annual$oldcoalplant==0 & is.finite(fullpanel.plants.annual$tau_off)])

co2emissions.delta[4:6,]=-co2emissions.delta[4:6,]
so2emissions.delta[4:6,]=-so2emissions.delta[4:6,]
noxemissions.delta[4:6,]=-noxemissions.delta[4:6,]

png(width=12,height=6,units="in",pointsize=20,res=300,file="Plots/SuppFig2a.png")
  par(mar=c(4,6,1,1),las=1)
  plot(co2emissions.delta$mean,1:6,xlim=c(min(co2emissions.delta$q3),max(co2emissions.delta$q3)),ylim=c(0.5,6.5),type="p",pch="|",col="blue",cex=2,xlab=expression(paste(CO[2]," Emissions [tons]")),ylab="",yaxt="n")
  axis(side=2,at=1:6,labels=c("On","On (NG)","On (Coal)","Off","Off (Coal)","Off (Other)"))
  grid(col="darkgrey",ny=NA)
  abline(v=0,col="black",lty=1,lwd=10)
  #segments(x0=co2emissions.delta$min,x1=co2emissions.delta$max,y0=1:6,col="black",lwd=1,lty=1)
  segments(x0=co2emissions.delta$q1,x1=co2emissions.delta$q3,y0=1:6,col="darkgrey",lwd=12,lty=1)
  points(co2emissions.delta$med,1:6,pch="|",col="red",cex=2)
  points(co2emissions.delta$mean,1:6,pch="|",col="blue",cex=2)
dev.off()

png(width=12,height=6,units="in",pointsize=20,res=300,file="Plots/SuppFig2b.png")
  par(mar=c(4,6,1,1),las=1)
  plot(so2emissions.delta$mean,1:6,xlim=c(min(so2emissions.delta$q3),max(so2emissions.delta$q3)),ylim=c(0.5,6.5),type="p",pch="|",col="blue",cex=2,xlab=expression(paste(SO[2],"Emissions [tons]")),ylab="",yaxt="n")
  axis(side=2,at=1:6,labels=c("On","On (NG)","On (Coal)","Off","Off (Coal)","Off (Other)"))
  grid(col="darkgrey",ny=NA)
  abline(v=0,col="black",lty=1,lwd=10)
  #segments(x0=so2emissions.delta$min,x1=so2emissions.delta$max,y0=1:6,col="black",lwd=1,lty=1)
  segments(x0=so2emissions.delta$q1,x1=so2emissions.delta$q3,y0=1:6,col="darkgrey",lwd=12,lty=1)
  points(so2emissions.delta$med,1:6,pch="|",col="red",cex=2)
  points(so2emissions.delta$mean,1:6,pch="|",col="blue",cex=2)
dev.off()

png(width=12,height=6,units="in",pointsize=20,res=300,file="Plots/SuppFig2c.png")
  par(mar=c(4,6,1,1),las=1)
  plot(noxemissions.delta$mean,1:6,xlim=c(min(noxemissions.delta$q3),max(noxemissions.delta$q3)),ylim=c(0.5,6.5),type="p",pch="|",col="blue",cex=2,xlab=expression(paste(NO[x]," Emissions [tons]")),ylab="",yaxt="n")
  axis(side=2,at=1:6,labels=c("On","On (NG)","On (Coal)","Off","Off (Coal)","Off (Other)"))
  grid(col="darkgrey",ny=NA)
  abline(v=0,col="black",lty=1,lwd=10)
  #segments(x0=noxemissions.delta$min,x1=noxemissions.delta$max,y0=1:6,col="black",lwd=1,lty=1)
  segments(x0=noxemissions.delta$q1,x1=noxemissions.delta$q3,y0=1:6,col="darkgrey",lwd=12,lty=1)
  points(noxemissions.delta$med,1:6,pch="|",col="red",cex=2)
  points(noxemissions.delta$mean,1:6,pch="|",col="blue",cex=2)
dev.off()

