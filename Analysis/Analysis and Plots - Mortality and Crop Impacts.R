# -------------------------------------------------------
# Plots - Mortality and Crop Impacts.R
# -------------------------------------------------------

# Load Data (county slow to load)
load(file="Data/L2 Processed Data/Cleaned Power Plant and Spatial Data 2005-2016.RData")
load(file="Data/Final Data Sets for Analysis/FullPanel_PlantLocations_Annual.Rdata")
load(file="Data/Final Data Sets for Analysis/FullPanel_CountiesWithPlantNeighbors_Annual.Rdata")
load(file="Data/L1 Processed Data/CropAreas.Rdata")
load(file="Analysis/Plant Level Results.Rdata")
load(file="Analysis/County Level Results.Rdata")

# -------------------------------------------------------
# Select Variables to Plot
# -------------------------------------------------------

mortind = c("l.Total.Crude.Rate","l.Crude.Rate.Under1","l.Crude.Rate.15to19","l.Crude.Rate.20to24","l.Crude.Rate.25to34",
            "l.Crude.Rate.35to44","l.Crude.Rate.45to54","l.Crude.Rate.55to64","l.Crude.Rate.65to74",
            "l.Crude.Rate.75to84","l.Crude.Rate.85over")

cropind = c("l.corn","l.soy","l.w.wheat")

# -------------------------------------------------------
# Coal Off Mortality Impacts
# Reduced form - both around plant and county level
# -------------------------------------------------------

y = c(-1,-5:-14)
ng = 0.2
cl = c("black",rainbow(10))
sig = 1.64 
xlims = c(-0.08,0.08)

# coal off - only age groups with enough counties
m = m.coaloff[mortind,]
ma = mod.11[mortind,]
mb = mod.14[mortind,]
mc1 = mod.22.coal[mortind,]
mc2 = mod.22.ng[mortind,]

pdf(file="Plots/Mortality and Crop Yield Impacts/Mortality_Impacts_CoalOff_loctFE.pdf",width=10,height=10)
  par(mar=c(5,6,2,1),las=1)
  plot(-100,-100,xlim=xlims,ylim=c(-15,0),xlab="Change in (log) Mortality Rate",ylab="",yaxt="n",cex.lab=2,cex.axis=2)
  abline(v=0,lty=1,col="black",lwd=1)
  abline(v=seq(-0.3,0.3,0.01),lwd=1,lty=3,col="darkgrey")
  abline(h=-2.5,lty=3,col="black",lwd=1)
  segments(y0=y,x0=-m[,1]-sig*m[,2],x1=-m[,1]+sig*m[,2],lty=1,lwd=1,col="black")
  points(-m[,1],y,pch=21,cex=2,bg=cl,col="black")
  segments(y0=y-ng,x0=-ma[,1]-sig*ma[,2],x1=-ma[,1]+sig*ma[,2],lty=1,lwd=1,col="black")
  points(-ma[,1],y-ng,pch=22,cex=2,bg=cl,col="black")
  segments(y0=y-2*ng,x0=-mb[,1]-sig*mb[,2],x1=-mb[,1]+sig*mb[,2],lty=1,lwd=1,col="black")
  points(-mb[,1],y-2*ng,pch=23,cex=2,bg=cl,col="black")
  #segments(y0=y-6*ng,x0=-mc1[,1]-sig*mc1[,2],x1=-mc1[,1]+sig*mc1[,2],lty=1,lwd=1,col="black")
  #points(-mc1[,1],y-6*ng,pch=25,cex=1,bg=cl,col="black")
  #segments(y0=y-6*ng,x0=-mc2[,1]-sig*mc2[,2],x1=-mc2[,1]+sig*mc2[,2],lty=1,lwd=1,col="black")
  #points(-mc2[,1],y-6*ng,pch=25,cex=1,bg=cl,col="black")
  axis(side=2,at=y,labels=c("Total","< 1","15-19","20-24","25-34","35-44","45-54","55-64","65-74","75-84","> 85"),cex.axis=2)
  mtext(text="Age:",side=2,at=-4,col="darkgreen",las=1,cex=1.5,font=2,line=1)
  sigs = rep("",length(y)); sigs[m[,4]<0.1] = "*"
  text(x=-0.12,y=y,labels=sigs,cex=3)
  #legend("bottomleft",pch=c(21:24),pt.cex=c(1),col="black",legend=c("Around Plant","Within 25km of County","Within 200km of County","Within 200km of County, NG Control"),bty="n",cex=1.5)
  legend("bottomleft",pch=c(21:23),pt.cex=c(2),col="black",legend=c("Around Plant","Within 25km of County","Within 200km of County"),bty="n",cex=1.5)
  #abline(v=0,lty=2,col="black",lwd=3)
  box(which="plot",lty=1,lwd=3,col="black")
dev.off()

# -------------------------------------------------------
# Coal Off Instrumented Mortality Impacts
# PM - both around plant and county level
# -------------------------------------------------------

xlims = c(-0.3,0.3)

# coal off - only age groups with enough counties
m = m.coaloffiv[mortind,]
ma = ivpmmod.11[mortind,]
mb = ivpmmod.14[mortind,]

pdf(file="Plots/Mortality and Crop Yield Impacts/Mortality_Impacts_CoalOff_IVPM_loctFE.pdf",width=10,height=10)
  par(mar=c(5,6,2,1),las=1)
  plot(-100,-100,xlim=xlims,ylim=c(-15,0),xlab="",ylab="",yaxt="n",cex.lab=2,cex.axis=2)
  mtext(side=1,line=4,text=expression(paste("Change in (log) Mortality Rate per ",mu,"g ",m^-3," ",PM[2.5],sep="")),cex=2)
  abline(v=0,lty=1,col="black",lwd=1)
  abline(v=seq(-0.3,0.3,0.05),lwd=1,lty=3,col="darkgrey")
  abline(h=-2.5,lty=3,col="black",lwd=1)
  segments(y0=y,x0=m[,1]-sig*m[,2],x1=m[,1]+sig*m[,2],lty=1,lwd=1,col="black")
  segments(y0=y-ng,x0=ma[,1]-sig*ma[,2],x1=ma[,1]+sig*ma[,2],lty=1,lwd=1,col="black")
  segments(y0=y-2*ng,x0=mb[,1]-sig*mb[,2],x1=mb[,1]+sig*mb[,2],lty=1,lwd=1,col="black")
  points(m[,1],y,pch=21,cex=2,bg=cl,col="black")
  points(ma[,1],y-ng,pch=22,cex=2,bg=cl,col="black")
  points(mb[,1],y-2*ng,pch=23,cex=2,bg=cl,col="black")
  axis(side=2,at=y,labels=c("Total","< 1","15-19","20-24","25-34","35-44","45-54","55-64","65-74","75-84","> 85"),cex.axis=2)
  mtext(text="Age:",side=2,at=-4,col="darkgreen",las=1,cex=1.5,font=2,line=1)
  legend("bottomleft",pch=c(21:23),pt.cex=c(2),col="black",legend=c("Around Plant","Within 25km of County","Within 200km of County"),bty="n",cex=1.5)
  box(which="plot",lty=1,lwd=3,col="black")
dev.off()

# -------------------------------------------------------
# Coal Off Crop Impacts
# Reduced form - both around plant and county level
# -------------------------------------------------------

y = c(-1:-3)
cl = c("darkgreen","yellow3","tan")

yd = m.coaloff[cropind,]
yd2 = mod.11[cropind,]
yd3 = mod.14[cropind,]
yd4 = mod.22.coal[cropind,]
xlims = c(-0.12,0.12)

pdf(file="Plots/Mortality and Crop Yield Impacts/CropYield_Impacts_CoalOff.pdf",width=10,height=4)
  par(mar=c(5,8,2,1),las=1)
  plot(-100,-100,xlim=xlims,ylim=c(-3.5,-0.5),xlab="Change in (log) Yield",ylab="",yaxt="n",cex.lab=2,cex.axis=2)
  abline(v=seq(-0.3,0.3,0.01),lwd=1,lty=3,col="darkgrey")
  abline(v=0,lty=1,col="black",lwd=1)
  segments(y0=y,x0=-yd[,1]-sig*yd[,2],x1=-yd[,1]+sig*yd[,2],lty=1,lwd=1,col="black")
  points(-yd[,1],y,pch=21,cex=2,bg=cl,col="black")
  segments(y0=y-ng,x0=-yd2[,1]-sig*yd2[,2],x1=-yd2[,1]+sig*yd2[,2],lty=1,lwd=1,col="black")
  points(-yd2[,1],y-ng,pch=22,cex=2,bg=cl,col="black")
  segments(y0=y-2*ng,x0=-yd3[,1]-sig*yd3[,2],x1=-yd3[,1]+sig*yd3[,2],lty=1,lwd=1,col="black")
  points(-yd3[,1],y-2*ng,pch=23,cex=2,bg=cl,col="black")
  #segments(y0=y-6*ng,x0=-yd4[,1]-sig*yd4[,2],x1=-yd4[,1]+sig*yd4[,2],lty=1,lwd=1,col="black")
  #points(-yd4[,1],y-6*ng,pch=24,cex=1,bg=cl,col="black")
  axis(side=2,at=y,labels=c("Corn","Soybean","Wheat"),cex.axis=2)
  #text(-0.2,y,labels=c("**","","*"),cex=2)
  box(which="plot",lty=1,lwd=3,col="black")
  #legend("bottomleft",pch=c(21:23),pt.cex=c(1),col="black",legend=c("Around Plant","Within 25km of County","Within 200km of County"),bty="n",cex=1.5)
  #legend("bottomleft",pch=c(21:22),pt.cex=c(2),col="black",legend=c("Around Plant","Within 25km of County"),bty="n",cex=1)
dev.off()

# -------------------------------------------------------
# Coal Off Instrumented Crop Impacts
# PM - both around plant and county level
# -------------------------------------------------------

xlims = c(-0.9,0.1)

m = m.coaloffiv[cropind,]
ma = ivpmmod.11[cropind,]
mb = ivpmmod.14[cropind,]

pdf(file="Plots/Mortality and Crop Yield Impacts/Crop_Impacts_CoalOFF_IVPM_loctFE.pdf",width=10,height=4)
  par(mar=c(6,8,2,1),las=1)
  plot(-100,-100,xlim=xlims,ylim=c(-3.5,-0.5),xlab="",ylab="",yaxt="n",cex.lab=2,cex.axis=2)
  mtext(side=1,line=4,text=expression(paste("Change in (log) Yield per ",mu,"g ",m^-3," ",PM[2.5],sep="")),cex=2)
  abline(v=seq(-0.9,0.3,0.05),lwd=1,lty=3,col="darkgrey")
  abline(v=0,lty=1,col="black",lwd=1)
  segments(y0=y,x0=m[,1]-sig*m[,2],x1=m[,1]+sig*m[,2],lty=1,lwd=1,col="black")
  points(m[,1],y,pch=21,cex=2,bg=cl,col="black")
  segments(y0=y-ng,x0=ma[,1]-sig*ma[,2],x1=ma[,1]+sig*ma[,2],lty=1,lwd=1,col="black")
  points(ma[,1],y-ng,pch=22,cex=2,bg=cl,col="black")
  segments(y0=y-2*ng,x0=mb[,1]-sig*mb[,2],x1=mb[,1]+sig*mb[,2],lty=1,lwd=1,col="black")
  points(mb[,1],y-2*ng,pch=23,cex=2,bg=cl,col="black")
  axis(side=2,at=y,labels=c("Corn","Soybean","Wheat"),cex.axis=2)
  #legend("bottomleft",pch=c(21:24),pt.cex=c(1),col="black",legend=c("Around Plant","Within 25km of County","Within 200km of County"),bty="n",cex=1.5)
  box(which="plot",lty=1,lwd=3,col="black")
dev.off()

# -----------------------------------------------------------------------
# New County Benefit Calculations & Maps (Figure 3, S8)
# -----------------------------------------------------------------------

# models to run
ind = c(11,14)
coaloffimpacts = data.frame(matrix(NA,nrow=length(ind),ncol=6))
coalremainingimpacts = data.frame(matrix(NA,nrow=length(ind),ncol=6))

for (i in 1:length(ind)) {
  
  model = get(paste("mod",ind[i],sep="."))
  vartot = str_replace(rhs[[ind[i]]],"~ ","")
  
  # klugey - oh well
  if (ind[i]==11) {
    varoff = "n_coaloff25"
  }
  if (ind[i]==14) {
    varoff = "tot_coaloff"
  }

  # -------------------
  # Get coefficients
  # -------------------
  beta_mort = model["l.Total.Crude.Rate",1]
  se_mort = model["l.Total.Crude.Rate",2]
  beta_mort_low = beta_mort-sig*se_mort
  beta_mort_high = beta_mort+sig*se_mort
  
  beta_corn = model["l.corn",1]
  se_corn = model["l.corn",2]
  beta_corn_low = beta_corn-sig*se_corn
  beta_corn_high = beta_corn+sig*se_corn
  
  beta_soy = model["l.soy",1]
  se_soy = model["l.soy",2]
  beta_soy_low = beta_soy-sig*se_soy
  beta_soy_high = beta_soy+sig*se_soy
  
  beta_wheat = model["l.w.wheat",1]
  se_wheat = model["l.w.wheat",2]
  beta_wheat_low = beta_wheat-sig*se_wheat
  beta_wheat_high = beta_wheat+sig*se_wheat
  
  df = final.county.data[,c("OBJECTID","County","State.Abb","Year","Total.Population","Total.Deaths","Total.Crude.Rate","l.Total.Crude.Rate","l.corn","l.soy","l.w.wheat","corn","soy","w.wheat",vartot,varoff)]
  df$Total.Deaths = as.numeric(df$Total.Deaths)
  
  # Need to merge in crop areas
  cropareas = merge(corn.areas[,c("State","County","countystate","State.Abb","Avg.Corn.Area")],
                    soy.areas[,c("State","County","countystate","State.Abb","Avg.soy.Area")],by=c("State","County","countystate","State.Abb"))
  cropareas = merge(cropareas,wheat.areas[,c("State","County","countystate","State.Abb","Avg.wheat.Area")],by=c("State","County","countystate","State.Abb"))
  
  df = merge(df,cropareas,by=c("State.Abb","County"))
  
  # -------------------
  # Calculate Impacts
  # -------------------
  # ln(MR) = b*x
  # b = 1/MR * d(MR)/dx
  # d(MR)/dx = b*MR
  # del_MR = b*MR*del_x
  # del_deaths = b*MR*del_x*pop or b*deaths*del_x
  
  # Deaths associated with # coal plants
  df = arrange(df,OBJECTID,Year)
  
  #df = df %>% group_by(OBJECTID) %>% mutate(dn_coal25=n_coal25-lag(n_coal25))
  
  df$mortimpact = df$Total.Deaths*(exp(beta_mort)-1)*(df[,varoff])
  df$mortimpact_low = df$Total.Deaths*(exp(beta_mort_low)-1)*(df[,varoff])
  df$mortimpact_high = df$Total.Deaths*(exp(beta_mort_high)-1)*(df[,varoff])
  
  df$deaths = df$Total.Deaths*(exp(beta_mort)-1)*(df[,vartot])
  df$deaths_low = df$Total.Deaths*(exp(beta_mort_low)-1)*(df[,vartot])
  df$deaths_high = df$Total.Deaths*(exp(beta_mort_high)-1)*(df[,vartot])
  
  totalavoideddeaths = df %>% group_by(OBJECTID) %>% summarize(deathsavoid=sum(mortimpact,na.rm=TRUE),
                                                               deathsavoid_low=sum(mortimpact_low,na.rm=TRUE),
                                                               deathsavoid_high=sum(mortimpact_high,na.rm=TRUE))
  
  totalcoaldeaths = df %>% group_by(OBJECTID) %>% summarize(coaldeaths=sum(deaths,na.rm=TRUE),
                                                            coaldeaths_low=sum(deaths_low,na.rm=TRUE),
                                                            coaldeaths_high=sum(deaths_high,na.rm=TRUE))
  
  countyimpacts=merge(counties48,totalavoideddeaths,by="OBJECTID")
  countyimpacts=merge(countyimpacts,totalcoaldeaths,by="OBJECTID")
  
  usa_deathsavoid = sum(countyimpacts$deathsavoid,na.rm=TRUE)
  usa_deathsavoid_low = sum(countyimpacts$deathsavoid_low,na.rm=TRUE)
  usa_deathsavoid_high = sum(countyimpacts$deathsavoid_high,na.rm=TRUE)
  usa_deaths = sum(countyimpacts$coaldeaths,na.rm=TRUE)
  usa_deaths_low = sum(countyimpacts$coaldeaths_low,na.rm=TRUE)
  usa_deaths_high = sum(countyimpacts$coaldeaths_high,na.rm=TRUE)
  
  # Crop Impacts
  df$cornimpact = (exp(beta_corn)-1)*df$corn*df$Avg.Corn.Area*(df[,varoff])
  df$cornimpact_low = (exp(beta_corn_low)-1)*df$corn*df$Avg.Corn.Area*(df[,varoff])
  df$cornimpact_high = (exp(beta_corn_high)-1)*df$corn*df$Avg.Corn.Area*(df[,varoff])
  df$soyimpact = (exp(beta_soy)-1)*df$soy*df$Avg.soy.Area*(df[,varoff])
  df$soyimpact_low = (exp(beta_soy_low)-1)*df$soy*df$Avg.soy.Area*(df[,varoff])
  df$soyimpact_high = (exp(beta_soy_high)-1)*df$soy*df$Avg.soy.Area*(df[,varoff])
  df$wheatimpact = (exp(beta_wheat)-1)*df$w.wheat*df$Avg.wheat.Area*(df[,varoff])
  df$wheatimpact_low = (exp(beta_wheat_low)-1)*df$w.wheat*df$Avg.wheat.Area*(df[,varoff])
  df$wheatimpact_high = (exp(beta_wheat_high)-1)*df$w.wheat*df$Avg.wheat.Area*(df[,varoff])
  
  df$corntotloss = (exp(beta_corn)-1)*df$corn*df$Avg.Corn.Area*(df[,vartot])
  df$corntotloss_low = (exp(beta_corn_low)-1)*df$corn*df$Avg.Corn.Area*(df[,vartot])
  df$corntotloss_high = (exp(beta_corn_high)-1)*df$corn*df$Avg.Corn.Area*(df[,vartot])
  df$soytotloss = (exp(beta_soy)-1)*df$soy*df$Avg.soy.Area*(df[,vartot])
  df$soytotloss_low = (exp(beta_soy_low)-1)*df$soy*df$Avg.soy.Area*(df[,vartot])
  df$soytotloss_high = (exp(beta_soy_high)-1)*df$soy*df$Avg.soy.Area*(df[,vartot])
  df$wheattotloss = (exp(beta_wheat)-1)*df$w.wheat*df$Avg.wheat.Area*(df[,vartot])
  df$wheattotloss_low = (exp(beta_wheat_low)-1)*df$w.wheat*df$Avg.wheat.Area*(df[,vartot])
  df$wheattotloss_high = (exp(beta_wheat_high)-1)*df$w.wheat*df$Avg.wheat.Area*(df[,vartot])
  
  totalcropimpact = df %>% group_by(County,State.Abb) %>% summarize(corn=sum(cornimpact,na.rm=TRUE),
                                                                     corn_low=sum(cornimpact_low,na.rm=TRUE),
                                                                     corn_high=sum(cornimpact_high,na.rm=TRUE),
                                                                     soy=sum(soyimpact,na.rm=TRUE),
                                                                     soy_low=sum(soyimpact_low,na.rm=TRUE),
                                                                     soy_high=sum(soyimpact_high,na.rm=TRUE),
                                                                     wheat=sum(wheatimpact,na.rm=TRUE),
                                                                     wheat_low=sum(wheatimpact_low,na.rm=TRUE),
                                                                     wheat_high=sum(wheatimpact_high,na.rm=TRUE)
  )
  
  totalcroploss = df %>% group_by(County,State.Abb) %>% summarize(corn=sum(corntotloss,na.rm=TRUE),
                                                                   corn_low=sum(corntotloss_low,na.rm=TRUE),
                                                                   corn_high=sum(corntotloss_high,na.rm=TRUE),
                                                                   soy=sum(soytotloss,na.rm=TRUE),
                                                                   soy_low=sum(soytotloss_low,na.rm=TRUE),
                                                                   soy_high=sum(soytotloss_high,na.rm=TRUE),
                                                                   wheat=sum(wheattotloss,na.rm=TRUE),
                                                                   wheat_low=sum(wheattotloss_low,na.rm=TRUE),
                                                                   wheat_high=sum(wheattotloss_high,na.rm=TRUE)
  )
  
  
  countycropimpacts=merge(counties48,totalcropimpact)
  countycropimpacts$Total = countycropimpacts$corn+countycropimpacts$soy+countycropimpacts$wheat
  countycropimpacts$Total_low = countycropimpacts$corn_low+countycropimpacts$soy_low+countycropimpacts$wheat_low
  countycropimpacts$Total_high = countycropimpacts$corn_high+countycropimpacts$soy_high+countycropimpacts$wheat_high
  
  countycroploss = merge(counties48,totalcroploss)
  countycroploss$Total = countycroploss$corn+countycroploss$soy+countycroploss$wheat
  countycroploss$Total_low = countycroploss$corn_low+countycroploss$soy_low+countycroploss$wheat_low
  countycroploss$Total_high = countycroploss$corn_high+countycroploss$soy_high+countycroploss$wheat_high
  
  usa_cropimpacts = sum(countycropimpacts$Total,na.rm=TRUE)*1e-6
  usa_cropimpacts_low = sum(countycropimpacts$Total_low,na.rm=TRUE)*1e-6
  usa_cropimpacts_high = sum(countycropimpacts$Total_high,na.rm=TRUE)*1e-6
  
  usa_croploss = sum(countycroploss$Total,na.rm=TRUE)*1e-6
  usa_croploss_low = sum(countycroploss$Total_low,na.rm=TRUE)*1e-6
  usa_croploss_high = sum(countycroploss$Total_high,na.rm=TRUE)*1e-6
  
  # Fill Small Tables
  coaloffimpacts[i,] = c(usa_deathsavoid,usa_deathsavoid_low,usa_deathsavoid_high,usa_cropimpacts,usa_cropimpacts_low,usa_cropimpacts_high)
  coalremainingimpacts[i,] = c(usa_deaths,usa_deaths_low,usa_deaths_high,usa_croploss,usa_croploss_low,usa_croploss_high)
  
  # Plots
  png(width=13,height=10,units="in",res=300,file=paste("Plots/Mortality and Crop Yield Impacts/LivesSaved_",vartot,".png",sep=""),type="cairo")
    mybks = c(1,20,50,100,200,500,2500)
    mycols = c("white",brewer.pal(length(mybks)+2,"YlGnBu")[2:(length(mybks+2))])
    par(mar=c(0,0,0,0))
    choropleth(countyimpacts,countyimpacts$deathsavoid,shading=shading(mybks,mycols))
    legend("bottomleft",fill=mycols,legend=c("<1","1-20","21-50","51-100","101-200","201-500",">501"),bty="n",inset=0.05,cex=1.3)
    points(oldcoal.locations,pch=16,col="red",cex=0.5)
  dev.off()
  
  png(width=13,height=10,units="in",res=300,file=paste("Plots/Mortality and Crop Yield Impacts/CoalDeaths_",vartot,".png",sep=""),type="cairo")
    mybks = c(1,20,50,100,200,500,2500)
    mycols = c("white",brewer.pal(length(mybks)+2,"YlOrBr")[2:(length(mybks+2))])
    par(mar=c(0,0,0,0))
    choropleth(countyimpacts,countyimpacts$coaldeaths,shading=shading(mybks,mycols))
    legend("bottomleft",fill=mycols,legend=c("<1","1-20","21-50","51-100","101-200","201-500",">501"),bty="n",inset=0.05,cex=1.3)
    points(plant.locations2[plant.locations2$fueltype=="Coal" & !is.na(plant.locations2$fueltype) & plant.locations2$oldcoalplant==0,],pch=16,col="red",cex=0.5)
  dev.off()
  
  png(width=13,height=10,units="in",res=300,file=paste("Plots/Mortality and Crop Yield Impacts/CropsSaved_",vartot,".png",sep=""),type="cairo")
    mybks = c(0.001,0.1,1,2,5,10,15)
    mycols = c("white",brewer.pal(length(mybks),"YlGnBu"))
    par(mar=c(0,0,0,0))
    choropleth(countyimpacts,-1*countycropimpacts$Total*1e-6,shading=shading(mybks,mycols))
    legend("bottomleft",fill=c(mycols),legend=c("<0.001","0.001-0.1","0.1-1","1-2","2-5","5-10","10-15",">15"),bty="n",inset=0.05,cex=1.3)
    points(oldcoal.locations,pch=16,col="red",cex=0.5)
  dev.off()
  
  png(width=13,height=10,units="in",res=300,file=paste("Plots/Mortality and Crop Yield Impacts/CropLoss_",vartot,".png",sep=""),type="cairo")
    mybks = c(0.01,1,2,5,10,20,50,100)
    mycols = c("white",brewer.pal(length(mybks),"YlOrBr"))
    par(mar=c(0,0,0,0))
    choropleth(countycroploss,-1*countycroploss$Total*1e-6,shading=shading(mybks,mycols))
    legend("bottomleft",fill=mycols,legend=c("<0.01","0.1-1","1-2","2-5","5-10","10-20","20-50","50-100",">100"),bty="n",inset=0.05,cex=1.3)
    points(plant.locations2[plant.locations2$fueltype=="Coal" & !is.na(plant.locations2$fueltype) & plant.locations2$oldcoalplant==0,],pch=16,col="red",cex=0.5)
  dev.off()

}

nms = c("Lives","Lives_low","Lives_high","Crops [MBu]","Crops_low","Crops_high")
names(coaloffimpacts) = names(coalremainingimpacts) = nms

write.csv(coaloffimpacts,file="Analysis/CoalOff_Total_Impacts.csv",row.names=rhs[ind])
write.csv(coalremainingimpacts,file="Analysis/CoalRemaining_Total_Impacts.csv",row.names=rhs[ind])

