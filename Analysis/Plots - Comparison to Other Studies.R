#dPM = c(4.33,6.90,4.63,28.72,NA,0.35,1.09,2.08,0.68,1.32,1.7,3.45,NA,5.09,3.53,4.00)
#SE = c(0.4*1.96,2.6*1.96,1.56*1.96,5.06*1.96,NA,0.02,0.19,0.6,0.19,0.37,1.4,1.88,NA,NA,NA,NA)

t1 = round(mod.11["l.Total.Crude.Rate",],3)[1]*100
t1se = round(mod.11["l.Total.Crude.Rate",],3)[2]*100
im1 = round(mod.11["l.Crude.Rate.Under1",],3)[1]*100
im1se = round(mod.11["l.Crude.Rate.Under1",],3)[2]*100

tiv1 = round(ivpmmod.11["l.Total.Crude.Rate",],3)[1]*100
tiv1se = round(ivpmmod.11["l.Total.Crude.Rate",],3)[2]*100
imiv1 = round(ivpmmod.11["l.Crude.Rate.Under1",],3)[1]*100
imiv1se = round(ivpmmod.11["l.Crude.Rate.Under1",],3)[2]*100

dpart = pmsurf_tot[[1]]$coefficients[1,1]

# t1 = round(m.coaloff["l.Total.Crude.Rate",],3)[1]*100
# t1se = round(m.coaloff["l.Total.Crude.Rate",],3)[2]*100
# im1 = round(m.coaloff["l.Crude.Rate.Under1",],3)[1]*100
# im1se = round(m.coaloff["l.Crude.Rate.Under1",],3)[2]*100
# 
# tiv1 = round(m.coaloffiv["l.Total.Crude.Rate",],3)[1]*100
# tiv1se = round(m.coaloffiv["l.Total.Crude.Rate",],3)[2]*100
# imiv1 = round(m.coaloffiv["l.Crude.Rate.Under1",],3)[1]*100
# imiv1se = round(m.coaloffiv["l.Crude.Rate.Under1",],3)[2]*100


dPM = c(t1/dpart,tiv1,im1/dpart,imiv1,NA,0.35,1.09,2.08,0.68,1.32,1.7,3.45,NA,5.09,3.53,4.00)
SE = c(t1se*1.64/dpart,tiv1se*1.64,im1se*1.64/dpart,imiv1se*1.64,NA,0.02,0.19,0.6,0.19,0.37,1.4,1.88,NA,NA,NA,NA)


study = c("Reduced Form, Total","IV Analysis, Total","Reduced Form, Infant","IV Analysis, Infant","","Thurston et al (2016)",
          "Eftim et all, ACS (2008)","Eftim et all, Harvard (2008)","Zeger et al, Eastern (2008)","Zeger et al, Central (2008)",
          "Chay & Greenstone (2003)", "Knittel et al (2016)","",
          "GBD, USA Results (2005-2013)","Apte et al, Q1 (2015)","Burnett et al, GBD-GEMM (2018)")

col = c(rep("grey",2),rep("lightblue",2),"white",rep("lightyellow",5),rep("lightblue",2),"white",rep("grey",3))

df = data.frame(study,dPM,SE,col,stringsAsFactors=FALSE)

# ind = c(1:14)
# df = df[ind,]

x = (1:dim(df)[1])*1.2-0.5


png(width=12,height=8,unit="in",res=300,file="Plots/Comparison/Comparison.png")
  par(mar=c(15,5,1,3),las=1)
  barplot(df$dPM,ylim=c(-1,15),ylab=expression(paste("% Change in Mortality Rate per ",mu,g,"/",m^3," ",PM[2.5],sep="")),col=df$col,cex.lab=1.5,cex.axis=1.2)
  segments(x0=x,y0=df$dPM-df$SE,y1=df$dPM+df$SE,col="black",lty=1,lwd=1)
  box(col="black",lwd=2)
  axis(side=1,at=x,labels=df$study,las=2,tick=FALSE)
  axis(side=4,at=seq(0,15,2),cex.axis=1.2)
  text(x[2],9,"This Study",col="red",font=2,cex=1,pos=4)
  text(x[9],9,"Empirical Concentration Studies",col="black",font=2,cex=1)
  text(x[15],9,"CTM-Based Studies",col="black",font=2,cex=1)
  abline(h=dPM[1],col="black",lty=2)
  legend("topright",inset=0.01,bty="n",fill=c("grey","lightyellow","lightblue"),legend=c("Total","Adult Only","Infant"),cex=1.5)
dev.off()

# Direct as pdf
pdf(width=12,height=8,pointsize=12,file="Plots/Comparison/Comparison.pdf")
  par(mar=c(15,5,1,3),las=1)
  barplot(df$dPM,ylim=c(-1,15),ylab=expression(paste("% Change in Mortality Rate per ",mu,g,"/",m^3," ",PM[2.5],sep="")),col=df$col,cex.lab=1.5,cex.axis=1.2)
  segments(x0=x,y0=df$dPM-df$SE,y1=df$dPM+df$SE,col="black",lty=1,lwd=1)
  box(col="black",lwd=2)
  axis(side=1,at=x,labels=df$study,las=2,tick=FALSE)
  axis(side=4,at=seq(0,15,2),cex.axis=1.2)
  text(x[2],9,"This Study",col="red",font=2,cex=1,pos=4)
  text(x[9],9,"Empirical Concentration Studies",col="black",font=2,cex=1)
  text(x[15],9,"CTM-Based Studies",col="black",font=2,cex=1)
  abline(h=dPM[1],col="black",lty=2)
  legend("topright",inset=0.01,bty="n",fill=c("grey","lightyellow","lightblue"),legend=c("Total","Adult Only","Infant"),cex=1.5)
dev.off()


