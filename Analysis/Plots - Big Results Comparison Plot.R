load("Analysis/Plant Level Results.Rdata")
load("Analysis/County Level Results.Rdata")

#### 
  
# Models to Include in comparison
models = c(1,5,10,11,14,15,18,19,22)
  rhs[models]

# Outcome variables to include
outcomes = c(1,2,15,16,17)
  varlist[outcomes]  

# Goal - make data frames holding these comparisons for nice plotting - tables suck!
# Has to be done in two steps -- first reduced form models, then IVs.
  # Ultimately:
  # 3 data frames - old coal (tot coal), new ng (tot ng), newcoal
  # and 3 data frames for SEs
  # Columns - pm, pm (surf), o3 (surf), MR & IMR, MR x IMR (IV)x3, Crops, Crops(IV)x3
  # 3 + 2 + 6 + 12 = 23 columns
  
# First step - get basic reduced form models
# ---------------------------------------------
coal = ng = newcoal = coalse = ngse = newcoalse = data.frame(matrix(NA,nrow=length(models),ncol=length(outcomes)))
names(coal)=names(coalse)=names(ng)=names(ngse)=names(newcoal)=names(newcoalse)=varlist[outcomes]
row.names(coal)=row.names(coalse)=row.names(ng)=row.names(ngse)=row.names(newcoal)=row.names(newcoalse)=rhs[models]

for (i in 1:length(models)) {
  
  myvar=get(paste("m",models[i],sep=""))
  
  for (j in 1:length(outcomes)) {
    
    if(models[i] %in% c(15,18)) {
      ng[i,j] = summary(myvar[[outcomes[j]]])$coefficients[1,1]
      ngse[i,j] = summary(myvar[[outcomes[j]]])$coefficients[1,2]
    } else{
      coal[i,j] = summary(myvar[[outcomes[j]]])$coefficients[1,1]
      coalse[i,j] = summary(myvar[[outcomes[j]]])$coefficients[1,2]
    }
    
    if(models[i] %in% c(10,19,22)) {
      ng[i,j] = summary(myvar[[outcomes[j]]])$coefficients[2,1]
      ngse[i,j] = summary(myvar[[outcomes[j]]])$coefficients[2,2]
    
      if(models[i]==10) {
        newcoal[i,j] = summary(myvar[[outcomes[j]]])$coefficients[3,1]
        newcoalse[i,j] = summary(myvar[[outcomes[j]]])$coefficients[3,2]
      }
    }
  }
}
        
  
# Second step - get pollution and IV outcomes
# ---------------------------------------------
vars = c("pm","pmsurf","o3surf",rep(c("l.Total.Crude.Rate","l.Crude.Rate.Under1","l.corn","l.soy","l.w.wheat"),times=2))
sffx = c("","","",rep(c(".ivpm",".ivpmsurf"),each=5))
nms = paste(vars,sffx,sep="")

c=cse=n=nse=nc=ncse = data.frame(matrix(NA,nrow=length(models),ncol=13))
names(c) = names(cse) = names(n) = names(nse) = names(nc) = names(ncse) = nms
row.names(c) = row.names(cse) = row.names(n) = row.names(nse) = row.names(nc) = row.names(ncse) = rhs[models]
vs = c("m_offon_1","m_offon_5","m_offon_10","m_tot_1","m_tot_4","m_tot_5","m_tot_8","m_tot_9","m_tot_12")

for (i in 1:length(models)) {

  myvar = get(paste(vs[i]))
  myvariv1 = get(paste("m.ivpm.",models[i],sep=""))
  myvariv2 = get(paste("m.ivpmsurf.",models[i],sep=""))

  for (j in 1:length(vars)) {
    if (j < 4) {
      
      if(models[i] %in% c(15,18)) {
        n[i,j] = summary(myvar[[vars[j]]])$coefficients[1,1]
        nse[i,j] = summary(myvar[[vars[j]]])$coefficients[1,2]
      } else{
        c[i,j] = summary(myvar[[vars[j]]])$coefficients[1,1]
        cse[i,j] = summary(myvar[[vars[j]]])$coefficients[1,2]
      }
      
      if(models[i] %in% c(10,19,22)) {
        n[i,j] = summary(myvar[[vars[j]]])$coefficients[2,1]
        nse[i,j] = summary(myvar[[vars[j]]])$coefficients[2,2]
        
        if(models[i]==10) {
          nc[i,j] = summary(myvar[[vars[j]]])$coefficients[3,1]
          ncse[i,j] = summary(myvar[[vars[j]]])$coefficients[3,2]
        }
      }
    } 
    if (j>3 & j<9) {
      c[i,j] = summary(myvariv1[[vars[j]]])$coefficients[1,1]
      cse[i,j] = summary(myvariv1[[vars[j]]])$coefficients[1,2]
    }
    if (j>8) {
      c[i,j] = summary(myvariv2[[vars[j]]])$coefficients[1,1]
      cse[i,j] = summary(myvariv2[[vars[j]]])$coefficients[1,2]
    }
  }
}


## put together
cl = data.frame(coal,c)
ng = data.frame(ng,n)
nc = data.frame(newcoal,nc)

cse = data.frame(coalse,cse)
ngse = data.frame(ngse,nse)
ncse = data.frame(newcoalse,ncse)

ord = c("pm","pmsurf","o3surf",
        "l.Total.Crude.Rate","l.Total.Crude.Rate.ivpm","l.Total.Crude.Rate.ivpmsurf",
        "l.Crude.Rate.Under1","l.Crude.Rate.Under1.ivpm","l.Crude.Rate.Under1.ivpmsurf",
        "l.corn","l.corn.ivpm","l.corn.ivpmsurf",
        "l.soy","l.soy.ivpm","l.soy.ivpmsurf",
        "l.w.wheat","l.w.wheat.ivpm","l.w.wheat.ivpmsurf")

cl=cl[,ord]; ng=ng[,ord]; nc=nc[,ord]
cse=cse[,ord]; ngse=ngse[,ord]; ncse=ncse[,ord]

cl = signif(cl,2)
ng = signif(ng,2)
nc = signif(nc,2)
cse = signif(cse,2)
ngse = signif(ngse,2)
ncse = signif(ncse,2)

# meta
meancl = colMeans(cl)
meancse = colMeans(cse)
meanng = colMeans(ng)
meanngse = colMeans(ngse)
meannc = colMeans(nc)
meanncse = colMeans(ncse)

# Plot it out
# ---------------------------------------------

yy=1:length(models)
sig=1.64

for (i in 1:length(ord)) {
  png(width=3,height=6,units="in",res=300,file=paste("Plots/Comparison/v-",ord[i],".png",sep=""))
    par(mar=c(5,5,3,1),las=1)
    rg = max(abs(min(cl[,i],na.rm=T)),max(cl[,i],na.rm=T))
    plot(-1e6,-1e6,ylim=c(0,max(yy)+1),xlim=c(-2*rg,2*rg),yaxt="n",ylab="",xlab="",bty="n")
    abline(v=0,col="darkgrey",lty=2)
    mtext(side=2,at=yy,text=rhs[models],cex=0.5,line=2)
    title(main=i)
    segments(x0=cl[,i]-sig*cse[,i],x1=cl[,i]+sig*cse[,i],y0=yy,col="black",lty=1)
    if (grepl("iv",ord[i])==TRUE) {
      col="black"
      pp = 22
    } else {
      col="red"
      pp = 21
    }
    points(cl[,i],y=yy,pch=pp,col="black",bg=col)
    segments(x0=ng[,i]-sig*ngse[,i],x1=ng[,i]+sig*ngse[,i],y0=(yy)-0.1,col="black",lty=1)
    points(ng[,i],y=(yy)-0.1,pch=pp,col="black",bg="blue")
    segments(x0=nc[,i]-sig*ncse[,i],x1=nc[,i]+sig*ncse[,i],y0=(yy)+0.1,col="black",lty=1)
    points(nc[,i],y=(yy)+0.1,pch=pp,col="red",bg="white")
    box(lwd=2)
  dev.off()
}

sig=1.64
ii=c(4,5,9)
tt=rhs[models]
yy=1:length(ii)

# meta
mncl = apply(cl[ii,],2,mean,na.rm=T)
mncse = apply(cse[ii,],2,mean,na.rm=T)
mnng = apply(ng[ii,],2,mean,na.rm=T)
mnngse = apply(ngse[ii,],2,mean,na.rm=T)
mnnc = apply(nc[ii,],2,mean,na.rm=T)
mnncse = apply(ncse[ii,],2,mean,na.rm=T)

for (i in 1:length(ord)) {
  png(width=3,height=3.5,units="in",res=300,file=paste("Plots/Comparison/w-",ord[i],".png",sep=""))
    par(mar=c(5,4,3,1),las=1)
    rg = max(abs(min(cl[ii,i],ng[ii,i],na.rm=T)),max(cl[ii,i],ng[ii,i],na.rm=T))
    plot(-1e6,-1e6,ylim=c(0.5,max(yy)+0.5),xlim=c(-2*rg,2*rg),yaxt="n",ylab="",xlab="",bty="n")
    abline(v=0,col="darkgrey",lty=2)
    mtext(side=2,at=yy,text=tt[ii],cex=0.5,line=2)
    title(main=i)
    if (grepl("iv",ord[i])==TRUE) {
      col="black"
      pp = 22
    } else {
      col="red"
      pp = 21
    }
    segments(x0=cl[ii,i]-sig*cse[ii,i],x1=cl[ii,i]+sig*cse[ii,i],y0=yy,col="black",lty=1)
    points(cl[ii,i],y=yy,pch=pp,col="black",bg=col,cex=1.5)
    segments(x0=ng[ii,i]-sig*ngse[ii,i],x1=ng[ii,i]+sig*ngse[ii,i],y0=(yy)-0.1,col="black",lty=1)
    points(ng[ii,i],y=(yy)-0.1,pch=pp,col="black",bg="blue",cex=1.5)
    segments(x0=nc[ii,i]-sig*ncse[ii,i],x1=nc[ii,i]+sig*ncse[ii,i],y0=(yy)+0.1,col="black",lty=1)
    points(nc[ii,i],y=(yy)+0.1,pch=pp,col="red",bg="white",cex=1.5)
    
    segments(x0=mncl[i]-sig*mncse[i],x1=mncl[i]+sig*mncse[i],y0=0.6,col="darkgrey",lwd=2)
    segments(x0=mncl[i]-sig*mncse[i],y0=0.55,y1=0.65,col="darkgrey",lwd=2)
    segments(x0=mncl[i]+sig*mncse[i],y0=0.55,y1=0.65,col="darkgrey",lwd=2)
    points(mncl[i],0.6,col="darkgrey",lwd=2,pch=16,cex=1)
    box(lwd=2)
  dev.off()
}

# Do one last one with pm and pmsurf IVs together
png(width=3,height=3.5,units="in",res=300,file="Plots/Comparison/w-allpm.png")
  par(mar=c(5,4,3,1),las=1)
  rg = max(abs(min(cl$pm,na.rm=T)),max(cl$pmsurf,na.rm=T))
  plot(-1e6,-1e6,ylim=c(0.5,max(yy)+0.5),xlim=c(-1.2*rg,1.2*rg),yaxt="n",ylab="",xlab="",bty="n")
  abline(v=0,col="darkgrey",lty=2)
  mtext(side=2,at=yy,text=tt[ii],cex=0.5,line=2)
  title(main="all pm")
  segments(x0=cl$pm[ii]-sig*cse$pm[ii],x1=cl$pm[ii]+sig*cse$pm[ii],y0=yy,col="black",lty=1)
  points(cl$pm[ii],y=yy,pch=21,col="black",bg="red",cex=1.5)
  segments(x0=cl$pmsurf[ii]-sig*cse$pmsurf[ii],x1=cl$pmsurf[ii]+sig*cse$pmsurf[ii],y0=yy,col="black",lty=1)
  points(cl$pmsurf[ii],y=yy,pch=22,col="black",bg="red",cex=1.5)
  
  segments(x0=ng$pm[ii]-sig*ngse$pm[ii],x1=ng$pm[ii]+sig*ngse$pm[ii],y0=(yy)-0.1,col="black",lty=1)
  points(ng$pm[ii],y=(yy)-0.1,pch=21,col="black",bg="blue",cex=1.5)
  segments(x0=ng$pmsurf[ii]-sig*ngse$pmsurf[ii],x1=ng$pmsurf[ii]+sig*ngse$pmsurf[ii],y0=(yy)-0.1,col="black",lty=1)
  points(ng$pmsurf[ii],y=(yy)-0.1,pch=22,col="black",bg="blue",cex=1.5)
  
  segments(x0=nc$pm[ii]-sig*ncse$pm[ii],x1=nc$pm[ii]+sig*ncse$pm[ii],y0=(yy)+0.1,col="black",lty=1)
  points(nc$pm[ii],y=(yy)+0.1,pch=pp,col="red",bg="white",cex=1.5)
  
  segments(x0=mncl[1]-sig*mncse[1],x1=mncl[1]+sig*mncse[1],y0=0.6,col="darkgrey",lwd=2)
  segments(x0=mncl[1]-sig*mncse[1],y0=0.55,y1=0.65,col="darkgrey",lwd=2)
  segments(x0=mncl[1]+sig*mncse[1],y0=0.55,y1=0.65,col="darkgrey",lwd=2)
  points(mncl[1],0.6,col="darkgrey",lwd=2,pch=16,cex=1)

  segments(x0=mncl[2]-sig*mncse[2],x1=mncl[2]+sig*mncse[2],y0=0.6,col="darkgrey",lwd=2)
  segments(x0=mncl[2]-sig*mncse[2],y0=0.55,y1=0.65,col="darkgrey",lwd=2)
  segments(x0=mncl[2]+sig*mncse[2],y0=0.55,y1=0.65,col="darkgrey",lwd=2)
  points(mncl[2],0.6,col="darkgrey",lwd=2,pch=15,cex=0.8)

  box(lwd=2)
  legend("bottomleft",inset=0.01,bty="n",pch=c(21,22),col="black",legend=c("Satellite","Surface"))
dev.off()

# Make nice plot for all IVs together
for (i in 1:length(varlist[outcomes])) {
  nm1 = paste(varlist[outcomes[i]],"ivpm",sep=".")
  nm2 = paste(varlist[outcomes[i]],"ivpmsurf",sep=".")
  
  png(width=3,height=3.5,units="in",res=300,file=paste("Plots/Comparison/w-iv-",varlist[outcomes[i]],".png",sep=""))
    par(mar=c(5,4,3,1),las=1)
    rg = max(abs(min(cl[ii,nm1],ng[ii,nm1],na.rm=T)),max(cl[ii,nm1],ng[ii,nm1],na.rm=T))
    plot(-1e6,-1e6,ylim=c(0.5,max(yy)+0.5),xlim=c(-2*rg,2*rg),yaxt="n",ylab="",xlab="",bty="n")
    abline(v=0,col="darkgrey",lty=2)
    mtext(side=2,at=yy,text=tt[ii],cex=0.5,line=2)
    title(main=varlist[outcomes[i]])
    
    segments(x0=cl[ii,nm1]-sig*cse[ii,nm1],x1=cl[ii,nm1]+sig*cse[ii,nm1],y0=yy,col="black",lty=1)
    points(cl[ii,nm1],y=yy,pch=16,col="black",cex=1.5)

    segments(x0=mncl[nm1]-sig*mncse[nm1],x1=mncl[nm1]+sig*mncse[nm1],y0=0.6,col="darkgrey",lwd=2)
    segments(x0=mncl[nm1]-sig*mncse[nm1],y0=0.55,y1=0.65,col="darkgrey",lwd=2)
    segments(x0=mncl[nm1]+sig*mncse[nm1],y0=0.55,y1=0.65,col="darkgrey",lwd=2)
    points(mncl[nm1],0.6,col="darkgrey",lwd=2,pch=16,cex=1)
    
    segments(x0=cl[ii,nm2]-sig*cse[ii,nm2],x1=cl[ii,nm2]+sig*cse[ii,nm2],y0=yy+0.1,col="black",lty=1)
    points(cl[ii,nm2],y=yy+0.1,pch=15,col="black",cex=1.5)

    segments(x0=mncl[nm2]-sig*mncse[nm2],x1=mncl[nm2]+sig*mncse[nm2],y0=0.7,col="darkgrey",lwd=2)
    segments(x0=mncl[nm2]-sig*mncse[nm2],y0=0.65,y1=0.75,col="darkgrey",lwd=2)
    segments(x0=mncl[nm2]+sig*mncse[nm2],y0=0.65,y1=0.75,col="darkgrey",lwd=2)
    points(mncl[nm2],0.7,col="darkgrey",lwd=2,pch=15,cex=0.8)
    
    box(lwd=2)
  dev.off()
}

