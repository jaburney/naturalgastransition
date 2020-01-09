# ------------------------------------------------------------------------------------------
# Main Analysis - County.R
# Produces basic county-level analyses in the paper, along with some plant level for ease
# ------------------------------------------------------------------------------------------

# Can toggle this to print out tables of difft models (do not appear in paper in this way)
do.tables = FALSE

# ------------------------------------------------------------------------------------------
# First Stages - Environmental Outcomes - County Level
# ------------------------------------------------------------------------------------------

varlist = c("pm","pmsurf","o3","o3surf","so2","no2","hcho","aod","ae")

# --------------------------------------------------
# Pure off and on - not ideal specification with
# FE (see below). Here for totals to compare. 
# --------------------------------------------------
rhs_offon = list()
rhs_offon[[1]] = "~ n_coaloff25"
rhs_offon[[2]] = "~ n_coaloff25 + n_coaloff50"
rhs_offon[[3]] = "~ n_coaloff25 + n_coaloff50 + n_coaloff100"
rhs_offon[[4]] = "~ n_coaloff25 + n_coaloff50 + n_coaloff100 + n_coaloff200"
rhs_offon[[5]] = "~ tot_coaloff"
rhs_offon[[6]] = "~ n_coaloff25 + n_coaloff50 + n_ngon25 + n_ngon50 + n_coalon25 + n_coalon50"
rhs_offon[[7]] = "~ n_coaloff25 + n_coaloff50 + n_coaloff100 + n_coaloff200 + n_ngon25 + n_ngon50 + n_ngon100 + n_ngon200"
rhs_offon[[8]] = "~ n_coaloff25 + n_coaloff50 + n_coaloff100 + n_coaloff200 + n_ngon25 + n_ngon50 + n_ngon100 + n_ngon200 + n_coalon25 + n_coalon50 + n_coalon100 + n_coalon200"
rhs_offon[[9]] = "~ tot_coaloff + tot_ngon"
rhs_offon[[10]] = "~ tot_coaloff + tot_ngon + tot_coalon"

# Loop over models and outcomes
for (j in 1:length(rhs_offon)) {
  tmp = list()
  rh = rhs_offon[[j]]
  for (i in 1:length(varlist)) {
    vv = varlist[i]
    fmla = formula(paste(vv,rh," | factor(Year) + factor(OBJECTID) | 0 | OBJECTID",sep=""))
    tmp[[i]] = felm(fmla,data=final.county.data)
  }
  names(tmp) = varlist
  assign(paste("m_offon_",j,sep=""),tmp)
}

# Tables
if (do.tables==TRUE) {
  ind = c(1:length(varlist))
  for (i in 1:length(rhs_offon)) {
    vv = get(paste("m_offon_",i,sep=""))
    stargazer(vv[ind],out=paste("Tables/m_offon_",i,".tex",sep=""),style="qje")
  }
}

# Also put things together by outcome variable for ease
for (i in 1:length(varlist)) {
  tmp = list()
  for (j in 1:length(rhs_offon)) {
    tmp[[j]] = get(paste("m_offon_",j,sep=""))[[i]]
  }
  assign(paste(varlist[i],"_offon",sep=""),tmp)
  if (do.tables==TRUE) {
    stargazer(tmp,out=paste("Tables/m_offon_",varlist[i],".tex",sep=""),style="qje")
  }
}

# --------------------------------------------------
# Total Number of plants - w/ FE operates as on/off
# --------------------------------------------------

rhs_tot = list()
rhs_tot[[1]] = "~ n_coal25"
rhs_tot[[2]] = "~ tot_coal50"
rhs_tot[[3]] = "~ tot_coal100"
rhs_tot[[4]] = "~ tot_coal"
rhs_tot[[5]] = "~ n_ng25"
rhs_tot[[6]] = "~ tot_ng50"
rhs_tot[[7]] = "~ tot_ng100"
rhs_tot[[8]] = "~ tot_ng"
rhs_tot[[9]] = "~ n_coal25 + n_ng25"
rhs_tot[[10]] = "~ tot_coal50 + tot_ng50"
rhs_tot[[11]] = "~ tot_coal100 + tot_ng100"
rhs_tot[[12]] = "~ tot_coal + tot_ng"

# Loop over models and outcomes
for (j in 1:length(rhs_tot)) {
  tmp = list()
  Ftmp = rep(NA,length(varlist))
  rh = rhs_tot[[j]]
  for (i in 1:length(varlist)) {
    vv = varlist[i]
    fmla = formula(paste(vv,rh," | factor(Year) + factor(OBJECTID) | 0 | OBJECTID",sep=""))
    tmp[[i]] = felm(fmla,data=final.county.data)
    Ftmp[i] = summary(tmp[[i]])$P.fstat["F"]
  }
  names(Ftmp) = varlist
  names(tmp) = varlist
  assign(paste("m_tot_",j,sep=""),tmp)
  assign(paste("Fstat_m_tot_",j,sep=""),Ftmp)
}

# Tables
if (do.tables==TRUE) {
  ind = c(1:length(varlist))
  for (i in 1:length(rhs_tot)) {
    vv = get(paste("m_tot_",i,sep=""))
    stargazer(vv[ind],out=paste("Tables/m_tot_",i,".tex",sep=""),style="qje")
  }
}

# Also put things together by outcome variable for ease
for (i in 1:length(varlist)) {
  tmp = list()
  for (j in 1:length(rhs_tot)) {
    tmp[[j]] = get(paste("m_tot_",j,sep=""))[[i]]
  }
  assign(paste(varlist[i],"_tot",sep=""),tmp)
  if (do.tables==TRUE) {
    stargazer(tmp,out=paste("Tables/m_tot_",varlist[i],".tex",sep=""),style="qje")
  }
}

# ------------------------------------------------------------------------------------------
# Calculate and Store Impacts for Main Outcome Variables - reduced form and IVs
# ------------------------------------------------------------------------------------------

varlist = names(final.county.data)[c(39:52,77:79)]

rhs = c(rhs_offon,rhs_tot)

# Loop over models and outcomes
for (j in 1:length(rhs)) {
  tmp = list()
  tmp.ivpm = list()
  tmp.ivpmsurf = list()
  tmp.ivpmo3surf = list()
  
  rh = rhs[[j]]
  
  for (i in 1:length(varlist)) {
    vv = varlist[i]
    
    fmla = formula(paste(vv,rh," | factor(Year) + factor(OBJECTID) | 0 | OBJECTID",sep=""))
    fmla.ivpm = formula(paste(vv,"~ 0 | factor(Year) + factor(OBJECTID) | (pm",rh,") | OBJECTID",sep=""))
    fmla.ivpmsurf = formula(paste(vv,"~ 0 | factor(Year) + factor(OBJECTID) | (pmsurf",rh,") | OBJECTID",sep=""))
    fmla.ivpmo3surf = formula(paste(vv,"~ 0 | factor(Year) + factor(OBJECTID) | (pm|o3surf",rh,") | OBJECTID",sep=""))
    
    tmp[[i]] = felm(fmla,data=final.county.data)
    tmp.ivpm[[i]] = felm(fmla.ivpm,data=final.county.data)
    tmp.ivpmsurf[[i]] = felm(fmla.ivpmsurf,data=final.county.data)
    tmp.ivpmo3surf[[i]] = felm(fmla.ivpmo3surf,data=final.county.data)
  }
  names(tmp) = names(tmp.ivpm) = names(tmp.ivpmsurf) = names(tmp.ivpmo3surf) = varlist
  assign(paste("m",j,sep=""),tmp)
  assign(paste("m.ivpm.",j,sep=""),tmp.ivpm)
  assign(paste("m.ivpmsurf.",j,sep=""),tmp.ivpmsurf)
  assign(paste("m.ivpmo3surf.",j,sep=""),tmp.ivpmo3surf)
}

# put in slightly nicer format for tables
ind = c(1:length(varlist))

if (do.tables==TRUE) {
  for (i in 1:length(rhs)) {
    vv = get(paste("m",i,sep=""))
    vv.ivpm = get(paste("m.ivpm.",i,sep=""))
    vv.ivpmsurf = get(paste("m.ivpmsurf.",i,sep=""))
    vv.ivpmo3surf = get(paste("m.ivpmo3surf.",i,sep=""))
    stargazer(vv[ind],out=paste("Tables/m",i,".tex",sep=""),style="qje")
    stargazer(vv.ivpm[ind],out=paste("Tables/m_ivpm_",i,".tex",sep=""),style="qje")
    stargazer(vv.ivpmsurf[ind],out=paste("Tables/m_ivpmsurf_",i,".tex",sep=""),style="qje")
    stargazer(vv.ivpmo3surf[ind],out=paste("Tables/m_ivpmo3surf_",i,".tex",sep=""),style="qje")
  }
}

# Get IVs nice format for plotting -- First PM
for (j in 1:length(rhs)) {
  nm1 = paste("ivpmmod.",j,sep="")
  nm2 = paste("ivpmsurfmod.",j,sep="")
  tmp1 = matrix(NA,ncol=4,nrow=length(varlist))
  tmp2 = matrix(NA,ncol=4,nrow=length(varlist))
  df1 = get(paste("m.ivpm.",j,sep=""))
  df2 = get(paste("m.ivpmsurf.",j,sep=""))
  for (i in 1:length(varlist)) {
    tmp1[i,] = summary(df1[[i]])$coefficients
    tmp2[i,] = summary(df2[[i]])$coefficients
  }
  row.names(tmp1) = row.names(tmp2) = varlist
  assign(nm1,tmp1)
  assign(nm2,tmp2)
}

# Get IVs nice format for plotting -- PM and O3surf
for (j in 1:length(rhs)) {
  nm1 = paste("ivpmo3surfmod.",j,".pm",sep="")
  nm2 = paste("ivpmo3surfmod.",j,".o3surf",sep="")
  tmp1 = tmp2 = matrix(NA,ncol=4,nrow=length(varlist))
  df = get(paste("m.ivpmo3surf.",j,sep=""))
  for (i in 1:length(varlist)) {
    tmp1[i,] = summary(df[[i]])$coefficients[1,]
    tmp2[i,] = summary(df[[i]])$coefficients[2,]
  }
  row.names(tmp1) = rownames(tmp2) = varlist
  assign(nm1,tmp1)
  assign(nm2,tmp2)
}

# Get Reduced Form models in nice format for plotting -- single variable
for (j in 1:length(rhs)) {
  nm = paste("mod.",j,sep="")
  tmp = matrix(NA,ncol=4,nrow=length(varlist))
  df = get(paste("m",j,sep=""))
  for (i in 1:length(varlist)) {
    tmp[i,] = summary(df[[i]])$coefficients[1,]
  }
  row.names(tmp) = varlist
  assign(nm,tmp)
}

# Get nice format for plotting - multiple vars
for (j in c(9,10,19,22)) {
  nm1 = paste("mod.",j,".coal",sep="")
  nm2 = paste("mod.",j,".ng",sep="")
  nm3 = paste("mod.",j,".newcoal",sep="")
  
  tmp1 = tmp2 = tmp3 =matrix(NA,ncol=4,nrow=length(varlist))
  df = get(paste("m",j,sep=""))
  for (i in 1:length(varlist)) {
    tmp1[i,] = summary(df[[i]])$coefficients[1,]
    tmp2[i,] = summary(df[[i]])$coefficients[2,]
    if (j==10) {
      tmp3[i,] = summary(df[[i]])$coefficients[3,]
    }
  }
  row.names(tmp1) = rownames(tmp2) = rownames(tmp3) = varlist
  assign(nm1,tmp1)
  assign(nm2,tmp2)
  if (j==10) {
    assign(nm3,tmp3)
  }
}

# ------------------------------------------------------------------------------------------
# Save - Saves into Directory
# ------------------------------------------------------------------------------------------

rm(i,j,df,df1,df2,tmp,tmp1,tmp2,tmp3,tmp.ivpm,tmp.ivpmsurf,tmp.ivpmo3surf,vv,vv.ivpm,vv.ivpmsurf,vv.ivpmo3surf,
   nm,nm1,nm2,nm3,rh,ind,fmla,fmla.ivpm,fmla.ivpmo3sat,fmla.ivpmo3surf,Ftmp,final.county.data)

save(list=ls(),file="Analysis/County Level Results.Rdata")

# ------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------
# Do more simple Plant-Level Analysis (for ease)
# ------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------

# Tiny bit of cleanup for ease
fullpanel.plants.annual$Total.Deaths = as.numeric(fullpanel.plants.annual$Total.Deaths)
fullpanel.plants.annual$l.corn[is.infinite(fullpanel.plants.annual$l.corn)] = NA 
fullpanel.plants.annual$l.w.wheat[is.infinite(fullpanel.plants.annual$l.w.wheat)] = NA 
fullpanel.plants.annual$l.soy[is.infinite(fullpanel.plants.annual$l.soy)] = NA 

fullpanel.plants.annual$CountyID = paste(fullpanel.plants.annual$County,fullpanel.plants.annual$State.Abb,sep="-")

# Just basic coal off, ng on, coal on, but saved more conveniently
m.coaloff = m.coalon = m.ngon = matrix(NA,ncol=4,nrow=length(varlist))
rh = "~ uniton | factor(Year) + factor(locationid) | 0 | locationid"

for (i in 1:length(varlist)) {
  var = varlist[i]
  if (i %in% c(4)) {next}
  fmla = formula(paste(var,rh,sep=""))
  tmp1 = felm(fmla,data=fullpanel.plants.annual[fullpanel.plants.annual$oldcoalplant==1,])
  tmp2 = felm(fmla,data=fullpanel.plants.annual[fullpanel.plants.annual$newcoalplant==1,])
  tmp3 = felm(fmla,data=fullpanel.plants.annual[fullpanel.plants.annual$newngplant==1,])
  
  m.coaloff[i,] = summary(tmp1)$coefficients
  m.coalon[i,] = summary(tmp2)$coefficients
  m.ngon[i,] = summary(tmp3)$coefficients
}
row.names(m.coaloff) = row.names(m.coalon) = row.names(m.ngon) = varlist


# Now IVs
m.coaloffiv = m.coaloniv = m.ngoniv = matrix(NA,ncol=4,nrow=length(varlist))
rh = "~ 0 | factor(Year) + factor(locationid) | (pm.100km~uniton) | locationid"
for (i in 1:length(varlist)) {
  var = varlist[i]
  if (i %in% c(4)) {next}
  fmla = formula(paste(var,rh,sep=""))
  tmp1 = felm(fmla,data=fullpanel.plants.annual[fullpanel.plants.annual$oldcoalplant==1,])
  tmp2 = felm(fmla,data=fullpanel.plants.annual[fullpanel.plants.annual$newcoalplant==1,])
  tmp3 = felm(fmla,data=fullpanel.plants.annual[fullpanel.plants.annual$newngplant==1,])
  
  m.coaloffiv[i,] = summary(tmp1)$coefficients
  m.coaloniv[i,] = summary(tmp2)$coefficients
  m.ngoniv[i,] = summary(tmp3)$coefficients
}
row.names(m.coaloffiv) = row.names(m.coaloniv) = row.names(m.ngoniv) = varlist

# Now IVs with Ozone as well (only surface)
m.coaloffiv2.pm = m.coaloniv2.pm = m.ngoniv2.pm = matrix(NA,ncol=4,nrow=length(varlist))
m.coaloffiv2.o3 = m.coaloniv2.o3 = m.ngoniv2.o3 = matrix(NA,ncol=4,nrow=length(varlist))
rh = "~ 0 | factor(Year) + factor(locationid) | (pm.100km|epa.o3.100km~uniton) | locationid"
for (i in 1:length(varlist)) {
  var = varlist[i]
  if (i %in% c(4)) {next}
  fmla = formula(paste(var,rh,sep=""))
  tmp1 = felm(fmla,data=fullpanel.plants.annual[fullpanel.plants.annual$oldcoalplant==1,])
  tmp2 = felm(fmla,data=fullpanel.plants.annual[fullpanel.plants.annual$newcoalplant==1,])
  tmp3 = felm(fmla,data=fullpanel.plants.annual[fullpanel.plants.annual$newngplant==1,])
  
  m.coaloffiv2.pm[i,] = summary(tmp1)$coefficients[1,]
  m.coaloniv2.pm[i,] = summary(tmp2)$coefficients[1,]
  m.ngoniv2.pm[i,] = summary(tmp3)$coefficients[1,]
  
  m.coaloffiv2.o3[i,] = summary(tmp1)$coefficients[2,]
  m.coaloniv2.o3[i,] = summary(tmp2)$coefficients[2,]
  m.ngoniv2.o3[i,] = summary(tmp3)$coefficients[2,]
}
row.names(m.coaloffiv2.pm) = row.names(m.coaloniv2.pm) = row.names(m.ngoniv2.pm) = varlist
row.names(m.coaloffiv2.o3) = row.names(m.coaloniv2.o3) = row.names(m.ngoniv2.o3) = varlist

# ------------------------------------------------------------------------------------------
# Save
# ------------------------------------------------------------------------------------------

save(m.coaloff,m.ngon,m.coalon,m.coaloffiv,m.ngoniv,m.coaloniv,
     m.coaloffiv2.pm,m.ngoniv2.pm,m.coaloniv2.pm,m.coaloffiv2.o3,m.ngoniv2.o3,m.coaloniv2.o3,
     rh,varlist,file="Analysis/Plant Level Results.Rdata")
