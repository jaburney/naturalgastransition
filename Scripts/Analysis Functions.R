# -----------------------------------------------------------------------
# Analysis - Functions.R
# Main functions for basic analysis at location level with no spillovers
# -----------------------------------------------------------------------

# -----------------------------------------------------------------------
# basic panel RD function
# -----------------------------------------------------------------------
rd <- function(df,y,x,fe,controls,modtype) {
  
  if (is.na(controls)) {
    d = data.frame(df[[fe[1]]],df[[fe[2]]],df[[y]],df[[x]])
    names(d)=c("unit","time","y","x")
  } else {
    d = data.frame(df[[fe[1]]],df[[fe[2]]],df[[controls]],df[[y]],df[[x]])
    names(d)=c("unit","time","controls","y","x")
    d$controls=as.factor(round(d$controls))
  }
  d$x=round(d$x)
  d$time=round(d$time)
  d$timefac=as.factor(d$time)
  d$unit=as.factor(d$unit)
  
  d = subset(d,(is.finite(d$time) & is.finite(d$y) & !is.na(d$y)))
  d = arrange(d,unit,time)
  
  if (is.na(controls)) {
    switch(modtype,
           "unit FE" = {fmla <- y~x | unit | 0 | unit},
           "unit time FE cl1" = {fmla <- y~x | unit + timefac | 0 | unit},
           "unit time FE" = {fmla <- y~x | unit + timefac | 0 | unit + timefac},
           stop("Not a valid model")
    )
  } else {
    switch(modtype,
           "unit FE" = {fmla <- y~x | unit+factor(controls) | 0 | unit},
           "unit time FE" = {fmla <- y~x | unit + timefac + factor(controls) | 0 | unit + timefac},
           stop("Not a valid model")
    )
  }
  
  # do fit using felm (package: lfe)
  model <- felm(fmla,data=d,na.action=na.exclude)
  aa = summary(model)
  rlist = list("model"=model,"data"=d,"results"=aa$coefficients["x",])
  
  return(rlist)
}

# -----------------------------------------------------------------------
# Tau function
# -----------------------------------------------------------------------

tau_fn <- function(df,y,tauvar,nt,fe,modtype) {
  
  d = data.frame(df[[fe[1]]],df[[fe[2]]],df[[y]],df[[tauvar]])
  names(d)=c("unit","time","y","tau")
  
  d$tau=round(d$tau)
  d$time=round(d$time)
  d$timefac=as.factor(d$time)
  d$unit=as.factor(d$unit)
  
  #d = subset(d,(is.finite(d$tau) & abs(d$tau)<=n & is.finite(d$y) & !is.na(d$y)))
  d = subset(d,(abs(d$tau)<=nt & is.finite(d$y) & !is.na(d$y)))
  d = arrange(d,unit,tau)
  
  switch(modtype,
         "unit FE" = {fmla <- y~factor(tau) | unit | 0 | unit},
         "unit time FE" = {fmla <- y~factor(tau) | unit + timefac | 0 | unit + timefac},
         stop("Not a valid model")
  )
  
  # do fit using felm (package: lfe)
  model <- felm(fmla,data=d,na.action=na.exclude)
  aa = summary(model)
  rlist = list("model"=model,"data"=d,"results"=aa$coefficients[1:(2*nt),])
  
  return(rlist)
}

# -----------------------------------------------------------------------
# Tau intensity function - more like truncated panel
# -----------------------------------------------------------------------

tau_intensity_fn <- function(df,y,tauvar,loadvar,nt,fe,modtype) {
  
  d = data.frame(df[[fe[1]]],df[[fe[2]]],df[[y]],df[[tauvar]],df[[loadvar]])
  names(d)=c("unit","time","y","tau","load")
  
  d$tau=round(d$tau)
  d$time=round(d$time)
  d$timefac=as.factor(d$time)
  d$unit=as.factor(d$unit)
  d$load=round(d$load)
  
  #d = subset(d,(is.finite(d$tau) & abs(d$tau)<=n & is.finite(d$y) & !is.na(d$y)))
  d = subset(d,(abs(d$tau)<=nt & is.finite(d$y) & !is.na(d$y)))
  d = arrange(d,unit,tau)
  
  switch(modtype,
         "unit FE" = {fmla <- y~load | unit | 0 | unit},
         "unit time FE" = {fmla <- y~load | unit + timefac | 0 | unit + timefac},
         stop("Not a valid model")
  )
  
  # do fit using felm (package: lfe)
  model <- felm(fmla,data=d,na.action=na.exclude)
  aa = summary(model)
  rlist = list("model"=model,"data"=d,"results"=aa$coefficients[1,])
  
  return(rlist)
}

# -----------------------------------------------------------------------
# IV function
# -----------------------------------------------------------------------

iv_fn <- function(df,y,x,z,fe,controls,modtype) {
  
  if (is.na(controls)) {
    d = data.frame(df[[fe[1]]],df[[fe[2]]],df[[y]],df[[x]],df[[z]])
    names(d)=c("unit","time","y","x","z")
  } else {
    d = data.frame(df[[fe[1]]],df[[fe[2]]],df[[controls]],df[[y]],df[[x]],df[[z]])
    names(d)=c("unit","time","controls","y","x","z")
    d$controls=as.factor(round(d$controls))
  }
  d$x=round(d$x)
  d$z=round(d$z)
  d$time=round(d$time)
  d$timefac=as.factor(d$time)
  d$unit=as.factor(d$unit)
  
  d = subset(d,(is.finite(d$time) & is.finite(d$y) & !is.na(d$y)))
  d = arrange(d,unit,time)
  
  if (is.na(controls)) {
    switch(modtype,
           "unit FE" = {fmla <- y ~ 0 | unit | (x~z) | unit},
           "unit time FE" = {fmla <- y ~ 0 | unit + timefac | (x~z) | unit + timefac},
           stop("Not a valid model")
    )
  } else {
    switch(modtype,
           "unit FE" = {fmla <- y ~ 0 | unit+factor(controls) | (x~z) | unit},
           "unit time FE" = {fmla <- y ~ 0 | unit + timefac + factor(controls) | (x~z) | unit + timefac},
           stop("Not a valid model")
    )
  }
  
  # do fit using felm (package: lfe)
  model <- felm(fmla,data=d,na.action=na.exclude)
  aa = summary(model)
  rlist = list("model"=model,"data"=d,"results"=aa$coefficients[1,])
  
  return(rlist)
}

# -----------------------------------------------------------------------
# Function to access these results for plotting
# -----------------------------------------------------------------------

getdf2plot = function(v,nt,nrm,s) {

  var = v

  if (dim(var)[1]==(2*nt) | row.names(var)[2*nt+1]!="factor(tau)3") {
    dat=data.frame(tt=-nt:nt,pt=c(0,t(var[1:(2*nt),1])))
  } else {
    dat=data.frame(tt=-nt:nt,pt=c(0,t(var[1:(2*nt),1])))
  }
  dat$se=c(0,var[1:(2*nt),2])
  dat$lb=dat$pt-s*dat$se
  dat$ub=dat$pt+s*dat$se
  return(dat)
}
