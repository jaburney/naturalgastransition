# ----------------------------------------------------------------------------------------------------------------------------------------------
# Main Analysis - Plant Location.R
# ----------------------------------------------------------------------------------------------------------------------------------------------
# Main Annual Analysis File at the plant level - mainly used for pollution metrics although calculated across outcomes
# County level analysis better accounts for spillovers.
# ----------------------------------------------------------------------------------------------------------------------------------------------
# Runs a *lot* of different specifications on whole range of outcomes
# Binary, Intensity, Leads/Lags, IVs
# Many loops is unsexy code but easier for re-running bits.
# Uses functions loaded from "Analysis Functions.R"
# ----------------------------------------------------------------------------------------------------------------------------------------------

setwd("Analysis/")

# -----------------------------------------------------------------------
# Macros for Analysis
# Annual analysis, +/- 3y on leads/lags models
# Run unit FE, unit + time FE
# -----------------------------------------------------------------------
# Main Analysis Loops: k=models (FE, etc.), j=experiments (switches), i=outcomes
# Different versions of loops for different overall models and underlying data sets
# -----------------------------------------------------------------------

# Macros for Experiments (j)
timeunit = "year"
controls = NA
nyears = 3

experiments = c("offcoal","onng","oncoal")
switch_variable = c(rep("uniton",5))
intensity_variable = "grossload"
tauvars = c("tau_off",rep("tau_on",2))

experimentsindex = 1:length(experiments)

mydir = " - Annual - "

# Macros for Models (k) - trends not sensible for much of this (lacking DOF)
models = c("unit FE","unit time FE")

# Outcome variables
varnames = names(fullpanel.plants.annual)

outvars = c(varnames[grepl("emissions",varnames,fixed=TRUE)],varnames[grepl("km",varnames,fixed=TRUE)],
            varnames[grep("l.Crude.Rate",varnames,fixed=TRUE)],varnames[grepl("l.CR",varnames,fixed=TRUE)],
            "l.corn","l.soy","l.w.wheat")
      outvars = outvars[! outvars %in% c("Total.Crude.Rate","CR.Total.Crude.Rate")]

ivoutvars = c(varnames[grepl("aod",varnames,fixed=TRUE)],varnames[grepl("ssa",varnames,fixed=TRUE)],
              varnames[grep("l.Crude.Rate",varnames,fixed=TRUE)],varnames[grepl("l.CR",varnames,fixed=TRUE)],
              "l.corn","l.soy","l.w.wheat")
      ivoutvars = ivoutvars[! ivoutvars %in% c("Total.Crude.Rate","CR.Total.Crude.Rate")]

# ----------------------------------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------------------------------
# Run analysis for unit switches
# ----------------------------------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------------------------------
# (1a) Basic Panel Structure - unit level, FE = unit
# Universe = Plant Locations of a Given Switch Type (e.g., old coal plants)
# Analog = ToT
# -----------------------------------------------------------------------

dobasicpanelunit = 1
if (dobasicpanelunit==1) {
  
  ff = fullpanel.plants.annual

  for (k in 1:length(models)) {
    
    modtype=models[k]
    
    for (j in experimentsindex) {
      
      # get correct subsets, directories, FEs
      experiment=experiments[j]
      switchvar=switch_variable[j]
      tauvar=tauvars[j]
      outdir=paste("RD Analysis",mydir,modtype,"/PlantType/",experiment,"/",sep="")
      dir.create(outdir,showWarnings=FALSE,recursive=TRUE)
      
      if (experiment=="onng") {
        df=subset(ff,ff$newngplant==1)
      } else if (experiment=="oncoal") {
        df=subset(ff,ff$newcoalplant==1)
      } else if (experiment=="offcoal") {
        df=subset(ff,ff$oldcoalplant==1)
      }
      
      # set up blank results matrix to hold results
      # 4 columns -- beta, cluster SE, t, p
      # 1 row per outcome variable
      results = matrix(NA,nrow=length(outvars),ncol=4)
      
      for (i in 1:length(outvars)) {
        print(i)
        flush.console()
        
        qty=outvars[i]
        fixedeffects=c("facunitid",timeunit)
        
        # run main analysis function
        possibleError <- tryCatch({
          dat=rd(df,qty,switchvar,fixedeffects,controls,modtype)
        }, error=function(e){cat("model:",models[k],"; experiment:",experiments[j],"; outcome:",outvars[i]," ERROR :",conditionMessage(e),"\n"); e})
        
        if(!inherits(possibleError,"error")){
          results[i,] = dat$results
        }
        
        # Can save full shebang per variable -- slows down -- opt instead below to just save main coeff estimates.
        #assign(qty,dat)
        #saveRDS(qty,file=paste(outdir,experiment,"_",qty,".RDS",sep=""))
      }
      
      row.names(results)=outvars
      stargazer(results,type="latex",out=paste(outdir,"Results.tex",sep=""))
      save(list=c("results","outdir","outvars","controls","switchvar","modtype"),file=paste(outdir,"Analysis_Annual","_PlantType_",experiment,".Rdata",sep=""))
    }
  }
  
}

# -----------------------------------------------------------------------
# (1b) Basic Panel Structure - unit level, FE = location
# Universe = Plant Locations of a Given Switch Type (e.g., old coal plants)
# Analog = ToT
# -----------------------------------------------------------------------

dobasicpanelunitloc = 1
if (dobasicpanelunitloc==1) {
  
  ff = fullpanel.plants.annual

  for (k in 1:length(models)) {
    
    modtype=models[k]
    
    for (j in experimentsindex) {
      
      # get correct subsets, directories, FEs
      experiment=experiments[j]
      switchvar=switch_variable[j]
      tauvar=tauvars[j]
      outdir=paste("RD Analysis LOCATION",mydir,modtype,"/PlantType/",experiment,"/",sep="")
      dir.create(outdir,showWarnings=FALSE,recursive=TRUE)
      
      if (experiment=="onng") {
        df=subset(ff,ff$newngplant==1)
      } else if (experiment=="oncoal") {
        df=subset(ff,ff$newcoalplant==1)
      } else if (experiment=="offcoal") {
        df=subset(ff,ff$oldcoalplant==1)
      }
      
      # set up blank results matrix to hold results
      # 4 columns -- beta, cluster SE, t, p
      # 1 row per outcome variable
      results = matrix(NA,nrow=length(outvars),ncol=4)
      
      for (i in 1:length(outvars)) {
        print(i)
        flush.console()
        
        qty=outvars[i]
        fixedeffects=c("locationid",timeunit)
        
        # run main analysis function
        possibleError <- tryCatch({
          dat=rd(df,qty,switchvar,fixedeffects,controls,modtype)
        }, error=function(e){cat("model:",models[k],"; experiment:",experiments[j],"; outcome:",outvars[i]," ERROR :",conditionMessage(e),"\n"); e})
        
        if(!inherits(possibleError,"error")){
          results[i,] = dat$results
        }
        
        # Can save full shebang per variable -- slows down.
        #assign(qty,dat)
        #saveRDS(qty,file=paste(outdir,experiment,"_",qty,".RDS",sep=""))
      }
      
      row.names(results)=outvars
      stargazer(results,type="latex",out=paste(outdir,"Results.tex",sep=""))
      save(list=c("results","outdir","outvars","controls","switchvar","modtype"),file=paste(outdir,"Analysis_Annual_Location","_PlantType_",experiment,".Rdata",sep=""))
    }
  }
  
}

# -----------------------------------------------------------------------
# (2a) Basic Panel Structure - unit level, FE = unit
# Universe = Plant Locations of a Given FUEL Type (e.g., all coal plants)
# Analog = To Potentially Treated (more info in FE)
# -----------------------------------------------------------------------

dobasicpanelplantunit = 1
if (dobasicpanelplantunit==1) {
  
  ff = fullpanel.plants.annual
  
  # only unit and unit+time FE (no trends)
  for (k in 1:length(models)) {
    
    modtype=models[k]
    
    for (j in experimentsindex) {
      
      # get correct subsets, directories, FEs
      experiment=experiments[j]
      switchvar=switch_variable[j]
      tauvar=tauvars[j]
      outdir=paste("RD Analysis",mydir,modtype,"/FuelType/",experiment,"/",sep="")
      dir.create(outdir,showWarnings=FALSE,recursive=TRUE)
      
      if (experiment=="onng") {
        df=subset(ff,ff$fueltype=="Natural Gas" & is.na(ff[,"tau_off"]))
      } else if (experiment=="oncoal") {
        df=subset(ff,ff$fueltype=="Coal" & is.na(ff[,"tau_off"]))
      } else if (experiment=="offcoal") {
        df=subset(ff,ff$fueltype=="Coal" & is.na(ff[,"tau_on"]))
      } 
      
      # set up blank results matrix to hold results
      # 4 columns -- beta, cluster SE, t, p
      # 1 row per outcome variable
      results = matrix(NA,nrow=length(outvars),ncol=4)
      
      for (i in 1:length(outvars)) {
        print(i)
        flush.console()
        
        qty=outvars[i]
        fixedeffects=c("facunitid",timeunit)
        
        # run main analysis function
        possibleError <- tryCatch({
          dat=rd(df,qty,switchvar,fixedeffects,controls,modtype)
        }, error=function(e){cat("model:",models[k],"; experiment:",experiments[j],"; outcome:",outvars[i]," ERROR :",conditionMessage(e),"\n"); e})
        
        if(!inherits(possibleError,"error")){
          results[i,] = dat$results
        }
        
      }
      
      row.names(results)=outvars
      stargazer(results,type="latex",out=paste(outdir,"Results.tex",sep=""))
      save(list=c("results","outdir","outvars","controls","switchvar","modtype"),file=paste(outdir,"Analysis_Annual","_FuelType_",experiment,".Rdata",sep=""))
    }
  }
  
}

# -----------------------------------------------------------------------
# (2b) Basic Panel Structure - unit level, FE = location
# Universe = Plant Locations of a Given FUEL Type (e.g., all coal plants)
# Analog = To Potentially Treated (more info in FE)
# -----------------------------------------------------------------------

dobasicpanelplantloc = 1
if (dobasicpanelplantloc==1) {
  
  ff = fullpanel.plants.annual

  # only unit and unit+time FE (no trends)
  for (k in 1:length(models)) {
    
    modtype=models[k]
    
    for (j in experimentsindex) {
      
      # get correct subsets, directories, FEs
      experiment=experiments[j]
      switchvar=switch_variable[j]
      tauvar=tauvars[j]
      outdir=paste("RD Analysis LOCATION",mydir,modtype,"/FuelType/",experiment,"/",sep="")
      dir.create(outdir,showWarnings=FALSE,recursive=TRUE)
      
      if (experiment=="onng") {
        df=subset(ff,ff$fueltype=="Natural Gas" & is.na(ff[,"tau_off"]))
      } else if (experiment=="oncoal") {
        df=subset(ff,ff$fueltype=="Coal" & is.na(ff[,"tau_off"]))
      } else if (experiment=="offcoal") {
        df=subset(ff,ff$fueltype=="Coal" & is.na(ff[,"tau_on"]))
      } 
      
      # set up blank results matrix to hold results
      # 4 columns -- beta, cluster SE, t, p
      # 1 row per outcome variable
      results = matrix(NA,nrow=length(outvars),ncol=4)
      
      for (i in 1:length(outvars)) {
        print(i)
        flush.console()
        
        qty=outvars[i]
        fixedeffects=c("facunitid",timeunit)
        
        # run main analysis function
        possibleError <- tryCatch({
          dat=rd(df,qty,switchvar,fixedeffects,controls,modtype)
        }, error=function(e){cat("model:",models[k],"; experiment:",experiments[j],"; outcome:",outvars[i]," ERROR :",conditionMessage(e),"\n"); e})
        
        if(!inherits(possibleError,"error")){
          results[i,] = dat$results
        }
        
      }
      
      row.names(results)=outvars
      stargazer(results,type="latex",out=paste(outdir,"Results.tex",sep=""))
      save(list=c("results","outdir","outvars","controls","switchvar","modtype"),file=paste(outdir,"Analysis_Annual_Location","_FuelType_",experiment,".Rdata",sep=""))
    }
  }
  
}

# -----------------------------------------------------------------------
# (3) Basic Panel Structure - counties
# Universe = Plant Locations of a Given FUEL Type (e.g., all coal plants)
# Analog = To Potentially Treated (more info in FE)
# -----------------------------------------------------------------------

dobasicpanelcounty = 1
if (dobasicpanelcounty==1) {
  
  ff = fullpanel.counties.annual

  # only unit and unit+time FE (no trends)
  for (k in 1:length(models)) {
    
    modtype=models[k]
    
    for (j in experimentsindex) {
      
      # get correct subsets, directories, FEs
      experiment=experiments[j]
      switchvar=switch_variable[j]
      tauvar=tauvars[j]
      outdir=paste("RD Analysis",mydir,modtype,"/Counties/",experiment,"/",sep="")
      dir.create(outdir,showWarnings=FALSE,recursive=TRUE)
      
      df = ff
      
      # set up blank results matrix to hold results
      # 4 columns -- beta, cluster SE, t, p
      # 1 row per outcome variable
      results = matrix(NA,nrow=length(outvars),ncol=4)
      
      for (i in 1:length(outvars)) {
        print(i)
        flush.console()
        
        qty=outvars[i]
        fixedeffects=c("County",timeunit)
        
        # run main analysis function
        possibleError <- tryCatch({
          dat=rd(df,qty,switchvar,fixedeffects,controls,modtype)
        }, error=function(e){cat("model:",models[k],"; experiment:",experiments[j],"; outcome:",outvars[i]," ERROR :",conditionMessage(e),"\n"); e})
        
        if(!inherits(possibleError,"error")){
          results[i,] = dat$results
        }
        
      }
      
      row.names(results)=outvars
      stargazer(results,type="latex",out=paste(outdir,"Results.tex",sep=""))
      save(list=c("results","outdir","outvars","controls","switchvar","modtype"),file=paste(outdir,"Analysis_Annual","_Counties_",experiment,".Rdata",sep=""))
    }
  }
  
}

# -----------------------------------------------------------------------
# (4a-c) Basic Panel Structure - all points
# Universe = All grid cells
# Analog = To Potentially Treated (more info in FE)
# -----------------------------------------------------------------------

dobasicpanelgridcell = 1
if (dobasicpanelgridcell==1) {
  
  # 0.25 Degree Cells
  ff = fullpanel.allpoints.annual.0p25
  vn = names(ff)
  ov = outvars[outvars %in% vn]
  
  # only unit and unit+time FE (no trends)
  for (k in 1:length(models)) {
    
    modtype=models[k]
    
    for (j in experimentsindex) {
      
      # get correct subsets, directories, FEs
      experiment=experiments[j]
      switchvar=switch_variable[j]
      tauvar=tauvars[j]
      outdir=paste("RD Analysis",mydir,modtype,"/GridCells0p25/",experiment,"/",sep="")
      dir.create(outdir,showWarnings=FALSE,recursive=TRUE)
      
      df = ff
      
      # set up blank results matrix to hold results
      # 4 columns -- beta, cluster SE, t, p
      # 1 row per outcome variable
      results = matrix(NA,nrow=length(ov),ncol=4)
      
      for (i in 1:length(ov)) {
        print(i)
        flush.console()
        
        qty=ov[i]
        fixedeffects=c("locationid2",timeunit)
        
        # run main analysis function
        possibleError <- tryCatch({
          dat=rd(df,qty,switchvar,fixedeffects,controls,modtype)
        }, error=function(e){cat("model:",models[k],"; experiment:",experiments[j],"; outcome:",ov[i]," ERROR :",conditionMessage(e),"\n"); e})
        
        if(!inherits(possibleError,"error")){
          results[i,] = dat$results
        }
      }
      
      row.names(results)=ov
      stargazer(results,type="latex",out=paste(outdir,"Results.tex",sep=""))
      save(list=c("results","outdir","ov","controls","switchvar","modtype"),file=paste(outdir,"Analysis_Annual","_GridCells0p25_",experiment,".Rdata",sep=""))
    }
  }
  
  
  # 0.125 Degree Cells
  ff = fullpanel.allpoints.annual.0p125
  vn = names(ff)
  ov = outvars[outvars %in% vn]
  
  # only unit and unit+time FE (no trends)
  for (k in 1:length(models)) {
    
    modtype=models[k]
    
    for (j in experimentsindex) {
      
      # get correct subsets, directories, FEs
      experiment=experiments[j]
      switchvar=switch_variable[j]
      tauvar=tauvars[j]
      outdir=paste("RD Analysis",mydir,modtype,"/GridCells0p125/",experiment,"/",sep="")
      dir.create(outdir,showWarnings=FALSE,recursive=TRUE)
      
      df = ff
      
      # set up blank results matrix to hold results
      # 4 columns -- beta, cluster SE, t, p
      # 1 row per outcome variable
      results = matrix(NA,nrow=length(ov),ncol=4)
      
      for (i in 1:length(ov)) {
        print(i)
        flush.console()
        
        qty=ov[i]
        fixedeffects=c("locationid2",timeunit)
        
        # run main analysis function
        possibleError <- tryCatch({
          dat=rd(df,qty,switchvar,fixedeffects,controls,modtype)
        }, error=function(e){cat("model:",models[k],"; experiment:",experiments[j],"; outcome:",ov[i]," ERROR :",conditionMessage(e),"\n"); e})
        
        if(!inherits(possibleError,"error")){
          results[i,] = dat$results
        }
      }
      
      row.names(results)=ov
      stargazer(results,type="latex",out=paste(outdir,"Results.tex",sep=""))
      save(list=c("results","outdir","ov","controls","switchvar","modtype"),file=paste(outdir,"Analysis_Annual","_GridCells0p125_",experiment,".Rdata",sep=""))
    }
  }
  
  # 1 Degree Cells
  ff = fullpanel.allpoints.annual.1
  vn = names(ff)
  ov = outvars[outvars %in% vn]
  
  # only unit and unit+time FE (no trends)
  for (k in 1:length(models)) {
    
    modtype=models[k]
    
    for (j in experimentsindex) {
      
      # get correct subsets, directories, FEs
      experiment=experiments[j]
      switchvar=switch_variable[j]
      tauvar=tauvars[j]
      outdir=paste("RD Analysis",mydir,modtype,"/GridCells1/",experiment,"/",sep="")
      dir.create(outdir,showWarnings=FALSE,recursive=TRUE)
      
      df = ff
      
      # set up blank results matrix to hold results
      # 4 columns -- beta, cluster SE, t, p
      # 1 row per outcome variable
      results = matrix(NA,nrow=length(ov),ncol=4)
      
      for (i in 1:length(ov)) {
        print(i)
        flush.console()
        
        qty=ov[i]
        fixedeffects=c("locationid2",timeunit)
        
        # run main analysis function
        possibleError <- tryCatch({
          dat=rd(df,qty,switchvar,fixedeffects,controls,modtype)
        }, error=function(e){cat("model:",models[k],"; experiment:",experiments[j],"; outcome:",ov[i]," ERROR :",conditionMessage(e),"\n"); e})
        
        if(!inherits(possibleError,"error")){
          results[i,] = dat$results
        }
      }
      
      row.names(results)=ov
      stargazer(results,type="latex",out=paste(outdir,"Results.tex",sep=""))
      save(list=c("results","outdir","ov","controls","switchvar","modtype"),file=paste(outdir,"Analysis_Annual","_GridCells1_",experiment,".Rdata",sep=""))
    }
  }
  
}

# -----------------------------------------------------------------------
# -----------------------------------------------------------------------
# (5a) Tau - Unit Level, FE = unit
# Universe = Plant Locations of a Given Switch Type (e.g., old coal plants)
# Analog = ToT
# -----------------------------------------------------------------------
# -----------------------------------------------------------------------

dotauunit = 1
if (dotauunit==1) {
  
  ff = fullpanel.plants.annual

  for (k in 1:length(models)) {
    
    modtype=models[k]
    
    for (j in experimentsindex) {
      
      # get correct subsets, directories, FEs
      experiment=experiments[j]
      tauvar=tauvars[j]
      outdir=paste("Tau Analysis",mydir,modtype,"/PlantType/",experiment,"/",sep="")
      dir.create(outdir,showWarnings=FALSE,recursive=TRUE)
      
      if (experiment=="onng") {
        df=subset(ff,ff$newngplant==1)
      } else if (experiment=="oncoal") {
        df=subset(ff,ff$newcoalplant==1)
      } else if (experiment=="offcoal") {
        df=subset(ff,ff$oldcoalplant==1)
      } 
      
      for (i in 1:length(outvars)) {
        print(i)
        flush.console()
        
        qty=outvars[i]
        fixedeffects=c("facunitid",timeunit)
        
        # run main analysis function
        possibleError <- tryCatch({
          dat=tau_fn(df,qty,tauvar,nyears,fixedeffects,modtype)
        }, error=function(e){cat("model:",models[k],"; experiment:",experiments[j],"; outcome:",outvars[i]," ERROR :",conditionMessage(e),"\n"); e})
        
        # save individual results
        if(!inherits(possibleError,"error")) {
          tmptab = rbind(dat$results)
          stargazer(tmptab,type="latex",out=paste(outdir,qty,"_Results.tex",sep=""))
          assign(qty,tmptab)
        }
      }
      
      a = ls()
      save(list=a[a %in% outvars],file=paste(outdir,"Analysis_Tau_Annual","_PlantType_",experiment,".Rdata",sep=""))
    }
  }
  
}

# -----------------------------------------------------------------------
# (5b) Tau - Unit Level, FE = location
# Universe = Plant Locations of a Given Switch Type (e.g., old coal plants)
# Analog = ToT
# -----------------------------------------------------------------------

dotauloc = 1
if (dotauloc==1) {
  
  # Macros for Outcomes (i)
  ff = fullpanel.plants.annual

  for (k in 1:length(models)) {
    
    modtype=models[k]
    
    for (j in experimentsindex) {
      
      # get correct subsets, directories, FEs
      experiment=experiments[j]
      tauvar=tauvars[j]
      outdir=paste("Tau Analysis LOCATION",mydir,modtype,"/PlantType/",experiment,"/",sep="")
      dir.create(outdir,showWarnings=FALSE,recursive=TRUE)
      
      if (experiment=="onng") {
        df=subset(ff,ff$newngplant==1)
      } else if (experiment=="oncoal") {
        df=subset(ff,ff$newcoalplant==1)
      } else if (experiment=="offcoal") {
        df=subset(ff,ff$oldcoalplant==1)
      } 
      
      for (i in 1:length(outvars)) {
        print(i)
        flush.console()
        
        qty=outvars[i]
        fixedeffects=c("locationid",timeunit)
        
        # run main analysis function
        possibleError <- tryCatch({
          dat=tau_fn(df,qty,tauvar,nyears,fixedeffects,modtype)
        }, error=function(e){cat("model:",models[k],"; experiment:",experiments[j],"; outcome:",outvars[i]," ERROR :",conditionMessage(e),"\n"); e})
        
        # save individual results
        if(!inherits(possibleError,"error")) {
          tmptab = rbind(dat$results)
          stargazer(tmptab,type="latex",out=paste(outdir,qty,"_Results.tex",sep=""))
          assign(qty,tmptab)
        }
      }
      
      a = ls()
      save(list=a[a %in% outvars],file=paste(outdir,"Analysis_Tau_Annual_Location","_PlantType_",experiment,".Rdata",sep=""))
    }
  }
  
}

# ----------------------------------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------------------------------
# Run analysis for intensity
# ----------------------------------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------------------------------
# (1a) Basic Panel Structure - unit level, FE = unit
# Universe = Plant Locations of a Given Switch Type (e.g., old coal plants)
# Analog = ToT
# Trends only on this one
# -----------------------------------------------------------------------

dointensitypanelunit = 1
if (dointensitypanelunit==1) {
  
  ff = fullpanel.plants.annual

  # no trends here
  for (k in 1:length(models)) {
    
    modtype=models[k]
    
    for (j in experimentsindex) {
      
      # get correct subsets, directories, FEs
      experiment=experiments[j]
      switchvar=intensity_variable
      tauvar=tauvars[j]
      outdir=paste("RD Analysis Intensity",mydir,modtype,"/PlantType/",experiment,"/",sep="")
      dir.create(outdir,showWarnings=FALSE,recursive=TRUE)
      
      if (experiment=="onng") {
        df=subset(ff,ff$newngplant==1 & !is.na(ff[,tauvar]))
      } else if (experiment=="oncoal") {
        df=subset(ff,ff$newcoalplant==1 & !is.na(ff[,tauvar]))
      } else if (experiment=="offcoal") {
        df=subset(ff,ff$oldcoalplant==1 & !is.na(ff[,tauvar]))
      } 
      
      # set up blank results matrix to hold results
      # 4 columns -- beta, cluster SE, t, p
      # 1 row per outcome variable
      results = matrix(NA,nrow=length(outvars),ncol=4)
      
      for (i in 1:length(outvars)) {
        print(i)
        flush.console()
        
        qty=outvars[i]
        fixedeffects=c("facunitid",timeunit)
        
        # run main analysis function
        possibleError <- tryCatch({
          dat=rd(df,qty,switchvar,fixedeffects,controls,modtype)
        }, error=function(e){cat("model:",models[k],"; experiment:",experiments[j],"; outcome:",outvars[i]," ERROR :",conditionMessage(e),"\n"); e})
        
        if(!inherits(possibleError,"error")){
          results[i,] = dat$results
        }
        
        # Can save full shebang per variable -- slows down.
        #assign(qty,dat)
        #saveRDS(qty,file=paste(outdir,experiment,"_",qty,".RDS",sep=""))
      }
      
      row.names(results)=outvars
      stargazer(results,type="latex",out=paste(outdir,"Results.tex",sep=""))
      save(list=c("results","outdir","outvars","controls","switchvar","modtype"),file=paste(outdir,"Intensity_Analysis_Annual","_PlantType_",experiment,".Rdata",sep=""))
    }
  }
  
}

# -----------------------------------------------------------------------
# (1b) Basic Panel Structure - unit level, FE = location
# Universe = Plant Locations of a Given Switch Type (e.g., old coal plants)
# Analog = ToT
# Trends only on this one
# -----------------------------------------------------------------------

dointensitypanelunitloc = 1
if (dointensitypanelunitloc==1) {
  
  ff = fullpanel.plants.annual

  for (k in 1:length(models)) {
    
    modtype=models[k]
    
    for (j in experimentsindex) {
      
      # get correct subsets, directories, FEs
      experiment=experiments[j]
      switchvar=intensity_variable
      tauvar=tauvars[j]
      outdir=paste("RD Analysis Intensity LOCATION",mydir,modtype,"/PlantType/",experiment,"/",sep="")
      dir.create(outdir,showWarnings=FALSE,recursive=TRUE)
      
      if (experiment=="onng") {
        df=subset(ff,ff$newngplant==1 & !is.na(ff[,tauvar]))
      } else if (experiment=="oncoal") {
        df=subset(ff,ff$newcoalplant==1 & !is.na(ff[,tauvar]))
      } else if (experiment=="offcoal") {
        df=subset(ff,ff$oldcoalplant==1 & !is.na(ff[,tauvar]))
      } 
      
      # set up blank results matrix to hold results
      # 4 columns -- beta, cluster SE, t, p
      # 1 row per outcome variable
      results = matrix(NA,nrow=length(outvars),ncol=4)
      
      for (i in 1:length(outvars)) {
        print(i)
        flush.console()
        
        qty=outvars[i]
        fixedeffects=c("locationid",timeunit)
        
        # run main analysis function
        possibleError <- tryCatch({
          dat=rd(df,qty,switchvar,fixedeffects,controls,modtype)
        }, error=function(e){cat("model:",models[k],"; experiment:",experiments[j],"; outcome:",outvars[i]," ERROR :",conditionMessage(e),"\n"); e})
        
        if(!inherits(possibleError,"error")){
          results[i,] = dat$results
        }
        
        # Can save full shebang per variable -- slows down.
        #assign(qty,dat)
        #saveRDS(qty,file=paste(outdir,experiment,"_",qty,".RDS",sep=""))
      }
      
      row.names(results)=outvars
      stargazer(results,type="latex",out=paste(outdir,"Results.tex",sep=""))
      save(list=c("results","outdir","outvars","controls","switchvar","modtype"),file=paste(outdir,"Intensity_Analysis_Annual_Location","_PlantType_",experiment,".Rdata",sep=""))
    }
  }
  
}

# -----------------------------------------------------------------------
# (2a) Basic Panel Structure - unit level, FE = unit
# Universe = Plant Locations of a Given FUEL Type (e.g., all coal plants)
# Analog = To Potentially Treated (more info in FE)
# -----------------------------------------------------------------------

dointensitypanelplantunit = 1
if (dointensitypanelplantunit==1) {
  
  ff = fullpanel.plants.annual

  # only unit and unit+time FE (no trends)
  for (k in 1:length(models)) {
    
    modtype=models[k]
    
    for (j in experimentsindex) {
      
      # get correct subsets, directories, FEs
      experiment=experiments[j]
      switchvar=intensity_variable
      tauvar=tauvars[j]
      outdir=paste("RD Analysis Intensity",mydir,modtype,"/FuelType/",experiment,"/",sep="")
      dir.create(outdir,showWarnings=FALSE,recursive=TRUE)
      
      if (experiment=="onng") {
        df=subset(ff,ff$fueltype=="Natural Gas" & is.na(ff[,"tau_off"]))
      } else if (experiment=="oncoal") {
        df=subset(ff,ff$fueltype=="Coal" & is.na(ff[,"tau_off"]))
      } else if (experiment=="offcoal") {
        df=subset(ff,ff$fueltype=="Coal" & is.na(ff[,"tau_on"]))
      } 
      
      # set up blank results matrix to hold results
      # 4 columns -- beta, cluster SE, t, p
      # 1 row per outcome variable
      results = matrix(NA,nrow=length(outvars),ncol=4)
      
      for (i in 1:length(outvars)) {
        print(i)
        flush.console()
        
        qty=outvars[i]
        fixedeffects=c("facunitid",timeunit)
        
        # run main analysis function
        possibleError <- tryCatch({
          dat=rd(df,qty,switchvar,fixedeffects,controls,modtype)
        }, error=function(e){cat("model:",models[k],"; experiment:",experiments[j],"; outcome:",outvars[i]," ERROR :",conditionMessage(e),"\n"); e})
        
        if(!inherits(possibleError,"error")){
          results[i,] = dat$results
        }
        
      }
      
      row.names(results)=outvars
      stargazer(results,type="latex",out=paste(outdir,"Results.tex",sep=""))
      save(list=c("results","outdir","outvars","controls","switchvar","modtype"),file=paste(outdir,"Intensity_Analysis_Annual","_FuelType_",experiment,".Rdata",sep=""))
    }
  }
  
}

# -----------------------------------------------------------------------
# (2b) Basic Panel Structure - unit level, FE = location
# Universe = Plant Locations of a Given FUEL Type (e.g., all coal plants)
# Analog = To Potentially Treated (more info in FE)
# -----------------------------------------------------------------------

dointensitypanelplantloc = 1
if (dointensitypanelplantloc==1) {
  
  ff = fullpanel.plants.annual

  # only unit and unit+time FE (no trends)
  for (k in 1:length(models)) {
    
    modtype=models[k]
    
    for (j in experimentsindex) {
      
      # get correct subsets, directories, FEs
      experiment=experiments[j]
      switchvar=intensity_variable
      tauvar=tauvars[j]
      outdir=paste("RD Analysis Intensity LOCATION",mydir,modtype,"/FuelType/",experiment,"/",sep="")
      dir.create(outdir,showWarnings=FALSE,recursive=TRUE)
      
      if (experiment=="onng") {
        df=subset(ff,ff$fueltype=="Natural Gas" & is.na(ff[,"tau_off"]))
      } else if (experiment=="oncoal") {
        df=subset(ff,ff$fueltype=="Coal" & is.na(ff[,"tau_off"]))
      } else if (experiment=="offcoal") {
        df=subset(ff,ff$fueltype=="Coal" & is.na(ff[,"tau_on"]))
      } 
      
      # set up blank results matrix to hold results
      # 4 columns -- beta, cluster SE, t, p
      # 1 row per outcome variable
      results = matrix(NA,nrow=length(outvars),ncol=4)
      
      for (i in 1:length(outvars)) {
        print(i)
        flush.console()
        
        qty=outvars[i]
        fixedeffects=c("facunitid",timeunit)
        
        # run main analysis function
        possibleError <- tryCatch({
          dat=rd(df,qty,switchvar,fixedeffects,controls,modtype)
        }, error=function(e){cat("model:",models[k],"; experiment:",experiments[j],"; outcome:",outvars[i]," ERROR :",conditionMessage(e),"\n"); e})
        
        if(!inherits(possibleError,"error")){
          results[i,] = dat$results
        }
        
      }
      
      row.names(results)=outvars
      stargazer(results,type="latex",out=paste(outdir,"Results.tex",sep=""))
      save(list=c("results","outdir","outvars","controls","switchvar","modtype"),file=paste(outdir,"Intensity_Analysis_Annual_Location","_FuelType_",experiment,".Rdata",sep=""))
    }
  }
  
}

# -----------------------------------------------------------------------
# (3) Basic Panel Structure - counties
# Universe = Plant Locations of a Given FUEL Type (e.g., all coal plants)
# Analog = To Potentially Treated (more info in FE)
# -----------------------------------------------------------------------

dointensitypanelcounty = 1
if (dointensitypanelcounty==1) {
  
  ff = fullpanel.counties.annual

  # only unit and unit+time FE (no trends)
  for (k in 1:length(models)) {
    
    modtype=models[k]
    
    for (j in experimentsindex) {
      
      # get correct subsets, directories, FEs
      experiment=experiments[j]
      switchvar=intensity_variable
      tauvar=tauvars[j]
      outdir=paste("RD Analysis",mydir,modtype,"/Counties Intensity/",experiment,"/",sep="")
      dir.create(outdir,showWarnings=FALSE,recursive=TRUE)
      
      df = ff
      
      # set up blank results matrix to hold results
      # 4 columns -- beta, cluster SE, t, p
      # 1 row per outcome variable
      results = matrix(NA,nrow=length(outvars),ncol=4)
      
      for (i in 1:length(outvars)) {
        print(i)
        flush.console()
        
        qty=outvars[i]
        fixedeffects=c("County",timeunit)
        
        # run main analysis function
        possibleError <- tryCatch({
          dat=rd(df,qty,switchvar,fixedeffects,controls,modtype)
        }, error=function(e){cat("model:",models[k],"; experiment:",experiments[j],"; outcome:",outvars[i]," ERROR :",conditionMessage(e),"\n"); e})
        
        if(!inherits(possibleError,"error")){
          results[i,] = dat$results
        }
        
      }
      
      row.names(results)=outvars
      stargazer(results,type="latex",out=paste(outdir,"Results.tex",sep=""))
      save(list=c("results","outdir","outvars","controls","switchvar","modtype"),file=paste(outdir,"Intensity_Analysis_Annual","_Counties_",experiment,".Rdata",sep=""))
    }
  }
  
}

# -----------------------------------------------------------------------
# (4a-c) Basic Panel Structure - all points
# Universe = All grid cells
# Analog = To Potentially Treated (more info in FE)
# -----------------------------------------------------------------------

dointensitypanelgridcell = 1
if (dointensitypanelgridcell==1) {
  
  # 0.25 Degree Cells
  ff = fullpanel.allpoints.annual.0p25
  vn = names(ff)
  ov = outvars[outvars %in% vn]
  
  # only unit and unit+time FE (no trends)
  for (k in 1:length(models)) {
    
    modtype=models[k]
    
    for (j in experimentsindex) {
      
      # get correct subsets, directories, FEs
      experiment=experiments[j]
      switchvar=intensity_variable
      tauvar=tauvars[j]
      outdir=paste("RD Analysis Intensity",mydir,modtype,"/GridCells0p25/",experiment,"/",sep="")
      dir.create(outdir,showWarnings=FALSE,recursive=TRUE)
      
      df = ff
      
      # set up blank results matrix to hold results
      # 4 columns -- beta, cluster SE, t, p
      # 1 row per outcome variable
      results = matrix(NA,nrow=length(ov),ncol=4)
      
      for (i in 1:length(ov)) {
        print(i)
        flush.console()
        
        qty=ov[i]
        fixedeffects=c("locationid2",timeunit)
        
        # run main analysis function
        possibleError <- tryCatch({
          dat=rd(df,qty,switchvar,fixedeffects,controls,modtype)
        }, error=function(e){cat("model:",models[k],"; experiment:",experiments[j],"; outcome:",ov[i]," ERROR :",conditionMessage(e),"\n"); e})
        
        if(!inherits(possibleError,"error")){
          results[i,] = dat$results
        }
      }
      
      row.names(results)=ov
      stargazer(results,type="latex",out=paste(outdir,"Results.tex",sep=""))
      save(list=c("results","outdir","ov","controls","switchvar","modtype"),file=paste(outdir,"Intensity_Analysis_Annual","_GridCells0p25_",experiment,".Rdata",sep=""))
    }
  }
  
  
  # 0.125 Degree Cells
  ff = fullpanel.allpoints.annual.0p125
  vn = names(ff)
  ov = outvars[outvars %in% vn]
  
  # only unit and unit+time FE (no trends)
  for (k in 1:length(models)) {
    
    modtype=models[k]
    
    for (j in experimentsindex) {
      
      # get correct subsets, directories, FEs
      experiment=experiments[j]
      switchvar=intensity_variable
      tauvar=tauvars[j]
      outdir=paste("RD Analysis Intensity",mydir,modtype,"/GridCells0p125/",experiment,"/",sep="")
      dir.create(outdir,showWarnings=FALSE,recursive=TRUE)
      
      df = ff
      
      # set up blank results matrix to hold results
      # 4 columns -- beta, cluster SE, t, p
      # 1 row per outcome variable
      results = matrix(NA,nrow=length(ov),ncol=4)
      
      for (i in 1:length(ov)) {
        print(i)
        flush.console()
        
        qty=ov[i]
        fixedeffects=c("locationid2",timeunit)
        
        # run main analysis function
        possibleError <- tryCatch({
          dat=rd(df,qty,switchvar,fixedeffects,controls,modtype)
        }, error=function(e){cat("model:",models[k],"; experiment:",experiments[j],"; outcome:",ov[i]," ERROR :",conditionMessage(e),"\n"); e})
        
        if(!inherits(possibleError,"error")){
          results[i,] = dat$results
        }
      }
      
      row.names(results)=ov
      stargazer(results,type="latex",out=paste(outdir,"Results.tex",sep=""))
      save(list=c("results","outdir","ov","controls","switchvar","modtype"),file=paste(outdir,"Intensity_Analysis_Annual","_GridCells0p125_",experiment,".Rdata",sep=""))
    }
  }
  
  # 1 Degree Cells
  ff = fullpanel.allpoints.annual.1
  vn = names(ff)
  ov = outvars[outvars %in% vn]
  
  # only unit and unit+time FE (no trends)
  for (k in 1:length(models)) {
    
    modtype=models[k]
    
    for (j in experimentsindex) {
      
      # get correct subsets, directories, FEs
      experiment=experiments[j]
      switchvar=intensity_variable
      tauvar=tauvars[j]
      outdir=paste("RD Analysis Intensity",mydir,modtype,"/GridCells1/",experiment,"/",sep="")
      dir.create(outdir,showWarnings=FALSE,recursive=TRUE)
      
      df = ff
      
      # set up blank results matrix to hold results
      # 4 columns -- beta, cluster SE, t, p
      # 1 row per outcome variable
      results = matrix(NA,nrow=length(ov),ncol=4)
      
      for (i in 1:length(ov)) {
        print(i)
        flush.console()
        
        qty=ov[i]
        fixedeffects=c("locationid2",timeunit)
        
        # run main analysis function
        possibleError <- tryCatch({
          dat=rd(df,qty,switchvar,fixedeffects,controls,modtype)
        }, error=function(e){cat("model:",models[k],"; experiment:",experiments[j],"; outcome:",ov[i]," ERROR :",conditionMessage(e),"\n"); e})
        
        if(!inherits(possibleError,"error")){
          results[i,] = dat$results
        }
      }
      
      row.names(results)=ov
      stargazer(results,type="latex",out=paste(outdir,"Results.tex",sep=""))
      save(list=c("results","outdir","ov","controls","switchvar","modtype"),file=paste(outdir,"Intensity_Analysis_Annual","_GridCells1_",experiment,".Rdata",sep=""))
    }
  }
  
}

# -----------------------------------------------------------------------
# -----------------------------------------------------------------------
# (5a) Tau - Unit Level, FE = unit
# Universe = Plant Locations of a Given Switch Type (e.g., old coal plants)
# Analog = ToT
# -----------------------------------------------------------------------
# -----------------------------------------------------------------------

dotauintensityunit = 1
if (dotauintensityunit==1) {
  
  ff = fullpanel.plants.annual
 
  for (k in 1:length(models)) {
    
    modtype=models[k]
    
    for (j in experimentsindex) {
      
      # get correct subsets, directories, FEs
      experiment=experiments[j]
      tauvar=tauvars[j]
      loadvar=intensity_variable
      outdir=paste("Tau Analysis Intensity",mydir,modtype,"/PlantType/",experiment,"/",sep="")
      dir.create(outdir,showWarnings=FALSE,recursive=TRUE)
      
      if (experiment=="onng") {
        df=subset(ff,ff$newngplant==1)
      } else if (experiment=="oncoal") {
        df=subset(ff,ff$newcoalplant==1)
      } else if (experiment=="offcoal") {
        df=subset(ff,ff$oldcoalplant==1)
      } 
      
      for (i in 1:length(outvars)) {
        print(i)
        flush.console()
        
        qty=outvars[i]
        fixedeffects=c("facunitid",timeunit)
        
        # run main analysis function
        possibleError <- tryCatch({
          dat=tau_intensity_fn(df,qty,tauvar,loadvar,nyears,fixedeffects,modtype)
        }, error=function(e){cat("model:",models[k],"; experiment:",experiments[j],"; outcome:",outvars[i]," ERROR :",conditionMessage(e),"\n"); e})
        
        # save individual results
        if(!inherits(possibleError,"error")) {
          tmptab = rbind(dat$results)
          stargazer(tmptab,type="latex",out=paste(outdir,qty,"_Results.tex",sep=""))
          assign(qty,tmptab)
        }
      }
      
      a = ls()
      save(list=a[a %in% outvars],file=paste(outdir,"Intensity_Analysis_Tau_Annual","_PlantType_",experiment,".Rdata",sep=""))
    }
  }
  
}

# -----------------------------------------------------------------------
# (5b) Tau - Unit Level, FE = location
# Universe = Plant Locations of a Given Switch Type (e.g., old coal plants)
# Analog = ToT
# -----------------------------------------------------------------------

dotauintensityloc = 1
if (dotauintensityloc==1) {
  
  ff = fullpanel.plants.annual

  for (k in 1:length(models)) {
    
    modtype=models[k]
    
    for (j in experimentsindex) {
      
      # get correct subsets, directories, FEs
      experiment=experiments[j]
      tauvar=tauvars[j]
      loadvar=intensity_variable
      outdir=paste("Tau Analysis Intensity LOCATION",mydir,modtype,"/PlantType/",experiment,"/",sep="")
      dir.create(outdir,showWarnings=FALSE,recursive=TRUE)
      
      if (experiment=="onng") {
        df=subset(ff,ff$newngplant==1)
      } else if (experiment=="oncoal") {
        df=subset(ff,ff$newcoalplant==1)
      } else if (experiment=="offcoal") {
        df=subset(ff,ff$oldcoalplant==1)
      } 
      
      for (i in 1:length(outvars)) {
        print(i)
        flush.console()
        
        qty=outvars[i]
        fixedeffects=c("locationid",timeunit)
        
        # run main analysis function
        possibleError <- tryCatch({
          dat=tau_intensity_fn(df,qty,tauvar,loadvar,nyears,fixedeffects,modtype)
        }, error=function(e){cat("model:",models[k],"; experiment:",experiments[j],"; outcome:",outvars[i]," ERROR :",conditionMessage(e),"\n"); e})
        
        # save individual results
        if(!inherits(possibleError,"error")) {
          tmptab = rbind(dat$results)
          stargazer(tmptab,type="latex",out=paste(outdir,qty,"_Results.tex",sep=""))
          assign(qty,tmptab)
        }
      }
      
      a = ls()
      save(list=a[a %in% outvars],file=paste(outdir,"Intensity_Analysis_Tau_Annual_Location","_PlantType_",experiment,".Rdata",sep=""))
    }
  }
  
}

# ----------------------------------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------------------------------
# Run IV analysis on main outcomes
# ----------------------------------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------------------------------
# (1a) IV for PM, unit FE, by switch type (ToT)
# -----------------------------------------------------------------------

doIVunit = 1
if (doIVunit==1) {
  
  ff = fullpanel.plants.annual

  for (k in 1:length(models)) {
    
    modtype=models[k]
    
    for (j in experimentsindex) {
      
      # get correct subsets, directories, FEs
      experiment=experiments[j]
      switchvar=switch_variable[j]
      tauvar=tauvars[j]
      instrumentedvar="pm.0km"
      outdir=paste("IV Analysis PM0km",mydir,modtype,"/PlantType/",experiment,"/",sep="")
      dir.create(outdir,showWarnings=FALSE,recursive=TRUE)
      
      if (experiment=="onng") {
        df=subset(ff,ff$newngplant==1 & !is.na(ff[,tauvar]))
      } else if (experiment=="oncoal") {
        df=subset(ff,ff$newcoalplant==1 & !is.na(ff[,tauvar]))
      } else if (experiment=="offcoal") {
        df=subset(ff,ff$oldcoalplant==1 & !is.na(ff[,tauvar]))
      } 
      
      # set up blank results matrix to hold results
      # 4 columns -- beta, cluster SE, t, p
      # 1 row per outcome variable
      results = matrix(NA,nrow=length(ivoutvars),ncol=4)
      
      for (i in 1:length(ivoutvars)) {
        print(i)
        flush.console()
        
        qty=ivoutvars[i]
        fixedeffects=c("facunitid",timeunit)
        
        # run main analysis function
        possibleError <- tryCatch({
          dat=iv_fn(df,qty,instrumentedvar,switchvar,fixedeffects,controls,modtype)
        }, error=function(e){cat("model:",models[k],"; experiment:",experiments[j],"; outcome:",ivoutvars[i]," ERROR :",conditionMessage(e),"\n"); e})
        
        if(!inherits(possibleError,"error")){
          results[i,] = dat$results
        }
        
        # Can save full shebang per variable -- slows down.
        #assign(qty,dat)
        #saveRDS(qty,file=paste(outdir,experiment,"_",qty,".RDS",sep=""))
      }
      
      row.names(results)=ivoutvars
      stargazer(results,type="latex",out=paste(outdir,"Results.tex",sep=""))
      save(list=c("results","outdir","ivoutvars","controls","switchvar","modtype"),file=paste(outdir,"IV_Analysis_Annual","_PlantType_",experiment,".Rdata",sep=""))
    }
  }
  
}

# -----------------------------------------------------------------------
# (1b) IV for PM, location FE, by switch type (ToT)
# -----------------------------------------------------------------------

doIVloc = 1
if (doIVloc==1) {
  
  ff = fullpanel.plants.annual

  for (k in 1:length(models)) {
    
    modtype=models[k]
    
    for (j in experimentsindex) {
      
      # get correct subsets, directories, FEs
      experiment=experiments[j]
      switchvar=switch_variable[j]
      tauvar=tauvars[j]
      instrumentedvar="pm.0km"
      outdir=paste("IV Analysis PM0km LOCATION",mydir,modtype,"/PlantType/",experiment,"/",sep="")
      dir.create(outdir,showWarnings=FALSE,recursive=TRUE)
      
      if (experiment=="onng") {
        df=subset(ff,ff$newngplant==1 & !is.na(ff[,tauvar]))
      } else if (experiment=="oncoal") {
        df=subset(ff,ff$newcoalplant==1 & !is.na(ff[,tauvar]))
      } else if (experiment=="offcoal") {
        df=subset(ff,ff$oldcoalplant==1 & !is.na(ff[,tauvar]))
      } 
      
      # set up blank results matrix to hold results
      # 4 columns -- beta, cluster SE, t, p
      # 1 row per outcome variable
      results = matrix(NA,nrow=length(ivoutvars),ncol=4)
      
      for (i in 1:length(ivoutvars)) {
        print(i)
        flush.console()
        
        qty=ivoutvars[i]
        fixedeffects=c("locationid",timeunit)
        
        # run main analysis function
        possibleError <- tryCatch({
          dat=iv_fn(df,qty,instrumentedvar,switchvar,fixedeffects,controls,modtype)
        }, error=function(e){cat("model:",models[k],"; experiment:",experiments[j],"; outcome:",ivoutvars[i]," ERROR :",conditionMessage(e),"\n"); e})
        
        if(!inherits(possibleError,"error")){
          results[i,] = dat$results
        }
        
        # Can save full shebang per variable -- slows down.
        #assign(qty,dat)
        #saveRDS(qty,file=paste(outdir,experiment,"_",qty,".RDS",sep=""))
      }
      
      row.names(results)=ivoutvars
      stargazer(results,type="latex",out=paste(outdir,"Results.tex",sep=""))
      save(list=c("results","outdir","ivoutvars","controls","switchvar","modtype"),file=paste(outdir,"IV_Analysis_Annual_Location","_PlantType_",experiment,".Rdata",sep=""))
    }
  }
  
}

# -----------------------------------------------------------------------
# (2a) IV for PM, location FE, by switch type (ToT)
# -----------------------------------------------------------------------

doIVplantunit = 1
if (doIVplantunit==1) {
  
  ff = fullpanel.plants.annual
 
  for (k in 1:length(models)) {
    
    modtype=models[k]
    
    for (j in experimentsindex) {
      
      # get correct subsets, directories, FEs
      experiment=experiments[j]
      switchvar=switch_variable[j]
      tauvar=tauvars[j]
      instrumentedvar="pm.0km"
      outdir=paste("IV Analysis PM0km",mydir,modtype,"/FuelType/",experiment,"/",sep="")
      dir.create(outdir,showWarnings=FALSE,recursive=TRUE)
      
      if (experiment=="onng") {
        df=subset(ff,ff$fueltype=="Natural Gas" & is.na(ff[,"tau_off"]))
      } else if (experiment=="oncoal") {
        df=subset(ff,ff$fueltype=="Coal" & is.na(ff[,"tau_off"]))
      } else if (experiment=="offcoal") {
        df=subset(ff,ff$fueltype=="Coal" & is.na(ff[,"tau_on"]))
      } 
      
      # set up blank results matrix to hold results
      # 4 columns -- beta, cluster SE, t, p
      # 1 row per outcome variable
      results = matrix(NA,nrow=length(ivoutvars),ncol=4)
      
      for (i in 1:length(ivoutvars)) {
        print(i)
        flush.console()
        
        qty=ivoutvars[i]
        fixedeffects=c("facunitid",timeunit)
        
        # run main analysis function
        possibleError <- tryCatch({
          dat=iv_fn(df,qty,instrumentedvar,switchvar,fixedeffects,controls,modtype)
        }, error=function(e){cat("model:",models[k],"; experiment:",experiments[j],"; outcome:",ivoutvars[i]," ERROR :",conditionMessage(e),"\n"); e})
        
        if(!inherits(possibleError,"error")){
          results[i,] = dat$results
        }
        
        # Can save full shebang per variable -- slows down.
        #assign(qty,dat)
        #saveRDS(qty,file=paste(outdir,experiment,"_",qty,".RDS",sep=""))
      }
      
      row.names(results)=ivoutvars
      stargazer(results,type="latex",out=paste(outdir,"Results.tex",sep=""))
      save(list=c("results","outdir","ivoutvars","controls","switchvar","modtype"),file=paste(outdir,"IV_Analysis_Annual","_FuelType_",experiment,".Rdata",sep=""))
    }
  }
  
}

# -----------------------------------------------------------------------
# (2b) IV for PM, location FE, by switch type (ToT)
# -----------------------------------------------------------------------

doIVplantloc = 1
if (doIVplantloc==1) {
  
  ff = fullpanel.plants.annual

  for (k in 1:length(models)) {
    
    modtype=models[k]
    
    for (j in experimentsindex) {
      
      # get correct subsets, directories, FEs
      experiment=experiments[j]
      switchvar=switch_variable[j]
      tauvar=tauvars[j]
      instrumentedvar="pm.0km"
      outdir=paste("IV Analysis PM0km LOCATION",mydir,modtype,"/FuelType/",experiment,"/",sep="")
      dir.create(outdir,showWarnings=FALSE,recursive=TRUE)
      
      if (experiment=="onng") {
        df=subset(ff,ff$fueltype=="Natural Gas" & is.na(ff[,"tau_off"]))
      } else if (experiment=="oncoal") {
        df=subset(ff,ff$fueltype=="Coal" & is.na(ff[,"tau_off"]))
      } else if (experiment=="offcoal") {
        df=subset(ff,ff$fueltype=="Coal" & is.na(ff[,"tau_on"]))
      } else if (experiment=="on") {
        
      # set up blank results matrix to hold results
      # 4 columns -- beta, cluster SE, t, p
      # 1 row per outcome variable
      results = matrix(NA,nrow=length(ivoutvars),ncol=4)
      
      for (i in 1:length(ivoutvars)) {
        print(i)
        flush.console()
        
        qty=ivoutvars[i]
        fixedeffects=c("locationid",timeunit)
        
        # run main analysis function
        possibleError <- tryCatch({
          dat=iv_fn(df,qty,instrumentedvar,switchvar,fixedeffects,controls,modtype)
        }, error=function(e){cat("model:",models[k],"; experiment:",experiments[j],"; outcome:",ivoutvars[i]," ERROR :",conditionMessage(e),"\n"); e})
        
        if(!inherits(possibleError,"error")){
          results[i,] = dat$results
        }
        
        # Can save full shebang per variable -- slows down.
        #assign(qty,dat)
        #saveRDS(qty,file=paste(outdir,experiment,"_",qty,".RDS",sep=""))
        }
      
      row.names(results)=ivoutvars
      stargazer(results,type="latex",out=paste(outdir,"Results.tex",sep=""))
      save(list=c("results","outdir","ivoutvars","controls","switchvar","modtype"),file=paste(outdir,"IV_Analysis_Annual_Location","_FuelType_",experiment,".Rdata",sep=""))
      }
    }
  }
}

# Not included here - grid-cell level IVs - very slow.
      
# ----------------------------
setwd("../")
      
