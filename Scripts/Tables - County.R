# -------------------------------------------------------------------------------------------------------------------
# Impacts Tables - County Level
# -------------------------------------------------------------------------------------------------------------------

setwd("Tables/")

stargazer(m_tot_1[[1]],m_tot_1[[2]],m_tot_1[[3]],
          m11[[1]],m.ivpm.11[[1]],m.ivpmsurf.11[[1]],
          m11[[15]],m.ivpm.11[[15]],m.ivpmsurf.11[[15]],
          m11[[16]],m.ivpm.11[[16]],m.ivpmsurf.11[[16]],
          m11[[17]],m.ivpm.11[[17]],m.ivpmsurf.11[[17]],
          out="TableS7_County_NCoal25.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
          out.header=T,dep.var.caption="Number of Coal-Fired Units within 25km of County",
          notes=c("All models include county and year fixed effects, with standard errors clustered at the county level."),
          notes.align="l"
)

stargazer(m_tot_4[[1]],m_tot_4[[2]],m_tot_4[[3]],
          m14[[1]],m.ivpm.14[[1]],m.ivpmsurf.14[[1]],
          m14[[15]],m.ivpm.14[[15]],m.ivpmsurf.14[[15]],
          m14[[16]],m.ivpm.14[[16]],m.ivpmsurf.14[[16]],
          m14[[17]],m.ivpm.14[[17]],m.ivpmsurf.14[[17]],
          out="TableS8_County_NCoal200.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
          out.header=T,dep.var.caption="Number of Coal-Fired Units within 200km of County",
          notes=c("All models include county and year fixed effects, with standard errors clustered at the county level."),
          notes.align="l"
)


stargazer(m_tot_12[[1]],m_tot_12[[2]],m_tot_12[[3]],
          m22[[1]],m.ivpm.22[[1]],m.ivpmsurf.22[[1]],
          m22[[15]],m.ivpm.22[[15]],m.ivpmsurf.22[[15]],
          m22[[16]],m.ivpm.22[[16]],m.ivpmsurf.22[[16]],
          m22[[17]],m.ivpm.22[[17]],m.ivpmsurf.22[[17]],
          out="TableS9_County_NCoalandNG200.tex",type="latex",style="default",float=T,float.env="table",font.size="tiny",keep.stat=c("n","adj.rsq"),
          out.header=T,dep.var.caption="Number of Coal- and Natural Gas- Fired Units within 200km of County",
          notes=c("All models include county and year fixed effects, with standard errors clustered at the county level."),
          notes.align="l"
)

setwd("../")

