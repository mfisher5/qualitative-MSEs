########### Create Figure 2 ###########
#
# Fig 2 shows qMSE parameters.
#
# !!! This script will not run without the file "Misc.AEP.R",
#     which is not publicly available in this repo. !!!
#
# Written by AE Punt
#
################################################################


# Set up ------------------------------------------------------------------
library(tidyverse)
library(here)
library("devtools")
library(RcppArmadillo)
library(Rcpp)
library(TMB)
library(DirichletReg)

source(here('R','fxns','Misc.AEP.R'))

# =======================================================================
# =======================================================================

PlotDyn <- function(BasicData)
 {
  par(mfrow=c(4,2),oma=c(2,2,2,2),mar=c(5,4,1,1))
  Ages <- seq(from=0,to=Amax-1,by=1)
  
  Iletter <- 1
  
  for (Istock in 1:2)
   {
    plot(Ages,100*BasicData[[Istock]]$LengthA,xlab="Age (yr)",ylab="Length (cm)",type="l",lty=1,ylim=c(0,110),yaxs="i",xaxs="i") 
    lines(Ages,100*BasicData[[Istock]]$LengthB,lty=2)
    if (Istock==1) legend("bottomright",leg=c("Start-year","Mid-year"),lty=1:2,bty="n",cex=1.4)
    text(1,100,paste0("(",letters[Iletter],")"),cex=1.4)
    Iletter <- Iletter + 1
   }
  for (Istock in 1:2)
  {
    plot(Ages,BasicData[[Istock]]$Wstart,xlab="Age (yr)",ylab="Weight (kg)",type="l",lty=1,ylim=c(0,1.10),yaxs="i",xaxs="i") 
    lines(Ages,BasicData[[Istock]]$Wmid,lty=2)
    if (Istock==1) legend("bottomright",leg=c("Start-year","Mid-year"),lty=1:2,bty="n",cex=1.4)
    text(1,1.00,paste0("(",letters[Iletter],")"),cex=1.4)
    Iletter <- Iletter + 1
  }
  for (Istock in 1:2)
   {
    plot(Ages,BasicData[[Istock]]$Fec,xlab="Age (yr)",ylab="Maturity",type="l",lty=1,ylim=c(0,1.10),yaxs="i",xaxs="i") 
    text(1,1.00,paste0("(",letters[Iletter],")"),cex=1.4)
    Iletter <- Iletter + 1
   }
  for (Istock in 1:2)
   {
    plot(Ages,BasicData[[Istock]]$MoveP[1,1,1,],xlab="Age (yr)",ylab="Probaility of not moving",type="l",lty=1,ylim=c(0,1.10),yaxs="i",xaxs="i") 
    lines(Ages,BasicData[[Istock]]$MoveP[1,2,2,],lty=2)
    if (Istock==1) legend("bottomright",leg=c("Area 1","Area 2"),lty=1:2,bty="n",cex=1.4)
    text(1,0.10,paste0("(",letters[Iletter],")"),cex=1.4)
    Iletter <- Iletter + 1
  }
  
  par(mfrow=c(2,2),oma=c(2,2,2,2),mar=c(5,4,1,1))
  Iletter <- 1
  for (Istock in 1:2)
  {
    plot(Ages,BasicData[[Istock]]$Sel[1,],xlab="Age (yr)",ylab="Fishery selectivity",type="l",lty=1,ylim=c(0,1.10),yaxs="i",xaxs="i") 
    lines(Ages,BasicData[[Istock]]$Sel[2,],lty=2)
    if (Istock==1) lines(Ages,BasicData[[Istock]]$Sel[3,],lty=3)
    if (Istock==1) legend("bottomright",leg=c("Fisheries 1 and 2","Fishery 3"),lty=c(1,2),bty="n",cex=1.2)
    text(1,1.00,paste0("(",letters[Iletter],")"),cex=1.4)
    Iletter <- Iletter + 1
  }
  for (Istock in 1:2)
  {
    plot(Ages,BasicData[[Istock]]$SurvSe[1,],xlab="Age (yr)",ylab="Survey selectivity",type="l",lty=1,ylim=c(0,1.10),yaxs="i",xaxs="i") 
    lines(Ages,BasicData[[Istock]]$SurvSe[2,],lty=2)
    text(1,1.00,paste0("(",letters[Iletter],")"),cex=1.3)
    Iletter <- Iletter + 1
  }

  set.seed(19101)  
  par(mfrow=c(3,1),oma=c(8,8,5,6),mar=c(0,4,0,1))
  Years <- 1:Nburn
  Istock <- 1
  for (Ifleet in 1:Maxfleet)
   {
    plot(Years,BasicData[[Istock]]$FtrajO[1:Nburn,Ifleet],xlab="Year",ylab="",yaxs="i",ylim=c(0,1.9),lwd=3,type='l',lty=1,xlim=c(15,52),xaxs="i",axes=F) 
    box()
    axis(2,cex.axis=1.4,cex.lab=1.2)
    if (Ifleet == Maxfleet) axis(1,cex.axis=1.4)
    for (Irep in 1:5)
     {
      Vals <- BasicData[[Istock]]$FtrajO[1:Nburn,Ifleet]*exp(rnorm(1:Nburn,0,0.3)-0.3^2/2) 
      lines(1:Nburn,Vals,lty=Irep)
      }
    }
   mtext("Year",side=1,line=3.5,outer=T,cex=1.3)
   mtext("Relative fishing mortality rate",side=2,line=2,outer=T,cex=1.3)
 }

# =======================================================================
# =======================================================================

 Narea  <- 2                                               # Number of areas
 NareaFut <- 3                                             # Number of areas in the OM
 #Nfleet <- 3                                               # Number of fleets   
 Amax <- 21                                                # Number of ages
 Ages <- seq(from=1,to=Amax)-1
 Nburn <- 50                                               # Burn-in lengths
 Nyear <- Nburn + 50                                       # Number of projection years
 Nstock <- 2                                               # Number of stocks in the OM   
 ChangSel <- rep(F,Nstock)                                 # Does selectivity change? (management action)
 ChangMov <- c(0,0)                                        # Movement into area 3 (climate)  
 ChangKappa <- c(0,0)                                      # Changed growth in the future (climate)
 ChangeAlpha <- c(0.05*0,0)                                # changed recruitment in the future (climate)
 Nsim <- 20                                                # Number of simulations 
 Opt <- c(1,1,1)                                           # Run all steps
 
 
 Nareas <- 2                                                # Number of areas in the assessment
 HCRopt <- 1                                                # Which HCR (0=Thorson; 1=perfect; 2=assessment-based)
 Closed <- c(1,1,1)                                         # Set future closures
 AssFreq <- 5                                               # Assessment frequency
 
 Base1 <- Stock1()
 Base2 <- Stock2()
 Ages <- seq(from=0,to=Amax-1)
 

# Figure 2 ----------------------------------------------------------------

 pdf(here('doc','Fig2-ices.pdf'),width=6.692913,height=5.2)
 par(mfrow=c(3,2),oma=c(1,2,1,2),mar=c(4,6,0,1),mgp=c(2, 0.5, 0))
 ymax <-max(Base1$LengthB,Base2$LengthB)*1.1
 plot(Ages,Base1$LengthB,ylab="Mid-year length",xlab="Age",ylim=c(0,ymax),yaxs="i",type="l",cex.lab=1.25,lwd=1.5,cex.axis=1)
 lines(Ages,Base2$LengthB,lty=2)
 legend("bottomright",leg=c("Species 1","Species 2"),lty=c(1,2),bty="n",cex=1)
 text(0.8,1,"(a)",cex=1.5)
 ymax <-max(Base1$Fec,Base2$Fec )*1.1
 plot(Ages,Base1$Fec,ylab="Fecundity",xlab="Age",ylim=c(0,ymax),yaxs="i",type="l",cex.lab=1.25,lwd=1.5,cex.axis=1)
 lines(Ages,Base2$Fec,lty=2,lwd=1.5)
 legend("bottomright",leg=c("Species 1","Species 2"),lty=c(1,2),bty="n",cex=1)
 text(0.8,1,"(b)",cex=1.5)

 ymax <-max(Base1$Sel)*1.1
 plot(Ages,Base1$Sel[1,],ylab="Selecivity\n(Species 1)",xlab="Age",ylim=c(0,ymax),yaxs="i",type="l",cex.lab=1.25,lwd=1.5,cex.axis=1)
 lines(Ages,Base1$Sel[3,],lty=2,lwd=1.5)
 text(0.8,1,"(c)",cex=1.5)
 legend("bottomright",leg=c("Fishery 1","Fishery 2"),lty=c(1,2),bty="n",cex=1)
 ymax <-max(Base2$Sel)*1.1
 plot(Ages,Base2$Sel[1,],ylab="Selecivity\n(Species 2)",xlab="Age",ylim=c(0,ymax),yaxs="i",type="l",cex.lab=1.25,lwd=1.5,cex.axis=1)
 lines(Ages,Base2$Sel[3,],lty=2,lwd=1.5)
 text(0.8,1,"(d)",cex=1.5)
 legend("bottomright",leg=c("Fishery 1","Fishery 2"),lty=c(1,2),bty="n",cex=1)
 
 Years <- 1:Nburn
 plot(Years,Base1$Ftraj[1:Nburn,1],xlab="Year",ylab="Relative fishing\nmortality",ylim=c(0,1.1),yaxs="i",type="l",cex.lab=1.25,lwd=1.5,cex.axis=1)
 lines(Years,Base1$Ftraj[1:Nburn,2],lty=2,lwd=1.5)
 text(2.3,1,"(e)",cex=1.5)
 legend("bottomleft",leg=c("Fishery 1-inhore","Fishery 1-offshore","Fishery 2-offshore"),lty=c(1,2,2),bty="n",cex=0.75)

 xx <- c(0,0.1,0.4,1.4); yy <- c(0,0,1,1)
 plot(xx,yy,xlab=expression(paste("Biomass relative to ",B[0])),ylab=expression(paste("Fishing mortality / ", F[target])),ylim=c(0,1.1),xlim=c(0,1.4),xaxs="i",yaxs="i",type="l",lwd=4,cex.lab=1.25,cex.axis=1)
 text(0.11,1,"(f)",cex=1.5)
 dev.off()