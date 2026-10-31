########### Create Figure 3 ###########
#
# Fig 3 shows how qMSE parameters are changed
#       to incorporate effects of climate change.
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


# Figure 3 ----------------------------------------------------------------
ChangMov <- c(0.05,0)                                        # Movement into area 3 (climate)  
ChangKappa <- c(1,0)                                      # Changed growth in the future (climate)
ChangeAlpha <- c(0.5,1)                                # changed recruitment in the future (climate)

Base1 <- Stock1()
pdf(here('doc','Fig3-ices.pdf'),width=3.149606,height=5.11811)
# par(mfrow=c(3,1),oma=c(2,10,2,10),mar=c(5,6,1,2))
par(mfrow=c(3,1),oma=c(0,0,1,0),mar=c(4,5,0,2),mgp=c(2, 1, 0))
AlphaMult <- matrix(1,nrow=Nstock,ncol=Nyear)
for (Istock in 1:Nstock)
{
  for (Iyear in Nburn:(Nburn+10)) AlphaMult[Istock,Iyear] <- 1.0+(Iyear-Nburn)/10*(ChangeAlpha[Istock]-1)
  for (Iyear in (Nburn+10):Nyear) AlphaMult[Istock,Iyear] <-ChangeAlpha[Istock]
}

Years <- 1:Nyear
Istock <- 1
plot(Years,AlphaMult[Istock,],xlab="Year",ylab=expression(atop("Stock recruitment", paste(alpha, " multiplier"))),ylim=c(0,1.2),lty=1,type="l",lwd=3,cex.lab=1,cex.axis=1)
text(98,1.11,"(a)",cex=1.4)
plot(Years,Base1$MoveRateV,xlab="Year",ylab="Movement from \noffshore to far offshore",ylim=c(0,0.1),lty=1,type="l",lwd=3,cex.lab=1,cex.axis=1)
text(2,0.095,"(b)",cex=1.4)
plot(Years,Base1$KappaYout,xlab="Year",ylab=expression(paste("Growth rate (",yr^-1,")")),ylim=c(0.15,0.45),lty=1,type="l",lwd=3,cex.lab=1,cex.axis=1)
text(2,0.43,"(c)",cex=1.4)
dev.off()
