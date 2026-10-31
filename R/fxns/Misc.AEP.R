Stock1 <- function()
{
  Nfleet <- 3
  BasicData <- NULL
  BasicData$Narea <- Narea
  BasicData$AreaFleet <- c(1,2,2)-1
  BasicData$PropRec <- c(1,rep(0,Narea-1))
  BasicData$Amax <- Amax
  BasicData$Nfleet <- Nfleet
  BasicData$M <- rep(0.2,Amax)
  BasicData$Sel <- matrix(0,nrow=Nfleet,ncol=Amax)
  BasicData$Sel[1,] <- 1.0/(1.0+exp(-log(19)*(Ages-3)/4))
  BasicData$Sel[2,] <- 1.0/(1.0+exp(-log(19)*(Ages-3)/4))
  BasicData$Sel[3,] <- 1.0/(1.0+exp(-log(19)*(Ages-5)/4))
  BasicData$SurvSel <- matrix(0,nrow=Narea,ncol=Amax)
  BasicData$SurvSel[1,] <- 1.0/(1.0+exp(-log(19)*(Ages-4)/4))
  BasicData$SurvSel[2,] <- 1.0/(1.0+exp(-log(19)*(Ages-4)/4))
  Sel2 <-1.0/(1.0+exp(-log(19)*(Ages-4)/4))
  BasicData$Sel2 <- Sel2
  
  BasicData$MoveP2 <- matrix(0,nrow=Narea*Narea,ncol=Amax)
  Mage0 <- 0.00001; Mage5 <- 0.2
  for (Iage in c(1:Amax))
    for (Jarea in 1:Narea)
    {
      Iarea <- 1
      if (Iage-1 <= 1) 
        MoveV <- Mage0
      else
        if (Iage-1 >= 5)
         MoveV <- Mage5  
        else
          { MoveV <- Mage0 + (Mage5-Mage0)*(Iage-1-1)/(5.0-1); }

      if (Iarea==Jarea)
      { BasicData$MoveP2[(Iarea-1)*Narea+Jarea,Iage] <- 1-MoveV; }
      else
      { BasicData$MoveP2[(Iarea-1)*Narea+Jarea,Iage] <- MoveV; }
    }
  for (Iage in c(1:Amax))
    for (Jarea in 1:Narea)
    {
      Iarea <- 2
      if (Iarea==Jarea)
      { BasicData$MoveP2[(Iarea-1)*Narea+Jarea,Iage] <- 0.99; }
      else
      { BasicData$MoveP2[(Iarea-1)*Narea+Jarea,Iage] <- 0.01; }
    }
 
  # Growth stuff
  Kappa <- 0.2; t0 <- 0.2
  LengthA <- (1.0-exp(-Kappa*(Ages-t0)))
  BasicData$Wstart <- LengthA^3
  LengthB <- (1.0-exp(-Kappa*(Ages+0.5-t0)))
  BasicData$Wmid <- LengthB^3
  #time-variation
  LenAYear <- matrix(0,nrow=Nyear,ncol=Amax)
  LenBYear <- matrix(0,nrow=Nyear,ncol=Amax)
  LenAYear[1,] <- LengthA
  LenBYear[1,] <- LengthB
  KappYout <-  Kappa
  for (Iyear in 2:Nyear)
   {
    if (Iyear >Nburn)
     KappaY <- Kappa*(1+ChangKappa[1]*(Iyear-Nburn)/(Nyear-Nburn))
    else
     KappaY <- Kappa
    KappYout[Iyear] <- KappaY
      LenAYear[Iyear,1] <- (1.0-exp(-KappaY*(0-t0)))
    LenBYear[Iyear,1] <- (1.0-exp(-KappaY*(0-t0+0.5)))
    for (Iage in 2:Amax)
     {
      LenAYear[Iyear,Iage] <- LenAYear[Iyear-1,Iage-1] + (1-LenAYear[Iyear-1,Iage-1])*(1.0-exp(-KappaY))
      LenBYear[Iyear,Iage] <- LenBYear[Iyear-1,Iage-1] + (1-LenBYear[Iyear-1,Iage-1])*(1.0-exp(-KappaY))
     }
    }
  WstartYear <- LenAYear^3.0
  WmidYear <- LenBYear^3.0
  BasicData$LengthA <- LengthA
  BasicData$LengthB <- LengthB
  BasicData$WstartYear <- WstartYear
  BasicData$WmidYear <-  WmidYear
  BasicData$KappaYout <- KappYout
  
  # Maturity
  BasicData$Fec <- 1.0/(1.0+exp(-log(19)*(Ages-6)/4)) 
  
  BasicData$Nyear <- Nyear
  BasicData$Nburn <- Nburn
  Steepness <- 0.7
  SigmaR <- 0.6
  BasicData$Steepness <- Steepness
  BasicData$SigmaR <- SigmaR
  # Set up the movement matrix
  MoveP <- array(0,dim=c(Nyear,NareaFut,NareaFut,Amax))
  for (Iyear in 1:Nyear)
   for (Iarea in 1:Narea)
    for (Karea in 1:Narea)
      for (Iage in 1:Amax)
        MoveP[Iyear,Iarea,Karea,Iage] = BasicData$MoveP2[(Iarea-1)*Narea+Karea,Iage];
  
  # ------------------------------------------------------------------------------------
  
  # Scaling between Fs
  Fscale <- c(1,3,2); Fscale<-Fscale/sum(Fscale)
  
  Ftraj <- matrix(0,nrow=Nyear,ncol=Nfleet)
  Ftraj[1:25,1] <- 0
  Ftraj[25:30,1] <- seq(from=0,to=1,length=6)
  Ftraj[31:Nyear,1] <- 1
  Ftraj[1:30,2] <- 0
  Ftraj[30:35,2] <- seq(from=0,to=1,length=6)
  Ftraj[36:Nyear,2] <- 1
  Ftraj[1:30,3] <- 0
  Ftraj[30:35,3] <- seq(from=0,to=1,length=6)
  Ftraj[36:Nyear,3] <- 1
  BasicData$FtrajO <- Ftraj
  BasicData$Ftraj <- Ftraj
  BasicData$Fscale <- Fscale
  
  # ----------------------------------------------------------------------------------------
  
  # Change in selection
  SelYear <- array(0,dim=c(Nyear,Nfleet,Amax))
  for (Iyear in 1:Nyear)  SelYear[Iyear,,] <- BasicData$Sel  
  if (ChangSel[1]==T)
   for (Iyear in (Nburn+1):Nyear) SelYear[Iyear,1,] <- Sel2  
  BasicData$SelYear <- SelYear
  
  # Change rate from area 2 and area 3
  MoveRateV <- rep(0,Nyear)
  for (Iyear in Nburn:Nyear)  
   for (Iage in 1:Amax)  
    {
     MoveO <- MoveP[Iyear,2,2,Iage]
     Prop <- ChangMov[1]*(Iyear-Nburn)/(Nyear-Nburn)
     MoveRateV[Iyear] <- Prop 
     MoveP[Iyear,2,2,Iage] <- (1-Prop)*MoveO
     MoveP[Iyear,2,3,Iage] <- Prop*MoveO
     MoveP[Iyear,3,3,Iage] <- 0.99
     MoveP[Iyear,3,2,Iage] <- 0.01
   }
  BasicData$MoveP <- MoveP
  BasicData$MoveRateV <-MoveRateV

  return(BasicData)
}

# ----------------------------------------------------------------------------------

Stock2 <- function()
{
  Nfleet <- 3
  BasicData <- NULL
  BasicData$Narea <- Narea
  BasicData$AreaFleet <- c(1,2,2)-1
  BasicData$PropRec <- c(1,rep(0,Narea-1))
  BasicData$Amax <- Amax
  BasicData$Nfleet <- Nfleet
  BasicData$M <- rep(0.4,Amax)
  BasicData$Sel <- matrix(0,nrow=Nfleet,ncol=Amax)
  BasicData$Sel[1,] <- 1.0/(1.0+exp(-log(19)*(Ages-1)/2))
  BasicData$Sel[2,] <- 1.0/(1.0+exp(-log(19)*(Ages-1)/2))
  BasicData$Sel[3,] <- 1.0/(1.0+exp(-log(19)*(Ages-3)/3))
  #BasicData$Sel[3,] <- 1.0/(1.0+exp(-log(19)*(Ages-3)/4))
  BasicData$SurvSel <- matrix(0,nrow=Narea,ncol=Amax)
  BasicData$SurvSel[1,] <- 1.0/(1.0+exp(-log(19)*(Ages-3)/3))
  BasicData$SurvSel[2,] <- 1.0/(1.0+exp(-log(19)*(Ages-3)/3))
  Sel2 <- 1.0/(1.0+exp(-log(19)*(Ages-4)/4))
  BasicData$Sel2 <- Sel2
  
  BasicData$MoveP2 <- matrix(0,nrow=Narea*Narea,ncol=Amax)
  Mage0 <- 0.00001; Mage4 <- 0.1
  for (Iage in c(1:Amax))
    for (Jarea in 1:Narea)
    {
      Iarea <- 1
      if (Iage-1 <= 1) 
        MoveV <- Mage0
      else
        if (Iage-1 >= 4)
          MoveV <- Mage4  
        else
        { MoveV <- Mage0 + (Mage4-Mage0)*(Iage-1-1)/(4.0-1); }
        if (Iarea==Jarea)
      { BasicData$MoveP2[(Iarea-1)*Narea+Jarea,Iage] <- 1-MoveV; }
      else
      { BasicData$MoveP2[(Iarea-1)*Narea+Jarea,Iage] <- MoveV; }
    }
  for (Iage in c(1:Amax))
    for (Jarea in 1:Narea)
    {
      Iarea <- 2
      if (Iarea==Jarea)
      { BasicData$MoveP2[(Iarea-1)*Narea+Jarea,Iage] <- 0.99; }
      else
      { BasicData$MoveP2[(Iarea-1)*Narea+Jarea,Iage] <- 0.01; }
    }
  
  # growth stuff
  Kappa <- 0.4; t0 <- 0.2
  LengthA <- (1.0-exp(-Kappa*(Ages-t0)))
  BasicData$Wstart <- LengthA^3
  LengthB <- (1.0-exp(-Kappa*(Ages+0.5-t0)))
  BasicData$Wmid <- LengthB^3
  #time-variation
  LenAYear <- matrix(0,nrow=Nyear,ncol=Amax)
  LenBYear <- matrix(0,nrow=Nyear,ncol=Amax)
  LenAYear[1,] <- LengthA
  LenBYear[1,] <- LengthB
  for (Iyear in 2:Nyear)
  {
    if (Iyear >Nburn)
      KappaY <- Kappa*(1+ChangKappa[2]*(Iyear-Nburn)/(Nyear-Nburn))
    else
      KappaY <- Kappa
    LenAYear[Iyear,1] <- (1.0-exp(-KappaY*(0-t0)))
    LenBYear[Iyear,1] <- (1.0-exp(-KappaY*(0-t0+0.5)))
    for (Iage in 2:Amax)
    {
      LenAYear[Iyear,Iage] <- LenAYear[Iyear-1,Iage-1] + (1-LenAYear[Iyear-1,Iage-1])*(1.0-exp(-KappaY))
      LenBYear[Iyear,Iage] <- LenBYear[Iyear-1,Iage-1] + (1-LenBYear[Iyear-1,Iage-1])*(1.0-exp(-KappaY))
    }
  }
  WstartYear <- LenAYear^3.0
  WmidYear <- LenBYear^3.0
  BasicData$LengthA <- LengthA
  BasicData$LengthB <- LengthB
  BasicData$WstartYear <- WstartYear
  BasicData$WmidYear <-  WmidYear
  
  # Maturity
  BasicData$Fec <- 1.0/(1.0+exp(-log(19)*(Ages-3)/3)) 
 
  BasicData$Nyear <- Nyear
  BasicData$Nburn <- Nburn
  Steepness <- 0.8
  SigmaR <- 0.7
  BasicData$Steepness <- Steepness
  BasicData$SigmaR <- SigmaR
  # Set up the movement matrix
  MoveP <- array(0,dim=c(Nyear,NareaFut,NareaFut,Amax))
  for (Iyear in 1:Nyear)
   for (Iarea in 1:Narea)
    for (Karea in 1:Narea)
      for (Iage in 1:Amax)
        MoveP[Iyear,Iarea,Karea,Iage] = BasicData$MoveP2[(Iarea-1)*Narea+Karea,Iage];
  BasicData$MoveP <- MoveP
  
  # ------------------------------------------------------------------------------------
  
  # Scaling between Fs
  Fscale <- c(1,3,2); Fscale<-Fscale/sum(Fscale)
  
  Ftraj <- matrix(0,nrow=Nyear,ncol=Nfleet)
  Ftraj[1:25,1] <- 0
  Ftraj[25:30,1] <- seq(from=0,to=1,length=6)
  Ftraj[31:Nyear,1] <- 1
  Ftraj[1:30,2] <- 0
  Ftraj[30:35,2] <- seq(from=0,to=1,length=6)
  Ftraj[36:Nyear,2] <- 1
  Ftraj[1:30,3] <- 0
  Ftraj[30:35,3] <- seq(from=0,to=1,length=6)
  Ftraj[36:Nyear,3] <- 1
  BasicData$FtrajO <- Ftraj
  BasicData$Ftraj <- Ftraj
  BasicData$Fscale <- Fscale

  # ------------------------------------------------------------------------------------
  
  # Change in selection
  SelYear <- array(0,dim=c(Nyear,Nfleet,Amax))
  for (Iyear in 1:Nyear)  SelYear[Iyear,,] <- BasicData$Sel  
  if (ChangSel[1]==T)
    for (Iyear in (Nburn+1):Nyear) SelYear[Iyear,1,] <- Sel2  
  BasicData$SelYear <- SelYear
  
  # Change rate from area 2 and area 3
  for (Iyear in Nburn:Nyear)  
    for (Iage in 1:Amax)  
    {
      MoveO <- MoveP[Iyear,2,2,Iage]
      Prop <- ChangMov[1]*(Iyear-Nburn)/(Nyear-Nburn)
      MoveP[Iyear,2,2,Iage] <- (1-Prop)*MoveO
      MoveP[Iyear,2,3,Iage] <- Prop*MoveO
      MoveP[Iyear,3,3,Iage] <- 0.99
      MoveP[Iyear,3,2,Iage] <- 0.01
    }
  BasicData$MoveP <- MoveP
  
 return(BasicData)
}


# ======================================================================================

PlotFit <- function(Isim,Istock,Iyear)
{
  par(mfrow=c(4,3),oma=c(1,1,1,1),mar=c(5,4,2,1))
  plot(1:Iyear,SSB[Istock,1:Iyear],ylim=c(0,max(SSB[Istock,1:(Iyear)],Rpt$SSB)*1.2),type="l",lwd=2)
  lines(1:Iyear,Rpt$SSB,col="red")
  lines(1:(Iyear-1),SurvIndex[Istock,1:(Iyear-1),1],col="green")
  lines(1:(Iyear-1),SurvIndex[Istock,1:(Iyear-1),2],col="blue")
  title(paste0("Sim=",Isim,";Stock=",Istock,";Year=",Iyear))
  for (Ifleet in 1:Data$Nfleet)
   {
    plot(1:(Iyear-1),Yield[Istock,1:(Iyear-1),Ifleet],ylim=c(0,max(Yield[Istock,1:(Iyear-1),Ifleet],Rpt$Yield[,Ifleet])*1.2),yaxs="i",pch=16)
    lines(1:(Iyear-1),Rpt$Yield[,Ifleet],col="red")
  }
  if (Nareas==1)
   {
    plot(1:(Iyear-1),SurvIndexTE[Istock,1:(Iyear-1)],ylim=c(0,max(SurvIndexTE[Istock,1:(Iyear-1)],Rpt$q*Rpt$SurvEInd)*1.2),yaxs="i",pch=16)
      lines(1:(Iyear-1),Rpt$q*Rpt$SurvEInd,col="red")
    }
  if (Nareas==2)
   for (Iarea in 1:Narea)
    {
     plot(1:(Iyear-1),SurvIndex[Istock,1:(Iyear-1),Iarea],ylim=c(0,max(SurvIndex[Istock,1:(Iyear-1),Iarea],Rpt$q[Iarea]*Rpt$SurvEInd[,Iarea])*1.2),yaxs="i",pch=16)
     lines(1:(Iyear-1),Rpt$q[Iarea]*Rpt$SurvEInd[,Iarea],col="red")
   }
  for (Ifleet in 1:Data$Nfleet)
   {
    Obs <- rep(0,Amax);Pred <- rep(0,Amax)
    for (Iyear in 1:(Iyear-1))
     if (Data$CatchObs[Iyear,Ifleet]>0)
      {
       Obs <- Obs + Data$CAAObs[Iyear,Ifleet,]  
       Pred <- Pred + Rpt$CAA[Iyear,Ifleet,]
      }
    ymax <- max(Obs,Pred)*1.2
    plot(0,0,xlim=c(-0.5,Amax-0.5),ylim=c(0,ymax),yaxs="i",xaxs="i",type="n",ylab="Fishery")
    for (Iage in 1:Amax)
     {
      xx <- Iage-1+c(-0.5,-0.5,0.5,0.5); yy <- c(0,Obs[Iage],Obs[Iage],0)
      polygon(xx,yy,col="gray90")
     }
    lines(1:Amax-1,Pred)
  }
  if (Nareas==2)
   for (Iarea in 1:Narea)
    {
     Obs <- rep(0,Amax);Pred <- rep(0,Amax)
     for (Iyear in 1:(Iyear-1))
       if (Data$SurvIndex[Iyear,Iarea]>0)
       {
         Obs <- Obs + Data$SurvCAA[Iyear,Iarea,]  
         Pred <- Pred + Rpt$SurvECAA[Iyear,Iarea,]
       }
     ymax <- max(Obs,Pred)*1.2
     plot(0,0,xlim=c(-0.5,Amax-0.5),ylim=c(0,ymax),yaxs="i",xaxs="i",type="n",ylab="survey")
     for (Iage in 1:Amax)
     {
       xx <- Iage-1+c(-0.5,-0.5,0.5,0.5); yy <- c(0,Obs[Iage],Obs[Iage],0)
       polygon(xx,yy,col="gray90")
     }
     lines(1:Amax-1,Pred)
     
    }   
  if (Nareas==1)
   {
    Iarea <- 1
    Obs <- rep(0,Amax);Pred <- rep(0,Amax)
    for (Iyear in 1:(Iyear-1))
     if (Data$SurvIndex[Iyear,Iarea]>0)
      {
       Obs <- Obs + Data$SurvCAA[Iyear,Iarea,]  
       Pred <- Pred + Rpt$SurvECAA[Iyear,Iarea,]
      }
    ymax <- max(Obs,Pred)*1.2
    plot(0,0,xlim=c(-0.5,Amax-0.5),ylim=c(0,ymax),yaxs="i",xaxs="i",type="n",ylab="survey")
    for (Iage in 1:Amax)
     {
      xx <- Iage-1+c(-0.5,-0.5,0.5,0.5); yy <- c(0,Obs[Iage],Obs[Iage],0)
      polygon(xx,yy,col="gray90")
     }
    lines(1:Amax-1,Pred)
   }   
  plot(1:(Iyear-1),RecDevs[Istock,1:(Iyear-1)],pch=16,ylim=c(-2,2))
  lines(1:(Iyear-1),rep$par.fixed[1+1:(Iyear-1)])
 }

# ============================================================================================

Select <- function(Slope1,Slope2,L50)
{
  Sel <- rep(0,Amax)
  for (Iage in 0:(Amax-1))
  {
   join <- 1.0/(1.0+exp(10.0*(Iage-L50)));
   Sel[Iage+1] = join/(1.0+exp(-1*Slope1*(Iage-L50))) +(1.0-join)/(1.0+exp(Slope2*(Iage-L50))) +1.0e-10;
  }
 print(Sel) 
}  

# ============================================================================================

QuickPlot <- function(Results,Nstock,Nyear)
 {

  # quick plot
  par(mfrow=c(2,1),oma=c(2,8,2,8),mar=c(5,4,1,1))
  for (Istock in 1:Nstock)  
   {
    quants <- matrix(0,nrow=5,ncol=Nyear+1)
    for (Iyear in 1:(Nyear+1))
      quants[,Iyear] <- quantile(Results[Istock,,Iyear,6+Maxfleet],prob=c(0.05,0.25,0.5,0.75,0.95))
    ymax <- max(quants)*1.1
    plot(1:(Nyear+1),quants[3,],xlab="Year",ylab=paste0("Depletion, stock ",Istock),ylim=c(0,ymax),xaxs="i",yaxs="i",type="l")
    xx <- c(1:(Nyear+1),rev(1:(Nyear+1)))
    yy <- c(quants[1,],rev(quants[5,]))
    polygon(xx,yy,col="gray10")
    xx <- c(1:(Nyear+1),rev(1:(Nyear+1)))
    yy <- c(quants[2,],rev(quants[4,]))
    polygon(xx,yy,col="gray90")
    lines(1:(Nyear+1),quants[3,],lwd=3,col="blue")
    abline(h=0.4*Results[Istock,1,1,6+Maxfleet],col="green",lwd=2)
    abline(h=0.2*Results[Istock,1,1,6+Maxfleet],col="red",lwd=2)
    abline(v=Nburn+0.5,lty=2)
   }
 }

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
