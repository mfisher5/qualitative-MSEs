########### Main r script for QNMs ###########
#
# Written 2023-09-07
# Last Updated: 2023-01-14 -- mcf
# Last Run: 2024-01-14
#
##############################################


# Set up ------------------------------------------------------------------

## Functions
library(here)
library(tidyverse)
library(magrittr)
library(ggplot2)
library(gridExtra)
library(cowplot)

#Load XML first--dependency not included in QPress
#To install QPress: 
#   library(remotes)
#   remotes::install_github("SWotherspoon/QPress")
library(QPress)
library(XML)
library(tcltk2)
library(igraph)


## custom functions
source(here('R/fxns/parse.constraints.SWotherspoon.R'))
source(here('R/fxns/prepare.dia.R'))
# source(here('R/fxns/impact.table.R'))
source(here('R/fxns/compare.baseline.impact.table.R'))
source(here('R/fxns/compare.models.impact.table.R'))
# source(here('R/fxns/compare.models.impact.dataframe.R'))
source(here('R/fxns/alternating.list.R'))


## 
set.seed(555)




# Models ------------------------------------------------------------------

## create a list of lists, so that each operating model scenario (0-5) 
##        is run with the five management strategies (A-E)
##        management strategies A,D use the same underlying QNM structure
##        but strategy C requires a separate dia object.


##### 0. A baseline scenario #####
mlist0 <- list(
  m <- prepare.dia(dia_name='MSE-QNM_2023-Oct_00-constantF',dia_path=here('data','qnm_models'))[[1]],  # no constraints
  m.b <- prepare.dia(dia_name='MSE-QNM_2023-Oct_00B-rm-small-mesh',dia_path=here('data','qnm_models'))[[1]],  # no const
  m.c <- prepare.dia(dia_name='MSE-QNM_2023-Oct_00C-rm-small-mesh-redistribute-small',dia_path=here('data','qnm_models'))[[1]],  # no constraints
  m.e <- prepare.dia(dia_name='MSE-QNM_2023-Oct_00E-rm-inshore-small-mesh',dia_path=here('data','qnm_models'))[[1]]
)

#### 2. Effort scales according to the Thorson effort model (Thorson et al., 2013) ####
mlist2 <- list(
  m2 <- prepare.dia(dia_name='MSE-QNM_2023-Oct_2-thorsonF',dia_path=here('data','qnm_models'))[[1]],  # no constraints
  m2.b <- prepare.dia(dia_name='MSE-QNM_2023-Oct_2B-rm-small-mesh',dia_path=here('data','qnm_models'))[[1]],   # no constraints
  m2.c <- prepare.dia(dia_name='MSE-QNM_2023-Oct_2C-rm-small-mesh-redistribute-small',dia_path=here('data','qnm_models'))[[1]],  # no constraints
  m2.e <- prepare.dia(dia_name='MSE-QNM_2023-Oct_2E-rm-inshore-small-mesh',dia_path=here('data','qnm_models'))[[1]]  # no constraints
)

#### 3. Climate reduces productivity ####
mlist3 <- mlist0

#### 4. Animals move into the “far offshore” area ####
mlist4 <- list(
  m4 <- prepare.dia(dia_name='MSE-QNM_2023-Oct_004-movement-to-inaccessible',dia_path=here('data','qnm_models'))[[1]],  # no constraints
  m4.b <- prepare.dia(dia_name='MSE-QNM_2023-Oct_004B-rm-small-mesh',dia_path=here('data','qnm_models'))[[1]],
  m4.c <- prepare.dia(dia_name='MSE-QNM_2023-Oct_004C-rm-small-mesh-redistribute-small',dia_path=here('data','qnm_models'))[[1]],  # no constraints
  m4.e <- prepare.dia(dia_name='MSE-QNM_2023-Oct_004E-rm-inshore-small-mesh',dia_path=here('data','qnm_models'))[[1]]
   )

#### 5. Climate increases growth rate ####
mlist5 <- list(
  m5 <- prepare.dia(dia_name='MSE-QNM_2023-Oct_005-growth-perturb',dia_path=here('data','qnm_models'))[[1]],  # no constraints
  m5.b <- prepare.dia(dia_name='MSE-QNM_2023-Oct_005B-rm-small-mesh',dia_path=here('data','qnm_models'))[[1]],
  m5.c <- prepare.dia(dia_name='MSE-QNM_2023-Oct_005C-rm-small-mesh-redistribute-small',dia_path=here('data','qnm_models'))[[1]],  # no constraints
  m5.e <- prepare.dia(dia_name='MSE-QNM_2023-Oct_005E-rm-inshore-small-mesh',dia_path=here('data','qnm_models'))[[1]]
  )


mlist_all <- list(mlist0,mlist2,mlist3,mlist4,mlist5)
names(mlist_all) <- c("QNM O","QNM 2 Thorson Effort","QNM 3 Fewer Recruits", "QNM 4 Offshore Inaccessible","QNM 5 Faster Growth")


#### Model variable key ####
# I use this to order the variables in the output tables.
vkey <- read_csv(here('data','qnm_models','om_variable_key.csv'))
vkey %<>% bind_rows(
  data.frame(variable=c("offshore biomass inaccessible large","offshore biomass inaccessible small"),
             group=rep("ecosystem",2),
             group_order=rep(1,2),specific_order=rep(6,2))
  )

# Write QNM matrices ------------------------------------------------------

## write out models as matrices
##     for use in other programs,
##     and building FCMs

## all
for(i in seq(1,length(mlist_all))){
  tmp_name <- names(mlist_all)[i]
  tmp_list <- mlist_all[[i]]
  for(j in seq(1:4)){
      m.mat.out <- adjacency.matrix(tmp_list[[j]],labels=TRUE)
      m.mat.df <- as.data.frame(m.mat.out) %>% rownames_to_column("x")
      
      if(j==2){suffix <- "_hcrB"} else if(j==3){
        suffix <- "_hcrC"}else if(j==4){
        suffix <- "_hcrE"}else{suffix <- ""}
      
      m.mat.name <- paste0(str_replace_all(tmp_name, " ", "_"),suffix,'.csv')
      
      write_csv(m.mat.df, file=here('data','CSVs',m.mat.name))
  }
  print(i)
}


## specific OMs
tmp_list <- mlist4
tmp_name <- "QNM 4 Offshore Inaccessible"
for(j in seq(1:4)){
  m.mat.out <- adjacency.matrix(tmp_list[[j]],labels=TRUE)
  m.mat.df <- as.data.frame(m.mat.out) %>% rownames_to_column("x")
  
  if(j==2){suffix <- "_hcrB"} else if(j==3){
    suffix <- "_hcrC"}else if(j==4){
      suffix <- "_hcrE"}else{suffix <- ""}
  
  m.mat.name <- paste0(str_replace_all(tmp_name, " ", "_"),suffix,'.csv')
  
  write_csv(m.mat.df, file=here('data','CSVs',m.mat.name))
}




# Perturbations -----------------------------------------------------------

# HCR A: Status Quo
PertA <-  c(`inshore small mesh effort`=0)

# HCR B: Remove small mesh fleet
PertB <- c(`inshore small mesh effort`=-1,`offshore small mesh effort`=-1)

# HCR C: Remove inshore small mesh fleet, allow effort redistribution to offshore small mesh fishery
PertC <- c(`inshore small mesh effort`=-1)

# HCR D: Increase the mesh size for fishery #1
PertD <- c(`inshore mesh size`=1,`offshore mesh size`=1)

# HCR E: remove fishery #1 from the inshore area
PertE <- c(`inshore small mesh effort`=-1)

# Save into a list
Pert.list <- list(PertA,PertB,PertC,PertD,PertE)
names(Pert.list) <- c("A:Status Quo","B:Rmv sm mesh fleet","C:Rmv inshore sm mesh fleet\nEffort redistribution",
                "D:Increase mesh size","E:Rmv inshore sm mesh fleet")


# OM 3:  There is a negative perturbation on “inshore recruits”
PertOM3 <- c(`inshore recruits`=-1)

# OM 4:  There a positive perturbation on “climate pressure"
PertOM4 <- c(`Climate pressure`=1)

# OM 5: There is a positive perturbation on "growth"
PertOM5 <- c(`Growth`=1)



# Summarize QNM complexity ------------------------------------------------
# how many variables in each QNM / HCR combo?
nvar_df <- as.data.frame(lapply(
  lapply(mlist_all, FUN=function(x){
  out <- lapply(x, FUN=function(y){
    return(length(unique(c(y[,1], y[,2]))))
  })
}), unlist
))

# link density?
ed_df <- as.data.frame(lapply(
  lapply(mlist_all, FUN=function(x){
  out <- lapply(x, FUN=function(y){
    g <- igraph::graph_from_data_frame(y)
    return(edge_density(g))
  })
}), unlist
))

complexity_df <- nvar_df %>% 
  mutate(MS=c('A','B','C','E')) %>%
  pivot_longer(all_of(colnames(nvar_df))) %>%
  rename(n_variables=value) %>%
  left_join(ed_df %>% 
              mutate(MS=c('A','B','C','E')) %>%
              pivot_longer(all_of(colnames(ed_df))) %>%
              rename(edge_density=value),
            by=c("MS","name")) %>%
  rename(QNM=name) %>%
  mutate(QNM=str_replace(QNM,'QNM.','')) %>%
  mutate(QNM=ifelse(QNM=="O","0.Baseline",QNM))

write_csv(complexity_df, here('data','qnm_models','qnm_complexity_stats.csv'))


# HCRs by Operating Model -------------------------------------------------

## Compare outcomes from different HCRs for each operating model
hcr_run_times <- c()


# HCR A -------------------------------------------------------------------
# simulation prep
p_hcr <- PertA   # perturbation
mlist_hcr <- sapply(mlist_all,"[",1)  # we only need the first version of the OM for HCR A
hcr_out <- list()   # empty list for simulation output

# run simulation
sstart <- Sys.time()  # start timer
for(i in seq(1,length(mlist_hcr))){
  
  # load 'i' operating model
  tmp_om <- mlist_hcr[[i]]
  tmp_om_name <- names(mlist_hcr)[i]
  
  # adjust perturbation if needed
  if(tmp_om_name=="OM 3 Fewer Recruits"){p_hcr <- c(p_hcr,PertOM3)}
  if(tmp_om_name=="OM 4 Offshore Inaccessible"){p_hcr <- c(p_hcr,PertOM4)}
  if(tmp_om_name=="OM 5 Faster Growth"){p_hcr <- c(p_hcr,PertOM5)}
  
  # run simulation, save to output list
  hcr_out[[i]] <- system.simulate(n.sims=10000, # number of randomly generated matrices to keep
                            edges=tmp_om, # the qnm
                            sampler=community.sampler(tmp_om, required.groups = c(0)), # thefx to generate the matrices without constraints
                            validators=list(press.validate(
                              tmp_om, # the qnm
                              perturb = p_hcr, # the perturbation
                              monitor = NA))) # a list of expected outcomes that should happen in the simulation (I don't usually use this)
  names(hcr_out)[i] <- tmp_om_name
  
  # reset perturbation
  p_hcr <- PertA
}
ssend <- Sys.time()-sstart; ssend  # end timer

# save time info
hcr_run_times[length(hcr_run_times)+1] <- ssend
names(hcr_run_times)[length(hcr_run_times)] <- "HCR A"

# HCR A 
# 15.79955s



# save simulation outputs for future use
saveRDS(hcr_out, here('data','simulations','mse0-5_hcrA_10K_2023-10.rds'))

# save simulation output as an image
pert_list <- list(p_hcr,p_hcr,c(p_hcr,PertOM3),c(p_hcr,PertOM4),c(p_hcr,PertOM5))
compare.table <- compare.models.impact.df(sim.list=hcr_out, # **named** list of simulations to look at
                                          perturb.list=pert_list,
                                          monitor.list=list(NA,NA,NA,NA,NA), # list of variable outcomes monitored (I don't use this)
                                          strong_lower=0.8, weak_lower=0.6,
                                          epsilon=1.0E-5,
                                          plot=TRUE, main.axis="Y",
                                          variable.key=vkey,variable.order="specific_order")
## plot (this one should show all perturbations in a single plot, so no need to use plot_grid)
plot.out <- compare.table[[3]] +
  geom_vline(aes(xintercept=8.5)) +
  geom_vline(aes(xintercept=16.5)) + ggtitle("HCR A: Status Quo")
plot.out

png(here('results','mse0-5_hcrA_10K_2023-10.png'), res=300,height=1500,width=2500)
plot.out
dev.off()


# HCR B -------------------------------------------------------------------
# simulation prep
p_hcr <- PertB   # perturbation
mlist_hcr <- sapply(mlist_all,"[",2)  # we only need the first version of the OM for HCR B for **all but QNM 2 thorson effort**
names(mlist_hcr) <- names(mlist_all)
hcr_out <- list()   # empty list for simulation output

# run simulation
sstart <- Sys.time()  # start timer
for(i in seq(1,length(mlist_hcr))){
  
  # load 'i' operating model
  tmp_om <- mlist_hcr[[i]]
  tmp_om_name <- names(mlist_hcr)[i]
  
  # adjust perturbation if needed
  if(tmp_om_name=="OM 3 Fewer Recruits"){p_hcr <- c(p_hcr,PertOM3)}
  if(tmp_om_name=="OM 4 Offshore Inaccessible"){p_hcr <- c(p_hcr,PertOM4)}
  if(tmp_om_name=="OM 5 Faster Growth"){p_hcr <- c(p_hcr,PertOM5)}
  
  # run simulation, save to output list
  hcr_out[[i]] <- system.simulate(n.sims=10000, # number of randomly generated matrices to keep
                                  edges=tmp_om, # the qnm
                                  sampler=community.sampler(tmp_om, required.groups = c(0)), # thefx to generate the matrices without constraints
                                  validators=list(press.validate(
                                    tmp_om, # the qnm
                                    perturb = p_hcr, # the perturbation
                                    monitor = NA))) # a list of expected outcomes that should happen in the simulation (I don't usually use this)
  names(hcr_out)[i] <- tmp_om_name
  
  # reset perturbation
  p_hcr <- PertB
}
ssend <- Sys.time()-sstart; ssend  # end timer

# save time info
hcr_run_times[length(hcr_run_times)+1] <- ssend
names(hcr_run_times)[length(hcr_run_times)] <- "HCR B"

# save simulation outputs for future use
saveRDS(hcr_out, here('data','simulations','mse0-5_hcrB_10K_2023-10.rds'))

# save simulation output as an image
pert_list <- list(p_hcr,p_hcr,c(p_hcr,PertOM3),c(p_hcr,PertOM4),c(p_hcr,PertOM5))
compare.table <- compare.models.impact.df(sim.list=hcr_out, # **named** list of simulations to look at
                                          perturb.list=pert_list,
                                          monitor.list=list(NA,NA,NA,NA,NA), # list of variable outcomes monitored (I don't use this)
                                          strong_lower=0.8, weak_lower=0.6,
                                          epsilon=1.0E-5,
                                          plot=TRUE, main.axis="Y",
                                        variable.key=vkey,variable.order="specific_order")
## plot (this one should show all perturbations in a single plot, so no need to use plot_grid)
plot.out <- compare.table[[3]] +
  geom_vline(aes(xintercept=8.5)) +
  geom_vline(aes(xintercept=16.5)) + ggtitle("HCR B: Remove Fishery 1 (Small Mesh)")
plot.out

png(here('results','mse0-5_hcrB_10K_2023-10.png'), res=300,height=1500,width=2500)
plot.out
dev.off()


# HCR C -------------------------------------------------------------------

# simulation prep
p_hcr <- PertC   # perturbation
mlist_hcr <- sapply(mlist_all,"[",3)  # we  need the FIRST version of the OM for HCR C
hcr_out <- list()   # empty list for simulation output

# run simulation
sstart <- Sys.time()  # start timer
for(i in seq(1,length(mlist_hcr))){
  
  # load 'i' operating model
  tmp_om <- mlist_hcr[[i]]
  tmp_om_name <- names(mlist_hcr)[i]
  
  # adjust perturbation if needed
  if(tmp_om_name=="OM 3 Fewer Recruits"){p_hcr <- c(p_hcr,PertOM3)}
  if(tmp_om_name=="OM 4 Offshore Inaccessible"){p_hcr <- c(p_hcr,PertOM4)}
  if(tmp_om_name=="OM 5 Faster Growth"){p_hcr <- c(p_hcr,PertOM5)}
  
  # run simulation, save to output list
  hcr_out[[i]] <- system.simulate(n.sims=10000, # number of randomly generated matrices to keep
                                  edges=tmp_om, # the qnm
                                  sampler=community.sampler(tmp_om, required.groups = c(0)), # thefx to generate the matrices without constraints
                                  validators=list(press.validate(
                                    tmp_om, # the qnm
                                    perturb = p_hcr, # the perturbation
                                    monitor = NA))) # a list of expected outcomes that should happen in the simulation (I don't usually use this)
  names(hcr_out)[i] <- tmp_om_name
  
  # reset perturbation
  p_hcr <- PertC
}
ssend <- Sys.time()-sstart; ssend  # end timer

# save time info
hcr_run_times[length(hcr_run_times)+1] <- ssend
names(hcr_run_times)[length(hcr_run_times)] <- "HCR C"

# save simulation outputs for future use
saveRDS(hcr_out, here('data','simulations','mse0-5_hcrC_10K_2023-10.rds'))

# save simulation output as an image
pert_list <- list(p_hcr,p_hcr,c(p_hcr,PertOM3),c(p_hcr,PertOM4),c(p_hcr,PertOM5))
compare.table <- compare.models.impact.df(sim.list=hcr_out, # **named** list of simulations to look at
                                          perturb.list=pert_list,
                                          monitor.list=list(NA,NA,NA,NA,NA), # list of variable outcomes monitored (I don't use this)
                                          strong_lower=0.8, weak_lower=0.6,
                                          epsilon=1.0E-5,
                                          plot=TRUE, main.axis="Y",
                                          variable.key=vkey,variable.order="specific_order")
## plot (this one should show all perturbations in a single plot, so no need to use plot_grid)
plot.out <- compare.table[[3]] +
  geom_vline(aes(xintercept=8.5)) +
  geom_vline(aes(xintercept=16.5)) + ggtitle("HCR C: Remove inshore small mesh, allow effort redistribution")
plot.out

png(here('results','mse0-5_hcrC_10K_2023-10.png'), res=300,height=1500,width=2500)
plot.out
dev.off()


# HCR D -------------------------------------------------------------------

# simulation prep
p_hcr <- PertD   # perturbation
mlist_hcr <- sapply(mlist_all,"[",1)  # we  need the FIRST version of the OM for HCR D
hcr_out <- list()   # empty list for simulation output

# run simulation
sstart <- Sys.time()  # start timer
for(i in seq(1,length(mlist_hcr))){
  
  # load 'i' operating model
  tmp_om <- mlist_hcr[[i]]
  tmp_om_name <- names(mlist_hcr)[i]
  
  # adjust perturbation if needed
  if(tmp_om_name=="OM 3 Fewer Recruits"){p_hcr <- c(p_hcr,PertOM3)}
  if(tmp_om_name=="OM 4 Offshore Inaccessible"){p_hcr <- c(p_hcr,PertOM4)}
  if(tmp_om_name=="OM 5 Faster Growth"){p_hcr <- c(p_hcr,PertOM5)}
  
  # run simulation, save to output list
  hcr_out[[i]] <- system.simulate(n.sims=10000, # number of randomly generated matrices to keep
                                  edges=tmp_om, # the qnm
                                  sampler=community.sampler(tmp_om, required.groups = c(0)), # thefx to generate the matrices without constraints
                                  validators=list(press.validate(
                                    tmp_om, # the qnm
                                    perturb = p_hcr, # the perturbation
                                    monitor = NA))) # a list of expected outcomes that should happen in the simulation (I don't usually use this)
  names(hcr_out)[i] <- tmp_om_name
  
  # reset perturbation
  p_hcr <- PertD
}
ssend <- Sys.time()-sstart; ssend  # end timer

# save time info
hcr_run_times[length(hcr_run_times)+1] <- ssend
names(hcr_run_times)[length(hcr_run_times)] <- "HCR D"

# save simulation outputs for future use
saveRDS(hcr_out, here('data','simulations','mse0-5_hcrD_10K_2023-10.rds'))

# save simulation output as an image
pert_list <- list(p_hcr,p_hcr,c(p_hcr,PertOM3),c(p_hcr,PertOM4),c(p_hcr,PertOM5))
compare.table <- compare.models.impact.df(sim.list=hcr_out, # **named** list of simulations to look at
                                          perturb.list=pert_list,
                                          monitor.list=list(NA,NA,NA,NA,NA), # list of variable outcomes monitored (I don't use this)
                                          strong_lower=0.8, weak_lower=0.6,
                                          epsilon=1.0E-5,
                                          plot=TRUE, main.axis="Y",
                                          variable.key=vkey,variable.order="specific_order")
## plot (this one should show all perturbations in a single plot, so no need to use plot_grid)
plot.out <- compare.table[[3]] +
  geom_vline(aes(xintercept=8.5)) +
  geom_vline(aes(xintercept=16.5)) + ggtitle("HCR D: Increase Mesh Size")
plot.out

png(here('results','mse0-5_hcrD_10K_2023-10.png'), res=300,height=1500,width=2500)
plot.out
dev.off()



# HCR E -------------------------------------------------------------------

# simulation prep
p_hcr <- PertE   # perturbation
mlist_hcr <- sapply(mlist_all,"[",4)  # we  need the FIRST version of the OM for HCR D
hcr_out <- list()   # empty list for simulation output

# run simulation
sstart <- Sys.time()  # start timer
for(i in seq(1,length(mlist_hcr))){
  
  # load 'i' operating model
  tmp_om <- mlist_hcr[[i]]
  tmp_om_name <- names(mlist_hcr)[i]
  
  # adjust perturbation if needed
  if(tmp_om_name=="OM 3 Fewer Recruits"){p_hcr <- c(p_hcr,PertOM3)}
  if(tmp_om_name=="OM 4 Offshore Inaccessible"){p_hcr <- c(p_hcr,PertOM4)}
  if(tmp_om_name=="OM 5 Faster Growth"){p_hcr <- c(p_hcr,PertOM5)}
  
  # run simulation, save to output list
  hcr_out[[i]] <- system.simulate(n.sims=10000, # number of randomly generated matrices to keep
                                  edges=tmp_om, # the qnm
                                  sampler=community.sampler(tmp_om, required.groups = c(0)), # thefx to generate the matrices without constraints
                                  validators=list(press.validate(
                                    tmp_om, # the qnm
                                    perturb = p_hcr, # the perturbation
                                    monitor = NA))) # a list of expected outcomes that should happen in the simulation (I don't usually use this)
  names(hcr_out)[i] <- tmp_om_name
  
  # reset perturbation
  p_hcr <- PertE
}
ssend <- Sys.time()-sstart; ssend  # end timer

# save time info
hcr_run_times[length(hcr_run_times)+1] <- ssend
names(hcr_run_times)[length(hcr_run_times)] <- "HCR E"

# save simulation outputs for future use
saveRDS(hcr_out, here('data','simulations','mse0-5_hcrE_10K_2023-10.rds'))

# save simulation output as an image
pert_list <- list(p_hcr,p_hcr,c(p_hcr,PertOM3),c(p_hcr,PertOM4),c(p_hcr,PertOM5))
compare.table <- compare.models.impact.df(sim.list=hcr_out, # **named** list of simulations to look at
                                          perturb.list=pert_list,
                                          monitor.list=list(NA,NA,NA,NA,NA), # list of variable outcomes monitored (I don't use this)
                                          strong_lower=0.8, weak_lower=0.6,
                                          epsilon=1.0E-5,
                                          plot=TRUE, main.axis="Y",
                                          variable.key=vkey,variable.order="specific_order")
## plot (this one should show all perturbations in a single plot, so no need to use plot_grid)
plot.out <- compare.table[[3]] +
  geom_vline(aes(xintercept=8.5)) +
  geom_vline(aes(xintercept=16.5)) + ggtitle("HCR E: Remove Inshore Small Mesh Fishery 1")
plot.out

png(here('results','mse0-5_hcrE_10K_2023-10.png'), res=300,height=1500,width=2500)
plot.out
dev.off()





# HCR Outcomes Across Operating Models ------------------------------------

# OM 2 --------------------------------------------------------------------

# do you want to load new models?
# m1 <- prepare.dia(dia_name='MSE-QNM_2023-Oct_2-thorsonF',dia_path=here('data'))[[1]]  # no constraints
# mb <- prepare.dia(dia_name='MSE-QNM_2023-Oct_2B-rm-small-mesh',dia_path=here('data'))[[1]]   # no constraints
# mc <- prepare.dia(dia_name='MSE-QNM_2023-Sep_2C-rm-small-mesh-redistribute-small',dia_path=here('data'))[[1]]  # no constraints

m1 <- mlist2[[1]]
mb <- mlist2[[2]]
mc <- mlist2[[3]]
me <- mlist2[[4]]

# simulation prep
om_out <- list()   # empty list for simulation output
pert_list <- Pert.list
om_run_times <- list()

# run simulation
for(i in seq(1,length(pert_list))){
  
  # load 'i' operating model
  tmp_p <- pert_list[[i]]
  tmp_p_name <- names(pert_list)[i]
  
  # run simulation for HCR C
  if(grepl("B:",tmp_p_name)){
    sstart <- Sys.time()  # start timer
    om_out[[i]] <- system.simulate(n.sims=10000, # number of randomly generated matrices to keep
                                   edges=mb, # the qnm
                                   sampler=community.sampler(mb, required.groups = c(0)), # thefx to generate the matrices without constraints
                                   validators=list(press.validate(
                                     mb, # the qnm
                                     perturb = tmp_p, # the perturbation
                                     monitor = NA))) # a list of expected outcomes that should happen in the simulation (I don't usually use this)
    names(om_out)[i] <- tmp_p_name
    
  } else if(grepl("C:",tmp_p_name)){
    sstart <- Sys.time()  # start timer
    om_out[[i]] <- system.simulate(n.sims=10000, # number of randomly generated matrices to keep
                                   edges=mc, # the qnm
                                   sampler=community.sampler(mc, required.groups = c(0)), # thefx to generate the matrices without constraints
                                   validators=list(press.validate(
                                     mc, # the qnm
                                     perturb = tmp_p, # the perturbation
                                     monitor = NA))) # a list of expected outcomes that should happen in the simulation (I don't usually use this)
    names(om_out)[i] <- tmp_p_name
    
  } else if(grepl("E:",tmp_p_name)){
    sstart <- Sys.time()  # start timer
    om_out[[i]] <- system.simulate(n.sims=10000, # number of randomly generated matrices to keep
                                   edges=me, # the qnm
                                   sampler=community.sampler(me, required.groups = c(0)), # thefx to generate the matrices without constraints
                                   validators=list(press.validate(
                                     me, # the qnm
                                     perturb = tmp_p, # the perturbation
                                     monitor = NA))) # a list of expected outcomes that should happen in the simulation (I don't usually use this)
    names(om_out)[i] <- tmp_p_name
    } else{
    # run simulation for HCR A-B, D-E
    sstart <- Sys.time()  # start timer
    om_out[[i]] <- system.simulate(n.sims=10000, # number of randomly generated matrices to keep
                                   edges=m1, # the qnm
                                   sampler=community.sampler(m1, required.groups = c(0)), # thefx to generate the matrices without constraints
                                   validators=list(press.validate(
                                     m1, # the qnm
                                     perturb = tmp_p, # the perturbation
                                     monitor = NA))) # a list of expected outcomes that should happen in the simulation (I don't usually use this)
    names(om_out)[i] <- tmp_p_name
  }
  
  # save time info
  om_run_times[length(om_run_times)+1] <- Sys.time()-sstart    # end timer
  names(om_run_times)[length(om_run_times)]
  
}
om_run_times

# save simulation outputs for future use
saveRDS(om_out, here('data','simulations','mse2_hcrA-E_10K_2023-10-19.rds'))

# save simulation output as an image
# om_out <- readRDS(here('data','simulations','mse4_hcrA-E_10K_2023-10-04.rds'))
# pert_list <- lapply(Pert.list, FUN=function(x){return(c(x,PertOM4))})     # OM 4:  There a positive perturbation on “climate pressure"
compare.table <- compare.models.impact.df(sim.list=om_out, # **named** list of simulations to look at
                                          perturb.list=pert_list,
                                          monitor.list=list(NA,NA,NA,NA,NA), # list of variable outcomes monitored (I don't use this)
                                          strong_lower=0.8, weak_lower=0.6,
                                          epsilon=1.0E-5,
                                          plot=TRUE, main.axis="Y",
                                          variable.key=vkey,variable.order="specific_order")
## plot (this one should show all perturbations in a single plot, so no need to use plot_grid)
plot.out <- compare.table[[3]] +
  labs(subtitle="OM 2 Thorson Effort") +
  geom_vline(aes(xintercept=6.5)) +
  geom_vline(aes(xintercept=12.5))
plot.out

png(here('results','mse2_hcrA-E_10K_2024-01-14.png'), res=300,height=1500,width=2500)
print(plot.out)
dev.off()



# OM 4 --------------------------------------------------------------------

# do you want to load new models?
# m1 <- prepare.dia(dia_name='MSE-QNM_2023-Oct_004-2-movement-to-inaccessible',dia_path=here('data'))[[1]]  # no constraints
# mc <- prepare.dia(dia_name='MSE-QNM_2023-Oct_004-2C-rm-small-mesh-redistribute-small',dia_path=here('data'))[[1]]  # no constraints

m1 <- mlist4[[1]]
mc <- mlist4[[2]]

# simulation prep
om_out <- list()   # empty list for simulation output
pert_list <- lapply(Pert.list, FUN=function(x){return(c(x,PertOM4))})     # OM 4:  There a positive perturbation on “climate pressure"
om_run_times <- list()

# run simulation
for(i in seq(1,length(pert_list))){
  
  # load 'i' operating model
  tmp_p <- pert_list[[i]]
  tmp_p_name <- names(pert_list)[i]
  
  # run simulation for HCR C
  if(grepl("Effort redistribution",tmp_p_name)){
    sstart <- Sys.time()  # start timer
    om_out[[i]] <- system.simulate(n.sims=10000, # number of randomly generated matrices to keep
                                    edges=mc, # the qnm
                                    sampler=community.sampler(mc, required.groups = c(0)), # thefx to generate the matrices without constraints
                                    validators=list(press.validate(
                                      mc, # the qnm
                                      perturb = tmp_p, # the perturbation
                                      monitor = NA))) # a list of expected outcomes that should happen in the simulation (I don't usually use this)
    names(om_out)[i] <- tmp_p_name
    
  } else{
    # run simulation for HCR A-B, D-E
    sstart <- Sys.time()  # start timer
    om_out[[i]] <- system.simulate(n.sims=10000, # number of randomly generated matrices to keep
                                    edges=m1, # the qnm
                                    sampler=community.sampler(m1, required.groups = c(0)), # thefx to generate the matrices without constraints
                                    validators=list(press.validate(
                                      m1, # the qnm
                                      perturb = tmp_p, # the perturbation
                                      monitor = NA))) # a list of expected outcomes that should happen in the simulation (I don't usually use this)
    names(om_out)[i] <- tmp_p_name
  }
  
  # save time info
  om_run_times[length(om_run_times)+1] <- Sys.time()-sstart    # end timer
  names(om_run_times)[length(om_run_times)]
  
}
om_run_times # ~4s

# save simulation outputs for future use
saveRDS(om_out, here('data','simulations','mse4_hcrA-E_10K_2024-01-14.rds'))

# save simulation output as an image
# om_out <- readRDS(here('data','simulations','mse4_hcrA-E_10K_2023-10-04.rds'))
# pert_list <- lapply(Pert.list, FUN=function(x){return(c(x,PertOM4))})     # OM 4:  There a positive perturbation on “climate pressure"
compare.table <- compare.models.impact.df(sim.list=om_out, # **named** list of simulations to look at
                                          perturb.list=pert_list,
                                          monitor.list=list(NA,NA,NA,NA,NA), # list of variable outcomes monitored (I don't use this)
                                          strong_lower=0.8, weak_lower=0.6,
                                          epsilon=1.0E-5,
                                          plot=TRUE, main.axis="Y",
                                          variable.key=vkey,variable.order="specific_order")
## plot (this one should show all perturbations in a single plot, so no need to use plot_grid)
plot.out <- compare.table[[3]] +
  labs(subtitle="OM 4 Offshore inaccessible biomass") +
  geom_vline(aes(xintercept=8.5)) +
  geom_vline(aes(xintercept=14.5))
plot.out

png(here('results','mse4_hcrA-E_10K_2024-01-14.png'), res=300,height=1500,width=2500)
plot.out
dev.off()



# OM 5 --------------------------------------------------------------------
# 5. Climate increases growth rate 

# do you want to load new models?
# m1 <- prepare.dia(dia_name='MSE-QNM_2023-Oct_005-growth-perturb',dia_path=here('data'))[[1]]  # no constraints
# mc <- prepare.dia(dia_name='MSE-QNM_2023-Oct_005C-rm-small-mesh-redistribute-small',dia_path=here('data'))[[1]]  # no constraints

m1 <- mlist5[[1]]
mc <- mlist5[[2]]

# simulation prep
om_out <- list()   # empty list for simulation output
pert_list <- lapply(Pert.list, FUN=function(x){return(c(x,PertOM5))})     # OM 5: There is a positive perturbation on "growth"
om_run_times <- list()

# run simulation
for(i in seq(1,length(pert_list))){
  
  # load 'i' operating model
  tmp_p <- pert_list[[i]]
  tmp_p_name <- names(pert_list)[i]
  
  # run simulation for HCR C
  if(grepl("Effort redistribution",tmp_p_name)){
    sstart <- Sys.time()  # start timer
    om_out[[i]] <- system.simulate(n.sims=10000, # number of randomly generated matrices to keep
                                   edges=mc, # the qnm
                                   sampler=community.sampler(mc, required.groups = c(0)), # thefx to generate the matrices without constraints
                                   validators=list(press.validate(
                                     mc, # the qnm
                                     perturb = tmp_p, # the perturbation
                                     monitor = NA))) # a list of expected outcomes that should happen in the simulation (I don't usually use this)
    names(om_out)[i] <- tmp_p_name
    
  } else{
    # run simulation for HCR A-B, D-E
    sstart <- Sys.time()  # start timer
    om_out[[i]] <- system.simulate(n.sims=10000, # number of randomly generated matrices to keep
                                   edges=m1, # the qnm
                                   sampler=community.sampler(m1, required.groups = c(0)), # thefx to generate the matrices without constraints
                                   validators=list(press.validate(
                                     m1, # the qnm
                                     perturb = tmp_p, # the perturbation
                                     monitor = NA))) # a list of expected outcomes that should happen in the simulation (I don't usually use this)
    names(om_out)[i] <- tmp_p_name
  }
  
  # save time info
  om_run_times[length(om_run_times)+1] <- Sys.time()-sstart    # end timer
  names(om_run_times)[length(om_run_times)]
  
}
om_run_times

# save simulation outputs for future use
saveRDS(om_out, here('data','simulations','mse5_hcrA-E_10K_2023-10-04.rds'))

# save simulation output as an image
compare.table <- compare.models.impact.df(sim.list=om_out, # **named** list of simulations to look at
                                          perturb.list=pert_list,
                                          monitor.list=list(NA,NA,NA,NA,NA), # list of variable outcomes monitored (I don't use this)
                                          strong_lower=0.8, weak_lower=0.6,
                                          epsilon=1.0E-5,
                                          plot=TRUE, main.axis="Y",
                                          variable.key=vkey,variable.order="specific_order")
## plot (this one should show all perturbations in a single plot, so no need to use plot_grid)
plot.out <- compare.table[[3]] +
  labs(subtitle="OM 5 Climate increases growth") 
plot.out

png(here('results','mse5_hcrA-E_10K_2023-10-19.png'), res=300,height=1500,width=2500)
plot.out
dev.off()

















# Re-save by Perturbation

## after running (or re-running) certain OMs, 
## save the new versions into the simulation lists

# simulation output saved as lists from above
hcrA_out <- readRDS(here('data','simulations','mse0-5_hcrA_10K_2023-10-19.rds'))
hcrB_out <- readRDS(here('data','simulations','mse0-5_hcrB_10K_2023-10-19.rds'))
hcrC_out <- readRDS(here('data','simulations','mse0-5_hcrC_10K_2023-10-19.rds'))
hcrD_out <- readRDS(here('data','simulations','mse0-5_hcrD_10K_2023-10-19.rds'))
hcrE_out <- readRDS(here('data','simulations','mse0-5_hcrE_10K_2023-10-19.rds'))


# add in new OM 4
hcrA_out[[4]] <- om_out[[1]]
hcrB_out[[4]] <- om_out[[2]]
hcrC_out[[4]] <- om_out[[3]]
hcrD_out[[4]] <- om_out[[4]]
hcrE_out[[4]] <- om_out[[5]]

# SAVE AGAIN
saveRDS(hcrA_out, here('data','simulations','mse0-5_hcrA_10K_2023-10-19.rds'))
saveRDS(hcrB_out, here('data','simulations','mse0-5_hcrB_10K_2023-10-19.rds'))
saveRDS(hcrC_out, here('data','simulations','mse0-5_hcrC_10K_2023-10-19.rds'))
saveRDS(hcrD_out, here('data','simulations','mse0-5_hcrD_10K_2023-10-19.rds'))
saveRDS(hcrE_out, here('data','simulations','mse0-5_hcrE_10K_2023-10-19.rds'))




# Re-save by Perturbation

## after running (or re-running) certain OMs, 
## save the new versions into the simulation lists

# simulation output saved as lists from above
hcrA_out <- readRDS(here('data','simulations','mse0-5_hcrA_10K_2023-10-19.rds'))
hcrB_out <- readRDS(here('data','simulations','mse0-5_hcrB_10K_2023-10-19.rds'))
hcrC_out <- readRDS(here('data','simulations','mse0-5_hcrC_10K_2023-10-19.rds'))
hcrD_out <- readRDS(here('data','simulations','mse0-5_hcrD_10K_2023-10-19.rds'))
hcrE_out <- readRDS(here('data','simulations','mse0-5_hcrE_10K_2023-10-19.rds'))

hcrAE_list <- list(hcrA_out,hcrB_out,hcrC_out,hcrD_out,hcrE_out)
names(hcrAE_list) <- c("A","B","C","D","E")


# re-load new OM 2
om_out <- readRDS(here('data','simulations','mse2_hcrA-E_10K_2024-01-14.rds'))
om <- 2

length(hcrAE_list) == length(om_out) # TRUE

# save new OM 2 into HCR-based list objects
for(i in seq(1:length(hcrAE_list))){
  hcrAE_list[[i]][[om]] <- om_out[[i]]
  length(hcrAE_list[[i]]) == 5
  saveRDS(hcrA_out, here('data','simulations',paste0('mse0-5_hcr',names(hcrAE_list)[i],'_10K_2024-01-14.rds')))
}


