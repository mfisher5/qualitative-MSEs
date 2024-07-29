########### Produce tables for QNM results for paper ###########
#
# Intended to take the simulation output from "qnm_mse_master.R"
#    and put it into the format of the quantitative MSE output tables
#    for the paper.
# Written 2023-10-29 M Fisher
# Last Updated: 2023-10-29 -- mcf
#
################################################################


# Set up ------------------------------------------------------------------

## Functions
library(here)
library(tidyverse)
library(magrittr)
library(ggplot2)
library(QPress)

source(here('../R','compare.models.MSEtable.R'))



# Supporting Data ---------------------------------------------------------
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

# OM 3:  There is a negative perturbation on “inshore recruits”
PertOM3 <- c(`inshore recruits`=-1)
# OM 4:  There a positive perturbation on “climate pressure"
PertOM4 <- c(`Climate pressure`=1)
# OM 5: There is a positive perturbation on "growth"
PertOM5 <- c(`Growth`=1)


# Create a data frame to order the variables in the output tables.
vkey <- data.frame(variable=c("Total catch", "Fishery 1", "Fishery 2", "spawning biomass"),
                   specific.order=c(1,2,3,4))

# These match QNM / HCR codes to text descriptors, for table columns
table.key <- read_csv(here('data','mse_table_key.csv'))







# Read in Simulations -----------------------------------------------------
# simulation output should have been saved as lists from qnm_mse_master.R
hcrA_out <- readRDS(here('data','simulations','mse0-5_hcrA_10K_2023-10.rds'))
hcrB_out <- readRDS(here('data','simulations','mse0-5_hcrB_10K_2023-10.rds'))
hcrC_out <- readRDS(here('data','simulations','mse0-5_hcrC_10K_2023-10.rds'))
hcrD_out <- readRDS(here('data','simulations','mse0-5_hcrD_10K_2023-10.rds'))
hcrE_out <- readRDS(here('data','simulations','mse0-5_hcrE_10K_2023-10.rds'))

hcr_out_all <- list(hcrA_out,hcrB_out,hcrC_out,hcrD_out,hcrE_out)
names(hcr_out_all) <- c("A","B","C","D","E")



# Tables: by Operating Model -----------------------------------------------------

#### QNM 0 Status Quo ###
pert.0 <- list(PertB,PertC,PertD,PertE)
sims.0 <- sapply(hcr_out_all,"[",1)    # first simulation in every list
sims.0 <- sims.0[2:5]                # remove first sim (HCR A)
names(sims.0) <- c("B","C","D","E")

table.0 <- compare.models.MSEtable(sim.list = sims.0, perturb.list = pert.0,
                        monitor.list = list(NA,NA,NA,NA), no.response=0) %>%
  mutate(QNM=0) %>% rename("HCR"="model") %>%
  left_join(table.key)

table.0 %<>% dplyr::select(all_of(colnames(table.key)),variable, response,change,p_sim, node_to_perturb) %>%
  mutate(p_sim=round(p_sim,0))

write_csv(table.0, here('results','mse0_hcrB-D_MSEtable_allvar.csv'))

#### QNM 2 Thorson Effort ##
pert.2 <- list(PertA,PertB,PertC,PertD,PertE)
sims.2 <- sapply(hcr_out_all,"[",2)    # second simulation in every list
names(sims.2) <- c("A","B","C","D","E")

table.2 <- compare.models.MSEtable(sim.list = sims.2, perturb.list = pert.2,
                                   monitor.list = list(NA,NA,NA,NA,NA), no.response=0) %>%
  mutate(QNM=2) %>% rename("HCR"="model") %>%
  left_join(table.key)

table.2 %<>% dplyr::select(all_of(colnames(table.key)),variable, response,change,p_sim, node_to_perturb) %>%
  mutate(p_sim=round(p_sim,0))

write_csv(table.2, here('results','qnm2_hcrB-D_MSEtable_allvar.csv'))


## QNM 3 Less recruitment ##
pert.3 <- lapply(pert.2, FUN=function(x){return(c(x,PertOM3))})     

sims.3 <- sapply(hcr_out_all,"[",3)    # second simulation in every list
names(sims.3) <- c("A","B","C","D","E")

table.3 <- compare.models.MSEtable(sim.list = sims.3, perturb.list = pert.3,
                                   monitor.list = list(NA,NA,NA,NA,NA), no.response=0) %>%
  mutate(QNM=3) %>% rename("HCR"="model") %>%
  left_join(table.key)

table.3 %<>% dplyr::select(all_of(colnames(table.key)),variable, response,change,p_sim, node_to_perturb) %>%
  mutate(p_sim=round(p_sim,0))

write_csv(table.3, here('results','qnm3_hcrA-D_MSEtable_allvar.csv'))


## QNM 4 Offshore biomass inaccessible ##
pert.4 <- lapply(pert.2, FUN=function(x){return(c(x,PertOM4))})     

sims.4 <- sapply(hcr_out_all,"[",4)    # second simulation in every list
names(sims.4) <- c("A","B","C","D","E")

table.4 <- compare.models.MSEtable(sim.list = sims.4, perturb.list = pert.4,
                                   monitor.list = list(NA,NA,NA,NA,NA), no.response=0) %>%
  mutate(QNM=4) %>% rename("HCR"="model") %>%
  left_join(table.key)

table.4 %<>% dplyr::select(all_of(colnames(table.key)),variable, response,change,p_sim, node_to_perturb) %>%
  mutate(p_sim=round(p_sim,0))

write_csv(table.4, here('results','qnm4_hcrA-D_MSEtable_allvar.csv'))


#### QNM 5 Faster growth ##
pert.5 <- lapply(pert.2, FUN=function(x){return(c(x,PertOM5))})     

sims.5 <- sapply(hcr_out_all,"[",5)    # second simulation in every list
names(sims.5) <- c("A","B","C","D","E")

table.5 <- compare.models.MSEtable(sim.list = sims.5, perturb.list = pert.5,
                                   monitor.list = list(NA,NA,NA,NA,NA), no.response=0) %>%
  mutate(QNM=5) %>% rename("HCR"="model") %>%
  left_join(table.key)

table.5 %<>% dplyr::select(all_of(colnames(table.key)),variable, response,change,p_sim, node_to_perturb) %>%
  mutate(p_sim=round(p_sim,0))

write_csv(table.5, here('results','qnm5_hcrA-D_MSEtable_allvar.csv'))



#### save summary tables ##
bind_rows(table.0,table.2,table.3,table.4,table.5) %>%
  filter(variable %in% vkey$variable) %>%
  ## make each variable in the variable key its own column
  dplyr::select(Code,`Operating model`,`Management strategy`,variable,response) %>%
  pivot_wider(names_from="variable",values_from="response") %>%
  ## reorder to match vkey specific order
  dplyr::select(Code,`Operating model`,`Management strategy`,all_of(vkey$variable)) %>%
  write_csv(here('results','TableY_qnm0-5_hcra-e_qualitative_2023-10.csv'))

bind_rows(table.0,table.2,table.3,table.4,table.5) %>% 
  filter(variable %in% vkey$variable) %>%
  ## make each variable in the variable key its own column
  mutate(p_sim=ifelse(change=="neg",p_sim*-1,p_sim)) %>%
  dplyr::select(Code,`Operating model`,`Management strategy`,variable,p_sim) %>%
  pivot_wider(names_from="variable",values_from="p_sim") %>%
  ## reorder to match vkey specific order
  dplyr::select(Code,`Operating model`,`Management strategy`,all_of(vkey$variable)) %>%
  write_csv(here('results','TableX_qnm0-5_hcra-e_pSims_2023-10.csv'))


# Tables: by HCR -----------------------------------------------















