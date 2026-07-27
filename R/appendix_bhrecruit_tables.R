###################### Create Tables ######################
#
# Create Tables in the main text / supplement for the 
#    re-parameterizations asked for during the ICES JMS
#    review - BH recruit model.
#    Data required for this script are created in "03_qnm_mse_compare.R"
#
# Written 2026-07-12 M Fisher
# Last Updated: 
#
################################################################


# Set up ------------------------------------------------------------------

## Functions
library(here)
library(tidyverse)
library(magrittr)
library(ggplot2)
library(janitor)
library(gridExtra)
library(PNWColors)
library(cowplot)

load_data <- function(sdate){
  speciesALL_compare <- read_csv(here('data','results',paste0('mseSENSVqnm_prob-psim_species1-2_allHCRallOM_',sim.date,'.csv')))
  
  ## fix variable names for legend
  speciesALL_compare %<>% 
    filter(code != "2-A") %>%     # QNM 2 HCR A doesn't have a perturbation
    mutate(variable_out=fct_recode(variable_out,"Fishery 1 Catch"="fishery_1_catch",
                                   "Fishery 2 Catch"="fishery_2_catch",
                                   "Total Catch"="total_catch",
                                   "Spawning Biomass"="spawning_biomass"))
  ## fix MS-C if needed
  speciesALL_compare %<>% mutate(management_strategy=ifelse(
    management_strategy=="C. Fishery 1 in \"offshore\" only",
    "C. Fishery 1 in \"offshore\" only, effort changes",
    management_strategy
  ))
  
  return(speciesALL_compare)
}



# Data --------------------------------------------------------------------
sim.date <- '20240923'
speciesALL_compare <- load_data()

## count the number of times there is a mismatch for plot 1, create dataframe for plotting
triage_df <- speciesALL_compare  %>%
  filter(mse_param=='BH') %>%
  dplyr::select(-model.x) %>%
  rename(qnm_psims=psims, mse_prob=prob, species=mse_species) %>%
  mutate(species=case_when(
    species=="Species1" ~ "(sp.1) med longevity", 
    species=="Species2" ~ "(sp.2) short longevity")) %>%
  mutate(qnm_equiv=ifelse(abs(qnm_psims) < 60, 1, 0),
         mse_equiv=ifelse(mse_prob > 40 & mse_prob < 60, 1, 0)) %>%
  # mutate(triage=ifelse(qnm_psims < 0 & mse_prob < 40, 'true negative',
  #                      ifelse(qnm_psims > 0 & mse_prob > 60, 'true positive',
  mutate(triage=ifelse(qnm_psims < 0 & mse_prob < 50, 'true negative',
                       ifelse(qnm_psims > 0 & mse_prob > 50, 'true positive',
                              ifelse(qnm_psims > 0 & mse_prob < 40, 'false positive',
                                     ifelse(qnm_psims < 0 & mse_prob > 60, 'false negative', NA))))) %>%
  mutate(triage_noEquiv=ifelse(qnm_equiv==0 & mse_equiv==0, triage, NA))



# Table 3 -------------------------------------------------

## identify match/mismatch
match_df <- speciesALL_compare  %>%
  dplyr::select(-model.x) %>%
  rename(qnm_psims=psims, mse_prob=prob, species=mse_species) %>%
  mutate(species=case_when(
    species=="Species1" ~ "(sp.1) med longevity", 
    species=="Species2" ~ "(sp.2) short longevity")) %>%
  mutate(qnm_equiv=ifelse(abs(qnm_psims) < 60, 1, 0),
         mse_equiv=ifelse(mse_prob > 40 & mse_prob < 60, 1, 0)) %>%
  mutate(match=ifelse(abs(qnm_psims)==50, 'uninf qnm',
                      ifelse((qnm_psims < 0 & mse_prob < 50) |
                               (qnm_psims > 0 & mse_prob > 50), 'match','mismatch')))

tbl3 <- match_df %>% group_by(mse_param,species,variable_out,qnm_equiv,match) %>%
  summarise(n=n()) %>% mutate(p=round(n/23,3)) %>% dplyr::select(-n) %>% 
  mutate(qnm_equiv=case_when(
    qnm_equiv==1 ~ '_qnm_equiv',
    .default=''
  )) %>% unite(match,qnm_equiv,sep='',col='match2') %>% ungroup() %>%
  pivot_wider(names_from='match2',values_from='p',values_fill=0) %>%
  rowwise() %>%
  mutate(total_match=sum(across(c(match,match_qnm_equiv))),
         total_mismatch=sum(across(c(mismatch,mismatch_qnm_equiv)))) %>%
  bind_rows(
    match_df %>% group_by(mse_param,species,variable_out,qnm_equiv,match) %>%
      summarise(n=n())%>% 
      mutate(qnm_equiv=case_when(
        qnm_equiv==1 ~ '_qnm_equiv',
        .default=''
      )) %>% unite(match,qnm_equiv,sep='',col='match2') %>% ungroup() %>%
      pivot_wider(names_from='match2',values_from='n',values_fill=0) %>%
      rowwise() %>%
      mutate(total_match=sum(across(c(match,match_qnm_equiv))),
             total_mismatch=sum(across(c(mismatch,mismatch_qnm_equiv))))
  )
tbl3_mse_equivs <- match_df %>% group_by(mse_param,species,variable_out,qnm_equiv,match) %>%
  summarise(n=sum(mse_equiv)) %>% filter(n>0)
View(tbl3_mse_equivs)
write_csv(tbl3,here('data','results','BHrecruit','qnmVmseSENS_tbl3_mismatches_20240923.csv'))

# Table 4 -----------------------------------------------------------------

triage_df %>% filter(triage=='false negative') %>%
  group_by(management_strategy, variable_out,species,mse_param) %>% summarise(n=n())

# by ms
triage_df %>% filter(triage_noEquiv=='false positive') %>%
  group_by(management_strategy, variable_out,species,mse_param) %>% summarise(n=n()) %>%
  pivot_wider(names_from=variable_out,values_from=n) %>%
  mutate(mismatch='false positive, no equiv') %>%
  arrange(mse_param,mismatch,species,management_strategy) %>%
  bind_rows(
    triage_df %>% filter(triage=='false positive') %>%
      group_by(mse_param,management_strategy, variable_out,species) %>% summarise(n=n()) %>%
      pivot_wider(names_from=variable_out,values_from=n) %>%
      mutate(mismatch='false positive') %>%
      arrange(mse_param,mismatch,species,management_strategy)
  ) %>%
  bind_rows(
    triage_df %>% filter(triage_noEquiv=='false negative') %>%
      group_by(mse_param,management_strategy, variable_out,species) %>% summarise(n=n()) %>%
      pivot_wider(names_from=variable_out,values_from=n) %>%
      mutate(mismatch='false negative, no equiv') %>%
      arrange(mse_param,mismatch,species,management_strategy),
    triage_df %>% filter(triage=='false negative') %>%
      group_by(mse_param,management_strategy, variable_out,species) %>% summarise(n=n()) %>%
      pivot_wider(names_from=variable_out,values_from=n) %>%
      mutate(mismatch='false negative') %>%
      arrange(mse_param,mismatch,species,management_strategy)
  ) %>%
  write_csv(here('data','results','BHrecruit','qnmVmseSENS_falseposneg_bySpbyMS_20240923.csv')) %>%
  write_csv(here('data','results','sp2_alt','qnmVmseSENS_falseposneg_bySpbyMS_20240923.csv'))
##

# by om
triage_df %>% filter(triage_noEquiv=='false positive') %>%
  group_by(mse_param, operating_model, variable_out,species) %>% summarise(n=n()) %>%
  pivot_wider(names_from=variable_out,values_from=n) %>%
  mutate(mismatch='false positive, no equiv') %>%
  arrange(mse_param,mismatch,species,operating_model) %>%
  bind_rows(
    triage_df %>% filter(triage=='false positive') %>%
      group_by(mse_param,operating_model, variable_out,species) %>% summarise(n=n()) %>%
      pivot_wider(names_from=variable_out,values_from=n) %>%
      mutate(mismatch='false positive') %>%
      arrange(mse_param,mismatch,species,operating_model)
  ) %>%
  bind_rows(
    triage_df %>% filter(triage_noEquiv=='false negative') %>%
      group_by(mse_param,operating_model, variable_out,species) %>% summarise(n=n()) %>%
      pivot_wider(names_from=variable_out,values_from=n) %>%
      mutate(mismatch='false negative, no equiv') %>%
      arrange(mse_param,mismatch,species,operating_model),
    triage_df %>% filter(triage=='false negative') %>%
      group_by(mse_param,operating_model, variable_out,species) %>% summarise(n=n()) %>%
      pivot_wider(names_from=variable_out,values_from=n) %>%
      mutate(mismatch='false negative') %>%
      arrange(mse_param,mismatch,species,operating_model)
  ) %>%
  write_csv(here('data','results','BHrecruit','qnmVmseSENS_falseposneg_bySpbyOM_20240923.csv'))




# Table S... --------------------------------------------------------------
########### how to deal with equivocal MSE output here? identify but ignore?? ############
# rm(list=ls())
sim.date <- '20240923'
#ricker (main text)
compare_main <- read_csv(here('data','results',
                              paste0('mseVqnm_prob-psim_species1-2_allHCRallOM_',sim.date,'.csv'))) %>%
  ## fix variable names for legend
  filter(code != "2-A") %>%     # QNM 2 HCR A doesn't have a perturbation
  mutate(variable_out=fct_recode(variable_out,"Fishery 1 Catch"="fishery_1_catch",
                                 "Fishery 2 Catch"="fishery_2_catch",
                                 "Total Catch"="total_catch",
                                 "Spawning Biomass"="spawning_biomass")) %>%
  ## fix MS-C if needed
  mutate(management_strategy=ifelse(
    management_strategy=="C. Fishery 1 in \"offshore\" only",
    "C. Fishery 1 in \"offshore\" only, effort changes",
    management_strategy
  )) %>%
  mutate(mse_param='Ricker')
triage_main <- compare_main  %>%
  dplyr::select(-model.x) %>%
  rename(qnm_psims=psims, mse_prob=prob) %>%
  mutate(species=case_when(
    grepl("Species1",model.y) ~ "(sp.1) med longevity", 
    grepl("Species2",model.y) ~ "(sp.2) short longevity")) %>%
  mutate(qnm_equiv=ifelse(abs(qnm_psims) < 60, 1, 0),
         mse_equiv=ifelse(mse_prob > 40 & mse_prob < 60, 1, 0)) %>%
  # mutate(triage=ifelse(qnm_psims < 0 & mse_prob < 40, 'true negative',
  #                      ifelse(qnm_psims > 0 & mse_prob > 60, 'true positive',
  mutate(triage=ifelse(qnm_psims < 0 & mse_prob < 50, 'true negative',
                       ifelse(qnm_psims > 0 & mse_prob > 50, 'true positive',
                              ifelse(qnm_psims > 0 & mse_prob < 40, 'false positive',
                                     ifelse(qnm_psims < 0 & mse_prob > 60, 'false negative', NA))))) %>%
  mutate(triage_noEquiv=ifelse(qnm_equiv==0 & mse_equiv==0, triage, NA))


## false positive rates by strategy / metric, include equivocal QNM output
tbl_fp <- triage_df %>% 
  filter(mse_param=='BH') %>%
  group_by(species,management_strategy, variable_out) %>%
  mutate(totn=n()) %>%
  ungroup() %>% group_by(species,management_strategy, variable_out, totn,triage) %>%
  summarise(n=n()) %>%
  mutate(p=n/totn)  %>% mutate(include_equiv='y') %>%
  bind_rows(
    
    ## false positive rates by strategy / metric, don't incude equivocal QNM output
    triage_df %>% group_by(species,management_strategy, variable_out) %>%
      mutate(totn=n()) %>%
      ungroup() %>% group_by(species,management_strategy, variable_out, totn,triage_noEquiv) %>%
      summarise(n=n()) %>%
      mutate(p=n/totn) %>%
      rename(triage=triage_noEquiv) %>% mutate(include_equiv='n')
  ) %>% mutate(mse_param='BH')

tbl_fp <- tbl_fp %>% bind_rows(
  triage_main %>% 
    group_by(species,management_strategy, variable_out) %>%
    mutate(totn=n()) %>%
    ungroup() %>% group_by(species,management_strategy, variable_out, totn,triage) %>%
    summarise(n=n()) %>%
    mutate(p=n/totn)  %>% mutate(include_equiv='y') %>% 
    bind_rows(
      
      ## false positive rates by strategy / metric, don't incude equivocal QNM output
      triage_main %>% group_by(species,management_strategy, variable_out) %>%
        mutate(totn=n()) %>%
        ungroup() %>% group_by(species,management_strategy, variable_out, totn,triage_noEquiv) %>%
        summarise(n=n()) %>%
        mutate(p=n/totn) %>%
        rename(triage=triage_noEquiv)  %>% mutate(include_equiv='n')
    ) %>% mutate(mse_param='Ricker')
)

tbl_fp %>% dplyr::select(-totn,-n) %>%
  
  mutate(variable_out=factor(variable_out,levels=c('Total Catch','Fishery 1 Catch','Fishery 2 Catch','Spawning Biomass','Overall'))) %>%
  arrange(mse_species,variable_out,desc(mse_param)) %>%
write_csv(,here('doc','supplement','qnmVmseBH_false_positive_rates_20240923.csv'))



# Table S... --------------------------------------------------------------

## data
rm(list=ls()) # clear
speciesALL_compare <- load_data() #bh
#ricker (main text)
compare_main <- read_csv(here('data','results',
                              paste0('mseVqnm_prob-psim_species1-2_allHCRallOM_',sim.date,'.csv'))) %>%
  ## fix variable names for legend
  filter(code != "2-A") %>%     # QNM 2 HCR A doesn't have a perturbation
  mutate(variable_out=fct_recode(variable_out,"Fishery 1 Catch"="fishery_1_catch",
                                 "Fishery 2 Catch"="fishery_2_catch",
                                 "Total Catch"="total_catch",
                                 "Spawning Biomass"="spawning_biomass")) %>%
  ## fix MS-C if needed
  mutate(management_strategy=ifelse(
    management_strategy=="C. Fishery 1 in \"offshore\" only",
    "C. Fishery 1 in \"offshore\" only, effort changes",
    management_strategy
  )) %>%
  mutate(mse_param='Ricker')


## how well do equivocal QNM outcomes match qMSE?

match_equiv <- speciesALL_compare  %>%
  filter(mse_param=='BH') %>%
  dplyr::select(-model.x) %>%
  rename(qnm_psims=psims, mse_prob=prob) %>%
  filter(abs(qnm_psims) < 60 & abs(qnm_psims) != 50) %>%
  bind_rows(
    compare_main %>% dplyr::select(-model.x) %>%
      rename(qnm_psims=psims, mse_prob=prob, mse_species=model.y) %>%
      filter(abs(qnm_psims) < 60& abs(qnm_psims) != 50)
  )

match_equiv <- match_equiv %>%
  mutate(mse_species=case_when(
    grepl("Species1",mse_species) ~ "Species 1", 
    grepl("Species2",mse_species) ~ "Species 2")) %>%
  mutate(mse_equiv=ifelse(mse_prob > 40 & mse_prob < 60, '_mse_equiv', '')) %>%
  mutate(match=ifelse(abs(qnm_psims)==50, 'uninf qnm',
                      ifelse((qnm_psims < 0 & mse_prob < 40) |
                               (qnm_psims > 0 & mse_prob > 60), 'match','mismatch'))) %>%
  unite(match,mse_equiv,col='match',sep='')

tbleq <- match_equiv %>% 
  group_by(mse_species,mse_param,variable_out) %>% mutate(totaln=n()) %>%
  ungroup() %>%
  group_by(mse_species,mse_param,variable_out,totaln,match) %>%
  summarise(n=n()) %>% mutate(p=round(n/totaln,3)) %>% dplyr::select(-n) %>%
  pivot_wider(names_from='match',values_from='p',values_fill=0) %>%
  rowwise() %>%
  mutate(total_mismatch=sum(across(c(mismatch,mismatch_mse_equiv))))

# check
sum(tbleq$totaln)==dim(match_equiv)[1]

# add overall
tbleq <- tbleq %>%
  bind_rows(
    match_equiv %>%
    group_by(mse_species,mse_param) %>% mutate(totaln=n()) %>%
      ungroup() %>%
      group_by(mse_species,mse_param,totaln,match) %>%
      summarise(n=n()) %>% mutate(p=round(n/totaln,3)) %>% dplyr::select(-n) %>%
      pivot_wider(names_from='match',values_from='p',values_fill=0) %>%
      rowwise() %>%
      mutate(total_mismatch=sum(across(c(mismatch,mismatch_mse_equiv)))) %>%
      mutate(variable_out='Overall')
  )


tbleq %>% 
  mutate(variable_out=factor(variable_out,levels=c('Total Catch','Fishery 1 Catch','Fishery 2 Catch','Spawning Biomass','Overall'))) %>%
  arrange(mse_species,variable_out,desc(mse_param)) %>%
write_csv(here('doc','supplement','qnmEquiv_V_mseBHvRicker_summary_20240923.csv'))

