###################### Create Figs 6,7 ######################
#
# Create Fig 6 and Fig 7 in the main text for the 
#    re-parameterizations asked for during the ICES JMS
#    review. 
#    Fig 6 summarizes false positive / false negatives  
#    from QNM output 
#    Fig 7 
#    Data required for this script are created in "03_qnm_mse_compare.R"
#
# Written 2024-11-13 M Fisher
# Last Updated: 
#
################################################################


# Set up ------------------------------------------------------------------

sim.date <- '20240923'

## Functions
library(here)
library(tidyverse)
library(magrittr)
library(ggplot2)
library(janitor)
library(gridExtra)


# Data --------------------------------------------------------------------

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


# Identify false/true positives/negatives ---------------------------------------------------------


## count the number of times there is a mismatch for plot 1, create dataframe for plotting
triage_df <- speciesALL_compare  %>%
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
  write_csv(here('data','results','BHrecruit','qnmVmseSENS_falseposneg_bySpbyOM_20240923.csv')) %>%
  write_csv(here('data','results','sp2_alt', 'qnmVmseSENS_falseposneg_bySpbyOM_20240923.csv'))


# Figure 6 -------------------------------------------------------------------

triage_df %<>%
  mutate(triage_equiv=ifelse(qnm_equiv==1, paste(triage,', equiv', sep=''), triage)) 


### BEVERTON-HOLT RECRUIT MODEL ###
triage_to_plot <- triage_df %>% filter(mse_param=='BH') %>% ungroup() %>%
  ## rename oms!
  mutate(operating_model=case_when(
    operating_model=="2. Thorson effort" ~ "1. Thorson effort",
    operating_model=="3. Lower productivity" ~ "2. Lower productivity",
    operating_model=="4. Move \"far offshore\"" ~ "3. Move \"far offshore\"",
    operating_model=="5. Lower growth rate" ~ "4. Lower growth rate",
    .default=operating_model
  )) %>%
  mutate(triage_equiv=fct_relevel(triage_equiv, c('false negative, equiv',
                                                  'false positive', 'false positive, equiv'))) %>%
  mutate(variable_out=fct_relevel(variable_out, c("Total Catch", "Fishery 1 Catch", "Fishery 2 Catch", "Spawning Biomass"))) %>%
  mutate(management_strategy=fct_rev(management_strategy))
  
  
triage_mse_equiv <- triage_to_plot %>% filter(mse_equiv==1 & mse_param=='BH')

png(here('data','results','BHrecruit','Fig6-equiv-BHrecruit.png'), res=300, height=1300,width=2900)
triage_to_plot  %>%
  filter(triage %in% c('false negative','false positive')) %>%
  ggplot(aes(x=operating_model,y=management_strategy, fill=triage_equiv, alpha=triage_equiv)) +
  geom_tile() +
  geom_point(aes(x=c(1),y=c(5)),pch=4, col='gray50', size=3, show.legend=FALSE) +
  geom_point(aes(x=c(2),y=c(5)),pch=4, col='gray50', size=3, show.legend=FALSE) +
  geom_point(data=triage_mse_equiv,aes(x=operating_model,y=management_strategy),pch=8,col='gray50', size=1, inherit.aes=FALSE) +
  facet_grid(rows=vars(species), cols=vars(variable_out)) +
  scale_fill_manual(values=c('darkorange','dodgerblue4','dodgerblue4')) + theme_bw() +
  scale_alpha_manual(values=c(0.5,1,0.5)) +
  labs(x='Operating model scenario',y='Management strategy') +
  theme_bw() + theme(axis.text.x=element_text(angle=45,hjust=1, vjust=1.05),
                     legend.title=element_blank())
dev.off()




triage_mse_equiv <- triage_df %>% filter(mse_equiv==1 & mse_param=='sp2alt')

### ALT SPECIES 2 PARAMETERIZATION ###
png(here('data','results','sp2_alt','Fig6-equiv-sp2alt.png'), res=300, height=1300,width=2900)
triage_df %>% filter(mse_param=='BH') %>% ungroup() %>%
  ## rename oms!
  mutate(operating_model=case_when(
    operating_model=="2. Thorson effort" ~ "1. Thorson effort",
    operating_model=="3. Lower productivity" ~ "2. Lower productivity",
    operating_model=="4. Move \"far offshore\"" ~ "3. Move \"far offshore\"",
    operating_model=="5. Lower growth rate" ~ "4. Lower growth rate",
    .default=operating_model
  )) %>%
  mutate(triage_equiv=fct_relevel(triage_equiv, c('false negative, equiv',
                                                  'false positive', 'false positive, equiv'))) %>%
  mutate(variable_out=fct_relevel(variable_out, c("Total Catch", "Fishery 1 Catch", "Fishery 2 Catch", "Spawning Biomass"))) %>%
  mutate(management_strategy=fct_rev(management_strategy)) %>%
  filter(triage %in% c('false negative','false positive')) %>%
  ggplot(aes(x=operating_model,y=management_strategy, fill=triage_equiv, alpha=triage_equiv)) +
  geom_tile() +
  geom_point(aes(x=c(1),y=c(5)),pch=4, col='gray50', size=3, show.legend=FALSE) +
  geom_point(aes(x=c(2),y=c(5)),pch=4, col='gray50', size=3, show.legend=FALSE) +
  geom_point(data=triage_mse_equiv,aes(x=operating_model,y=management_strategy),pch=8,col='gray50', size=1, inherit.aes=FALSE) +
  facet_grid(rows=vars(species), cols=vars(variable_out)) +
  scale_fill_manual(values=c('darkorange','dodgerblue4','dodgerblue4')) + theme_bw() +
  scale_alpha_manual(values=c(0.5,1,0.5)) +
  labs(x='Operating model scenario',y='Management strategy') +
  theme_bw() + theme(axis.text.x=element_text(angle=45,hjust=1, vjust=1.05),
                     legend.title=element_blank())
dev.off()



######################### compare all ######################
############################################################


# main text ---------------------------------------------------------------

## data

main_compare <- read_csv(here('data','results',paste0('mseVqnm_prob-psim_species1-2_allHCRallOM_',sim.date,'.csv')))

## fix variable names for legend
main_compare %<>% 
  filter(code != "2-A") %>%     # QNM 2 HCR A doesn't have a perturbation
  mutate(variable_out=fct_recode(variable_out,"Fishery 1 Catch"="fishery_1_catch",
                                 "Fishery 2 Catch"="fishery_2_catch",
                                 "Total Catch"="total_catch",
                                 "Spawning Biomass"="spawning_biomass"))


## count the number of times there is a mismatch for plot 1, create dataframe for plotting
main_triage_df <- main_compare  %>%
  dplyr::select(-model.x) %>%
  rename(qnm_psims=psims, mse_prob=prob, species=model.y) %>%
  mutate(species=case_when(
    species=="MSE-Species1" ~ "(sp.1) med longevity", 
    species=="MSE-Species2" ~ "(sp.2) short longevity")) %>%
  mutate(qnm_equiv=ifelse(abs(qnm_psims) < 60, 1, 0),
         mse_equiv=ifelse(abs(mse_prob) < 60, 1, 0)) %>%
  # mutate(triage=ifelse(qnm_psims < 0 & mse_prob < 40, 'true negative',
  #                      ifelse(qnm_psims > 0 & mse_prob > 60, 'true positive',
  mutate(triage=ifelse(qnm_psims < 0 & mse_prob < 0, 'true negative',
                       ifelse(qnm_psims > 0 & mse_prob > 0, 'true positive',
                              ifelse(qnm_psims > 0 & mse_prob < 0, 'false positive',
                                     ifelse(qnm_psims < 0 & mse_prob > 0, 'false negative', NA))))) %>%
  mutate(triage_noEquiv=ifelse(qnm_equiv==0 & mse_equiv==0, triage, NA))

##
main_triage_df %<>%
  mutate(triage_equiv=ifelse(qnm_equiv==1, paste(triage,', equiv', sep=''), triage)) 



# Data frames for Fig 6 ---------------------------------------------------
### MAIN TEXT ###
to_plot_main <- main_triage_df %>% ungroup() %>%
  ## rename oms!
  mutate(operating_model=case_when(
    operating_model=="2. Thorson effort" ~ "1. Thorson\neffort",
    operating_model=="3. Lower productivity" ~ "2. Lower\nproductivity",
    operating_model=="4. Move \"far offshore\"" ~ "3. Move\n\"far offshore\"",
    operating_model=="5. Lower growth rate" ~ "4. Lower\ngrowth rate",
    .default=operating_model
  )) %>%
  mutate(triage_equiv=fct_relevel(triage_equiv, c('false negative, equiv',
                                                  'false positive', 'false positive, equiv'))) %>%
  mutate(variable_out=fct_relevel(variable_out, c("Total Catch", "Fishery 1 Catch", "Fishery 2 Catch", "Spawning Biomass"))) %>%
  mutate(management_strategy=fct_rev(management_strategy))
to_plot_main %<>% mutate(mse_param='Main Text')
  
### BEVERTON-HOLT RECRUIT MODEL ###
to_plot_bh <- triage_df %>% filter(mse_param=='BH') %>% ungroup() %>%
  ## rename oms!
  mutate(operating_model=case_when(
    operating_model=="2. Thorson effort" ~ "1. Thorson\neffort",
    operating_model=="3. Lower productivity" ~ "2. Lower\nproductivity",
    operating_model=="4. Move \"far offshore\"" ~ "3. Move\n\"far offshore\"",
    operating_model=="5. Lower growth rate" ~ "4. Lower\ngrowth rate",
    .default=operating_model
  )) %>%
  mutate(triage_equiv=fct_relevel(triage_equiv, c('false negative, equiv',
                                                  'false positive', 'false positive, equiv'))) %>%
  mutate(variable_out=fct_relevel(variable_out, c("Total Catch", "Fishery 1 Catch", "Fishery 2 Catch", "Spawning Biomass"))) %>%
  mutate(management_strategy=fct_rev(management_strategy))

to_plot_bh %<>% mutate(mse_param='B-H recruit')

### ALT SPECIES 2 PARAMS ###
to_plot_sp2 <- triage_df %>% filter(mse_param=='sp2alt') %>% ungroup() %>%
  ## rename oms!
  mutate(operating_model=case_when(
    operating_model=="2. Thorson effort" ~ "1. Thorson\neffort",
    operating_model=="3. Lower productivity" ~ "2. Lower\nproductivity",
    operating_model=="4. Move \"far offshore\"" ~ "3. Move\n\"far offshore\"",
    operating_model=="5. Lower growth rate" ~ "4. Lower\ngrowth rate",
    .default=operating_model
  )) %>%
  mutate(triage_equiv=fct_relevel(triage_equiv, c('false negative, equiv',
                                                  'false positive', 'false positive, equiv'))) %>%
  mutate(variable_out=fct_relevel(variable_out, c("Total Catch", "Fishery 1 Catch", "Fishery 2 Catch", "Spawning Biomass"))) %>%
  mutate(management_strategy=fct_rev(management_strategy))

to_plot_sp2 %<>% mutate(mse_param='Alt. Sp 2')


all_to_plot <- bind_rows(
  to_plot_main,
  to_plot_bh,
  to_plot_sp2
)
all_to_plot %<>% mutate(mse_param=fct_relevel(mse_param,
                                              c('Main Text','B-H recruit','Alt. Sp 2')))


all_mse_equiv <- all_to_plot %>% filter(mse_equiv==1) %>%
  mutate(variable_out=case_when(
    variable_out=='Total Catch' ~ 'Total\nCatch',
    variable_out=='Fishery 1 Catch' ~ 'Fishery 1\nCatch',
    variable_out=='Fishery 2 Catch' ~ 'Fishery 2\nCatch',
    variable_out=='Spawning Biomass' ~ 'Spawning\nBiomass',
    .default=NA
  )) %>%
  mutate(variable_out=fct_relevel(variable_out, c("Total\nCatch", "Fishery 1\nCatch", "Fishery 2\nCatch", "Spawning\nBiomass")))
  

no_qnms <- data.frame(operating_model=c(rep("0. Baseline",4),rep("1. Thorson\neffort",4)),
                      management_strategy=rep("A. Status-quo",8),
                      variable_out=rep(c('Total\nCatch','Fishery 1\nCatch','Fishery 2\nCatch','Spawning\nBiomass'),2))
no_qnms <- bind_rows(
  no_qnms %<>% mutate(mse_param='Main Text'),
  no_qnms %<>% mutate(mse_param='B-H recruit'),
  no_qnms %<>% mutate(mse_param='Alt. Sp 2'),
)

# Figure 6 ----------------------------------------------------------------

png(here('doc','supplement','Fig6-equiv-sp1-altmse.png'), res=300, height=1600,width=2200)
all_to_plot  %>%
  filter(grepl('sp.1',species)) %>%
  mutate(variable_out=case_when(
    variable_out=='Total Catch' ~ 'Total\nCatch',
    variable_out=='Fishery 1 Catch' ~ 'Fishery 1\nCatch',
    variable_out=='Fishery 2 Catch' ~ 'Fishery 2\nCatch',
    variable_out=='Spawning Biomass' ~ 'Spawning\nBiomass',
    .default=NA
  )) %>%
  mutate(variable_out=fct_relevel(variable_out, c("Total\nCatch", "Fishery 1\nCatch", "Fishery 2\nCatch", "Spawning\nBiomass"))) %>%
  filter(triage %in% c('false negative','false positive')) %>%
  ggplot(aes(x=mse_param,y=management_strategy, fill=triage_equiv, alpha=triage_equiv)) +
  geom_tile() +
  geom_point(data=no_qnms, aes(x=mse_param,y=management_strategy),pch=4, col='gray50', size=3, inherit.aes=FALSE) +
  geom_point(data=all_mse_equiv,aes(x=mse_param,y=management_strategy),pch=8,col='gray50', size=1, inherit.aes=FALSE) +
  facet_grid(rows=vars(operating_model), cols=vars(variable_out)) +
  scale_fill_manual(values=c('darkorange','dodgerblue4','dodgerblue4')) + theme_bw() +
  scale_alpha_manual(values=c(0.5,1,0.5)) +
  labs(x='MSE parameterization',y='Management strategy') +
  theme_bw() + theme(axis.text.x=element_text(angle=45,hjust=1, vjust=1.05),
                     legend.title=element_blank(),
                     panel.spacing.y = unit(0,'lines'))
dev.off()


png(here('doc','supplement','Fig6-equiv-sp2-altmse.png'), res=300, height=1600,width=2200)
all_to_plot  %>%
  filter(grepl('sp.2',species)) %>%
  mutate(variable_out=case_when(
    variable_out=='Total Catch' ~ 'Total\nCatch',
    variable_out=='Fishery 1 Catch' ~ 'Fishery 1\nCatch',
    variable_out=='Fishery 2 Catch' ~ 'Fishery 2\nCatch',
    variable_out=='Spawning Biomass' ~ 'Spawning\nBiomass',
    .default=NA
  )) %>%
  mutate(variable_out=fct_relevel(variable_out, c("Total\nCatch", "Fishery 1\nCatch", "Fishery 2\nCatch", "Spawning\nBiomass"))) %>%
  filter(triage %in% c('false negative','false positive')) %>%
  ggplot(aes(x=mse_param,y=management_strategy, fill=triage_equiv, alpha=triage_equiv)) +
  geom_tile() +
  geom_point(data=no_qnms, aes(x=mse_param,y=management_strategy),pch=4, col='gray50', size=3, inherit.aes=FALSE) +
  geom_point(data=all_mse_equiv,aes(x=mse_param,y=management_strategy),pch=8,col='gray50', size=1, inherit.aes=FALSE) +
  facet_grid(rows=vars(operating_model), cols=vars(variable_out)) +
  scale_fill_manual(values=c('darkorange','dodgerblue4','dodgerblue4')) + theme_bw() +
  scale_alpha_manual(values=c(0.5,1,0.5)) +
  labs(x='MSE parameterization',y='Management strategy') +
  theme_bw() + theme(axis.text.x=element_text(angle=45,hjust=1, vjust=1.05),
                     legend.title=element_blank(),
                     panel.spacing.y = unit(0,'lines'))
dev.off()

png(here('doc','supplement','Fig6-equiv-altmse.png'), res=300, height=1600,width=3200)
plot_grid(
  all_to_plot  %>%
    filter(grepl('sp.1',species)) %>%
    mutate(variable_out=case_when(
      variable_out=='Total Catch' ~ 'Total\nCatch',
      variable_out=='Fishery 1 Catch' ~ 'Fishery 1\nCatch',
      variable_out=='Fishery 2 Catch' ~ 'Fishery 2\nCatch',
      variable_out=='Spawning Biomass' ~ 'Spawning\nBiomass',
      .default=NA
    )) %>%
    mutate(variable_out=fct_relevel(variable_out, c("Total\nCatch", "Fishery 1\nCatch", "Fishery 2\nCatch", "Spawning\nBiomass"))) %>%
    filter(triage %in% c('false negative','false positive')) %>%
    ggplot(aes(x=mse_param,y=management_strategy, fill=triage_equiv, alpha=triage_equiv)) +
    geom_tile() +
    geom_point(data=no_qnms, aes(x=mse_param,y=management_strategy),pch=4, col='gray50', size=3, inherit.aes=FALSE) +
    geom_point(data=all_mse_equiv,aes(x=mse_param,y=management_strategy),pch=8,col='gray50', size=1, inherit.aes=FALSE) +
    facet_grid(rows=vars(operating_model), cols=vars(variable_out)) +
    scale_fill_manual(values=c('darkorange','dodgerblue4','dodgerblue4')) + theme_bw() +
    scale_alpha_manual(values=c(0.5,1,0.5)) +
    labs(x='MSE parameterization',y='Management strategy') +
    theme_bw() + theme(axis.text.x=element_text(angle=45,hjust=1, vjust=1.05),
                       legend.position='none',
                       panel.spacing.y = unit(0,'lines'),
                       strip.text.y=element_blank()),
  all_to_plot  %>%
    filter(grepl('sp.2',species)) %>%
    mutate(variable_out=case_when(
      variable_out=='Total Catch' ~ 'Total\nCatch',
      variable_out=='Fishery 1 Catch' ~ 'Fishery 1\nCatch',
      variable_out=='Fishery 2 Catch' ~ 'Fishery 2\nCatch',
      variable_out=='Spawning Biomass' ~ 'Spawning\nBiomass',
      .default=NA
    )) %>%
    mutate(variable_out=fct_relevel(variable_out, c("Total\nCatch", "Fishery 1\nCatch", "Fishery 2\nCatch", "Spawning\nBiomass"))) %>%
    filter(triage %in% c('false negative','false positive')) %>%
    ggplot(aes(x=mse_param,y=management_strategy, fill=triage_equiv, alpha=triage_equiv)) +
    geom_tile() +
    geom_point(data=no_qnms, aes(x=mse_param,y=management_strategy),pch=4, col='gray50', size=3, inherit.aes=FALSE) +
    geom_point(data=all_mse_equiv,aes(x=mse_param,y=management_strategy),pch=8,col='gray50', size=1, inherit.aes=FALSE) +
    facet_grid(rows=vars(operating_model), cols=vars(variable_out)) +
    scale_fill_manual(values=c('darkorange','dodgerblue4','dodgerblue4')) + theme_bw() +
    scale_alpha_manual(values=c(0.5,1,0.5)) +
    labs(x='',y='Management strategy') +
    theme_bw() + theme(axis.text.x=element_text(angle=45,hjust=1, vjust=1.05),
                       axis.text.y=element_blank(),
                       axis.title.y=element_blank(),
                       legend.title=element_blank(),
                       panel.spacing.y = unit(0,'lines')),
  nrow=1,ncol=2,rel_widths=c(1,0.85),
  labels = "AUTO",hjust=c(-19,0.3)
  
)
dev.off()



tmp_no_qnms <- no_qnms %>% filter(mse_param!='Alt. Sp 2')
tmp_mse_equiv <- all_mse_equiv %>% filter(mse_param!='Alt. Sp 2')
png(here('doc','supplement','Fig6-equiv-bh.png'), res=300, height=1600,width=3000)
plot_grid(
  all_to_plot  %>%
    filter(mse_param!='Alt. Sp 2') %>%
    filter(grepl('sp.1',species)) %>%
    mutate(variable_out=case_when(
      variable_out=='Total Catch' ~ 'Total\nCatch',
      variable_out=='Fishery 1 Catch' ~ 'Fishery 1\nCatch',
      variable_out=='Fishery 2 Catch' ~ 'Fishery 2\nCatch',
      variable_out=='Spawning Biomass' ~ 'Spawning\nBiomass',
      .default=NA
    )) %>%
    mutate(variable_out=fct_relevel(variable_out, c("Total\nCatch", "Fishery 1\nCatch", "Fishery 2\nCatch", "Spawning\nBiomass"))) %>%
    filter(triage %in% c('false negative','false positive')) %>%
    ggplot(aes(x=mse_param,y=management_strategy, fill=triage_equiv, alpha=triage_equiv)) +
    geom_tile() +
    geom_point(data=tmp_no_qnms, aes(x=mse_param,y=management_strategy),pch=4, col='gray50', size=3, inherit.aes=FALSE) +
    geom_point(data=tmp_mse_equiv,aes(x=mse_param,y=management_strategy),pch=8,col='gray50', size=1, inherit.aes=FALSE) +
    facet_grid(rows=vars(operating_model), cols=vars(variable_out)) +
    scale_fill_manual(values=c('darkorange','dodgerblue4','dodgerblue4')) + theme_bw() +
    scale_alpha_manual(values=c(0.5,1,0.5)) +
    labs(x='MSE parameterization',y='Management strategy') +
    theme_bw() + theme(axis.text.x=element_text(angle=45,hjust=1, vjust=1.05),
                       legend.position='none',
                       panel.spacing.y = unit(0,'lines'),
                       strip.text.y=element_blank()),
  all_to_plot  %>%
    filter(mse_param!='Alt. Sp 2') %>%
    filter(grepl('sp.2',species)) %>%
    mutate(variable_out=case_when(
      variable_out=='Total Catch' ~ 'Total\nCatch',
      variable_out=='Fishery 1 Catch' ~ 'Fishery 1\nCatch',
      variable_out=='Fishery 2 Catch' ~ 'Fishery 2\nCatch',
      variable_out=='Spawning Biomass' ~ 'Spawning\nBiomass',
      .default=NA
    )) %>%
    mutate(variable_out=fct_relevel(variable_out, c("Total\nCatch", "Fishery 1\nCatch", "Fishery 2\nCatch", "Spawning\nBiomass"))) %>%
    filter(triage %in% c('false negative','false positive')) %>%
    ggplot(aes(x=mse_param,y=management_strategy, fill=triage_equiv, alpha=triage_equiv)) +
    geom_tile() +
    geom_point(data=tmp_no_qnms, aes(x=mse_param,y=management_strategy),pch=4, col='gray50', size=3, inherit.aes=FALSE) +
    geom_point(data=tmp_mse_equiv,aes(x=mse_param,y=management_strategy),pch=8,col='gray50', size=1, inherit.aes=FALSE) +
    facet_grid(rows=vars(operating_model), cols=vars(variable_out)) +
    scale_fill_manual(values=c('darkorange','dodgerblue4','dodgerblue4')) + theme_bw() +
    scale_alpha_manual(values=c(0.5,1,0.5)) +
    labs(x='',y='Management strategy') +
    theme_bw() + theme(axis.text.x=element_text(angle=45,hjust=1, vjust=1.05),
                       axis.text.y=element_blank(),
                       axis.title.y=element_blank(),
                       legend.title=element_blank(),
                       panel.spacing.y = unit(0,'lines')),
  nrow=1,ncol=2,rel_widths=c(1,0.85),
  labels = "AUTO",hjust=c(-19,0.3)
  
)
dev.off()

