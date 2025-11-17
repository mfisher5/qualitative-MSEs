########### Create Figure 6 ###########
#
# Fig 6 summarizes false positive / false negatives  
#    from QNM output 
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

speciesALL_compare <- read_csv(here('data','results',paste0('mseVqnm_prob-psim_species1-2_allHCRallOM_',sim.date,'.csv')))

## fix variable names for legend
speciesALL_compare %<>% 
  filter(code != "2-A") %>%     # QNM 2 HCR A doesn't have a perturbation
  mutate(variable_out=fct_recode(variable_out,"Fishery 1 Catch"="fishery_1_catch",
                                 "Fishery 2 Catch"="fishery_2_catch",
                                 "Total Catch"="total_catch",
                                 "Spawning Biomass"="spawning_biomass"))


# Identify false/true positives/negatives ---------------------------------------------------------

## count the number of times there is a mismatch for plot 1, create dataframe for plotting
triage_df <- speciesALL_compare  %>%
  dplyr::select(-model.x) %>%
  rename(qnm_psims=psims, mse_prob=prob, species=model.y) %>%
  mutate(species=case_when(
    species=="MSE-Species1" ~ "(sp.1) med longevity", 
    species=="MSE-Species2" ~ "(sp.2) short longevity")) %>%
  mutate(qnm_equiv=ifelse(abs(qnm_psims) < 60, 1, 0)) %>%
  mutate(triage=ifelse(qnm_psims < 0 & mse_prob < 0, 'true negative',
                       ifelse(qnm_psims > 0 & mse_prob > 0, 'true positive',
                              ifelse(qnm_psims > 0 & mse_prob < 0, 'false positive',
                                     ifelse(qnm_psims < 0 & mse_prob > 0, 'false negative', NA))))) %>%
  mutate(triage_noEquiv=ifelse(qnm_equiv==0, triage, NA))



# Table 4 -----------------------------------------------------------------

triage_df %>% filter(triage=='false negative') %>%
  group_by(management_strategy, variable_out,species) %>% summarise(n=n())

# by ms
triage_df %>% filter(triage_noEquiv=='false positive') %>%
  group_by(management_strategy, variable_out,species) %>% summarise(n=n()) %>%
  pivot_wider(names_from=variable_out,values_from=n) %>%
  mutate(mismatch='false positive, no equiv') %>%
  arrange(mismatch,species,management_strategy) %>%
  bind_rows(
    triage_df %>% filter(triage=='false positive') %>%
      group_by(management_strategy, variable_out,species) %>% summarise(n=n()) %>%
      pivot_wider(names_from=variable_out,values_from=n) %>%
      mutate(mismatch='false positive') %>%
      arrange(mismatch,species,management_strategy)
  ) %>%
  bind_rows(
    triage_df %>% filter(triage_noEquiv=='false negative') %>%
      group_by(management_strategy, variable_out,species) %>% summarise(n=n()) %>%
      pivot_wider(names_from=variable_out,values_from=n) %>%
      mutate(mismatch='false negative, no equiv') %>%
      arrange(mismatch,species,management_strategy),
    triage_df %>% filter(triage=='false negative') %>%
      group_by(management_strategy, variable_out,species) %>% summarise(n=n()) %>%
      pivot_wider(names_from=variable_out,values_from=n) %>%
      mutate(mismatch='false negative') %>%
      arrange(mismatch,species,management_strategy)
  ) %>%
  write_csv(here('data','results','qnmVmse_falseposneg_bySpbyMS_20240923.csv'))
##

# by om
triage_df %>% filter(triage_noEquiv=='false positive') %>%
  group_by(operating_model, variable_out,species) %>% summarise(n=n()) %>%
  pivot_wider(names_from=variable_out,values_from=n) %>%
  mutate(mismatch='false positive, no equiv') %>%
  arrange(mismatch,species,operating_model) %>%
  bind_rows(
    triage_df %>% filter(triage=='false positive') %>%
      group_by(operating_model, variable_out,species) %>% summarise(n=n()) %>%
      pivot_wider(names_from=variable_out,values_from=n) %>%
      mutate(mismatch='false positive') %>%
      arrange(mismatch,species,operating_model)
  ) %>%
  bind_rows(
    triage_df %>% filter(triage_noEquiv=='false negative') %>%
      group_by(operating_model, variable_out,species) %>% summarise(n=n()) %>%
      pivot_wider(names_from=variable_out,values_from=n) %>%
      mutate(mismatch='false negative, no equiv') %>%
      arrange(mismatch,species,operating_model),
    triage_df %>% filter(triage=='false negative') %>%
      group_by(operating_model, variable_out,species) %>% summarise(n=n()) %>%
      pivot_wider(names_from=variable_out,values_from=n) %>%
      mutate(mismatch='false negative') %>%
      arrange(mismatch,species,operating_model)
  ) %>%
  write_csv(here('data','results','qnmVmse_falseposneg_bySpbyOM_20240923.csv'))


# Figure 6 -------------------------------------------------------------------

triage_df %<>%
  mutate(triage_equiv=ifelse(qnm_equiv==1, paste(triage,', equiv', sep=''), triage)) 

png(here('data','results','Fig7-equiv.png'), res=300, height=1300,width=2900)
triage_df %>%
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
  facet_grid(rows=vars(species), cols=vars(variable_out)) +
  scale_fill_manual(values=c('darkorange','dodgerblue4','dodgerblue4')) + theme_bw() +
  scale_alpha_manual(values=c(0.5,1,0.5)) +
  labs(x='Operating model scenario',y='Management strategy') +
  theme_bw() + theme(axis.text.x=element_text(angle=45,hjust=1, vjust=1.05),
                     legend.title=element_blank())
dev.off()


