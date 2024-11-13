########### Create Figure 7 ###########
#
# Fig 7 summarizes false positive / false negatives  
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
library(QPress)
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
    species=="MSE-Species1" ~ "species 1", 
    species=="MSE-Species2" ~ "species 2")) %>%
  mutate(qnm_equiv=ifelse(abs(qnm_psims) < 60, 1, 0)) %>%
  mutate(triage=ifelse(qnm_psims < 0 & mse_prob < 0, 'true negative',
                       ifelse(qnm_psims > 0 & mse_prob > 0, 'true positive',
                              ifelse(qnm_psims > 0 & mse_prob < 0, 'false positive',
                                     ifelse(qnm_psims < 0 & mse_prob > 0, 'false negative', NA))))) %>%
  mutate(triage_noEquiv=ifelse(qnm_equiv==1 & mse_prob < 0, 'false positive', 
                               ifelse(qnm_equiv==1 & mse_prob > 0, 'true positive',
                                      ifelse(qnm_equiv==0, triage, NA))))
## check against excel table
triage_df %>% filter(triage=='false negative') %>%
  group_by(management_strategy, variable_out,species) %>% summarise(n=n())

triage_df %>% filter(triage_noEquiv=='false positive') %>%
  group_by(management_strategy, variable_out,species) %>% summarise(n=n()) %>%
  pivot_wider(names_from=variable_out,values_from=n) %>%
  arrange(species,management_strategy)

triage_df %>% filter(triage=='false positive') %>%
  group_by(management_strategy, variable_out,species) %>% summarise(n=n()) %>%
  pivot_wider(names_from=variable_out,values_from=n) %>%
  arrange(species,management_strategy)
##



# plot -------------------------------------------------------------------
species_1_triage <- filter(triage_df, species=='species 1') %>%
  mutate(variable_out=fct_relevel(variable_out, c("Total Catch", "Fishery 1 Catch", "Fishery 2 Catch", "Spawning Biomass"))) %>%
  mutate(management_strategy=fct_rev(management_strategy))

p <- species_1_triage %>% 
  pivot_longer(cols=c(triage, triage_noEquiv), names_to='triage_type',values_to='triage_outcome') %>%
  mutate(triage_type=case_when(triage_type=='triage' ~ 'use equiv.',
                               triage_type=='triage_noEquiv' ~ 'retain all equiv.')) %>%
  filter(triage_outcome=='false positive') %>%
  group_by(management_strategy, variable_out, triage_type) %>% 
  summarise(count=n()) %>%
  ggplot(aes(x=variable_out,y=management_strategy)) +
  geom_tile(aes(fill=count)) +
  facet_wrap(~triage_type, strip.position='top', ncol=1) +
  scale_x_discrete(position = "top") +
  scale_fill_continuous(high = "#132B43", low = "#56B1F7") +
  labs(title='Species 1', y='Management Strategy', x='') +
  theme_bw() +
  theme(panel.grid.minor.x=element_line(color='black', linewidth=3),
        axis.text.x=element_text(face='bold', size=12, angle=90, hjust=0),
        legend.position='left',
        strip.placement='inside')

pcol.side <- species_1_triage %>% 
  pivot_longer(cols=c(triage, triage_noEquiv), names_to='triage_type',values_to='triage_outcome') %>%
  mutate(triage_type=case_when(triage_type=='triage' ~ 'use equiv.',
                               triage_type=='triage_noEquiv' ~ 'retain all equiv.')) %>%
  filter(triage_outcome=='false positive') %>%
  group_by(management_strategy, variable_out, triage_type) %>% 
  summarise(count=n()) %>%
  ggplot(aes(y=management_strategy, x=count)) +
  geom_col(fill='gray60') + 
  facet_wrap(~triage_type, ncol=1) +
  xlim(c(0,10)) +
    theme_void() +
    theme(axis.title=element_blank(),axis.text.y=element_blank(),
          strip.text=element_text(color='white'),
          panel.grid.major.y=element_blank(),
          panel.grid.minor.y=element_blank())
pcol.side


empty <- ggplot()

grid.arrange(p + theme(plot.margin=margin(t=5,r=0,b=5,l=5)), 
             pcol.side + theme(plot.margin=margin(t=160,r=5,b=5,l=0)), ncol=2, nrow=1, widths=c(4,1))




species_2_triage <- filter(triage_df, species=='species 2') %>%
  mutate(variable_out=fct_relevel(variable_out, c("Total Catch", "Fishery 1 Catch", "Fishery 2 Catch", "Spawning Biomass"))) %>%
  mutate(management_strategy=fct_rev(management_strategy))

p2 <- species_2_triage %>% 
  pivot_longer(cols=c(triage, triage_noEquiv), names_to='triage_type',values_to='triage_outcome') %>%
  mutate(triage_type=case_when(triage_type=='triage' ~ 'use equiv.',
                               triage_type=='triage_noEquiv' ~ 'retain all equiv.')) %>%
  filter(triage_outcome=='false positive') %>%
  group_by(management_strategy, variable_out, triage_type) %>% 
  summarise(count=n()) %>%
  ggplot(aes(x=variable_out,y=management_strategy)) +
  geom_tile(aes(fill=count)) +
  facet_wrap(~triage_type, strip.position='top', ncol=1) +
  scale_x_discrete(position = "top") +
  scale_fill_continuous(high = "#132B43", low = "#56B1F7") +
  labs(title='Species 2', y='Management Strategy', x='') +
  theme_bw() +
  theme(panel.grid.minor.x=element_line(color='black', linewidth=3),
        axis.text.x=element_text(face='bold', size=12, angle=90, hjust=0),
        legend.position='none',
        strip.placement='inside',
        axis.text.y=element_blank(),
        axis.title.y=element_blank())

pcol2.side <- species_2_triage %>% 
  pivot_longer(cols=c(triage, triage_noEquiv), names_to='triage_type',values_to='triage_outcome') %>%
  mutate(triage_type=case_when(triage_type=='triage' ~ 'use equiv.',
                               triage_type=='triage_noEquiv' ~ 'retain all equiv.')) %>%
  filter(triage_outcome=='false positive') %>%
  group_by(management_strategy, triage_type) %>% 
  summarise(count=n()) %>%
  ggplot(aes(y=management_strategy, x=count)) +
  geom_col(fill='gray60') + 
  facet_wrap(~triage_type, ncol=1) +
  xlim(c(0,10)) +
  theme_void() +
  theme(axis.title=element_blank(),axis.text.y=element_blank(),
        strip.text=element_text(color='white'),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.y=element_blank())


grid.arrange(p + theme(plot.margin=margin(t=5,r=0,b=5,l=5)), 
             pcol.side + theme(plot.margin=margin(t=160,r=5,b=5,l=0)),
             p2 + theme(plot.margin=margin(t=5,r=0,b=5,l=5)), 
             pcol2.side + theme(plot.margin=margin(t=160,r=5,b=5,l=0)),
             ncol=4, nrow=1, widths=c(4,1,1.8,1))

