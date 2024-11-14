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




# expanded heat map -------------------------------------------------------------------
png(here('data','results','Fig7-equiv.png'), res=300, height=1300,width=2600)
triage_df %>%
  mutate(triage_equiv=ifelse(qnm_equiv==1, paste(triage,', equiv', sep=''), triage)) %>%
  mutate(triage_equiv=fct_relevel(triage_equiv, c('false negative, equiv',
                                                  'false positive', 'false positive, equiv'))) %>%
  mutate(variable_out=fct_relevel(variable_out, c("Total Catch", "Fishery 1 Catch", "Fishery 2 Catch", "Spawning Biomass"))) %>%
  mutate(management_strategy=fct_rev(management_strategy)) %>%
  filter(triage %in% c('false negative','false positive')) %>%
  ggplot(aes(x=operating_model,y=management_strategy, fill=triage_equiv, alpha=triage_equiv)) +
  geom_tile() +
  facet_grid(rows=vars(species), cols=vars(variable_out)) +
  scale_fill_manual(values=c('darkorange','dodgerblue4','dodgerblue4')) + theme_bw() +
  scale_alpha_manual(values=c(0.5,1,0.5)) +
  labs(x='Operating model scenario',y='Management strategy') +
  theme_bw() + theme(axis.text.x=element_text(angle=45,hjust=1, vjust=1.05),
                     legend.title=element_blank())
dev.off()


# summary heat map --------------------------------------------------------

species_1_triage <- filter(triage_df, species=='(sp.1) med longevity') %>%
  mutate(variable_out=fct_relevel(variable_out, c("Total Catch", "Fishery 1 Catch", "Fishery 2 Catch", "Spawning Biomass"))) %>%
  mutate(management_strategy=fct_rev(management_strategy))

species_2_triage <- filter(triage_df, species=='(sp.2) short longevity') %>%
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
        axis.text.x=element_text(face='bold', angle=80, hjust=0),
        legend.position='right',
        strip.placement='inside')

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
        axis.text.x=element_text(face='bold', angle=80, hjust=0),
        legend.position='none',
        strip.placement='inside',
        axis.text.y=element_blank(),
        axis.title.y=element_blank())


png(here('data','results','Fig8.png'), res=300, height=1300,width=1700)
grid.arrange(p + theme(plot.margin=margin(t=5,r=0,b=5,l=5)), 
             p2 + theme(plot.margin=margin(t=5,r=0,b=5,l=5)), ncol=2, nrow=1, widths=c(2,0.75))

dev.off()



# summary heat map, histograms --------------------------------------------
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

