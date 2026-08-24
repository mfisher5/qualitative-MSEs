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

triage_df <- read_csv(here('data','results','fig6_data.csv'))

## fix variable names for legend
triage_df %<>% 
  mutate(variable_out=fct_recode(variable_out,"Fishery 1 Catch"="fishery_1_catch",
                                 "Fishery 2 Catch"="fishery_2_catch",
                                 "Total Catch"="total_catch",
                                 "Spawning Biomass"="spawning_biomass"))



# Figure 6 -------------------------------------------------------------------

triage_df %<>%
  mutate(triage_equiv=ifelse(qnm_equiv==1, paste(triage,', equiv', sep=''), triage)) 

png(here('doc','Fig6-equiv.png'), res=300, height=1300,width=2900)
triage_df %>%
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


