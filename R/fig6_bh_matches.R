########### Create Figure 6 ###########
#
# Fig 6 summarizes false positive / false negatives  
#    from QNM output. This version compares Ricker v. 
#    BH qMSE predictions.
#    Data required for this script are created in "03b_qnm_mse_compare.R"
#
# Written 2024-11-13 M Fisher
# Last Updated: 2026-08-29
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
triage_bh_df <- read_csv(here('data','results','fig6_bh_data.csv'))

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

# Data frames for Fig 6 ---------------------------------------------------
### MAIN TEXT ###
to_plot_main <- triage_df %>% ungroup() %>%
  # make one column for equiv / non-equiv mismatches
  mutate(triage_equiv=ifelse(qnm_equiv==1, paste(triage,', equiv', sep=''), triage)) %>%
  ## fix variable names for legend
  mutate(variable_out=fct_recode(variable_out,"Fishery 1 Catch"="fishery_1_catch",
                                 "Fishery 2 Catch"="fishery_2_catch",
                                 "Total Catch"="total_catch",
                                 "Spawning Biomass"="spawning_biomass")) %>%
  ## rename oms!
  mutate(operating_model=case_when(
    operating_model=="1. Thorson effort" ~ "1. Thorson\neffort",
    operating_model=="2. Lower productivity" ~ "2. Lower\nproductivity",
    operating_model=="3. Move \"far offshore\"" ~ "3. Move\n\"far offshore\"",
    operating_model=="4. Lower growth rate" ~ "4. Lower\ngrowth rate",
    .default=operating_model
  )) %>%
  ## rename ms!
  mutate(management_strategy=case_when(
    management_strategy=='C. Fishery 1 in "offshore" only' ~ 'C. Fishery 1 in "offshore" only,\neffort changes',
    .default=management_strategy
  )) %>%
  mutate(triage_equiv=fct_relevel(triage_equiv, c('false negative, equiv',
                                                  'false positive', 'false positive, equiv'))) %>%
  mutate(variable_out=fct_relevel(variable_out, c("Total Catch", "Fishery 1 Catch", "Fishery 2 Catch", "Spawning Biomass"))) %>%
  mutate(management_strategy=fct_rev(management_strategy))
to_plot_main %<>% mutate(mse_param='Ricker')

### BEVERTON-HOLT RECRUIT MODEL ###
to_plot_bh <- triage_bh_df %>% ungroup() %>%
  # make one column for equiv / non-equiv mismatches
  mutate(triage_equiv=ifelse(qnm_equiv==1, paste(triage,', equiv', sep=''), triage)) %>%
  ## fix variable names for legend
  mutate(variable_out=fct_recode(variable_out,"Fishery 1 Catch"="fishery_1_catch",
                                 "Fishery 2 Catch"="fishery_2_catch",
                                 "Total Catch"="total_catch",
                                 "Spawning Biomass"="spawning_biomass")) %>%
  ## rename oms!
  mutate(operating_model=case_when(
    operating_model=="1. Thorson effort" ~ "1. Thorson\neffort",
    operating_model=="2. Lower productivity" ~ "2. Lower\nproductivity",
    operating_model=="3. Move \"far offshore\"" ~ "3. Move\n\"far offshore\"",
    operating_model=="4. Lower growth rate" ~ "4. Lower\ngrowth rate",
    .default=operating_model
  )) %>%                                
  ## rename ms!
  mutate(management_strategy=case_when(
    management_strategy=='C. Fishery 1 in "offshore" only' ~ 'C. Fishery 1 in "offshore" only,\neffort changes',
    .default=management_strategy
  )) %>%
  mutate(triage_equiv=fct_relevel(triage_equiv, c('false negative, equiv',
                                                  'false positive', 'false positive, equiv'))) %>%
  mutate(variable_out=fct_relevel(variable_out, c("Total Catch", "Fishery 1 Catch", "Fishery 2 Catch", "Spawning Biomass"))) %>%
  mutate(management_strategy=fct_rev(management_strategy))

to_plot_bh %<>% mutate(mse_param='B-H')

all_to_plot <- bind_rows(
  to_plot_main,
  to_plot_bh
)
all_to_plot %<>% mutate(mse_param=fct_relevel(mse_param,
                                              c('Ricker','B-H')))


all_mse_equiv <- all_to_plot %>% filter(mse_equiv==1) %>%
  mutate(variable_out=case_when(
    variable_out=='Total Catch' ~ 'Total\nCatch',
    variable_out=='Fishery 1 Catch' ~ 'Fishery 1\nCatch',
    variable_out=='Fishery 2 Catch' ~ 'Fishery 2\nCatch',
    variable_out=='Spawning Biomass' ~ 'Spawning\nBiomass',
    .default=NA
  )) %>%
  mutate(variable_out=factor(variable_out, levels=c("Total\nCatch", "Fishery 1\nCatch", "Fishery 2\nCatch", "Spawning\nBiomass")))


no_qnms <- data.frame(operating_model=c(rep("0. Baseline",4),rep("1. Thorson\neffort",4)),
                      management_strategy=rep("A. Status-quo",8),
                      variable_out=rep(c('Total\nCatch','Fishery 1\nCatch','Fishery 2\nCatch','Spawning\nBiomass'),2))
no_qnms <- bind_rows(
  no_qnms %<>% mutate(mse_param='Ricker'),
  no_qnms %<>% mutate(mse_param='B-H')
) %>%
  mutate(variable_out=factor(variable_out, levels=c("Total\nCatch", "Fishery 1\nCatch", "Fishery 2\nCatch", "Spawning\nBiomass")))
  

# Figure 6 ----------------------------------------------------------------

png(here('doc','supplement','Fig6-bhVricker-equiv.png'), res=300, height=1800,width=3000)
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
    mutate(variable_out=factor(variable_out, levels=c("Total\nCatch", "Fishery 1\nCatch", "Fishery 2\nCatch", "Spawning\nBiomass"))) %>%
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
  nrow=1,ncol=2,rel_widths=c(1,1),
  labels = "AUTO",hjust=c(-13,0.3)
  
)
dev.off()




