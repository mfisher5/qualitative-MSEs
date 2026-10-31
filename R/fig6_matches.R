########### Create Figure 6 ###########
#
# Fig 6 summarizes false positive / false negatives  
#    from QNM output 
#    Data required for this script are created in 
#    "03_qnm_mse_compare.R"
#
# Written 2024-11-13 M Fisher
# Last Updated: 2026-08-30
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

## fix variable names for facets
triage_df %<>% 
  mutate(variable_out=fct_recode(variable_out,"Fishery 1 Catch"="fishery_1_catch",
                                 "Fishery 2 Catch"="fishery_2_catch",
                                 "Total Catch"="total_catch",
                                 "Spawning Biomass"="spawning_biomass"))

## fix OM / HCR names for labels
triage_df %<>% mutate(operating_model=ifelse(grepl('4',operating_model),
                                             "4. Higher growth rate", operating_model),
                      management_strategy=ifelse(grepl("C.",management_strategy),
                                    "C. Fishery 1 in \"offshore\" only,\neffort changes", management_strategy))

## which QNM is split 50/50 (uninformative)?
triage_df %>% filter(triage=='split') %>% dplyr::select(species,code)


# Figure 6 -------------------------------------------------------------------
## combine the fp/fn column with the equivocal column
triage_df %<>%
  mutate(triage_equiv=ifelse(qnm_equiv==1, paste(triage,', equiv', sep=''), triage)) 

## create fig!
png(here('doc','Fig6.png'), res=300, height=1300,width=2900,units='mm')
triage_df %>%
  mutate(triage_equiv=fct_relevel(triage_equiv, c('false negative, equiv',
                                                  'false positive', 'false positive, equiv'))) %>%
  mutate(variable_out=fct_relevel(variable_out, c("Total Catch", "Fishery 1 Catch", "Fishery 2 Catch", "Spawning Biomass"))) %>%
  mutate(management_strategy=fct_rev(management_strategy)) %>%
  filter(triage %in% c('false negative','false positive')) %>%
  ggplot(aes(x=operating_model,y=management_strategy, fill=triage_equiv, alpha=triage_equiv)) +
  geom_tile() +
  ## no 0-A
  geom_point(aes(x=c(1),y=c(5)),pch=4, col='gray50', size=3, show.legend=FALSE) +
  ## no 1-A
  geom_point(aes(x=c(2),y=c(5)),pch=4, col='gray50', size=3, show.legend=FALSE) +
  ## no 3-A because results 50/50 -- decided not to mark this the same as 0A/1A
  # geom_point(aes(x=c(4),y=c(5)),pch=4, col='gray50', size=3, show.legend=FALSE) +  # 
  facet_grid(rows=vars(species), cols=vars(variable_out)) +
  scale_fill_manual(values=c('darkorange','dodgerblue4','dodgerblue4')) + theme_bw() +
  scale_alpha_manual(values=c(0.5,1,0.5)) +
  labs(x='Operating model scenario',y='Management strategy') +
  theme_bw() + theme(axis.text.x=element_text(angle=45,hjust=1, vjust=1.05, size=7),
                     axis.text.y=element_text(size=8),
                     legend.text=element_text(size=8),
                     axis.title=element_text(size=9),
                     legend.title=element_blank(),
                     legend.margin = margin(0, 0, 0, -10))
dev.off()




# Figure 6, for publication -------------------------------------------------
## combine the fp/fn column with the equivocal column
triage_df %<>%
  mutate(triage_equiv=ifelse(qnm_equiv==1, paste(triage,',\nequiv', sep=''), triage)) 

## create fig!
jpeg(here('doc','Fig6-ices.jpg'), res=300, height=87,width=170,units='mm')
triage_df %>%
  mutate(triage_equiv=fct_relevel(triage_equiv, c('false negative,\nequiv',
                                                  'false positive', 'false positive,\nequiv'))) %>%
  mutate(variable_out=fct_relevel(variable_out, c("Total Catch", "Fishery 1 Catch", "Fishery 2 Catch", "Spawning Biomass"))) %>%
  mutate(management_strategy=fct_rev(management_strategy)) %>%
  filter(triage %in% c('false negative','false positive')) %>%
  ggplot(aes(x=operating_model,y=management_strategy, fill=triage_equiv, alpha=triage_equiv)) +
  geom_tile() +
  ## no 0-A
  geom_point(aes(x=c(1),y=c(5)),pch=4, col='gray50', size=3, show.legend=FALSE) +
  ## no 1-A
  geom_point(aes(x=c(2),y=c(5)),pch=4, col='gray50', size=3, show.legend=FALSE) +
  ## no 3-A because results 50/50 -- decided not to mark this the same as 0A/1A
  # geom_point(aes(x=c(4),y=c(5)),pch=4, col='gray50', size=3, show.legend=FALSE) +  # 
  facet_grid(rows=vars(species), cols=vars(variable_out)) +
  scale_fill_manual(values=c('darkorange','dodgerblue4','dodgerblue4')) + theme_bw() +
  scale_alpha_manual(values=c(0.5,1,0.5)) +
  labs(x='Operating model scenario',y='Management strategy') +
  theme_bw() + theme(axis.text.x=element_text(angle=45,hjust=1, vjust=1.05, size=7),
                     axis.text.y=element_text(size=7),
                     # legend.text=element_text(size=7),
                     axis.title=element_text(size=8),
                     strip.text=element_text(size=8),
                     legend.title=element_blank(),
                     legend.margin = margin(0, 0, 0, -8),
                     legend.key.spacing.y=unit(2,'mm'),
                     legend.text=element_text(size=7, margin=margin(l=0.1)))
dev.off()


pdf(here('doc','Fig6-ices.pdf'),height=3.42,width=6.78)
triage_df %>%
  mutate(triage_equiv=fct_relevel(triage_equiv, c('false negative,\nequiv',
                                                  'false positive', 'false positive,\nequiv'))) %>%
  mutate(variable_out=fct_relevel(variable_out, c("Total Catch", "Fishery 1 Catch", "Fishery 2 Catch", "Spawning Biomass"))) %>%
  mutate(management_strategy=fct_rev(management_strategy)) %>%
  filter(triage %in% c('false negative','false positive')) %>%
  ggplot(aes(x=operating_model,y=management_strategy, fill=triage_equiv, alpha=triage_equiv)) +
  geom_tile() +
  ## no 0-A
  geom_point(aes(x=c(1),y=c(5)),pch=4, col='gray50', size=3, show.legend=FALSE) +
  ## no 1-A
  geom_point(aes(x=c(2),y=c(5)),pch=4, col='gray50', size=3, show.legend=FALSE) +
  ## no 3-A because results 50/50 -- decided not to mark this the same as 0A/1A
  # geom_point(aes(x=c(4),y=c(5)),pch=4, col='gray50', size=3, show.legend=FALSE) +  # 
  facet_grid(rows=vars(species), cols=vars(variable_out)) +
  scale_fill_manual(values=c('darkorange','dodgerblue4','dodgerblue4')) + theme_bw() +
  scale_alpha_manual(values=c(0.5,1,0.5)) +
  labs(x='Operating model scenario',y='Management strategy') +
  theme_bw() + theme(axis.text.x=element_text(angle=45,hjust=1, vjust=1.05, size=7),
                     axis.text.y=element_text(size=7),
                     # legend.text=element_text(size=7),
                     axis.title=element_text(size=8),
                     strip.text=element_text(size=8),
                     legend.title=element_blank(),
                     legend.margin = margin(0, 0, 0, -8),
                     legend.key.spacing.y=unit(2,'mm'),
                     legend.text=element_text(size=7, margin=margin(l=0.1)))
dev.off()

