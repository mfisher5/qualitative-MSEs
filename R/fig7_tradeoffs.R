########## Create Table 7 ###########
#
# Table 7 summarizes trade-offs in catch, biomass  
#    comparing QNM v MSE output. 
#    Data required for this script are created in "03_qnm_mse_compare.R"
#    Also use this script to create: Table C6, 
#
# Written 2024-12-27 M Fisher
# Last Updated: 2025-11-16
#
################################################################


# Set up ------------------------------------------------------------------

sim.date <- '20240923'      # date sims were run (in object names)
update.supplement <- FALSE  # do you want to save out new supplementary tables, figures?

## Functions
library(here)
library(tidyverse)
library(magrittr)
library(ggplot2)
library(janitor)
library(gridExtra)
library(PNWColors)
library(cowplot)



# Data --------------------------------------------------------------------

speciesALL_compare <- read_csv(here('data','results',paste0('mseVqnm_prob-psim_species1-2_allHCRallOM_',sim.date,'.csv')))

## fix variable names for legend
speciesALL_compare %<>% 
  filter(code != "2-A") %>%     # QNM 2 HCR A doesn't have a perturbation
  mutate(variable_out=fct_recode(variable_out,"Fishery 1 Catch"="fishery_1_catch",
                                 "Fishery 2 Catch"="fishery_2_catch",
                                 "Total Catch"="total_catch",
                                 "Spawning Biomass"="spawning_biomass"))

## prep df to describe trade-offs
tos <- speciesALL_compare  %>%
  dplyr::select(-model.x) %>%
  rename(qnm_psims=psims, mse_prob=prob, species=model.y) %>%
  mutate(species=case_when(
    species=="MSE-Species1" ~ "(sp.1) med longevity", 
    species=="MSE-Species2" ~ "(sp.2) short lived")) %>%
  mutate(qnm_equiv=ifelse(abs(qnm_psims) < 60, 1, 0))

## create full pairwise df 
tradeoffs_vec <- c("catch increases,\nbiomass increases","catch increases,\nbiomass decreases","catch decreases,\nbiomass increases", "catch decreases,\nbiomass decreases")
pairwise_df <- data.frame(MSE= rep(tradeoffs_vec,each=4),
  QNM=rep(tradeoffs_vec, 4))
pairwise_df %<>% 
  mutate(species="(sp.1) med longevity") %>%
  bind_rows(
    pairwise_df %>% mutate(species="(sp.2) short lived")
  )

## create full pairwise df for ARROWS
tradeoffs_vec2 <- c('\u2191 catch,\n\u2191 biomass', 
                    '\u2191 catch,\n\u2193 biomass',
                    '\u2193 catch,\n\u2191 biomass',
                    '\u2193 catch,\n\u2193 biomass')
pairwise_arrows <- data.frame(MSE= rep(tradeoffs_vec2,each=4),
                          QNM=rep(tradeoffs_vec2, 4))
pairwise_arrows %<>% 
  mutate(species="(sp.1) med longevity") %>%
  bind_rows(
    pairwise_arrows %>% mutate(species="(sp.2) short lived")
  )

## add in equivocal QNM output
tradeoffs_vec2_equiv <- c('\u2191 catch,\n\u2191 biomass', 
                          '\u2191 catch,\n\u2193 biomass',
                          '\u2193 catch,\n\u2191 biomass',
                          '\u2193 catch,\n\u2193 biomass',
                          '\u2191 catch,\n? biomass',
                          '\u2193 catch,\n? biomass',
                          '? catch,\n\u2191 biomass',
                          '? catch,\n\u2193 biomass',
                          '? catch,\n? biomass')

# Write to excel to manually code -----------------------------------------
# 
# tos %>%
#   filter(variable_out %in% c("Total Catch", "Spawning Biomass")) %>%
#   mutate(variable_out=case_when(
#     variable_out == "Total Catch" ~ "Total Catch QNM",
#     variable_out == "Spawning Biomass" ~ "Spawning Biomass QNM"
#   )) %>%
#   dplyr::select(operating_model, management_strategy, code, variable_out, qnm_psims, species) %>%
#   pivot_wider(names_from=variable_out, values_from=qnm_psims) %>%
#   left_join(
# 
#     tos %>%
#       filter(variable_out %in% c("Total Catch", "Spawning Biomass")) %>%
#       mutate(variable_out=case_when(
#         variable_out == "Total Catch" ~ "Total Catch MSE",
#         variable_out == "Spawning Biomass" ~ "Spawning Biomass MSE"
#       )) %>%
#       dplyr::select(operating_model, management_strategy, code, variable_out, mse_prob, species) %>%
#       pivot_wider(names_from=variable_out, values_from=mse_prob)
#   ) %>%
#   write_csv(here('data','results',paste0('mseVqnm_tradeoffs_species1-2_allHCRallOM_',sim.date,'.csv')))




# Total catch: allow equiv -------------------------------------------------------------

tos_total <- tos %>%
  filter(variable_out %in% c("Total Catch", "Spawning Biomass")) %>%
  mutate(variable_out=ifelse(variable_out=="Total Catch","total_catch","spawning_biomass")) %>%
  dplyr::select(operating_model, management_strategy, code, variable_out, qnm_psims, species) %>%
  pivot_wider(names_from=variable_out, values_from=qnm_psims) %>%
  ## \u2191 for increasing, \u2193 for decreasing
  mutate(QNM=case_when(
    total_catch > 0 & spawning_biomass > 0 ~ '\u2191 catch,\n\u2191 biomass',
    total_catch < 0 & spawning_biomass > 0 ~ '\u2193 catch,\n\u2191 biomass',
    total_catch < 0 & spawning_biomass < 0 ~ '\u2193 catch,\n\u2193 biomass',
    total_catch > 0 & spawning_biomass < 0 ~ '\u2191 catch,\n\u2193 biomass'
  )) %>%
  dplyr::select(operating_model, management_strategy, code, species, QNM) %>%
  left_join(
    
    tos %>%
      filter(variable_out %in% c("Total Catch", "Spawning Biomass")) %>%
      mutate(variable_out=ifelse(variable_out=="Total Catch","total_catch","spawning_biomass")) %>%
      dplyr::select(operating_model, management_strategy, code, variable_out, mse_prob, species) %>%
      pivot_wider(names_from=variable_out, values_from=mse_prob) %>%
      mutate(MSE=case_when(
        total_catch > 0 & spawning_biomass > 0 ~ '\u2191 catch,\n\u2191 biomass',
        total_catch < 0 & spawning_biomass > 0 ~ '\u2193 catch,\n\u2191 biomass',
        total_catch < 0 & spawning_biomass < 0 ~ '\u2193 catch,\n\u2193 biomass',
        total_catch > 0 & spawning_biomass < 0 ~ '\u2191 catch,\n\u2193 biomass'
      ))  %>%
      dplyr::select(operating_model, management_strategy, code, species, MSE)
    
    
  )


pal=pnw_palette("Moth",100)
tos_total %>%
  group_by(species, QNM, MSE) %>%
  summarise(n=length(unique(code))) %>%
  full_join(pairwise_arrows, by=c("QNM","MSE","species")) %>%
  ggplot(aes(x=fct_relevel(MSE, tradeoffs_vec2), y=fct_relevel(QNM, rev(tradeoffs_vec2)), fill=n)) +
  geom_tile() + 
  geom_rect(aes(xmin=c(0.5),xmax=c(1.5),
                ymin=c(3.5),ymax=c(4.5)), fill='transparent',col='black') +
  geom_rect(aes(xmin=1.5, xmax=2.5,
                ymin=2.5, ymax=3.5), fill='transparent',col='black') +
  geom_rect(aes(xmin=2.5, xmax=3.5,
                ymin=1.5, ymax=2.5), fill='transparent',col='black') +
  geom_rect(aes(xmin=3.5, xmax=4.5,
                ymin=0.5, ymax=1.5), fill='transparent',col='black') +
  facet_wrap(~species,strip.position='bottom') +
  labs(x="MSE outcome",y="QNM outcome") +
  scale_x_discrete(position='top') +
  scale_fill_gradientn(colours = rev(pal), na.value='white', limits=c(0,23)) +
  theme_bw() +
  theme(legend.title=element_blank()) -> plot.total

plot.total

# Total catch: no equiv -------------------------------------------------------------

tos2_total <- tos %>%
  filter(variable_out %in% c("Total Catch", "Spawning Biomass")) %>%
  mutate(variable_out=ifelse(variable_out=="Total Catch","total_catch","spawning_biomass")) %>%
  mutate(qnm_psims=ifelse(qnm_equiv==1, NA, qnm_psims)) %>%
  dplyr::select(operating_model, management_strategy, code, variable_out, qnm_psims, species) %>%
  pivot_wider(names_from=variable_out, values_from=qnm_psims) %>%
  mutate(QNM=case_when(
    is.na(total_catch) & spawning_biomass > 0 ~ '? catch,\n\u2191 biomass', 
    is.na(total_catch) & spawning_biomass < 0 ~ '? catch,\n\u2193 biomass', 
    total_catch > 0 & is.na(spawning_biomass) ~ '\u2191 catch,\n? biomass',
    total_catch < 0 & is.na(spawning_biomass) ~ '\u2193 catch,\n? biomass',
    total_catch > 0 & spawning_biomass > 0 ~ '\u2191 catch,\n\u2191 biomass',
    total_catch < 0 & spawning_biomass > 0 ~ '\u2193 catch,\n\u2191 biomass',
    total_catch < 0 & spawning_biomass < 0 ~ '\u2193 catch,\n\u2193 biomass',
    total_catch > 0 & spawning_biomass < 0 ~ '\u2191 catch,\n\u2193 biomass', 
    is.na(total_catch) & is.na(spawning_biomass) ~ '? catch,\n? biomass'
  )) %>%
  filter(QNM != '? catch,\n? biomass') %>%
  dplyr::select(operating_model, management_strategy, code, species, QNM) %>%
  left_join(
    
    tos %>%
      filter(variable_out %in% c("Total Catch", "Spawning Biomass")) %>%
      mutate(variable_out=ifelse(variable_out=="Total Catch","total_catch","spawning_biomass")) %>%
      dplyr::select(operating_model, management_strategy, code, variable_out, mse_prob, species) %>%
      pivot_wider(names_from=variable_out, values_from=mse_prob) %>%
      mutate(MSE=case_when(
        total_catch > 0 & spawning_biomass > 0 ~ '\u2191 catch,\n\u2191 biomass',
        total_catch < 0 & spawning_biomass > 0 ~ '\u2193 catch,\n\u2191 biomass',
        total_catch < 0 & spawning_biomass < 0 ~ '\u2193 catch,\n\u2193 biomass',
        total_catch > 0 & spawning_biomass < 0 ~ '\u2191 catch,\n\u2193 biomass'
      ))  %>%
      dplyr::select(operating_model, management_strategy, code, species, MSE)
    
    
  )

tradeoffs_vec2_qnm <- tradeoffs_vec2_equiv[
  which((tradeoffs_vec2_equiv %in% tradeoffs_vec2) | (tradeoffs_vec2_equiv %in% unique(tos2_total$QNM)))
]
pal=pnw_palette("Shuksan",100)
tos2_total %>%
  group_by(species, QNM, MSE) %>%
  summarise(n=length(unique(code))) %>%
  full_join(pairwise_arrows, by=c("QNM","MSE","species")) %>%
  mutate(n=ifelse(is.na(n), 0, n),n_lab=ifelse(n==0,NA,n),n_col=ifelse(n>12,'gray90','gray40')) %>%
  ggplot(aes(x=fct_relevel(MSE, tradeoffs_vec2), y=fct_relevel(QNM, c(rev(tradeoffs_vec2_qnm))), fill=n)) +
  geom_tile() + 
  geom_text(aes(label=n_lab, fontface='italic'), col=c(rep('gray40',9),'gray99',rep('gray40',27))) +
  geom_rect(aes(xmin=c(0.5),xmax=c(1.5),
                ymin=c(5.5),ymax=c(6.5)), fill='transparent',col='black') +
  geom_rect(aes(xmin=1.5, xmax=2.5,
                ymin=4.5, ymax=5.5), fill='transparent',col='black') +
  geom_rect(aes(xmin=2.5, xmax=3.5,
                ymin=3.5, ymax=4.5), fill='transparent',col='black') +
  geom_rect(aes(xmin=3.5, xmax=4.5,
                ymin=2.5, ymax=3.5), fill='transparent',col='black') +
  geom_hline(aes(yintercept=2.5)) +
  facet_wrap(~species,strip.position='bottom') +
  labs(x="Quant MSE",y="QNM") +
  scale_x_discrete(position='top') +
  scale_fill_gradientn(colours = rev(pal), na.value='white', limits=c(0,15)) +
  theme_bw() +
  theme(legend.title=element_blank(), strip.text=element_blank()) -> plot.total.equiv

plot.total.equiv

## update Table C6
if(update.supplement){
  tos2_total %>% 
    mutate(MSE=case_when(
      grepl('\u2191',MSE) | grepl('\n\u2191',MSE) ~ str_replace_all(MSE,'\u2191','+'),
      .default=MSE),
    MSE=case_when(
      grepl('\u2193',MSE) | grepl('\n\u2193',MSE) ~ str_replace_all(MSE,'\u2193','-'),
      .default=MSE),
    MSE=case_when(grepl('\n',MSE) ~ str_replace_all(MSE,'\n',' '),
                  .default=MSE)) %>%
    mutate(QNM=case_when(
        grepl('\u2191',QNM) | grepl('\n\u2191',QNM) ~ str_replace_all(QNM,'\u2191','+'),
        .default=QNM),
      QNM=case_when(
        grepl('\u2193',QNM) | grepl('\n\u2193',QNM) ~ str_replace_all(QNM,'\u2193','-'),
        .default=QNM),
      QNM=case_when(grepl('\n',QNM) ~ str_replace_all(QNM,'\n',' '),
                    .default=QNM)) %>%
    mutate(MSE=paste0('"',MSE,'"'), QNM=paste0('"',QNM,'"')) %>%
    mutate(catch_metric='Total catch') %>%
    write_csv(file=here('data','results',paste0('tableC6_qnmVmse_tradeoffs_byOMbyHCRbySp_',sim.date,'.csv')), quote='all')
}




# Fishery 1: allow equiv ---------------------------------------------------------

tos_fish1 <- tos %>%
  filter(variable_out %in% c("Fishery 1 Catch", "Spawning Biomass")) %>%
  mutate(variable_out=ifelse(variable_out=="Fishery 1 Catch","fishery1","spawning_biomass")) %>%
  dplyr::select(operating_model, management_strategy, code, variable_out, qnm_psims, species) %>%
  pivot_wider(names_from=variable_out, values_from=qnm_psims) %>%
  mutate(QNM=case_when(
    fishery1 > 0 & spawning_biomass > 0 ~ '\u2191 catch,\n\u2191 biomass',
    fishery1 < 0 & spawning_biomass > 0 ~ '\u2193 catch,\n\u2191 biomass',
    fishery1 < 0 & spawning_biomass < 0 ~ '\u2193 catch,\n\u2193 biomass',
    fishery1 > 0 & spawning_biomass < 0 ~ '\u2191 catch,\n\u2193 biomass'
  )) %>%
  dplyr::select(operating_model, management_strategy, code, species, QNM) %>%
  left_join(
    
    tos %>%
      filter(variable_out %in% c("Fishery 1 Catch", "Spawning Biomass")) %>%
      mutate(variable_out=ifelse(variable_out=="Fishery 1 Catch","fishery1","spawning_biomass")) %>%
      dplyr::select(operating_model, management_strategy, code, variable_out, mse_prob, species) %>%
      pivot_wider(names_from=variable_out, values_from=mse_prob) %>%
      mutate(MSE=case_when(
        fishery1 > 0 & spawning_biomass > 0 ~ '\u2191 catch,\n\u2191 biomass',
        fishery1 < 0 & spawning_biomass > 0 ~ '\u2193 catch,\n\u2191 biomass',
        fishery1 < 0 & spawning_biomass < 0 ~ '\u2193 catch,\n\u2193 biomass',
        fishery1 > 0 & spawning_biomass < 0 ~ '\u2191 catch,\n\u2193 biomass'
      ))  %>%
      dplyr::select(operating_model, management_strategy, code, species, MSE)
    
    
  )

pal=pnw_palette("Moth",100)
tos_fish1 %>%
  group_by(species, QNM, MSE) %>%
  summarise(n=length(unique(code))) %>%
  full_join(pairwise_arrows, by=c("QNM","MSE","species")) %>%
  ggplot(aes(x=fct_relevel(MSE, tradeoffs_vec2), y=fct_relevel(QNM, rev(tradeoffs_vec2)), fill=n)) +
  geom_tile() + 
  geom_rect(aes(xmin=c(0.5),xmax=c(1.5),
                ymin=c(3.5),ymax=c(4.5)), fill='transparent',col='black') +
  geom_rect(aes(xmin=1.5, xmax=2.5,
                ymin=2.5, ymax=3.5), fill='transparent',col='black') +
  geom_rect(aes(xmin=2.5, xmax=3.5,
                ymin=1.5, ymax=2.5), fill='transparent',col='black') +
  geom_rect(aes(xmin=3.5, xmax=4.5,
                ymin=0.5, ymax=1.5), fill='transparent',col='black') +
  facet_wrap(~species,strip.position='bottom') +
  labs(x="MSE outcome",y="QNM outcome") +
  scale_x_discrete(position='top') +
  scale_fill_gradientn(colours = rev(pal), na.value='white') +
  theme_bw() +
  theme(legend.title=element_blank()) -> plot.fish1

plot.fish1


# Fishery 1: no equiv -----------------------------------------------

### remove equivocal QNM results as 'uncertain'
tos2_fish1 <- tos %>%
  filter(variable_out %in% c("Fishery 1 Catch", "Spawning Biomass")) %>%
  mutate(variable_out=ifelse(variable_out=="Fishery 1 Catch","fishery1","spawning_biomass")) %>%
  mutate(qnm_psims=ifelse(qnm_equiv==1, NA, qnm_psims)) %>%
  dplyr::select(operating_model, management_strategy, code, variable_out, qnm_psims, species) %>%
  pivot_wider(names_from=variable_out, values_from=qnm_psims) %>%
  mutate(QNM=case_when(
    is.na(fishery1) & spawning_biomass > 0 ~ '? catch,\n\u2191 biomass', 
    is.na(fishery1) & spawning_biomass < 0 ~ '? catch,\n\u2193 biomass', 
    fishery1 > 0 & is.na(spawning_biomass) ~ '\u2191 catch,\n? biomass',
    fishery1 < 0 & is.na(spawning_biomass) ~ '\u2193 catch,\n? biomass',
    fishery1 > 0 & spawning_biomass > 0 ~ '\u2191 catch,\n\u2191 biomass',
    fishery1 < 0 & spawning_biomass > 0 ~ '\u2193 catch,\n\u2191 biomass',
    fishery1 < 0 & spawning_biomass < 0 ~ '\u2193 catch,\n\u2193 biomass',
    fishery1 > 0 & spawning_biomass < 0 ~ '\u2191 catch,\n\u2193 biomass', 
    is.na(fishery1) & is.na(spawning_biomass) ~ '? catch,\n? biomass'
  )) %>%
  filter(QNM != '? catch,\n? biomass') %>%
  dplyr::select(operating_model, management_strategy, code, species, QNM) %>%
  left_join(
    
    tos %>%
      filter(variable_out %in% c("Fishery 1 Catch", "Spawning Biomass")) %>%
      mutate(variable_out=ifelse(variable_out=="Fishery 1 Catch","fishery1","spawning_biomass")) %>%
      dplyr::select(operating_model, management_strategy, code, variable_out, mse_prob, species) %>%
      pivot_wider(names_from=variable_out, values_from=mse_prob) %>%
      mutate(MSE=case_when(
        fishery1 > 0 & spawning_biomass > 0 ~ '\u2191 catch,\n\u2191 biomass',
        fishery1 < 0 & spawning_biomass > 0 ~ '\u2193 catch,\n\u2191 biomass',
        fishery1 < 0 & spawning_biomass < 0 ~ '\u2193 catch,\n\u2193 biomass',
        fishery1 > 0 & spawning_biomass < 0 ~ '\u2191 catch,\n\u2193 biomass'
      ))  %>%
      dplyr::select(operating_model, management_strategy, code, species, MSE)
    
    
  )

tradeoffs_vec2_qnm <- tradeoffs_vec2_equiv[
  which((tradeoffs_vec2_equiv %in% tradeoffs_vec2) | (tradeoffs_vec2_equiv %in% unique(tos2_fish1$QNM)))
]
pal=pnw_palette("Shuksan",100)
tos2_fish1 %>%
  group_by(species, QNM, MSE) %>%
  summarise(n=length(unique(code))) %>%
  full_join(pairwise_arrows, by=c("QNM","MSE","species")) %>%
  mutate(n=ifelse(is.na(n), 0, n), n_lab=ifelse(n==0,NA,n)) %>%
  ggplot(aes(x=fct_relevel(MSE, tradeoffs_vec2), y=fct_relevel(QNM, c(rev(tradeoffs_vec2_qnm))), fill=n)) +
  geom_tile() + 
  geom_text(aes(label=n_lab, fontface='italic'), color='gray40', ) +
  geom_rect(aes(xmin=c(0.5),xmax=c(1.5),
                ymin=c(5.5),ymax=c(6.5)), fill='transparent',col='black') +
  geom_rect(aes(xmin=1.5, xmax=2.5,
                ymin=4.5, ymax=5.5), fill='transparent',col='black') +
  geom_rect(aes(xmin=2.5, xmax=3.5,
                ymin=3.5, ymax=4.5), fill='transparent',col='black') +
  geom_rect(aes(xmin=3.5, xmax=4.5,
                ymin=2.5, ymax=3.5), fill='transparent',col='black') +
  geom_hline(aes(yintercept=2.5)) +
  facet_wrap(~species,strip.position='bottom') +
  labs(x="Quant MSE",y="QNM") +
  scale_x_discrete(position='top') +
  scale_fill_gradientn(colours = rev(pal), na.value='white', limits=c(0,15)) +
  theme_bw() +
  theme(legend.title=element_blank(), strip.text=element_blank()) -> plot.fish1.equiv

plot.fish1.equiv

## update Table C6
if(update.supplement){
  tos2_fish1 %>% 
    mutate(MSE=case_when(
      grepl('\u2191',MSE) | grepl('\n\u2191',MSE) ~ str_replace_all(MSE,'\u2191','+'),
      .default=MSE),
      MSE=case_when(
        grepl('\u2193',MSE) | grepl('\n\u2193',MSE) ~ str_replace_all(MSE,'\u2193','-'),
        .default=MSE),
      MSE=case_when(grepl('\n',MSE) ~ str_replace_all(MSE,'\n',' '),
                    .default=MSE)) %>%
    mutate(QNM=case_when(
      grepl('\u2191',QNM) | grepl('\n\u2191',QNM) ~ str_replace_all(QNM,'\u2191','+'),
      .default=QNM),
      QNM=case_when(
        grepl('\u2193',QNM) | grepl('\n\u2193',QNM) ~ str_replace_all(QNM,'\u2193','-'),
        .default=QNM),
      QNM=case_when(grepl('\n',QNM) ~ str_replace_all(QNM,'\n',' '),
                    .default=QNM)) %>%
    mutate(MSE=paste0('"',MSE,'"'), QNM=paste0('"',QNM,'"')) %>%
    mutate(catch_metric='Fishery 1 catch') %>%
    write_csv(file=here('data','results',paste0('tableC6_qnmVmse_tradeoffs_byOMbyHCRbySp_',sim.date,'.csv')), quote='all', append=TRUE)
}



# Fishery 2: allow equiv ---------------------------------------------------------

tos_fish2 <- tos %>%
  filter(variable_out %in% c("Fishery 2 Catch", "Spawning Biomass")) %>%
  mutate(variable_out=ifelse(variable_out=="Fishery 2 Catch","fishery2","spawning_biomass")) %>%
  dplyr::select(operating_model, management_strategy, code, variable_out, qnm_psims, species) %>%
  pivot_wider(names_from=variable_out, values_from=qnm_psims) %>%
  mutate(QNM=case_when(
    fishery2 > 0 & spawning_biomass > 0 ~ '\u2191 catch,\n\u2191 biomass',
    fishery2 < 0 & spawning_biomass > 0 ~ '\u2193 catch,\n\u2191 biomass',
    fishery2 < 0 & spawning_biomass < 0 ~ '\u2193 catch,\n\u2193 biomass',
    fishery2 > 0 & spawning_biomass < 0 ~ '\u2191 catch,\n\u2193 biomass'
  )) %>%
  dplyr::select(operating_model, management_strategy, code, species, QNM) %>%
  left_join(
    
    tos %>%
      filter(variable_out %in% c("Fishery 2 Catch", "Spawning Biomass")) %>%
      mutate(variable_out=ifelse(variable_out=="Fishery 2 Catch","fishery2","spawning_biomass")) %>%
      dplyr::select(operating_model, management_strategy, code, variable_out, mse_prob, species) %>%
      pivot_wider(names_from=variable_out, values_from=mse_prob) %>%
      mutate(MSE=case_when(
        fishery2 > 0 & spawning_biomass > 0 ~ '\u2191 catch,\n\u2191 biomass',
        fishery2 < 0 & spawning_biomass > 0 ~ '\u2193 catch,\n\u2191 biomass',
        fishery2 < 0 & spawning_biomass < 0 ~ '\u2193 catch,\n\u2193 biomass',
        fishery2 > 0 & spawning_biomass < 0 ~ '\u2191 catch,\n\u2193 biomass'
      ))  %>%
      dplyr::select(operating_model, management_strategy, code, species, MSE)
    
    
  )

pal=pnw_palette("Moth",100)
tos_fish2 %>%
  group_by(QNM, MSE, species) %>%
  summarise(n=length(unique(code))) %>%
  full_join(pairwise_arrows, by=c("QNM","MSE","species")) %>%
  ggplot(aes(x=fct_relevel(MSE, tradeoffs_vec2), y=fct_relevel(QNM, rev(tradeoffs_vec2)), fill=n)) +
  geom_tile() + 
  geom_rect(aes(xmin=c(0.5),xmax=c(1.5),
                ymin=c(3.5),ymax=c(4.5)), fill='transparent',col='black') +
  geom_rect(aes(xmin=1.5, xmax=2.5,
                ymin=2.5, ymax=3.5), fill='transparent',col='black') +
  geom_rect(aes(xmin=2.5, xmax=3.5,
                ymin=1.5, ymax=2.5), fill='transparent',col='black') +
  geom_rect(aes(xmin=3.5, xmax=4.5,
                ymin=0.5, ymax=1.5), fill='transparent',col='black') +
  facet_wrap(~species, strip.position='bottom') +
  labs(x="MSE outcome",y="QNM outcome") +
  scale_x_discrete(position='top') +
  scale_fill_gradientn(colours = rev(pal), na.value='white') +
  theme_bw() +
  theme(legend.title=element_blank()) -> plot.fish2

plot.fish2


# Fishery 2 catch: no equiv -----------------------------------------------

### remove equivocal QNM results as 'uncertain'
tos2_fish2 <- tos %>%
  filter(variable_out %in% c("Fishery 2 Catch", "Spawning Biomass")) %>%
  mutate(variable_out=ifelse(variable_out=="Fishery 2 Catch","fishery2","spawning_biomass")) %>%
  mutate(qnm_psims=ifelse(qnm_equiv==1, NA, qnm_psims)) %>%
  dplyr::select(operating_model, management_strategy, code, variable_out, qnm_psims, species) %>%
  pivot_wider(names_from=variable_out, values_from=qnm_psims) %>%
  mutate(QNM=case_when(
    is.na(fishery2) & spawning_biomass > 0 ~ '? catch,\n\u2191 biomass', 
    is.na(fishery2) & spawning_biomass < 0 ~ '? catch,\n\u2193 biomass', 
    fishery2 > 0 & is.na(spawning_biomass) ~ '\u2191 catch,\n? biomass',
    fishery2 < 0 & is.na(spawning_biomass) ~ '\u2193 catch,\n? biomass',
    fishery2 > 0 & spawning_biomass > 0 ~ '\u2191 catch,\n\u2191 biomass',
    fishery2 < 0 & spawning_biomass > 0 ~ '\u2193 catch,\n\u2191 biomass',
    fishery2 < 0 & spawning_biomass < 0 ~ '\u2193 catch,\n\u2193 biomass',
    fishery2 > 0 & spawning_biomass < 0 ~ '\u2191 catch,\n\u2193 biomass', 
    is.na(fishery2) & is.na(spawning_biomass) ~ '? catch,\n? biomass'
  )) %>%
  filter(QNM != '? catch,\n? biomass') %>%
  dplyr::select(operating_model, management_strategy, code, species, QNM) %>%
  left_join(
    
    tos %>%
      filter(variable_out %in% c("Fishery 2 Catch", "Spawning Biomass")) %>%
      mutate(variable_out=ifelse(variable_out=="Fishery 2 Catch","fishery2","spawning_biomass")) %>%
      dplyr::select(operating_model, management_strategy, code, variable_out, mse_prob, species) %>%
      pivot_wider(names_from=variable_out, values_from=mse_prob) %>%
      mutate(MSE=case_when(
        fishery2 > 0 & spawning_biomass > 0 ~ '\u2191 catch,\n\u2191 biomass',
        fishery2 < 0 & spawning_biomass > 0 ~ '\u2193 catch,\n\u2191 biomass',
        fishery2 < 0 & spawning_biomass < 0 ~ '\u2193 catch,\n\u2193 biomass',
        fishery2 > 0 & spawning_biomass < 0 ~ '\u2191 catch,\n\u2193 biomass'
      ))  %>%
      dplyr::select(operating_model, management_strategy, code, species, MSE)
    
    
  )

tradeoffs_vec2_qnm <- tradeoffs_vec2_equiv[
  which((tradeoffs_vec2_equiv %in% tradeoffs_vec2) | (tradeoffs_vec2_equiv %in% unique(tos2_fish2$QNM)))
]
pal=pnw_palette("Shuksan",100)
tos2_fish2 %>%
  group_by(QNM, MSE, species) %>%
  summarise(n=length(unique(code))) %>%
  full_join(pairwise_arrows, by=c("QNM","MSE","species")) %>%
  mutate(n=ifelse(is.na(n), 0, n),n_lab=ifelse(n==0,NA,n)) %>%
  ggplot(aes(x=fct_relevel(MSE, tradeoffs_vec2), y=fct_relevel(QNM, rev(tradeoffs_vec2_qnm)), fill=n)) +
  geom_tile() + 
  geom_text(aes(label=n_lab, fontface='italic'), col=c(rep('gray40',2),'gray99','gray99',rep('gray40',32))) +
  geom_rect(aes(xmin=c(0.5),xmax=c(1.5),
                ymin=c(5.5),ymax=c(6.5)), fill='transparent',col='black') +
  geom_rect(aes(xmin=1.5, xmax=2.5,
                ymin=4.5, ymax=5.5), fill='transparent',col='black') +
  geom_rect(aes(xmin=2.5, xmax=3.5,
                ymin=3.5, ymax=4.5), fill='transparent',col='black') +
  geom_rect(aes(xmin=3.5, xmax=4.5,
                ymin=2.5, ymax=3.5), fill='transparent',col='black') +
  geom_hline(aes(yintercept=2.5)) +
  facet_wrap(~species, strip.position='bottom') +
  labs(x="Quant MSE",y="QNM") +
  scale_x_discrete(position='top') +
  scale_fill_gradientn(colours = rev(pal), na.value='white', limits=c(0,15)) +
  theme_bw() +
  theme(legend.title=element_blank(), strip.text=element_blank()) -> plot.fish2.equiv

plot.fish2.equiv

## update Table C6
if(update.supplement){
  tos2_fish2 %>% 
    mutate(MSE=case_when(
      grepl('\u2191',MSE) | grepl('\n\u2191',MSE) ~ str_replace_all(MSE,'\u2191','+'),
      .default=MSE),
      MSE=case_when(
        grepl('\u2193',MSE) | grepl('\n\u2193',MSE) ~ str_replace_all(MSE,'\u2193','-'),
        .default=MSE),
      MSE=case_when(grepl('\n',MSE) ~ str_replace_all(MSE,'\n',' '),
                    .default=MSE)) %>%
    mutate(QNM=case_when(
      grepl('\u2191',QNM) | grepl('\n\u2191',QNM) ~ str_replace_all(QNM,'\u2191','+'),
      .default=QNM),
      QNM=case_when(
        grepl('\u2193',QNM) | grepl('\n\u2193',QNM) ~ str_replace_all(QNM,'\u2193','-'),
        .default=QNM),
      QNM=case_when(grepl('\n',QNM) ~ str_replace_all(QNM,'\n',' '),
                    .default=QNM)) %>%
    mutate(MSE=paste0('"',MSE,'"'), QNM=paste0('"',QNM,'"')) %>%
    mutate(catch_metric='Fishery 2 catch') %>%
    write_csv(file=here('data','results',paste0('tableC6_qnmVmse_tradeoffs_byOMbyHCRbySp_',sim.date,'.csv')), quote='all', append=TRUE)
}

  

# FIGURE 7 ----------------------------------------------------------


plot_grid(plotlist=list(
  plot.total + ggtitle("(A) Total catch"),
  plot.fish1 + ggtitle("(B) Fishery 1 catch"),
  plot.fish2 + ggtitle("(C) Fishery 2 catch")
), ncol=1, nrow=3)



png(here('data','results','fig8-tradeoffs-include-equiv.png'), res=300,height=2800,width=2200)
plot_grid(plotlist=list(
  plot.total + ggtitle("(A) Total catch"),
  plot.fish1 + ggtitle("(B) Fishery 1 catch"),
  plot.fish2 + ggtitle("(C) Fishery 2 catch")
), ncol=1, nrow=3)
dev.off()

png(here('data','results','fig7-tradeoffs-separate-equiv.png'), res=300,height=2600,width=2100)
ggdraw(plot_grid(plotlist=list(
  plot.total.equiv+theme(legend.position='none',plot.margin = unit(c(1, 3.75, 2, 2), "lines"), axis.title=element_blank()),
  plot.fish1.equiv+theme(plot.margin = unit(c(0, 0, 2, 2), "lines"), axis.title=element_blank()),
  plot.fish2.equiv+theme(legend.position='none',plot.margin = unit(c(0, 3.75, 2, 2), "lines"), axis.title=element_blank())
), labels=c("(a) Total catch (sp.1)                           (d) Total catch (sp.2)", 
            "(b) Fishery 1 catch (sp.1)                    (e) Fishery 1 catch (sp.2)", 
            "(c) Fishery 2 catch (sp.1)                    (f) Fishery 2 catch (sp.2)"), 
label_size=12.5,
label_x=c(-0.08, -0.1, -0.1),label_y=c(1.01,1.05,1.05), hjust=c(-0.25,-0.25,-0.25), vjust=c(1.2,0.75,0.75), 
ncol=1, nrow=3))  + draw_label("Quantitative MSE", x = 0.5, y = 0.02, color = "gray20", size=11) +
  draw_line(x = c(0.61, 0.9),y = c(0.02, 0.02), color = "gray20", size = 0.25)+
  draw_line(x = c(0.16, 0.39),y = c(0.02, 0.02), color = "gray20", size = 0.25) +
  draw_label("QNMs", x = 0.02, y = 0.5, angle=90,  color = "gray20", size=11) +
  draw_line(y = c(0.54, 0.9),x = c(0.02, 0.02), color = "gray20", size = 0.25)+
  draw_line(y = c(0.1, 0.46),x = c(0.02, 0.02), color = "gray20", size = 0.25)
dev.off()


# FIGURE C3 ----------------------------------------------------------

# Species 1 ---------------------------------------------------------------
## tabulate "+" and "-" by OM / MS
tos_total2 <- tos %>%
  filter(variable_out %in% c("Total Catch", "Spawning Biomass")) %>%
  mutate(variable_out=ifelse(variable_out=="Total Catch","total_catch","spawning_biomass")) %>%
  dplyr::select(operating_model, management_strategy, code, variable_out, qnm_psims, qnm_equiv, mse_prob, species) %>%
  mutate(QNM=case_when(
    (qnm_psims > 0 & qnm_equiv==0) ~ '+',
    (qnm_psims > 0 & qnm_equiv==1) ~ '(+)',
    (qnm_psims < 0 & qnm_equiv==0) ~ '-',
    (qnm_psims < 0 & qnm_equiv==1) ~ '(-)'
  )) %>%
  mutate(MSE=case_when(
    (mse_prob > 0) ~ '+',
    (mse_prob < 0) ~ '-'))
  
any(is.na(tos_total2$MSE))
any(is.na(tos_total2$QNM))

tos_total2 %<>% 
  mutate(qnm_compare=case_when(
    (QNM==MSE | (QNM=="(+)" & MSE=="+") | (QNM=="(-)" & MSE=="-")) ~ "same", 
    ((QNM=="+" | QNM=="(+)" ) & MSE=="-") ~ "optimistic",
    ((QNM=="-" | QNM=="(-)" ) & MSE=="+") ~ "pessimistic"))
any(is.na(tos_total2$qnm_compare))

## create table
p2_total <- tos_total2 %>%
  dplyr::select(-qnm_psims,-qnm_equiv,-mse_prob) %>%
  rename(qMSE=MSE) %>%
  pivot_longer(cols=c(QNM, MSE), names_to="model", values_to="change") %>%
  filter(grepl("1", species))  %>%
  mutate(management_strategy=ifelse(
    management_strategy=="C. Fishery 1 in \"offshore\" only, effort changes",
    "C. Fishery 1 in \"offshore\" only,\n effort changes",
    management_strategy
  )) %>%
  mutate(variable_out=case_when(
    variable_out=="spawning_biomass" ~ "biomass",
    variable_out=="total_catch" ~ "catch"
  )) %>%
  ggplot(aes(y=model,x=variable_out, fill=qnm_compare)) +
  geom_tile() +
  geom_text(aes(label=change)) +
  facet_grid(cols=vars(operating_model), rows=vars(management_strategy),
             switch='y') +
  geom_hline(aes(yintercept=1.5)) +
  labs(y="",x="") +
  scale_x_discrete(position='bottom') +
  scale_y_discrete(position='right')+
  scale_fill_manual(values=c("goldenrod","#8da0cb","white"), na.value="white",name='QNM is...') +
  guides(fill=guide_legend(nrow=2, position='bottom')) +
  theme_bw() +
  theme(strip.background=element_rect(fill='white'),
        strip.placement = "outside",
        strip.text.x.top=element_text(angle=90),
        strip.text.y.left=element_text(angle=0,hjust=1),
        legend.background=element_rect(fill='gray90'),
        axis.text.x=element_text(angle=90, vjust=0.5, hjust=1),
        panel.spacing.x=unit(0, "lines") , panel.spacing.y=unit(0,"lines"))
  
p2_total


## tabulate "+" and "-" by OM / MS
tos3_fish1 <- tos %>%
  filter(variable_out %in% c("Fishery 1 Catch", "Spawning Biomass")) %>%
  mutate(variable_out=ifelse(variable_out=="Fishery 1 Catch","fish1","spawning_biomass")) %>%
  dplyr::select(operating_model, management_strategy, code, variable_out, qnm_psims, qnm_equiv, mse_prob, species) %>%
  mutate(QNM=case_when(
    (qnm_psims > 0 & qnm_equiv==0) ~ '+',
    (qnm_psims > 0 & qnm_equiv==1) ~ '(+)',
    (qnm_psims < 0 & qnm_equiv==0) ~ '-',
    (qnm_psims < 0 & qnm_equiv==1) ~ '(-)'
  )) %>%
  mutate(MSE=case_when(
    (mse_prob > 0) ~ '+',
    (mse_prob < 0) ~ '-'))

any(is.na(tos3_fish1$MSE))
any(is.na(tos3_fish1$QNM))

tos3_fish1 %<>% 
  mutate(qnm_compare=case_when(
    (QNM==MSE | (QNM=="(+)" & MSE=="+") | (QNM=="(-)" & MSE=="-")) ~ "same", 
    ((QNM=="+" | QNM=="(+)" ) & MSE=="-") ~ "optimistic",
    ((QNM=="-" | QNM=="(-)" ) & MSE=="+") ~ "pessimistic"))

any(is.na(tos3_fish1$qnm_compare))

## create table
p2_fish1 <- tos3_fish1 %>%
  rename(qMSE=MSE) %>%
  dplyr::select(-qnm_psims,-qnm_equiv,-mse_prob) %>%
  pivot_longer(cols=c(QNM,qMSE), names_to="model", values_to="change") %>%
  filter(grepl("1", species))  %>%
  mutate(management_strategy=ifelse(
    management_strategy=="C. Fishery 1 in \"offshore\" only, effort changes",
    "C. Fishery 1 in \"offshore\" only,\n effort changes",
    management_strategy
  )) %>%
  mutate(variable_out=case_when(
    variable_out=="spawning_biomass" ~ "biomass",
    variable_out=="fish1" ~ "catch"
  )) %>%
  ggplot(aes(y=model,x=variable_out, fill=qnm_compare)) +
  geom_tile() +
  geom_text(aes(label=change)) +
  facet_grid(cols=vars(operating_model), rows=vars(management_strategy),
             switch='y') +
  geom_hline(aes(yintercept=1.5)) +
  labs(y="",x="") +
  scale_x_discrete(position='bottom') +
  scale_y_discrete(position='right')+
  scale_fill_manual(values=c("goldenrod","#8da0cb","white"), na.value="white",name='QNM is...') +
  theme_bw() +
  theme(strip.background=element_rect(fill='white'),
        strip.placement = "outside",
        strip.text.x.top=element_text(angle=90),
        strip.text.y.left=element_text(angle=0,hjust=1),
        axis.text.x=element_text(angle=90, vjust=0.5, hjust=1),
        panel.spacing.x=unit(0, "lines") , panel.spacing.y=unit(0,"lines"))

p2_fish1

## tabulate "+" and "-" by OM / MS
tos3_fish2 <- tos %>%
  filter(variable_out %in% c("Fishery 2 Catch", "Spawning Biomass")) %>%
  mutate(variable_out=ifelse(variable_out=="Fishery 2 Catch","fish2","spawning_biomass")) %>%
  dplyr::select(operating_model, management_strategy, code, variable_out, qnm_psims, qnm_equiv, mse_prob, species) %>%
  mutate(QNM=case_when(
    (qnm_psims > 0 & qnm_equiv==0) ~ '+',
    (qnm_psims > 0 & qnm_equiv==1) ~ '(+)',
    (qnm_psims < 0 & qnm_equiv==0) ~ '-',
    (qnm_psims < 0 & qnm_equiv==1) ~ '(-)'
  )) %>%
  mutate(MSE=case_when(
    (mse_prob > 0) ~ '+',
    (mse_prob < 0) ~ '-'))

any(is.na(tos3_fish2$MSE))
any(is.na(tos3_fish2$QNM))

tos3_fish2 %<>% 
  mutate(qnm_compare=case_when(
    (QNM==MSE | (QNM=="(+)" & MSE=="+") | (QNM=="(-)" & MSE=="-")) ~ "same", 
    ((QNM=="+" | QNM=="(+)" ) & MSE=="-") ~ "optimistic",
    ((QNM=="-" | QNM=="(-)" ) & MSE=="+") ~ "pessimistic"))

any(is.na(tos3_fish2$qnm_compare))

## create table
p2_fish2 <- tos3_fish2 %>%
  rename(qMSE=MSE) %>%
  dplyr::select(-qnm_psims,-qnm_equiv,-mse_prob) %>%
  pivot_longer(cols=c(QNM, qMSE), names_to="model", values_to="change") %>%
  filter(grepl("1", species))  %>%
  mutate(management_strategy=ifelse(
    management_strategy=="C. Fishery 1 in \"offshore\" only, effort changes",
    "C. Fishery 1 in \"offshore\" only,\n effort changes",
    management_strategy
  )) %>%
  mutate(variable_out=case_when(
    variable_out=="spawning_biomass" ~ "biomass",
    variable_out=="fish2" ~ "catch"
  )) %>%
  ggplot(aes(y=model,x=variable_out, fill=qnm_compare)) +
  geom_tile() +
  geom_text(aes(label=change)) +
  facet_grid(cols=vars(operating_model), rows=vars(management_strategy),
             switch='y') +
  geom_hline(aes(yintercept=1.5)) +
  labs(y="",x="") +
  scale_x_discrete(position='bottom') +
  scale_y_discrete(position='right')+
  scale_fill_manual(values=c("goldenrod","white"), na.value="white",name='QNM is...') +
  theme_bw() +
  theme(strip.background=element_rect(fill='white'),
        strip.placement = "outside",
        strip.text.x.top=element_text(angle=90),
        strip.text.y.left=element_text(angle=0,hjust=1),
        axis.text.x=element_text(angle=90, vjust=0.5,hjust=1),
        panel.spacing.x=unit(0, "lines") , panel.spacing.y=unit(0,"lines"))
p2_fish2

## FIGURE C3 - ALL TOGETHER
pout <- plot_grid(plotlist=list(
  p2_total + theme(legend.position='bottom', legend.title=element_text(size=10),
                   legend.text=element_text(size=10),legend.justification.bottom = 'left',
                   plot.margin = unit(c(1.7, 0, 0, 1), "lines"),
                   axis.text.y=element_blank()),
  p2_fish1 + theme(legend.position='none',plot.margin = unit(c(1.7, 0, 4.27,0), "lines"),
                   axis.text.y=element_blank(),
                   strip.text.y.left = element_blank()),
  p2_fish2 + theme(legend.position='none',plot.margin = unit(c(1.7, 0, 4.27, 0), "lines"),
                   strip.text.y.left = element_blank())),
  ncol=3, nrow=1, rel_widths=c(1,0.55,0.6))
pout

png(here('doc','supplement','figC5_tradeoffs_expanded_sp1.png'), res=300,height=1500,width=3000)
pout + draw_text(c('(A) Total Catch','(B) Fishery 1 Catch',
                   '(C) Fishery 2 Catch', 'SPECIES 1: '), 
                 x=c(0.265, 0.547, 0.805, 0.13), 
                 y=c(0.95,0.95, 0.95, 0.95))
dev.off()



# FIGURE C3 ----------------------------------------------------------

# Species 2 ---------------------------------------------------------------

## tabulate "+" and "-" by OM / MS
tos_total2 <- tos %>%
  filter(variable_out %in% c("Total Catch", "Spawning Biomass")) %>%
  mutate(variable_out=ifelse(variable_out=="Total Catch","total_catch","spawning_biomass")) %>%
  dplyr::select(operating_model, management_strategy, code, variable_out, qnm_psims, qnm_equiv, mse_prob, species) %>%
  mutate(QNM=case_when(
    (qnm_psims > 0 & qnm_equiv==0) ~ '+',
    (qnm_psims > 0 & qnm_equiv==1) ~ '(+)',
    (qnm_psims < 0 & qnm_equiv==0) ~ '-',
    (qnm_psims < 0 & qnm_equiv==1) ~ '(-)'
  )) %>%
  mutate(MSE=case_when(
    (mse_prob > 0) ~ '+',
    (mse_prob < 0) ~ '-'))

any(is.na(tos_total2$MSE))
any(is.na(tos_total2$QNM))

tos_total2 %<>%
  mutate(qnm_compare=case_when(
    (QNM==MSE | (QNM=="(+)" & MSE=="+") | (QNM=="(-)" & MSE=="-")) ~ "same",
    ((QNM=="+" | QNM=="(+)" ) & MSE=="-") ~ "optimistic",
    ((QNM=="-" | QNM=="(-)" ) & MSE=="+") ~ "pessimistic"))
any(is.na(tos_total2$qnm_compare))

## create table
p2_total <- tos_total2 %>%
  rename(qMSE=MSE) %>%
  dplyr::select(-qnm_psims,-qnm_equiv,-mse_prob) %>%
  pivot_longer(cols=c(QNM, qMSE), names_to="model", values_to="change") %>%
  filter(grepl("2", species))  %>%
  mutate(management_strategy=ifelse(
    management_strategy=="C. Fishery 1 in \"offshore\" only, effort changes",
    "C. Fishery 1 in \"offshore\" only,\n effort changes",
    management_strategy
  )) %>%
  mutate(variable_out=case_when(
    variable_out=="spawning_biomass" ~ "biomass",
    variable_out=="total_catch" ~ "catch"
  )) %>%
  mutate(qnm_compare=factor(qnm_compare,levels=c('optimistic','pessimistic','same'))) %>% 
  ggplot(aes(y=model,x=variable_out, fill=qnm_compare)) +
  geom_tile(aes(fill=qnm_compare), show.legend=c(fill=TRUE)) +
  geom_text(aes(label=change)) +
  facet_grid(cols=vars(operating_model), rows=vars(management_strategy),
             switch='y') +
  geom_hline(aes(yintercept=1.5)) +
  labs(y="",x="") +
  scale_x_discrete(position='bottom') +
  scale_y_discrete(position='right')+
  # scale_fill_manual(values=c("goldenrod","#8da0cb","white"), na.value="white",name='QNM is...') +
  scale_fill_manual(values=c("goldenrod","#8da0cb","white"), na.value="white",
                    name='QNM is...',drop=FALSE) +
  theme_bw() +
  theme(strip.background=element_rect(fill='white'),
        strip.placement = "outside",
        strip.text.x.top=element_text(angle=90),
        strip.text.y.left=element_text(angle=0,hjust=1),
        axis.text.x=element_text(angle=90, vjust=0.5, hjust=1),
        panel.spacing.x=unit(0, "lines") , panel.spacing.y=unit(0,"lines"))
  
p2_total


## tabulate "+" and "-" by OM / MS
tos3_fish1 <- tos %>%
  filter(variable_out %in% c("Fishery 1 Catch", "Spawning Biomass")) %>%
  mutate(variable_out=ifelse(variable_out=="Fishery 1 Catch","fish1","spawning_biomass")) %>%
  dplyr::select(operating_model, management_strategy, code, variable_out, qnm_psims, qnm_equiv, mse_prob, species) %>%
  mutate(QNM=case_when(
    (qnm_psims > 0 & qnm_equiv==0) ~ '+',
    (qnm_psims > 0 & qnm_equiv==1) ~ '(+)',
    (qnm_psims < 0 & qnm_equiv==0) ~ '-',
    (qnm_psims < 0 & qnm_equiv==1) ~ '(-)'
  )) %>%
  mutate(MSE=case_when(
    (mse_prob > 0) ~ '+',
    (mse_prob < 0) ~ '-'))

any(is.na(tos3_fish1$MSE))
any(is.na(tos3_fish1$QNM))

tos3_fish1 %<>%
  mutate(qnm_compare=case_when(
    (QNM==MSE | (QNM=="(+)" & MSE=="+") | (QNM=="(-)" & MSE=="-")) ~ "same",
    ((QNM=="+" | QNM=="(+)" ) & MSE=="-") ~ "optimistic",
    ((QNM=="-" | QNM=="(-)" ) & MSE=="+") ~ "pessimistic"))

any(is.na(tos3_fish1$qnm_compare))

## create table
p2_fish1 <- tos3_fish1 %>%
  rename(qMSE=MSE) %>%
  dplyr::select(-qnm_psims,-qnm_equiv,-mse_prob) %>%
  pivot_longer(cols=c(QNM, qMSE), names_to="model", values_to="change") %>%
  filter(grepl("2", species))  %>%
  mutate(management_strategy=ifelse(
    management_strategy=="C. Fishery 1 in \"offshore\" only, effort changes",
    "C. Fishery 1 in \"offshore\" only,\n effort changes",
    management_strategy
  )) %>%
  mutate(variable_out=case_when(
    variable_out=="spawning_biomass" ~ "biomass",
    variable_out=="fish1" ~ "catch"
  )) %>%
  ggplot(aes(y=model,x=variable_out, fill=qnm_compare)) +
  geom_tile() +
  geom_text(aes(label=change)) +
  facet_grid(cols=vars(operating_model), rows=vars(management_strategy),
             switch='y') +
  geom_hline(aes(yintercept=1.5)) +
  labs(y="",x="") +
  scale_x_discrete(position='bottom') +
  scale_y_discrete(position='right')+
  scale_fill_manual(values=c("goldenrod","white"), na.value="white",name='QNM is...',drop=FALSE) +
  # scale_fill_manual(values=c("goldenrod","#8da0cb","white"), na.value="white",name='QNM is...') +
  theme_bw() +
  theme(strip.background=element_rect(fill='white'),
        strip.placement = "outside",
        strip.text.x.top=element_text(angle=90),
        strip.text.y.left=element_text(angle=0,hjust=1),
        axis.text.x=element_text(angle=90, vjust=0.5, hjust=1),
        panel.spacing.x=unit(0, "lines") , panel.spacing.y=unit(0,"lines"))

p2_fish1

## tabulate "+" and "-" by OM / MS
tos3_fish2 <- tos %>%
  filter(variable_out %in% c("Fishery 2 Catch", "Spawning Biomass")) %>%
  mutate(variable_out=ifelse(variable_out=="Fishery 2 Catch","fish2","spawning_biomass")) %>%
  dplyr::select(operating_model, management_strategy, code, variable_out, qnm_psims, qnm_equiv, mse_prob, species) %>%
  mutate(QNM=case_when(
    (qnm_psims > 0 & qnm_equiv==0) ~ '+',
    (qnm_psims > 0 & qnm_equiv==1) ~ '(+)',
    (qnm_psims < 0 & qnm_equiv==0) ~ '-',
    (qnm_psims < 0 & qnm_equiv==1) ~ '(-)'
  )) %>%
  mutate(MSE=case_when(
    (mse_prob > 0) ~ '+',
    (mse_prob < 0) ~ '-'))

any(is.na(tos3_fish2$MSE))
any(is.na(tos3_fish2$QNM))

tos3_fish2 %<>%
  mutate(qnm_compare=case_when(
    (QNM==MSE | (QNM=="(+)" & MSE=="+") | (QNM=="(-)" & MSE=="-")) ~ "same",
    ((QNM=="+" | QNM=="(+)" ) & MSE=="-") ~ "optimistic",
    ((QNM=="-" | QNM=="(-)" ) & MSE=="+") ~ "pessimistic"))

any(is.na(tos3_fish2$qnm_compare))

## create table
p2_fish2 <- tos3_fish2 %>%
  rename(qMSE=MSE) %>%
  dplyr::select(-qnm_psims,-qnm_equiv,-mse_prob) %>%
  pivot_longer(cols=c(QNM, qMSE), names_to="model", values_to="change") %>%
  filter(grepl("2", species))  %>%
  mutate(management_strategy=ifelse(
    management_strategy=="C. Fishery 1 in \"offshore\" only, effort changes",
    "C. Fishery 1 in \"offshore\" only,\n effort changes",
    management_strategy
  )) %>%
  mutate(variable_out=case_when(
    variable_out=="spawning_biomass" ~ "biomass",
    variable_out=="fish2" ~ "catch"
  )) %>%
  ggplot(aes(y=model,x=variable_out, fill=qnm_compare)) +
  geom_tile() +
  geom_text(aes(label=change)) +
  facet_grid(cols=vars(operating_model), rows=vars(management_strategy),
             switch='y') +
  geom_hline(aes(yintercept=1.5)) +
  labs(y="",x="") +
  scale_x_discrete(position='bottom') +
  scale_y_discrete(position='right')+
  scale_fill_manual(values=c("goldenrod","#8da0cb","white"), na.value="white",name='QNM is...') +
  theme_bw() +
  theme(strip.background=element_rect(fill='white'),
        strip.placement = "outside",
        strip.text.x.top=element_text(angle=90),
        strip.text.y.left=element_text(angle=0,hjust=1),
        axis.text.x=element_text(angle=90, vjust=0.5,hjust=1),
        panel.spacing.x=unit(0, "lines") , panel.spacing.y=unit(0,"lines"))
p2_fish2



## FIGURE C3 - ALL TOGETHER

pout2 <- plot_grid(plotlist=list(
  p2_total + theme(legend.position='none', plot.margin = unit(c(1.7, 0, 0, 1), "lines"),
                   axis.text.y=element_blank()),
  p2_fish1 + theme(legend.position='none',plot.margin = unit(c(1.7, 0, 0,0), "lines"),
                   axis.text.y=element_blank(),
                   strip.text.y.left = element_blank()),
  p2_fish2 + theme(legend.position='none',plot.margin = unit(c(1.7, 0, 0, 0), "lines"),
                   strip.text.y.left = element_blank())),
  ncol=3, nrow=1, rel_widths=c(1,0.6,0.65))

png(here('doc','supplement','figC5_tradeoffs_expanded_sp2.png'), res=300,height=1200,width=3000)
pout2 + draw_text(c('(D) Total Catch','(E) Fishery 1 Catch',
                   '(F) Fishery 2 Catch', 'SPECIES 2: '), 
                 x=c(0.265, 0.529, 0.795, 0.13), 
                 y=c(0.95,0.95, 0.95, 0.95))
dev.off()






##########################################################################
##########################################################################
##########################################################################
##########################################################################
## no arrows version of Fig 7##


# Total catch -------------------------------------------------------------

tos_total <- tos %>%
  filter(variable_out %in% c("Total Catch", "Spawning Biomass")) %>%
  mutate(variable_out=ifelse(variable_out=="Total Catch","total_catch","spawning_biomass")) %>%
  dplyr::select(operating_model, management_strategy, code, variable_out, qnm_psims, species) %>%
  pivot_wider(names_from=variable_out, values_from=qnm_psims) %>%
  ## \u2191 for increasing, \u2193 for decreasing
  mutate(QNM=case_when(
    fishery1 > 0 & spawning_biomass > 0 ~ 'catch increases,\nbiomass increases',
    fishery1 < 0 & spawning_biomass > 0 ~ 'catch decreases,\nbiomass increases',
    fishery1 < 0 & spawning_biomass < 0 ~ 'catch decreases,\nbiomass decreases',
    fishery1 > 0 & spawning_biomass < 0 ~ 'catch increases,\nbiomass decreases'
  )) %>%
  dplyr::select(operating_model, management_strategy, code, species, QNM) %>%
  left_join(
    
    tos %>%
      filter(variable_out %in% c("Total Catch", "Spawning Biomass")) %>%
      mutate(variable_out=ifelse(variable_out=="Total Catch","total_catch","spawning_biomass")) %>%
      dplyr::select(operating_model, management_strategy, code, variable_out, mse_prob, species) %>%
      pivot_wider(names_from=variable_out, values_from=mse_prob) %>%
      mutate(MSE=case_when(
        fishery2 > 0 & spawning_biomass > 0 ~ 'catch increases,\nbiomass increases',
        fishery2 < 0 & spawning_biomass > 0 ~ 'catch decreases,\nbiomass increases',
        fishery2 < 0 & spawning_biomass < 0 ~ 'catch decreases,\nbiomass decreases',
        fishery2 > 0 & spawning_biomass < 0 ~ 'catch increases,\nbiomass decreases'
      ))  %>%
      dplyr::select(operating_model, management_strategy, code, species, MSE)
    
    
  )

pal=pnw_palette("Moth",100)
tos_total %>%
  group_by(species, QNM, MSE) %>%
  summarise(n=length(unique(code))) %>%
  full_join(pairwise_df, by=c("QNM","MSE","species")) %>%
  ggplot(aes(x=fct_relevel(MSE, tradeoffs_vec), y=fct_relevel(QNM), fill=n)) +
  geom_tile() + 
  geom_rect(aes(xmin=c(0.5),xmax=c(1.5),
                ymin=c(3.5),ymax=c(4.5)), fill='transparent',col='black') +
  geom_rect(aes(xmin=1.5, xmax=2.5,
                ymin=2.5, ymax=3.5), fill='transparent',col='black') +
  geom_rect(aes(xmin=2.5, xmax=3.5,
                ymin=1.5, ymax=2.5), fill='transparent',col='black') +
  geom_rect(aes(xmin=3.5, xmax=4.5,
                ymin=0.5, ymax=1.5), fill='transparent',col='black') +
  facet_wrap(~species,strip.position='bottom') +
  labs(x="MSE outcome",y="QNM outcome") +
  scale_x_discrete(position='top') +
  scale_fill_gradientn(colours = rev(pal), na.value='white') +
  theme_bw() +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=20,hjust=0)) -> plot.total

plot.total


# Fishery 1 catch ---------------------------------------------------------

tos_fish1 <- tos %>%
  filter(variable_out %in% c("Fishery 1 Catch", "Spawning Biomass")) %>%
  mutate(variable_out=ifelse(variable_out=="Fishery 1 Catch","fishery1","spawning_biomass")) %>%
  dplyr::select(operating_model, management_strategy, code, variable_out, qnm_psims, species) %>%
  pivot_wider(names_from=variable_out, values_from=qnm_psims) %>%
  mutate(QNM=case_when(
    fishery1 > 0 & spawning_biomass > 0 ~ 'catch increases,\nbiomass increases',
    fishery1 < 0 & spawning_biomass > 0 ~ 'catch decreases,\nbiomass increases',
    fishery1 < 0 & spawning_biomass < 0 ~ 'catch decreases,\nbiomass decreases',
    fishery1 > 0 & spawning_biomass < 0 ~ 'catch increases,\nbiomass decreases'
  )) %>%
  dplyr::select(operating_model, management_strategy, code, species, QNM) %>%
  left_join(
    
    tos %>%
      filter(variable_out %in% c("Fishery 1 Catch", "Spawning Biomass")) %>%
      mutate(variable_out=ifelse(variable_out=="Fishery 1 Catch","fishery1","spawning_biomass")) %>%
      dplyr::select(operating_model, management_strategy, code, variable_out, mse_prob, species) %>%
      pivot_wider(names_from=variable_out, values_from=mse_prob) %>%
      mutate(MSE=case_when(
        fishery2 > 0 & spawning_biomass > 0 ~ 'catch increases,\nbiomass increases',
        fishery2 < 0 & spawning_biomass > 0 ~ 'catch decreases,\nbiomass increases',
        fishery2 < 0 & spawning_biomass < 0 ~ 'catch decreases,\nbiomass decreases',
        fishery2 > 0 & spawning_biomass < 0 ~ 'catch increases,\nbiomass decreases'
      ))  %>%
      dplyr::select(operating_model, management_strategy, code, species, MSE)
    
    
  )

pal=pnw_palette("Moth",100)
tos_fish1 %>%
  group_by(species, QNM, MSE) %>%
  summarise(n=length(unique(code))) %>%
  full_join(pairwise_df, by=c("QNM","MSE","species")) %>%
  ggplot(aes(x=fct_relevel(MSE, tradeoffs_vec), y=fct_relevel(QNM), fill=n)) +
  geom_tile() + 
  geom_rect(aes(xmin=c(0.5),xmax=c(1.5),
                ymin=c(3.5),ymax=c(4.5)), fill='transparent',col='black') +
  geom_rect(aes(xmin=1.5, xmax=2.5,
                ymin=2.5, ymax=3.5), fill='transparent',col='black') +
  geom_rect(aes(xmin=2.5, xmax=3.5,
                ymin=1.5, ymax=2.5), fill='transparent',col='black') +
  geom_rect(aes(xmin=3.5, xmax=4.5,
                ymin=0.5, ymax=1.5), fill='transparent',col='black') +
  facet_wrap(~species) +
  labs(x="MSE outcome",y="QNM outcome") +
  scale_x_discrete(position='top') +
  scale_fill_gradientn(colours = rev(pal), na.value='white') +
  theme_bw() +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=20,hjust=0)) -> plot.fish1

plot.fish1




# Fishery 2 catch ---------------------------------------------------------

tos_fish2 <- tos %>%
  filter(variable_out %in% c("Fishery 2 Catch", "Spawning Biomass")) %>%
  mutate(variable_out=ifelse(variable_out=="Fishery 2 Catch","fishery2","spawning_biomass")) %>%
  dplyr::select(operating_model, management_strategy, code, variable_out, qnm_psims, species) %>%
  pivot_wider(names_from=variable_out, values_from=qnm_psims) %>%
  mutate(QNM=case_when(
    fishery2 > 0 & spawning_biomass > 0 ~ 'catch increases,\nbiomass increases',
    fishery2 < 0 & spawning_biomass > 0 ~ 'catch decreases,\nbiomass increases',
    fishery2 < 0 & spawning_biomass < 0 ~ 'catch decreases,\nbiomass decreases',
    fishery2 > 0 & spawning_biomass < 0 ~ 'catch increases,\nbiomass decreases'
  )) %>%
  dplyr::select(operating_model, management_strategy, code, species, QNM) %>%
  left_join(
    
    tos %>%
      filter(variable_out %in% c("Fishery 2 Catch", "Spawning Biomass")) %>%
      mutate(variable_out=ifelse(variable_out=="Fishery 2 Catch","fishery2","spawning_biomass")) %>%
      dplyr::select(operating_model, management_strategy, code, variable_out, mse_prob, species) %>%
      pivot_wider(names_from=variable_out, values_from=mse_prob) %>%
      mutate(MSE=case_when(
        fishery2 > 0 & spawning_biomass > 0 ~ 'catch increases,\nbiomass increases',
        fishery2 < 0 & spawning_biomass > 0 ~ 'catch decreases,\nbiomass increases',
        fishery2 < 0 & spawning_biomass < 0 ~ 'catch decreases,\nbiomass decreases',
        fishery2 > 0 & spawning_biomass < 0 ~ 'catch increases,\nbiomass decreases'
      ))  %>%
      dplyr::select(operating_model, management_strategy, code, species, MSE)
    
    
  )

pal=pnw_palette("Moth",100)
tos_fish2 %>%
  group_by(QNM, MSE, species) %>%
  summarise(n=length(unique(code))) %>%
  full_join(pairwise_df, by=c("QNM","MSE","species")) %>%
  ggplot(aes(x=fct_relevel(MSE, tradeoffs_vec), y=fct_relevel(QNM), fill=n)) +
  geom_tile() + 
  geom_rect(aes(xmin=c(0.5),xmax=c(1.5),
                ymin=c(3.5),ymax=c(4.5)), fill='transparent',col='black') +
  geom_rect(aes(xmin=1.5, xmax=2.5,
                ymin=2.5, ymax=3.5), fill='transparent',col='black') +
  geom_rect(aes(xmin=2.5, xmax=3.5,
                ymin=1.5, ymax=2.5), fill='transparent',col='black') +
  geom_rect(aes(xmin=3.5, xmax=4.5,
                ymin=0.5, ymax=1.5), fill='transparent',col='black') +
  facet_wrap(~species) +
  labs(x="MSE outcome",y="QNM outcome") +
  scale_x_discrete(position='top') +
  scale_fill_gradientn(colours = rev(pal), na.value='white') +
  theme_bw() +
  theme(legend.title=element_blank(),
        axis.text.x=element_text(angle=20,hjust=0)) -> plot.fish2

plot.fish2
