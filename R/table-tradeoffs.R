########## Create Table X ###########
#
# Table X summarizes trade-offs in catch, biomass  
#    comparing QNM v MSE output 
#    Data required for this script are created in "03_qnm_mse_compare.R"
#
# Written 2024-12-27 M Fisher
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

## create full pairwise df 
tradeoffs_vec <- c("catch increases,\nbiomass increases","catch increases,\nbiomass decreases","catch decreases,\nbiomass increases", "catch decreases,\nbiomass decreases")
pairwise_df <- data.frame(MSE= c(rep("catch decreases,\nbiomass increases",4), 
  rep("catch increases,\nbiomass increases",4),
  rep("catch increases,\nbiomass decreases",4),
  rep("catch decreases,\nbiomass decreases",4)),
  QNM=rep(tradeoffs_vec, 4))
pairwise_df %<>% 
  mutate(species="(sp.1) med longevity") %>%
  bind_rows(
    pairwise_df %>% mutate(species="(sp.2) short longevity")
  )

# Write to excel to manually code -----------------------------------------

tos %>%
  filter(variable_out %in% c("Total Catch", "Spawning Biomass")) %>%
  mutate(variable_out=case_when(
    variable_out == "Total Catch" ~ "Total Catch QNM",
    variable_out == "Spawning Biomass" ~ "Spawning Biomass QNM"
  )) %>%
  dplyr::select(operating_model, management_strategy, code, variable_out, qnm_psims, species) %>%
  pivot_wider(names_from=variable_out, values_from=qnm_psims) %>%
  left_join(
    
    tos %>%
      filter(variable_out %in% c("Total Catch", "Spawning Biomass")) %>%
      mutate(variable_out=case_when(
        variable_out == "Total Catch" ~ "Total Catch MSE",
        variable_out == "Spawning Biomass" ~ "Spawning Biomass MSE"
      )) %>%
      dplyr::select(operating_model, management_strategy, code, variable_out, mse_prob, species) %>%
      pivot_wider(names_from=variable_out, values_from=mse_prob) 
  ) %>%
  write_csv(here('data','results',paste0('mseVqnm_tradeoffs_species1-2_allHCRallOM_',sim.date,'.csv')))




# Total catch -------------------------------------------------------------
tos <- speciesALL_compare  %>%
  dplyr::select(-model.x) %>%
  rename(qnm_psims=psims, mse_prob=prob, species=model.y) %>%
  mutate(species=case_when(
    species=="MSE-Species1" ~ "(sp.1) med longevity", 
    species=="MSE-Species2" ~ "(sp.2) short longevity")) %>%
  mutate(qnm_equiv=ifelse(abs(qnm_psims) < 60, 1, 0))

tos_total <- tos %>%
  filter(variable_out %in% c("Total Catch", "Spawning Biomass")) %>%
  mutate(variable_out=ifelse(variable_out=="Total Catch","total_catch","spawning_biomass")) %>%
  dplyr::select(operating_model, management_strategy, code, variable_out, qnm_psims, species) %>%
  pivot_wider(names_from=variable_out, values_from=qnm_psims) %>%
  mutate(QNM=case_when(
    total_catch > 0 & spawning_biomass > 0 ~ 'catch increases,\nbiomass increases',
    total_catch < 0 & spawning_biomass > 0 ~ 'catch decreases,\nbiomass increases',
    total_catch < 0 & spawning_biomass < 0 ~ 'catch decreases,\nbiomass decreases',
    total_catch > 0 & spawning_biomass < 0 ~ 'catch increases,\nbiomass decreases'
  )) %>%
  dplyr::select(operating_model, management_strategy, code, species, QNM) %>%
  left_join(
    
    tos %>%
      filter(variable_out %in% c("Total Catch", "Spawning Biomass")) %>%
      mutate(variable_out=ifelse(variable_out=="Total Catch","total_catch","spawning_biomass")) %>%
      dplyr::select(operating_model, management_strategy, code, variable_out, mse_prob, species) %>%
      pivot_wider(names_from=variable_out, values_from=mse_prob) %>%
      mutate(MSE=case_when(
        total_catch > 0 & spawning_biomass > 0 ~ 'catch increases,\nbiomass increases',
        total_catch < 0 & spawning_biomass > 0 ~ 'catch decreases,\nbiomass increases',
        total_catch < 0 & spawning_biomass < 0 ~ 'catch decreases,\nbiomass decreases',
        total_catch > 0 & spawning_biomass < 0 ~ 'catch increases,\nbiomass decreases'
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
  facet_wrap(~species) +
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
        fishery1 > 0 & spawning_biomass > 0 ~ 'catch increases,\nbiomass increases',
        fishery1 < 0 & spawning_biomass > 0 ~ 'catch decreases,\nbiomass increases',
        fishery1 < 0 & spawning_biomass < 0 ~ 'catch decreases,\nbiomass decreases',
        fishery1 > 0 & spawning_biomass < 0 ~ 'catch increases,\nbiomass decreases'
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



# bring together ----------------------------------------------------------


plot_grid(plotlist=list(
  plot.total + ggtitle("(A) Total catch"),
  plot.fish1 + ggtitle("(B) Fishery 1 catch"),
  plot.fish2 + ggtitle("(C) Fishery 2 catch")
), ncol=1, nrow=3)



png(here('data','results','fig8-tradeoffs-no-equiv.png'), res=300,height=3200,width=2800)
plot_grid(plotlist=list(
  plot.total + ggtitle("(A) Total catch"),
  plot.fish1 + ggtitle("(B) Fishery 1 catch"),
  plot.fish2 + ggtitle("(C) Fishery 2 catch")
), ncol=1, nrow=3)
dev.off()