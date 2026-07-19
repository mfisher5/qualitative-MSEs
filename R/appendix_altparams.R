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




# Figure 7 ----------------------------------------------------------------

## prep df to describe trade-offs
tos <- speciesALL_compare  %>%
  dplyr::select(-model.x) %>%
  filter(mse_param=='BH') %>% dplyr::select(-mse_param) %>%
  rename(qnm_psims=psims, mse_prob=prob) %>%
  mutate(mse_species=case_when(
    mse_species=="Species1" ~ "(sp.1) med longevity", 
    mse_species=="Species2" ~ "(sp.2) short lived")) %>%
  mutate(qnm_equiv=ifelse(abs(qnm_psims) < 60, 1, 0),
         mse_equiv=ifelse(mse_prob < 60 & mse_prob > 40, 1, 0))

## create full pairwise df 
tradeoffs_vec <- c("catch increases,\nbiomass increases","catch increases,\nbiomass decreases","catch decreases,\nbiomass increases", "catch decreases,\nbiomass decreases")
pairwise_df <- data.frame(MSE= rep(tradeoffs_vec,each=4),
                          QNM=rep(tradeoffs_vec, 4))
pairwise_df %<>% 
  mutate(mse_species="(sp.1) med longevity") %>%
  bind_rows(
    pairwise_df %>% mutate(mse_species="(sp.2) short lived")
  )

## create full pairwise df for ARROWS
tradeoffs_vec2 <- c('\u2191 catch,\n\u2191 biomass', 
                    '\u2191 catch,\n\u2193 biomass',
                    '\u2193 catch,\n\u2191 biomass',
                    '\u2193 catch,\n\u2193 biomass')
pairwise_arrows <- data.frame(MSE= rep(tradeoffs_vec2,each=4),
                              QNM=rep(tradeoffs_vec2, 4))
pairwise_arrows %<>% 
  mutate(mse_species="(sp.1) med longevity") %>%
  bind_rows(
    pairwise_arrows %>% mutate(mse_species="(sp.2) short lived")
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
# Fig 7 Total catch: no equiv -------------------------------------------------------------

tos2_total <- tos %>%
  filter(variable_out %in% c("Total Catch", "Spawning Biomass")) %>%
  mutate(variable_out=ifelse(variable_out=="Total Catch","total_catch","spawning_biomass")) %>%
  mutate(qnm_psims=ifelse(qnm_equiv==1, NA, qnm_psims)) %>%
  dplyr::select(operating_model, management_strategy, code, variable_out, qnm_psims, mse_species) %>%
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
  dplyr::select(operating_model, management_strategy, code, mse_species, QNM) %>%
  left_join(
    
    tos %>%
      filter(variable_out %in% c("Total Catch", "Spawning Biomass")) %>%
      mutate(variable_out=ifelse(variable_out=="Total Catch","total_catch","spawning_biomass")) %>%
      ## new
      group_by(mse_species,operating_model, management_strategy, code) %>%
      mutate(tot_mse_equiv=sum(mse_equiv)) %>% ungroup() %>%
      dplyr::select(operating_model, management_strategy, code, variable_out, mse_prob, mse_species, tot_mse_equiv) %>%
      pivot_wider(names_from=variable_out, values_from=mse_prob) %>%
      mutate(MSE=case_when(
        total_catch > 50 & spawning_biomass > 50 ~ '\u2191 catch,\n\u2191 biomass',
        total_catch < 50 & spawning_biomass > 50 ~ '\u2193 catch,\n\u2191 biomass',
        total_catch < 50 & spawning_biomass < 50 ~ '\u2193 catch,\n\u2193 biomass',
        total_catch > 50 & spawning_biomass < 50 ~ '\u2191 catch,\n\u2193 biomass'
      ))  %>%
      dplyr::select(operating_model, management_strategy, code, mse_species, MSE, tot_mse_equiv)
    
    
  )

tradeoffs_vec2_qnm <- tradeoffs_vec2_equiv[
  which((tradeoffs_vec2_equiv %in% tradeoffs_vec2) | (tradeoffs_vec2_equiv %in% unique(tos2_total$QNM)))
]
pal=pnw_palette("Shuksan",100)
to_plot <- tos2_total %>%
  group_by(mse_species, QNM, MSE) %>%
  summarise(n=length(unique(code)),
            mse_equiv=sum(tot_mse_equiv)) %>%
  full_join(pairwise_arrows, by=c("QNM","MSE","mse_species")) %>%
  mutate(n=ifelse(is.na(n), 0, n),
         n_lab=ifelse(n==0,NA,n),
         n_col=ifelse(n>12,'gray90','gray40')) %>%
  mutate(n_lab=ifelse(mse_equiv==1, paste0(n_lab,'*'),paste0(n_lab)))

plot.total.equiv <- ggplot(to_plot, aes(x=fct_relevel(MSE, tradeoffs_vec2), y=fct_relevel(QNM, c(rev(tradeoffs_vec2_qnm))), fill=n)) +
  geom_tile() + 
  geom_text(aes(label=n_lab, fontface='italic'), col=to_plot$n_col) + # col=c(rep('gray40',9),'gray99',rep('gray40',27))) +
  geom_rect(aes(xmin=c(0.5),xmax=c(1.5),
                ymin=c(5.5),ymax=c(6.5)), fill='transparent',col='black') +
  geom_rect(aes(xmin=1.5, xmax=2.5,
                ymin=4.5, ymax=5.5), fill='transparent',col='black') +
  geom_rect(aes(xmin=2.5, xmax=3.5,
                ymin=3.5, ymax=4.5), fill='transparent',col='black') +
  geom_rect(aes(xmin=3.5, xmax=4.5,
                ymin=2.5, ymax=3.5), fill='transparent',col='black') +
  geom_hline(aes(yintercept=2.5)) +
  facet_wrap(~mse_species,strip.position='bottom') +
  labs(x="Quant MSE",y="QNM") +
  scale_x_discrete(position='top') +
  scale_fill_gradientn(colours = rev(pal), na.value='white', limits=c(0,15)) +
  theme_bw() +
  theme(legend.title=element_blank(), strip.text=element_blank()) -> plot.total.equiv

plot.total.equiv


# Fig 7 Fishery 1 catch: no equiv -------------------------------------------------------------

tos2_f1 <- tos %>%
  filter(variable_out %in% c("Fishery 1 Catch", "Spawning Biomass")) %>%
  mutate(variable_out=ifelse(variable_out=="Fishery 1 Catch","total_catch","spawning_biomass")) %>%
  mutate(qnm_psims=ifelse(qnm_equiv==1, NA, qnm_psims)) %>%
  dplyr::select(operating_model, management_strategy, code, variable_out, qnm_psims, mse_species) %>%
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
  dplyr::select(operating_model, management_strategy, code, mse_species, QNM) %>%
  left_join(
    
    tos %>%
      filter(variable_out %in% c("Fishery 1 Catch", "Spawning Biomass")) %>%
      mutate(variable_out=ifelse(variable_out=="Fishery 1 Catch","total_catch","spawning_biomass")) %>%
      ## new
      group_by(mse_species,operating_model, management_strategy, code) %>%
      mutate(tot_mse_equiv=sum(mse_equiv)) %>% ungroup() %>%
      dplyr::select(operating_model, management_strategy, code, variable_out, mse_prob, mse_species, tot_mse_equiv) %>%
      pivot_wider(names_from=variable_out, values_from=mse_prob) %>%
      mutate(MSE=case_when(
        total_catch > 50 & spawning_biomass > 50 ~ '\u2191 catch,\n\u2191 biomass',
        total_catch < 50 & spawning_biomass > 50 ~ '\u2193 catch,\n\u2191 biomass',
        total_catch < 50 & spawning_biomass < 50 ~ '\u2193 catch,\n\u2193 biomass',
        total_catch > 50 & spawning_biomass < 50 ~ '\u2191 catch,\n\u2193 biomass'
      ))  %>%
      dplyr::select(operating_model, management_strategy, code, mse_species, MSE, tot_mse_equiv)
    
    
  )

tradeoffs_vec2_qnm <- tradeoffs_vec2_equiv[
  which((tradeoffs_vec2_equiv %in% tradeoffs_vec2) | (tradeoffs_vec2_equiv %in% unique(tos2_total$QNM)))
]
pal=pnw_palette("Shuksan",100)
to_plot <- tos2_f1 %>%
  group_by(mse_species, QNM, MSE) %>%
  summarise(n=length(unique(code)),
            mse_equiv=sum(tot_mse_equiv)) %>%
  full_join(pairwise_arrows, by=c("QNM","MSE","mse_species")) %>%
  mutate(n=ifelse(is.na(n), 0, n),
         n_lab=ifelse(n==0,NA,n),
         n_col=ifelse(n>12,'gray90','gray40')) %>%
  mutate(n_lab=ifelse(mse_equiv==1, paste0(n_lab,'*'),paste0(n_lab)))

plot.f1.equiv <- ggplot(to_plot, aes(x=fct_relevel(MSE, tradeoffs_vec2), y=fct_relevel(QNM, c(rev(tradeoffs_vec2_qnm))), fill=n)) +
  geom_tile() + 
  geom_text(aes(label=n_lab, fontface='italic'), col=to_plot$n_col) + # col=c(rep('gray40',9),'gray99',rep('gray40',27))) +
  geom_rect(aes(xmin=c(0.5),xmax=c(1.5),
                ymin=c(5.5),ymax=c(6.5)), fill='transparent',col='black') +
  geom_rect(aes(xmin=1.5, xmax=2.5,
                ymin=4.5, ymax=5.5), fill='transparent',col='black') +
  geom_rect(aes(xmin=2.5, xmax=3.5,
                ymin=3.5, ymax=4.5), fill='transparent',col='black') +
  geom_rect(aes(xmin=3.5, xmax=4.5,
                ymin=2.5, ymax=3.5), fill='transparent',col='black') +
  geom_hline(aes(yintercept=2.5)) +
  facet_wrap(~mse_species,strip.position='bottom') +
  labs(x="Quant MSE",y="QNM") +
  scale_x_discrete(position='top') +
  scale_fill_gradientn(colours = rev(pal), na.value='white', limits=c(0,15)) +
  theme_bw() +
  theme(legend.title=element_blank(), strip.text=element_blank())

plot.f1.equiv


# Fig 7 Fishery 1 catch: no equiv -------------------------------------------------------------

tos2_f2 <- tos %>%
  filter(variable_out %in% c("Fishery 2 Catch", "Spawning Biomass")) %>%
  mutate(variable_out=ifelse(variable_out=="Fishery 2 Catch","total_catch","spawning_biomass")) %>%
  mutate(qnm_psims=ifelse(qnm_equiv==1, NA, qnm_psims)) %>%
  dplyr::select(operating_model, management_strategy, code, variable_out, qnm_psims, mse_species) %>%
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
  dplyr::select(operating_model, management_strategy, code, mse_species, QNM) %>%
  left_join(
    
    tos %>%
      filter(variable_out %in% c("Fishery 2 Catch", "Spawning Biomass")) %>%
      mutate(variable_out=ifelse(variable_out=="Fishery 2 Catch","total_catch","spawning_biomass")) %>%
      ## new
      group_by(mse_species,operating_model, management_strategy, code) %>%
      mutate(tot_mse_equiv=sum(mse_equiv)) %>% ungroup() %>%
      dplyr::select(operating_model, management_strategy, code, variable_out, mse_prob, mse_species, tot_mse_equiv) %>%
      pivot_wider(names_from=variable_out, values_from=mse_prob) %>%
      mutate(MSE=case_when(
        total_catch > 50 & spawning_biomass > 50 ~ '\u2191 catch,\n\u2191 biomass',
        total_catch < 50 & spawning_biomass > 50 ~ '\u2193 catch,\n\u2191 biomass',
        total_catch < 50 & spawning_biomass < 50 ~ '\u2193 catch,\n\u2193 biomass',
        total_catch > 50 & spawning_biomass < 50 ~ '\u2191 catch,\n\u2193 biomass'
      ))  %>%
      dplyr::select(operating_model, management_strategy, code, mse_species, MSE, tot_mse_equiv)
    
    
  )

# tradeoffs_vec2_qnm <- tradeoffs_vec2_equiv[
#   which((tradeoffs_vec2_equiv %in% tradeoffs_vec2) | (tradeoffs_vec2_equiv %in% unique(tos2_total$QNM)))
# ]
pal=pnw_palette("Shuksan",100)
to_plot <- tos2_f2 %>%
  group_by(mse_species, QNM, MSE) %>%
  summarise(n=length(unique(code)),
            mse_equiv=sum(tot_mse_equiv)) %>%
  full_join(pairwise_arrows, by=c("QNM","MSE","mse_species")) %>%
  mutate(n=ifelse(is.na(n), 0, n),
         n_lab=ifelse(n==0,NA,n),
         n_col=ifelse(n>12,'gray90','gray40')) %>%
  mutate(n_lab=ifelse(mse_equiv==1, paste0(n_lab,'*'),paste0(n_lab)))

plot.f2.equiv <- ggplot(to_plot, aes(x=fct_relevel(MSE, tradeoffs_vec2), y=fct_relevel(QNM, c(rev(tradeoffs_vec2_qnm))), fill=n)) +
  geom_tile() + 
  geom_text(aes(label=n_lab, fontface='italic'), col=to_plot$n_col) + # col=c(rep('gray40',9),'gray99',rep('gray40',27))) +
  geom_rect(aes(xmin=c(0.5),xmax=c(1.5),
                ymin=c(5.5),ymax=c(6.5)), fill='transparent',col='black') +
  geom_rect(aes(xmin=1.5, xmax=2.5,
                ymin=4.5, ymax=5.5), fill='transparent',col='black') +
  geom_rect(aes(xmin=2.5, xmax=3.5,
                ymin=3.5, ymax=4.5), fill='transparent',col='black') +
  geom_rect(aes(xmin=3.5, xmax=4.5,
                ymin=2.5, ymax=3.5), fill='transparent',col='black') +
  geom_hline(aes(yintercept=2.5)) +
  facet_wrap(~mse_species,strip.position='bottom') +
  labs(x="Quant MSE",y="QNM") +
  scale_x_discrete(position='top') +
  scale_fill_gradientn(colours = rev(pal), na.value='white', limits=c(0,15)) +
  theme_bw() +
  theme(legend.title=element_blank(), strip.text=element_blank())

plot.f2.equiv

# FIGURE 7 ----------------------------------------------------------

png(here('doc','supplement','fig7-tradeoffs-separate-equiv-bh.png'), res=300,height=2600,width=2100)
ggdraw(plot_grid(plotlist=list(
  plot.total.equiv+theme(legend.position='none',plot.margin = unit(c(1, 3.75, 2, 2), "lines"), axis.title=element_blank()),
  plot.f1.equiv+theme(plot.margin = unit(c(0, 0, 2, 2), "lines"), axis.title=element_blank()),
  plot.f2.equiv+theme(legend.position='none',plot.margin = unit(c(0, 3.75, 2, 2), "lines"), axis.title=element_blank())
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
  dplyr::select(operating_model, management_strategy, code, variable_out, qnm_psims, qnm_equiv, mse_prob, mse_species) %>%
  mutate(QNM=case_when(
    (qnm_psims > 0 & qnm_equiv==0) ~ '+',
    (qnm_psims > 0 & qnm_equiv==1) ~ '(+)',
    (qnm_psims < 0 & qnm_equiv==0) ~ '-',
    (qnm_psims < 0 & qnm_equiv==1) ~ '(-)'
  )) %>%
  mutate(MSE=case_when(
    (mse_prob > 60) ~ '+',
    (mse_prob > 50) ~ '(+)',
    (mse_prob < 40) ~ '-',
    (mse_prob < 50) ~ '(-)'))

any(is.na(tos_total2$MSE))
any(is.na(tos_total2$QNM))

tos_total2 %<>% 
  mutate(qnm_compare=case_when(
    (QNM==MSE | (QNM=="(+)" & MSE=="+") | (QNM=="+" & MSE=="(+)") | (QNM=="(-)" & MSE=="-")) ~ "same", 
    ((QNM=="+" | QNM=="(+)" ) & (MSE=="-"  | MSE=="(-)")) ~ "optimistic",
    ((QNM=="-" | QNM=="(-)" ) & (MSE=="+"  | MSE=="(+)")) ~ "pessimistic"))
any(is.na(tos_total2$qnm_compare))

## create table
p2_total <- tos_total2 %>%
  dplyr::select(-qnm_psims,-qnm_equiv,-mse_prob) %>%
  rename(qMSE=MSE) %>%
  pivot_longer(cols=c(QNM, qMSE), names_to="model", values_to="change") %>%
  filter(grepl("1", mse_species))  %>%
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
  dplyr::select(operating_model, management_strategy, code, variable_out, qnm_psims, qnm_equiv, mse_prob, mse_species) %>%
  mutate(QNM=case_when(
    (qnm_psims > 0 & qnm_equiv==0) ~ '+',
    (qnm_psims > 0 & qnm_equiv==1) ~ '(+)',
    (qnm_psims < 0 & qnm_equiv==0) ~ '-',
    (qnm_psims < 0 & qnm_equiv==1) ~ '(-)'
  )) %>%
  mutate(MSE=case_when(
    (mse_prob > 60) ~ '+',
    (mse_prob > 50) ~ '(+)',
    (mse_prob < 40) ~ '-',
    (mse_prob < 50) ~ '(-)'))

any(is.na(tos3_fish1$MSE))
any(is.na(tos3_fish1$QNM))

tos3_fish1 %<>% 
  mutate(qnm_compare=case_when(
    (QNM==MSE | (QNM=="(+)" & MSE=="+") | (QNM=="+" & MSE=="(+)") | (QNM=="(-)" & MSE=="-")) ~ "same", 
    ((QNM=="+" | QNM=="(+)" ) & (MSE=="-"  | MSE=="(-)")) ~ "optimistic",
    ((QNM=="-" | QNM=="(-)" ) & (MSE=="+"  | MSE=="(+)")) ~ "pessimistic"))

any(is.na(tos3_fish1$qnm_compare))

## create table
p2_fish1 <- tos3_fish1 %>%
  rename(qMSE=MSE) %>%
  dplyr::select(-qnm_psims,-qnm_equiv,-mse_prob) %>%
  pivot_longer(cols=c(QNM,qMSE), names_to="model", values_to="change") %>%
  filter(grepl("1", mse_species))  %>%
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
  dplyr::select(operating_model, management_strategy, code, variable_out, qnm_psims, qnm_equiv, mse_prob, mse_species) %>%
  mutate(QNM=case_when(
    (qnm_psims > 0 & qnm_equiv==0) ~ '+',
    (qnm_psims > 0 & qnm_equiv==1) ~ '(+)',
    (qnm_psims < 0 & qnm_equiv==0) ~ '-',
    (qnm_psims < 0 & qnm_equiv==1) ~ '(-)'
  )) %>%
  mutate(MSE=case_when(
    (mse_prob > 60) ~ '+',
    (mse_prob > 50) ~ '(+)',
    (mse_prob < 40) ~ '-',
    (mse_prob < 50) ~ '(-)'))

any(is.na(tos3_fish2$MSE))
any(is.na(tos3_fish2$QNM))

tos3_fish2 %<>% 
  mutate(qnm_compare=case_when(
    (QNM==MSE | (QNM=="(+)" & MSE=="+") | (QNM=="+" & MSE=="(+)") | (QNM=="(-)" & MSE=="-")) ~ "same", 
    ((QNM=="+" | QNM=="(+)" ) & (MSE=="-"  | MSE=="(-)")) ~ "optimistic",
    ((QNM=="-" | QNM=="(-)" ) & (MSE=="+"  | MSE=="(+)")) ~ "pessimistic"))

any(is.na(tos3_fish2$qnm_compare))

## create table
p2_fish2 <- tos3_fish2 %>%
  rename(qMSE=MSE) %>%
  dplyr::select(-qnm_psims,-qnm_equiv,-mse_prob) %>%
  pivot_longer(cols=c(QNM, qMSE), names_to="model", values_to="change") %>%
  filter(grepl("1", mse_species))  %>%
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

png(here('doc','supplement','figC5_BH_tradeoffs_expanded_sp1.png'), res=300,height=1500,width=3000)
pout + draw_text(c('(A) Total Catch','(B) Fishery 1 Catch',
                   '(C) Fishery 2 Catch', 'SPECIES 1: '), 
                 x=c(0.265, 0.547, 0.805, 0.13), 
                 y=c(0.95,0.95, 0.95, 0.95))
dev.off()


# Species 2 ---------------------------------------------------------------

## create table
p2s2_total <- tos_total2 %>%
  dplyr::select(-qnm_psims,-qnm_equiv,-mse_prob) %>%
  rename(qMSE=MSE) %>%
  pivot_longer(cols=c(QNM, qMSE), names_to="model", values_to="change") %>%
  filter(grepl("2", mse_species))  %>%
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

p2s2_total



## create table
p2s2_fish1 <- tos3_fish1 %>%
  rename(qMSE=MSE) %>%
  dplyr::select(-qnm_psims,-qnm_equiv,-mse_prob) %>%
  pivot_longer(cols=c(QNM,qMSE), names_to="model", values_to="change") %>%
  filter(grepl("2", mse_species))  %>%
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
  scale_fill_manual(values=c("goldenrod","white"), na.value="white",name='QNM is...') +
  theme_bw() +
  theme(strip.background=element_rect(fill='white'),
        strip.placement = "outside",
        strip.text.x.top=element_text(angle=90),
        strip.text.y.left=element_text(angle=0,hjust=1),
        axis.text.x=element_text(angle=90, vjust=0.5, hjust=1),
        panel.spacing.x=unit(0, "lines") , panel.spacing.y=unit(0,"lines"))

p2s2_fish1

## create table
p2s2_fish2 <- tos3_fish2 %>%
  rename(qMSE=MSE) %>%
  dplyr::select(-qnm_psims,-qnm_equiv,-mse_prob) %>%
  pivot_longer(cols=c(QNM, qMSE), names_to="model", values_to="change") %>%
  filter(grepl("2", mse_species))  %>%
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
p2s2_fish2

## FIGURE C3 - ALL TOGETHER
pouts2 <- plot_grid(plotlist=list(
  p2s2_total + theme(legend.position='bottom', legend.title=element_text(size=10),
                   legend.text=element_text(size=10),legend.justification.bottom = 'left',
                   plot.margin = unit(c(1.7, 0, 0, 1), "lines"),
                   axis.text.y=element_blank()),
  p2s2_fish1 + theme(legend.position='none',plot.margin = unit(c(1.7, 0, 4.27,0), "lines"),
                   axis.text.y=element_blank(),
                   strip.text.y.left = element_blank()),
  p2s2_fish2 + theme(legend.position='none',plot.margin = unit(c(1.7, 0, 4.27, 0), "lines"),
                   strip.text.y.left = element_blank())),
  ncol=3, nrow=1, rel_widths=c(1,0.55,0.6))
pouts2

png(here('doc','supplement','figC5_BH_tradeoffs_expanded_sp2.png'), res=300,height=1500,width=3000)
pouts2 + draw_text(c('(A) Total Catch','(B) Fishery 1 Catch',
                   '(C) Fishery 2 Catch', 'SPECIES 2: '), 
                 x=c(0.265, 0.547, 0.805, 0.13), 
                 y=c(0.95,0.95, 0.95, 0.95))
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



