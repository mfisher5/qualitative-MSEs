########### Create Figure 6 ###########
#
# Fig 6 compares the MSE output "probability increase / decrease"  
#    to the QNM output "proportion of matrices" 
#    Data required for this script are created in "03_qnm_mse_compare.R"
#
# Written 2024-02-06 M Fisher
# Last Updated: 2024-07-30 mcf
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


# Data --------------------------------------------------------------------

speciesALL_compare <- read_csv(here('data','results',paste0('mseVqnm_prob-psim_species1-2_allHCRallOM_',sim.date,'.csv')))



# Data formatting ---------------------------------------------------------

## fix variable names for legend
speciesALL_compare %<>% 
  filter(code != "2-A") %>%     # QNM 2 HCR A doesn't have a perturbation
  mutate(variable_out=fct_recode(variable_out,"Fishery 1 Catch"="fishery_1_catch",
                                 "Fishery 2 Catch"="fishery_2_catch",
                                 "Total Catch"="total_catch",
                                 "Spawning Biomass"="spawning_biomass"))


## count the number of times there is a mismatch for plot 1, create dataframe for plotting
compare_counts_om <- speciesALL_compare  %>%
  filter(code != "2-A") %>%
  mutate(match=ifelse((psims < 0 & prob < 0) | (psims > 0 & prob > 0), "match",
                      ifelse(psims < 0 & prob > 0, "negQNM--posMSE",
                             ifelse(psims > 0 & prob < 0, "posQNM--negMSE", "other")))) %>%
  group_by(operating_model, model.y, match) %>%
  summarise(n=n()) %>%
  filter(!(match %in% c("match","other")))
compare_counts_om %<>% mutate(y=ifelse(match=="negQNM--posMSE", -40, 40),
                              x=ifelse(match=="negQNM--posMSE", 95, -95)) %>%
  filter(n>1) %>%
  mutate(n=paste0("n=",n))


## count the number of times there is a mismatch for plot2, create dataframe for plotting
compare_counts_ms <- speciesALL_compare  %>%
  filter(code != "2-A") %>%
  mutate(match=ifelse((psims < 0 & prob < 0) | (psims > 0 & prob > 0), "match",
                      ifelse(psims < 0 & prob > 0, "negQNM--posMSE",
                             ifelse(psims > 0 & prob < 0, "posQNM--negMSE", "other")))) %>%
  group_by(management_strategy, model.y, match) %>%
  summarise(n=n()) %>%
  filter(!(match %in% c("match","other")))
compare_counts_ms %<>% mutate(y=ifelse(match=="negQNM--posMSE", -40, 40),
                              x=ifelse(match=="negQNM--posMSE", 95, -95)) %>%
  filter(n>1) %>%
  mutate(n=paste0("n=",n))



# FIGURE -------------------------------------

## by om
p1 <- ggplot(data=speciesALL_compare, aes(x=prob, y=psims, 
                                          pch=fct_relevel(variable_out, 
                                                          c("Total Catch","Fishery 1 Catch","Fishery 2 Catch", "Spawning Biomass")))) +
  geom_point(size=2) +
  geom_text(data=compare_counts_om,aes(x=x,y=y,label=n, col=match), 
            size=3.5, inherit.aes=FALSE) +
  geom_hline(aes(yintercept=60), col="orange", lty=2) +
  geom_hline(aes(yintercept=80), col="darkorange", lty=2) +
  geom_hline(aes(yintercept=-60), col="lightblue", lty=2) +
  geom_hline(aes(yintercept=-80), col="darkblue", lty=2) +
  facet_grid(cols=vars(operating_model), rows=vars(model.y)) +
  scale_shape_manual(values=c(0,1,2,4), name="Output Metric") +
  scale_color_manual(values=c("lightblue3","orange")) +
  xlim(c(-110, 110)) +
  guides(color='none') +
  labs(x="MSE: Probability of Increase/Decrease",y="QNM: Proportion Simulations with Increase/Decrease",
       subtitle="by operating model scenario") +
  theme_bw() + theme(legend.title=element_blank())


# by ms
p2 <- ggplot(data=speciesALL_compare, aes(x=prob, y=psims, 
                                          pch=fct_relevel(variable_out, 
                                                          c("Total Catch","Fishery 1 Catch","Fishery 2 Catch", "Spawning Biomass")))) +
  geom_point(size=2.5) +
  geom_text(data=compare_counts_ms,aes(x=x,y=y,label=n, col=match), 
            size=4, inherit.aes=FALSE) +
  geom_hline(aes(yintercept=60), col="orange", lty=2) +
  geom_hline(aes(yintercept=80), col="darkorange", lty=2) +
  geom_hline(aes(yintercept=-60), col="lightblue", lty=2) +
  geom_hline(aes(yintercept=-80), col="darkblue", lty=2) +
  facet_grid(cols=vars(management_strategy), rows=vars(model.y)) +
  scale_shape_manual(values=c(0,1,2,4), name="Output Metric") +
  scale_color_manual(values=c("lightblue3","orange")) +
  xlim(c(-110, 110)) +
  guides(color='none') +
  labs(x="MSE: Probability of Increase/Decrease",y="QNM: Proportion Simulations with Increase/Decrease",
       subtitle="by management strategy") +
  theme_bw() + theme(legend.title=element_blank())


# plot separately
png(here('data','results','Fig6a.png'), res=300, height=1300,width=2800)
p1
dev.off()


png(here('data','results','Fig6b.png'), res=300, height=1300,width=2800)
p2
dev.off()


# plot together
png(here('data','results','Fig6.png'), res=300, height=2500,width=3500)
plot_grid(p1,p2,nrow=2)
dev.off()





# testing -----------------------------------------------------------------

speciesALL_compare  %>%
  filter(code != "2-A") %>%
  mutate(equiv=ifelse(abs(psims)<60, 'y','n')) %>%
  mutate(match=ifelse((psims < 0 & prob < 0) | (psims > 0 & prob > 0), "match","mismatch")) %>%
  mutate(match_is_equiv=ifelse(match=='match' & equiv=='y','match, qnm equiv',
                               ifelse(match=='mismatch'& equiv=='y','mismatch, qnm equiv',match))) %>%
  mutate(match_is_equiv=fct_relevel(match_is_equiv, c('match','match, qnm equiv','mismatch','mismatch, qnm equiv'))) %>%
  mutate(variable_out=fct_relevel(variable_out, c("Total Catch", "Fishery 1 Catch", "Fishery 2 Catch", "Spawning Biomass"))) %>%
  mutate(management_strategy=fct_rev(management_strategy)) %>%
  mutate(model.y=case_when(model.y=="MSE-Species1"~'(sp.1) med longevity',.default='(sp.2) short longevity')) %>%
  ggplot(aes(x=operating_model,y=management_strategy)) +
  geom_tile(aes(fill=match_is_equiv, alpha=match_is_equiv)) +
  facet_grid(cols=vars(variable_out), rows=vars(model.y)) +
  labs(x='Operating model scenario',y='Management strategy') +
  scale_fill_manual(values=c('white','gray70','darkred', 'darkred')) +
  scale_alpha_manual(values=c(0,0.5,1,0.5)) +
  theme_bw() + theme(axis.text.x=element_text(angle=45,hjust=1, vjust=1.05),
                     legend.title=element_blank())



png(here('data','results','Fig6c.png'), res=300, height=1100,width=2400)
speciesALL_compare  %>%
  filter(code != "2-A") %>%
  mutate(match=ifelse((psims < 0 & prob < 0) | (psims > 0 & prob > 0), "match","mismatch")) %>%
  mutate(variable_out=fct_relevel(variable_out, c("Total Catch", "Fishery 1 Catch", "Fishery 2 Catch", "Spawning Biomass"))) %>%
  mutate(management_strategy=fct_rev(management_strategy)) %>%
  mutate(model.y=case_when(model.y=="MSE-Species1"~'(1) medium long.',.default='(2) short long.')) %>%
  ggplot(aes(x=operating_model,y=management_strategy)) +
  geom_tile(aes(fill=match, alpha=match)) +
  facet_grid(cols=vars(variable_out), rows=vars(model.y)) +
  labs(x='Operating model scenario',y='Management strategy') +
  scale_fill_manual(values=c('white','darkred')) +
  scale_alpha_manual(values=c(0,1)) +
  theme_bw() + theme(axis.text.x=element_text(angle=45,hjust=1, vjust=1.05),
                     legend.title=element_blank())
dev.off()





png(here('data','results','Fig6c-equiv.png'), res=300, height=1300,width=2600)
speciesALL_compare  %>%
  filter(code != "2-A") %>%
  mutate(equiv=ifelse(abs(psims)<60, 'y','n')) %>%
  mutate(match=ifelse((psims < 0 & prob < 0) | (psims > 0 & prob > 0), "match","mismatch")) %>%
  mutate(match_is_equiv=ifelse(match=='match' & equiv=='y','match, qnm equiv',
                               ifelse(match=='mismatch'& equiv=='y','mismatch, qnm equiv',match))) %>%
  mutate(match_is_equiv=fct_relevel(match_is_equiv, c('match','match, qnm equiv','mismatch','mismatch, qnm equiv'))) %>%
  mutate(variable_out=fct_relevel(variable_out, c("Total Catch", "Fishery 1 Catch", "Fishery 2 Catch", "Spawning Biomass"))) %>%
  mutate(management_strategy=fct_rev(management_strategy)) %>%
  mutate(model.y=case_when(model.y=="MSE-Species1"~'(sp.1) med longevity',.default='(sp.2) short longevity')) %>%
  ggplot(aes(x=operating_model,y=management_strategy)) +
  geom_tile(aes(fill=match_is_equiv, alpha=match_is_equiv)) +
  facet_grid(cols=vars(variable_out), rows=vars(model.y)) +
  labs(x='Operating model scenario',y='Management strategy') +
  scale_fill_manual(values=c('white','gray70','darkred', 'darkred')) +
  scale_alpha_manual(values=c(0,0.5,1,0.5)) +
  theme_bw() + theme(axis.text.x=element_text(angle=45,hjust=1, vjust=1.05),
                     legend.title=element_blank())
dev.off()



new_table2_counts <- speciesALL_compare  %>%
  filter(code != "2-A") %>% dplyr::select(-model.x, -operating_model, -management_strategy) %>%
  mutate(match=ifelse((psims < 0 & prob < 0) | (psims > 0 & prob > 0), "match","mismatch")) %>%
 group_by(model.y, variable_out) %>% mutate(compare_total=length(unique(code))) %>%
  ungroup() %>% dplyr::select(-psims, -prob) %>%
  group_by(model.y, variable_out, match, compare_total) %>% 
  summarise(n_compare=length(unique(code))) %>%
  mutate(p_compare=n_compare/compare_total)




new_table2_mismatch_counts <- speciesALL_compare  %>%
  filter(code != "2-A") %>% dplyr::select(-model.x, -operating_model, -management_strategy) %>%
  mutate(equiv=ifelse(abs(psims)<60, 'y','n')) %>%
  mutate(match=ifelse((psims < 0 & prob < 0) | (psims > 0 & prob > 0), "match",
                      ifelse(equiv=='y',"mismatch, equiv", "mismatch"))) %>%
  group_by(model.y,variable_out) %>% mutate(compare_total=length(unique(code))) %>%
  ungroup() %>% dplyr::select(-equiv, -prob, -psims) %>%
  group_by(model.y, variable_out, match, compare_total) %>% 
  summarise(n_compare=length(unique(code))) %>%
  mutate(p_compare=n_compare/compare_total)
  



