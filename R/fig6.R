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

## Functions
library(here)
library(tidyverse)
library(magrittr)
library(ggplot2)
library(QPress)
library(janitor)


# Data --------------------------------------------------------------------

speciesALL_compare <- readRDS(here('data','results','mse-probs_V_qnm-props_Species12_allHCRallMS_202402.rds'))



# Data formatting ---------------------------------------------------------

## fix variable names for legend
speciesALL_compare %<>% 
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
  geom_text(data=compare_counts,aes(x=x,y=y,label=n, col=match), 
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
       subtitle="by managment strategy") +
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


