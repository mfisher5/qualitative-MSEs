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
##

## make table of false positive frequencies
table_c4 <- triage_df %>% 
  group_by(management_strategy,species) %>%
  mutate(total_n=length(unique(operating_model))) %>%
  ungroup() %>%
  filter(triage=="false positive" | triage_noEquiv == "false positive") %>%
  pivot_longer(cols=c(triage,triage_noEquiv), names_to='triage_type',values_to='decision') %>%
  group_by(management_strategy, variable_out,species, total_n, triage_type, decision) %>% summarise(n=n()) %>%
  mutate(p=n/total_n) %>% dplyr::select(-n) %>%
  pivot_wider(names_from=variable_out,values_from=p) %>%
  arrange(species,management_strategy)

View(filter(table_c4, grepl('1', species)))
View(filter(table_c4, grepl('2', species)))




# summary scatter change equiv cutoff -------------------------------------

## calculate false positives / negatives when cutoff is 0.51, 0.55, 0.6, 0.65, 0.70, 0.75, 0.80

make_triage_df <- function(x, cutoff){
  out <- x  %>%
    mutate(qnm_equiv=ifelse(abs(qnm_psims) < cutoff, 1, 0)) %>%
    mutate(triage0=ifelse(qnm_psims < 0 & mse_prob < 0, 'true negative',
                          ifelse(qnm_psims > 0 & mse_prob > 0, 'true positive',
                                 ifelse(qnm_psims > 0 & mse_prob < 0, 'false positive',
                                        ifelse(qnm_psims < 0 & mse_prob > 0, 'false negative', NA))))) %>%
    mutate(triage=ifelse(qnm_equiv==1 & mse_prob < 0, 'false positive', 
                         ifelse(qnm_equiv==1 & mse_prob > 0, 'true positive',
                                ifelse(qnm_equiv==0, triage0, NA)))) %>%
    dplyr::select(-triage0, -qnm_equiv)
  return(out)
}


speciesALL_compare_format <- speciesALL_compare %>%
  dplyr::select(-model.x) %>%
  rename(qnm_psims=psims, mse_prob=prob, species=model.y) %>%
  mutate(species=case_when(
    species=="MSE-Species1" ~ "(sp.1) med longevity", 
    species=="MSE-Species2" ~ "(sp.2) short longevity"))

t_Step <- make_triage_df(x=speciesALL_compare_format,cutoff=51) %>%
  rename(p51=triage) %>%
  left_join(
    make_triage_df(x=speciesALL_compare_format,cutoff=55) %>%
      rename(p55=triage)
  ) %>%
  left_join(
    make_triage_df(x=speciesALL_compare_format,cutoff=60) %>%
      rename(p60=triage)
  ) %>%
  left_join(
    make_triage_df(x=speciesALL_compare_format,cutoff=65) %>%
      rename(p65=triage)
  ) %>%
  left_join(
    make_triage_df(x=speciesALL_compare_format,cutoff=70) %>%
      rename(p70=triage)
  ) %>%
  left_join(
    make_triage_df(x=speciesALL_compare_format,cutoff=75) %>%
      rename(p75=triage)
  ) %>%
  left_join(
    make_triage_df(x=speciesALL_compare_format,cutoff=80) %>%
      rename(p80=triage)
  ) %>%
  left_join(
    make_triage_df(x=speciesALL_compare_format,cutoff=85) %>%
      rename(p85=triage)
  ) %>%
  left_join(
    make_triage_df(x=speciesALL_compare_format,cutoff=90) %>%
      rename(p90=triage)
  )  %>%
  left_join(
    make_triage_df(x=speciesALL_compare_format,cutoff=95) %>%
      rename(p95=triage)
  )  %>%
  left_join(
    make_triage_df(x=speciesALL_compare_format,cutoff=99) %>%
      rename(p99=triage)
  )

total_comparisons <- length(unique(t_Step$code))
colnames(t_Step)

png(here('data','results','Fig8.png'), res=300, height=1100,width=1900)
t_Step %>%
  pivot_longer(cols=8:18, names_to="cutoff", values_to="triage") %>%
  mutate(cutoff=substr(cutoff,2,3)) %>% 
  mutate(total_n=length(unique(t_Step$code))) %>%
  group_by(species,triage,cutoff, total_n) %>% summarize(n=length(unique(code))) %>%
  ## make zeros to fill empty false negative entries
  pivot_wider(names_from=triage, values_from=n, values_fill=0) %>% 
  pivot_longer(cols=c(`true positive`, `true negative`, `false positive`, `false negative`), names_to='triage', values_to='n') %>%
  mutate(p=n/total_n) %>%
  filter(triage %in% c('false negative','false positive')) %>%
  ggplot(aes(x=cutoff,y=p, col=species, pch=species, group=species)) +
  geom_point() + 
  geom_path() +
  facet_wrap(~triage) + 
  labs(x='Minimum sign consistency (%)',y='Prop. OM scenarios x Management strategies') +
  theme_bw()
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





############## show only matches and mismatches in red scale ############## 
# FIGURE: strong/weak -------------------------------------

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
                              x=ifelse(match=="negQNM--posMSE", 90, -90)) %>%
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
compare_counts_ms %<>% mutate(y=ifelse(match=="negQNM--posMSE", -30, 40),
                              x=ifelse(match=="negQNM--posMSE", 88, -88)) %>%
  filter(n>1) %>%
  mutate(n=paste0("n=",n))

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
  labs(x="MSE: Probability of Increase/Decrease",y="QNM: Proportion Matrices with Increase/Decrease",
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



# FIGURE: equivocal only --------------------------------------------------

## count the number of times there is a mismatch for plot 1, create dataframe for plotting
compare_counts_om <- speciesALL_compare  %>%
  filter(code != "2-A") %>%
  mutate(match=ifelse((psims < 0 & prob < 0) | (psims > 0 & prob > 0), "match",
                      ifelse(psims < 0 & prob > 0, "negQNM--posMSE",
                             ifelse(psims > 0 & prob < 0, "posQNM--negMSE", "other")))) %>%
  group_by(operating_model, model.y, match) %>%
  summarise(n=n()) %>%
  filter(!(match %in% c("match","other")))
compare_counts_om %<>% mutate(y=ifelse(match=="negQNM--posMSE", -30, 40),
                              x=ifelse(match=="negQNM--posMSE", 90, -90)) %>%
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
compare_counts_ms %<>% mutate(y=ifelse(match=="negQNM--posMSE", -30, 40),
                              x=ifelse(match=="negQNM--posMSE", 88, -88)) %>%
  filter(n>1) %>%
  mutate(n=paste0("n=",n))

compare_counts_ms %<>% mutate(management_strategy=case_when(
  management_strategy=='C. Fishery 1 in "offshore" only, effort changes'~'C. Fishery 1 in "offshore"\nonly, effort changes',
  management_strategy=='E. Fishery 1 in "offshore" only'~'E. Fishery 1 in "offshore"\n only',
  management_strategy=='D. Increase fishery mesh size'~'D. Increase fishery\n mesh size',
  .default=management_strategy))


## by om
p1 <- speciesALL_compare %>%
  mutate(variable_out=fct_relevel(variable_out,c("Total Catch","Fishery 1 Catch","Fishery 2 Catch", "Spawning Biomass"))) %>%
  ggplot(aes(x=prob, y=psims,pch=variable_out)) +
  geom_rect(aes(xmin=-110,xmax=110,ymin=-60,ymax=60), fill='gray95', col='transparent', alpha=0.5) +
  geom_point(size=2) +
  geom_text(data=compare_counts_om,aes(x=x,y=y,label=n, col=match), 
            size=3.5, inherit.aes=FALSE) +
  facet_grid(cols=vars(operating_model), rows=vars(model.y)) +
  scale_shape_manual(values=c(0,1,2,4), name="Output Metric") +
  scale_color_manual(values=c("darkblue","darkorange")) +
  scale_x_continuous(limits=c(-110, 110), expand=c(0,0)) +
  guides(color='none') +
  labs(x="MSE: Probability of Increase/Decrease",y="QNM: Proportion Matrices with Increase/Decrease",
       subtitle="(a) by operating model scenario") +
  theme_bw() + theme(legend.title=element_blank())
p1

# by ms
p2 <- speciesALL_compare %>%
  mutate(management_strategy=case_when(
    management_strategy=='C. Fishery 1 in "offshore" only, effort changes'~'C. Fishery 1 in "offshore"\nonly, effort changes',
    management_strategy=='E. Fishery 1 in "offshore" only'~'E. Fishery 1 in "offshore"\n only',
    management_strategy=='D. Increase fishery mesh size'~'D. Increase fishery\n mesh size',
    .default=management_strategy)) %>%
  mutate(variable_out=fct_relevel(variable_out,c("Total Catch","Fishery 1 Catch","Fishery 2 Catch", "Spawning Biomass"))) %>%
  ggplot(aes(x=prob, y=psims, pch=variable_out)) +
  geom_rect(aes(xmin=-110,xmax=110,ymin=-60,ymax=60), fill='gray95', col='transparent', alpha=0.5) +
  geom_point(size=2.5) +
  geom_text(data=compare_counts_ms,aes(x=x,y=y,label=n, col=match), 
            size=4, inherit.aes=FALSE) +
  facet_grid(cols=vars(management_strategy), rows=vars(model.y)) +
  scale_shape_manual(values=c(0,1,2,4), name="Output Metric") +
  scale_color_manual(values=c("darkblue","darkorange")) +
  scale_x_continuous(limits=c(-110, 110), expand=c(0,0)) +
  guides(color='none') +
  labs(x="MSE: Probability of Increase/Decrease",y="QNM: Proportion Matrices with Increase/Decrease",
       subtitle="(b) by management strategy") +
  theme_bw() + theme(legend.title=element_blank())
p2

# plot separately
png(here('data','results','FigC4a.png'), res=300, height=1300,width=2800)
p1
dev.off()


png(here('data','results','FigC4b.png'), res=300, height=1300,width=3200)
p2
dev.off()


# plot together
# png(here('data','results','FigC3.png'), res=300, height=2500,width=3500)
# plot_grid(p1,p2,nrow=2)
# dev.off()



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
  

new_table2_match_counts <- speciesALL_compare  %>%
  filter(code != "2-A") %>% dplyr::select(-model.x, -operating_model, -management_strategy) %>%
  mutate(equiv=ifelse(abs(psims)<60, 'y','n')) %>%
  mutate(match=ifelse((psims < 0 & prob < 0) | (psims > 0 & prob > 0), "match","mismatch")) %>%
  mutate(match_equiv=ifelse(equiv=='y' & match=="match","match, equiv",
                            ifelse(equiv=='n' & match=="match", "match, not equiv", "mismatch"))) %>%
  group_by(model.y,variable_out) %>% mutate(compare_total=length(unique(code))) %>%
  ungroup() %>% dplyr::select(-equiv, -prob, -psims, -match) %>%
  group_by(model.y, variable_out, match_equiv, compare_total) %>% 
  summarise(n_compare=length(unique(code))) %>%
  mutate(p_compare=n_compare/compare_total) %>% pivot_wider(id_cols=c(model.y, variable_out), names_from=match_equiv,values_from=p_compare)



