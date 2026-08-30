########## Create Fig 7 ###########
#
# Fig 7 summarizes trade-offs in catch, biomass  
#    comparing QNM v MSE output. 
#    Data required for this script are created in "03_qnm_mse_compare.R"
#
# Written 2024-12-27 M Fisher
# Last Updated: 2026-08-24
#
################################################################


# Set up ------------------------------------------------------------------

sim.date <- '20240923'      # date sims were run (in object names)

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

tos_list <- readRDS(here('data','results','fig7_data_arrows.rds'))
tos2_total <- tos_list[['total_catch']]
tos2_fish1 <- tos_list[['fishery_1_catch']]
tos2_fish2 <- tos_list[['fishery_2_catch']]


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


############### separate equivocal output  ##############
#########################################################

# Total catch -------------------------------------------------------------

tradeoffs_vec2_qnm <- tradeoffs_vec2_equiv[
  which((tradeoffs_vec2_equiv %in% tradeoffs_vec2) | (tradeoffs_vec2_equiv %in% unique(tos2_total$QNM)))
]
pal=pnw_palette("Shuksan",100)
to_plot <- tos2_total %>%
  group_by(species, QNM, MSE) %>%
  summarise(n=length(unique(code)),
            mse_equiv=sum(tot_mse_equiv)) %>%
  full_join(pairwise_arrows, by=c("QNM","MSE","species")) %>%
  mutate(n=ifelse(is.na(n), 0, n),
         n_lab=ifelse(n==0,NA,n),
         n_col=ifelse(n>12,'gray90','gray40')) %>%
  mutate(n_lab=ifelse(mse_equiv==1, paste0(n_lab,'*'),paste0(n_lab)))

plot.total.equiv <- ggplot(to_plot, aes(x=fct_relevel(MSE, tradeoffs_vec2), y=fct_relevel(QNM, c(rev(tradeoffs_vec2_qnm))), fill=n)) +
  geom_tile() + 
  geom_text(aes(label=n_lab, fontface='italic'), col=to_plot$n_col) + # col=c(rep('gray40',9),'gray99',rep('gray40',27))) +
  geom_rect(aes(xmin=c(0.5),xmax=c(1.5),
                ymin=c(6.5),ymax=c(7.5)), fill='transparent',col='black') +
  geom_rect(aes(xmin=1.5, xmax=2.5,
                ymin=5.5, ymax=6.5), fill='transparent',col='black') +
  geom_rect(aes(xmin=2.5, xmax=3.5,
                ymin=4.5, ymax=5.5), fill='transparent',col='black') +
  geom_rect(aes(xmin=3.5, xmax=4.5,
                ymin=3.5, ymax=4.5), fill='transparent',col='black') +
  geom_hline(aes(yintercept=3.5)) +
  facet_wrap(~species,strip.position='bottom') +
  labs(x="Quant MSE",y="QNM") +
  scale_x_discrete(position='top') +
  scale_fill_gradientn(colours = rev(pal), na.value='white', limits=c(0,15)) +
  theme_bw() +
  theme(legend.title=element_blank(), strip.text=element_blank()) -> plot.total.equiv

plot.total.equiv





# Fishery 1 -----------------------------------------------

tradeoffs_vec2_qnm <- tradeoffs_vec2_equiv[
  which((tradeoffs_vec2_equiv %in% tradeoffs_vec2) | (tradeoffs_vec2_equiv %in% unique(tos2_fish1$QNM)))
]
pal=pnw_palette("Shuksan",100)
to_plot <- tos2_fish1 %>%
  group_by(species, QNM, MSE) %>%
  summarise(n=length(unique(code)),
            mse_equiv=sum(tot_mse_equiv)) %>%
  full_join(pairwise_arrows, by=c("QNM","MSE","species")) %>%
  mutate(n=ifelse(is.na(n), 0, n),
         n_lab=ifelse(n==0,NA,n),
         n_col=ifelse(n>12,'gray90','gray40')) %>%
  mutate(n_lab=ifelse(mse_equiv==1, paste0(n_lab,'*'),paste0(n_lab)))

plot.f1.equiv <- ggplot(to_plot, aes(x=fct_relevel(MSE, tradeoffs_vec2), y=fct_relevel(QNM, c(rev(tradeoffs_vec2_qnm))), fill=n)) +
  geom_tile() + 
  geom_text(aes(label=n_lab, fontface='italic'), col=to_plot$n_col) + # col=c(rep('gray40',9),'gray99',rep('gray40',27))) +
  geom_rect(aes(xmin=c(0.5),xmax=c(1.5),
                ymin=c(6.5),ymax=c(7.5)), fill='transparent',col='black') +
  geom_rect(aes(xmin=1.5, xmax=2.5,
                ymin=5.5, ymax=6.5), fill='transparent',col='black') +
  geom_rect(aes(xmin=2.5, xmax=3.5,
                ymin=4.5, ymax=5.5), fill='transparent',col='black') +
  geom_rect(aes(xmin=3.5, xmax=4.5,
                ymin=3.5, ymax=4.5), fill='transparent',col='black') +
  geom_hline(aes(yintercept=3.5)) +
  facet_wrap(~species,strip.position='bottom') +
  labs(x="Quant MSE",y="QNM") +
  scale_x_discrete(position='top') +
  scale_fill_gradientn(colours = rev(pal), na.value='white', limits=c(0,15)) +
  theme_bw() +
  theme(legend.title=element_blank(), strip.text=element_blank())

plot.f1.equiv


# Fishery 2 catch -----------------------------------------------
tradeoffs_vec2_qnm <- tradeoffs_vec2_equiv[
  which((tradeoffs_vec2_equiv %in% tradeoffs_vec2) | (tradeoffs_vec2_equiv %in% unique(tos2_fish2$QNM)))
]
pal=pnw_palette("Shuksan",100)
to_plot <- tos2_fish2 %>%
  group_by(species, QNM, MSE) %>%
  summarise(n=length(unique(code)),
            mse_equiv=sum(tot_mse_equiv)) %>%
  full_join(pairwise_arrows, by=c("QNM","MSE","species")) %>%
  mutate(n=ifelse(is.na(n), 0, n),
         n_lab=ifelse(n==0,NA,n),
         n_col=ifelse(n>12,'gray90','gray40')) %>%
  mutate(n_lab=ifelse(mse_equiv==1, paste0(n_lab,'*'),paste0(n_lab)))

plot.f2.equiv <- ggplot(to_plot, aes(x=fct_relevel(MSE, tradeoffs_vec2), y=fct_relevel(QNM, c(rev(tradeoffs_vec2_qnm))), fill=n)) +
  geom_tile() + 
  geom_text(aes(label=n_lab, fontface='italic'), col=to_plot$n_col) + # col=c(rep('gray40',9),'gray99',rep('gray40',27))) +
  geom_rect(aes(xmin=c(0.5),xmax=c(1.5),
                ymin=c(6.5),ymax=c(7.5)), fill='transparent',col='black') +
  geom_rect(aes(xmin=1.5, xmax=2.5,
                ymin=5.5, ymax=6.5), fill='transparent',col='black') +
  geom_rect(aes(xmin=2.5, xmax=3.5,
                ymin=4.5, ymax=5.5), fill='transparent',col='black') +
  geom_rect(aes(xmin=3.5, xmax=4.5,
                ymin=3.5, ymax=4.5), fill='transparent',col='black') +
  geom_hline(aes(yintercept=3.5)) +
  facet_wrap(~species,strip.position='bottom') +
  labs(x="Quant MSE",y="QNM") +
  scale_x_discrete(position='top') +
  scale_fill_gradientn(colours = rev(pal), na.value='white', limits=c(0,15)) +
  theme_bw() +
  theme(legend.title=element_blank(), strip.text=element_blank())

plot.f2.equiv


# FIGURE 7 ----------------------------------------------------------

png(here('doc','fig7-tradeoffs.png'), res=300,height=2600,width=2100)
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

figs_list <- readRDS(file=here('data','results',paste0('figs_tradeoffs_expanded_byOMbyHCRbySp_',sim.date,'.RDS')))
tos_total <- figs_list[['total_catch']] %>%
  mutate(operating_model=ifelse(grepl('4',operating_model),
                                "4. Higher growth rate", operating_model)) %>%
  mutate(management_strategy=ifelse(grepl("C.",management_strategy),
                                    "C. Fishery 1 in \"offshore\" only,\neffort changes", management_strategy))

tos_fish1 <- figs_list[['fishery_1_catch']] %>%
  mutate(operating_model=ifelse(grepl('4',operating_model),
                                "4. Higher growth rate", operating_model)) %>%
  mutate(management_strategy=ifelse(grepl("C.",management_strategy),
                                    "C. Fishery 1 in \"offshore\" only,\neffort changes", management_strategy))

tos_fish2 <- figs_list[['fishery_2_catch']] %>%
  mutate(operating_model=ifelse(grepl('4',operating_model),
                                "4. Higher growth rate", operating_model)) %>%
  mutate(management_strategy=ifelse(grepl("C.",management_strategy),
                                    "C. Fishery 1 in \"offshore\" only,\neffort changes", management_strategy))


# Species 1 ---------------------------------------------------------------
## total catch
tos_total <- tos_total %>%
  mutate(qnm_compare=factor(qnm_compare,levels=c('optimistic','pessimistic','same','50/50')))
any(is.na(tos_total$qnm_compare))

p2_total <- tos_total %>%
  dplyr::select(-qnm_psims,-qnm_equiv,-mse_prob) %>%
  rename(qMSE=MSE) %>%
  pivot_longer(cols=c(QNM, qMSE), names_to="model", values_to="change") %>%
  filter(grepl("1", species))  %>%
  mutate(variable_out=case_when(
    variable_out=="spawning_biomass" ~ "biomass",
    .default=variable_out
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
  scale_fill_manual(values=c("goldenrod","#8da0cb","white","gray80"), na.value="white",name='QNM is...') +
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

## fishery 1
tos_fish1 <- tos_fish1 %>%
  mutate(qnm_compare=factor(qnm_compare,levels=c('optimistic','pessimistic','same','50/50')))
any(is.na(tos_fish1$qnm_compare))


p2_fish1 <- tos_fish1 %>%
  rename(qMSE=MSE) %>%
  dplyr::select(-qnm_psims,-qnm_equiv,-mse_prob) %>%
  pivot_longer(cols=c(QNM,qMSE), names_to="model", values_to="change") %>%
  filter(grepl("1", species))  %>%
  mutate(variable_out=case_when(
    variable_out=="spawning_biomass" ~ "biomass",
    .default=variable_out
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
  scale_fill_manual(values=c("goldenrod","#8da0cb","white","gray80"), na.value="white",name='QNM is...') +
  theme_bw() +
  theme(strip.background=element_rect(fill='white'),
        strip.placement = "outside",
        strip.text.x.top=element_text(angle=90),
        strip.text.y.left=element_text(angle=0,hjust=1),
        axis.text.x=element_text(angle=90, vjust=0.5, hjust=1),
        panel.spacing.x=unit(0, "lines") , panel.spacing.y=unit(0,"lines"))

p2_fish1

## fishery 2
tos_fish2 <- tos_fish2 %>% 
  mutate(qnm_compare=factor(qnm_compare,levels=c('optimistic','pessimistic','same','50/50')))
any(is.na(tos_fish2$qnm_compare))

p2_fish2 <- tos_fish2 %>%
  rename(qMSE=MSE) %>%
  dplyr::select(-qnm_psims,-qnm_equiv,-mse_prob) %>%
  pivot_longer(cols=c(QNM, qMSE), names_to="model", values_to="change") %>%
  filter(grepl("1", species))  %>%
  mutate(management_strategy=ifelse(
    management_strategy=="C. Fishery 1 in \"offshore\" only, effort changes",
    "C. Fishery 1 in \"offshore\" only,\n effort changes",
    management_strategy
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
  scale_fill_manual(values=c("goldenrod","white","gray80"), na.value="white",name='QNM is...') +
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

png(here('doc','supplement','figC_tradeoffs_expanded_sp1.png'), res=300,height=1500,width=3000)
pout + draw_text(c('(A) Total Catch','(B) Fishery 1 Catch',
                   '(C) Fishery 2 Catch', 'SPECIES 1: '), 
                 x=c(0.265, 0.547, 0.805, 0.13), 
                 y=c(0.95,0.95, 0.95, 0.95))
dev.off()


# Species 2 ---------------------------------------------------------------

## create table
p2s2_total <- tos_total %>%
  dplyr::select(-qnm_psims,-qnm_equiv,-mse_prob) %>%
  rename(qMSE=MSE) %>%
  pivot_longer(cols=c(QNM, qMSE), names_to="model", values_to="change") %>%
  filter(grepl("2", species))  %>%
  mutate(variable_out=case_when(
    variable_out=="spawning_biomass" ~ "biomass",
    .default=variable_out
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
  scale_fill_manual(values=c("goldenrod","white","gray80"), na.value="white",name='QNM is...') +
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
p2s2_fish1 <- tos_fish1 %>%
  rename(qMSE=MSE) %>%
  dplyr::select(-qnm_psims,-qnm_equiv,-mse_prob) %>%
  pivot_longer(cols=c(QNM,qMSE), names_to="model", values_to="change") %>%
  filter(grepl("2", species))  %>%
  mutate(variable_out=case_when(
    variable_out=="spawning_biomass" ~ "biomass",
    .default=variable_out
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
  scale_fill_manual(values=c("goldenrod","white","gray80"), na.value="white",name='QNM is...') +
  theme_bw() +
  theme(strip.background=element_rect(fill='white'),
        strip.placement = "outside",
        strip.text.x.top=element_text(angle=90),
        strip.text.y.left=element_text(angle=0,hjust=1),
        axis.text.x=element_text(angle=90, vjust=0.5, hjust=1),
        panel.spacing.x=unit(0, "lines") , panel.spacing.y=unit(0,"lines"))

p2s2_fish1

## create table
p2s2_fish2 <- tos_fish2 %>%
  rename(qMSE=MSE) %>%
  dplyr::select(-qnm_psims,-qnm_equiv,-mse_prob) %>%
  pivot_longer(cols=c(QNM, qMSE), names_to="model", values_to="change") %>%
  filter(grepl("2", species))  %>%
  mutate(variable_out=case_when(
    variable_out=="spawning_biomass" ~ "biomass",
    .default=variable_out
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
  scale_fill_manual(values=c("goldenrod","#8da0cb","white","gray80"), na.value="white",name='QNM is...') +
  theme_bw() +
  theme(strip.background=element_rect(fill='white'),
        strip.placement = "outside",
        strip.text.x.top=element_text(angle=90),
        strip.text.y.left=element_text(angle=0,hjust=1),
        axis.text.x=element_text(angle=90, vjust=0.5,hjust=1),
        panel.spacing.x=unit(0, "lines") , panel.spacing.y=unit(0,"lines"))
p2s2_fish2


## FIGURE C3 - ALL TOGETHER

pout2 <- plot_grid(plotlist=list(
  p2s2_total + theme(legend.position='none', plot.margin = unit(c(1.7, 0, 0, 1), "lines"),
                   axis.text.y=element_blank()),
  p2s2_fish1 + theme(legend.position='none',plot.margin = unit(c(1.7, 0, 0,0), "lines"),
                   axis.text.y=element_blank(),
                   strip.text.y.left = element_blank()),
  p2s2_fish2 + theme(legend.position='none',plot.margin = unit(c(1.7, 0, 0, 0), "lines"),
                   strip.text.y.left = element_blank())),
  ncol=3, nrow=1, rel_widths=c(1,0.6,0.65))

png(here('doc','supplement','figC_tradeoffs_expanded_sp2.png'), res=300,height=1200,width=3000)
pout2 + draw_text(c('(D) Total Catch','(E) Fishery 1 Catch',
                   '(F) Fishery 2 Catch', 'SPECIES 2: '), 
                 x=c(0.265, 0.529, 0.795, 0.13), 
                 y=c(0.95,0.95, 0.95, 0.95))
dev.off()






# pasture -----------------------------------------------------------------

### warning: old code! everything below is not updated for manuscript.
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








############### allow equivocal output to count to QNM directions ##############
################################################################################

# Total catch: allow equiv -------------------------------------------------------------

tos_total0 <- tos_list[['total_catch']]
tos_total0 <- tos_total0 %>%
  mutate(QNM=case_when(
    QNM > 0 ~ '\u2191 catch,\n\u2191 biomass',
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



# FIGURE 7: allow equiv ----------------------------------------------------------


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
