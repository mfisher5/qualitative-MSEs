########### Compare QNM results to MSE results ###########
#
# Compare the MSE output to the QNM output from "qnm_mse_master.R"
#    Tables required for this script are created in "qnm_mse_tables.R"
#
# Written 2023-12-20 M Fisher
# Last Updated: 2024-02-06 mcf
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

source(here('R','compare.models.MSEtable.R'))  # custom function



# Data --------------------------------------------------------------------

qnm <- read_csv(here('data','results','raw','qnm_0-5_hcra-e_pSims_202310.csv')) %>% clean_names()
mse <- read_csv(here('data','results','raw','mse_Table4_Species12_20240206.csv')) %>% clean_names()
mse_column_key <- data.frame(mse_column=c("total_c","cat_fl1","cat_fl2","ssb_ssb",
                                          "eff_fl1","eff_fl2"),
                             new_name=c("total_catch","fishery_1_catch","fishery_2_catch","spawning_biomass",
                                        "fishery_1_effort","fishery_2_effort"))

# clean up MSE output table
mse %<>% mutate(trial=str_replace(trial,'Results//Resu',''))
mse %<>% mutate(trial=str_replace(trial,'.out',''))
mse %<>% separate(col='trial',into=c('tmp1','tmp2','tmp3','tmp4'), sep='') %>%
  unite(tmp2,tmp4,col='code',sep="-") %>% dplyr::select(-tmp1,-tmp3)

mse_format <- mse %>% pivot_longer(cols=2:(dim(mse)[2]),names_to="variable",values_to="prob") %>%
  mutate(variable=ifelse(grepl("ssb",variable), paste0("ssb_",variable),variable)) %>%
  separate(variable,into=c("v1","v2","col_num"),sep="_") %>%
  filter(!is.na(col_num)) %>%    ## gets rid of column that just lists sim coutn (num_s1)
  mutate(model=ifelse(as.numeric(col_num) < 9, "MSE-Species1","MSE-Species2")) %>%
  unite(v1,v2, col="variable",sep="_") %>%
  left_join(mse_column_key,by=c("variable"="mse_column")) %>%
  dplyr::select(-col_num, -variable) %>% rename(variable_out=new_name)
      


# Format Data -------------------------------------------------------------

# melt tables so that they can be combined

qnm %<>% pivot_longer(cols=c(total_catch, fishery_1,fishery_2,spawning_biomass), names_to="variable_out", values_to="psims") %>%
  mutate(model="QNM") %>%
  mutate(variable_out=ifelse(variable_out=="fishery_1","fishery_1_catch",
                             ifelse(variable_out=="fishery_2","fishery_2_catch",variable_out)))

mse1 <- mse_format %>% filter(model=="MSE-Species1") %>%
  filter(variable_out %in% qnm$variable_out) %>%
  filter(code != "2-A")

mse2 <- mse_format %>% filter(model=="MSE-Species2") %>%
  filter(variable_out %in% qnm$variable_out) %>%
  filter(code != "2-A")


# match mse species 1 / qnm

species1_compare <- qnm %>% left_join(mse1, by=c("code","variable_out"))
ggplot(species1_compare, aes(x=prob,y=psims, pch=variable_out)) +
  geom_point() +
  geom_hline(aes(yintercept=60), col="orange", lty=2) +
  geom_hline(aes(yintercept=80), col="darkorange", lty=2) +
  geom_hline(aes(yintercept=-60), col="lightblue", lty=2) +
  geom_hline(aes(yintercept=-80), col="darkblue", lty=2) +
  facet_grid(cols=vars(operating_model)) +
  scale_shape_manual(values=c(0,1,2,4), name="Output Metric") +
  labs(x="MSE: Probability of Increase",y="QNM: Proportion Simulations with Increase/Decrease",
       title="MSE Species 1 v. QNM") +
  theme_bw()


# match mse species 2 / qnm

species2_compare <- qnm %>% left_join(mse2, by=c("code","variable_out"))
ggplot(species2_compare, aes(x=prob,y=psims, pch=variable_out)) +
  geom_point() +
  geom_hline(aes(yintercept=60), col="orange", lty=2) +
  geom_hline(aes(yintercept=80), col="darkorange", lty=2) +
  geom_hline(aes(yintercept=-60), col="lightblue", lty=2) +
  geom_hline(aes(yintercept=-80), col="darkblue", lty=2) +
  facet_grid(cols=vars(operating_model)) +
  scale_shape_manual(values=c(0,1,2,4), name="Output Metric") +
  labs(x="MSE: Probability of Increase/Decrease",y="QNM: Proportion Simulations with Increase/Decrease",
       title="MSE Species 2 v. QNM") +
  theme_bw()


# match multiple MSE versions of 2 Thorson Effort to QNM
compareThorson <- qnm %>% filter(operating_model=="2. Thorson effort") %>% 
  separate(code, into=c("om-code","ms-code"),sep="-",remove=FALSE) %>%
  dplyr::select(-"om-code") %>%
  left_join(mse_format %>% rename(mse_code=code) %>%
              separate(mse_code, into=c("om-code","ms-code"),sep="-",remove=FALSE) %>%
              filter(`om-code` %in% c("2","6","7")),
            by=c("ms-code","variable_out"))
              
compareThorson %<>% mutate(operating_model = ifelse(`om-code`=="6","Target 0.75x Biomass",
                                                    ifelse(`om-code`=="7","Target 1.25x Biomass","Target 1x Biomass (OG)")))
compareThorson$operating_model <- factor(compareThorson$operating_model, levels=c("Target 0.75x Biomass",
                                                                                  "Target 1x Biomass (OG)",
                                                                                  "Target 1.25x Biomass"))

# Save data frames --------------------------------------------------------

speciesALL_compare <- bind_rows(species1_compare, species2_compare)
saveRDS(speciesALL_compare, here('data','results','mse-probs_V_qnm-props_Species12_allHCRallMS_202402.rds'))




# Explore results: equivocal QNM ------------------------------------------

## when the QNM was qualitatively equivocal, did the direction generally agree with the quant MSE?
speciesALL_equiv <- speciesALL_compare %>%
  filter((psims < 60 & psims > 0) | (psims > -60 & psims < 0))

speciesALL_equiv %<>% mutate(match_direction=ifelse(psims==50 | psims==-50,"split",
                                                    ifelse(psims < 0 & prob > 0, "negQNM-posMSE",
                                                    ifelse(psims > 0 & prob < 0, "posQNM-negMSE","match"))))

## species 1
with(filter(speciesALL_equiv,grepl("Species1",model.y)), table(match_direction)/dim(filter(speciesALL_equiv,grepl("Species1",model.y)))[1])

# match       negQNM-posMSE    posQNM-negMSE         split 
# 0.45454545    0.22727273    0.22727273        0.09090909 


speciesALL_equiv %>% filter(grepl("Species1",model.y)) %>%
  group_by(variable_out) %>%
  mutate(var_n=n()) %>%
  ungroup() %>%
  group_by(variable_out,var_n,match_direction) %>%
  summarise(ndiff=n()) %>%
  mutate(pdiff=ndiff/var_n) %>%
  dplyr::select(variable_out,match_direction,var_n, pdiff) %>%
  pivot_wider(names_from=match_direction, values_from=pdiff)

## Species 2
with(filter(speciesALL_equiv,grepl("Species2",model.y)), table(match_direction)/dim(filter(speciesALL_equiv,grepl("Species2",model.y)))[1])

# match      negQNM-posMSE    posQNM-negMSE         split 
# 0.40909091    0.09090909    0.40909091       0.09090909  

speciesALL_equiv %>% filter(grepl("Species2",model.y)) %>%
  group_by(variable_out) %>%
  mutate(var_n=n()) %>%
  ungroup() %>%
  group_by(variable_out,var_n,match_direction) %>%
  summarise(ndiff=n()) %>%
  mutate(pdiff=ndiff/var_n) %>%
  dplyr::select(variable_out,match_direction,var_n, pdiff) %>%
  pivot_wider(names_from=match_direction, values_from=pdiff)



# Explore results: both species by OM -------------------------------------
 
ggplot(speciesALL_compare, aes(x=prob,y=psims, pch=variable_out)) +
  geom_point() +
  geom_hline(aes(yintercept=60), col="orange", lty=2) +
  geom_hline(aes(yintercept=80), col="darkorange", lty=2) +
  geom_hline(aes(yintercept=-60), col="lightblue", lty=2) +
  geom_hline(aes(yintercept=-80), col="darkblue", lty=2) +
  facet_grid(cols=vars(operating_model), rows=vars(model.y)) +
  labs(x="MSE: Probability of Increase/Decrease",y="QNM: Proportion Simulations with Increase/Decrease",
       title="MSE v. QNM") +
  theme_bw() + theme(legend.title=element_blank())

# 
# png(here('results','QNM_v_MSE_quantitative_scatterplot_2024-03-28.png'), width=2800,height=2000,res=300)
# ggplot(speciesALL_compare, aes(x=prob,y=psims, pch=variable_out)) +
#   geom_point() +
#   geom_hline(aes(yintercept=60), col="orange", lty=2) +
#   geom_hline(aes(yintercept=80), col="darkorange", lty=2) +
#   geom_hline(aes(yintercept=-60), col="lightblue", lty=2) +
#   geom_hline(aes(yintercept=-80), col="darkblue", lty=2) +
#   facet_grid(cols=vars(operating_model), rows=vars(model.y)) +
#   scale_shape_manual(values=c(0,1,2,4), name="Output Metric") +
#   labs(x="MSE: Probability of Increase/Decrease",y="QNM: Proportion Simulations with Increase/Decrease",
#        title="MSE v. QNM") +
#   theme_bw() + theme(legend.title=element_blank())
# dev.off()


png(here('results','QNM_v_MSEsp2_quantitative_scatterplot_2024-03-28.png'), width=2600,height=2000,res=300)
ggplot(species2_compare, aes(x=prob,y=psims, pch=variable_out)) +
  geom_point() +
  geom_hline(aes(yintercept=60), col="orange", lty=2) +
  geom_hline(aes(yintercept=80), col="darkorange", lty=2) +
  geom_hline(aes(yintercept=-60), col="lightblue", lty=2) +
  geom_hline(aes(yintercept=-80), col="darkblue", lty=2) +
  facet_grid(cols=vars(operating_model), rows=vars(management_strategy)) +
  scale_shape_manual(values=c(0,1,2,4), name="Output Metric") +
  labs(x="MSE: Probability of Increase",y="QNM: Proportion Simulations with Increase/Decrease",
       title="MSE Species 2 v. QNM") +
  theme_bw() + theme(strip.text.y=element_text(hjust=0))
dev.off()
# 
# png(here('results','QNM_v_MSEsp1_quantitative_scatterplot_2024-03-28.png'), width=2800,height=2000,res=300)
# ggplot(species1_compare, aes(x=prob,y=psims, pch=variable_out)) +
#   geom_point() +
#   geom_hline(aes(yintercept=60), col="orange", lty=2) +
#   geom_hline(aes(yintercept=80), col="darkorange", lty=2) +
#   geom_hline(aes(yintercept=-60), col="lightblue", lty=2) +
#   geom_hline(aes(yintercept=-80), col="darkblue", lty=2) +
#   scale_shape_manual(values=c(0,1,2,4), name="Output Metric") +
#   facet_grid(cols=vars(operating_model), rows=vars(management_strategy)) +
#   labs(x="MSE: Probability of Increase",y="QNM: Proportion Simulations with Increase/Decrease",
#        title="MSE Species 1 v. QNM") +
#   theme_bw() + theme(strip.text.y=element_text(hjust=0))
# dev.off()




# Explore results: Thorson Effort ------------------------------------------


## for specific operating models, break out comparisons by management strategies and species
png(here('results','QNM_v_MSE_ThorsonEffort_quantitative_scatterplot_2024-03-28.png'), width=3300,height=1600,res=300)
compareThorson %>%
  ggplot(aes(x=prob,y=psims, pch=variable_out)) +
  geom_point() +
  geom_hline(aes(yintercept=60), col="orange", lty=2) +
  geom_hline(aes(yintercept=80), col="darkorange", lty=2) +
  geom_hline(aes(yintercept=-60), col="lightblue", lty=2) +
  geom_hline(aes(yintercept=-80), col="darkblue", lty=2) +
  facet_grid(cols=vars(management_strategy), rows=vars(operating_model)) +
  scale_shape_manual(values=c(0,1,2,4), name="Output Metric") +
  labs(x="MSE: Probability of Increase/Decrease",y="QNM: Proportion Simulations with Increase/Decrease",
       title="MSE 2 v. QNM 2 (Thorson Effort)") +
  theme_bw() + theme(legend.title=element_blank())
dev.off()


# Explore results: each operating model --------------------------------------
## for specific operating models, break out comparisons by management strategies and species

png(here('results','QNM_v_MSE_0BaseModel_quantitative_scatterplot_2024-02-01.png'), width=3200,height=1500,res=300)
speciesALL_compare %>%
  filter(grepl('0',operating_model)) %>%
  ggplot(aes(x=prob,y=psims, pch=variable_out)) +
  geom_point() +
  geom_hline(aes(yintercept=60), col="orange", lty=2) +
  geom_hline(aes(yintercept=80), col="darkorange", lty=2) +
  geom_hline(aes(yintercept=-60), col="lightblue", lty=2) +
  geom_hline(aes(yintercept=-80), col="darkblue", lty=2) +
  facet_grid(cols=vars(management_strategy), rows=vars(model.y)) +
  scale_shape_manual(values=c(0,1,2,4), name="Output Metric") +
  labs(x="MSE: Probability of Increase/Decrease",y="QNM: Proportion Simulations with Increase/Decrease",
       title="MSE 0 v. QNM 0 (Base Model)") +
  theme_bw() + theme(legend.title=element_blank())
dev.off()


png(here('results','QNM_v_MSE_3LowerProductivity_quantitative_scatterplot_2024-02-01.png'), width=3200,height=1500,res=300)
speciesALL_compare %>%
  filter(grepl('3',operating_model)) %>%
  ggplot(aes(x=prob,y=psims, pch=variable_out)) +
  geom_point() +
  geom_hline(aes(yintercept=60), col="orange", lty=2) +
  geom_hline(aes(yintercept=80), col="darkorange", lty=2) +
  geom_hline(aes(yintercept=-60), col="lightblue", lty=2) +
  geom_hline(aes(yintercept=-80), col="darkblue", lty=2) +
  facet_grid(cols=vars(management_strategy), rows=vars(model.y)) +
  scale_shape_manual(values=c(0,1,2,4), name="Output Metric") +
  labs(x="MSE: Probability of Increase/Decrease",y="QNM: Proportion Simulations with Increase/Decrease",
       title="MSE 3 v. QNM 3 (Lower Productivity)") +
  theme_bw() + theme(legend.title=element_blank())
dev.off()









