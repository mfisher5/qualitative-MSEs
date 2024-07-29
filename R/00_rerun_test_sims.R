# Written 2023-09-07
# Last Updated: 2023-09-27 -- mcf
#
##############################################


# Set up ------------------------------------------------------------------

## Functions
library(here)
library(tidyverse)
library(ggplot2)

#Load XML first--dependency not included in QPress
#To install QPress: 
#   library(remotes)
#   remotes::install_github("SWotherspoon/QPress")
library(QPress)
library(XML)
library(tcltk2)
library(gridExtra)
library(cowplot)


## custom functions
source(here('../R/parse.constraints.SWotherspoon.R'))
source(here('../R/prepare.dia.R'))
source(here('../R/impact.table.R'))
source(here('../R/compare.baseline.impact.table.R'))
source(here('../R/compare.models.impact.table.R'))
source(here('../R/compare.models.impact.dataframe.R'))
source(here('../R/alternating.list.R'))


## 
set.seed(555)

#### Model variable key ####
# I use this to order the variables in the output tables.
vkey <- read_csv(here('data','om_variable_key.csv'))


# Perturbations -----------------------------------------------------------

# HCR A: Status Quo
PertA <-  c(`inshore small mesh effort`=0)

# HCR B: Remove small mesh fleet
PertB <- c(`inshore small mesh effort`=-1,`offshore small mesh effort`=-1)

# HCR C: Remove inshore small mesh fleet, allow effort redistribution to offshore small mesh fishery
PertC <- c(`inshore small mesh effort`=-1)

# HCR D: Increase the mesh size for fishery #1
PertD <- c(`inshore mesh size`=1,`offshore mesh size`=1)

# HCR E: remove fishery #1 from the inshore area
PertE <- c(`inshore small mesh effort`=-1)


# OM 3:  There is a negative perturbation on “inshore recruits”
PertOM3 <- c(`inshore recruits`=1)

# OM 4:  There a positive perturbation on “climate pressure"
PertOM4 <- c(`Climate pressure`=1)

# OM 5: There is a positive perturbation on "growth"
PertOM5 <- c(`Growth`=1)




# OM 4 --------------------------------------------------------------------

#### 4. Animals move into the “far offshore” area ####
mlist4 <- list(
  m <- prepare.dia(dia_name='MSE-QNM_2023-Sep_004-movement-to-inaccessible',dia_path=here('data'))[[1]],  # no constraints
  m.c <- prepare.dia(dia_name='MSE-QNM_2023-Sep_004C-rm-small-mesh-redistribute-small',dia_path=here('data'))[[1]]  # no constraints
)















