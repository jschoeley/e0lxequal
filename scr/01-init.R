# Init --------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(grid)
library(devtools)
source_gist("6cc00cd36ae3b8d0532f") # linear model equation annotation

# install_github("UCBdemography/DemogBerkeley", subdir = "DemogBerkeley")
library(DemogBerkeley)

# install_github("jschoeley/ggtheme")
library(ggtheme)

# install_github("jschoeley/rcpal")
library(rcpal)

# set the HMD credentials if data are not in data folder
if(!"hmd_f_lt_per.Rdata" %in% list.files("data")) {
# HMD credentials
  HMDcred <- read.table("data/HMDcred.txt")
  hmd_username <- as.character(HMDcred[1, 1])
  hmd_password <- as.character(HMDcred[1, 2])
}
