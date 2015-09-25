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

# HMD credentials
hmd_username <- "***"
hmd_password <- "***"
