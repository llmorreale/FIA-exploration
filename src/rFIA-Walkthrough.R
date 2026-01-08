#install/load rFIA
#setwd("FIA-exploration")
#install.packages("rFIA")
library(rFIA)

### Download FIA data for VA###
options(timeout = 3600)

VA_FIA <- getFIA("VA",dir ="data/", tables = c("TREE", "PLOT")) #download just the tree & plot table for VA

#once downloaded, can read it from file instead of having to download it again#
#VA_FIA <- readFIA("VA", dir = "/")

