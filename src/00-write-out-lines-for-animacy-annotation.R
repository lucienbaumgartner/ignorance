## ---------------------------
##
## Script name: a00-write-out-lines.R
##
## Project: Tracing Thick Concepts
##
## Purpose of script: Write out text for animacy annotation in Python
##
## Author: Lucien Baumgartner
##
## Date created: 25.08.2021
##
## Email: lucienbaumgartner@philos.uzh.ch

library(dplyr)
rm(list =ls())

## ---------------------------
## 2 Set working directory ###
## ---------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

df <- lapply(list.files("../output/data/", full.names = T), function(x){read.csv(x, stringsAsFactors = F)})
nms <- rep(list.files("../output/data/"), sapply(df, nrow))
df <- do.call(rbind, df)
df <- cbind(df, nms)

df <- df %>% filter(CCONJ == 'and', !duplicated(id))
smpl <- df
#set.seed(6274)
#smpl <- df %>% group_by(nms) %>% sample_n(100)
# check
all(table(smpl$nms)==100)

for(i in 1:nrow(smpl)){
  writeLines(smpl$txt[i], paste0('../output/animacy-annotation/raw/', smpl$id[i], '.txt'))
}
