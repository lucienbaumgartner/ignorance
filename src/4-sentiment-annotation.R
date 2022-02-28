## ---------------------------
##
## Script name: a03-merge-data.R
##
## Project: Tracing Thick Concepts
##
## Purpose of script: Write out text for animacy annotation in Python
##
## Author: Lucien Baumgartner
##
## Date created: 26.08.2021
##
## Email: lucien.baumgartner@philos.uzh.ch

library(tidyverse)
library(rvest)
library(quanteda)

rm(list =ls())

## ---------------------------
## 2 Set working directory ###
## ---------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

## ---------------------------
######## 3 Load data #########
## ---------------------------
load("../input/sentiWords-db.RDS")
load('../output/corpus/corpus.RDS')
head(data_annot)
tail(data_annot)

## ---------------------------
### 3 Sentiment Annotation ###
## ---------------------------
df <- data_annot %>% filter(!(!grepl("%20that%22|%20of%22", nms)&is.na(dep_subclass)))
names(df)
annot <- tokens_lookup(tokens(unique(df$ADJ)), dictionary = sentiWords$num)
head(annot)
annot <- tibble(ADJ = unique(df$ADJ), sentiment = sapply(annot, function(x) x[1]))
df <- left_join(df, annot)
df <- df %>% filter(!is.na(sentiment)|!duplicated(id))
save(df, file = "../output/corpus/corpus_final.RDS")
