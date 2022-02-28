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
library(ggplot2)

rm(list =ls())

## ---------------------------
## 2 Set working directory ###
## ---------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

## ---------------------------
######## 3 Load data #########
## ---------------------------
load("../output/corpus/corpus_final.RDS")
df <- df %>% mutate(method = ifelse(grepl("%20that%22|%20of%22", nms), "of/that", "animacy"),
                    submethod = ifelse(grepl("%20that%22", nms), "that", NA),
                    submethod = ifelse(grepl("%20of%22", nms), "of", submethod),
                    sentiment = as.numeric(sentiment),
                    TARGET = as.factor(TARGET),
                    date = as.POSIXct(created_utc, origin="1970-01-01")) %>% as_tibble
table(df$dep_subclass)
dfx <- df %>% filter(dep_subclass %in% c("abstract", "event", "place", "time", NA) & !is.na(sentiment))
range(dfx$date)

aggr <- dfx %>% 
  group_by(TARGET, method) %>% 
  summarise(avg = mean(sentiment),
            median = median(sentiment))

q <- ggplot(dfx) +
  geom_boxplot(aes(x=TARGET, y=sentiment)) +
  facet_wrap(~method)
q

q <- ggplot(dfx) +
  geom_density(aes(x=sentiment)) +
  geom_vline(aes(xintercept=0), lty = "dotted") +
  geom_vline(data = aggr, aes(xintercept=avg), colour = "red") +
  geom_vline(data = aggr, aes(xintercept=median), colour = "blue") +
  facet_grid(TARGET~method) +
  theme_bw()
ggsave(q, file = "../output/plots/density.png", width = 6, height = 4)

levels(dfx$TARGET)
wilcox.test(sentiment ~ TARGET, data = dfx[dfx$method=="animacy",], alternative = "less")
wilcox.test(sentiment ~ TARGET, data = dfx[dfx$method=="of/that",], alternative = "less")

res <- lm(sentiment ~ TARGET + dep_subclass , data = dfx[dfx$method=="animacy",])
summary(res)

res <- lm(sentiment ~ TARGET + submethod , data = dfx[dfx$method=="of/that",])
summary(res)

ggplot(dfx[dfx$method=="of/that",]) +
  geom_boxplot(aes(x=TARGET, y=sentiment)) +
  facet_wrap(~submethod)
