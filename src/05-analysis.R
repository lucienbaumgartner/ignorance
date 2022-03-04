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
df <- df %>% mutate(method = ifelse(grepl("%20that%22|%20of%22", nms), "preposition", "entity type"),
                    submethod = ifelse(grepl("%20that%22", nms), "that", NA),
                    submethod = ifelse(grepl("%20of%22", nms), "of", submethod),
                    sentiment = as.numeric(sentiment),
                    TARGET = as.factor(TARGET),
                    date = as.POSIXct(created_utc, origin="1970-01-01")) %>% as_tibble
table(df$dep_subclass)
dfx <- df %>% filter(dep_subclass %in% c("abstract", "event", "place", "time", NA) & !is.na(sentiment) & is.na(TARGET_mod) & is.na(ADV))
range(dfx$date)
nrow(dfx)
nrow(dfx[dfx$method=="entity type",])
nrow(dfx[dfx$method=="preposition",])

table(dfx$method, dfx$submethod)

se <- function(x) sqrt(var(x) / length(x))
aggr <- dfx %>% 
  filter(method == "entity type" | submethod == "of") %>% 
  group_by(TARGET, method) %>% 
  summarise(avg = mean(sentiment),
            median = median(sentiment),
            SE =se(sentiment)) 

q <-  dfx %>% 
  filter(method == "entity type" | submethod == "of") %>% 
  ggplot(.) +
  geom_density(aes(x=sentiment)) +
  geom_vline(aes(xintercept=0), lty = "dotted") +
  geom_vline(data = aggr, aes(xintercept=avg), colour = "red") +
  geom_vline(data = aggr, aes(xintercept=median), colour = "blue") +
  geom_text(data = aggr, aes(x = avg, y = 0, label = round(avg, 2)), colour = "red", hjust = -0.1, size = 3) +
  geom_text(data = aggr, aes(x = median, y = 0, label = round(median, 2)), colour = "blue", hjust = 1.1, size = 3) +
  facet_grid(TARGET~method) +
  theme_bw()
q
ggsave(q, file = "../output/plots/density.png", width = 6, height = 4)

write.csv(aggr, file = "../output/dfs/mean_medians.csv")

top_words <- dfx %>% 
  filter(method == "preposition") %>% 
  group_by(TARGET, ADJ, sentiment) %>% 
  summarise(n = n()) %>% 
  ungroup %>% 
  group_by(TARGET) %>% 
  arrange(TARGET, desc(n)) %>% 
  slice_head(n = 50)
write.csv(top_words, file = "../output/dfs/top_words.csv")

dfx %>% 
  filter(method == "preposition", ADJ == "sighted") %>% 
  pull(corpus)

levels(dfx$TARGET)
wilcox.test(sentiment ~ TARGET, data = dfx[dfx$method=="entity type",], alternative = "less")
wilcox.test(sentiment ~ TARGET, data = dfx[dfx$method=="preposition",], alternative = "less")

res <- lm(sentiment ~ TARGET + dep_subclass , data = dfx[dfx$method=="entity type",])
summary(res)

res <- lm(sentiment ~ TARGET + submethod , data = dfx[dfx$method=="preposition",])
summary(res)

top_words <- dfx %>% 
  filter(submethod == "of") %>% 
  group_by(TARGET, ADJ, sentiment) %>% 
  summarise(n = n()) %>% 
  ungroup %>% 
  group_by(TARGET) %>% 
  arrange(TARGET, desc(n)) %>% 
  slice_head(n = 50)
write.csv(top_words, file = "../output/dfs/top_words_OF_PREP_ONLY.csv")

wilcox.test(sentiment ~ TARGET, data = dfx[dfx$submethod=="of",], alternative = "less")

