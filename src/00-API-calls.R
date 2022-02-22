# !diagnostics off

## ---------------------------
##
## Script name: 00b-sweep.R
##
## Project: Tracing Thick Concepts
##
## Purpose of script: Handler for API calls
##
## Author: Lucien Baumgartner
##
## Date created: 13.09.2020
##
## Email: lucienbaumgartner@philos.uzh.ch
##
## ---------------------------
##
## Notes:
##    DO NOT RUN
##
## ---------------------------

## ---------------------------
######## 1 Libraries #########
## ---------------------------
library(dplyr)
library(pbmcapply)
library(httr)
library(lubridate)
library(anytime)
library(stringi)
library(stringr)
library(rlist)
library(Hmisc)
library(spacyr)
library(jsonlite)
library(utc)
rm(list=ls())

## ---------------------------
## 2 Set working directory ###
## ---------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#setwd('~/tc_methods_paper/src')
getwd()

## ---------------------------
######## 3 Load data #########
## ---------------------------
search.terms <- read.table('../input/words.txt', header = T, stringsAsFactors = F, sep=',')
search.terms <- 
  tibble(word=c(paste0('%22', capitalize(search.terms$word), '%20and%22'), 
                paste0('%22and%20',capitalize(search.terms$word),'%20of%22'),
                paste0('%22and%20',capitalize(search.terms$word),'%20that%22')))

## ---------------------------
########### 4 UDFs ###########
## ---------------------------
## formulate the regex to extract specific regex matches from responses
## according to PoS-tags
syntax.regex <- '(ADV\\s)?ADJ\\s(PUNCT\\s)?CCONJ\\s(ADV\\s)?ADJ'
make_regex <- function(INDEX){
  TARGET = paste0('\\\\b',df$TARGET[INDEX], '\\\\b')
  LEMMA = paste0(txtparsed[[INDEX]], collapse = ' ')
  SYNTAX = paste0(names(txtparsed[[INDEX]]), collapse = ' ')
  SYNTAX <- unlist(str_extract(SYNTAX, syntax.regex))
  SYNTAX <- unique(SYNTAX)
  tmp <- str_replace_all(SYNTAX, c(
    'ADV' = '\\\\w+',
    'CCONJ' = '(and|but)',
    'PUNCT' = '\\\\,'
  ))
  regex1 <- sub('ADJ', TARGET, tmp)
  regex1 <- str_replace_all(regex1, c(
    ' ' = '\\\\s',
    'ADJ' = '\\\\w+'
  ))
  regex2 <- stri_replace_last(tmp, replacement = TARGET, regex = 'ADJ')
  regex2 <- str_replace_all(regex2, c(
    ' ' = '\\\\s',
    'ADJ' = '\\\\w+'
  ))
  match1 <- unlist(str_extract_all(LEMMA, regex1))
  match2 <- unlist(str_extract_all(LEMMA, regex2))
  MATCH <- unlist(c(match1, match2))
  tryCatch(tmp <- lapply(MATCH, function(y){
    POS <- spacy_parse(y, pos = T)
    if(paste0(POS$pos, collapse = ' ') %in% SYNTAX){
      ADJ <- filter(POS, pos == 'ADJ' & !lemma == df$TARGET[INDEX])$lemma
      TARGET_mod <- filter(POS, (pos == 'ADJ' & lemma == df$TARGET[INDEX]) | pos == 'ADV')
      TARGET_mod <- TARGET_mod$lemma[TARGET_mod$pos == 'ADV' & TARGET_mod$token_id == TARGET_mod$token_id[TARGET_mod$pos == 'ADJ'] - 1]
      TARGET_mod <- ifelse(identical(TARGET_mod, character(0)), NA, TARGET_mod)
      
      ADV <- filter(POS, (pos == 'ADJ' & !lemma == df$TARGET[INDEX]) | pos == 'ADV')
      ADV <- ADV$lemma[ADV$pos == 'ADV' & ADV$token_id == ADV$token_id[ADV$pos == 'ADJ'] - 1]
      ADV <- ifelse(identical(ADV, character(0)), NA, ADV)
      
      first <- filter(POS, pos == 'ADJ')
      first <- ifelse(first$token_id[first$lemma == df$TARGET[INDEX]] < first$token_id[!first$lemma == df$TARGET[INDEX]], 1, 0)
      dta <- tibble(match = y, ADJ, TARGET_mod, ADV, first)
      return(dta)
    }
  }), warning = function(e) print(INDEX))
  if(is.list(tmp)) tmp <- do.call(rbind, tmp)
  if(is.list(tmp)|is.null(tmp)) return(tmp)
}

## ---------------------------
##### 4 Formulate Calls ######
## ---------------------------
Lurls <- lapply(search.terms$word, function(x){
  paste0(
    'https://api.pushshift.io/reddit/search/comment/?q=',
    x,
    '&before='
  )})

names(Lurls) <- search.terms$word

## ---------------------------
######## 5 Do Calls ##########
## ---------------------------
## Starting from t1, we make calls for every URL
## until we have 5000 usable observations.
## Each call uses the datetime of the last call 
## as a starting point. If the calls do not
## produce usable observations, we induce
## a time jump of a day (480000 utc units).
for(m in search.terms$word){
  #m <- search.terms$word[1]
  urls <- Lurls[[m]] # extract root of API call
  print(m)
  ## ---------------------------
  ## Reset parameters used in the loop
  rowLOG <- T # maintains the outer while loop
  its <- 0 # iteration index
  time.index <- toUTC(as.Date('01-01-2022', '%d-%m-%Y')) # time index
  time.index <- as.numeric(time.index)
  BREAK.COUNTER.OLD <- 0  # index to decide whether to induce time jump
  BREAK.COUNTER.NEW <- 0  # indices to decide whether to induce time jump
  
  ## ---------------------------
  ## Make calls until 5000 observations are collected
  while(rowLOG){
    ## ---------------------------
    ## Complete API request
    if(its == 0){geturl <- paste0(urls, time.index)}else{
      if(BREAK.COUNTER.OLD < BREAK.COUNTER.NEW){
        time.index <- time.index - 480000
      }else{
        if(min(df$created_utc) == Inf){
          time.index <- time.index - 480000
        }else{
          time.index <- min(df$created_utc)
        }
      }
      print(time.index)
      geturl <- paste0(urls, time.index)
    }
    
    ## ---------------------------
    ## Actual API call
    TRYING <- T
    while(TRYING){
      response <- try(jsonlite::fromJSON(geturl))
      TRYING <- 'try-error' %in% class(response)
    }
    ## Coerce to tibble
    df <- as_tibble(response$data)
    ## If the API call is empty, skip to next url
    if(nrow(df)==0) break
    ## Test whether all the variables we need are present
    if(!'body'%in%names(df)){
      df <- mutate(df, body = NA)
    }else if(!'id'%in%names(df)){
      df <- mutate(df, id = NA)
    }else if(!'created_utc'%in%names(df)){
      df <- mutate(df, created_utc = NA)
    }
    ## Mutate df
    df <- rename(df, txt = body)
    df <- select(df, id, txt, created_utc)
    
    ## ---------------------------
    ## Text mining to filter out the target structures BY LEXICAL CONTENT
    .word <- tolower(stri_extract(regex='(?<=22)(.*)(?=\\%20)',str=urls)) # regex
    .lookup <- paste0(paste0('(\\w+(\\,)?\\s\\band\\b\\s\\b', .word, '\\b', ')|(\\b', .word, '\\b'), '(\\,)?\\s\\band\\b\\s\\w+)', collapse = '|')
    
    ## Mining Operation
    corpus <- mclapply(unlist(df$txt), function(x){
      tmp <- tokenizers::tokenize_sentences(x)
      tmp <- unlist(tmp)
      tmp <- tolower(tmp)
      tmp <- tmp[grepl(.lookup, tolower(tmp), perl = T)]
      tmp <- unname(tmp)
      return(tmp)
    }, mc.cores = 4)
    
    ## Reduce initial response according to the occurence of target structures (nrow X n matches)
    df <- cbind(df[rep(1:nrow(df), lengths(corpus)),], corpus=unlist(corpus)) %>% as_tibble
    rm(corpus)
    df <- mutate(df, corpus = as.character(corpus))
    
    ## Extract matches and add them to df
    reg_matches <- mclapply(df$corpus, function(x) stringr::str_extract_all(x, .lookup), mc.cores=4)
    reg_matches <- unlist(reg_matches, recursive=F)
    df <- cbind(df[rep(1:nrow(df), lengths(reg_matches)),], match=unlist(reg_matches)) %>% as_tibble
    df <- mutate(df, match = as.character(match))
    df <- as_tibble(df)
    
    ## Mutate df (mainly unlisting match metadata)
    df <- mutate(df, TARGET = strsplit(gsub('\\,', '', match), '\\s'))
    df <- mutate(df, CCONJ = sapply(TARGET, function(x) return(x[x %in% c('and')][1])))
    df <- mutate(df, TARGET = sapply(df$TARGET, function(x) return(x[x %in% .word][1])))
    df <- mutate(df, comma = grepl('\\,', match))
    
    ## ---------------------------
    ## PoS-tag filtering
    ## Annotate text with PoS-tags
    txtparsed <- spacy_parse(tolower(df$corpus), pos = TRUE)
    txtparsed <- split(txtparsed, txtparsed$doc_id, lex.order = F)
    txtparsed <- txtparsed[gtools::mixedsort(names(txtparsed))]
    txtparsed <- mclapply(txtparsed, function(x) x$lemma %>% setNames(., x$pos), mc.cores = 4)

    ## Inspect target structures and filter out the false positives
    txtparsed_adj <- mclapply(1:length(txtparsed), make_regex, mc.cores=4)
    
    ## Expand df to accomodate the finalized matching
    reps <- unlist(lapply(sapply(txtparsed_adj, nrow), function(x) ifelse(is.null(x), 0, x)))
    df <- df[rep(1:nrow(df), reps),]
    txtparsed_adj <- do.call(rbind, txtparsed_adj)
    df <- rename(df, match_first = match)
    df <- cbind(df, txtparsed_adj)
    df <- as_tibble(df)
    
    ## Last checks
    df <- filter(df, TARGET%in%.word) # only targets
    df <- df[,!sapply(df[,1:length(df)], is.list)] # filter out list variables
    
    ## ---------------------------
    ## Saving process and meta-updates
    out <- paste0('../output/data/', m, '.csv')
    ## If file doesn't exist, make it
    if(!file.exists(out)){
      write.csv(df, file=out, row.names = F)
      rowLOG <- T
    }else{
      ## If df has no rows after the matching, it means that the current 100 API responses do not contain target structures and we have to initiate time jump
      if(nrow(df) == 0){
        BREAK.COUNTER.NEW <- BREAK.COUNTER.OLD + 1
        if(BREAK.COUNTER.NEW - BREAK.COUNTER.OLD > 1) BREAK.COUNTER.OLD <- (BREAK.COUNTER.NEW - 1)
      }
      ## Append data
      df2 <- read.csv(out) %>% as_tibble
      df <- rbind(df2, df)
      df <- filter(df, !duplicated(id))
      write.csv(df, file=out, row.names = F)
      print(nrow(df))
      ## Update the log for the while loop
      rowLOG <- !nrow(df) >= 100
    }
    ## Udate iteration index
    its <- its + 1
  }
}
