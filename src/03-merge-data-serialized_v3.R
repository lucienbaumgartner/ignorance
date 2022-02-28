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
library(udpipe)
library(stringi)
library(xlsx)

rm(list =ls())

## ---------------------------
## 2 Set working directory ###
## ---------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

## ---------------------------
######## 3 Load data #########
## ---------------------------
## load corpus data
df <- lapply(list.files("../output/data/", full.names = T), function(x){read.csv(x, stringsAsFactors = F)})
nms <- rep(list.files("../output/data/"), sapply(df, nrow))
df <- do.call(rbind, df)
df <- cbind(df, nms)
df <- df %>% filter(CCONJ == 'and', !duplicated(id))
unique(df$nms)
## get animacy data
# get names of annotated comments
path <- '../output/animacy-annotation/animacy/'
files <- list.files(path = path, pattern = 'conllu$')
ids <- gsub('\\..*', '', files)
head(ids)
## subset corpus data to those instances which have been annotated
data <- df %>% filter(id %in% ids)
## checks
nrow(data) == length(ids)
all(ids %in% data$id)

ofThat <- data[grepl("%20that%22|%20of%22", data$nms),]

files <- files[gsub('\\..*', '', files) %in% data$id[!grepl("%20that%22|%20of%22", data$nms)]]
table(data$nms)
data <- data[!grepl("%20that%22|%20of%22", data$nms),]
table(data$nms)

#### ----------------------------------------------------------- ####
#################### Outer Loop ################################
#### ----------------------------------------------------------- ####

container <- list() # create empty container
## define problematic cases
# manual example
#prblm <- c('dvbnpoo', 'e9udx81', 'due6u5c', 'dsla5rh', 'dwz6vlo') 
#files <- paste0(prblm, '.conllu')
# automatic example
#files <- files[!files %in% names(container)]
#pb = txtProgressBar(min = 0, max = length(files), initial = 0, style = 3) 
pb = txtProgressBar(min = 0, max = length(files), initial = 0, style = 3) 
#for(file_index in 1:length(files)){
start = Sys.time()
for(file_index in 1:length(files)){
  #file_index = 1
  setTxtProgressBar(pb,file_index)
  file <- files[file_index] # chose specific file if needed
  if(data$comma[data$id == gsub('\\..*', '', file)]) next # only do the predefined tasks
  
  ## ----------------------------
  # Join Depparse and Refchains #
  ## ----------------------------
  ## read the stanza dependency parse (depparse)
  #depparse_file <- paste0('/Users/lucienbaumgartner/phd/projects/done/tc_methods_paper/output/animacy-annotation/animacy/', file)
  depparse_file <- paste0('../output/animacy-annotation/depparse/', file)
  depparse <- udpipe_read_conllu(depparse_file)
  depparse$token_id_ff <- 1:nrow(depparse)
  tail(depparse)
  ## read the animacy annotation and join with depparse
  animannot <- paste0('../output/animacy-annotation/animacy/', file)
  anim <- readLines(animannot)
  anim <- anim[-c(1, length(anim), length(anim)-1)] # ignore first and last two lines (conll format stuff)
  anim <- strsplit(anim, split = '\\\t')
  anim <- as.data.frame(do.call(rbind, anim)) %>% setNames(., c('token_id_ff', 'token', 'group'))
  anim$token_id_ff <- as.numeric(anim$token_id_ff) + 1
  anim <- left_join(depparse, anim, by = c("token", "token_id_ff"))
  
  ## ----------------------------
  ######## Unwrap Chains ########
  ## ----------------------------
  chains <- anim$group
  for(i in 1:length(chains)){
  if(grepl('\\(', chains[i]) & !grepl('\\)', chains[i])){
    chain_numbers <- stri_extract(chains[i], regex = '[0-9]+')
    chain_end <- paste0(stri_extract(chains[i], regex = '[0-9]+'), '\\)')
    new_index <- i + 1
    while(!grepl(chain_end, chains[new_index])){
      if(new_index>length(chains)) break
      empty <- gsub('_', '', chains[new_index])
      if(empty==""){
        chains[new_index] <- chain_numbers
      }else{
        chains[new_index] <- paste0(chains[new_index], ',', chain_numbers)
      }
      new_index <- new_index + 1
    }
  }
  }
  chains
  anim$chains <- chains
  
  ## ----------------------------
  ##### Extract Dependency ######
  ## ----------------------------
  ## get the initial ADJ AND ADJ match from the corpus data
  .match <- unlist(strsplit(data[data$id==gsub('\\..*', '', file),]$match_first, '\\s'))
  ## preprocessing of variables
  anim$token_id <- as.numeric(anim$token_id)
  anim$head_token_id <- as.numeric(anim$head_token_id)
  groups <- anim[anim$token_id == 1,]$token_id_ff
  anim$sentence_id <- rep(1:length(groups), c(na.omit(lead(groups) - groups), nrow(anim)+1-groups[length(groups)]))
  ## find the headtoken of the first conjunct
  headtok <- anim %>% 
    mutate(token = tolower(token)) %>% 
    filter(token == .match[1] & lead(token) == .match[2] & lead(token, 2) == .match[3] |
           token == .match[2] & lag(token) == .match[1] & lead(token) == .match[3] |
           token == .match[3] & lag(token,1) == .match[2] & lag(token, 2) == .match[1]
          ) %>% 
  slice(1) %>% 
  select(head_token_id, sentence_id, token_id)
  if(nrow(headtok)==0) next # skip if no headtoken could be found
  ##-------------------------------------------------------------
  ## find the subject of the predication (SuP) based on headtoken
  ##-------------------------------------------------------------
  ## subset target sentence
  t_sentence <- anim %>% filter(sentence_id == headtok$sentence_id)
  ## specify initial data frame object
  nsubj <- data.frame()
  ## define filter conditions used in while loops
  hierarch_filter_conds <- rev(c('xcomp', 'obl', 'obj', 'nmod(?!\\:poss)', 'attr', 'nsubj|nsubj\\:pass'))
  obl_hierarch_filter_conds <- c('obj', 'nmod(?!\\:poss)', 'attr', 'nsubj|nsubj\\:pass')
  ## FIRST STEP: fetch cases where the target adjectives is the head of the SuP
  adj_as_head <- t_sentence %>% filter(head_token_id == headtok$token_id)
  index <- 1
  while(nrow(nsubj)==0 & index<=length(hierarch_filter_conds)){
    condition <- hierarch_filter_conds[index] # we use a subject over object approach here
    nsubj <- adj_as_head %>% filter(grepl(condition, dep_rel, perl = T))
    index <- index+1
  }
  ## if no SuP could be found, move to next step
  if(nrow(nsubj)==0){
    ## SECOND STEP: fetch cases where the SuP is the head of the adjective (inverse case)
    ## Here we have to monitor our search for the head of the adjective
    ## We do not want accept the following as heads: ADJ, ADV, (VERB whose head is another VERB)
    jumped_head <- F # normally we don't have to jump the head
    adj_head <- t_sentence %>% filter(token_id == headtok$head_token_id) # get head of adjective (<headtok$head_token_id>)
    ## I will refer to this head as the "active head" in the following
    if(!nrow(adj_head)==0){
      ## If the active head as defined in <headtok> is ADJ or ADV, we move on to their respective head and overwrite the active head
      if(adj_head$upos%in%c('ADJ', 'ADV')){
        adj_head <- t_sentence %>% filter(token_id == adj_head$head_token_id)
        jumped_head <- T # mark that we jumped the head, as this implies a different treatment later on
      ## If the active as defined in <headtok> is VERB, we move on to their respective head and overwrite the active head
      }else if(adj_head$upos=='VERB' & !adj_head$head_token_id==0){
        adj_head <- try(t_sentence %>% filter(token_id == t_sentence$token_id[t_sentence$token_id == adj_head$head_token_id]), silent = T)
        if('try-error'%in%class(adj_head)) break # VERBs sometimes have the root a headtoken, which has the id==0, in that case we abort
        ## if the new head is - again - a VERB, we use the second VERB as head
        if(!adj_head$upos=='VERB') adj_head <- t_sentence %>% filter(token_id == headtok$head_token_id)
        jumped_head <- T # see above
      }
      index <- 1
      while(nrow(nsubj)==0 & index<=length(hierarch_filter_conds)){
        condition <- hierarch_filter_conds[index]
        nsubj <- adj_head %>% filter(grepl(condition, dep_rel, perl = T) & !upos=='ADJ') # same conditions as above, but we explicitly cancel out ADJ
        index <- index+1
      }
    }
    ## if no SuP could be found, move to next step
    if(nrow(nsubj)==0){
      ## THIRD STEP: fetch cases where the SuP has the same active head as the adjective
      if(!jumped_head){
        ## if we did NOT jump heads, we use the initial head
        same_head <- t_sentence %>% filter(head_token_id == headtok$head_token_id)
      }else{
        ## if we did, we use the currently active head (usually a VERB)
        same_head <- try(t_sentence %>% filter(head_token_id == adj_head$token_id), silent = T)
        if('try-error'%in%class(same_head)) next
      }
      index <- 1
      while(nrow(nsubj)==0 & index<=length(hierarch_filter_conds)){
        ## if the adjective itself is NOT in an adjective clause (i.e. the adjective has the dependecy relation "obl" or "xcomp")..
        if(!t_sentence$dep_rel[t_sentence$token_id==headtok$token_id]%in%c('obl', 'xcomp')){
          ## we use the initial hierarchy
          condition <- hierarch_filter_conds[index]
        }else{
          ## otherwise, we need to adapt the hierarchy of the filtering to passive clause (object over subject approach)
          condition <- obl_hierarch_filter_conds[index]
        }
        nsubj <- same_head %>% filter(grepl(condition, dep_rel, perl = T))
        index <- index+1
      }
    }
  }
  nsubj
  
  ## ----------------------------
  ###### Extract Referent #######
  ## ----------------------------
  ## find the referent based on the chain index of the pred subj in the animacy html
  mchain <- as.numeric(unlist(str_split(nsubj$chains, '\\(|\\)|\\,')))
  mchain <- min(unlist(na.omit(mchain)))
  if(identical(mchain, Inf)) next
  referent <- paste0('#referent_', mchain)
  html_file <- paste0(animannot, '.html')
  doc <- read_html(html_file)
  refinfo <- doc %>% html_nodes(referent) %>% html_attr('title')
  refinfo <- gsub('\\\n', ' | ', refinfo)
  refmeta <- unlist(stri_extract_all(refinfo, regex='(?<=:\\s)(\\w+(\\s\\w+)*|\\s)')[1])
  nms <- paste0('dep_', unlist(stri_extract_all(refinfo, regex='\\w+(?=\\:\\s)')[1]))
  if(length(nms)==length(refmeta)) names(refmeta) <- nms
  refmeta <- enframe(x = refmeta) %>% spread(key = name, value = value)
  refmeta$id <- gsub('\\..*', '', file)
  refmeta
  
  ## ----------------------------
  ###### SOME POSTPROCESSING #######
  ## ----------------------------
  container[[file]] <- refmeta
}
end = Sys.time()
difftime(start, end)
close(pb)
warnings()
length(container)

table(lengths(container))
container <- container[lengths(container)==10]
annot <- do.call(plyr::rbind.fill, container)
annot <- select(annot, -c(`1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`))
data_annot <- left_join(data, annot)
head(data_annot)
table(data$nms)
data_annot <- plyr::rbind.fill(data_annot, ofThat)
tail(data_annot)
save(data_annot, file = '../output/corpus/corpus.RDS')
