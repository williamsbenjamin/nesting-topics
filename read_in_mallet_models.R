#load in the mallet models 

options(java.parameters = "-Xmx1024m")
library(mallet)
library(dfrtopics)
library(data.table)
library(tidyverse)
library(tidytext)
library(flexmix)
library(proxy)
library(Matrix)
library(textmineR)
library(RColorBrewer)
library(bigtabulate)
library(bigmemory)
library(stringr)
library(ggthemes)


#load in the mallet topic models
m4 <- load_from_mallet_state(
  mallet_state_file =  "topic_state_all_hans3_2_stops_4.gz",
  instances_file = "stemmed_20170517_3_2_m.mallet"
)

m20 <- load_from_mallet_state(
  mallet_state_file =  "topic_state_all_hans3_2_stops_20.gz",
  instances_file = "stemmed_20170517_3_2_m.mallet"
)

m100 <- load_from_mallet_state(
  mallet_state_file =  "topic_state_all_hans3_2_stops_100.gz",
  instances_file = "stemmed_20170517_3_2_m.mallet"
)

#gives a new function for reading in very big mallet files
source("big_load_mallet_function.R") #loads load_from_mallet_state_b500

m500 <- load_from_mallet_state_b500(
  mallet_state_file =  "topic_state_all_hans3_2_stops_500.gz",
  instances_file = "stemmed_20170517_3_2_m.mallet"
)

############
#save them
############

#get topic-word weights
topic.words4 <- topic_words(m4)
topic.words20 <- topic_words(m20)
topic.words100 <- topic_words(m100)
topic.words500 <- topic_words(m500)

#combine the topic models
topic.words.all <- rbind(topic.words4,
                         topic.words20,
                         topic.words100,
                         topic.words500)

#normalize the rows
topic.words.all.norm <- normalize_rows(topic.words.all,
                                       norm = "L1")

#get topic-to-topic JS divergences 
js.topic.dists.all.norm <- row_dists(topic.words.all.norm) 

js.topic.dists.all.norm <- as.data.frame(js.topic.dists.all.norm) #turn it into dataframe

#save it!
write.csv(js.topic.dists.all.norm,file="js.topic.dists.all.norm.csv")

kl.topic.dists.all.norm <- KLdiv(as.matrix(topic.words.all.norm)) 

kl.topic.dists.all.norm <- as.data.frame(kl.topic.dists.all.norm) #turn it into dataframe
#save it!
write.csv(kl.topic.dists.all.norm,file="kl.topic.dists.all.norm.m2.csv")





