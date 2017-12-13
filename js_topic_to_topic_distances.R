
#Read in the normalized topic distribution matrix



#get topic-to-topic JS divergences 
js.topic.dists.all.norm <- row_dists(topic.words.all.norm) #normalized
js.topic.dists.all.norm <- as.data.frame(js.topic.dists.all.norm) #turn it into dataframe

#save it!
write.csv(js.topic.dists.all.norm,file="js.topic.dists.all.norm.csv")
