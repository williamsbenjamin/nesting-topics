library(dfrtopics)

#Read in the normalized topic distribution matrix
topic.words.all.norm<- read_csv("topic.words.all.norm.csv")

#get topic-to-topic JS divergences 
js.topic.dists.all.norm <- row_dists(topic.words.all.norm, 
                                     g= JS_divergence) 

js.topic.dists.all.norm <- as.data.frame(js.topic.dists.all.norm) #turn it into dataframe

#save it!
write.csv(js.topic.dists.all.norm,file="js.topic.dists.all.norm.csv")
