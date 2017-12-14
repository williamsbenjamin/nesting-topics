library(dfrtopics)

#Read in the normalized topic distribution matrix
topic.words.all.norm<- read_csv("topic.words.all.norm.csv")

#get topic-to-topic JS divergences 
kl.topic.dists.all.norm <- row_dists(topic.words.all.norm, 
                                     g= KLdiv) 

kl.topic.dists.all.norm <- as.data.frame(kl.topic.dists.all.norm) #turn it into dataframe

#save it!
write.csv(kl.topic.dists.all.norm,file="kl.topic.dists.all.norm.csv")
