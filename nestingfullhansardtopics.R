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
library(philentropy)

#Read in the distance matrix, could be JS could be KL
#test.js <- read_csv("js.topic.dists.all.norm.csv") #adds on a leading column
test.js <- read_csv("js.topic.dists.all.norm.m2.csv") #adds on a leading column
js.topic.dists.all.norm <- test.js %>%
                            select(-X1) #remove the first column

#subset js.topic.dists for 4 topics to 20 topics branch, normalized
js.topic.dists.sub.norm.4.20 <- js.topic.dists.all.norm %>% 
                                            slice(1:4) %>% 
                                            select(5:24)

#row 5 will contain the topic (1-4: from the 4 topic model) 
#that is closest to each column(5-24 : from the 20 topic model)

js.topic.dists.sub.norm.4.20[5,] <- apply(js.topic.dists.sub.norm.4.20,2,which.min) 
x.4.20 <- as.numeric(js.topic.dists.sub.norm.4.20[5,]) #the distribution of topic closeness

qplot(x.4.20,bins=4)#look at it

xx4.20 <- tibble(topic=c(1:4),counts=tabulate(x.4.20))
xx4.20 <- xx4.20 %>% 
              mutate(percents=counts/sum(counts))
xx4.20 <- xx4.20 %>% 
              mutate(percents=paste(percents*100,"%",sep=""))
xx4.20 %>% 
  ggplot(aes(y=counts,x=topic))+geom_bar(stat="identity",fill=xx4.20$topic) +
  scale_fill_brewer()+
  coord_polar() + 
  geom_text(aes(x=xx4.20$topic,y=2.2,label=xx4.20$topic),color="white",size=5) +
  geom_text(aes(x=xx4.20$topic,y=7.5,label=xx4.20$percents),color="black",size=6) +
  theme(axis.ticks.y=element_blank(),axis.text.y=element_blank(),axis.title.y = element_blank(),
        axis.ticks.x=element_blank(),axis.text.x=element_blank(),axis.title.x = element_blank())+
  labs(title="4 base topics in white, percent of 20 topics categorized within each of the 4 topics in Black")

xx4.20 %>% 
  ggplot(aes(y=percents,x=topic))+geom_bar(stat="identity") +
  theme_economist()+
  theme(axis.title.y = element_blank())+
  labs(title="Percent of 20 topics nested by Jensen-Shannon within each of the 4 topics") +
  xlab("topics from 4 topic model")

#experiment and look at 20 topics vs 100 topics, with normalized rows
js.topic.dists.sub.norm.20.100 <- js.topic.dists.all.norm %>% 
                                              slice(5:24) %>% 
                                              select(25:124)

#row 21 will contain the topic (5-24: from the 20 topic model) 
#that is closest to each column(25-124 : from the 100 topic model)
js.topic.dists.sub.norm.20.100[21,] <- apply(js.topic.dists.sub.norm.20.100,2,which.min)
x.20.100 <- as.numeric(js.topic.dists.sub.norm.20.100[21,])#the distribution of topic closeness

qplot(x.20.100,bins=10)#look at it


xx20.100 <- tibble(topic=c(1:20),counts=tabulate(x.20.100))
xx20.100 <- xx20.100 %>% 
  mutate(percents=counts/sum(counts))
xx20.100 <- xx20.100 %>% 
  mutate(percents=paste(percents*100,"%",sep=""))
xx20.100 %>% 
  ggplot(aes(y=counts,x=topic))+geom_bar(stat="identity",fill=xx20.100$topic) +
  scale_fill_brewer()+ coord_polar() + 
  geom_text(aes(x=xx20.100$topic,y=2.2,label=xx20.100$topic),color="white",size=5) +
  geom_text(aes(x=xx20.100$topic,y=8.5,label=xx20.100$percents),color="black",size=6) +
  theme(axis.ticks.y=element_blank(),axis.text.y=element_blank(),axis.title.y = element_blank(),
        axis.ticks.x=element_blank(),axis.text.x=element_blank(),axis.title.x = element_blank())+
  labs(title="20 branch topics in white, percent of 100 topics categorized within each of the 20 topics in Black")


xx20.100 %>% 
  ggplot(aes(y=percents,x=topic))+
  geom_bar(stat="identity") +
  theme_economist()+
  theme(axis.title.y = element_blank())+
  labs(title="Percent of 100 topics nested by Jensen-Shannon within each of the 20 topics") +
  xlab("topics from 20 topic model")


#experiment and look at 20 topics vs 100 topics, with normalized rows
js.topic.dists.sub.norm.100.500 <- js.topic.dists.all.norm %>% 
                                    slice(25:124) %>% 
                                    select(125:624)

#row 21 will contain the topic (5-24: from the 20 topic model) 
#that is closest to each column(25-124 : from the 100 topic model)
js.topic.dists.sub.norm.100.500[501,] <- apply(js.topic.dists.sub.norm.100.500,2,which.min)
x.100.500 <- as.numeric(js.topic.dists.sub.norm.100.500[501,])#the distribution of topic closeness
qplot(x.100.500,bins=30)#look at it


xx100.500 <- tibble(topic=c(1:100),counts=tabulate(x.100.500))
xx100.500 <- xx100.500 %>% 
                mutate(percents=counts/sum(counts))
xx100.500 <- xx100.500 %>% 
                mutate(percents=paste(percents*100,"%",sep=""))
xx100.500 %>% 
  ggplot(aes(y=counts,x=topic))+
  geom_bar(stat="identity",fill=xx100.500$topic) +
  scale_fill_brewer()+ 
  coord_polar() + 
  geom_text(aes(x=xx100.500$topic,y=2.2,label=xx100.500$topic),color="white",size=5) +
  geom_text(aes(x=xx100.500$topic,y=8.5,label=xx100.500$percents),color="black",size=6) +
  theme(axis.ticks.y=element_blank(),axis.text.y=element_blank(),axis.title.y = element_blank(),
        axis.ticks.x=element_blank(),axis.text.x=element_blank(),axis.title.x = element_blank())+
  labs(title="100 branch topics in white, percent of 500 topics categorized within each of the 20 topics in Black")

xx100.500 %>% 
  ggplot(aes(y=percents,x=topic))+
  geom_bar(stat="identity") +
  theme_economist()+
  theme(axis.title.y = element_blank())+
  labs(title="Percent of 500 topics nested by Jensen Shannon within each of the 100 topics") +
  xlab("topics from 100 topic model")




#rename the xx... tibbles with their topics, i.e.1-4,5-24,25-124,...

xx4.20.n <- xx4.20 %>%
              mutate(topic_name = c(1:4))
xx20.100.n <- xx20.100 %>%
                mutate(topic_name = c(5:24))
xx100.500.n <- xx100.500 %>%
                mutate(topic_name = c(25:124))

test.4.20 <- tibble(branch=c(5:24),root=x.4.20)
test.4.20 <- test.4.20 %>%
                unite("hier",c("root","branch"),sep="-",remove=F)
test.4.20$hier <- as.character(test.4.20$hier)

test.20.100 <- tibble(branch=c(25:124),root=(x.20.100+4))
test.20.100 <- test.20.100 %>%
                unite("hier",c("root","branch"),sep="-",remove=F)

test.100.500 <- tibble(branch=c(125:624),root=(x.100.500+4+20))
test.100.500 <- test.100.500 %>%
  unite("hier",c("root","branch"),sep="-",remove=F)

test.all <- bind_rows(test.4.20,test.20.100,test.100.500)

root1.branch1 <- test.4.20$hier[grep("1-",test.4.20$hier)]
root2.branch1 <- test.4.20$hier[grep("2-",test.4.20$hier)]
root3.branch1 <- test.4.20$hier[grep("3-",test.4.20$hier)]
root4.branch1 <- test.4.20$hier[grep("4-",test.4.20$hier)]


branch1.5.branch2 <- test.20.100$hier[grep("^5-",test.20.100$hier)]
branch1.6.branch2 <- test.20.100$hier[grep("^6-",test.20.100$hier)]
branch1.7.branch2 <- test.20.100$hier[grep("^7-",test.20.100$hier)]
branch1.8.branch2 <- test.20.100$hier[grep("^8-",test.20.100$hier)]
branch1.9.branch2 <- test.20.100$hier[grep("^9-",test.20.100$hier)]
branch1.10.branch2 <- test.20.100$hier[grep("^10-",test.20.100$hier)]
branch1.11.branch2 <- test.20.100$hier[grep("^11-",test.20.100$hier)]
branch1.12.branch2 <- test.20.100$hier[grep("^12-",test.20.100$hier)]
branch1.13.branch2 <- test.20.100$hier[grep("^13-",test.20.100$hier)]
branch1.14.branch2 <- test.20.100$hier[grep("^14-",test.20.100$hier)]
branch1.15.branch2 <- test.20.100$hier[grep("^15-",test.20.100$hier)]
branch1.16.branch2 <- test.20.100$hier[grep("^16-",test.20.100$hier)]
branch1.17.branch2 <- test.20.100$hier[grep("^17-",test.20.100$hier)]
branch1.18.branch2 <- test.20.100$hier[grep("^18-",test.20.100$hier)]
branch1.19.branch2 <- test.20.100$hier[grep("^19-",test.20.100$hier)]
branch1.20.branch2 <- test.20.100$hier[grep("^20-",test.20.100$hier)]
branch1.21.branch2 <- test.20.100$hier[grep("^21-",test.20.100$hier)]
branch1.22.branch2 <- test.20.100$hier[grep("^22-",test.20.100$hier)]
branch1.23.branch2 <- test.20.100$hier[grep("^23-",test.20.100$hier)]
branch1.24.branch2 <- test.20.100$hier[grep("^24-",test.20.100$hier)]


branch2.25.branch3 <- test.100.500$hier[grep("^25-",test.100.500$hier)]
branch2.26.branch3 <- test.100.500$hier[grep("^26-",test.100.500$hier)]
branch2.27.branch3 <- test.100.500$hier[grep("^27-",test.100.500$hier)]
branch2.28.branch3 <- test.100.500$hier[grep("^28-",test.100.500$hier)]
branch2.29.branch3 <- test.100.500$hier[grep("^29-",test.100.500$hier)]
branch2.30.branch3 <- test.100.500$hier[grep("^30-",test.100.500$hier)]
branch2.31.branch3 <- test.100.500$hier[grep("^31-",test.100.500$hier)]
branch2.32.branch3 <- test.100.500$hier[grep("^32-",test.100.500$hier)]
branch2.33.branch3 <- test.100.500$hier[grep("^33-",test.100.500$hier)]
branch2.34.branch3 <- test.100.500$hier[grep("^34-",test.100.500$hier)]
branch2.35.branch3 <- test.100.500$hier[grep("^35-",test.100.500$hier)]
branch2.36.branch3 <- test.100.500$hier[grep("^36-",test.100.500$hier)]
branch2.37.branch3 <- test.100.500$hier[grep("^37-",test.100.500$hier)]
branch2.38.branch3 <- test.100.500$hier[grep("^38-",test.100.500$hier)]
branch2.39.branch3 <- test.100.500$hier[grep("^39-",test.100.500$hier)]
branch2.40.branch3 <- test.100.500$hier[grep("^40-",test.100.500$hier)]
branch2.41.branch3 <- test.100.500$hier[grep("^41-",test.100.500$hier)]
branch2.42.branch3 <- test.100.500$hier[grep("^42-",test.100.500$hier)]
branch2.43.branch3 <- test.100.500$hier[grep("^43-",test.100.500$hier)]
branch2.44.branch3 <- test.100.500$hier[grep("^44-",test.100.500$hier)]
branch2.45.branch3 <- test.100.500$hier[grep("^45-",test.100.500$hier)]
branch2.46.branch3 <- test.100.500$hier[grep("^46-",test.100.500$hier)]
branch2.47.branch3 <- test.100.500$hier[grep("^47-",test.100.500$hier)]
branch2.48.branch3 <- test.100.500$hier[grep("^48-",test.100.500$hier)]
branch2.49.branch3 <- test.100.500$hier[grep("^49-",test.100.500$hier)]
branch2.50.branch3 <- test.100.500$hier[grep("^50-",test.100.500$hier)]
branch2.51.branch3 <- test.100.500$hier[grep("^51-",test.100.500$hier)]
branch2.52.branch3 <- test.100.500$hier[grep("^52-",test.100.500$hier)]
branch2.53.branch3 <- test.100.500$hier[grep("^53-",test.100.500$hier)]
branch2.54.branch3 <- test.100.500$hier[grep("^54-",test.100.500$hier)]
branch2.55.branch3 <- test.100.500$hier[grep("^55-",test.100.500$hier)]
branch2.56.branch3 <- test.100.500$hier[grep("^56-",test.100.500$hier)]
branch2.57.branch3 <- test.100.500$hier[grep("^57-",test.100.500$hier)]
branch2.58.branch3 <- test.100.500$hier[grep("^58-",test.100.500$hier)]
branch2.59.branch3 <- test.100.500$hier[grep("^59-",test.100.500$hier)]
branch2.60.branch3 <- test.100.500$hier[grep("^60-",test.100.500$hier)]
branch2.61.branch3 <- test.100.500$hier[grep("^61-",test.100.500$hier)]
branch2.62.branch3 <- test.100.500$hier[grep("^62-",test.100.500$hier)]
branch2.63.branch3 <- test.100.500$hier[grep("^63-",test.100.500$hier)]
branch2.64.branch3 <- test.100.500$hier[grep("^64-",test.100.500$hier)]
branch2.65.branch3 <- test.100.500$hier[grep("^65-",test.100.500$hier)]
branch2.66.branch3 <- test.100.500$hier[grep("^66-",test.100.500$hier)]
branch2.67.branch3 <- test.100.500$hier[grep("^67-",test.100.500$hier)]
branch2.68.branch3 <- test.100.500$hier[grep("^68-",test.100.500$hier)]
branch2.69.branch3 <- test.100.500$hier[grep("^69-",test.100.500$hier)]
branch2.70.branch3 <- test.100.500$hier[grep("^70-",test.100.500$hier)]
branch2.71.branch3 <- test.100.500$hier[grep("^71-",test.100.500$hier)]
branch2.72.branch3 <- test.100.500$hier[grep("^72-",test.100.500$hier)]
branch2.73.branch3 <- test.100.500$hier[grep("^73-",test.100.500$hier)]
branch2.74.branch3 <- test.100.500$hier[grep("^74-",test.100.500$hier)]
branch2.75.branch3 <- test.100.500$hier[grep("^75-",test.100.500$hier)]
branch2.76.branch3 <- test.100.500$hier[grep("^76-",test.100.500$hier)]
branch2.77.branch3 <- test.100.500$hier[grep("^77-",test.100.500$hier)]
branch2.78.branch3 <- test.100.500$hier[grep("^78-",test.100.500$hier)]
branch2.79.branch3 <- test.100.500$hier[grep("^79-",test.100.500$hier)]
branch2.80.branch3 <- test.100.500$hier[grep("^80-",test.100.500$hier)]
branch2.81.branch3 <- test.100.500$hier[grep("^81-",test.100.500$hier)]
branch2.82.branch3 <- test.100.500$hier[grep("^82-",test.100.500$hier)]
branch2.83.branch3 <- test.100.500$hier[grep("^83-",test.100.500$hier)]
branch2.84.branch3 <- test.100.500$hier[grep("^84-",test.100.500$hier)]
branch2.85.branch3 <- test.100.500$hier[grep("^85-",test.100.500$hier)]
branch2.86.branch3 <- test.100.500$hier[grep("^86-",test.100.500$hier)]
branch2.87.branch3 <- test.100.500$hier[grep("^87-",test.100.500$hier)]
branch2.88.branch3 <- test.100.500$hier[grep("^88-",test.100.500$hier)]
branch2.89.branch3 <- test.100.500$hier[grep("^89-",test.100.500$hier)]
branch2.90.branch3 <- test.100.500$hier[grep("^90-",test.100.500$hier)]
branch2.91.branch3 <- test.100.500$hier[grep("^91-",test.100.500$hier)]
branch2.92.branch3 <- test.100.500$hier[grep("^92-",test.100.500$hier)]
branch2.93.branch3 <- test.100.500$hier[grep("^93-",test.100.500$hier)]
branch2.94.branch3 <- test.100.500$hier[grep("^94-",test.100.500$hier)]
branch2.95.branch3 <- test.100.500$hier[grep("^95-",test.100.500$hier)]
branch2.96.branch3 <- test.100.500$hier[grep("^96-",test.100.500$hier)]
branch2.97.branch3 <- test.100.500$hier[grep("^97-",test.100.500$hier)]
branch2.98.branch3 <- test.100.500$hier[grep("^98-",test.100.500$hier)]
branch2.99.branch3 <- test.100.500$hier[grep("^99-",test.100.500$hier)]
branch2.100.branch3 <- test.100.500$hier[grep("^100-",test.100.500$hier)]
branch2.101.branch3 <- test.100.500$hier[grep("^101-",test.100.500$hier)]
branch2.102.branch3 <- test.100.500$hier[grep("^102-",test.100.500$hier)]
branch2.103.branch3 <- test.100.500$hier[grep("^103-",test.100.500$hier)]
branch2.104.branch3 <- test.100.500$hier[grep("^104-",test.100.500$hier)]
branch2.105.branch3 <- test.100.500$hier[grep("^105-",test.100.500$hier)]
branch2.106.branch3 <- test.100.500$hier[grep("^106-",test.100.500$hier)]
branch2.107.branch3 <- test.100.500$hier[grep("^107-",test.100.500$hier)]
branch2.108.branch3 <- test.100.500$hier[grep("^108-",test.100.500$hier)]
branch2.109.branch3 <- test.100.500$hier[grep("^109-",test.100.500$hier)]
branch2.110.branch3 <- test.100.500$hier[grep("^110-",test.100.500$hier)]
branch2.111.branch3 <- test.100.500$hier[grep("^111-",test.100.500$hier)]
branch2.112.branch3 <- test.100.500$hier[grep("^112-",test.100.500$hier)]
branch2.113.branch3 <- test.100.500$hier[grep("^113-",test.100.500$hier)]
branch2.114.branch3 <- test.100.500$hier[grep("^114-",test.100.500$hier)]
branch2.115.branch3 <- test.100.500$hier[grep("^115-",test.100.500$hier)]
branch2.116.branch3 <- test.100.500$hier[grep("^116-",test.100.500$hier)]
branch2.117.branch3 <- test.100.500$hier[grep("^117-",test.100.500$hier)]
branch2.118.branch3 <- test.100.500$hier[grep("^118-",test.100.500$hier)]
branch2.119.branch3 <- test.100.500$hier[grep("^119-",test.100.500$hier)]
branch2.120.branch3 <- test.100.500$hier[grep("^120-",test.100.500$hier)]
branch2.121.branch3 <- test.100.500$hier[grep("^121-",test.100.500$hier)]
branch2.122.branch3 <- test.100.500$hier[grep("^122-",test.100.500$hier)]
branch2.123.branch3 <- test.100.500$hier[grep("^123-",test.100.500$hier)]
branch2.124.branch3 <- test.100.500$hier[grep("^124-",test.100.500$hier)]

#str_sub(filter(test.20.100,root == 20)$hier,3)


#root topic to branch 2 to branch 3
#middle branches of 5-9
for(i in 1:5){
  assign(paste("hierarch",i+4,sep="_"),
   paste(filter(test.4.20,branch == i+4)$hier,
        str_sub(filter(test.20.100,root == i+4)$hier,3),sep="-")
  )
}

#middle branches of 10-24
for(i in 6:20){
  assign(paste("hierarch",i+4,sep="_"),
         paste(filter(test.4.20,branch == i+4)$hier,
               str_sub(filter(test.20.100,root == i+4)$hier,4),sep="-")
  )
}

#combine the 20 vectors into 1
hi.first <- list()
for(ii in 1:20){
  hi.first[[ii]]<-get(ls()[grep("^hierarch_",ls())][ii])
}

lev <- tibble(hi = unlist(hi.first))


#branch 3 to branch 4

#middle branches of 25-99
for(i in 1:75){
  assign(paste("arch",i+24,sep="_"),
         paste(filter(test.20.100,branch == i+24)$hier,
               str_sub(filter(test.100.500,root == i+24)$hier,4),sep="-")
  )
}

#middle branches of 100-
for(i in 76:100){
  assign(paste("arch",i+24,sep="_"),
         paste(filter(test.20.100,branch == i+24)$hier,
               str_sub(filter(test.100.500,root == i+24)$hier,5),sep="-")
  )
}

#combine the 100 vectors into 1
hi.second <- list()
for(ii in 1:100){
  hi.second[[ii]]<-get(ls()[grep("^arch_",ls())][ii])
}
lev.2 <- tibble(hi = unlist(hi.second))

###lev has the order of the hierarchy
###use lev.2 to count the number of the 500 topics within
###each root-branch-branch combination
lev.2$grouping <- str_sub(lev.2$hi,1,-5)

lev.2.groups <- lev.2 %>%
                  count(grouping,sort=T)
str_sub(lev[1,1],3)
lev$grouping <-str_sub(lev$hi,3)

#join the counted dataset (lev.2.groups) onto lev
#so we will have exactly what we need

lev.all <- lev %>%
            left_join(lev.2.groups)

#get the data in format sunburst likes
lev.sunburst <- lev.all %>%
                  select(-grouping)
write.csv(lev.sunburst,"lev.sunburst.csv") # this is used to make sunburst in "make_sunburst.R"


