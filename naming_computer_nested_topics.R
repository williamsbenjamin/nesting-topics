library(tidyverse)
library(readxl)

lev_sunburst <- read_csv("lev.sunburst.csv") #read in the sunburst csv

lev_sunburst <- lev_sunburst %>%
                  select(-X1)

broken_up <- lev_sunburst %>%
  separate(hi,c("root","branch1","branch2"),sep="-")

broken_up <-broken_up %>%
  mutate(root = as.numeric(root),
         branch1 = as.numeric(branch1),
         branch2 = as.numeric(branch2))

root_names <- read_xlsx("topic_names.xlsx",sheet=2,col_names = F)#make into a csv

root_names <- root_names %>%
  rename("root" = X__1 , "name" = X__2) #make into a csv

branch1_names <- read_xlsx("topic_colors.xlsx",sheet=3,col_names = F) #make into a csv

branch1_names <- branch1_names %>%
  rename("branch1" = X__1 , "name1" = X__2)
branch2_names <- read_xlsx("topic_colors.xlsx",sheet=4,col_names = F)
branch2_names <- branch2_names %>%
  rename("branch2" = X__1 , "name2" = X__2)


broken_up_with_names <- broken_up %>%
  left_join(root_names) %>%
  left_join(branch1_names) %>%
  left_join(branch2_names)

closed_with_names<- broken_up_with_names %>%
  unite("closed",c("name","name1","name2"),sep="-")

lev_sunburst_with_names <- closed_with_names %>%
  select(closed,n) %>%
  rename("hi"= closed)

write.csv(lev_sunburst_with_names,"lev_sunburst_with_names.csv")
