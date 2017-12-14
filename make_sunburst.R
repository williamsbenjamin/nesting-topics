library(sunburstR)

lev.sunburst <- read_csv("lev.sunburst.csv")
lev.sunburst <- lev.sunburst %>%
                  select(-X1)

sunburst(lev.sunburst)
write.csv(lev.sunburst,"lev.sunburst.csv",row.names=FALSE)
#write.csv(lev.sunburst,"C:/Users/32443181/Box Sync/Digital Land Wars Research/code/sunburst/500-Computer-Nested-Topics/lev.sunburst.csv",row.names=FALSE)
