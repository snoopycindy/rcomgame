setwd("e:/Dropbox/workspace/rcomgame/R")
# source("../R.source/common.R")
# source("../R.source/my.fig.R")
# source("../R.source/my.legend.R")
# source("../R.source/rey.f.R")
source("../R.src/yx.common.R")
source("../R.src/route.R")

# source('a.R')
# plot game-rating (per game with all sub)
#read std
std = {}
for(p in p.list){
  fn = paste("../gestureLog/gameStd/", p, ".csv", sep="")
  d = read.table(fn)
  d = cbind(p, d)
  std = rbind(std, d)
}
ratings = c("all", "speed", "content", "number", "distance", "space", "suitable")

d = std
for(r in 1:len(ratings)){
  plot(d$code, d[,ratings[r]], main=paste("rating =",ratings[r]))
  cat(ratings[r],"\n")
  
  xyplot(d[,ratings[r]] ~ code | p, data = d, 
         layout = c(1, len(unique(d$p))),
         type="p", main=paste("rating =",ratings[r]))
  
  qplot(d$code, d[,ratings[r]], data=d, main=paste("rating =",ratings[r]))
}

