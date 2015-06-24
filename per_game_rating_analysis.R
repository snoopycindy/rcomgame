setwd("/Users/yxhung/Dropbox/workspace/rcomgame/R" )
source("../R.src/common.R")
# source("../R.source/common.R")
# source("../R.source/my.fig.R")
# source("../R.source/my.legend.R")
# source("../R.source/rey.f.R")
source("../R.src/yx.common.R")
source("../R.src/route.R")

# source('a.R')
# plot game-rating (per game with all sub)
#read std

std={}
for(p in 1:len(p.list)){
  fn = paste(sdir, p.grp[p], "_", p.list[p],".csv",sep="")
  d = read.table(fn)
  d = cbind(p.list[p], d)
  std = rbind(std, d)
}



ratings = c("all", "speed", "content", "number", "distance", "space", "suitable")

names(std)[1] = "sub"
d = std[,c(1,5:12)]
dimnames(d)[[1]] = d$sub

std = read.csv("std.csv")
std = std[,-1]
d = std[,c(1,5:12)]

library(ggplot2)
lapply(1:len(ratings), function(r) { 
  fn  = paste(picdir, "pic_rating/", ratings[r], ".png", sep = "")
  # png(fn)
  qplot(code, d[,ratings[r]], facets = sub~.,
        data = d, 
        main=paste("rating =",ratings[r]))
  # dev.off()
})



require("fmsb")
library("fmsb")

lapply(gcode$gc, function(g){
  nowd = d[d$code==g,]
  dimnames(nowd)[[1]] = nowd$sub
  cat(g, "\n")
  palette(rainbow(7, s = 0.6, v = 0.8))
  stars(nowd[,3:9], len = 0.2, scale = F,
        main = g, draw.segments = TRUE, nrow = 2,
        key.loc = c(9.5, 1), mar = c(1, 1, 1, 1))
})

lapply(gcode$gc, function(g){
  nowd = d[d$code==g,]
  dimnames(nowd)[[1]] = nowd$sub
  cat(g, "\n")
  
  npic = len(unique(nowd$sub))
  layout(matrix(1:8, ncol=4), title(main = g)) 
  
  #loop over rows to draw them, add 7 as max and 1 as min for each var
  lapply(1:npic, function(i){ 
    radarchart(rbind(rep(1,7), rep(7,7), nowd[i,3:9]), 
               title = nowd$sub[i], seg=7)
  })
})
library(randomForest)
lapply(p.list, function(p){
  nowd = d[d$sub==p,]
  
  svm = svm(all ~ ., data = nowd[,c(3:4, 6:8)], type='C-classification',
            cross = 10)
  acc = summary(svm)$tot.accuracy
  pred = predict(svm, nowd[,c(4, 6:8)])
  table(nowd$all, pred)
  action = nowd$all
  success = sum(pred==action)/length(action)
  cat(p, ":", success, "\n")
})

lapply(unique(d$code), function(c){
  nowd = d[d$code==c,]
  cat(simil(nowd[,c(3:4, 6:8)]), "\n")
})





