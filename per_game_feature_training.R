setwd("e:/Dropbox/workspace/done_app_yx/R")
# source("../R.source/common.R")
# source("../R.source/my.fig.R")
# source("../R.source/my.legend.R")
source("../R.source/rey.f.R")
source("../R.source/yx.common.R")
source("../R.source/route.R")
library(proxy) # Library of similarity/dissimilarity measures for 'dist()'

#representive features
name.f = c("game", "func", "num.act", "frq.act", "itv.act", 
           "num.tap", "frq.tap", "ratio.tap", "dur.tap", "itv.tap",
           "num.sld", "frq.sld", "ratio.sld", "dur.sld", "itv.sld", 
           "dis.sld", "spd.sld", "spd_sd.sld")
fn = paste(fdir_rep, "/allgame.txt", sep="")
d_rep = read.table(fn)
names(d_rep) = name.f

fn = paste(fdir_rep, "/allgame_q70.txt", sep="")
d_rep70 = read.table(fn)
names(d_rep70) = name.f

# test data
name.f = c("game", "sub", "num.act", "frq.act", "itv.act", 
           "num.tap", "frq.tap", "ratio.tap", "dur.tap", "itv.tap",
           "num.sld", "frq.sld", "ratio.sld", "dur.sld", "itv.sld", 
           "dis.sld", "spd.sld", "spd_sd.sld", "rate")
fn = paste(fdir_test, "/allgame.txt", sep="")
d_test = read.table(fn)
names(d_test) = name.f

#find similarity between sub.feature and rep.feature (min/max/mean...)
dis_all = {}
sim_all = {}
for(g in d_rep$game){
  rep_g = d_rep[d_rep$game==g,]
  test_g = d_test[d_test$game==g,]
  rep = d_rep[d_rep$game==g,3:18]
  test = d_test[d_test$game==g,3:18]
  if(nrow(test)<=0)
    next
  
  dis = dist(test, rep)
  dis = cbind(g, as.character(test_g$sub), dis)
  dis_all = rbind(dis_all, dis)
  
  sim = simil(test, rep)
  sim = cbind(g,as.character(test_g$sub), sim)
  sim_all = rbind(sim_all, sim)
}

colnames(dis_all) = c("game","sub", funcs)
rownames(dis_all) = NULL
dis_all = as.data.frame(dis_all)
colnames(sim_all) = c("game","sub", funcs)
rownames(sim_all) = NULL
sim_all = as.data.frame(sim_all)

#plot the similarity between sub.feature and rep.feature (min/max/mean...) 
#per subject with rating
plot.f = function(d){
  plot(0, type='n', xlim=c(0,nrow(d)), ylim=c(0,5), main = s)
  legend("topleft", legend=colnames(d)[3:7], col=col_p, lwd=2)
  for(s in 3:7){
    lines(as.numeric(as.character(d[,s])), col=col_p[s])
  }
  lines(rate, lwd=2, col="#e66101") 
}

for(s in p.list){
  test = d_test[d_test$sub==s,]
  if(nrow(test)<=0)
    next
  rate = test$rate
  
  
  # plot dis====================
  dis = dis_all[dis_all$sub==s,]
  new_dis={}
  for(g in test$game){
    new_dis = rbind(new_dis, dis[min(which(dis$game==g)),])
  }
  new_dis = cbind(new_dis, rate)
  plot.f(new_dis)
  
  #plot sim=====================
  sim = sim_all[sim_all$sub==s,]
  new_sim={}
  for(g in test$game){
    new_sim = rbind(new_sim, sim[min(which(sim$game==g)),])
  }
  new_sim = cbind(new_sim, rate)
  plot.f(new_sim)
  
  
  #plot acc=====================
#   svm = svm(rate ~ min, data = new_dis, type='C-classification', cross = 10)
#   acc.min = summary(svm)$tot.accuracy
#   
# 
#   svm = svm(rate ~ mean, data = new_dis, type='C-classification', cross = 10)
#   acc.mean = summary(svm)$tot.accuracy
#   svm = svm(rate ~ median, data = new_dis, type='C-classification', cross = 10)
#   acc.med = summary(svm)$tot.accuracy
#   svm = svm(rate ~ mode, data = new_dis, type='C-classification', cross = 10)
#   acc.mode = summary(svm)$tot.accuracy
#   svm = svm(rate ~ max, data = new_dis, type='C-classification', cross = 10)
#   acc.max = summary(svm)$tot.accuracy
#   acc = c(acc.min, acc.mean, acc.med, acc.mode, acc.max)
#   plot(acc, type="p", main=s)
  
}



#find similarity between sub.feature and rep.feature (q70)
dis_all = {}
sim_all = {}
for(g in d_rep70$game){
  rep_g = d_rep70[d_rep70$game==g,]
  test_g = d_test[d_test$game==g,]
  rep = d_rep70[d_rep70$game==g,3:18]
  test = d_test[d_test$game==g,3:18]
  if(nrow(test)<=0)
    next
  
  dis = dist(test, rep)
  dis = cbind(g, as.character(test_g$sub), dis)
  dis_all = rbind(dis_all, dis)
  
  sim = simil(test, rep)
  sim = cbind(g,as.character(test_g$sub), sim)
  sim_all = rbind(sim_all, sim)
}

colnames(dis_all) = c("game","sub", "q70")
rownames(dis_all) = NULL
dis_all = as.data.frame(dis_all)
colnames(sim_all) = c("game","sub", "q70")
rownames(sim_all) = NULL
sim_all = as.data.frame(sim_all)

for(s in p.list){
  test = d_test[d_test$sub==s,]
  if(nrow(test)<=0)
    next
  rate = test$rate
  
  
  # plot dis====================
  dis = dis_all[dis_all$sub==s,]
  plot(0, type='n', xlim=c(0,nrow(dis)), ylim=c(0,5), main = s)
  lines(as.numeric(as.character(dis$q70)), col=col_p[2])
  lines(rate, lwd=2, col="#e66101") 

  
  #plot sim=====================
  sim = sim_all[sim_all$sub==s,]
  lines(as.numeric(as.character(sim$q70)), col=col_p[5])
  
  
  #plot acc=====================
  new_dis = cbind(dis, rate)
  svm = svm(rate ~ q70, data = new_dis, type='C-classification', cross = 10)
  acc.dis = summary(svm)$tot.accuracy
  new_sim = cbind(sim, rate)
  svm = svm(rate ~ q70, data = new_sim, type='C-classification', cross = 10)
  acc.sim = summary(svm)$tot.accuracy
  
  acc = c(acc.dis, acc.sim)
  plot(acc, type="l", main=s)
  
}

#training================XXXXXXXXXXXXXXXXXXXXXX
for(p in p.list){
  
  d = d_test[d_test$sub==p,]
  train = {}
  for(g in unique(d$game)){
    n_rep = d_rep70[d_rep70$game==g, 3:18]
    n_rating = as.numeric(d$rate[d$game==g])
    train = rbind(train, c(n_rep, d$rate[d$game==g]))
  }
  train = as.data.frame(train)
  names(train)[17]="rate"
  
  for(f in unique(funcs)){
    train = {}
    for(g in unique(d$game)){
      n_rep = d_rep[d_rep$func==f & d_rep$game==g, 3:18]
      n_rating = as.numeric(d$rate[d$game==g])
      train = rbind(train, c(n_rep, n_rating))
    }
    train = as.data.frame(train)
    names(train)[17]="rating"
  }
  
  train = d[,-(1:2)]
  model <- randomForest(train[,-len(train)])
  print(model)
  round(importance(model), 2)
  
  mds <- cmdscale(1 - model$proximity, eig=TRUE)
  op <- par(pty="s")
  pairs(cbind(train[,-len(train)], mds$points), cex=0.6, gap=0,
        col=c(1:5)[as.numeric(train$rate)],
        main="")
  par(op)
  print(mds$GOF)
  MDSplot(model, train$rate, main=paste(p, f))
    
  fit <- glm(rate~.,data=train)
  summary(fit) # display results
  confint(fit) # 95% CI for the coefficients
  exp(coef(fit)) # exponentiated coefficients
  exp(confint(fit)) # 95% CI for exponentiated coefficients
  predict(fit, type="response") # predicted values
  residuals(fit, type="deviance") # residuals
  anova(fit, test="Chisq")
  
  svm = svm(rate ~ ., data = train, 
            type='C-classification', cross = 10)
  acc = summary(svm)$tot.accuracy
  
}


