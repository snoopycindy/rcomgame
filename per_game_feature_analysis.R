setwd("e:/Dropbox/workspace/done_app_yx/R")
setwd("/Users/yxhung/Dropbox/workspace/rcomgame/R")
# source("../R.source/common.R")
# source("../R.source/my.fig.R")
# source("../R.source/my.legend.R")
source("../R.src/rey.f.R")
source("../R.src/yx.common.R")


#remove the lines in this dataframe that contain NAs across all columns
delete_NA = function(d){
  d[complete.cases(d),]
}


funcs = c("min", "mean", "median", "max", "mode")

fn = paste(fdir_yx, "/allgame_rating.txt", sep="")
d_yx_rating = read.table(fn)
d_yx_rating = delete_NA(d_yx_rating)


f.list = c(1:27, 29) #game sub
d_test_yx = d_yx_rating[,f.list]
d_test_yx = delete_NA(d_test_yx)
# normalize feature
# d_test_yx[,3:18] = apply(d_test_yx[,3:18], 2, normalize.vector)


# read test data
name.f = c("game", "sub", "num.act", "frq.act", "itv.act", 
           "num.tap", "frq.tap", "ratio.tap", "dur.tap", "itv.tap",
           "num.sld", "frq.sld", "ratio.sld", "dur.sld", "itv.sld", 
           "dis.sld", "spd.sld", "spd_sd.sld", "action")
fn = paste(fdir_test, "/allgame.txt", sep="")
d_test = read.table(fn)
names(d_test) = name.f

#=========test=================
d = d_yx_rating
for(i in 3:27){
  xyplot(all ~ d[,i] | sub, data = d, 
         layout = c(1, len(unique(d$sub))),
         type="p", main=paste("rating vs",names(d)[i]))
}

#kmeans
require(graphics)
x = as.matrix(d_yx_rating[,c(4:27)])
k = 7
cl <- kmeans(x, k)
summary(cl)

d_gn = cbind(d_yx_rating[,c(1:2,4:34)], cl$cluster)

plot(x, col = cl$cluster, pch = d_gn$all)
points(cl$centers, col = 1:7, pch = 8, cex = 2)



colnames(d_gn)[len(d_gn)] = "gn"
xyplot(gn+all ~ game | sub, data = d_gn, 
       layout = c(1, len(unique(d_gn$sub))),
       type="p", main=paste("class = kmeans"))




#randomforest====
train = d_gn[,-c(25:31)]
train = d_gn
model <- randomForest(train[,-len(train)])
print(model)
round(importance(model), 2)
mds <- cmdscale(1 - model$proximity, eig=TRUE)
op <- par(pty="s")
pairs(cbind(train[,-len(train)], mds$points), cex=0.6, gap=0,
      col=c(1:7)[as.numeric(train$gn)],
      main="")
par(op)
print(mds$GOF)
model <- randomForest(gn ~ ., train, proximity=TRUE, keep.forest=FALSE)
MDSplot(model, train$gn, palette=c(1:7))
MDSplot(model, train$all, palette=c(1:7), pch=train$gn)

#Logistic regression
fit <- glm(gn~.,data=train)
summary(fit) # display results
predict(fit, type="response") # predicted values
residuals(fit, type="deviance") # residuals
anova(fit, test="Chisq")

# SVM
train = d_gn[,-c(1:2, 28:34)]
svm = svm(all ~ ., data = train, type='C-classification', cross = 10)
acc = summary(svm)$tot.accuracy
pred = predict(svm, train[,-len(train)])
table(train[,len(train)],pred)
action = train$all
sum(pred==action)/length(action)

# PCA with function princomp
install.packages("stats")
library(stats)
pca1 <- prcomp(~., data = train[,-len(train)], cor = T)
summary(pca1) # print variance accounted for 
pca1$loadings    # loadings
screeplot(pca1, type="lines")
biplot(pca1)
predict(pca1, newdata=tail(d, 2))
# loadings
l = head(pca1$rotation)
rownames(l)[apply(l, 2, function(x) which.max(abs(x)))]

# PCA with function PCA
install.packages("FactoMineR")
library(FactoMineR)
d=train
pca3 = PCA(d[,-len(d)], graph=FALSE)
pca3 = PCA(d[,-len(d)]) # graphs generated automatically
pca3$eig         # matrix with eigenvalues
pca3$var$coord   # correlations between variables and PCs

# per sub per kmeans-class
k = 5
for(p in p.list){
  d = d_test_yx[d_test_yx$sub==p, ]
  d[,c(3:18)] = apply(d[,c(3:18)], 2, normalize.vector)
  x = as.matrix(d[,c(3:18)])
  
  
  cl <- kmeans(x, k)
  summary(cl)
  #   plot(x, col = cl$cluster)
  #   points(cl$centers, col = 1:5, pch = 8, cex = 2)
  
  d_gn = cbind(d, cl$cluster)
  colnames(d_gn)[20] = "gn"
  #   xyplot(d_gn$gn ~ d_gn$frq.tap, data = d_gn, 
  #          layout = c(1, len(unique(d_gn$sub))),
  #          type="p", main=paste("class = kmeans"))
  
  train = d_gn[,-c(1:2)]
  model <- randomForest(gn ~ ., train, proximity=TRUE, keep.forest=FALSE)
  MDSplot(model, train$gn, palette=c(1:10))
  
  fit <- glm(gn~.,data=train)
  anova(fit, test="Chisq")
  
  # SVM
  svm = svm(gn ~ ., data = train, type='C-classification', cross = 10)
  acc = summary(svm)$tot.accuracy
  pred = predict(svm, train[,-len(train)])
  table(train[,len(train)],pred)
  action = train$gn
  sum(pred==action)/length(action)
  
  
  
}

# per sub per action-rating
for(p in p.list){
  d = d_test_yx[d_test_yx$sub==p, ]
  d[,c(3:18)] = apply(d[,c(3:18)], 2, normalize.vector)
  x = as.matrix(d[,c(3:18)])
  
  #randomforest====
  train = d[,-c(1:2)]
  model <- randomForest(train[,-len(train)])
  print(model)
  round(importance(model), 2)
  mds <- cmdscale(1 - model$proximity, eig=TRUE)
  op <- par(pty="s")
  pairs(cbind(train[,-len(train)], mds$points), cex=0.6, gap=0,
        col=c(1:10)[as.numeric(train$gn)],
        main="")
  par(op)
  print(mds$GOF)
  model <- randomForest(action ~ ., train, proximity=TRUE, keep.forest=FALSE)
  MDSplot(model, train$action, palette=c(1:10))
  MDSplot(model, train$action, palette=c(1:10), pch=train$gn)
  
  #Logistic regression
  fit <- glm(action~.,data=train)
  summary(fit) # display results
  predict(fit, type="response") # predicted values
  residuals(fit, type="deviance") # residuals
  anova(fit, test="Chisq")
  
  # SVM
  svm = svm(action ~ ., data = train, type='C-classification', cross = 10)
  acc = summary(svm)$tot.accuracy
  pred = predict(svm, train[,-len(train)])
  table(train[,len(train)],pred)
  action = train$action
  sum(pred==action)/length(action)
}

#find similarity between sub.feature and rep.feature (min/max/mean...)
dis_all = {}
sim_all = {}
for(g in d_rep$game){
  d = d_test[d_test$game==g,c(3:18)]
  dist(d, method = "Euclidean")
  d = d_test_yx[d_test_yx$game==g,c(3:18)]
  dist(d, method = "Euclidean")
  d = d_test_rey[d_test_rey$code==g,c(1:70)]
  dist(d, method = "Euclidean")
  
  if(nrow(test)<=0)
    next
  
  dis = dist(test, rep)
  dis = cbind(g, as.character(test_g$sub), dis)
  dis_all = rbind(dis_all, dis)
  
  sim = simil(test, rep)
  sim = cbind(g,as.character(test_g$sub), sim)
  sim_all = rbind(sim_all, sim)
}


#find the representive features (per game) from test data
fn = paste(fdir_rep, "/allgame_q70.txt", sep="")
sink(fn)
for(g in gcode$gc[-(24:25)]){ 
  d = d_test[d_test$game==g,3:18]

  #   # normalize feature for observing
  #   d[,-len(d)] = apply(d[,-len(d)], 2, normalize.vector)
  #   d = apply(d, 2, normalize, c(0,1))
  
  #     fn.d4 = paste(fdir_rep, "/", g, ".txt", sep="")
  #     sink(fn.d4)
  if(0){
  for(func in funcs){
    f = get(func)
    feature = apply(d, 2, f, na.rm = T)
    cat(g, func, feature, "\n")
  }
  }
  func = "quantile"
  feature=apply(d, 2, quantile, probs=0.7)
  cat(g, func, feature, "\n")
  #     sink()
  
}
sink()

#find the representive features (per sub rate) from test data
for(p in p.list){
  fn = paste(fdir_rep, "/", p, ".txt", sep="")
  #   sink(fn)

  d = d_test[d_test$sub==p,3:19]
  #find the representive features (per rating)
  for(r in unique(d$rate)){
    for(func in funcs){
      f = get(func)
      feature = apply(d[d$rate==r,-len(d)], 2, f, na.rm = T)
      cat(p, r, func, feature, "\n")
    }
  }
#   sink()
}


#plot test features
# per sub per rate with all game
d_test = d_yx_rating
for(p in p.list){
  pic.name = paste(picdir, "pic_rating/", p, ".png",sep="")
  png(file = pic.name, width = 800, height = 1200)
  par(mfrow=c(5,1))
  par(mar=c(1,1,1,1))
  for(r in 1:5){
    d = d_test[d_test$sub==p & d_test$action==r,]
    
    if(nrow(d)<=0)
      next
    
    # normalize feature for observing
    d[,c(3:27)] = apply(d[,c(3:27)], 2, normalize.vector)
#     boxplot(d[,3:18], main = paste(s, "rating=", r))
#     colorRampPalette(brewer.pal(9,"GnBu"))(nrow(d))[i]
    plot(0, type='n', xlim=c(0,len(d)), ylim=c(0,1), 
         main = paste(p, "rating=", r), xlab=NA, ylab=NA)
    for(i in 1:nrow(d)){
      lines(as.numeric(d[i,3:27]), col=col_p[3])
    }
    #plot the rep feature (mean, min, max...)
    feature=apply(d[d$action==r,3:27], 2, quantile, probs=0.7)
    lines(feature, col=col_o[3])

#     for(f in 1:len(funcs)){
#       func = get(funcs[f])
#       feature = apply(d[d$rate==r,3:18], 2, func, na.rm = T)
#       lines(as.numeric(feature), col=col_o[f])
#     }
  }
  dev.off()
}

# per sub per rate (rep feature: quantile, probs=0.7)
par(mfrow=c(6,1))
par(mar=c(1,1,1,1))
for(p in p.list){
  plot(0, type='n', xlim=c(0,len(d)), ylim=c(0,1), 
       main = p, xlab=NA, ylab=NA)
  legend("topleft", legend=c(1:5), col=col_o,lwd=2)
  
  d = d_test[d_test$sub==p,]
  # normalize feature for observing
  d[,c(3:27)] = apply(d[,c(3:27)], 2, normalize.vector)
  
  for(r in 1:5){

    #     boxplot(d[,3:18], main = paste(s, "rating=", r))
    #     colorRampPalette(brewer.pal(9,"GnBu"))(nrow(d))[i]
    
    #plot the rep feature (mean, min, max...)
    feature=apply(d[d$action==r,c(3:27)], 2, quantile, probs=0.8)
    lines(feature, col=col_o[r])
  }
  
}

#per game with all sub
for(g in gcode$gc[-(24:25)]){
  pic.name = paste(picdir, "pic_game_feature/", g, ".png",sep="")
  png(file = pic.name, width = 800, height = 800)
  par(mfrow=c(2,1))
  d = d_test[d_test$game==g,]
  if(!nrow(d))
    next
  
  # normalize feature for observing
  d[,c(3:27)] = apply(d[,c(3:27)], 2, normalize.vector)
  #   d = apply(d, 2, normalize, c(0,1))
  
  #1st pic
  boxplot(d[,c(3:27)], main = g)

  #2nd pic
  plot(0, type='n', xlim=c(0,len(d)), ylim=c(0,1), main = paste(g))
  for(i in 1:nrow(d)){
    lines(as.numeric(d[i,c(3:27)]), col=col_p[i])
  }
  legend("topleft", as.character(d$sub), col = col_p[1:nrow(d)], bty='n', lwd=2)
  #plot the rep feature (mean, min, max...)
  feature=apply(d[,c(3:27)], 2, quantile, probs=0.7)
  lines(feature, col=col_o[3])

#   for(f in 1:len(funcs)){
#     func = get(funcs[f])
#     feature = apply(d, 2, func, na.rm = T)
#     lines(as.numeric(feature), col=col_o[f])
#   }
  dev.off()
}




