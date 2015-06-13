#source("3build_segment.R")  #有segments才能做
#yh2-----------------------------------
write.table(rec.all, file = "rec.all")
write.table(seg.all, file = "seg.all")
write.table(gt.all, file = "gt.all")
rec.all=read.table(file = "rec.all")
seg.all=read.table(file = "seg.all")
gt.all=read.table(file = "gt.all")
#yh2-end-------------------------------
#utils:::menuInstallPkgs()
library(ape)
library(e1071)
library(vegan)
library(plyr)

#分類參數
method=2  #設定哪種分類 0為原始，1為節奏，2是htempo的遊戲再分類，3為ltempo的遊戲再分類，4為htempo+square分類，10改變遊戲跟分類
fchoose=0   #設定要哪類features 0為全分
segchoose=1 #設定要取哪些segments, 0全取， 1取每個人玩每個遊戲時25%以上的action

#SVM參數
svm=0       #1為計算SVM
digit=3     #準確率取到小數點第digit位
groupnum=5  #二次投票中，幾個segment為一組
show_allresult=0 #0為不show, 1為show NROW的投票結果

#recommend參數
mds=1      #畫MDS
r.k=3      #要推薦幾款遊戲


#===========對segments作取捨=======

#segments全取
if(segchoose==0)
  ss<-seg.all[,]

#segments取75%
if(segchoose==1){
  ss <-list()
  for(game in gcode$gc)
    for(sub in p.list){
      seg.temp=seg.all[seg.all$code==game & seg.all$user==sub,]
      if(nrow(seg.temp)==0)
        next
      else{
      act.lim=quantile(seg.temp$rate.action,prob=0.25)
      seg.temp=seg.temp[seg.temp$rate.action>=act.lim,]      
      ss=append(ss,list(seg.temp))
      }
    }
  ss=ldply(ss)
}

#==============設定分類============

ss <- ss[!grepl('sub05|sub06',ss$user),]  

ss$category <- NA
#拿掉不太能分類的遊戲
ss <- ss[!grepl('mug_5|td_4|cdef|square_5|square_6|manage|td|pz|fps_3|fps_6',ss$code),]
#ss <- ss[!grepl('manage|td',ss$code),]
#ss <- ss[ss$code %in% gpool,]
if(method==0)  #0為原始分類
  ss$category <- sub("_.+$",replacement="",ss$code)

#以節奏分類
if(method==1){ 
  ss[grepl('mug|bhell|run|fps',ss$code),'category'] <- 'htempo'
  ss[grepl('cdef|td|manage|pz|sqg',ss$code),'category'] <- 'ltempo'
}

#以遊戲方式分類
if(method==2){ 
   ss[grepl('mug',ss$code),'category'] <- 'Rhythm'
   ss[grepl('bhell',ss$code),'category'] <- 'Scrolling Shooter'
   ss[grepl('fps',ss$code),'category'] <- 'First-Person Shooter'
   ss[grepl('run',ss$code),'category'] <- 'Endless Running'
   ss[grepl('square',ss$code),'category'] <- 'Tile Matching'
}

#以taps跟slides分類
if(method==3){ 
   #ss[grepl('td',ss$code),'category'] <- 'td'
   #ss[grepl('pz',ss$code),'category'] <- 'pz'
   ss[grepl('sld',ss$code),'category'] <- 'sld'  
   ss[grepl('tap',ss$code),'category'] <- 'tap'  
}

if(method==10){
  ss=ss[!(ss$code %in% c("run_5","mug_1","cdef_3")),]
  ss$category <- sub("_.+$",replacement="",ss$code)
  ss$category[ss$code %in% c("square_5","square_6")] <-"sqg slide"
  ss$category[ss$code %in% c("pz_1","pz_2","pz_3")] <-"puz-tap"
  ss$category[ss$code %in% c("pz_6","pz_7")] <-"puz-connect"
  ss$category[ss$code %in% c("pz_4","pz_5")] <-"puz-slide"
}

if(method==11){
  ss$category=ss$code
}
ss=ss[!is.na(ss$category),]


#====設定train segments and test segments=========
if(method==10)  
  tr=ss[grepl('_2|square_5|pz_4|pz_6',ss$code),] #每一類型的二號遊戲為train game
if(method<10)
  tr=ss[grepl('02',ss$code),] #每一類型的二號遊戲為train game
if(method==11)
  tr=ss[ss$user=='sub02',]
#因為沒有data.frame的setdiff, 所以這樣寫  
seg.te=ss[!(ss$code %in% tr$code & ss$user %in% tr$user),] 
seg.re=seg.te[,c("user","code","category")]

#==========對feature分類 (need f.class in "build.segment.R")=========
f.c=data.frame(name=colnames(seg.all),cl=NA)
for (pat in f.class){
  ft=grep(pat,f.c$name)
  
  if(grepl("rate.|ratio.action|burst.",pat))
    f.c$cl[ft]="tempo"        #count, burst
  if(grepl("multi.",pat))
    f.c$cl[ft]="multi"
  if(grepl("ratio.dir|dir.sin.|dir.cos.",pat))
    f.c$cl[ft]="dir"
  
  if(pat=="^ratio.butt")
    f.c$cl[ft]="butt-kind"
  
  if(grepl("disp",pat))
    f.c$cl[ft]="disp"
  if(grepl("dist",pat))
    f.c$cl[ft]="dist"
  
  if(grepl("dur",pat))
    f.c$cl[ft]="dur"
  if(grepl("speed",pat))
    f.c$cl[ft]="speed"
  
  if(grepl("x",pat))
    f.c$cl[ft]="x"
  if(grepl("y",pat))
    f.c$cl[ft]="y"  
  
  if(grepl("wait",pat))
    f.c$cl[ft]="wait"
}

#===============設定想使用的features==================
omit.d=which(colnames(ss) %in% c('code','user')) #預設要刪去col: code and user

if(fchoose==0) #所有features都看
  omit.col=omit.d
if(fchoose==1){ #只看某些features
  omit.p=which(!grepl("tempo|dur",f.c$cl)) #其他要刪的col(即features): omit.p
  omit.col=na.omit(union(omit.d,omit.p))
}
tr=tr[,-omit.col]
exp=ss[,-omit.col]

#若只看部份的features
if(fchoose!=0)
  ss=ss[,-setdiff(omit.p,omit.d)] #ss拿掉不要看的feature (留下user跟code)



#==========recommend遊戲==============
ug=ss  #to identify by users and games
ug$cl=paste(ss$user,ss$code)
r.m=ldply(lapply(unique(ug$cl),function(n){ #把segments取mean合而為一segment
  t=subset(ug[ug$cl==n,],select=-c(user,code,cl,category))
  apply(t, 2, mean) 
}))
rownames(r.m) <- unique(ug$cl)
ca <-ug[!duplicated(ug$cl),]          #these points category
for(i in 1:ncol(r.m))  r.m[[i]]=normalize(r.m[[i]],c(0,1))

d <-vegdist(r.m)           #??? data are fragmented
#d <-dist(r.m)
d <-isomapdist(d,k=3) #isomap distance   

dm<-as.matrix(d)  #convert to dist matrix

#從trace推薦遊戲
tca=ca[!duplicated(ca$cl),] #每個trace的資料
trac=ldply(lapply(unique(ca$cl), function(t){ #所有traces跟其他遊戲相似度排名
  names(recomm(t,dm))
  }))
rownames(trac) <- unique(ca$cl)

len(unique(paste(ss$user,ss$code)))

#找到跟自己一樣的遊戲(r1 code一樣, r2 class一樣)，其相似度排名
trac.r=ldply(lapply(1:nrow(trac), function(x)  {  
  #找到code跟自己一樣的trace傳回其排名
  t=trac[x,]
  d=tca$code[x]
  r1=which(t==d)
  
  #找到class跟自己一樣的trace傳回其第一個排名
  f=sapply(t,function(x) tca$category[tca$code==x][1]) 
  c=tca$category[x]
  r2=which(f==c)[1]
  data.frame(r1,r2)
  }))
rownames(trac.r) <- tca$cl

#從遊戲推薦遊戲
gca=ca[!duplicated(ca$code),]
gn.r=ldply(lapply(unique(tca$code), function(x){
  t=which(tca$code==x)
  r1=mean(trac.r$r1[t])
  r2=mean(trac.r$r2[t])
  data.frame(r1,r2)
  }))

g.bco=gca$code[which(gn.r$r1>10)] #game.bad.code 自己遊戲排不進前20名的games
g.bcl=gca$code[which(gn.r$r2>10)] #game.bad.class 自己分類排不進前10名的games
g.gco=gca$code[which(gn.r$r1<5)]
g.gcl=gca$code[which(gn.r$r2<3)]


#判定遊戲投票，每個trace取前r.k名，得r.k~1分，最後看總合分數
ans=ldply(lapply(unique(ca$code), function(n){
  r.k=r.k+1 #因為把自己這款遊戲加進去，所以加一排名
  t=ca[ca$code==n,]
  v=ldply(lapply(t$cl,function(x) {  #每個trace取前k名
    s=recomm(x,dm)
    #data.frame(name=names(s[names(s)!=n])[1:r.k],score=r.k:1)
    data.frame(name=names(s)[1:r.k],score=r.k:1)
    }))

  set=sapply(unique(v$name), function(x)  sum(v$score[v$name==x]))
  names(set) <- unique(v$name)
  vt=sort(set,decreasing=T)  #照最多的排序
  names(vt[1:r.k])
  }))
rownames(ans) <- unique(ca$code)

ans.o=ldply(lapply(1:nrow(ans), function(x){
  trow=subset(ans[x,],select=which(ans[x,]!=rownames(ans[x,])))
  r=trow[,1:r.k]
  colnames(r) <- paste(1:r.k,"th")
  r
}))
rownames(ans.o) <- unique(ca$code)

ans$ca=ca[!duplicated(ca$code),]$category #幫投票結果補上遊戲的本身category
ans[ans[ans$e1,]$ca!=ans$ca,] #推薦遊戲跟本身遊戲非同類型的
ans[ans$e1.r<0.5,] #推薦遊戲的得票率低

ans.o$sameclass=sapply(rownames(ans.o), function(n) {
  t=ans.o[rownames(ans.o)==n,1:r.k]
  cli=sapply(t,function(x) gca$category[gca$code==x])
  cn=gca$category[gca$code==n]
  len(which(cli==cn))/r.k*100
  })

acc=mean(ans.o$sameclass)



#=======Support Vector Machine，training and classify games============
#=========model and predict==============
if(svm==1){
  model<-svm(as.factor(category)~.,data=tr,type='C-classification',cross=10)
  tr.res <- predict(model,na.omit(tr))
  
  #training的segments比較結果 t1
  t1<-as.data.frame.matrix(table(tr.res,na.omit(tr)$category))
  pre <-predict(model,na.omit(seg.te[,-omit.col]))
  
  #experient的segments比較結果 t2
  t2 <-as.data.frame.matrix(table(pre,na.omit(seg.te)$category))
  
  col_sum=apply(t2,2,sum)
  col_row=apply(t2,1,sum)
  last_row=rep(0,NROW(t2))
  last_col=rep(0,(NROW(t2)+1)) #最後跟last_row的交界放0
  for(i in 1:NROW(t2)){
    last_row[i] <- round(t2[i,i]/col_sum[i],digit)
    last_col[i] <- round(t2[i,i]/col_row[i],digit)
  }
  n = row.names(t2)
  t2 <- rbind(t2,last_row)
  t2$accuracy=last_col
  row.names(t2) <- union(n,'recognition rate')
  #=========vote==================
  seg.re[labels(pre),'pre']=pre
  v=merge(unique(seg.re$user),unique(seg.re$code)) #設定須投票的遊戲與人數
  colnames(v) <- c("user","code")
  
  #投票結果e1 e2 e3
  v.r=ldply(lapply(1:nrow(v), function(n) {
    t=seg.re[seg.re$code==v$code[n] & seg.re$user==v$user[n],]
    if(nrow(t)==0 |is.na(t$category[1]))
      return(data.frame(ca=NA,e1=NA,e2=NA,e3=NA))
    
    t$pre=as.character(t$pre)
    ca=t$category[1] #設定這個遊戲的category
    e1=names(which.max(table(t$pre)))
    e3=names(which.max(table(t$pre[t$pre!="zz"])))
    if(nrow(t)>groupnum){
      te=(sapply(0:(as.integer(NROW(t)/groupnum)-1), function(x) {
        t1=t[seq(from=1+x*groupnum,len=groupnum),]
        return (names(which.max(table(t1$pre))))
      }))
      e2=names(which.max(table(te)))
    }
    else
      e2=NA
    
    data.frame(ca,e1,e2,e3)
  }))
  
  v.r=na.omit(cbind(v,v.r))
  
  #=============投票準度分析===========
  #sub
  table(v.r$user[v.r$ca==v.r$e1])/table(v.r$user)
  
  #election
  elec=ldply(lapply(c('e1','e2','e3'), function(i) {
    v.t=sapply(1:len(unique(v.r$ca)), function(x) {
      n=unique(v.r$ca)[x]
      r=nrow(v.r[v.r[,i]==v.r$ca & v.r$ca==n,])/nrow(v.r[v.r$ca==n,])
    })
    names(v.t)=unique(v.r$ca)
    v.t
  }))
  rownames(elec)=c('e1','e2','e3')
  print(elec)
  
  seg.wr=seg.re[seg.re$category!=seg.re$pre,c('code','category','pre')]
}


#yx====================================================
# average the feature from Rey's analysis
sink("feature.txt")
for(g in gcode$gc){
  for(sub in p.list){
    isd = which(ss$code == g & ss$user == sub)
    if(!len(isd))
      next
    seg.part = sapply(ss[isd,1:72], mean)
    seg.part = c(seg.part, sub, g)
    cat(seg.part, "\n")
  }
}
sink()

#處理後的std檔
sdir <- "../gesture log/std"
pdir <- "../gesture log/parse70"
# add the rate from subject
d = read.table("feature.txt")
names(d) = names(ss)

#rm(vote_sample,tempcode,tempname,t.res,tempv,tr,seg.te,method,fchoose,col_row,col_sum,i,last_col,last_row)