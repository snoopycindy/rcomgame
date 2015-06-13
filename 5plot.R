source('../R.source/common.R')
source('../R.source/my.fig.R')
source('../R.source/my.legend.R')
library(xtable)
library(MASS)
library(taRifx)

#設定參數
fea=1     #draw feature plot
mds=1   #1 draw mds trace distribution
recom=1 #1 draw reomm result
special=1 #1 draw specical case
splot=1 #1 draw the limit between tap and slide
dtrace=1 #1 display gesture trace
strace=1
pic='./pic/'

options(digits=2)
out.tab=0 # output Latex table
#設定常數
sublist=as.character(sort(factor(unique(tca$user)))) #把player轉成paper上的代碼順序
sublist=c('1201sub01','1202sub01','1202sub02', '1203sub01', '1205sub01','1205sub02')
#exp obj. 只有feature跟category
#s1 = exp[sample(1:nrow(exp),1000),]
s1 = exp
r=subset(s1,select=-category)
cates = sort(unique(exp$category))

#gtab, 存取code,shorthand, full game title, #download
gtab=gcode[gcode$code %in% gca$code,]
colnames(gtab) <- c("code","Full game title")
gtab$Category=gca$category
gtab$shorthand=c("Temple","Pitfall","Subway","Gran","Fred","Trigger","CS","Strike","Gun","Cytus","Tap",
                 "Dance","Guitar","iFighter","Galaxy","Raiden","Laser","AirAttack","Jewels",
                 "Diamond","Blink","Marble")
gtab$installs.thousand=c(50000,1000,100000,10000,5000,1000,10000,5000,10000,10000,10000,1000,500,1000,500,5000,5000,5000,1000,5000,5000,1000)




#=======start drawing==============
if(out.tab==1){
  #===========show games summary=======
  #l.gtab=xtable(gtab[,c(3,4,2)])
  l.gtab=latex.table.by(gtab[,c(3,4,2,5)],num.by.vars = 1)
  print(l.gtab, include.rownames = FALSE, include.colnames = TRUE, sanitize.text.function = force)
  
  #show category summary
  ctab=data.frame(Category=unique(gca$category),Description=NA,Screenshot=NA)
  print(xtable(ctab))

  #=====show fake game record table==========
  fag=gt[11:18,c(1,3,4,5)]
  fag$t=c(36.1,36.2,36.3,36.4,37.1,37.2,37.4,37.4)
  fag$x=c(110,330,467,984,221,224,237,237)
  fag$y=c(721,661,332,227,429,456,434,434)
  fag$id=c("press","press","press","release","press","release","press","release")
  colnames(fag) <-c("record time","state","position of x-axis","position of y-axis")
  print(xtable(fag))

  #=====show feature description========
  sumf=data.frame(class=c("general","general","general","general","general","general","general","tap","tap","slide",
                            "slide","slide","slide","slide","slide"),shorthand=c("rate","burst","wait","position",
                          "movement","button","multi","t-duration","t-position","s-duration","displacement"
                          ,"distance","speed","Sin-Cos","direction"),
                  Meaning=NA,D=c(3,4,4,8,4,2,2,5,4,5,5,5,5,6,3),
                  Type=c("tempo","tempo","tempo","position","movement","movement","tempo","duration",
                         "position","duration","movement","movement","speed","direction","direction"))
                 
  print(latex.table.by(sumf,num.by.vars = 1), include.rownames = FALSE, include.colnames = TRUE, 
        sanitize.text.function = force)
#  print(xtable(sumf,digits = 0))
 
  #=====show summarizing subjects===========
  u=sublist
  g=unique(ss$code)
  rec.s=ldply(lapply(g,function(n) rec.all[rec.all$game==n,]))
  act.t=ldply(lapply(u,function(n){
      t=rec.s[rec.s$user==n,]
      tg=unlist(sapply(g,function(x) tail(t$t[t$game==x],n=1)-t$t[t$game==x][1]))
      data.frame(time=sum(tg),games=len(tg),time.per.game=mean(tg))
    }))
  act.t$actions=table(rec.s$user[rec.s$user!="sub05" & rec.s$user!="sub06"])
  act.t$action.per.second=act.t$actions/act.t$time
  act.t$sex=c("male","male","female","male","female")
  act.t$age=c("27","27","22","19","28")
  act.t=act.t[,c(7,6,2,1,3,4,5)]
  rownames(act.t)=paste("sub",1:5,sep="")
  print(xtable(act.t), include.rownames = T, include.colnames = TRUE, 
        sanitize.text.function = force)

#====show recommendation list========
  recomm=as.data.frame(sapply(1:r.k, function(n) {
    t=ans.o[,n]
    sapply(t,function(x) gtab$shorthand[gtab$code==x])
    }))
  recomm$Targetgame <- sapply(rownames(ans.o),function(x) gtab$shorthand[gtab$code==x])
  rownames(recomm) <- 1:nrow(recomm)
  recomm$Genre <- sapply(rownames(ans.o),function(x) gtab$Category[gtab$code==x])
  recomm$Samegenre <- ans.o$sameclass
  recomm <-recomm[,c(5,4,1,2,3,6)]
  mean(recomm$Samegenre)
  recomm=ldply(lapply(unique(recomm$Genre), function(x){
    t=recomm[recomm$Genre==x,]
    sg=mean(t$Samegenre)
    ans=c(x,"Overall","-","-","-",sg)
    rbind(t,ans)
    }))
  recomm$Samegenre=round(as.double(recomm$Samegenre),digits=0)
  recomm$Samegenre=paste(recomm$Samegenre,'%',sep="")
  colnames(recomm) <- c("Genre","Target game",paste(1:3,c("st","nd","rd"),sep=""),"Same Genre")
  print(xtable(recomm[,-1]),include.rownames= F)
  #print(latex.table.by(recomm,num.by.vars = 1), include.rownames = FALSE, include.colnames = TRUE,sanitize.text.function = force)

}

#action的移動距離 summary
if(splot==1){
  my.fig(paste(pic,"limit between tap and slide.eps",sep=""))
  par(mfrow=c(1,2))
  act=rec.all[rec.all$game %in% unique(ss$code),]
  tmp=seq(min(act$dist),max(act$dist), len=6000)
  plot(tmp,ecdf(act$dist)(tmp),type='l',ylab='Fn(x)',xlab=' distance',main="CDF in distance",xlim=c(0,4000))
  
  my.hist<- hist(act$dist[act$dist<700],breaks=(0:70)*10 ,plot=F)
  plot(my.hist$counts,log='y',type='h',xaxt='n',xlab='moving distance (interval=10)',ylab="actions' Counts (lg10)",main="hist in distance(0~700)")
  axis(1,1:100,labels=(1:100)*10)
  my.fig.off()
}


#draw box plot for every features
if(fea==1){
  my.fig(paste(pic,"1features.png",sep=""),main= T,width=10,height=4)
  #par(mfrow=c(3,7))
  layout(matrix(c(1:19,20,20), 3, 7, byrow = TRUE))
  f.select=c(1:3,6:8,5,4,17,19,36,42,44,46,50,55,61,67:68)    
  for(i in f.select){
    #for(i in dn){
    p1=data.frame(r[,i],exp$category)
    colnames(p1) <- union(colnames(exp)[i],'category')
    p1.m=sapply(1:length(cates),function(x) mean(p1[p1$category==cates[x],1]))
    p1.sd=sapply(1:length(cates),function(x) sd(p1[p1$category==cates[x],1]))
    
    if(i==5 || i==4 || i== 7) #若是title太長的,給予小點的cex
      p1.b=barplot(p1.m,names.arg=cates,col=blue.colors(len(unique(exp$category))),ylim=c(0,max(p1.m+1.96*p1.sd/10)),
                   ylab=NULL,,xaxt='n',xlab=NULL,main=colnames(exp)[i],cex.main=.999) #,cex.main = 2.5,cex.axis=2.5
    else     
      p1.b=barplot(p1.m,names.arg=cates,col=blue.colors(len(unique(exp$category))),ylim=c(0,max(p1.m+1.96*p1.sd/10)),
                       ylab=NULL,,xaxt='n',xlab=NULL,main=colnames(exp)[i]) #,cex.main = 2.5,cex.axis=2.5
    
    error.bar(p1.b,p1.m, 1.96*p1.sd/10)
  }
  #  }
  plot.new()
  legend("center",unique(exp$category),fill=blue.colors(len(unique(exp$category))),cex=1.1,bty='n')
  my.fig.off()
}


if(mds==1){
    
  fit <- isoMDS(d, k=2)
  #fit <- cmdscale(d,eig=TRUE, k=2)
  # plot solution 
  x <- fit$points[,1]
  y <- fit$points[,2]
  
  #畫trace分佈圖
  my.fig(paste(pic,"2trace_distribution.eps",sep=""))
  par(mfrow=c(1,1))
  plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",xlim=c(min(x),max(x)+1),ylim=c(min(y),max(y)+0.5),
       type="n") #main="isoMDS: traces distribution"
  for(i in 1:len(unique(gca$category))){
    tcate=unique(gca$category)[i]
  #  tcode=unique(gca$code[gca$category==tcate])
    if(i==1 || i==2)
      points(x[tca$category %in% tcate], y[tca$category %in% tcate], col=mycol[i],pch=mypch[i],cex=2,lwd=2)
    else
      points(x[tca$category %in% tcate], y[tca$category %in% tcate], col=mycol[i],pch=mypch[i],cex=2,lwd=3)
#     for(j in 1:len(tcode)){
#       points(x[tca$code %in% tcode[j]],y[tca$code %in% tcode[j]], col=mycol[i],pch=mypch[j],cex=2,lwd=3)
#     }
   }
#手動標註  
#   text(x=x[tca$cl=='sub01 run_5'],y=y[tca$cl=="sub01 run_5"],labels="Fred",pos=3,cex=1.6,offset=0.8)
#   text(x=x[tca$cl=='sub02 run_5'],y=y[tca$cl=="sub02 run_5"],labels="Fred",pos=4,cex=1.6,offset=0.5)
#   text(x=x[tca$cl=='sub01 mug_1'],y=y[tca$cl=='sub01 mug_1'],labels="Cytus",pos=4,cex=1.6)
#   text(x=x[tca$cl=='sub03 mug_1'],y=y[tca$cl=='sub03 mug_1'],labels="Cytus",pos=4,cex=1.6,offset=0.5)
#   text(x=x[tca$cl=='yx01 mug_1'],y=y[tca$cl=='yx01 mug_1'],labels="Cytus",pos=1,cex=1.6,offset=1)
  #legend(x=max(x)+0.1,y=min(y)+0.85,paste('Game',1:5),pch=mypch[1:len(unique(gca$category))],cex=1.5)
#自動標註
  text(x=x[tca$code=='run_5'],y=y[tca$code=="run_5"],labels="Fred",pos=3,cex=1.6,offset=0.8)
  text(x=x[tca$code=='mug_1'],y=y[tca$code=="mug_1"],labels="Cytus",pos=3,cex=1.6,offset=0.5)
  legend(cex=1.5,x=max(x)-1,y=max(y)+.5, unique(gca$category), col=mycol[1:len(unique(gca$category))], pch=mypch[1:len(unique(gca$category))])
  #legend(x=max(x)-1,y=max(y)+.5, unique(gca$category), fill=mycol[1:len(unique(gca$category))],cex=1.5)
  my.fig.off()
}

if(recom==1){
  #取名，genres上的名稱調整，換行
  ca.n=c("Endless\nRunning","First-Person\nShooter","Rhythm","Scrolling\nShooter","Tile\nMatching")
  
  #====從trace推薦game=========
  #畫排名資料(跟自己遊戲的推薦排名, unit: trace)  
  #my.fig("./pic/3trace_recomm_selfgame.eps")
  postscript(file=paste(pic,"3trace_recomm_selfgame.eps",sep=""),
             width = 8,
             height = 7,
             horizontal = F, onefile = FALSE, paper = "special", family="Helvetica")
  
  par(mar = c(5, 4.2, 5, 2))
  myplot=data.frame(r1=trac.r$r1,code=tca$code,user=factor(tca$user))
  bo=which(!duplicated(gca$category))
  
  myplot$uni=unlist(lapply(1:len(unique(myplot$code)), function(n) {
    k=len(which(bo<=n))+1
    t=myplot[myplot$code==unique(myplot$code)[n],]
    n+runif(nrow(t),min=.2,max=.8)+k
    }))
  plot(myplot$uni~myplot$r1,type='n',xaxt='n',yaxt='n',ylim=c(1,max(trac.r$r1)+3),
     xlim=range(myplot$uni),col=mycol[5],ylab='',xlab='') #main='recommend self-game (unit: trace)')
  
  a=unique(as.integer(myplot$uni))   #畫每款遊戲的分界
  b=table(gca$category)
  c=unlist(lapply(1:len(b), function(n) { #每個分類的第一款遊戲不要畫 
    if(n==1){
      a[1:b[n]]+.5
     # axis(3,k,label=gtab$shorthand[1:b[n]],cex=.8)
    }
    else{
      a[(sum(b[1:(n-1)])+1):sum(b[1:n])]+.5
    #  axis(3,k,label=gtab$shorthand[(sum(b[1:(n-1)])+1):sum(b[1:n])],cex=.8)
      }
  }))
  axis(3,c,label=gtab$shorthand)
  mtext(side = 3, "Game shorthand",line=3,cex=1.5)
  
  
#   c=unlist(lapply(1:len(b), function(n) { #對分類畫線(第一款遊戲不要畫 )
#     if(n==1)
#       a[2:b[n]]
#     else
#       a[(sum(b[1:(n-1)])+2):sum(b[1:n])]
#     }))
#   abline(v=c,col="#CCCCCC")
  
  c=which(!duplicated(gca$category))
  chk=0
  for(i in 1:len(unique(myplot$code))){
    t=myplot[myplot$code==unique(myplot$code)[i],]
    if(len(c[c==i])!=0) #若到換顏色的地方
      chk=chk+1
    for(j in 1:nrow(t))
      #points(t$uni[j],t$r1[j],pch=mypch[as.integer(t$user[j])], col=mycol[chk])
      points(t$uni[j],t$r1[j],pch=mypch[as.integer(t$user[j])], col=mycol[as.integer(t$user[j])], cex=1.5,lwd=2.5)
  }
  r1=which(!duplicated(tca$category))[-1] #find boundary for different categories
  abline(v=sapply(r1,function(n) mean(c(myplot$uni[n],myplot$uni[n-1]))),col="#404040") #draw lines for different category
  
  labx=sapply(1:(len(r1)+1),function(n) { #計算每個category的mean
    if(n==1)
      mean(myplot$uni[1:r1[n]])
    else if(n==len(r1)+1)
      mean(myplot$uni[r1[n-1]:nrow(myplot)])
    else
      mean(myplot$uni[r1[n-1]:r1[n]])
    })
  #cn=unique(tca$category)

  par(mgp = c(4, 2, 0))
  title(xlab="Trace",cex.lab=1.5)
  axis(1,labx,cex.axis=1.3,labels=ca.n) #畫X軸
  par(mgp = c(3, 1, 0))
  title(ylab="Similar Rank",cex.lab=1.5)  
  axis(2,c(1,5,10,15,20,nrow(gca)),cex.axis=1.2)

  #abline(h=nrow(gca)+1,lty=2) #加最後一名的虛線
  legend(x=min(myplot$uni),y=max(trac.r$r1)+3,
         paste("sub",1:len(sublist),sep=""),pch=mypch[1:len(sublist)],ncol=3,bg="white",col=mycol[1:5],cex=2.0)
#  legend(x=max(myplot$uni)-15,y=max(trac.r$r1)+12,unique(gca$category),
#         fill=mycol[1:5],ncol=2,bg="white")
dev.off()
  #  my.fig.off()
  
  
  #====performace of self game recommendation (table)======
  sg=gtab[,c("Category","shorthand")]
  sg$ar=sapply(unique(myplot$code), function(x) mean(myplot$r1[myplot$code==x]))
  sg$rp=(nrow(gtab)-sg$ar)/(nrow(gtab)-1)*100
  c(mean(sg$ar), mean(sg$rp))
  sg=ldply(lapply(unique(sg$Category), function(x) {
    t=sg[sg$Category==x,]
    ar=mean(t$ar)
    rc=mean(t$rp)
    ans=c(Category=x,shorthand="Overall",ar,rc)
    rbind(t,ans) 
    }))
  sg$ar=round(as.double(sg$ar),digits=2)
  sg$rp=round(as.integer(sg$rp),digits=0)
  sg$rp=paste(sg$rp,"%",sep="")
  print(latex.table.by(sg,num.by.vars = 1), include.rownames = FALSE, 
        include.colnames = TRUE, sanitize.text.function = force)
  
  
  #畫排名資料(跟自己遊戲同個class的個數, unit: trace/game)
  emp=rep(NA,len=r.k) #default empty row (for making boundry in every category in plot)
  bo=which(!duplicated(gca$category))
  
  yax=rapply(lapply(1:len(bo),function(x){ #add empty row in y-axis
    if(x==len(bo))
      gca$code[bo[x]:len(gca$code)]
    else
      c(gca$code[bo[x]:(bo[x+1]-1)],NA)
  }),how="unlist",function(n) n)
  
  
  for(by in c("bytrace","bygame")){
    #X軸以trace為單位做推薦
    if(by=='bytrace'){
      mna=3  #how many NAs want to add in the data (for draw border)
      myplot=data.frame(trac[,1:r.k])
      #a=which(!duplicated(tca$category))
      #myplot=myplot[c(54:77,1:18,38:53,19:37,78:nrow(tca)),] #reorder with bhell,run,FPS,mug,tile-matching
#      myplot$V4=tca$code #cant change col names
      bo=which(!duplicated(tca$category))
      myplot=add.emptyrow(myplot,bo,mna)
#      c=which(!duplicated(myplot$V4) & !is.na(myplot$V4)) #which change code name
#      my.fig("./pic/4trace_recomm_sameclass.eps")
      postscript(file=paste(pic,"4trace_recomm_sameclass.eps",sep=""),
                 width = 8,
                 height = 7,
                 horizontal = F, onefile = FALSE, paper = "special", family="Helvetica")      
      layout(matrix(c(2,1,1,1,1), 5, 1, byrow = TRUE))
      plot(1:nrow(myplot),ylim=c(1,len(yax)),xlim=c(1,nrow(myplot)),type='n',xaxt='n',
           yaxt='n',xlab="",ylab="") # main=paste("recommend games in different traces(Rank 1~3th)")
#     abline(v=c,col="#E6E6E6")
      x.bo=which(is.na(myplot[,1])) #boder in x-axis
      x.bo=sapply(seq(from=1,to=len(x.bo),by=mna), function(n) median(x.bo[n:(n+mna-1)]))
    }
    #X軸以game為單位做推薦
    if(by=='bygame'){
      myplot=data.frame(ans.o)
      bo=which(!duplicated(gca$category))
      myplot=ldply(lapply(1:len(bo),function(x){ #add empty row in myplot
        if(x==len(bo))
          myplot[bo[x]:nrow(myplot),]
        else
          rbind(myplot[bo[x]:(bo[x+1]-1),],emp)
      }))
#      my.fig("./pic/5game_recomm_sameclass.eps")
      postscript(file=paste(pic,"5game_recomm_sameclass.eps",sep=""),
                 width = 8,
                 height = 7,
                 horizontal = F, onefile = F, paper = "special", family="Helvetica")    
      
      #layout(matrix(c(2,1,1,1,1), 5, 1, byrow = TRUE))
      plot(1:nrow(myplot),ylim=c(1,len(yax)),xlim=c(1,nrow(myplot)),type='n',xaxt='n',xlab="",
           ylab="",yaxt='n') #main=paste("recommend games in different games(Rank 1~3th)"),xlab="trace index",ylab="games"
      x.bo=which(is.na(myplot[,1])) #boder in x-axis
    }
    
    cate.fa=factor(unique(gca$category))     #調整X軸黑線的位置
    xlab=sapply(1:(len(x.bo)+1),function(n) {
      if(n==1)
        mean(c(1,x.bo[1]))
      else if(n==len(x.bo)+1)
        mean(c(x.bo[n-1],nrow(myplot)))
      else
        mean(c(x.bo[n-1],x.bo[n]))
      })
    
    y.bo=which(is.na(yax)) #border in y-axis
    ylab=sapply(1:(len(y.bo)+1),function(n) { #調整Y軸黑線的位置
      if(n==1)
        mean(c(1,y.bo[1]))
      else if(n==len(y.bo)+1)
        mean(c(y.bo[n-1],len(yax)))
      else
        mean(c(y.bo[n-1],y.bo[n]))
    })    
    
    abline(v=x.bo,h=y.bo,col='gray80')
    
    for(i in 1:nrow(myplot)){
      if(is.na(myplot[i,1])) #若第一名無資料，代表是border的位置，跳過
        next
      for(j in 1:r.k){
        t=gca[which(gca$code==myplot[i,j]),]
        if(by=='bytrace')
          points(i,which(yax==t$code),pch=mypch[j],col=mycol[which(cate.fa==t$category)],cex=1.8,lwd=1.8)
        else
          points(i,which(yax==t$code),pch=mypch[j],col=mycol[which(cate.fa==t$category)],cex=2,lwd=2.5)
      }
    }
    
    

    if(by=="bytrace"){
      par(mar = c(9, 12, 12.2, 3),xpd=T)
      par(mgp = c(8, 5.5, 3.9))      
      title(xlab="Trace Index",cex.lab=1.5)
      axis(1,xlab,cex.axis=1.3,labels=ca.n) #畫X軸
      par(mgp = c(11, 7.9, 7.9))
      text(-2.5, 30, "Recommended\nGames", pos = 1, xpd = T,cex=1.5)
      #title(ylab="Recommendation Game",cex.lab=1.5)  
      axis(2,ylab,labels=ca.n,cex.axis=1.3)
      legend('topright',inset=c(0,-0.15),'groups',legend=c("1st","2nd","3rd"),pch=mypch[1:r.k],ncol=3,bty='n',cex=2)
    }  
    else{
      par(mar = c(9, 12, 12.2, 3),xpd=T)
      par(mgp = c(8, 5.5, 3.9))
      title(xlab="Game Index",cex.lab=1.5)
      axis(1,xlab,cex.axis=1.3,labels=ca.n) #畫X軸
      par(mgp = c(11, 7.9, 7.9))
      text(0.3, 30, "Recommended\nGames", pos = 1, xpd = T,cex=1.5)
      legend('topright',inset=c(0,-0.2),'groups',legend=c("1st","2nd","3rd"),pch=mypch[1:r.k],ncol=3,bty='n',cex=2)
      axis(2,ylab,labels=ca.n,cex.axis=1.04)
    }
    
  #  par(mgp = c(0, 4, 0))
 #   axis(1,xlab,labels=ca.n,cex.lab=2) #看分類的資料
    
#    mtext(side = 2, "Recoomendation games",line=5,cex=1.5)
    
    
    #legend('bottomright','groups',cate.fa, fill=mycol[1:len(cate.fa)],ncol=3,bty='n',cex=1.2)
    #legend('topright',inset=c(0.2,-0.15),'groups',cate.fa, fill=mycol[1:len(cate.fa)],ncol=3,bty='n',cex=1.2)
    

    dev.off()
#    my.fig.off()
  }
}



#畫特殊遊戲(分類相同，操作不同)  
if(special==1){
  for(i in c('Endless Running','Rhythm','Tile Matching')){
    if(i=="Endless Running")
      name="fred"
    else if(i=='Rhythm')
      name="cytus"
    else
      name="sqg"
    my.fig(paste(pic,"6particular-",name,".eps",sep=""))
    #plot: original games
    #    png(paste("./mds/",i,'.png',sep=''), width = 2500, height = 1200)
    plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",xlim=range(x),ylim=range(y),
         type="n")
    
    #set pch and color // other points, target genre, target game
    p=c(20,5,10)
    c=c(mycol[6],mycol[3],mycol[9])
    
    points(x[tca$category!=i],y[tca$category!=i],col=c[1],pch=p[1],cex=1.2)   #其它點
    xt=x[tca$category==i]
    yt=y[tca$category==i]
    ip=which(!grepl('mug_1|run_5',tca$code[tca$category==i]))
    points(xt[ip],yt[ip],col=c[2],pch=p[2],cex=2.5,lwd=4)   #目標點
    
    ep=grep('mug_1|run_5',tca$code[tca$category==i])
    #ep=which(abs(scale(xt))>1|abs(scale(yt))>1)
    #text(xt[ep],yt[ep],labels= tca$code[tca$cl %in% names(xt)[ep]])
    if(len(ep)>0)
      points(xt[ep],yt[ep],col=c[3],pch=p[3],cex=3.5,lwd=3)
    if(i=='Endless Running')
      legend(x=max(x)-1.7,y=max(y),c("Fred",i,"Other games")
             ,col=c(c[3],c[2],c[1]),pch=c(p[3],p[2],p[1]),cex=1.3)
    else
      legend(x=max(x)-1.5,y=max(y),c("Cytus",i,"Other games")
             ,col=c(c[3],c[2],c[1]),pch=c(p[3],p[2],p[1]),cex=1.5)
    my.fig.off()
  }
}


#畫game的起始action圖
if(dtrace==1){
  for(i in unique(ss$code)){
    postscript(file=paste(pic,"s_",i,".eps",sep=""),
               width = 6,
               height = 8,
               horizontal = F, onefile = FALSE, paper = "special", family="Helvetica")
    
    #my.fig(paste("./pic/",i,".eps",sep=""),width=4.8,height=6.4)
    par(mar = c(0.1, 1, 0.1, 0.1))
    data=rec.all[rec.all$game==i,]
    y=0-data$y+1900
    x=0-data$x+1200
    plot(x~y,pch=19,cex=0.7,col=mycol[7],xaxt='n',xlab="",ylab="",yaxt='n')
    #axis(1,at=seq(from=0,to=1900,by=300),las=2) #ylim=c(-1900,0) ,yaxt='n' ,xlim=c(0,1200)
    #axis(2,at=seq(from=0,to=1200,by=200),las=3)
    #my.fig.off()
    dev.off()
  }
}

#畫game的軌跡圖
if(strace==1){
  g=gt.all
  g$y=0-gt.all$y+1900  #先翻正
  g$x=0-gt.all$x+1200
  
  for(gm in gca$code){
    postscript(file=paste(pic,"t_",gm,".eps",sep=""),
               width = 6,
               height = 8,
               horizontal = F, onefile = FALSE, paper = "special", family="Helvetica")
    par(mar = c(0.1, 1, 0.1, 0.1))
    plot(x~y,data=g[1:200,],pch=19,cex=0.7,col=mycol[7],xaxt='n',xlab="",ylab=""
         ,yaxt='n',xlim=range(g$y),ylim=range(g$x), type='n') 
    myplot=g[g$sl==ntect & g$game==gm,]
    as=which(myplot$ev!=ntect)
    
    t=sapply(1:(len(as)-1), function(j){
      k=myplot$class[as[j]]
      if(is.na(k))
        return(0)
      else if(k=='tap')
        points(myplot$x[as[j]]~myplot$y[as[j]],cex=0.3,pch=19,col=mycol[7])
    })
    s=sapply(1:(len(as)-1), function(j){
      k=myplot$class[as[j]]
      if(is.na(k))
        return(0)
      else if(k=='slide'){
        t=myplot[as[j]:(as[j+1]-1),]
        points(t$x~t$y,type='l',cex=0.01,col=mycol[4])    
      }
    })
    
    
    print(gm)
    dev.off()
  }
}
