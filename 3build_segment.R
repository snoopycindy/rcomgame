#說明:
#1.螢幕偵測到按鈕動作的開始，稱為一個event
#2.若此event為多按鈕事件，偵測slot會啟動，其sl參數記錄0,1,2...來表示第1,2,3..個按鈕的變化
#3.每一個按鈕從壓下去到最後拿起來為止稱為一個action 

setwd("e:/Dropbox/workspace/done_app_yx/R")
source('2load_game_table.R')
options(warn=1) 

#輸出參數
splot=0 #若要看plot寫0 else寫1

#定義常數
ntect=-9       #在原始資料上沒有讀取到的資料紀錄為none detect
data.seg=0.1  #原始資料對於持續壓住的事件，是約每0.02秒check一次探針
seg.len=10     #一個segment的長度(sec)
tap.lim=45     #一個tap的滑動最多在tap.lim距離內(1cm約等於89.29格)
na.rp=0        #feature中的參數，取代NA跟NaN為na.rp


f.class=c("^rate.","^ratio.action","^ratio.dir","^dir.sin.","^dir.cos.",
          "^ratio.butt", "^burst.", "^dur.action.", "^x.action.", "^y.action.",
          "^dur.tap.","^x.tap.","^y.tap.",
          "^dur.slide.","^dist.slide.","^disp.slide.","^speed.slide.","^multi.", "^wait.") #feature的類別

rec.all=list() #存取所有action的record
seg.all=list()
gt.all=list() #存原始data的record

#針對每個玩家玩的每款遊戲，存取table在gt上
for(game in gcode$gc){
  for(sub in allsub) {
    # list all subject files =================
    fn.full = list.files(sub, full=T)         #list all the path
    fn.part = list.files(sub)                 #list file names only
    
    # find the game log =================  
    n = grep(game, fn.part)   #回傳fn.part的位置，如果吻合
    if(length(n)==0) 
      next
    
    # read file =================
    gt = read.table(fn.full[n], header=T)
    names(gt)<-c('t','sl','ev','x','y','prs') #time, multi-event, event, pos.x, pos.y, press
    
    #clean the record which is in the prev game===============
    if(gt$ev[1]==ntect){  # if the first record is not a start of actions
      act.last=which(gt$ev!=ntect)[1]-1
      if(is.na(act.last))
        stop()
      gt <- gt[-(1:act.last),]
    }
    
    #若x,y,prs為ntect(因為資料在上個遊戲)，拿掉data
    gt <- gt[gt$x!=ntect & gt$y!=ntect,]
    gt$prs[gt$prs==ntect] <- NA

    # write {duration, displacement,  distance} in every action ========
    gt$t.e=NA
    gt$t.s=NA
    gt$act.dur=NA
    gt$disp=NA
    gt$dist=NA
    butt=gt$sl[!duplicated(gt$sl)] #butt存取gt中所有的按鍵種類
    
    #針對每一種按鍵做處理,修改較好讀的game log table (gt)
    for(i in butt){
      mt=gt[gt$sl==i,]
      
      #計算按鈕間的時間與距離差距
      bt=sapply(1:(nrow(mt)-1),function(x) mt$t[x+1]-mt$t[x]) #計算同一個按鍵record間的時間差距
      x.diff=sapply(1:(nrow(mt)-1),function(b) mt$x[b+1]-mt$x[b]) #計算同一個按鍵record間的x差距
      y.diff=sapply(1:(nrow(mt)-1),function(b) mt$y[b+1]-mt$y[b]) #計算同一個按鍵record間的y差距
      
      #找到每個按鈕的起始位置與結束位置
      if(i!=ntect) #若i是multiple 的情況
        e.pos=intersect(which(bt>data.seg),union(which(x.diff!=0),which(y.diff!=0))) #若時間差距>data.seg且x或y的位置有變動，當作不同的事件
      else{
        e.pos=which(mt$ev!=ntect)-1
        e.pos=e.pos[e.pos!=0]       #拿掉end pos是0的狀況(因為第一個ev的上一個結束位置不干這個seg的事)
      }
      e.pos=c(e.pos,nrow(mt)) #結束位置也算在e.pos
      s.pos=c(1,e.pos+1)                                      #換算開始位置，(減掉最後一個起始點因為是最後一個record+1, 超出範圍)
      s.pos=s.pos[-length(s.pos)] 
      
      #計算此按鈕的開始and結束時間，disp，dist
      t.e=mt$t[e.pos]
      t.s=mt$t[s.pos]
      act.dur=mt$t[e.pos]-mt$t[s.pos]
      disp=sapply(1:length(s.pos),function(x) fdisp(mt,s.pos[x],e.pos[x]))
      dist=sapply(1:length(s.pos),function(x) fdist(mt,s.pos[x],e.pos[x]))  
      
      #填回gt(在每個action的頭與尾皆填上需要的action attributions)
      
      gt.s.pos=which(row.names(gt) %in% row.names(mt[s.pos,]))
      gt.e.pos=which(row.names(gt) %in% row.names(mt[e.pos,]))
      #填入action的開始、結束時間
      gt$t.s[gt.e.pos] = gt$t.s[gt.s.pos] = t.s
      gt$t.e[gt.s.pos]= gt$t.e[gt.e.pos] = t.e 
      #填入action的duraion
      gt$act.dur[gt.s.pos]=act.dur
      #填入action的disp跟dist
      gt$disp[gt.s.pos]=disp
      gt$dist[gt.s.pos]=dist
      #填入action的calss(開始記錄跟結束記錄都要填)
      gt$class[!is.na(gt$dist) & (gt$dist>tap.lim)]='slide'
      gt$class[!is.na(gt$dist) & (gt$dist<=tap.lim)]='tap'
      gt$class[gt.e.pos]=gt$class[gt.s.pos]
    }
    #====join event features, times between events========
    gev=gt[gt$ev!=ntect,] #找到event發生時的action
    gt$wait=NA
    if(nrow(gev)>1) { #若有兩個以上ev發生，計算此features
      g1=gev$t.s[-1] #這個event的開始時間
      g2=gev$t.e[-nrow(gev)] #上個event的結束時間
      gt$wait[gt$ev!=ntect & rownames(gt)!=rownames(gev[1,])] <-g1-g2
    }
    
    
    #==== divide segment and calculate features==========
    seg.t=sapply(0:((tail(gt,n=1)$t-gt$t[1])/seg.len),function(x) gt$t[1]+x*seg.len) #所有segment的time區間
    if(length(seg.t)<=1) next #若segment不到1個，跳出
    seg.n=1:(length(seg.t)-1) #segments的個數(從1,2,3...)
    
    #看features的需要決定傳入gt,gt.act, or gt.e
    gt.act=gt[gt$t==gt$t.s & !is.na(gt$t.s),] #只有action一開始的records 
    gt.e=gt[gt$t==gt$t.e & !is.na(gt$t.e),] #只有action結束時的records
    
    if(nrow(gt.e)!=nrow(gt.act)) #debug, 若action開始個數跟action結束個數不一致 stop()
      stop()
    
    seg.f=ldply(lapply(seg.n,function(n) {
      
      #在seg.t[n]~seg.t[n+1]所牽扯到的actions
      t1=seg.t[n]
      t2=seg.t[n+1]
      gs=f.act(gt.act,t1,t2)
      ge=f.act(gt.e,t1,t2)
      
      #action counts features
      rate.action=nrow(gs)/seg.len
      burst.action.10=f.b.act(gs,t1,t2,seg.len,10)
      burst.action.5=f.b.act(gs,t1,t2,seg.len,5)
      ratio.action.active10=f.ratio.act.active(gs,t1,t2,seg.len,10)
      ratio.action.active100=f.ratio.act.active(gs,t1,t2,seg.len,100)
      
      #action duration features
      dur.action.mean = mean(gs$act.dur)
      dur.action.max = f.max(gs$act.dur)
      dur.action.p95 = quantile(gs$act.dur,probs=0.95)
      dur.action.p75 = quantile(gs$act.dur,probs=0.75)
      dur.action.med = quantile(gs$act.dur,probs=0.50)
      
      #the time between different events 有可能有NA
      wact=gt.act[!is.na(gt.act$wait),]
      wact.t=wact[wact$class=='tap',]
      wact.s=wact[wact$class=='slide',]
      if(nrow(wact)==0){
        wait.action.mean =   seg.len
        wait.action.sd = 0
      } else {
        wait.action.mean=mean(wact$wait)
        wait.action.sd=sd(wact$wait)
      }
      if(nrow(wact.t)==0){
        wait.tap.mean =   seg.len
        wait.tap.sd = 0
      } else {
        wait.tap.mean=mean(wact.t$wait)
        wait.tap.sd=sd(wact.t$wait)
      }
      if(nrow(wact.s)==0){
        wait.slide.mean =   seg.len
        wait.slide.sd = 0
      } else {
        wait.slide.mean=mean(wact.s$wait)
        wait.slide.sd=sd(wact.s$wait)
      }

  
      #action start/end position features
      x.action.start.sd = sd(gs$x)
      x.action.start.mean = mean(gs$x)
      y.action.start.sd = sd(gs$y)
      y.action.start.mean = mean(gs$y)
      
      x.action.end.sd = sd(ge$x)
      x.action.end.mean = mean(ge$x)
      y.action.end.sd = sd(ge$y)
      y.action.end.mean = mean(ge$y)
      
      #action distance in x or y features
      x.action.movement.sd = sd(abs(f.act.movement(gs,ge,'x')))
      x.action.movement.mean = mean(abs(f.act.movement(gs,ge,'x')))
      y.action.movement.sd = sd(abs(f.act.movement(gs,ge,'y')))
      y.action.movement.mean =  mean(abs(f.act.movement(gs,ge,'y')))
      
      #multiaction
      multi.ratio.action = nrow(gs[gs$sl!=ntect,])/seg.len/rate.action
      multi.dur.mean = mean(gs$act.dur[gs$sl!=ntect])
    
      #ta=gs's taps, sli=gs's slide
      ta=gs[gs$class=='tap',]
      sli=gs[gs$class=='slide',]
      sli.e=ge[ge$class=='slide',]
      
      #count taps/slide
      rate.taps = nrow(ta)/seg.len
      rate.slides = nrow(sli)/seg.len
      ratio.butt.taps = rate.taps/rate.action
      ratio.butt.slides = rate.slides/rate.action
      
      #about taps..
      dur.tap.mean = mean(ta$act.dur)
      dur.tap.max = f.max(ta$act.dur)
      dur.tap.p95 = quantile(ta$act.dur,prob=0.95)
      dur.tap.p75 = quantile(ta$act.dur,prob=0.75)
      dur.tap.med = quantile(ta$act.dur,prob=0.50)
      
      x.tap.sd = sd(ta$x)
      x.tap.mean = mean(ta$x)
      y.tap.sd = sd(ta$y)
      y.tap.mean = mean(ta$y)
      
      #about slides' duration, dist, disp, speed
      dur.slide.mean = mean(sli$act.dur)
      dur.slide.max = f.max(sli$act.dur)
      dur.slide.p95 = quantile(sli$act.dur,prob=0.95)
      dur.slide.p75 = quantile(sli$act.dur,prob=0.75)
      dur.slide.med = quantile(sli$act.dur,prob=0.50)
      
      dist.slide.mean = mean(sli$dist)
      dist.slide.max = f.max(sli$dist)
      dist.slide.p95 = quantile(sli$dist, prob=0.95)
      dist.slide.p75 = quantile(sli$dist, prob=0.75)
      dist.slide.med = quantile(sli$dist, prob=0.50)
      
      disp.slide.mean = mean(sli$disp)
      disp.slide.max = f.max(sli$disp)
      disp.slide.p95 = quantile(sli$disp, prob=0.95)
      disp.slide.p75 = quantile(sli$disp, prob=0.75)
      disp.slide.med = quantile(sli$disp, prob=0.50)
      
      sli$speed=sli$dist/sli$act.dur
      speed.slide.mean = mean(sli$speed)
      speed.slide.max = f.max(sli$speed)
      speed.slide.p95 = quantile(sli$speed,prob=0.95)
      speed.slide.p75 = quantile(sli$speed,prob=0.75)
      speed.slide.med = quantile(sli$speed,prob=0.50)
      
      #direction(sin, cos)
      dir.sin.slide.mean = mean(f.dir.trigon(sli,sli.e,'sin'))
      dir.cos.slide.mean = mean(f.dir.trigon(sli,sli.e,'cos'))
      dir.sin.slide.sd = sd(f.dir.trigon(sli,sli.e,'sin'))
      dir.cos.slide.sd = sd(f.dir.trigon(sli,sli.e,'cos'))
      dir.sin.slide.max = f.max(f.dir.trigon(sli,sli.e,'sin'))
      dir.cos.slide.max = f.max(f.dir.trigon(sli,sli.e,'cos'))
      
      
      #ratio other slide
      ratio.dir.ver.slide = f.dir.func.slide(sli,sli.e,'ver')/seg.len/rate.slides
      ratio.dir.hor.slide = f.dir.func.slide(sli,sli.e,'hor')/seg.len/rate.slides
      ratio.dir.other.slide= f.dir.func.slide(sli,sli.e,'other')/seg.len/rate.slides
      
      
      # create segments ==========
      #抓sub的名字
      name.s=regexpr("[^[:punct:]]+$",sub) 
      name=substr(sub,start=name.s,stop=nchar(sub))
      
      #union obj
      vars = NULL
      for (pat in f.class)
        vars = union(vars, grep(pat, ls(), value=T))
      
      #combine obj. to data.frame
      r=data.frame(nouse=1)
      for(i in 1:length(vars)){     
        ri=get(vars[i])
        if(is.na(ri))
          ri=na.rp
        #ri[is.na(ri)]=na.rp
        r =  cbind(r,ri)   
      }
      r=r[,-1]
      names(r) = vars
      r$user=name
      r$code=game      
      return (r)
      
      }))    
    
    seg.all=append(seg.all,list(seg.f))
    
    # save all action records ========
    gt$game=game
    name.s=regexpr("[^[:punct:]]+$",sub) 
    gt$user=substr(sub,start=name.s,stop=nchar(sub))
    rec.all=append(rec.all,list(gt[!is.na(gt$act.dur),]))
    gt.all=append(gt.all,list(gt))
  }      
}


rec.all=ldply(rec.all)
seg.all=ldply(seg.all)
gt.all=ldply(gt.all)
#yh2-----------------------------------
write.table(rec.all, file = "rec.all")
write.table(seg.all, file = "seg.all")
write.table(gt.all, file = "gt.all")
rec.all=read.table(file = "rec.all")
seg.all=read.table(file = "seg.all")
gt.all=read.table(file = "gt.all")
#yh2-end-------------------------------

#debug (dist)
#  t=rec.all[rec.all$dist>5000,]
#  t[order(t$act.dur),]

#plot every action distance (決定tap跟slide的界限)  
if(splot==1){
  par(mfrow=c(1,2))
  #CDF, all 
  tmp=seq(min(rec.all$dist),max(rec.all$dist), len=100)
  plot(tmp,ecdf(rec.all$dist)(tmp),type='l',ylab='Fn(x)',xlab=' distance',main="CDF in distance")
  axis(1,seq(from = 1000, to = 40000, by=2000))
  #hist, 0~700
  my.hist<- hist(rec.all$dist[rec.all$dist<700],breaks=(0:70)*10 ,plot=F)
  plot(my.hist$counts,log='y',type='h',xaxt='n',xlab='moving distance (interval=10)',ylab="actions' Counts (lg10)",main="hist in distance(0~700)")
  axis(1,1:100,labels=(1:100)*10)
  
  #hist, 0~200
  my.hist<- hist(rec.all$dist[rec.all$dist<200],breaks=0:200 ,plot=F)
  plot(my.hist$counts,log='y',type='h',xlab='moving distance',ylab="actions' Counts(lg10)",main="hist in distance(0~200)")

  #hist, 10~150
  my.hist<- hist(rec.all$dist[rec.all$dist<=150 & rec.all$dist>=10],breaks=10:150 ,plot=F)
  plot(my.hist$counts,log='y',type='h',xaxt='n',xlab='moving distance',ylab="actions' Counts(lg10)",main="hist in distance(10~150)")
  axis(1,1:151,labels=10:160)
    
}


#rm(bt,butt,data.seg,disp,duration,e.pos,fn.full,fn.part,gt.s.pos,i,n,ntect,dist,s.pos,x.diff,y.diff)
