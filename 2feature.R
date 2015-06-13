setwd("E:/Dropbox/touchparse_stroke")
source("C:/R.source/my.fig.R")
source("C:/R.source/common.R")
source("C:/R.source/legend_col.R")
source("C:/R.source/rey.f.R")
source("C:/R.source/yx.common.R")
# utils:::menuInstallPkgs()
# install.packages("")
require(Hmisc)
require(plotrix)
library(Hmisc)
library(plotrix)

name.d = c("time", "type", "event", "value")
name.d2 = c("t", "sl", "id", "lv", "x", "y", "prs")

#game table
datadir <-("d2")
#game name and code data 
gn <- ("../gesture log/gl/gn.txt")

#設定參數
ntect = -9 #設定none detect為-9
time_limit = 60 #game log檔至少要60S以上
gcode = read.table("gn_new.txt", header=T, sep="\t", as.is=T, encoding="UTF-8") #讀取game列表
p.list=c('1201sub01','1202sub01','1202sub02', '1203sub01', '1205sub01','1205sub02') #設定player列表

#table:game name vs game code 
gcode = read.table("gn_new.txt", header=T, sep="\t", as.is=T, encoding="UTF-8") #讀取game列表
allsub = list.files(path=datadir, full=T)
name.all=list.files(path=datadir)

slen <-list(xmax=0,xmin=9999,ymax=0,ymin=9999) #calculate the max and min of (x,y) in the touch pad
for(game in gcode$gc){
  for(subno in 1:NROW(allsub)) {
    i_sub=allsub[subno]
    # 1. list all subject files =================
    fn.full = list.files(i_sub, full=T)         #list all the path
    fn.part = list.files(i_sub)                 #list file names only
    # 2. find the game log =================  
    n = grep(game, fn.part)   #return the location in fn.part
    if(length(n)==0) 
      next
    
    # 3. read file =================
    gt = read.table(fn.full[n], header=T)
    if(max(gt$x)>slen$xmax)
      slen$xmax <- max(gt$x)
    if(max(gt$y)>slen$ymax)
      slen$ymax <- max(gt$y)
    if(min(gt$x)<slen$xmin)
      slen$xmin <- min(gt$x)
    if(min(gt$y)<slen$ymin)
      slen$ymin <- min(gt$y)
  }
}  
rm(datadir,fn.full,fn.part,game,subno,i_sub,n)


#說明:
#1.螢幕偵測到按鈕動作的開始，稱為一個event
#2.若此event為多按鈕事件，偵測slot會啟動，其sl參數記錄0,1,2...來表示第1,2,3..個按鈕的變化
#3.每一個按鈕從壓下去到最後拿起來為止稱為一個action 


setwd("E:/Dropbox/touchparse_stroke/done_app_proj_gesture/R")
source('2load_game_table.R')
source('rey.f.R')
options(warn=1) 

#輸出參數
splot=0 #若要看plot寫0 else寫1

#定義常數
ntect=-9       #在原始資料上沒有讀取到的資料紀錄為none detect
data.seg=0.1  #原始資料對於持續壓住的事件，是約每0.02秒check一次探針
seg.len=10     #一個segment的長度(sec)
tap.lim=45     #一個tap的滑動最多在tap.lim距離內(1cm約等於89.29格)
na.rp=0        #feature中的參數，取代NA跟NaN為na.rp


f.class=c("^rate.","^ratio.action","^ratio.dir","^dir.sin.","^dir.cos.","^ratio.butt", "^burst.", "^dur.action.", "^x.action.", "^y.action.", "^dur.tap.","^x.tap.","^y.tap."
          ,"^dur.slide.","^dist.slide.","^disp.slide.","^speed.slide.","^multi.", "^wait.") #feature的類別

rec.all=list() #存取所有action的record
seg.all=list()
gt.all=list() #存原始data的record

#針對每個玩家玩的每款遊戲，存取table在gt上
for(game in gcode$code){
  for(sub in p.list) {
    # list all subject files =================
    fn.full = list.files(sub, full=T)         #list all the path
    fn.part = list.files(sub)                 #list file names only
    
    # find the game log =================  
    n = grep(game, fn.part)   #回傳fn.part的位置，如果吻合
    if(length(n)==0) 
      next
    
    # read file =================
    gt = read.table(fn.full[n], header=T)
    names(gt)<-c("t", "sl", "id", "lv", "x", "y", "prs") #time, multi-event, event, pos.x, pos.y, press
    
    #clean the record which is in the prev game===============
    if(gt$id[1]==ntect){  # if the first record is not a start of actions
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
    butt = unique(gt$id)
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
        e.pos=e.pos[e.pos!=0] #拿掉end pos是0的狀況(因為第一個ev的上一個結束位置不干這個seg的事)
      }
      e.pos=c(e.pos,nrow(mt)) #結束位置也算在e.pos
      s.pos=c(1,e.pos+1)      #換算開始位置，(減掉最後一個起始點因為是最後一個record+1, 超出範圍)
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
seg.all=read.table(file = "seg.all", header=T)
gt.all=read.table(file = "gt.all")
#yh2-end-------------------------------

gcode.list = unique(seg.all$code)
sub = unique(seg.all$user)
sink("game_feature.txt")
for(g in gcode.list){
  for(s in sub) {
    
    isd = which(seg.all$code == g & seg.all$user == s)
#     if(len(isd))
#       next
#     cat(isd, "\n")
    rate.act = mean(seg.all$rate.action[isd], na.rm = T)
    
    rate.tap = mean(seg.all$rate.taps[isd], na.rm = T)
    dur.tap = mean(seg.all$dur.tap.mean[isd], na.rm = T)
    itv.tap = mean(seg.all$wait.tap.mean[isd], na.rm = T)
    
    rate.sld = mean(seg.all$rate.slides[isd], na.rm = T)
    dur.sld = mean(seg.all$dur.slide.mean[isd], na.rm = T)
    dist.sld = mean(seg.all$dist.slide.mean[isd], na.rm = T)
    speed.sld = mean(seg.all$speed.slide.mean[isd], na.rm = T)
    itv.sld = mean(seg.all$wait.slide.mean[isd], na.rm = T)
    d = c(g, s, rate.act, rate.tap, dur.tap, itv.tap, 
          rate.sld, dur.sld, dist.sld, speed.sld, itv.sld)
    cat(d, "\n")
    
  }
}
sink()

d = read.table("game_feature.txt")
names(d) = name.d2
fn.d = paste("d2/",p,"/",p,"_",gl$code[i.g],".txt",sep="")
write.table(d2, file = fn.d2)

> names(seg.all)
[1] "rate.action"            "rate.slides"            "rate.taps"             
[4] "ratio.action.active10"  "ratio.action.active100" "ratio.dir.hor.slide"   
[7] "ratio.dir.other.slide"  "ratio.dir.ver.slide"    "dir.sin.slide.max"     
[10] "dir.sin.slide.mean"     "dir.sin.slide.sd"       "dir.cos.slide.max"     
[13] "dir.cos.slide.mean"     "dir.cos.slide.sd"       "ratio.butt.slides"     
[16] "ratio.butt.taps"        "burst.action.10"        "burst.action.5"        
[19] "dur.action.max"         "dur.action.mean"        "dur.action.med"        
[22] "dur.action.p75"         "dur.action.p95"         "x.action.end.mean"     
[25] "x.action.end.sd"        "x.action.movement.mean" "x.action.movement.sd"  
[28] "x.action.start.mean"    "x.action.start.sd"      "y.action.end.mean"     
[31] "y.action.end.sd"        "y.action.movement.mean" "y.action.movement.sd"  
[34] "y.action.start.mean"    "y.action.start.sd"      "dur.tap.max"           
[37] "dur.tap.mean"           "dur.tap.med"            "dur.tap.p75"           
[40] "dur.tap.p95"            "x.tap.mean"             "x.tap.sd"              
[43] "y.tap.mean"             "y.tap.sd"               "dur.slide.max"         
[46] "dur.slide.mean"         "dur.slide.med"          "dur.slide.p75"         
[49] "dur.slide.p95"          "dist.slide.max"         "dist.slide.mean"       
[52] "dist.slide.med"         "dist.slide.p75"         "dist.slide.p95"        
[55] "disp.slide.max"         "disp.slide.mean"        "disp.slide.med"        
[58] "disp.slide.p75"         "disp.slide.p95"         "speed.slide.max"       
[61] "speed.slide.mean"       "speed.slide.med"        "speed.slide.p75"       
[64] "speed.slide.p95"        "multi.dur.mean"         "multi.ratio.action"    
[67] "wait.action.mean"       "wait.action.sd"         "wait.slide.mean"       
[70] "wait.slide.sd"          "wait.tap.mean"          "wait.tap.sd"           
[73] "user"



# Parse information==================
for(game in gcode$gc){
  fn.d3 = paste("d3/",  game, sep="")
  sink(fn.d3)
  for(sub in allsub) {
    # list all subject files =================
    fn.full = list.files(sub, full=T)         #list all the path
    fn.part = list.files(sub)                 #list file names only
    
    # find the game log =================  
    n = grep(game, fn.part)   #回傳fn.part的位置，如果吻合
    if(length(n)==0) 
      next
    
    # read file =================
    d2 = read.table(fn.full[n], header=T)
    names(d2)<-name.d2 #time, multi-event, event, pos.x, pos.y, press
    
    #clean the record which is in the prev game===============
    if(d2$id[1]==ntect){  # if the first record is not a start of actions
      act.last=which(d2$id!=ntect)[1]-1
      if(is.na(act.last))
        stop()
      d2 <- d2[-(1:act.last),]
    }
    
    #若x,y,prs為ntect(因為資料在上個遊戲)，拿掉data
    d2 <- d2[d2$x!=ntect & d2$y!=ntect,]
    d2$prs[d2$prs==ntect] <- NA
    
    
    #if "sum.l>thd.dis" means that the type is belong to "slide"  
    thd.dis = 48  
    # To find the id list in each trace
    id.list = unique(d2$id)
    
    for(j in id.list)
    {
      isd = which(d2$id == j)
      dx = diff(d2$x[isd])
      dy = diff(d2$y[isd])
      vec = cbind(dx, dy)
      st = d2$t[min(isd)]
      et = d2$t[max(isd)]
      # Velocity & Speed =======================
      part.l = sqrt(dx^2+dy^2)
      part.t = diff(d2$t[isd])
      sum.l = sum(part.l)
      sum.t = diff(range(d2$t[isd]))
      if(sum.l < thd.dis){
        type="tap"
        mean.s = NA
        mean.v = NA
        sd.v = NA
      }else{
        type="sld"
        mean.s = sum.l/sum.t
        part.v = part.l/part.t
        mean.v = mean(part.v, na.rm = T)
        sd.v = sd(part.v, na.rm = T)
      } 
      
      # Pressure=========================
      mean.prs = mean(d2$prs[isd])
      sd.prs = sd(d2$prs[isd], na.rm = T)
      # game feature belong to one subject
      d3 = c(game, sub, type, sum.l, st, et, sum.t, mean.prs, sd.prs, mean.v, sd.v)
      cat(d3, "\n")
    }
  }
  sink()
}

sink("d5")
for(game in gcode$gc){
  
  fn.d3 = paste("d3/",  game, sep="")
  d3 = read.table(file = fn.d3)
  names(d3) = c("gn", "sub", "type", "sum.l", "st", "et", "sum.t", 
                "mean.prs", "sd.prs", "mean.v", "sd.v")
  sub.list = unique(d3$sub)
  fn.d4 = paste("d4/",  game, sep="")
#   sink(fn.d4)
  for(s in sub.list){
    isd = which(d3$sub == s)
    t.s = d3$st[min(isd)]
    t.e = d3$et[max(isd)]
    t.all = t.e-t.s
    num.act = len(isd)
    frq.act = num.act/t.all
    
    id.tap = which(d3$type[isd]=="tap")
    num.tap = len(id.tap)
    frq.tap = num.tap/t.all
    ratio.tap = num.tap/num.act
    dur.tap = mean(d3$sum.t[id.tap], na.rm=T)
    
    id.sld = which(d3$type[isd]=="sld")
    num.sld = len(id.sld)
    frq.sld = num.sld/t.all
    ratio.sld = num.sld/num.act
    dis.sld = mean(d3$sum.l[id.sld], na.rm=T)
    d4 = c(game, s, t.all, num.act, frq.act, 
           num.tap, frq.tap, ratio.tap, dur.tap,
           num.sld, frq.sld, ratio.sld, dis.sld)
    cat(d4, "\n")
  }
#   sink()
}
sink()

d5 = read.table("d5")
names(d5) = c("gn", "sub", "t.all", "num.act", "frq.act", 
              "num.tap", "frq.tap", "ratio.tap", "dur.tap",
              "num.sld", "frq.sld", "ratio.sld", "dis.sld")
game.list = unique(d5$gn)
plot(d5$t, d5$prs, type="n", 
     xlim = range(d5$frq.tap), 
     ylim = range(d5$frq.sld),
     xlab = "num.tap",
     ylab = "num.sld")

for(g in 1:50){
  isd = which(d5$gn==unique(d5$gn)[g])
  points(d5$frq.tap[isd], d5$frq.sld[isd], col=g)
}

f.list = c(4:13)
d5 = d5[-268,f.list]
d5 = scale(d5)
cl <- kmeans(d5, 2)
plot(d5, col = cl$cluster)
points(cl$centers, col = 1:5, pch = 8, cex = 2)
ss <- function(d5) sum(scale(d5, scale = FALSE)^2)

for(game in gcode$gc){
  d4 = read.table(file = fn.d4)
  names(d4) = c("gn", "sub", "t.all", "num.act", "frq.act", 
                "num.tap", "frq.tap", "ratio.tap", "dur.tap",
                "num.sld", "frq.sld", "ratio.sld", "dis.sld")
}