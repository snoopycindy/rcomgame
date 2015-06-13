# setwd("C:/rey/done_app_proj_gesture/R")
setwd("e:/Dropbox/workspace/done_app_yx/R")
source("../R.source/common.R")
source("../R.source/my.fig.R")
source("../R.source/my.legend.R")
source("../R.source/rey.f.R")
# utils:::menuInstallPkgs()
#設定讀取路徑

#每個玩家遊戲時間的資料
gdir <- "../gesture log/gl"

#原始log檔
ldir <- "../gesture log/log"

#處理後的log檔
adir <- "../gesture log/accomplish"
#處理後的std檔
sdir <- "../gesture log/std"

#遊戲跟其code的列表
gn <- "../gesture log/gl/gn.txt"

name.std = c("t", "at", "rt", "order", "gn", "pn", "p", "pc")
#設定參數
ntect = -9 #設定none detect為-9
time_limit = 60 #game log檔至少要60S以上
gcode = read.table(gn, header=T, sep="\t", as.is=T, encoding="UTF-8") #讀取game列表
p.list=c('1201sub01','1202sub01','1202sub02', '1203sub01', '1205sub01','1205sub02') #設定player列表


sapply(p.list,function(x) dir.create(file.path(adir,x), showWarnings = FALSE)) #create directory
bug.p=list()              #存取有bug的遊戲 (錄製時間有重疊的遊戲)

for(p in p.list){
  
  #讀取game name, start time, end time 到gl
  fn=paste(gdir,"/",p,".csv",sep="")
  gl = read.csv(fn, header=F, as.is=T, encoding="UTF-8")
  names(gl)=name.std #設定column name
  
  sink("std.txt")
  for(i in unique(gl$order)){
    i.s = min(which(gl$order==i))
    i.e = max(which(gl$order==i))
    cat(gl$pn[i.s], gl$t[i.s], gl$t[i.e],"\n")
  }
  sink()
  
  #替gl加上遊戲代號 (game code)
  gl=data.frame()
  gl=read.table("std.txt")
  names(gl)=c("pn", "st", "et") #設定column name
  gc <- sapply(1:len(gl$pn),function(x) gcode$gc[gcode$pn==gl$pn[x]])
  gl$code <- gc
  gl <- gl[order(gl$st),]
  #替gl加上遊戲評價 (game evaluation)
  fn=paste(gdir,"/",p,"_e.csv",sep="")
#   fn=paste(odir, "fnstd/",p,"_e.csv",sep="")
  std_e = read.table(fn, header=F, as.is=T)
  names(std_e) = c("all", "action", "content") #設定column name
  gl <- cbind(gl,std_e)
  #save the std data
  fn.std = paste(sdir,"/",p,".csv",sep="")
  write.table(gl, file = fn.std)
  
  
  #讀取touch log data到d
  log.name = c("time", "type", "event", "value")
  event = c("t", "sl", "id", "x", "y", "prs")
  fn=paste(ldir,"/",p,".s",sep="")
  d = read.table(fn, header=F, as.is = T)
  d = d[,-1]
  d[1] = unlist(strsplit(as.character(d[,1]), "]"))
  names(d) = log.name
  d$value = (paste("0x", d$value, sep=""))  #轉成16進位的模式
  t = sl = id = x = y = prs = ntect


  #找到記錄時間有重疊的遊戲
  bug.t=sapply(1:nrow(gl),function(x) gl$st[x+1]-gl$et[x])
  bug.p=append(bug.p,list(which(bug.t<0)))
  print(bug.t)
  
  #處理時間差的部分(因為遊戲記錄時間跟實際log檔時間有差距)
#   t.err=as.numeric(min(gl$st))-as.numeric(min(d$time))
#   gl$st = gl$st-t.err
#   gl$et = gl$et-t.err
  
  # 1st parse
  for(i.g in 1:nrow(gl))
  {
    if(i.g<nrow(gl)){   #若i.g不是最後一款game的話
      if(bug.t[i.g]<0) next                    #如果這款遊戲跟下款遊戲時間有重疊，跳掉
      if(i.g!=1) if(bug.t[i.g-1]<0) next       #如果這款遊戲跟上款遊戲時間有重疊，跳掉
    }
    name.info = paste(adir,"/",p,"/",p,"_", gl$code[i.g], ".txt", sep="")
    i.s = min(which(as.numeric(d$time)>gl$st[i.g]))   #這玩家在這款遊戲的起始rownumber
    i.e = max(which(as.numeric(d$time)<gl$et[i.g]))   #這玩家在這款遊戲的結束rownumber
    
    cat(name.info,"  ",i.s, " ", i.e, "\n",sep="")
    
    if(i.e<=i.s) {        #若結束時間比起始時間早，bug 跳掉
      print("Start, End time wrong" )
      next
    }

    if(i.e-i.s<time_limit) {        #若結束時間比起始時間早，bug 跳掉
      print("log time is too short in this game" )
      next
    }
    
    #=========開始parse原始log檔===============
    d2 = data.frame()
    for(i in i.s:i.e) #在切好的遊戲時間內，針對每一筆log檔的資料
    {
      t = d$time[i]
      
      if(d$event[i] == "SYN_REPORT"){
        d2 = rbind(d2, cbind(t, sl, id, x, y, prs))
        t = sl = id = x = y = prs = ntect
      }else if(d$event[i] == "ABS_MT_SLOT"){
        if(i == 1){
          sl = as.numeric(d$value[i])
        }else if(d$event[i-1] != "SYN_REPORT"){
          d2 = rbind(d2, cbind(t, sl, id, x, y, prs))
          t = sl = id = x = y = prs = ntect
          sl = as.numeric(d$value[i])
        }else{
          sl = as.numeric(d$value[i])
        }
      }else if (d$event[i] == "ABS_MT_TRACKING_ID"){
        if(d$value[i] == "0xffffffff")
          id = ntect
        else
          id = as.numeric(d$value[i])
      }else if (d$event[i] == "ABS_MT_POSITION_X"){
        x = as.numeric(d$value[i])
      }else if (d$event[i] == "ABS_MT_POSITION_Y"){
        y = as.numeric(d$value[i])
      }else if (d$event[i] == "ABS_MT_PRESSURE"){
        prs = as.numeric(d$value[i])
      }else{
        next()
      }
    }
    
    # change the data type (factor => numeric)===============
    d2$t = as.numeric(as.character(d2$t))
    d2$x = as.numeric(as.character(d2$x))
    d2$y = as.numeric(as.character(d2$y))
    d2$prs = as.numeric(as.character(d2$prs))
    
    
    #檢查資料，如果第一筆資料的id沒有值，代表屬於前一個資料
    i.nouse=min(which(d2$id != ntect))-1
    if(i.nouse>0)
      d2 = d2[-(1:i.nouse),]
    
    # 處理 multi- actions: 把屬於那個slot的所有records，其sl改為它的slot==========
    mul.but=which(d2$sl!=ntect)
    for(s in mul.but){
      if(s==nrow(d2))   #若此multi action是最後一個record, 不記錄
        next
      ev.act=which(d2$id[(s+1):nrow(d2)]!=ntect)
      sl.act=which(d2$sl[(s+1):nrow(d2)]!=ntect)
      if(is.na(rey.min(ev.act,sl.act)))
        d2$sl[s:nrow(d2)]=d2$sl[s]                         #後面都是ntect的資料，表示到最後都屬於這個multi-action
      else{
        e=rey.min(ev.act,sl.act)
        d2$sl[s:(s+e-1)]=d2$sl[s]
      }
    } 
    
    #消除x,y,prs等於ntect的狀況
    for(dn in c('x','y','prs')){
      dn.n=which(d2[,dn]==ntect)
      for(s in dn.n){
        if(s==1)
          next  #若此x.ntect是第一筆資料，不處理
        sl.s=intersect(which(d2$sl[1:s]== d2$sl[s]), which(d2[,dn]!=ntect))
        sl.n=intersect(which(d2$sl[1:s]==ntect), which(d2[,dn]!=ntect))
        if(is.na(rey.max(sl.s,sl.n)))
          next
        else
          d2[s,dn]=d2[rey.max(sl.s,sl.n),dn]        
      }
    }
    #寫入至.txt檔
    write.table(d2, name.info, sep = "\t")  
  }
}  