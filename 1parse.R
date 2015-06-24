# setwd("/home/yaxuan/rcomgame/R")
setwd("/Users/yxhung/Dropbox/workspace/rcomgame/R" )
source("../R.src/common.R")
source("../R.src/yx.common.R")
source("../R.src/route.R")
# utils:::menuInstallPkgs()
# install.packages("")
require(Hmisc)
require(plotrix)
library(Hmisc)
library(plotrix)

name.d = c("time", "type", "event", "value")
name.d2 = c("t", "sl", "id", "lv", "x", "y", "prs")
name.std = c("t", "at", "rt", "order", "gn", "pn", "p", "pc")
none = -9

# utils:::menuInstallPkgs()

#gesture log data
gldir <- "../gestureLog/"

#設定參數
ntect = -9 #設定none detect為-9
time_limit = 60 #game log檔至少要60S以上
#讀取game列表
gcode = read.table("../gestureLog/gNameCode.txt", 
                   header=T, sep="\t", as.is=T, encoding="UTF-8") 
#讀取game 評價
geval = read.table("../gestureLog/gEvaluate.txt", 
                   header=T, as.is=T, sep="\t", encoding="UTF-8")

for(p in 1:len(p.list)){
  
  #讀取game name, start time, end time 
  #stdand data
  fn=paste(gldir, "gameLog/",p.list[p],".csv",sep="")
  std = read.table(fn, header=F, as.is=T, sep=",")
  names(std) = name.std
  sink("std.txt")
  for(i in unique(std$order)){
    i.s = min(which(std$order==i))
    i.e = max(which(std$order==i))
    cat(std$pn[i.s], std$t[i.s], std$t[i.e],"\n")
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
  gl <- cbind(gl, geval[geval$subcode==p.list[p],2:8])
  #save the std data
  fn.std = paste(gldir, "gameStd/", p.grp[p], "_", p.list[p],".csv",sep="")
  write.table(gl, file = fn.std)
  
  
  #讀取touch log data到d
  fn=paste(gldir, "touchLog/", p.list[p],".s",sep="")
  alld = read.table(fn, header=F, as.is = T)
  alld = alld[,-1]
  alld[1] = unlist(strsplit(as.character(alld[,1]), "]"))
  names(alld)=name.d
  for(i.g in 1:nrow(gl)){
    i.s = min(which(as.numeric(alld$time)>gl$st[i.g]))       #這玩家在這款遊戲的起始rownumber
    i.e = max(which(as.numeric(alld$time)<gl$et[i.g]))       #這玩家在這款遊戲的結束rownumber
    name.info = paste(p ,gl$code[i.g],sep=" ")
#     cat(name.info, gl$st[i.g], alld$time[i.s], gl$et[i.g], alld$time[i.e], "\n",sep=" ")
    cat(name.info, i.s, i.e, "\n",sep=" ")
    
    #若結束時間比起始時間早，bug 跳掉
    if(i.e<=i.s) {        
      print("Start, End time wrong" )
      next
    }
    
    #若log時間過短，bug 跳掉
    if(gl$et[i.g]-gl$st[i.g]<time_limit) {        
      print("log time is too short in this game" )
      next
    }
    
    d = alld[i.s:i.e,]
    source("p_sony_log.R")
#     fn.d2 = paste(gldir, "1parseData/", p.grp[p], "/", p.list[p],"/",p.list[p],"_", 
#                       gl$code[i.g], ".txt", sep="")
    fn.d2 = paste(gldir, "1parseData/", p.grp[p], "_", p.list[p], "_", 
                  gl$code[i.g],".txt",sep="")
    write.table(d2, file = fn.d2)
  }
  
}  
