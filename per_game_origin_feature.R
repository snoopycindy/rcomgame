setwd("e:/Dropbox/workspace/done_app_yx/R")
# source("../R.source/common.R")
# source("../R.source/my.fig.R")
# source("../R.source/my.legend.R")
source("../R.src/rey.f.R")
source("../R.src/yx.common.R")
source("../R.src/route.R")

# YX's features===========================
# 1st Parse info - yx ==================
#處理後的log檔
fdir_yx <- "../gestureLog/per_game_table_sld-tap"
adir <- "../gestureLog/1parseData/"
thd.dis = 20
for(game in gcode$gc){
  fn.part = list.files(adir, pattern=game)
  fn.d3 = paste(fdir_yx, "/", game, ".txt", sep="")
  sink(fn.d3)
  for(p in 1:len(p.list)) {
    # find the game log =================  
    n = grep(p.list[p], fn.part) #回傳fn.part的位置，如果吻合
    if(length(n)==0) 
      next
    
    # read file =================
    #time, multi-event, event, pos.x, pos.y, press
    #names(d2) = c("t", "sl", "id", "lv", "x", "y", "prs") 
    fn = paste("../gestureLog/1parseData/", p.grp[p], "_",
               p.list[p], "_", game, ".txt", sep="")
    d2 = read.table(fn, header=T)
    
    
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
      
    # To find the id list in each trace
    id.list = unique(d2$id)
    
    for(j in id.list){
      isd = which(d2$id == j)
      st = d2$t[min(isd)]
      et = d2$t[max(isd)]
      sx = d2$x[min(isd)]
      sy = d2$y[min(isd)]
      ex = d2$x[max(isd)]
      ey = d2$y[max(isd)]
      
      # Speed =======================
      dx = diff(d2$x[isd])
      dy = diff(d2$y[isd])
      part.l = sqrt(dx^2+dy^2)
      part.t = diff(d2$t[isd])
      sum.l = sum(part.l)
      sum.t = diff(range(d2$t[isd]))
      if(sum.l <= thd.dis){
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
      mean.prs = mean(d2$prs[isd], na.rm = T)
      sd.prs = sd(d2$prs[isd], na.rm = T)
      # game feature belong to one subject
      d3 = c(game, p.list[p], type, 
             sx, sy, ex, ey, sum.l, 
             st, et, sum.t, mean.v, sd.v,
             mean.prs, sd.prs)
      cat(d3, "\n")
#       fn.d3 = paste(fdir_yx1, "/", p.list[sub], "_", game, ".txt", sep="")
#       write.table(d3, file=fn.d3, append=T)
      
    }
    
  }
  sink()
}

#calculat the average interval between action
cal_itv = function(st,et){
  itv_d = {}
  for(i in 2:len(st)){
    itv_d = c(itv_d, st[i]-et[i-1])
  }
  mean(itv_d)
}

# fdir_yx <- "../gesture log/per_game_table_sub-feature_yx"
fn.d4 = paste(fdir_yx, "/allgame.txt", sep="")
sink(fn.d4)
for(game in gcode$gc){
  fn.d3 = paste(fdir_yx, "/", game, ".txt", sep="")
  d3 = read.table(file = fn.d3)
  names(d3) = c("gn", "sub", "type", 
                "sx", "sy", "ex", "ey","sum.l", 
                "st", "et", "sum.t", "mean.v", "sd.v",
                "mean.prs", "sd.prs")
  sub.list = unique(d3$sub)
  
  for(s in sub.list){
    now.d = d3[d3$sub==s,]
    t.s = min(now.d$st) 
    t.e = max(now.d$et)
    t.all = t.e-t.s
    num.act = nrow(now.d)
    frq.act = num.act/t.all # action frequency
    itv.act = cal_itv(now.d$st, now.d$et) # action interval
    
    d.tap = now.d[now.d$type=="tap",]
    num.tap = nrow(d.tap)
    frq.tap = num.tap/t.all
    ratio.tap = num.tap/num.act
    dur.tap = mean(d.tap$sum.t, na.rm=T)
    itv.tap = cal_itv(d.tap$st, d.tap$et)
    x.tap.start.sd = sd(d.tap$sx, na.rm=T)
    y.tap.start.sd = sd(d.tap$sy, na.rm=T)
    x.tap.end.sd = sd(d.tap$ex, na.rm=T)
    y.tap.end.sd = sd(d.tap$ey, na.rm=T)
    
    d.sld = now.d[now.d$type=="sld", ]
    num.sld = nrow(d.sld)
    frq.sld = num.sld/t.all
    ratio.sld = num.sld/num.act
    dur.sld = mean(d.sld$sum.t, na.rm=T)
    itv.sld = cal_itv(d.sld$st, d.sld$et)
    dis.sld = mean(d.sld$sum.l, na.rm=T)
    spd.sld = mean(d.sld$mean.v, na.rm=T)
    spd_sd.sld = mean(d.sld$sd.v, na.rm=T)
    x.sld.start.sd = sd(d.sld$sx, na.rm=T)
    y.sld.start.sd = sd(d.sld$sy, na.rm=T)
    x.sld.end.sd = sd(d.sld$ex, na.rm=T)
    y.sld.end.sd = sd(d.sld$ey, na.rm=T)
    
    
    d4 = c(game, s, t.all, num.act, frq.act, itv.act, 
           num.tap, frq.tap, ratio.tap, dur.tap, itv.tap,
           x.tap.start.sd, y.tap.start.sd, x.tap.end.sd, y.tap.end.sd,
           num.sld, frq.sld, ratio.sld, dur.sld, itv.sld, 
           dis.sld, spd.sld, spd_sd.sld,
           x.sld.start.sd, y.sld.start.sd, x.sld.end.sd, y.sld.end.sd)
    cat(d4, "\n")
  }

}
sink()

std={}
for(p in 1:len(p.list)){
  fn = paste(sdir, p.grp[p], "_", p.list[p],".csv",sep="")
  d = read.table(fn)
  d = cbind(p.list[p], d)
  std = rbind(std, d)
}
names(std)[1] = "sub"

# add the rate from subject 
d4 = read.table(fn.d4)
names(d4) = c("game", "sub", "t.all", "num.act", "frq.act", "itv.act", 
              "num.tap", "frq.tap", "ratio.tap", "dur.tap", "itv.tap",
              "x.tap.start.sd", "y.tap.start.sd", "x.tap.end.sd", "y.tap.end.sd",
              "num.sld", "frq.sld", "ratio.sld", "dur.sld", "itv.sld", 
              "dis.sld", "spd.sld", "spd_sd.sld",
              "x.sld.start.sd", "y.sld.start.sd", "x.sld.end.sd", "y.sld.end.sd")
d={}
for(p in p.list){
  data.now = d4[d4$sub==p,]
  std.now = std[std$sub==p,]
#   fn.std = paste(sdir,"/",p,".csv",sep="")
#   std = read.table(fn.std)
#   std = std[,-(1:3)]
  
  game.list = unique(data.now$game)
  d6 = data.frame()
  for(game in game.list){
    n = grep(game, std.now$code)   #回傳fn.part的位置，如果吻合
    if(length(n)==0){
      cat(game)
      next
    } 
      
    d6 = rbind(d6, std.now[n,])
  }
  d6 = d6[,6:12]
  
  d6 = cbind(data.now, d6)
  d = rbind(d, d6)
}

fn.d = paste(fdir_yx, "/allgame_rating.txt", sep="")
write.table(d[order(d$game),], file = fn.d)

