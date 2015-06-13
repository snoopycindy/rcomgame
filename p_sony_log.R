name.d = c("time", "type", "event", "value")
name.d2 = c("t", "sl", "id", "lv", "x", "y", "prs")
none = -9
names(d) = name.d
d$value = (paste("0x", d$value, sep=""))
t = sl = id = lv = x = y = prs = none

sink("data2.txt")
for(i in 1:nrow(d))
{
  t = d$time[i]
  
  if(d$event[i] == "SYN_REPORT"){
    if(d$event[i-1] == "ABS_MT_TRACKING_ID" && d$value[i-1] == "0xffffffff")
    {
      lv = 1
      d2 = c(t, sl, id, lv, x, y, prs)
      cat(d2, "\n")
      t = sl = id = lv = x = y = prs = none
#       next
    }else{
      d2 = c(t, sl, id, lv, x, y, prs)
      cat(d2, "\n")
      t = sl = id = lv = x = y = prs = none
    }
  }else if(d$event[i] == "ABS_MT_SLOT"){
    if(i == 1){
      sl = as.numeric(d$value[i])
    }else if(d$event[i-1] != "SYN_REPORT"){
      d2 = c(t, sl, id, lv, x, y, prs)
      cat(d2, "\n")
      t = sl = id = lv = x = y = prs = none
      sl = as.numeric(d$value[i])
    }else{
      sl = as.numeric(d$value[i])
    }
  }else if (d$event[i] == "ABS_MT_TRACKING_ID"){
    if(d$value[i] == "0xffffffff")
      id = none
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
sink()


d2 = read.table("data2.txt")
names(d2) = name.d2

# change the data type===============
d2$t = as.numeric(as.character(d2$t))
d2$x = as.numeric(as.character(d2$x))
d2$y = as.numeric(as.character(d2$y))
d2$prs = as.numeric(as.character(d2$prs))

#檢查資料，如果第一筆資料的id沒有值，代表屬於前一個資料
i.nouse=min(which(d2$id != none))-1
if(i.nouse>0)
  d2 = d2[-(1:i.nouse),]


# 處理 multi- actions: 把屬於那個slot的所有records，其sl改為它的slot==========
mul.but=which(d2$sl!=none)
if(len(mul.but)){
  for(s in mul.but){
    if(s==nrow(d2))   #若此multi action是最後一個record, 不記錄
      next
    ev.act=which(d2$id[(s+1):nrow(d2)]!=none)
    sl.act=which(d2$sl[(s+1):nrow(d2)]!=none)
    if(is.na(rey.min(ev.act,sl.act)))
      d2$sl[s:nrow(d2)]=d2$sl[s]  #後面都是ntect的資料，表示到最後都屬於這個multi-action
    else{
      e=rey.min(ev.act,sl.act)
      d2$sl[s:(s+e-1)]=d2$sl[s]
    }
  }
}


# yx 處理lv : lv=1 代表手指離開==========
isd = which(d2$id!=none)
end = len(isd)-1
for(i in 1:end){
  #     cat(i,"\n")
  s = isd[i]
  e = isd[i+1]
  if(s == nrow(d2))
    next
  if(d2$lv[e-1]==1)
    next
  if(d2$sl[s] == none && d2$sl[e] != none)
    d2$sl[s:(e-1)]=0
}


#消除x,y,prs等於none的狀況=========
for(dn in c('x','y','prs')){
  dn.n=which(d2[,dn]==none)
  for(s in dn.n){
    if(s==1)
      next  #若此x.none是第一筆資料，不處理
    sl.s=intersect(which(d2$sl[1:s]== d2$sl[s]), which(d2[,dn]!=none))
    sl.n=intersect(which(d2$sl[1:s]==none), which(d2[,dn]!=none))
    if(is.na(rey.max(sl.s,sl.n)))
      next
    else
      d2[s,dn]=d2[rey.max(sl.s,sl.n),dn]
    
  }
}

# 如果還有None 輸出==========
for(i in 2:nrow(d2))
{
  if(d2$x[i]==none) print(paste(i,"terrible x"))
  if(d2$y[i]==none) print(paste(i,"terrible y"))
  if(d2$prs[i]==none) print(paste(i,"terrible prs"))
}

#將id改好==============
for(i in 1:nrow(d2))
{
  if(d2$id[i] == none){
    if(d2$sl[i] == none){
      d2$id[i] = d2$id[i-1]
    }else{
      for(j in (i-1):1){
        if(d2$sl[i] == d2$sl[j]){
          d2$id[i] = d2$id[j]
          break
        }   
      }
    }
    
  }
}

