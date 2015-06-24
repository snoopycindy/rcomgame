adir_id <- "../gestureLog/1parseData/"

picdir = "../pic/"

#read std
std={}
for(p in 1:len(p.list)){
  fn = paste(gdir, "gameStd/", p.grp[p], "_", p.list[p],".csv",sep="")
  d = read.table(fn)
  d = cbind(p.list[p], d)
  std = rbind(std, d)
}
names(std)[1]="sub"


# list all subject files =================
#list file names only
for(game in gcode$gc){
  pic.name = paste(picdir, "pic_trajectory/", game, ".png",sep="")
  # png(file = pic.name, width = 800, height = 800)
  
  fn.full = list.files(adir_id, full=T, pattern=game)
  fn.part = list.files(adir_id, pattern=game)
  if(len(fn.part)>6)
    par(mfrow=c(3,3))
  else
    par(mfrow=c(2,3))
  
  for(f in 1:len(fn.full)){
    d2 = read.table(fn.full[f], header=T)
    
    id.list = unique(d2$id)
    xlim = c(0, 1920)
    ylim = c(0, 1200)
    plot(0, 0, type = "n",
         xlim = xlim, ylim=rev(ylim), #because the pad y is from top to down
         xlab="x", ylab ="y", main = paste(p.list[p], game))
    
    fn = paste(gdir, "gameStd/", p.grp[p], "_", p.list[p],".csv",sep="")
    std = read.table(fn)
    text(0, 0, std$all[std$code==game], col=2, cex=2)
    
    
    for(j in 1:len(id.list))
    {
      isd = which(d2$id == id.list[j])
      mean.prs = mean(d2$prs[isd])
      lines(d2$x[isd], d2$y[isd], pch = mypch[j], lwd=2, col=mycol[3])
    }
  }
  
    

  # dev.off()
}

for(game in gcode$gc){
  pic.name = paste(picdir, "pic_trajectory/", game, ".png",sep="")
  fn.full = list.files(adir_id, full=T, pattern=game)
  fn.part = list.files(adir_id, pattern=game)
  
  png(file = pic.name, width = 1500, height = 600)
  par(mfrow=c(3,5))
  
  for(p in 1:len(p.list)) {  
  
  # find the game log =================  
  n = grep(p.list[p], fn.part)   #回傳fn.part的位置，如果吻合
  if(length(n)==0){
    xlim = c(0, 1920)
    ylim = c(0, 1200)
    plot(0, 0, type = "n",
         xlim = xlim, ylim=rev(ylim), #because the pad y is from top to down
         xlab="x", ylab ="y", main = paste(p.list[p], game))
    text(1000, 1000, "None", cex=2)
    next
  } 
  
  
  # read file =================
  
  fn = paste("../gestureLog/1parseData/", p.grp[p], "_",
             p.list[p], "_", game, ".txt", sep="")
  d2 = read.table(fn, header=T)
  
  
  
  # To find the id list in each trace
  id.list = unique(d2$id)
  xlim = c(0, 1920)
  ylim = c(0, 1200)
  plot(0, 0, type = "n",
       xlim = xlim, ylim=rev(ylim), #because the pad y is from top to down
       xlab="x", ylab ="y", main = paste(p.list[p], game))
  fn = paste(gdir, "gameStd/", p.grp[p], "_", p.list[p],".csv",sep="")
  std = read.table(fn)
  text(2, 2, std$all[std$code==game], col=2, cex=2)
  
  for(j in 1:len(id.list))
  {
    isd = which(d2$id == id.list[j])
    mean.prs = mean(d2$prs[isd])
    lines(d2$x[isd], d2$y[isd], pch = mypch[j], lwd=2, col=mycol[3])
  }
  
  }
  
  dev.off()
}
