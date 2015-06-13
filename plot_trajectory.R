adir_id <- "../gestureLog/1parseData/"
p.list=c('0603ot01', '0603ot02', '0603ot03', '0603ot04', '0605ot01','0605ot02',
         '0609ot01', '0609ot02') #設定player列表
picdir = "../pic/"

#read std
std = {}
for(p in p.list){
  fn = paste("../gestureLog/gameStd/", p, ".csv", sep="")
  d = read.table(fn)
  d = cbind(p, d)
  std = rbind(std, d)
}


# list all subject files =================
#list file names only
for(game in gcode$gc){
  pic.name = paste(picdir, "pic_trajectory/", game, ".png",sep="")
  png(file = pic.name, width = 800, height = 800)
  par(mfrow=c(2,4))
#   fn.full = list.files(adir_id, full=T, pattern=game)
  fn.part = list.files(adir_id, pattern=game)
    
  for(p in 1:len(p.list)) {  
    # find the game log =================  
    n = grep(p.list[p], fn.part)   #回傳fn.part的位置，如果吻合
    if(length(n)==0){
      xlim = c(0, 4000)
      ylim = c(0, 4000)
      plot(0, 0, type = "n",
           xlim = xlim, ylim=rev(ylim), #because the pad y is from top to down
           xlab="x", ylab ="y", main = paste(p.list[p], game))
      text(2000, 2000, "None", cex=5)
      next
    } 
      
    
    # read file =================
    
    fn = paste("../gestureLog/1parseData/", p.list[p], "_", game, ".txt", sep="")
    d2 = read.table(fn, header=T)
    
    

    # To find the id list in each trace
    id.list = unique(d2$id)
    xlim = c(0, 1920)
    ylim = c(0, 1200)
    plot(0, 0, type = "n",
         xlim = xlim, ylim=rev(ylim), #because the pad y is from top to down
         xlab="x", ylab ="y", main = paste(p.list[p], game))
    id_rating = which(std$code==game & std$p==p.list[p])
    if(len(id_rating))
      text(0, 0, std$all[id_rating], col=2, cex=3)
    else
      text(0, 0, "Na", col=2, cex=3)
    
    for(j in 1:len(id.list))
    {
      isd = which(d2$id == id.list[j])
      mean.prs = mean(d2$prs[isd])
      lines(d2$x[isd], d2$y[isd], pch = mypch[j], lwd=2, col=mycol[3])
    }
    
  }
  dev.off()
}