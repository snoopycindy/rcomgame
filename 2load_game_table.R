


#table:game name vs game code 

allsub = list.files(path=adir, full=T)
name.all.s=regexpr("[^[:punct:]]+$",allsub)
name.all=sapply(1:length(name.all.s),function(x) substr(allsub[x],start=name.all.s[x],stop=nchar(allsub[x])))

slen <-list(xmax=0,xmin=9999,ymax=0,ymin=9999) #calculate the max and min of (x,y) in the touch pad
for(game in gcode$code){
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
