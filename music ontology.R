
cumu<-function(x){
  c(0,sapply(1:length(x),function(a)sum(x[1:a])))
}
major.interval<-c(2,2,1,2,2,2,1)
minor.interval<-c(2,1,2,2,1,2,2)
mixolydian.interval<-c(2,2,1,2,2,1,2)
pitch<-cumu(c(major.interval,major.interval,major.interval))
names(pitch)<-LETTERS[c(3:7,1:7,1:7,1:3)]

find.notes<-function(interval,root){
  if(sum(interval)!=12){
    stop("the interval sum is not 12!")
  }else{
    key.pitch<-cumu(interval)[-8]
  }
  if(nchar(gsub("[A-G#]","",toupper(root)))>0) stop("illegal characters in root")
  root.pitch.adj<-root.pitch<-pitch[which(substring(root,1,1)==names(pitch)[2:8])+1]
  if(nchar(root)>1){root.pitch.adj<-root.pitch+ifelse(substring(root,2,2)=="#",1,-1)}
  origin.pitch<-pitch[which(root.pitch==pitch)+0:6]
  diff<-root.pitch.adj+key.pitch-origin.pitch
  note.name<-names(origin.pitch)
  sharp.flat<-sapply(diff,function(x){
    if(x==0) return("")
    else{
      char<-ifelse(x>0,"#","b")
      return(paste(rep(char,abs(x)),collapse=""))
    }
  })
  note.name<-paste(note.name,sharp.flat,sep="")
  return(note.name)
}





