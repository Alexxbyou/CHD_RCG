#full scale, based on piano key board
setwd("G:/tab analysis/music ontology")
full.pitch<-mode.scale(major.interval,start=6,no.octave=ceiling(88/12),rep.name=T,back.to.root=T)
full.pitch<-full.pitch[full.pitch<=87]
name<-rep(LETTERS[1:7],9)[1:length(full.pitch)]

sharp.flat.trans<-function(piano.keyboard,trans.type=1,no.trans=1){
  trans<-piano.keyboard
  sharp.flat<-paste(rep(ifelse(trans.type==1,"#","b"),no.trans),collapse="")
  trans$pitch<-trans$pitch+trans.type*no.trans
  trans$pitch.name<-paste(trans$pitch.name,sharp.flat,sep="")
  trans$sharp_flat<-sharp.flat
  trans<-trans[trans$pitch>=0&trans$pitch<=87,]
  return(trans)
}

piano.keyboard<-data.frame(pitch=full.pitch,pitch.name=name,alphabet=name,stringsAsFactors=F)
piano.keyboard$sharp_flat<-""
piano.keyboard<-rbind(piano.keyboard
                      ,sharp.flat.trans(piano.keyboard,1,1)
                      ,sharp.flat.trans(piano.keyboard,-1,1)
                      ,sharp.flat.trans(piano.keyboard,1,2)
                      ,sharp.flat.trans(piano.keyboard,-1,2))
piano.keyboard<-piano.keyboard[order(piano.keyboard$pitch,piano.keyboard$sharp_flat),]
saveRDS(piano.keyboard,"piano.keyboard.RDS")
piano.keyboard<-readRDS("piano.keyboard.RDS")

write.table(piano.keyboard,"piano.keyboard.csv",sep=",",row.names=F)

# circle of fifth 
circle.of.fifth.p1<-c()
circle.of.fifth.p2<-c()
root<-"C"
for(i in 1:9){
  scale<-find.major.scale(root)
  circle.of.fifth.p1<-rbind(circle.of.fifth.p1,scale)
  root<-scale[4]
}
circle.of.fifth.p1<-circle.of.fifth.p1[9:2,]

root<-"C"
for(i in 1:13){
  scale<-find.major.scale(root)
  circle.of.fifth.p2<-rbind(circle.of.fifth.p2,scale)
  root<-scale[5]
}

circle.of.fifth<-rbind(circle.of.fifth.p1,circle.of.fifth.p2)
circle.of.fifth<-as.data.frame(circle.of.fifth,stringsAsFactors=F)
names(circle.of.fifth)<-c("root","2nd","3rd","4th","5th","6th","7th")
rownames(circle.of.fifth)<-circle.of.fifth$root
write.table(circle.of.fifth,"circle.of.fifth.csv",sep=",",row.names=F)
saveRDS(circle.of.fifth,"circle.of.fifth.RDS")
circle.of.fifth<-readRDS("circle.of.fifth.RDS")

major.diatonic.chords<-circle.of.fifth[,1:6]
for(i in c(2,3,6)){
  major.diatonic.chords[,i]<-paste(major.diatonic.chords[,i],"m",sep="")
}











