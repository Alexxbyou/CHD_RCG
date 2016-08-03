major.interval<-c(2,2,1,2,2,2,1)
# find pitch based on the interval in one octave
cumu<-function(x){
  c(0,sapply(1:length(x),function(a)sum(x[1:a])))
}

# solmization naming base on the major scale, input scale must be the pitch of a seven notes scale
solmization.naming<-function(int.vect,rep.name=F){
  no.rep<-ceiling(length(int.vect)/7)+1
  major.comp<-cumu(rep(major.interval,no.rep))[1:length(int.vect)]
  if(rep.name){
    names(major.comp)<-rep(1:7,no.rep)[1:length(int.vect)]
  }else{
    names(major.comp)<-1:length(int.vect) 
  }
  diff<-as.integer(int.vect-major.comp)
  shp.flt<-sapply(diff,sharp.flat)
  name<-paste(names(major.comp),shp.flt,sep="")
  return(name)
}

# sharp flat symbol
sharp.flat<-function(x){
  x<-as.integer(x)
  symbol<-ifelse(x>0,"#","b")
  return(paste(rep(symbol,abs(x)),collapse=""))
}

# given the parent scale, find mode pitch and the solmization name
mode.scale<-function(parent.intv,start=1,no.octave=1,rep.name=F,back.to.root=F){
  intv<-rep(parent.intv,2)[start+0:6]
  scale.pitch<-cumu(rep(intv,no.octave))
  names(scale.pitch)<-solmization.naming(scale.pitch,rep.name=rep.name)
  if(!back.to.root)scale.pitch<-scale.pitch[-length(scale.pitch)]
  return(scale.pitch)
}

find.arpeggio<-function(family,arp.type){
  interval<-switch(family,
         major=major.scale.2octave,
         minor=minor.scale.2octave,
         dominant=mixolydian.scale.2octave)
  no.note<-switch(arp.type,
                  triad=3,
                  seventh=4,
                  nineth=5,
                  eleventh=6,
                  thirteenth=7)
  return(interval[1:no.note*2-1])
}

# find major scale based on 
find.major.scale<-function(root){
  substring(root,1,1)<-toupper(substring(root,1,1))
  root.pitch<-min(piano.keyboard$pitch[piano.keyboard$pitch.name==root])
  scale.pitch<-root.pitch+mode.scale(major.interval,start=1,no.octave=1,rep.name=F,back.to.root=F)
  pitch.df<-piano.keyboard[piano.keyboard$pitch%in%scale.pitch,]
  scale.pitch.name<-root
  alphabet<-substring(root,1,1)
  start.alphabet<-which(LETTERS[1:7]==alphabet)
  alpha.vect<-rep(LETTERS[1:7],2)[start.alphabet+0:6]
  for(nt in 2:7){
    df<-pitch.df[pitch.df$pitch==scale.pitch[nt],]
    new.note.name<-df$pitch.name[df$alphabet==alpha.vect[nt]]
    scale.pitch.name<-c(scale.pitch.name,new.note.name)
  }
  return(scale.pitch.name)
}






# scale for arpeggio
major.scale.2octave<-mode.scale(major.interval,start=1,no.octave=2,rep.name=F,back.to.root=F)
minor.scale.2octave<-mode.scale(major.interval,start=6,no.octave=2,rep.name=F,back.to.root=F)
mixolydian.scale.2octave<-mode.scale(major.interval,start=5,no.octave=2,rep.name=F,back.to.root=F)


# arpeggio
find.arpeggio("dominant","nineth")











