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


major.interval<-c(2,2,1,2,2,2,1)
major.penta.interval<-c(2,2,3,2,3)
major.rel.pitch<-cumu(major.interval)
major.penta.rel.pitch<-cumu(major.penta.interval)
major.solmization<-1:7
major.penta.solmization<-c(1,2,3,5,6)



#defining scale s4 object

scale.profile<-setClass(
  "scale.profile",
  
  # slots
  slots = c(
     name = "character"
    ,interval = "integer"
    ,interval.char = "character"
    ,relative.pitch = "integer"
    ,somization = "character"
    ,family = "character"
    ,no.notes = "integer"
  ),
  validity = function(object){
    if(sum(object@interval)!=12){
      return("The interval does not apply to 12 equal temperaments.")
    }
    return(TRUE)
  }
         )

setGeneric(name = "GenScale",
           def = function(name,family,interval){
             standardGeneric("GenScale")
           })

setMethod(f = "GenScale",
          definition = function(name,family,interval){
            scale<-scale.profile()
            scale@name<-name
            scale@interval<-as.integer(interval)
            scale@interval.char<-intvl.char(interval)
            scale@relative.pitch<-as.integer(cumu(interval))
            scale@family<-family
            scale@no.notes<-length(interval)
            if(scale@no.notes%in%c(5,7)){
              scale@somization<-solmztn.name(interval)
            }
            return(scale)
          })



intvl.char<-function(interval){
  intvl.vect<-c("H","W","WH")
  return(intvl.vect[interval])
}

solmztn.name<-function(interval){
  scale<-cumu(interval)
  if(length(interval)==5){
    comp.pitch<-major.penta.rel.pitch
    solm<-major.penta.solmization
  }else{
    comp.pitch<-major.rel.pitch
    solm<-major.solmization
  }
  diff<-as.integer(scale-comp.pitch)[1:length(interval)]
  shp.flt<-sapply(diff,sharp.flat)
  solm<-paste(solm,shp.flt,sep="")
  return(solm)
}






major.family.mode<-c("Ionian","Dorian","Phrygian","Lydian","Mixolydian","Aolian","Locrian")

mode.df<-data.frame()
mode.seq.no<-1
for(mode in major.family.mode){
  interval<-rep(major.interval,2)[mode.seq.no+0:7]
  interval.char<-paste(interval,collapse=",")
  mode.df<-rbind(c(mode,"Major scale family",interval.char))
  assign(mode,GenScale(mode,"Major scale family",interval))
}








setClass(
  "test",
  slot=c(
    s1="s4"
  )
)






setGeneric(name="setLocation",
           def=function(theObject,position)
           {
             standardGeneric("setLocation")
           }
)

setMethod(f="setLocation",
          signature="Agent",
          definition=function(theObject,position)
          {
            theObject@location <- position
            validObject(theObject)
            return(theObject)
          }
)


Agent <- setClass(
  # Set the name for the class
  "Agent",
  
  # Define the slots
  slots = c(
    location = "numeric",
    velocity   = "numeric",
    active   = "logical"
  ),
  
  # Set the default values for the slots. (optional)
  prototype=list(
    location = c(0.0,0.0),
    active   = TRUE,
    velocity = c(0.0,0.0)
  ),
  
  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity=function(object)
  {
    if(sum(object@velocity^2)>100.0) {
      return("The velocity level is out of bounds.")
    }
    return(TRUE)
  }
)