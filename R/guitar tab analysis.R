# work directory
root.path<-"E:/guitar tab"
target.path<-"E:/tab collect"
setwd(root.path)
# directory structure: root->alphabet->artist-><song folder, song list csv>

alphabet.list<-dir(getwd())

song.info<-data.frame()
all.song.df<-data.frame()
sys.file<-c("not_found.csv","song.list.csv","song.no.record.txt")
for(alph in alphabet.list){
  alphabet.path<-paste(root.path,alph,sep="/")
  artist.list<-dir(alphabet.path)
  for(artist in artist.list){
    artist.path<-paste(alphabet.path,artist,sep="/")
    setwd(artist.path)
    if(length(dir(getwd()))>0){
      song.csv<-read.table("song.list.csv",sep=",",header=T,as.is=T)
      song.csv<-song.csv[toupper(gsub(" ","",song.csv$Format))=="TXT",]
      if(nrow(song.csv)>0){
        song.folders<-dir(artist.path)
        song.folders<-song.folders[!song.folders%in%sys.file]
        for(song in song.folders){
          song.path<-paste(artist.path,song,sep="/")
          song.files<-dir(song.path)
          tab_ind<-tolower(substring(song.files,nchar(song.files)-2))=="txt"
          song.files<-song.files[tab_ind]
          if(length(song.files)>0){
            from<-paste(song.path,song.files,sep="/")
            to<-paste(target.path,song.files,sep="/")
            file.copy(from,to)
            song.df<-data.frame(song.name=song,artist=artist,alphabet=alph,path=from)
            all.song.df<-rbind(all.song.df,song.df)
          }
          song.left<-song.folders[(which(song.folders==song)+1):length(song.folders)]
        }
        song.info<-rbind(song.info,song.csv)
      }
    }
    artist.left<-artist.list[(which(artist.list==artist)+1):length(artist.list)]
  }
  alphabet.left<-alphabet.list[(which(alphabet.list==alph)+1):length(alphabet.list)]
}

#artist.path
#alphabet.list<-alphabet.left
#artist.list<-artist.left[-1]


setwd("E:/tab collect")
tab<-dir(getwd())


find.chord<-function(tab.cont,diat.only=T){
  line.len<-unlist(lapply(tab.cont,nchar))
  space.len<-char.match('\\s+',tab.cont)
  chord.char.len<-char.match('[ABCDEFGMm]',tab.cont)
  score<-(space.len+chord.char.len)/line.len
  chord.line.ind<-which(score>.8&chord.char.len>0)
  chord.seq<-unlist(strsplit(tab.cont[chord.line.ind],split="\\s"))
  chord.seq<-chord.seq[nchar(chord.seq)>0]
  chord.seq<-gsub("[\\(\\)]","",chord.seq)
  not_chord_ind<-grep("[^A-Gm\\#b(maj)(add)(dim)(sus)(aug)(dom)M\\/1-9]",chord.seq)
  if(sum(not_chord_ind)>0)chord.seq<-chord.seq[-not_chord_ind]
  # matching sharp/flat sign, and change sequence
  chord.seq<-gsub("^([\\#b])([A-G])","\\2\\1",chord.seq)
  # some of the tabs use capital M for minor
  if(!all(grepl("m",chord.seq))&any(grep("M",chord.seq))){
    chord.seq<-gsub("M","m",chord.seq)
  }
  if(diat.only){
    #remove slash chord, maj, M and number
    chord.seq<-gsub("\\/.|maj|M|[1-9]","",chord.seq)
    #exclude character after m
    chord.seq<-gsub("m.","m",chord.seq)
  }
  return(chord.seq)
}



char.match<-function(pattern,x){
  match.len<-unlist(lapply(gregexpr(pattern,x),function(a)sum(attr(a,"match.length"))))
  match.len[match.len<0]<-0
  return(match.len)
}




# find key from text
key.pattern<-"(??|1)\\s?(\\=|??)\\s?"
capo.pattern<-"(??????)|(capo)|Ʒ|(fret)"



##############################################
# analysis process
##############################################
find.progression<-function(chord.seq){
  # remove same chords in consecutive position
  same.ind<-which(chord.seq[-length(chord.seq)]==chord.seq[-1])
  if(sum(same.ind)>0)chord.seq<-chord.seq[-same.ind]
  
  # find repete sequence
  prog.df<-c()
  for(len in c(2,3,4,6,8)){
    no.row<-ceiling(length(chord.seq)/len)
    for(pos in 1:len){
      seq.temp<-c(rep(NA,pos-1),chord.seq,rep(NA,16))[1:(no.row*len)]
      mat.temp<-matrix(seq.temp,no.row,len,byrow=T)
      prog.pat<-apply(mat.temp,1,paste,collapse="~~")
      rep_ind<-which(prog.pat[-length(prog.pat)]==prog.pat[-1])
      if(length(rep_ind)>0){
        pat_ind<-prog.pat%in%prog.pat[rep_ind]
        mat.temp<-mat.temp[pat_ind,]
        seq<-apply(mat.temp,1,paste,collapse="~~")
        cnt<-table(seq)
        cnt.df<-data.frame(seq=names(cnt),len=len,freq=as.numeric(cnt),stringsAsFactors = F)
        prog.df<-rbind(prog.df,cnt.df)
      }
    }
  }
  prog.df<-unique(prog.df)
  redundant.pattern<-c(
     paste(prog.df$seq,prog.df$seq,sep="~~")
    ,paste(prog.df$seq,prog.df$seq,prog.df$seq,sep="~~")
  )
  rdd_ind<-prog.df$seq%in%redundant.pattern
  prog.df<-prog.df[!rdd_ind,]
  na_row<-grep("NA",prog.df$seq)
  if(length(na_row)>0)prog.df<-prog.df[-na_row,]
  return(prog.df)
}



# change for NA progression
find.diat.prog<-function(prog.df){
  chord.order<-c("I","ii","iii","IV","V","vi")
  key.df<-c()
  prog.df$possible_key<-""
  prog.df$ML_key<-""
  prog.df$prog<-""
  prog.df$component<-""
  for(i in 1:nrow(prog.df)){
    prog<-prog.df$seq[i]
    prog.vect<-unlist(strsplit(prog,split="~~"))
    df.temp<-find.possible.key.diat(prog.vect)
    if(!is.null(df.temp)){
      prog.df$possible_key[i]<-paste(df.temp$key,collapse=" ")
      key.df<-rbind(key.df,df.temp)
    }
  }
  if(!is.null(key.df)){
    key.df<-aggregate(key.df$wt, by=list(key=key.df$key), FUN=sum)
    row.names(key.df)<-key.df$key
    for(i in 1:nrow(prog.df)){
      if(nchar(prog.df$possible_key[i])>0){
        possible_key<-unlist(strsplit(prog.df$possible_key[i],split=" "))
        key.guess<-key.df[possible_key,]
        ML_key<-key.guess[order(-key.guess$x),1][1]
        prog.df$ML_key[i]<-ML_key
        chord.vect<-unlist(strsplit(prog.df$seq[i],split="~~"))
        names(chord.order)<-major.diatonic.chords[ML_key,]
        prog.df$prog[i]<-paste(chord.order[chord.vect],collapse="-")
        prog.df$component[i]<-paste(sort(unique(chord.order[chord.vect])),collapse=" ")
      }
    }
  }
  return(prog.df)
}



##################################################################
#Error
#Error in data.frame(key = possible_key, wt = 1/length(possible_key), stringsAsFactors = F) : 
##################################################################






find.possible.key.diat<-function(prog.vect){
  possible_key_ind<-apply(major.diatonic.chords,1,function(x)all(prog.vect%in%x))
  if(sum(possible_key_ind)>0){
    possible_key<-major.diatonic.chords$root[possible_key_ind]
    pk.df<-data.frame(key=possible_key,wt=1/length(possible_key),stringsAsFactors=F)
    return(pk.df)
  }else{
    return(NULL)
  }
}

# music ontonlogy









#Stairway To Heaven (Ver 2, View 157).txt
#Knockin On Heavens Door (Ver 2, View 59).txt
tab.cont<-readLines("Knockin On Heavens Door (Ver 2, View 59).txt")
setwd("E:/tab collect")
tabs<-dir(getwd())
song.progs<-c()
for(j in 1:10806){
  t<-tabs[j]
  tab.cont<-readLines(t)
  chord.seq<-find.chord(tab.cont)
  if(length(chord.seq)>0){
    rm("prog.df")
    prog.df<-find.progression(chord.seq)
    if(!is.null(prog.df)){
      prog.df<-find.diat.prog(prog.df)
      prog.df$song.name<-gsub(".txt","",t,fixed = T)
      prog.df<-prog.df[,c(8,1:3,5:7)]
      song.progs<-rbind(song.progs,prog.df)
    }
  }
}
setwd("E:/guitar tab analysis")
saveRDS(song.progs,"song.progs.RDS")
head(sort(table(song.progs$component),decreasing = T)[-1],30)

