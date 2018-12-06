setwd('C:\\Users\\giri-thb\\Desktop\\Girdhar\\dia_emr')
library(stringdist)
library(stringr)
library("tm")
library(RWeka)
library(udpipe)
library(ngram)
library(dplyr)
library(tidyr)
library(tidytext)
library(textmineR)
library(RTextTools)
library(data.table)
library(corpus)
library(wordcloud)
library(stringi)
library(googlesheets)
library(splitstackshape)
prescription_lifespan[,drug_name:=tolower(drug_name)]
lifespan_drug<-prescription_lifespan[,.(drug_name)]
dim(lifespan_drug)

cln_fn <- function(lifespan_drug,drug_name) {
  lifespan_drug[,drug_name1:=drug_name]
  #lifespan_drug[,drug_name:=removeNumbers(drug_name)]
  #lifespan_drug[,drug_name:=removeWords(drug_name,stopwords('english'))]
  lifespan_drug[,drug_name:=tolower(drug_name)]
  #lifespan_drug[,drug_name:=gsub("\\",' ',drug_name,fixed=T)]
  lifespan_drug[,drug_name:=gsub(":",' ',drug_name,fixed=T)]
  lifespan_drug[,drug_name:=gsub("\\(|\\)",' ',drug_name)]
  
  #lifespan_drug[,drug_name:=str_replace_all(drug_name,'[^[:alnum:]]',' ')]
  #lifespan_drug[,drug_name:=str_replace_all(drug_name,'tab\\.?|t |[[|]]|[+]|cap\\.?|mg\\.?|inj\\.?| gm| units| unit|ml\\.?',' ')]
  lifespan_drug[,drug_name:=str_replace_all(drug_name,'tab |inj.|inj|tab,|tab',' ')]
  
  #lifespan_drug[,drug_name:=stripWhitespace(drug_name)]
  lifespan_drug[,drug_name:=trimws(drug_name,which = c('both','left','right'))]
  return(lifespan_drug)
}

lifespan_drug<-cln_fn(lifespan_drug,drug_name)
lifespan_drug<-lifespan_drug[,.N,.(drug_name)][order(-N)]
lifespan_drug<-merge(lifespan_drug,prescription_ascl[,.N,.(drug_name)], by = 'drug_name',all.x = TRUE)
lifespan_drug<-lifespan_drug[is.na(N.y)][order(-N.x)][1:1000]
lifespan_drug<-lifespan_drug[,len_drug:=nchar(drug_name)][order(-len_drug)]
lifespan_drug<-lifespan_drug[len_drug>3]
lifespan_drug[,N.x:=NULL]
lifespan_drug[,N.y:=NULL]
lifespan_drug[,len_drug:=NULL]
write.csv(lifespan_drug,"lifespan_drug.csv")

setwd('C:\\Users\\Analytics1\\Desktop\\Girdhar\\Lifespan_Dia')

emr <- fread('ascl_emr.csv')
#setwd('C:\\Users\\Analytics1\\Desktop\\Girdhar\\dia_emr')

ascl<-emr[,.(emrid,advice,drug_name,plan)]
ascl[,drug:=tolower(paste(advice,drug_name,plan,sep = ' '))]
ascl[,advice:=NULL]
ascl[,drug_name:=NULL]
ascl[,plan:=NULL]


colnames(ascl)[2]<-'drug_name'
ascl[,drug_name1:=tolower(drug_name)]
ascl[,drug_name:=drug_name1]

ascl[,cleared_drug:='0']
ascl[,dosage:='0']
ascl[,freq:='0']
ascl<-cln_fn(ascl,drug_name)
ascl<-ascl[drug_name!='']
dim(ascl[cleared_drug==0])
#lifespan_drug<-'lantus'
#lifespan_drug<-as.data.table(lifespan_drug)
#colnames(lifespan_drug)<-'drug_name'
#lifespan_drug[,len_drug:=nchar(drug_name)]
lifespan_drug<-lifespan_drug[len_drug>4]

ae <- gs_title('drug_master')
m_drug <- ae %>% gs_read(ws='Sheet1',range = cell_cols(1:2))
m_drug<-as.data.table(m_drug)
m_drug[,drug_name:=clean_brand]
m_drug[,purpose:=NULL]
m_drug[,clean_brand:=NULL]

ascl_drug_old<-as.data.table(prescription_ascl[,.N,.(drug_name)][,drug_name])
colnames(ascl_drug_old)<-'drug_name'
ascl_drug<-rbind(m_drug,lifespan_drug,ascl_drug_old)
ascl_drug<-unique(ascl_drug)
ascl_drug<-ascl_drug[nchar(drug_name)>3]
gs_edit_cells(ss = ae, ws='ascl_drugs', input = ascl_drug)
map_drug <- ae %>% gs_read(ws='ascl_drugs',range = cell_cols(1:2))
map_drug<-as.data.table(map_drug)
map_drug<-map_drug[,len_drug:=nchar(drug_name)][order(-len_drug)]
map_drug<-map_drug[!duplicated(map_drug)]

#map_drug <- ae %>% gs_read(ws='ascl_drugs',range = cell_cols(1:2))
tab_units<-c('mgtablets', 'MG Tablets','mgtab',  'MG TAB','mgcaps',  'MG CAPS','mgcap', 'MG CAP','mg',  'MG','gmcream',  'GM Cream','gmgel',  'GM Gel',   'gmtab',  'GM TAB','gm',  'GM',   'mldrops', 'ML Drops','mlinj',  'ML INJ', 'mloralsuspension', 'ML Oral Suspension','mlsyp',  'ML SYP', 'ml',  'ML','iuinj',  'IU INJ', 'iucartridges','IU Cartridges','iu', 'IU','mcgtab',  'MCG TAB','mcgcaps',  'MCG Caps','mcgcap',  'MCG CAP','mcg',  'MCG','grmcream', 'GRM Cream','grmgel',  'GRM Gel','grmtab',  'GRM TAB','grm',  'GRM','kcaps','K Caps','kcap',  'K cap','ksofgels',  'K Sofgels', 'ksofgel',  'K Sofgel','units', 'UNITS', 'unit', 'UNIT','gram', 'GRAM')
#gs_edit_cells(ss = ae, ws='ascl_drugs', input = ascl_drug)
tab_units<-tolower(tab_units)
tab_units<-unique(tab_units)
tab_units<-as.data.table(tab_units)
tab_units[,levels:=str_count(tab_units,pattern = ' ')]
tab_units[,levels:=1+levels]

freq<-c('onceaday' , 'once a day','twiceaday' , 'twice a day','thriceaday' , 'thrice a day','qds' , 'qds','qd' , 'qd','bid' , 'bid','tid' , 'tid','sos' , 'sos','daily' , 'daily')
freq<-as.data.table(unique(freq))



cleaned_bm_function<-function(ascl,tab_units, map_drug){
  t<-paste('[0-9]',unlist(tab_units[,tab_units]),sep = '',collapse = '|')
  ascl_u<-ascl
  ascl<-unique(ascl[,.(drug_name,cleared_drug,dosage,freq)])
  ascl<-as.data.table(ascl)
  ascl[,drugs:=drug_name]
  ascl_u[,cleared_drug:=NULL][,dosage:=NULL][,freq:=NULL]
  
  for (i in 1:length(ascl[,drug_name])) {
    w<-words(ascl[,drug_name][i])
    w<-as.data.table(w)
    w<-sapply(w, as.character)
    l<-length(w)
    
    for(k in (3:l)){
      tryCatch({
        if(grepl('.-.-.',w[k]) & !is.na(as.numeric(w[k-2])) ){
          j<-k
          if(nchar(w[j-3])>3){
            ascl[i,cleared_drug:=paste(cleared_drug,as.character(w[j-3]),sep = '~')]
            ascl[i,drug_name:=gsub(w[j-3],'',drug_name)]
            ascl[i,dosage:=paste(dosage,paste(w[j-2],w[j-1],sep = ' '),sep='~')]
            ascl[i,drug_name:=gsub(paste(w[j-2],w[j-1],sep = ' '),'',drug_name)]
            ascl[i,freq:=paste(freq,paste(w[j],sep = ' '),sep='~')]
            ascl[i,drug_name:=gsub(paste(w[j],sep = ' '),'',drug_name)]
          }
          else{
            ascl[i,cleared_drug:=paste(cleared_drug,paste(w[j-4],w[j-3], sep = ' '),sep = '~')]
            ascl[i,drug_name:=gsub(paste(w[j-4],w[j-3],sep = ' '),'',drug_name)]
            ascl[i,dosage:=paste(dosage,paste(w[j-2],w[j-1],sep = ' '),sep='~')]
            ascl[i,drug_name:=gsub(paste(w[j-2],w[j-1],sep = ' '),'',drug_name)]
            ascl[i,freq:=paste(freq,paste(w[j],sep = ' '),sep='~')]
            ascl[i,drug_name:=gsub(paste(w[j],sep = ' '),'',drug_name)]
          }
        }
        
        else if(grepl('.-.-.',w[k]) & !is.na(as.numeric(w[k-1])) ){
          j<-k
          
          if(nchar(w[j-2])>3){
            ascl[i,cleared_drug:=paste(cleared_drug,paste(w[j-2],w[j-1], sep = ' '),sep = '~')]
            ascl[i,drug_name:=gsub(paste(w[j-2],w[j-1], sep = ' '),'',drug_name)]
            ascl[i,dosage:=paste(dosage,'0',sep='~')]
            #ascl[i,drug_name:=gsub(paste(w[j-2],w[j-1],sep = ' '),'',drug_name)]
            ascl[i,freq:=paste(freq,paste(w[j],sep = ' '),sep='~')]
            ascl[i,drug_name:=gsub(paste(w[j],sep = ' '),'',drug_name)]
          }
          else{
            ascl[i,cleared_drug:=paste(cleared_drug,paste(w[j-3],w[j-2],w[j-1], sep = ' '),sep = '~')]
            ascl[i,drug_name:=gsub(paste(w[j-3],w[j-2],w[j-1],sep = ' '),'',drug_name)]
            ascl[i,dosage:=paste(dosage,'0',sep='~')]
            #ascl[i,drug_name:=gsub(paste(w[j-2],w[j-1],sep = ' '),'',drug_name)]
            ascl[i,freq:=paste(freq,paste(w[j],sep = ' '),sep='~')]
            ascl[i,drug_name:=gsub(paste(w[j],sep = ' '),'',drug_name)]
          }
        }
        
        else if(grepl('.-.-.',w[k]) & grepl('./.',w[k-1])  ){
          j<-k
          if(nchar(w[j-2])>3){
            ascl[i,cleared_drug:=paste(cleared_drug,paste(w[j-2],w[j-1],sep = ' '),sep = '~')]
            ascl[i,drug_name:=gsub(paste(w[j-2],w[j-1],sep = ' '),'',drug_name)]
            ascl[i,dosage:=paste(dosage,'0',sep='~')]
            #ascl[i,drug_name:=gsub(paste(w[j-2],w[j-1],sep = ' '),'',drug_name)]
            ascl[i,freq:=paste(freq,w[j],sep='~')]
            ascl[i,drug_name:=gsub(w[j],'',drug_name)]
          }
          else{
            ascl[i,cleared_drug:=paste(cleared_drug,paste(w[j-3],w[j-2],w[j-1], sep = ' '),sep = '~')]
            ascl[i,drug_name:=gsub(paste(w[j-3],w[j-2],w[j-1],sep = ' '),'',drug_name)]
            ascl[i,dosage:=paste(dosage,'0',sep='~')]
            #ascl[i,drug_name:=gsub(paste(w[j-2],w[j-1],sep = ' '),'',drug_name)]
            ascl[i,freq:=paste(freq,w[j],sep='~')]
            ascl[i,drug_name:=gsub(w[j],'',drug_name)]
          }
        }
        
        else if(grepl('.-.-.',w[k]) & grepl(t,w[k-1])  ){
          j<-k+1
          if(nchar(w[j-3])>3){
            ascl[i,cleared_drug:=paste(cleared_drug,as.character(w[j-3]),sep = '~')]
            ascl[i,drug_name:=gsub(w[j-3],'',drug_name)]
            ascl[i,dosage:=paste(dosage,w[j-2],sep='~')]
            ascl[i,drug_name:=gsub(w[j-2],'',drug_name)]
            ascl[i,freq:=paste(freq,w[j-1],sep='~')]
            ascl[i,drug_name:=gsub(w[j-1],'',drug_name)]
          }
          else{
            ascl[i,cleared_drug:=paste(cleared_drug,paste(w[j-4],w[j-3], sep = ' '),sep = '~')]
            ascl[i,drug_name:=gsub(paste(w[j-4],w[j-3],sep = ' '),'',drug_name)]
            ascl[i,dosage:=paste(dosage,w[j-2],sep='~')]
            ascl[i,drug_name:=gsub(w[j-2],'',drug_name)]
            ascl[i,freq:=paste(freq,w[j-1],sep='~')]
            ascl[i,drug_name:=gsub(w[j-1],'',drug_name)]
          }
        }  
        
        else if(grepl('.-.-.',w[k]) & is.na(as.numeric(as.character(w[k-1]))  )  ){
          j<-k+1
          if(nchar(w[j-2])>3){
            ascl[i,cleared_drug:=paste(cleared_drug,as.character(w[j-2]),sep = '~')]
            ascl[i,drug_name:=gsub(w[j-2],'',drug_name)]
            ascl[i,dosage:=paste(dosage,'0',sep='~')]
            #ascl[i,drug_name:=gsub(paste(w[j-2],w[j-1],sep = ' '),'',drug_name)]
            ascl[i,freq:=paste(freq,w[j-1],sep='~')]
            ascl[i,drug_name:=gsub(w[j-1],'',drug_name)]
          }
          else{
            ascl[i,cleared_drug:=paste(cleared_drug,paste(w[j-3],w[j-2], sep = ' '),sep = '~')]
            ascl[i,drug_name:=gsub(paste(w[j-3],w[j-2],sep = ' '),'',drug_name)]
            ascl[i,dosage:=paste(dosage,'0',sep='~')]
            #ascl[i,drug_name:=gsub(paste(w[j-2],w[j-1],sep = ' '),'',drug_name)]
            ascl[i,freq:=paste(freq,w[j-1],sep='~')]
            ascl[i,drug_name:=gsub(w[j-1],'',drug_name)]
          }
        }
        
        else if(w[k] %in% unlist(tab_units[,(tab_units)]) & !is.na(as.numeric(w[k-1])) ){
   
          j<-k
          if(nchar(w[j-2])>3){
            ascl[i,cleared_drug:=paste(cleared_drug,as.character(w[j-2]),sep = '~')]
            ascl[i,drug_name:=gsub(w[j-2],'',drug_name)]
            ascl[i,dosage:=paste(dosage,paste(w[j-1],w[j],sep = ' '),sep='~')]
            ascl[i,drug_name:=gsub(paste(w[j-1],w[j],sep = ' '),'',drug_name)]
            ascl[i,freq:=paste(freq,'0',sep='~')]
            # ascl[i,drug_name:=gsub(paste(w[j],sep = ' '),'',drug_name)]
            
          }
          else{
            ascl[i,cleared_drug:=paste(cleared_drug,paste(w[j-3],w[j-2], sep = ' '),sep = '~')]
            ascl[i,drug_name:=gsub(paste(w[j-3],w[j-2],sep = ' '),'',drug_name)]
            ascl[i,dosage:=paste(dosage,paste(w[j-1],w[j],sep = ' '),sep='~')]
            ascl[i,drug_name:=gsub(paste(w[j-1],w[j],sep = ' '),'',drug_name)]
            ascl[i,freq:=paste(freq,'0',sep='~')]
            #ascl[i,drug_name:=gsub(paste(w[j],sep = ' '),'',drug_name)]
          }
        }
        
        else if(grepl(t,w[k])){
    
          j<-k+1
          if(nchar(w[j-2])>3){
            ascl[i,cleared_drug:=paste(cleared_drug,as.character(w[j-2]),sep = '~')]
            ascl[i,drug_name:=gsub(w[j-2],'',drug_name)]
            ascl[i,dosage:=paste(dosage,w[j-1],sep='~')]
            ascl[i,drug_name:=gsub(w[j-1],'',drug_name)]
            ascl[i,freq:=paste(freq,'0',sep='~')]
            # ascl[i,drug_name:=gsub(paste(w[j],sep = ' '),'',drug_name)]
            
          }
          else{
            ascl[i,cleared_drug:=paste(cleared_drug,paste(w[j-3],w[j-2], sep = ' '),sep = '~')]
            ascl[i,drug_name:=gsub(paste(w[j-3],w[j-2],sep = ' '),'',drug_name)]
            ascl[i,dosage:=paste(dosage,w[j-1],sep='~')]
            ascl[i,drug_name:=gsub(w[j-1],'',drug_name)]
            ascl[i,freq:=paste(freq,'0',sep='~')]
            #ascl[i,drug_name:=gsub(paste(w[j],sep = ' '),'',drug_name)]
          }
        }
        
        
        
      }, error=function(e){}) 
    }
    print(i) 
  }
  colnames(ascl)[1]<-'left_drugs'
  ascl<-merge(ascl_u,ascl,by.x = 'drug_name', by.y = 'drugs')
  da<-map_drug
  da<-da[len_drug > 3,.(drug_name)]
  da<-unique(da)
  
  ########## exact match ############################
  if (length(da$drug_name) > 0) {
   for (i in 1:length(da$drug_name)) {
    ascl[grepl(da$drug_name[i],left_drugs),cleared_drug:=paste(cleared_drug,as.character(da$drug_name[i]),sep = '~')][grepl(da$drug_name[i],left_drugs),dosage:=paste(dosage,'0',sep = '~')][grepl(da$drug_name[i],left_drugs),freq:=paste(freq,'0',sep = '~')]
   ascl[grepl(da$drug_name[i],left_drugs),left_drugs:=gsub(da$drug_name[i],'',left_drugs)]
   
  print(i) 
  }}
  return(as.data.table(ascl))
}
cleaned_bm_function2<-function(ascl){
  for (i in 1:NROW(ascl)) {
    
    
    ascl[i,cleared_drug:=paste(strsplit(drug_name,"\n")[[1]], collapse = '~' )]
    
  }
  return(ascl)
}

ascl<-cleaned_bm_function2(ascl)


ascl2<-ascl
ascl_long<-cSplit(ascl2,'cleared_drug',sep = '~', direction = 'long')
ascl_long[,cleared_drug:=stripWhitespace(as.character(cleared_drug))]
ascl_long[,drug_name:=as.character(cleared_drug)]
ascl_long[,cleared_drug:='0']

ascl_long<-cleaned_bm_function(ascl,tab_units,map_drug)

ascl2<-ascl_long[,no_drugs:=str_count(cleared_drug,pattern = "~")]
ascl2<-ascl2[,no_dosages:=str_count(dosage,pattern = "~")]
ascl2<-ascl2[,no_freq:=str_count(freq,pattern = "~")]
ascl2<-ascl2[no_drugs==no_dosages]
ascl2<-ascl2[no_drugs==no_freq]
ascl2[,.N,.(emrid,cleared_drug,dosage,freq)]

ascl2<-ascl2[!duplicated(ascl2)]

ascl22<-merge(ascl_old,ascl_long, by=c('emrid','cleared_drug') , all=T)


ascl_long<-cSplit(ascl2,'cleared_drug',sep = '~', direction = 'long')
ascl_do<-cSplit(ascl2,'dosage',sep = '~', direction = 'long')
ascl_fr<-cSplit(ascl2,'freq',sep = '~', direction = 'long')
ascl_long[,dosage:=NULL]
ascl_long[,freq:=NULL]

ascl_long<-cbind(ascl_long,ascl_do[,dosage],ascl_fr[,freq])
ascl_long[,no_drugs:=NULL]
ascl_long[,no_dosages:=NULL]
ascl_long[,no_freq:=NULL]
ascl_long[,dosage:=V2]
ascl_long[,freq:=V3]
ascl_long[,V2:=NULL]
ascl_long[,V3:=NULL]
ascl_long<-ascl_long[!duplicated(ascl_long)]
#ascl_long[,cleared_drug:=stripWhitespace(as.character(cleared_drug))]
ascl_long[,.N,.(cleared_drug)][order(-N)]









write.csv(ascl2,"ascl_mapped_by_pattern_all.csv")
write.csv(ascl_long,"ascl_mapped_by_pattern_all_long2.csv")

ascl2

ascl[grepl(',',cleared_drug)][1:20]
unique(ascl[cleared_drug==0,.(drug_name)])


####### mapped_before ############
ascl_before<-prescription_all[substr(pph_id,1,4)=='ascl']
ascl_before <- merge(ascl_before,drug_name_map,by='clean_brand',all.x=TRUE)
dim(ascl_before[is.na(replaced_name)])

wordcloud(Corpus(VectorSource(ascl$drug_name)))
###########################################################
not_found<-ascl[cleared_drug==0,.(drug_name)]
not_found<-not_found[,.N,.(drug_name)][order(-N)]


######## dosages and time ################
#ascl<-as.data.table(read.csv('ascl_mapped_by_lifespan.csv'))
ascl2<-ascl
ascl<-ascl[cleared_drug!=0]
ascl[,dosages:='0']
ascl[,dosages:=as.character(dosages)]
ascl[,time:='0']
ascl[,time:=as.character(time)]

ascl[,drug_name1:=tolower(drug_name1)]

for(i in 1:nrow(ascl)){
  for(j in 2:(1+ascl$no_drugs[i])){
    dg<-str_split(ascl$cleared_drug[i], ',')[[1]][j]
    b<-words(strsplit(ascl$drug_name1[i],split = dg)[[1]][2])[1:5]
    #if(sum(b[2:3] %in% c('@+','at','ta','s/c'))>=1){
    # c<-paste(b[3:5],collapse = ' ')
    #  b<-paste(b[1:2],collapse = ' ')
    #}
    #  else{
    #   b<-paste(b[1:2], collapse = ' ')
    #  c<-'0'
    #}
    b<-paste(b,collapse = ' ')
    ascl[i,dosages:=paste(dosages,b,sep = '~')]
    #ascl[i,time:=paste(time,c,sep = ',')]
    
    
  }
}

write.csv(ascl,'mapped_ascl_dosages_by_lifespan.csv')


######################## Lantus ################################
t<-term_stats(Corpus(VectorSource(ascl[,(drug_name)])) , ngrams = 1 )
t<-as.data.table(t)
t[1:20]
t[,len:=nchar(term)]
t<-t[len>4]
t[,no_words:=1]

t2<-term_stats(Corpus(VectorSource(ascl[,(drug_name)])) , ngrams = 2 )
t2<-as.data.table(t2)
t2[1:20]
t2[,len:=nchar(term)]
t2<-t2[len>4]
t2[,no_words:=2]

t3<-term_stats(Corpus(VectorSource(ascl[,(drug_name)])) , ngrams = 3 )
t3<-as.data.table(t3)
t3[1:20]
t3[,len:=nchar(term)]
t3<-t3[len>4]
t3[,no_words:=3]

tall<-rbind(t,t2,t3)
write.csv(tall,'text_ngrams.csv')
getwd()






























