rm(list=ls())

Sys.getenv("JAVA_HOME")

library(dplyr)
library(data.table)
library(stringr)
library(lubridate)
library(splitstackshape)
library(RPostgreSQL)
library(googlesheets)
library(gdata)
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_221\\')
library(qdap)
library(zoo)
library(stringdist)
`%ni%`<-Negate(`%in%`)
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
library(gtools)

##### prep for dosages #########
tab_units<-c('mgtablets', 'MG Tablets','mgtab',  'MG TAB','mgcaps',  'MG CAPS','mgcap', 'MG CAP','mg',  'MG','gmcream',  'GM Cream','gmgel',  'GM Gel',   'gmtab',  'GM TAB','gm',  'GM',   'mldrops', 'ML Drops','mlinj',  'ML INJ', 'mloralsuspension', 'ML Oral Suspension','mlsyp',  'ML SYP', 'ml',  'ML','iuinj',  'IU INJ', 'iucartridges','IU Cartridges','iu', 'IU','mcgtab',  'MCG TAB','mcgcaps',  'MCG Caps','mcgcap',  'MCG CAP','mcg',  'MCG','grmcream', 'GRM Cream','grmgel',  'GRM Gel','grmtab',  'GRM TAB','grm',  'GRM','kcaps','K Caps','kcap',  'K cap','ksofgels',  'K Sofgels', 'ksofgel',  'K Sofgel','units', 'UNITS', 'unit', 'UNIT','gram', 'GRAM')

tab_units<-tolower(tab_units)
tab_units<-unique(tab_units)
tab_units<-as.data.table(tab_units)
tab_units[,levels:=str_count(tab_units,pattern = ' ')]
tab_units[,levels:=1+levels]
t0<-tab_units[levels>1]
t0[,tab_unit:=str_replace_all(tab_units,' ','')]
t0[,rplc:=paste(' ',tab_units,sep = '')]
t2<-tab_units[levels==1]

tab2<-t2
tab2[,tab_unit:=tab_units]
tab2[,rplc:=paste(' ',tab_units,sep = '')]
tab2<-rbind(tab2,t0)

tab_units<-unique(tab2)
tab_units<-tab_units[,.(tab_unit)]
tab_units<-unique(tab_units)
#add_tab<-tab_units[1]
#add_tab[,tab_unit:='[0-9]/[0-9]']
#tab_units<-rbind(add_tab,tab_units)
tab_units[,tab_unit:=paste('*',tab_unit,sep = '')]

#lifespan_drug<-prescription_all[,clean_brand:=tolower(clean_brand)][str_sub(pph_id,1,4)=='life',.N,.(clean_brand)][N>60]
#lifespan_drug[,from:='lifespan']
cln_fn <- function(lifespan_drug,drug_name_mod) {
  #lifespan_drug[,drug_name_mod1:=drug_name_mod]
  #lifespan_drug[,drug_name_mod:=removeNumbers(drug_name_mod)]
  #lifespan_drug[,drug_name_mod:=removeWords(drug_name_mod,stopwords('english'))]
  lifespan_drug[,drug_name_mod:=tolower(drug_name_mod)]
  #lifespan_drug[,drug_name_mod:=gsub("\\",' ',drug_name_mod,fixed=T)]
  lifespan_drug[,drug_name_mod:=gsub(":",' ',drug_name_mod,fixed=T)]
  lifespan_drug[,drug_name_mod:=gsub("\\(|\\)",' ',drug_name_mod)]
  lifespan_drug[,drug_name_mod:=gsub("//",' ',drug_name_mod)]
  lifespan_drug[,drug_name_mod:=gsub("\\\\",' ',drug_name_mod)]
  lifespan_drug[,drug_name_mod:=gsub("\\=",' ',drug_name_mod)]
  lifespan_drug[,drug_name_mod:=gsub("i.u.",'iu',drug_name_mod,fixed = T)]
  lifespan_drug[,drug_name_mod:=gsub("i.u",'iu',drug_name_mod,fixed = T)]  
  lifespan_drug[,drug_name_mod:=str_replace_all(drug_name_mod,',',' ')]
  
  lifespan_drug[,drug_name_mod:=str_squish(drug_name_mod)]
  lifespan_drug[,drug_name_mod:=str_replace_all(drug_name_mod,'gp3 / ','gp3/')]
  lifespan_drug[,drug_name_mod:=str_replace_all(drug_name_mod,'mg /','mg/')]
  lifespan_drug[,drug_name_mod:=gsub(" -",'-',drug_name_mod)]
  lifespan_drug[,drug_name_mod:=str_replace_all(drug_name_mod,' 1 ',' ')]
  lifespan_drug[,drug_name_mod:=str_replace_all(drug_name_mod,' / ',' ')]
  lifespan_drug[,drug_name_mod:= trimws(str_replace_all((drug_name_mod),'tab\\.|cap\\.|inj\\.|^tab |^cap |^inj |^tablet|^injection|^tab-|^cap-|^inj-',''))]
  lifespan_drug[,drug_name_mod:=gsub("[[:alnum:].-_]+@[[:alnum:].-]+",' ',drug_name_mod)]
  
  lifespan_drug[,drug_name_mod:=stri_replace_all_fixed(drug_name_mod, tab2[,rplc], tab2[,tab_unit], vectorize_all=FALSE)]
  lifespan_drug[,drug_name_mod:=stri_replace_all_fixed(drug_name_mod, tab2[,tab_units], tab2[,tab_unit], vectorize_all=FALSE)]
  lifespan_drug[,drug_name_mod:=str_replace_all(drug_name_mod,' inj\\.| inj| tablets| tablet| tab ',' ')]
  
  #lifespan_drug[,drug_name_mod:=str_replace_all(drug_name_mod,'[^[:alnum:]]',' ')]
  #  lifespan_drug[,drug_name_mod:=str_replace_all(drug_name_mod,'tab\\.?|t |[[|]]|[+]|cap\\.?|mg\\.?|inj\\.?| gm| units| unit|ml\\.?',' ')]
  
  lifespan_drug[,drug_name_mod:=str_squish(drug_name_mod)]
  return(lifespan_drug)
}

mapping<-function(dt){
  t<-paste(unlist(tab_units[,tab_unit]),collapse = '|')
  dt<-unique(dt)
  dt[is.na(dosage),dosage:='0']
  dt0<-dt[dosage=='0']
  dt1<-dt[!dosage=='0']
  dt0<-cln_fn(dt0,drug_name_mod)
  
  uni_dt0<-dt0[,.N,.(drug_name_mod,dosage)]
  
  for (i in 1:nrow(uni_dt0)){
    print(i)
    tryCatch({
      w<-words(uni_dt0[,drug_name_mod][i])
      l<- length(w)
      for ( j in l:1){
        if(grepl(t,w[j])){
          uni_dt0[i,dosage:=w[j]]
          j<-0
        }
      }
    }, error=function(e){}) 
    
  }
  
  dt0[,dosage:=NULL]
  uni_dt0[,N:=NULL]
  uni_dt0<-unique(uni_dt0)
  uni_dt0[,drug_name_mod:=as.character(drug_name_mod)]
  dt0[,drug_name_mod:=as.character(drug_name_mod)]
  dt0<-merge(dt0,uni_dt0, by='drug_name_mod')
  
  dt<-rbind(dt0,dt1)
  return(as.data.table(dt))
  
}

setwd('E:\\girdhar\\Lifespan_Dia\\latest')




#gs_auth(cache = FALSE)
#4/MADhPNuBnHo3Dow_MqcQ02mpDDgzhyLTQEXAmBi3ndg-8LFzjQg8-oU
#token <- gs_auth()
#gd_token()
#saveRDS(token, file = "googlesheets_token.rds")

gs_auth(token = "googlesheets_token.rds")

medical_history_map <- gs_title('Copy of drug_master')
medical_history_map <- medical_history_map %>% gs_read(ws='emr_medical_history_map')

complaints_map_sheet <- gs_title('Copy of drug_master')
complaints_map_sheet <- complaints_map_sheet %>% gs_read(ws='complaints_grep',range = cell_cols(1:2))

diagnosis_map_sheet <- gs_title('Copy of drug_master')
diagnosis_map_sheet <- diagnosis_map_sheet %>% gs_read(ws='diagnosis_grep',range = cell_cols(1:2))

Sys.sleep(10)

drug_name_map <- gs_title('Copy of drug_master')
drug_name_map <- drug_name_map %>% gs_read(ws='drug_name_map',range = cell_cols(1:2))

doc_map <- gs_title('Copy of drug_master')
doc_map <- doc_map %>% gs_read(ws='doc_map',range = cell_cols(c(1,8)))
doc_map<-sapply(doc_map, as.character)
doc_map<-as.data.table(doc_map)
doc_map[,c('city','N','Qualification'):=NULL]
doc_map<-unique(doc_map)
doc_map[,`Diabetes Mapping_Abbott_11june19`:=str_squish(tolower(`Diabetes Mapping_Abbott_11june19`))]

molecules <- gs_title('Copy of drug_master')
molecules <- molecules %>% gs_read(ws='Drug_master', range = cell_cols(1:7))

insulin_acting <- gs_title('Copy of drug_master')
insulin_acting <- insulin_acting %>% gs_read(ws='insulin', range = cell_cols(1:3))
insulin_acting<-as.data.table(insulin_acting)

mol_class <- gs_title('Copy of drug_master')
mol_class <- mol_class %>% gs_read(ws='Sheet2',range = cell_cols(1:2))

code_map <- gs_title('Copy of drug_master')
code_map <- code_map %>% gs_read(ws='emr_investigation_map')

Sys.sleep(10)

state_region_map <- gs_title('Copy of drug_master')
state_region_map <- state_region_map %>% gs_read(ws='emr_state_region_map')

clinic_map <- gs_title('Copy of drug_master')
clinic_map <- clinic_map %>% gs_read(ws='lifespan_lab_centre',range = cell_cols(2:4))

ascl_location_map <- gs_title('Copy of drug_master')
ascl_location_map <- ascl_location_map %>% gs_read(ws='ascl_location_map',range = cell_cols(1:3))


patient_master <- fread('lifespan_patient.csv')
clinic_master <- fread('lifespan_clinic_master.csv')[,.(clinic_id=CLINIC_ID,clinic_name=CLINIC_NAME)]
ppv <- fread('lifespan_prescription_patient_view.csv')
medical_history <- fread('lifespan_medical_history.csv')
investigation_details <- fread('lifespan_investigation_details.csv')

drug_purpose <- gs_title('Copy of drug_master')
drug_purpose <- drug_purpose %>% gs_read(ws='Diagnosis_purpose',range = cell_cols(1:2))
map_drug<-as.data.table(molecules)[,.(clean_brand)]
map_drug[,length:=nchar(clean_brand)]
map_drug<-map_drug[!duplicated(map_drug)]
map_drug<-map_drug[order(-length)]
map_drug<-map_drug[length>3]

emr <- fread('ascl_emr.csv')
lab <- fread('ascl_lab.csv')
demo <- fread('ascl_demo.csv')
# medicines_all <- fread('ascl_medicines.csv')

{cln_fn_ascl <- function(lifespan_drug,drug_name) {
  #lifespan_drug[,drug_name1:=drug_name]
  #lifespan_drug[,drug_name:=removeNumbers(drug_name)]
  #lifespan_drug[,drug_name:=removeWords(drug_name,stopwords('english'))]
  lifespan_drug[,drug_name:=tolower(drug_name)]
  #lifespan_drug[,drug_name:=gsub("\\",' ',drug_name,fixed=T)]
  lifespan_drug[,drug_name:=gsub(":",' ',drug_name,fixed=T)]
  lifespan_drug[,drug_name:=gsub("\\(|\\)",' ',drug_name)]
  lifespan_drug[,drug_name:=gsub("//",' ',drug_name)]
  lifespan_drug[,drug_name:=gsub("\\\\",' ',drug_name)]
  lifespan_drug[,drug_name:=gsub("\\=",' ',drug_name)]
  lifespan_drug[,drug_name:=gsub("i.u.",'iu',drug_name,fixed = T)]
  lifespan_drug[,drug_name:=gsub("i.u",'iu',drug_name,fixed = T)]  
  lifespan_drug[,drug_name:=str_replace_all(drug_name,',',' ')]
  
  lifespan_drug[,drug_name:=str_squish(drug_name)]
  lifespan_drug[,drug_name:=str_replace_all(drug_name,'gp3 / ','gp3/')]
  lifespan_drug[,drug_name:=str_replace_all(drug_name,'mg /','mg/')]
  lifespan_drug[,drug_name:=gsub(" -",'-',drug_name)]
  lifespan_drug[,drug_name:=str_replace_all(drug_name,' 1 ',' ')]
  lifespan_drug[,drug_name:=str_replace_all(drug_name,' / ',' ')]
  lifespan_drug[,drug_name:= trimws(str_replace_all((drug_name),'tab\\.|cap\\.|inj\\.|^tab |^cap |^inj |^tablet|^injection|^tab-|^cap-|^inj-',''))]
  lifespan_drug[,drug_name:=gsub("[[:alnum:].-_]+@[[:alnum:].-]+",' ',drug_name)]
  
  lifespan_drug[,drug_name:=stri_replace_all_fixed(drug_name, tab2[,rplc], tab2[,tab_unit], vectorize_all=FALSE)]
  lifespan_drug[,drug_name:=stri_replace_all_fixed(drug_name, tab2[,tab_units], tab2[,tab_unit], vectorize_all=FALSE)]
  lifespan_drug[,drug_name:=str_replace_all(drug_name,' inj\\.| inj| tablets| tablet| tab ',' ')]
  
  #lifespan_drug[,drug_name:=str_replace_all(drug_name,'[^[:alnum:]]',' ')]
  #  lifespan_drug[,drug_name:=str_replace_all(drug_name,'tab\\.?|t |[[|]]|[+]|cap\\.?|mg\\.?|inj\\.?| gm| units| unit|ml\\.?',' ')]
  
  lifespan_drug[,drug_name:=str_squish(drug_name)]
  return(lifespan_drug)
}
  
  emr <- fread('ascl_emr.csv')
  
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
  ascl<-cSplit(ascl,'drug_name',sep = '\n', direction = 'long')
  ascl<-cln_fn_ascl(ascl,drug_name)
  ascl<-ascl[drug_name!='']
  dim(ascl[cleared_drug==0])
  ascl[str_count(drug_name,pattern = ' ')==0,drug_name:=str_replace_all(drug_name,'-',' ')]
  ascl<-ascl[!duplicated(ascl)]
  
  
  
  cleaned_bm_function<-function(ascl,tab_units, map_drug){
    t<-paste(unlist(tab_units[,tab_unit]),collapse = '|')
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
      #  scnd<-c('forte','plus','gold','solostar','gp3/850','xr-mex')
      for(k in (l:2)){
        tryCatch({
          if(grepl('.-.-.',w[k]) & grepl(t,w[k-1]) ){
            ascl[i,freq:=w[k]]
            ascl[i,dosage:=w[k-1]]
            ascl[i,cleared_drug:= paste(unlist(w[1:(k-2)]), collapse =   ' ' )]
            ascl[i,drug_name:='']
          }
          
          else if(grepl('.-.-.',w[k])){
            ascl[i,freq:=w[k]]
            #          ascl[i,dosage:=w[k-1]]
            ascl[i,cleared_drug:= paste(unlist(w[1:(k-1)]), collapse =   ' ' )]
            ascl[i,drug_name:='']
          }
          
          else if(grepl(t,w[k]) ){
            #ascl[i,freq:=w[k]]
            ascl[i,dosage:=w[k]]
            ascl[i,cleared_drug:= paste(unlist(w[1:(k-1)]), collapse =   ' ' )]
            ascl[i,drug_name:='']
          }
          
          
        }, error=function(e){}) 
      }
      print(i) 
      #  ascl[i,drug_name:=str_squish(paste(w, collapse =  ' '))]
    }
    
    colnames(ascl)[1]<-'left_drugs'
    ascl<-merge(ascl_u,ascl,by.x = 'drug_name', by.y = 'drugs')
    da<-map_drug
    da<-da[length > 3,.(clean_brand)]
    
    ascl[drug_name!='',cleared_drug:=drug_name]
    ascl[,final_drug:='0']
    
    for (i in 1:length(da$clean_brand)) {
      ascl[grepl(da$clean_brand[i],cleared_drug) & final_drug=='0' ,final_drug:=da$clean_brand[i]]
      print(i)
    }
    
    ascl<-ascl[!duplicated(ascl)]
    return(as.data.table(ascl))
  }
  ascl_long<-cleaned_bm_function(ascl,tab_units,map_drug)
  write.csv(ascl_long,'ascl_long.csv')
  
  
  ################################################################
  ################## new medicine all csv ########################
  ###############################################################
  
  medicines_all <- fread('ascl_medicines.csv')
  ascl_before<-medicines_all[type=='medication' & source %in% c('advice','plan','drug_name')]
  
  emrid_f_old<-ascl_before[,.N,.(emrid)]
  emrid_f<-ascl_long[final_drug!=0,.N,.(final_drug,dosage,emrid)][,.N,.(emrid)]
  emrid_f_t<-ascl_long[cleared_drug!=0,.N,.(cleared_drug,dosage,emrid)][,.N,.(emrid)]
  emrid_f_t$emrid<-as.character(emrid_f_t$emrid)
  
  emrid_f$emrid<-as.character(emrid_f$emrid)
  emrid_f_old$emrid<-as.character(emrid_f_old$emrid)
  
  emrid_all_mapped<-merge(emrid_f,emrid_f_old, by = 'emrid', all = T)
  emrid_all_mapped_t<-merge(emrid_f_t,emrid_f_old, by = 'emrid', all = T)
  emrid_all_mapped_t<-merge(emrid_all_mapped_t,emrid_f, by = 'emrid', all = T)
  
  #ascl_long[grepl(insulin_drugs,cleared_drug)][dosage!=0]
  
  emrid_all_mapped_t$emrid<-as.character(emrid_all_mapped_t$emrid)
  ascl_before$emrid<-as.character(ascl_before$emrid)
  old_to_add<-merge(ascl_before,emrid_all_mapped_t[is.na(N) & !is.na(N.y)], by='emrid')
  old_to_add[,N.y:=NULL][,N.x:=NULL][,N:=NULL][,mid:=NULL][,source:=NULL]
  colnames(old_to_add)
  colnames(medicines_all)
  new_to_add<-merge(ascl_long[final_drug!=0],emrid_all_mapped_t[!is.na(N)],by='emrid')
  colnames(new_to_add)
  new_to_add[,emrid:=as.character(emrid)]
  medicines_all[,emrid:=as.character(emrid)]
  new_to_add<-merge(new_to_add,medicines_all[type!='medication',.(emrid,asclid,uhid,PatientName,gender,age,doctor_name,created_date)],by = 'emrid')
  colnames(new_to_add)
  new_to_add[,N.x:=NULL][,N.y:=NULL][,left_drugs:=NULL][,drug_name:=NULL][,drug_name1:=NULL][,cleared_drug:=NULL][,N:=NULL]
  new_to_add[,value_1:=final_drug][,value_2:=dosage][,value_3:=freq]
  new_to_add<-new_to_add[!duplicated(new_to_add)]
  new_to_add[,dosage:=NULL][,freq:=NULL][,final_drug:=NULL]
  new_to_add[,type:='medication']
  new_medicine_all<-rbind(new_to_add,old_to_add)
  new_medicine_all<-rbind(new_medicine_all,medicines_all[type!='medication',.(type,emrid,asclid,uhid,PatientName,gender,age,doctor_name,created_date,value_1,value_2,value_3)])
  new_medicine_all[,created_date:=as.Date(created_date)]
  new_medicine_all[,created_date:=as.character(created_date)]
  
  write.csv(new_medicine_all,'new_medicine_all.csv', row.names = F)
  
}

medicines_all <- fread('new_medicine_all.csv')


patient_tp <- fread('patientdimension.csv')
gender_dim <- fread('genderdim.csv')
booking_master <- fread('bookingdimension.csv')
drug_master <- fread('drug.csv')
emr_dim <- fread('emrdim.csv')
emr_master <- fread('emrstaging.csv')
tp_lab <- fread('tp_lab_data.csv')


patient_master <- as.data.table(patient_master)
colnames(patient_master) <- tolower(colnames(patient_master))

patient_master_red <- patient_master[,.(patient_id,clinic_id,date_of_birth,gender,city_id,state_id,country_id,pincode,marital_status,entry_date)]

patient_master_red[,gender := str_extract(gender,'[A-Z][a-z]+')]
patient_master_red[,date_of_birth := as.Date(date_of_birth)]
patient_master_red[,entry_date := as.Date(entry_date)]
patient_master_red <- patient_master_red[!clinic_id == 5]


clinic_map <- as.data.table(clinic_map)
clinic_map[state == 'Odisha',state := 'Orissa']
clinic <- merge(clinic_master,clinic_map,by='clinic_name', all.x = TRUE)
clinic[,city := str_trim(city)]
clinic[,state := str_trim(state)]
clinic <- unique(clinic)

patient_master_red <- merge(patient_master_red,clinic,by='clinic_id',all.x=TRUE)
patient_master_red <- unique(patient_master_red)


demo[,registrationDate := as.Date(registrationDate)]
demo[,dob := as.Date(dob)]
demo[,insert_date := NULL]

ascl_location_map <- as.data.table(ascl_location_map)
ascl_location_map[,city := str_trim(city)]
ascl_location_map[,state := str_trim(state)]
ascl_location_map <- unique(ascl_location_map)

demo <- merge(demo,ascl_location_map,by='center_location',all.x=TRUE)

patient_tp <- merge(patient_tp,gender_dim,by.x='genderindex',by.y='index',all.x=TRUE)

patient_lifespan <- patient_master_red[,.(patient_id = as.character(patient_id),date_of_birth,gender,registration_date = entry_date,city,state)]
patient_lifespan[,source := 'lifespan']
patient_ascl <- demo[,.(patient_id = uhid,date_of_birth = dob,gender,registration_date = registrationDate,city,state)]
patient_ascl[,source := 'ascl']
patient_tp <- patient_tp[,.(patient_id = as.character(index),date_of_birth = as.Date(paste(yob,'01','01',sep='-')),gender,registration_date=as.Date(joiningdate),city='Delhi',state='Delhi')]
patient_tp[,source := 'tp']

patient_all <- rbind(patient_lifespan,patient_ascl,patient_tp)
patient_all[,gender := tolower(gender)]
patient_all[,yob := year(date_of_birth)]
patient_all[,date_of_birth := NULL]
patient_all[,patient_id := paste(source,patient_id,sep='_')]
patient_all[,age := year(Sys.Date()) - yob]
patient_all[age > 100 | age < 1,age := NA]
patient_all[is.na(age),yob := NA]

##################################################################################################################



ppv <- as.data.table(ppv)
colnames(ppv) <- tolower(colnames(ppv))
ppv_red <- ppv[,comment_dosages:=paste(noofdosagemon,noofdosageaftern,noofdosageeve,noofdosagebedtime, time_mon, time_after, time_eve,sep = '~'),.(patient_id,created_date,drug_name,dosage_id,frequency_id_multiple,duration_number,display_name)]
ppv_red[,created_date := as.Date(created_date)]
ppv_red <- ppv_red[,source := 'lifespan']


medicines_all <- medicines_all[,.(patient_id = uhid,doctor_name,type,value_1,value_2,value_3,created_date)]
medicines_all[,created_date := as.Date(created_date)]
medicines_all <- medicines_all[,source := 'ascl']


drug_master <- merge(drug_master,booking_master,by.x='bookingindex',by.y='index')
prescription_tp <- drug_master[,.(patient_id = as.character(patientid.x),created_date = as.Date(bookingdate.x),drug_name = value,dosage_id = dosage,frequency_id=frequency,doctor_name = as.character(centreindex))]
prescription_tp <- prescription_tp[,source := 'tp']
prescription_tp <- prescription_tp[drug_name !='NO. OF DAYS',]

prescription_header_lifespan <- unique(ppv_red[,.(patient_id,created_date,source,doctor_name=display_name)])
prescription_header_lifespan <- prescription_header_lifespan[,.(doctor_name = toString(doctor_name)),.(patient_id,created_date,source)]
prescription_header_lifespan[,source := 'lifespan']
prescription_header_lifespan <- prescription_header_lifespan[,pph_id := 1:.N]
prescription_header_lifespan <- prescription_header_lifespan[,pph_id := paste(source,pph_id,sep='_')]

prescription_header_ascl <- unique(medicines_all[,.(patient_id,created_date,source,doctor_name)])
prescription_header_ascl <- prescription_header_ascl[,.(doctor_name = toString(doctor_name)),.(patient_id,created_date,source)]
prescription_header_ascl[,source := 'ascl']
prescription_header_ascl <- prescription_header_ascl[,pph_id := 1:.N]
prescription_header_ascl <- prescription_header_ascl[,pph_id := paste(source,pph_id,sep='_')]

prescription_header_tp <- unique(prescription_tp[,.(patient_id = as.character(patient_id),created_date,source,doctor_name)])
prescription_header_tp <- prescription_header_tp[,pph_id := 1:.N]
prescription_header_tp <- prescription_header_tp[,pph_id := paste(source,pph_id,sep='_')]

prescription_header <- rbind(prescription_header_lifespan,prescription_header_ascl,prescription_header_tp)
prescription_header[,patient_id := paste(source,patient_id,sep='_')]
prescription_header[year(created_date)<2005,created_date:='' ]
##################################################################################################################################


medical_history <- as.data.table(medical_history)
colnames(medical_history) <- tolower(colnames(medical_history))

medical_history_red <- medical_history[,.(id,patient_id,created_date,variable = name,comment)]
medical_history_red[,created_date := as.Date(created_date)]

medical_history_red <- medical_history_red[!is.na(comment)]
medical_history_red <- medical_history_red[!comment == '']
medical_history_red <- medical_history_red[!comment == '.']
medical_history_red <- medical_history_red[order(patient_id,created_date)]
medical_history_red[,variable := tolower(trimws(variable))]

lifepsan_diag <- medical_history_red[str_to_lower(variable) %like% 'diagnosis details']

t <- medical_history_red[variable == 'bp (mm/hg)']
t[,comment:=str_replace_all(comment,'\\\\|\\//','/')]
t[,comment:=str_replace_all(comment,'\\\\|\\//','/')]
t[,comment:=str_replace_all(comment,'\\\\|\\//','/')]
t[,c('bp_systolic','bp_diastolic'):= tstrsplit(comment,'/')[1:2]]
t <- t[!is.na(bp_systolic) & !is.na(bp_diastolic),]
t[,variable := NULL]
t[,comment := NULL]
t <- melt.data.table(t,id.vars= c('id','patient_id','created_date'))
t <- t[!is.na(value),.(id,patient_id, created_date, variable, comment = value)]

medical_history_red <- medical_history_red[!variable == 'bp (mm/hg)']
medical_history_red <- rbind(medical_history_red,t)

#medical_history_red <- medical_history_red[variable %in% c('alcohol','bp (mm/hg)','bmi','cns','complaint text','complication text','current medication details','insulin','oha','others curent medication','cvs','diabetes history details','diabetologist notes','family history other details','foot comments','height (centimeters)','other lifestyle factors','sedentary','general examination details','smoking','waist (centimeters)','weight (kilograms)','duration diabetes','year diagnosis','bp_diastolic','bp_systolic')]


setnames(emr,c('bp_diatolic'),c('bp_diastolic'))
emr[,c('asclid', 'exercise_tolerance', 'diet', 'temperature', 'bsa', 'respiration', 'other_tests', 'investigations_results','referred_to', 'procedures_performed', 'followup', 'note_to_secretary', 'special_notes', 'service_name', 'insert_date' ):=NULL]
emr <- melt.data.table(emr,id.vars = c('emrid','uhid','created_date'))
emr <- emr[!value == '']

emr[,created_date := as.Date(created_date)]

#emr <- emr[variable %in% c('alcohol','bmi','bsa','bp_diastolic','significant_family_history','bp_systolic','chief_complaints','pulse','respiration','temperature','height','hopi','past_medical_history','physical_examination','smoking','weight')]



emr_tp <- merge(emr_master,emr_dim,by.x='biomarkerid',by.y='index')
emr_tp <- merge(emr_tp,booking_master,by.x='labbillid',by.y='index')
#emr_tp <- emr_tp[level2_n %in% c('bpmmhg','weightkg','chest','abd','cvs','bp-supinemmhg','cyanosis','pallor','physicalactivity','hypertension','cns','fundus','pulsemin','heightcm','durationofdiabetesyrs','ageatthetimeofdetectionyrs','alcohol','bmi','smoking')]
emr_tp[level2_n == 'bpmmhg',level2_n := paste(level2_n,level3_n,sep='_')]

##@rohit - complaint and diagnosis
emr_tp <- emr_tp[level1_n %in% c('diagnosis','complaint','complaints'), value := paste(level2, value, comments, sep = " ")]
emr_tp <- emr_tp[level1_n %in% c('diagnosis'), level2 := "diagnosis"]
emr_tp <- emr_tp[level1_n %in% c('complaint','complaints'), level2 := "complaints"]

emr_tp <- emr_tp[level1_n == 'hOMedicalHistory' & grepl("yes",value, ignore.case = T), value := level2_n]
emr_tp <- emr_tp[level1_n == 'hOMedicalHistory' & grepl("yes",value, ignore.case = T), level2 := "diagnosis"]

medical_history_tp <- emr_tp[,.(patient_id = as.character(patientid), created_date = as.Date(bookingdate.y),variable = level2_n,value)]

t <- medical_history_tp[variable %in% c('bp-supinemmhg','bp-standingmmhg','bp-sittingmmhg','bpmmhg')]
t[,c('bpmmhg_systolic','bpmmhg_diastolic'):= trim(tstrsplit(value,'/')[1:2])]
t <- t[!is.na(bpmmhg_systolic) & !is.na(bpmmhg_diastolic),]
t[,variable := NULL]
t[,value := NULL]
t <- melt.data.table(t,id.vars= c('patient_id','created_date'))
t <- t[!is.na(value),.(patient_id, created_date, variable, value)]

medical_history_tp <- rbind(medical_history_tp[variable != "bp-supinemmhg"],t)

medical_history_tp[,source := 'tp']
medical_history_tp_header <- unique(medical_history_tp[,.(patient_id,created_date,source)])
medical_history_tp_header <- medical_history_tp_header[,history_id := 1:.N]
medical_history_tp_header <- medical_history_tp_header[,history_id := paste(source,history_id,sep='_')]

medical_history_lifespan <- medical_history_red[,.(patient_id,created_date,variable,value=comment)]
medical_history_lifespan[,source := 'lifespan']
medical_history_lifespan_header <- unique(medical_history_lifespan[,.(patient_id,created_date,source)])
medical_history_lifespan_header <- medical_history_lifespan_header[,history_id := 1:.N]
medical_history_lifespan_header <- medical_history_lifespan_header[,history_id := paste(source,history_id,sep='_')]

medical_history_ascl <- emr[,.(patient_id = uhid, created_date, variable, value)]
medical_history_ascl[,source := 'ascl']
medical_history_ascl_header <- unique(medical_history_ascl[,.(patient_id,created_date,source)])
medical_history_ascl_header <- medical_history_ascl_header[,history_id := 1:.N]
medical_history_ascl_header <- medical_history_ascl_header[,history_id := paste(source,history_id,sep='_')]

medical_history_header <- rbind(medical_history_tp_header,medical_history_lifespan_header,medical_history_ascl_header)
medical_history_header[,patient_id := paste(source,patient_id,sep='_')]
setnames(medical_history_header,'created_date','history_date')

medical_history_all <- rbind(medical_history_lifespan, medical_history_ascl,medical_history_tp)
setnames(medical_history_all,'created_date','history_date')
medical_history_all[,patient_id := paste(source,patient_id,sep='_')]
medical_history_all <- unique(medical_history_all)

medical_history_all <- merge(medical_history_all,medical_history_header,by=c('patient_id','history_date','source'))
medical_history_all <- medical_history_all[,.(history_id,variable,value,source)]
medical_history_all <- medical_history_all[,variable := as.character(variable)]
medical_history_map <- as.data.table(medical_history_map)

medical_history_all <- merge(medical_history_all,medical_history_map,by=c('variable','source'))

medical_history_all <- unique(medical_history_all)
medical_history_all <- medical_history_all[,chars := nchar(value)]
medical_history_all <- medical_history_all[order(history_id, variable, -chars)]
medical_history_all <- medical_history_all[,head(.SD,1),.(history_id, variable)]
medical_history_all <- medical_history_all[,variable := NULL]
medical_history_all <- unique(medical_history_all)

medical_history_all <- medical_history_all[,.(value = paste(value, collapse = '~')),.(history_id,source,vital, min_value, max_value, unique)]
medical_history_all <- medical_history_all[,.(history_id, source, value, vital, min_value, max_value, unique)]

########################################################################################

complaints <- medical_history_all[vital == 'complaints']
complaints <- complaints[,value := str_replace_all(value,"\\\\","")]
complaints <- complaints[,value := str_to_lower(value)]
#complaints <- complaints[,value := bracketX(value, "angle")]
complaints <- complaints[,value := str_replace_all(value,'[^[A-Za-z ]]','')]
complaints <- complaints[,value := str_replace_all(value,'[ ]+',' ')]
complaints <- complaints[,value := trimws(value)]


complaints_map_sheet <- as.data.table(complaints_map_sheet)
complaints_map_sheet <- complaints_map_sheet[, negate := paste("no", value, sep = " ")]
complaints_map_sheet <- complaints_map_sheet[,.(string = paste0(value, collapse = "|"), negate = paste0(negate, collapse = "|") ),unified_name]

a <- Sys.time()

for (row in 1:nrow(complaints_map_sheet)) {
  complaints$temp <- grepl(complaints_map_sheet$string[row], complaints$value) & !grepl(complaints_map_sheet$negate[row], complaints$value)
  names(complaints)[names(complaints) == 'temp'] <- complaints_map_sheet$unified_name[row]
}
Sys.time() - a

complaints <- complaints[,c('source','value','vital','min_value','max_value','unique') := NULL]

complaints <- melt.data.table(complaints,id.vars=c('history_id'))
complaints <- complaints[value == TRUE]
complaints$value <- NULL
names(complaints)[names(complaints) == "variable"] <- "complaint"
# 
# 
# complaints <- cSplit(complaints,'value',',',direction='long')
# complaints <- cSplit(complaints,'value','.',direction='long')
# complaints <- complaints[,value := str_replace_all(value,'[^[A-Za-z ]]','')]
# complaints <- complaints[,value := str_replace_all(value,' of | in | and |rbs|mgdl|nad|nbsp| since ',' ')]
# complaints <- complaints[,value := str_replace_all(value,'[ ]+',' ')]
# complaints <- complaints[,value := trimws(value)]
# complaints <- complaints[!value == '']
# 
# complaints_map_sheet <- gs_title('complaints_emr_mapping')
# complaints_map_sheet <- complaints_map_sheet %>% gs_read(ws='Sheet1',range = cell_cols(1:3))
# complaints_map_sheet <- as.data.table(complaints_map_sheet)
# 
# complaints_all_map <- merge(complaints,complaints_map_sheet,by=c('source','value'),all.x=TRUE)
# complaints_final <- complaints_all_map[!is.na(unified_name)]
# complaints_final <- complaints_final[,.(history_id,complaint = unified_name)]
# complaints_final <- unique(complaints_final)

complaints_final_csv <- merge(complaints,medical_history_header,by='history_id')
complaints_final_csv <- complaints_final_csv[,.(patient_id,complaint)]
complaints_latest <- unique(complaints_final_csv)
complaints_final_csv <- complaints_latest[,.(complaint = toString(complaint)),.(patient_id)]


##################################################################################################################################

lifepsan_diag[,comment_new := str_replace_all(comment,'\\\n|\\\r|<.*?>',' ')]
#lifepsan_diag <- cSplit(lifepsan_diag,'comment_new',sep=',',direction = 'long')
lifepsan_diag[,comment_new := str_to_lower(comment_new)]
lifepsan_diag[,comment_new := str_replace_all(comment_new,'[ ]+',' ')]

ascl_diag <- medicines_all[type == 'diagnosis']
ascl_diag[,comment := str_replace_all(value_1,'\\\n|\\\r|<.*?>',' ')]
#ascl_diag <- cSplit(ascl_diag,'value_1',sep=',',direction='long')
ascl_diag[,comment := str_to_lower(comment)]

##@rohit include tp diagnosis

diagnosis_lifespan <- lifepsan_diag[,.(patient_id,created_date,diagnosis = comment_new)]
diagnosis_lifespan[,source := 'lifespan']
diagnosis_lifespan <- diagnosis_lifespan[,diag_id := 1:.N]
diagnosis_lifespan <- diagnosis_lifespan[,diag_id := paste(source,diag_id,sep='_')]

diagnosis_ascl <- ascl_diag[,.(patient_id,created_date,diagnosis = value_1)]
diagnosis_ascl[,source := 'ascl']
diagnosis_ascl <- diagnosis_ascl[,diag_id := 1:.N]
diagnosis_ascl <- diagnosis_ascl[,diag_id := paste(source,diag_id,sep='_')]

diagnosis_tp <- medical_history_tp[variable == "diagnosis",.(diagnosis = paste(value, collapse = ",")), .(patient_id, created_date)]
diagnosis_tp[,source := 'tp']
diagnosis_tp <- diagnosis_tp[,diag_id := 1:.N]
diagnosis_tp <- diagnosis_tp[,diag_id := paste(source,diag_id,sep='_')]


diagnosis_all <- rbind(diagnosis_lifespan,diagnosis_ascl, diagnosis_tp)
diagnosis_all[,diagnosis := str_to_lower(diagnosis)]
diagnosis_all[,diagnosis := str_replace_all(diagnosis,"[^[:alnum:]]",' ')]
diagnosis_all[,diagnosis := str_replace_all(diagnosis,' nbsp | div | li | span | amp ','')]
diagnosis_all[,diagnosis := str_replace_all(diagnosis,'^nbsp|^div|^li|^span|^amp','')]
diagnosis_all[,diagnosis := str_replace_all(diagnosis,'nbsp$|div$|li$|span$|amp$','')]
diagnosis_all[,diagnosis := trimws(diagnosis)]
diagnosis_all <- diagnosis_all[,.(diag_id,patient_id,created_date,diagnosis,source)]
diagnosis_all[,patient_id := paste(source,patient_id,sep='_')]

# diagnosis_map_sheet <- gs_title('diagnosis_emr_mapping')
# diagnosis_map_sheet <- diagnosis_map_sheet %>% gs_read(ws='Sheet1',range = cell_cols(1:4))
# diagnosis_map_sheet <- as.data.table(diagnosis_map_sheet)
# diagnosis_map_sheet <- diagnosis_map_sheet[!duplicated(diagnosis_map_sheet[,c("source","diagnosis")]),]
# diagnosis_map_sheet <- diagnosis_map_sheet[!is.na(unified_name),]
# 
# diagnosis_all_map <- merge(diagnosis_all,diagnosis_map_sheet,by=c('source','diagnosis'),all.x=TRUE)

# 
# diagnosis_all_map <-  diagnosis_all_map[is.na(unified_name) & grepl("retinop|npdr|ndpr|pdr|retinal detach|optic nerve disease", x = diagnosis), unified_name := 'diabetes retinopathy']
# diagnosis_all_map <- diagnosis_all_map[is.na(unified_name) & grepl("neuropath|psn|pn |dn |tingling", x = diagnosis), unified_name := 'diabetes neuropathy']
# diagnosis_all_map <- diagnosis_all_map[is.na(unified_name) & grepl("cad|coronary arte|cabg|ihd|ischemic heart|ischaemic heart|atherosclero|cag|ptca|angiopla|stent|heart disea|myocardial infar|heart failure", x = diagnosis) , unified_name := 'CAD']
# diagnosis_all_map <- diagnosis_all_map[is.na(unified_name) & grepl("stroke|cva|cerebro|ischemic attack|hemorrhage|ischaemia cerebral", x = diagnosis) , unified_name := 'stroke']
# #lvd <- diagnosis_all_map[grepl("lvd|amputation|gangrene", x = diagnosis) & year(created_date) == 2018 & month(created_date) < 7 ,]
# #diab_foot <- diagnosis_all_map[grepl("diabetic foot|arthropathy|charcot|foot ulcer", x = diagnosis) & year(created_date) == 2018 & month(created_date) < 7 ,]
# diagnosis_all_map <- diagnosis_all_map[is.na(unified_name) & grepl("ckd|kd |chronic kidney|nephropath|dkd|kidney di", x = diagnosis) , unified_name := 'nephropathy']
# diagnosis_all_map <- diagnosis_all_map[is.na(unified_name) & grepl("htn|hypertension|hyper tension|high blood pressure|high bp", x = diagnosis) , unified_name := 'hypertension']


diagnosis_map_sheet <- as.data.table(diagnosis_map_sheet)
diagnosis_map_sheet <- diagnosis_map_sheet[, negate := paste("no", value, sep = " ")]
diagnosis_map_sheet <- diagnosis_map_sheet[,.(string = paste0(value, collapse = "|"), negate = paste0(negate, collapse = "|") ),unified_name]

a <- Sys.time()

for (row in 1:nrow(diagnosis_map_sheet)) {
  diagnosis_all$temp <- grepl(diagnosis_map_sheet$string[row], diagnosis_all$diagnosis) & !grepl(diagnosis_map_sheet$negate[row], diagnosis_all$diagnosis)
  names(diagnosis_all)[names(diagnosis_all) == 'temp'] <- diagnosis_map_sheet$unified_name[row]
}
Sys.time() - a

diagnosis_all_map <- diagnosis_all
diagnosis_all_map <- diagnosis_all_map[,diagnosis:=NULL]


diagnosis_all_map <- melt.data.table(diagnosis_all_map,id.vars=c('diag_id','patient_id','created_date','source'))
diagnosis_all_map <- diagnosis_all_map[value == TRUE]
diagnosis_all_map$value <- NULL

diagnosis_final <- diagnosis_all_map[,.(diag_id,patient_id,created_date,source,diagnosis = variable)]

diagnosis_final_csv <- diagnosis_final[,.(patient_id,diagnosis,date=created_date)]
diagnosis_latest <- unique(diagnosis_final_csv)
diagnosis_latest[,dia_source:='doctor']
#diagnosis_latest <- diagnosis_latest[order(patient_id,diagnosis)]
#diagnosis_final_csv <- diagnosis_latest[,.(diagnosis = toString(diagnosis)),.(patient_id)]

# diagnosis_new <- diagnosis_all_map[is.na(unified_name)]
# diagnosis_new <- diagnosis_new[,.N,.(source,diagnosis)][order(-N)]
# write.csv(diagnosis_new[N>10],'diagnosis_new_unmapped.csv',row.names=F,na='')
# gs_upload('diagnosis_new_unmapped.csv',sheet_title = 'diagnosis_new_unmapped', overwrite = T)

##################################################################################################################################

# Commenting as not getting used anywhere and only for ASCL till not
# prescription_others <- medicines_all[!type %in% c('medication','diagnosis'),.(patient_id,created_date,source,type,value = value_1)]
# prescription_others[,patient_id := paste(source,patient_id,sep='_')]

##################################################################################################################################

medicines <- medicines_all[type == 'medication']
medicines[,type := NULL]
medicines[,source := NULL]
medicines[,value_1 := gsub('-',' ',value_1)]
medicines[,value_1 := gsub('\\.',' ',value_1)]
medicines[,value_1 := gsub("\\s+", " ", str_trim(value_1))]
prescription_ascl <- medicines[,.(patient_id,created_date,drug_name = value_1,dosage_id = value_2,frequency_id = value_3,doctor_name)]
prescription_ascl[,source := 'ascl']

prescription_lifespan <- ppv_red[,.(comment_dosages,patient_id,created_date,drug_name,dosage_id,frequency_id = frequency_id_multiple,doctor_name = display_name)]
prescription_lifespan[,source := 'lifespan']


prescription_all <- rbind(prescription_lifespan,prescription_ascl[,comment_dosages:=''],prescription_tp[,comment_dosages:=''])
prescription_all[,patient_id := paste(source,patient_id,sep='_')]
prescription_all[,doctor_name := NULL]
prescription_all <- merge(prescription_all,prescription_header,by=c('patient_id','created_date','source'))
prescription_all <- prescription_all[,c('patient_id','created_date','source','doctor_name') := NULL]

prescription_all[,drug_name := trimws(drug_name)]
prescription_all[,drug_name_mod := trimws(str_replace(str_to_lower(drug_name),'tab\\.|cap\\.|inj\\.|^tab |^cap |^inj |^tablets|^tablet|^injection|^tab-|^cap-|^inj-',''))]
prescription_all[,drug_name_mod := str_replace_all(drug_name_mod,'^inj','')]
prescription_all[,drug_name_mod := str_replace_all(drug_name_mod,'\\(|\\)',' ')]
prescription_all[,drug_name_mod := gsub('[-]+',' ',drug_name_mod)]
prescription_all[,drug_name_mod := gsub('[ ]+',' ',drug_name_mod)]
prescription_all[,drug_name_mod := trimws(drug_name_mod)]

prescription_all[,dosage := trimws(str_extract(drug_name_mod,' [0-9].*'))]
prescription_all[is.na(dosage) & !dosage_id == '',dosage := str_to_lower(str_replace_all(dosage_id,'\\(|\\)',' '))]
prescription_all[,dosage := gsub('cap|tablet|-|tablets|tab','',dosage)]
prescription_all[,dosage := gsub('\\+','/',dosage)]
prescription_all[,dosage := gsub('[ ]*','',dosage)]
prescription_all[,dosage := str_replace(dosage,'/1gm','/1000mg')]

prescription_all<-unique(prescription_all)

prescription_all<-mapping(prescription_all)

#prescription_all[,dosage_num := str_extract(dosage,'[0-9]+(\\.?)[0-9]*(/?)[0-9]*(\\.?)[0-9]*')]
prescription_all[,unit := str_replace(dosage,'[0-9]+(\\.?)[0-9]*(/?)[0-9]*(\\.?)[0-9]*','')]

prescription_all[str_detect(dosage,'[0-9]+(\\.?)[0-9]*(mg)(/)[0-9]*(\\.?)[0-9]*(mg)'),unit := 'mg']
prescription_all[str_detect(dosage,'[0-9]+(\\.?)[0-9]*(mg)(/)[0-9]*(\\.?)[0-9]*(mg)'),dosage := str_replace_all(dosage,'mg','')]

prescription_all[,dosage_num := str_extract(dosage,'[0-9]+(\\.?)[0-9]*(/?)[0-9]*(\\.?)[0-9]*')]
prescription_all[,unit := str_replace_all(unit,'\\.|,','')]
prescription_all[unit %in% c('gram','grm','g'),unit := 'gm']
prescription_all[unit %in% c('microgm','micgm'),unit := 'mcg']

prescription_all[unit == 'omg',dosage_num := paste0(dosage_num,'0')]
prescription_all[unit == 'omg',unit := 'mg']


prescription_all[,brand := trimws(str_replace(drug_name_mod,' [0-9].*',''))]
prescription_all[unit %in% c('forte','mfforte','mf'),brand := drug_name_mod]
prescription_all[unit == 'forte',dosage_num := NA]
prescription_all[unit == 'forte',unit := NA]

prescription_all[,brand := str_replace(brand,'[0-9]+/[0-9]+.+','')]
prescription_all[,brand := str_replace_all(brand,'/','')]

prescription_all[,brand_new := gsub('[.-]+',' ',brand)]
prescription_all[,brand_new := gsub('[ ]+',' ',brand_new)]
prescription_all[,brand_new := trimws(brand_new)]
prescription_all[,brand_new := gsub('^t ','',brand_new)]
prescription_all[,brand_new := gsub('^tb ','',brand_new)]
prescription_all[,brand_new := gsub('^ing ','',brand_new)]
prescription_all[,brand_new := gsub('^ta ','',brand_new)]
prescription_all[,brand_new := gsub('^ab ','',brand_new)]
prescription_all[,clean_brand := str_replace_all(brand_new,' sr$| xr$','')]


drug_name_map <- as.data.table(drug_name_map)
setkey(drug_name_map, "clean_brand")
drug_name_map <- unique(drug_name_map)
drug_name_map <-drug_name_map[,head(.SD,1), clean_brand]

prescription_all <- merge(prescription_all,drug_name_map,by='clean_brand',all.x=TRUE)
prescription_all <- prescription_all[!is.na(replaced_name),clean_brand := replaced_name]
prescription_all[,replaced_name := NULL]

prescription_all_csv <- prescription_all[,.(clean_brand = toString(clean_brand)),.(pph_id)]

prescription_header[,doctor_name:=tolower(str_squish(doctor_name))]
doc_map<-doc_map[,head(.SD,1),doctor_name][,.(doctor_name,Doctor_Category,`Diabetes Mapping_Abbott_11june19`,RSSDI)]
prescription_header<-merge(prescription_header,doc_map, by='doctor_name',all.x = T)
prescription_header_latest <- prescription_header[order(patient_id,-created_date)]
prescription_header_latest <- prescription_header_latest[,head(.SD,1),patient_id][,.(pph_id, Doctor_Category,`Diabetes Mapping_Abbott_11june19`,RSSDI)]
prescription_all_latest <- prescription_all[pph_id %in% prescription_header_latest[,pph_id]]

##########################################


molecules <- as.data.table(molecules)
setkey(molecules, "clean_brand")
molecules <- unique(molecules)
molecules <- cSplit(molecules,'molecule',sep=',',direction='long')

molecules <- molecules[!molecule == '']
molecules <- molecules[!formulation == '']

molecules <- molecules[,molecule := str_to_lower(molecule)]
molecules <- molecules[,molecule := str_replace_all(molecule,'\\\xa0','')]
molecules <- molecules[,molecule := str_trim(molecule)]
molecules <- molecules[,purpose := str_to_lower(purpose)]
molecules <- molecules[,formulation := str_to_lower(formulation)]
molecules <- molecules[,formulation := str_replace_all(formulation,' sr','')]
molecules <- molecules[,formulation := str_replace_all(formulation,' er','')]


mol_class <- as.data.table(mol_class)
mol_class <- unique(mol_class)
mol_class <- mol_class[!molecule == '']
mol_class <- mol_class[,molecule := trimws(str_to_lower(molecule))]
mol_class <- mol_class[,head(.SD,1),molecule]
mol_class <- mol_class[,.(molecule,class)]

molecules <- unique(molecules)
molecules <- merge(molecules,mol_class,by='molecule',all.x=TRUE)
molecules <- molecules[,clean_brand := trimws(str_to_lower(clean_brand))]
molecules <- molecules[order(clean_brand, molecule)] 
temp <- dcast.data.table(molecules[,molecule_c:= "composition"],clean_brand ~ molecule_c, value.var = 'molecule', fun.aggregate = paste,collapse = ",")
molecules <- merge(molecules[,molecule_c:=NULL], temp, by=c("clean_brand"))
molecules <- molecules[order(clean_brand, class)] 
temp <- dcast.data.table(molecules[,molecule_c:= "composition_class"],clean_brand ~ molecule_c, value.var = 'class', fun.aggregate = paste,collapse = ",")
molecules <- merge(molecules[,molecule_c:=NULL], temp, by=c("clean_brand"))
temp <- NULL

insulin_drug<-molecules[grepl('insulin',molecule)]

dosages<-prescription_all
dosages<-merge(dosages,molecules,by='clean_brand', allow.cartesian = T)
dosages[,c('drug_name_mod','drug_name','brand','brand_new','dosage_id'):=NULL]
dosages<-merge(dosages,prescription_header, by='pph_id')
dosages<-dosages[!(dosage %in%  c('0','') & frequency_id %in% c('0',''))]

dosages_insulin<-dosages[grepl('insulin',molecule)]
#dosages_insulin<-dosages_insulin[dosage!='0']
dosages_insulin[,doctor_name:=NULL]

prescription_all <- prescription_all[,.(pph_id,clean_brand)]
prescription_all <- unique(prescription_all)
prescription_all_csv <- prescription_all[,.(clean_brand = toString(clean_brand)),.(pph_id)]

drug_composition <- unique(molecules[,.(clean_brand, composition, composition_class)])

prescription_new_brands <- prescription_all[!clean_brand %in% molecules[,clean_brand]]
prescription_new_brands <- prescription_new_brands[,.N,clean_brand][order(-N)]
write.csv(prescription_new_brands[N>10],'prescription_new_brands_unmapped.csv',row.names=F,na='')
gs_upload('prescription_new_brands_unmapped.csv',sheet_title = 'prescription_new_brands_unmapped', overwrite = TRUE)

prescription_brand_molecules <- merge(prescription_all,molecules,by='clean_brand',allow.cartesian = TRUE)
prescription_molecules_csv <- prescription_brand_molecules[,.(molecule = toString(molecule)),.(pph_id)]
prescription_molecules_latest <- unique(prescription_brand_molecules[pph_id %in% prescription_header_latest[,pph_id],.(pph_id,molecule)])

#############################################

# switching <- gs_title('emr_switching')
# switching <- switching %>% gs_read(ws='Sheet1')
# switching <- as.data.table(switching)

prescription_all_brand <- unique(prescription_brand_molecules[purpose == 'antidiabetic',.(pph_id,clean_brand)])

brand_switching <- dcast.data.table(prescription_all_brand,pph_id ~ clean_brand,value.var = 'clean_brand',fun.aggregate = function(x){as.integer(length(x) > 0)})
brand_switching <- melt.data.table(brand_switching,id.vars=c('pph_id'))
brand_switching <- merge(brand_switching,prescription_header[,.(pph_id,patient_id,created_date)],by='pph_id')

a <- Sys.time()
temp <- brand_switching[value > 0,.N,.(patient_id,variable)]
brand_switching <- merge(brand_switching, temp, by=c("patient_id","variable"))
brand_switching$N <- NULL
brand_switching <- brand_switching[order(patient_id,variable,created_date)]
brand_switching <- brand_switching[,drug_change := value - data.table::shift(value),.(patient_id,variable)]
Sys.time() - a

brand_switching <- brand_switching[!is.na(drug_change) | value == 1,]

brand_first_given <- brand_switching[value==1, min(created_date),.(patient_id,variable)]
brand_first_removed <- brand_switching[drug_change == -1, min(created_date),.(patient_id,variable)]
brand_duration <- merge(brand_first_given, brand_first_removed, by=c("patient_id","variable"))
brand_duration <- brand_duration[,duration := as.integer(V1.y) - as.integer(V1.x)]
brand_duration$V1.x <- NULL
brand_first_given <- NULL
brand_first_removed <- NULL

brand_switching <- brand_switching[!is.na(drug_change)]
brand_switching <- brand_switching[!(value == 0 & drug_change == 0)]
brand_switching <- merge(x = brand_switching, y = brand_duration, by.x = c("patient_id","variable","created_date"), by.y = c("patient_id","variable","V1.y"), all.x = TRUE)

brand_switching_antidiabetic <- copy(brand_switching)

brand_switching <- brand_switching[drug_change == -1,status := 'removed_brand']
brand_switching <- brand_switching[drug_change == 1,status := 'added_brand']
brand_switching <- brand_switching[drug_change == 0,status := 'continued_brand']

brand_switching_molecule <- merge(brand_switching, drug_composition, by.x = c("variable"), by.y = c("clean_brand"))
brand_switching_molecule <- merge(brand_switching_molecule[status == "removed_brand"], brand_switching_molecule[status == "added_brand",.(pph_id, variable, composition, composition_class)], by=c("pph_id"), allow.cartesian = T)
brand_switching_molecule <- brand_switching_molecule[,same_mol:=ifelse(composition.x == composition.y,1,ifelse(mapply(grepl, composition.x, composition.y, fixed = TRUE),2,0))]
brand_switching_molecule <- brand_switching_molecule[,same_class:=ifelse(composition_class.x == composition_class.y,1,ifelse(mapply(grepl, composition_class.x, composition_class.y, fixed = TRUE),2,0))]
brand_switching_molecule <- brand_switching_molecule[,.(pph_id, variable.x,variable.y,composition.x, composition.y, composition.x, composition.y, same_mol, same_class)]
names(brand_switching_molecule) <- c("pph_id", "brand_removed","brand_added","composition_removed","composition_aded","class_removed","class_aded","same_molecule","same_class")
brand_switching_molecule <- merge(brand_switching_molecule, insulin_acting,by.x ='brand_removed' , by.y='clean_brand',all.x = T)
setnames(brand_switching_molecule,c('category','molecule'),c('removed_category','removed_molecule'))
brand_switching_molecule <- merge(brand_switching_molecule, insulin_acting,by.x ='brand_added' , by.y='clean_brand',all.x = T)
setnames(brand_switching_molecule,c('category','molecule'),c('added_category','added_molecule'))
brand_switching_molecule <- brand_switching_molecule[,same_category:=ifelse(removed_category == added_category,'1','0')]
brand_switching_molecule[is.na(added_category) & !is.na(removed_category), same_category:='Insulin to OHA']
brand_switching_molecule[!is.na(added_category) & is.na(removed_category), same_category:='OHA to Insulin']
brand_switching_molecule[,same_insulin_molecule:=ifelse(added_molecule==removed_molecule,"1","0")]
brand_switching_molecule[is.na(removed_category),removed_category:='Non Insulin']
brand_switching_molecule[is.na(added_category),added_category:='Non Insulin']
brand_switching_molecule[is.na(removed_molecule),removed_molecule:='Non Insulin Molecule']
brand_switching_molecule[is.na(added_molecule),added_molecule:='Non Insulin Molecule']

net_gain<-merge(brand_switching_molecule,prescription_header_latest, by='pph_id')
net_gain_mol_remove<-net_gain[,.N,removed_molecule]
net_gain_mol_added<-net_gain[,.N,added_molecule]
net_gain_mol<-merge(net_gain_mol_added,net_gain_mol_remove, by.x = 'added_molecule',by.y = 'removed_molecule')
setnames(net_gain_mol,c('added_molecule','N.x','N.y'),c('molecule','gain','loss'))

net_gain<-merge(brand_switching_molecule,prescription_header_latest, by='pph_id')
net_gain_cat_remove<-net_gain[,.N,removed_category]
net_gain_cat_added<-net_gain[,.N,added_category]
net_gain_cat<-merge(net_gain_cat_added,net_gain_cat_remove, by.x = 'added_category',by.y = 'removed_category')
setnames(net_gain_cat,c('added_category','N.x','N.y'),c('category','gain','loss'))


brand_switching_dcast <- dcast.data.table(brand_switching,pph_id ~ status,value.var = 'variable',fun.aggregate = paste,collapse = ',')

molecule_switching <- dcast.data.table(prescription_brand_molecules[purpose == 'antidiabetic'],pph_id ~ molecule,value.var = 'molecule',fun.aggregate = function(x){as.integer(length(x) > 0)})
molecule_switching <- melt.data.table(molecule_switching,id.vars=c('pph_id'))
molecule_switching <- merge(molecule_switching,prescription_header[,.(pph_id,patient_id,created_date)],by='pph_id')

temp <- molecule_switching[value > 0,.N,.(patient_id,variable)]
molecule_switching <- merge(molecule_switching, temp, by=c("patient_id","variable"))
molecule_switching$N <- NULL

molecule_switching <- molecule_switching[order(patient_id,variable,created_date)]
molecule_switching <- molecule_switching[,drug_change := value - data.table::shift(value),.(patient_id,variable)]

molecule_switching <- molecule_switching[!is.na(drug_change) | value == 1,]

molecule_first_given <- molecule_switching[value==1, min(created_date),.(patient_id,variable)]
molecule_first_removed <- molecule_switching[drug_change == -1, min(created_date),.(patient_id,variable)]
molecule_duration <- merge(molecule_first_given, molecule_first_removed, by=c("patient_id","variable"))
molecule_duration <- molecule_duration[,duration := as.integer(V1.y) - as.integer(V1.x)]
molecule_duration$V1.x <- NULL
molecule_first_given <- NULL
molecule_first_removed <- NULL

molecule_switching <- molecule_switching[!is.na(drug_change)]
molecule_switching <- molecule_switching[!(value == 0 & drug_change == 0)]
molecule_switching <- merge(x = molecule_switching, y = molecule_duration, by.x = c("patient_id","variable","created_date"), by.y = c("patient_id","variable","V1.y"), all.x = TRUE)

molecule_switching <- molecule_switching[drug_change == -1,status := 'removed_molecule']
molecule_switching <- molecule_switching[drug_change == 1,status := 'added_molecule']
molecule_switching <- molecule_switching[drug_change == 0,status := 'continued_molecule']
molecule_switching<-merge(molecule_switching, unique(insulin_acting[,.(molecule,category)]), by.x = 'variable', by.y = 'molecule', all.x = T)



molecule_switching_dcast <- dcast.data.table(molecule_switching,pph_id ~ status,value.var = 'variable',fun.aggregate = paste,collapse = ',')

brand_switching_antidiabetic <- brand_switching_antidiabetic[!drug_change == 0]
brand_switching_antidiabetic <- brand_switching_antidiabetic[order(patient_id,-created_date)]
brand_switching_antidiabetic <- brand_switching_antidiabetic[,head(.SD,1),patient_id]
brand_switching_antidiabetic <- brand_switching_antidiabetic[,.(patient_id,date_changed = created_date)]

class_switching <- dcast.data.table(prescription_brand_molecules[purpose == 'antidiabetic'],pph_id ~ class,value.var = 'class',fun.aggregate = function(x){as.integer(length(x) > 0)})
class_switching <- melt.data.table(class_switching,id.vars=c('pph_id'))
class_switching <- merge(class_switching,prescription_header[,.(pph_id,patient_id,created_date)],by='pph_id')

temp <- class_switching[value > 0,.N,.(patient_id,variable)]
class_switching <- merge(class_switching, temp, by=c("patient_id","variable"))
class_switching$N <- NULL

class_switching <- class_switching[order(patient_id,variable,created_date)]
class_switching <- class_switching[,drug_change := value - data.table::shift(value),.(patient_id,variable)]

class_switching <- class_switching[!is.na(drug_change) | value == 1,]

class_first_given <- class_switching[value==1, min(created_date),.(patient_id,variable)]
class_first_removed <- class_switching[drug_change == -1, min(created_date),.(patient_id,variable)]
class_first_conti <- class_switching[drug_change == 0 & value ==1, max(created_date),.(patient_id,variable)]


class_duration <- merge(class_first_given, class_first_removed, by=c("patient_id","variable"), all.x = T)
class_duration <- merge(class_duration, class_first_conti, by=c("patient_id","variable"), all.x = T)


class_duration <- class_duration[,duration := as.integer(V1.y) - as.integer(V1.x)]
class_duration <- class_duration[is.na(duration),duration := as.integer(V1) - as.integer(V1.x)]

class_duration$V1.x <- NULL
class_duration$V1 <- NULL
class_first_given <- NULL
class_first_removed <- NULL

class_switching <- class_switching[!is.na(drug_change)]
class_switching <- class_switching[!(value == 0 & drug_change == 0)]
class_switching <- merge(x = class_switching, y = class_duration, by.x = c("patient_id","variable","created_date"), by.y = c("patient_id","variable","V1.y"), all.x = TRUE)

class_switching <- class_switching[drug_change == -1,status := 'removed_class']
class_switching <- class_switching[drug_change == 1,status := 'added_class']
class_switching <- class_switching[drug_change == 0,status := 'continued_class']
class_switching_dcast <- dcast.data.table(class_switching,pph_id ~ status,value.var = 'variable',fun.aggregate = paste,collapse = ',')


##################################################################################################################################


investigation_details <- as.data.table(investigation_details)
colnames(investigation_details) <- tolower(colnames(investigation_details))
investigation_details[,c('created_by','local_id','active_flag') := NULL]
investigation_details[,created_date := as.Date(created_date)]
investigation_details <- investigation_details[investigation_date != '',]
investigation_details[,investigation_date := as.Date(investigation_date)]

investigation_details <- melt.data.table(investigation_details,id.vars=c('inv_id','investigation_date','patient_id','created_date','notes','clinic_id'))
investigation_details <- investigation_details[!is.na(value)]
setnames(investigation_details,'variable','biomarkername')
#investigation_details <- investigation_details[biomarker %in% c('hba1c','ppglucose','fasting_glucose','total_cholesterol','triglycerides','ldl','creatnine')]


lab[,testDate := as.Date(testDate)]
setnames(lab,'asclParamId','biomarkername')


code_map <- as.data.table(code_map)
#lab <- merge(lab,code_map,by.x='asclParamId',by.y='biomarkercode')



investigation_details_tp <- tp_lab[,.(patient_id = as.character(patientid),investigation_date = as.Date(bookingdate),biomarkername=tolower(biomarkername),value = as.numeric(valuetext))]
investigation_details_tp <- investigation_details_tp[!is.na(value),]

investigation_details_tp_presc <- emr_tp[level1_n == "investigationMonitoring", .(patient_id = patientid, investigation_date = as.Date(bookingdate.x), biomarkername = level2, valuetext = value)]
investigation_details_tp_presc <- investigation_details_tp_presc[biomarkername %in% c('HBA1C','HB A 1C'), biomarkername := 'HBA1C']
investigation_details_tp_presc <- investigation_details_tp_presc[biomarkername == 'HBA1C', valuetext :=  gsub("\\s+", "", str_trim(gsub('%','',valuetext)))]
investigation_details_tp_presc <- investigation_details_tp_presc[,value:=as.numeric(valuetext)]
investigation_details_tp_presc <- investigation_details_tp_presc[!is.na(value),]
investigation_details_tp_presc <- investigation_details_tp_presc[,.(patient_id , investigation_date, biomarkername ,  value)]


investigation_details_tp[,source := 'tp']
investigation_details_tp_header <- unique(investigation_details_tp[,.(patient_id,investigation_date,source)])
investigation_details_tp_header[,investigation_id := 1:.N]
investigation_details_tp_header[,investigation_id := paste(source,investigation_id,sep='_')]

investigation_details_lifespan <- investigation_details[,.(patient_id, investigation_date, biomarkername, value)]
investigation_details_lifespan[,source := 'lifespan']
investigation_details_lifespan <- investigation_details_lifespan[value!='',]
investigation_details_lifespan_header <- unique(investigation_details_lifespan[,.(patient_id,investigation_date,source)])
investigation_details_lifespan_header[,investigation_id := 1:.N]
investigation_details_lifespan_header[,investigation_id := paste(source,investigation_id,sep='_')]

investigation_details_ascl <- lab[,.(patient_id = uhid, investigation_date = testDate, biomarkername, value = asclresult)]
investigation_details_ascl[,source := 'ascl']
investigation_details_ascl_header <- unique(investigation_details_ascl[,.(patient_id,investigation_date,source)])
investigation_details_ascl_header[,investigation_id := 1:.N]
investigation_details_ascl_header[,investigation_id := paste(source,investigation_id,sep='_')]

investigation_all <- rbind(investigation_details_lifespan,investigation_details_ascl,investigation_details_tp)
investigation_all[,patient_id := paste(source,patient_id,sep='_')]
investigation_header <- rbind(investigation_details_tp_header,investigation_details_lifespan_header,investigation_details_ascl_header)
investigation_header[,patient_id := paste(source,patient_id,sep='_')]

investigation_all <- merge(investigation_all,investigation_header,by=c('patient_id','investigation_date','source'))

investigation_all <- investigation_all[,head(.SD,1),.(investigation_id,biomarkername)]
investigation_all[,value := as.numeric(value)]

investigation_all <- merge(investigation_all,code_map,by=c('biomarkername','source'))
investigation_all <- investigation_all[!value < min_value]
investigation_all <- investigation_all[!value > max_value]
setnames(investigation_all,'unified_name','biomarker')
investigation_all <- investigation_all[value > max_normal_value,comorbidity := paste('high',biomarker,sep='_')]
investigation_all <- investigation_all[value < min_normal_value,comorbidity := paste('low',biomarker,sep='_')]

comorbidity_final <- investigation_all[!is.na(comorbidity),.(patient_id,investigation_id,comorbidity,investigation_date)]
comorbidity_patient_date <- unique(comorbidity_final[,.(patient_id,investigation_date)])[order(patient_id,-investigation_date)]
comorbidity_patient_date <- comorbidity_patient_date[,head(.SD,1),patient_id]
comorbidity_latest <- merge(comorbidity_patient_date,comorbidity_final,by=c('patient_id','investigation_date'))
comorbidity_final_csv <- comorbidity_latest[,.(comorbidity = toString(comorbidity)),.(patient_id)]
comorbidity_final <- comorbidity_final[,.(investigation_id,comorbidity)]

investigation_latest <- investigation_all[,.(patient_id,investigation_date,biomarker,value)]
investigation_latest <- investigation_latest[order(patient_id,biomarker,-investigation_date)]
investigation_latest <- investigation_latest[,head(.SD,1),.(patient_id,biomarker)]
investigation_latest <- dcast.data.table(investigation_latest,patient_id ~ biomarker,value.var = 'value')

investigation_all <- dcast.data.table(investigation_all,investigation_id ~ biomarker,value.var = 'value')



######## Diagnosis updating ##############
###############  by drug purpose   #######################

drug_purpose<-as.data.table(drug_purpose)
drug_purpose<-drug_purpose[!is.na(purpose)]


diagnosis_map_purpose<-merge(merge(prescription_header_latest,prescription_brand_molecules, by ='pph_id'), prescription_header, by='pph_id')
diagnosis_map_purpose<-unique(diagnosis_map_purpose)
diagnosis_map_purpose<-merge(diagnosis_map_purpose,drug_purpose, by='purpose', all.x = T)
diagnosis_map_purpose<-diagnosis_map_purpose[!is.na(Diagnosis)]
diagnosis_map_purpose<-diagnosis_map_purpose[,.(patient_id,diagnosis=Diagnosis,date=created_date)]
diagnosis_map_purpose<-diagnosis_map_purpose[order(patient_id,date)]
diagnosis_map_purpose[,dia_source:='curr_treatment']
#diagnosis_map_purpose<-diagnosis_map_purpose[,.N,.(patient_id,diagnosis)]
#diagnosis_map_purpose[,N:=NULL]
diagnosis_latest<-rbind(diagnosis_latest,diagnosis_map_purpose)


###############  by variable labvalues   #######################

diagnosis_labvalue <- gs_title('Copy of drug_master')
diagnosis_labvalue <- diagnosis_labvalue %>% gs_read(ws='diagnosis_labvalue',range = cell_cols(1:4))
diagnosis_labvalue<-as.data.table(diagnosis_labvalue)
diagnosis_labvalue<-diagnosis_labvalue[!is.na(variable)]

diagnosis_map_labvalue<-merge(medical_history_all,medical_history_header, by ='history_id')
diagnosis_map_labvalue<-diagnosis_map_labvalue[,.(patient_id,date=history_date,variable=vital,value)]
diagnosis_map_labvalue<-diagnosis_map_labvalue[!(variable=='bmi' & (as.numeric(value) <10 | as.numeric(value) >50))]


diagnosis_map_labvalue_biomarker <- rbind(investigation_details_lifespan,investigation_details_ascl,investigation_details_tp)
diagnosis_map_labvalue_biomarker[,patient_id := paste(source,patient_id,sep='_')]
investigation_header <- rbind(investigation_details_tp_header,investigation_details_lifespan_header,investigation_details_ascl_header)
investigation_header[,patient_id := paste(source,patient_id,sep='_')]

diagnosis_map_labvalue_biomarker <- merge(diagnosis_map_labvalue_biomarker,investigation_header,by=c('patient_id','investigation_date','source'))

diagnosis_map_labvalue_biomarker <- diagnosis_map_labvalue_biomarker[,head(.SD,1),.(investigation_id,biomarkername)]
diagnosis_map_labvalue_biomarker[,value := as.numeric(value)]

diagnosis_map_labvalue_biomarker <- merge(diagnosis_map_labvalue_biomarker,code_map,by=c('biomarkername','source'))

diagnosis_map_labvalue_biomarker <- diagnosis_map_labvalue_biomarker[!value < min_value]
diagnosis_map_labvalue_biomarker <- diagnosis_map_labvalue_biomarker[!value > max_value]
setnames(diagnosis_map_labvalue_biomarker,'unified_name','biomarker')
diagnosis_map_labvalue_biomarker<-diagnosis_map_labvalue_biomarker[,.(biomarker,value,date=investigation_date,investigation_id)]
diagnosis_map_labvalue_biomarker<-merge(diagnosis_map_labvalue_biomarker,investigation_header, by='investigation_id')
diagnosis_map_labvalue_biomarker[,investigation_date:=NULL]
diagnosis_map_labvalue_biomarker<-diagnosis_map_labvalue_biomarker[,.(patient_id,date,variable=biomarker,value)]
diagnosis_map_labvalue_all<-rbind(diagnosis_map_labvalue,diagnosis_map_labvalue_biomarker)
diagnosis_map_labvalue_all<-merge(diagnosis_map_labvalue_all,diagnosis_labvalue, by='variable')
diagnosis_map_labvalue_all<-diagnosis_map_labvalue_all[value>max_lab_value]
diagnosis_map_labvalue_all<-unique(diagnosis_map_labvalue_all[,.(patient_id, diagnosis,date)])
diagnosis_map_labvalue_all[,dia_source:='lab_value']
diagnosis_latest<-rbind(diagnosis_latest, diagnosis_map_labvalue_all)
diagnosis_latest<-unique(diagnosis_latest)
diagnosis_latest <- diagnosis_latest[order(patient_id,dia_source,-date,diagnosis)]
diagnosis_uni<-diagnosis_latest
#diagnosis_uni<-diagnosis_uni[order(patient_id,diagnosis)]
#diagnosis_uni2<-diagnosis_uni[,.(diagnosis=toString(diagnosis)),.(patient_id)]
diagnosis_latest <- diagnosis_latest[,.(diagnosis = toString(diagnosis)),.(patient_id,date,dia_source)]
diagnosis_latest <- diagnosis_latest[order(patient_id,dia_source,-date)]
diagnosis_latest <-  diagnosis_latest[,visit_no:=1:.N,.(patient_id,dia_source)]
diagnosis_latest<-diagnosis_latest[grepl('retinopathy|neuropathy',tolower(diagnosis)) | visit_no==1]


diagnosis_latest2<-cSplit(diagnosis_latest,'diagnosis',sep = ',',direction = 'long')
#diagnosis_latest2[,c('date','dia_source'):=NULL]
diagnosis_latest2<-unique(diagnosis_latest2)
diagnosis_latest_long<-diagnosis_latest2
setnames(diagnosis_latest_long,c('diagnosis','visit_no'),c('variable','value'))
diagnosis_latest2 <- diagnosis_latest2[,.(diagnosis = toString(variable)),.(patient_id)]
diagnosis_latest<-diagnosis_latest2


########################################




prescription_investigation <- merge(prescription_header,investigation_header,by='patient_id',allow.cartesian=TRUE)
prescription_investigation <- prescription_investigation[,diff_dates := abs(as.numeric(created_date-investigation_date))]
prescription_investigation <- prescription_investigation[diff_dates < 31]
prescription_investigation <- prescription_investigation[,.(patient_id,pph_id,investigation_id,diff_dates)]
prescription_investigation <- prescription_investigation[order(patient_id,pph_id,diff_dates)]
prescription_investigation <- prescription_investigation[,head(.SD,1),.(patient_id,pph_id)]
prescription_investigation <- prescription_investigation[,diff_dates := NULL]

prescription_history <- merge(prescription_header,medical_history_header,by='patient_id',allow.cartesian=TRUE)
prescription_history <- prescription_history[,diff_dates := abs(as.numeric(created_date-history_date))]
prescription_history <- prescription_history[diff_dates < 31]
prescription_history <- prescription_history[,.(patient_id,pph_id,history_id,diff_dates)]
prescription_history <- prescription_history[order(patient_id,pph_id,diff_dates)]
prescription_history <- prescription_history[,head(.SD,1),.(patient_id,pph_id)]
prescription_history <- prescription_history[,diff_dates := NULL]

prescription_investigation_history <- merge(prescription_header, prescription_investigation,by=c('patient_id','pph_id'),all.x=TRUE)
prescription_investigation_history <- merge(prescription_investigation_history,prescription_history,by=c('patient_id','pph_id'),all.x=TRUE)
prescription_investigation_history <- prescription_investigation_history[order(patient_id,created_date)]
prescription_investigation_history <- prescription_investigation_history[,visit := 1:.N,patient_id]
prescription_investigation_history <- prescription_investigation_history[,.(patient_id,pph_id,investigation_id,history_id,visit)]

prescription_investigation_history <- merge(prescription_investigation_history,investigation_all,by='investigation_id',all.x=TRUE)

##############################

medical_history_all_numeric <- medical_history_all[vital %in% c('bmi','weight','height','duration_diabetes','year_diabetes_diagnosis','bp_systolic','bp_diastolic','waist')]
medical_history_all_text <- medical_history_all[!vital %in% c('bmi','weight','height','duration_diabetes','year_diabetes_diagnosis','bp_systolic','bp_diastolic','waist')]

medical_history_all_numeric <- medical_history_all_numeric[,value := as.numeric(value)]
medical_history_all_numeric <- medical_history_all_numeric[!is.na(value) & !value < min_value]
medical_history_all_numeric <- medical_history_all_numeric[!value > max_value]

medical_history_unique <- medical_history_all_numeric[unique == 'yes']
medical_history_unique <- merge(medical_history_unique,medical_history_header,by='history_id')
medical_history_unique <- medical_history_unique[order(patient_id,vital,-history_date)]
medical_history_unique <- medical_history_unique[,head(.SD,1),.(patient_id,vital)]
medical_history_unique <- dcast.data.table(medical_history_unique,patient_id ~ vital,value.var = 'value')
medical_history_unique <- medical_history_unique[,bmi := ifelse(is.na(height)|is.na(weight),bmi, (weight * 10000) / (height * height))]
medical_history_unique <- medical_history_unique[,bmi := ifelse(!is.na(bmi) & (bmi < 12 | bmi > 60), NA, bmi)]

medical_history_all_numeric <- dcast.data.table(medical_history_all_numeric,history_id ~ vital,value.var = 'value')
medical_history_all_text <- dcast.data.table(medical_history_all_text,history_id ~ vital,value.var = 'value')

medical_history_all1 <- merge(medical_history_all_text,medical_history_all_numeric,by='history_id',all=TRUE)

prescription_investigation_history <- merge(prescription_investigation_history,medical_history_unique,by='patient_id',all.x=TRUE)
prescription_investigation_history <- merge(prescription_investigation_history,brand_switching_antidiabetic,by='patient_id',all.x=TRUE)
prescription_investigation_history <- merge(prescription_investigation_history,prescription_header,by=c('patient_id','pph_id'))
prescription_investigation_history <- prescription_investigation_history[is.na(date_changed),continued_drug := TRUE]
prescription_investigation_history <- prescription_investigation_history[!is.na(date_changed) & created_date > date_changed,continued_drug := FALSE]
prescription_investigation_history <- prescription_investigation_history[!is.na(date_changed) & created_date <= date_changed,continued_drug := TRUE]
prescription_investigation_history <- prescription_investigation_history[order(patient_id,created_date)]
prescription_investigation_history <- prescription_investigation_history[,hba1c_ff := na.locf(hba1c,na.rm=FALSE)]
prescription_investigation_history <- prescription_investigation_history[,hba1c_ff_change := hba1c_ff - data.table::shift(hba1c_ff),patient_id]
prescription_investigation_history <- prescription_investigation_history[,visit_diff := created_date - data.table::shift(created_date),patient_id]
prescription_investigation_history <- prescription_investigation_history[,visit_diff := as.numeric(visit_diff)]
prescription_investigation_history <- prescription_investigation_history[is.na(visit_diff),visit_diff := 0]
prescription_investigation_history <- prescription_investigation_history[,cum_diff := cumsum(visit_diff),patient_id]

drug_efficacy <- prescription_investigation_history[patient_id %in% prescription_investigation_history[cum_diff %between% c(90,180),patient_id]]
drug_efficacy <- drug_efficacy[,.(total_hba1c_change = sum(hba1c_ff_change,na.rm=T)),patient_id]
prescription_investigation_history <- merge(prescription_investigation_history,drug_efficacy,by='patient_id',all.x=TRUE)


state_region_map <- as.data.table(state_region_map)
patient_all<-patient_all[,head(.SD,1),.(patient_id)]
in_latest<-merge(investigation_latest,patient_all, by='patient_id')
in_latest[gender=='female',egfr:=186*((creatnine)^(-1.154))*((age)^(-0.203))*0.742]
in_latest[gender!='female',egfr:=186*((creatnine)^(-1.154))*((age)^(-0.203))]
investigation_latest<-in_latest


medical_history_header_latest<-medical_history_header[order(patient_id,-history_date)]
medical_history_header_latest<-medical_history_header_latest[,head(.SD,1),.(patient_id)]
#################################

#setwd("~/Workspace/Output")

setwd('E:\\girdhar\\Lifespan_Dia\\latest\\output')

not_t2<-diagnosis_latest_long[variable=='t1dm']
not_t2[,c('variable','value'):=NULL]
not_t2_pph<-merge(prescription_header, prescription_header_latest,by='pph_id')
not_t2_pph<-merge(not_t2,not_t2_pph,by='patient_id')
not_t2_pph[,c('patient_id','created_date','source','doctor_name'):=NULL]


#dia_id<-unique(merge(prescription_brand_molecules[purpose=='antidiabetic'],prescription_header, by='pph_id')[,(pph_id)])
dia_id<-unique(merge(prescription_brand_molecules[purpose=='antidiabetic'],prescription_header, by='pph_id' )[,patient_id])
dia_id<-as.data.table(dia_id)
setnames(dia_id,'dia_id','patient_id')
#dia_id<-merge(prescription_header,dia_id,by='pph_id')
dia_id<-dia_id[,.N,patient_id][,N:=NULL]
dia_id<-dia_id[patient_id %ni% not_t2[,patient_id]]

prescription_header_latest<-unique(merge(prescription_brand_molecules[purpose=='antidiabetic'],prescription_header_latest, by='pph_id')[,.(pph_id,Doctor_Category,`Diabetes Mapping_Abbott_11june19`,RSSDI)])
prescription_header_latest<-as.data.table(prescription_header_latest)
#setnames(prescription_header_latest,'prescription_header_latest','pph_id')
prescription_header_latest<-prescription_header_latest[pph_id %ni% not_t2_pph[,pph_id]]


setwd('E:\\girdhar\\Lifespan_Dia\\latest')
dosages_insulin<-dosages[grepl('insulin',molecule)]
dosages_insulin<-merge(dosages_insulin,prescription_header_latest, by='pph_id')
dosages_insulin<-dosages_insulin[,freq:=frequency_id]
all_long<-dosages_insulin
all_long[,dosage:=str_remove_all(dosage,'tablets|tablet|tab')]
all_long[,dosage:=str_remove_all(dosage,'\\(|\\)|inj|cartridge')]
all_long[,dosage:=str_squish(dosage)]
all_long[,dosage:=str_remove_all(dosage,'-')]
all_long[,freq:=str_replace_all(freq,'--','-')]
all_long[,freq:=str_replace_all(freq,'--','-')]
all_long[,freq:=str_replace_all(freq,'--','-')]
all_long[,freq:=str_replace_all(freq,'--','-')]
all_long[,freq:=str_replace_all(freq,'--','-')]
all_long[,freq:=str_replace_all(freq,'--','-')]

all_long[,freq:=str_remove_all(freq,'units|unit')]
all_long[str_sub(freq,-1)=='.',freq:=str_sub(freq,0,-2)]
all_long$freq<-as.factor(all_long$freq)
freq_clean<-fread('freq_for_dosages.csv')
freq_clean<-freq_clean[order(level)]
freq_clean<-freq_clean[level %in% unlist(levels(all_long$freq)[levels(all_long$freq) %in% c(unlist(freq_clean$level))])]

levels(all_long$freq)[levels(all_long$freq) %in% c(unlist(freq_clean$level))]<-freq_clean$New_level
all_long[,freq:=as.character(freq)]
all_long[,raw_freq:=freq]
a<-cSplit(all_long, 'freq','-',direction = 'wide')
all_long<-a
all_long[,freq_6:=NULL]
all_long[,freq_7:=NULL]
all_long[,freq_8:=NULL]
all_long[,freq_5:=NULL]

all_long[freq_1 %in%  c('[ab]','ab]','[ab'),freq_1_method:='after_breakfast'][freq_1 %in%  c('[ab]','ab]','[ab'),freq_1:=dosage_num]
all_long[freq_1 %in%  c('[bb]','bb]','[bb'),freq_1_method:='before_breakfast'][freq_1 %in%  c('[bb]','bb]','[bb'),freq_1:=dosage_num]
all_long[freq_1 %in%  c('[wb]','wb]','[wb'),freq_1_method:='with_breakfast'][freq_1 %in%  c('[wb]','wb]','[wb'),freq_1:=dosage_num]
all_long[freq_1 %in%  c('[em]','em]','[em'),freq_1_method:='early_morning'][freq_1 %in%  c('[em]','em]','[em'),freq_1:=dosage_num]


all_long[freq_1 %in% c(']','am') & dosage_num>1,freq_1:=dosage_num]

all_long<-sapply(all_long, as.character)
all_long<-as.data.table(all_long)


all_long_raw<-all_long
dosages_lifespan<-all_long[source=='lifespan']
dosages_lifespan<-cSplit(dosages_lifespan,'comment_dosages','~',direction = 'wide')
dosages_lifespan[,':='(freq_1=comment_dosages_1,freq_2=comment_dosages_2,freq_3=comment_dosages_3,freq_4=comment_dosages_4,freq_1_method=comment_dosages_5,freq_2_method=comment_dosages_6,freq_3_method=comment_dosages_7)]
dosages_lifespan[,c('comment_dosages_1','comment_dosages_2','comment_dosages_3','comment_dosages_4','comment_dosages_5','comment_dosages_6','comment_dosages_7'):=NULL]
all_long_raw<-all_long_raw[source!='lifespan']
all_long_raw[,comment_dosages:=NULL]
all_long_raw<-rbind(all_long_raw[,':='(freq_2_method='',freq_3_method='')],dosages_lifespan)

a<-all_long_raw[!is.na(freq_1)]
#a[freq_1 %in% c(']','am'),freq_1:=1]
#a<-all_long_raw[!is.na(freq_1)]
a[,freq_1:=as.numeric(as.character(freq_1))]
a[,freq_2:=as.numeric(as.character(freq_2))]
a[,freq_3:=as.numeric(as.character(freq_3))]
a[,freq_4:=as.numeric(as.character(freq_4))]
a[is.na(freq_4),freq_4:=0]
a[is.numeric(dosage_num) & dosage_num!='0' & (freq_1==1 |freq_2==1 |freq_3==1 |freq_4==1),`:=`(freq_1=freq_1*dosage_num,freq_2=freq_2*dosage_num,freq_3=freq_3*dosage_num,freq_4=freq_4*dosage_num)]
a<-a[!is.na(freq_1)]
a<-a[!is.na(freq_2)]
a<-a[!is.na(freq_3)]

a<-a[freq_4!=1]
a<-a[freq_3!=1]
a<-a[freq_2!=1]
a<-a[freq_1!=1]

a[,total_dosages:=freq_1+freq_2+freq_3+freq_4]
a<-a[total_dosages<1000]
#write.csv(a,'insulin_latest.csv')
a[,drug:=NULL]
a<-unique(a)

a[,visit_no:=1:.N,.(patient_id,clean_brand)]
insulin_count<-a[,.N,.(patient_id)][order(-N)]
a<-as.data.table(a)
a[,f_all:=paste('1',freq_1,freq_2,freq_3,freq_4,sep = '~')]
a[,dosage_count:=(4-str_count(f_all,'~0'))]
ad<-merge(diagnosis_latest,a,by='patient_id')

a_id<-a[,.N,patient_id]
a_id[,N:=NULL]

a_other_drugs<-merge(prescription_brand_molecules,prescription_header_latest, by='pph_id')
a_other_drugs<-merge(a_other_drugs,prescription_header, by='pph_id')
#a_other_drugs<-merge(a_other_drugs,a_id, by='patient_id')
antidia<-a_other_drugs
a_other_drugs<-a_other_drugs[,.N,.(patient_id,clean_brand,purpose)][,N:=NULL]
a_other_drugs<-a_other_drugs[,.N,.(patient_id,purpose)]
antidia<-antidia[purpose=='antidiabetic']
antidia<-unique(antidia[,.(patient_id,molecule)])
antidia[,molecule_count:=1:.N,patient_id]
antidia[,molecule_count_total:=max(molecule_count),patient_id]

antidia[grepl('insulin',molecule, ignore.case = T),insulin_status:=1]
antidia[is.na(insulin_status),insulin_status:=0]
antidia[insulin_status==1,insulin_status_count:=1:.N,patient_id]
antidia[is.na(insulin_status_count),insulin_status_count:=0]
antidia[,insulin_count_total:=max(insulin_status_count),patient_id]
antidia[insulin_status==0,oha_counts:=1:.N,patient_id]
antidia[is.na(oha_counts),oha_counts:=0]
antidia[,oha_total:=max(oha_counts),patient_id]
antidia[insulin_count_total>0 & oha_total==0, insulin_therapy:="Insulin Only"]
#antidia[insulin_count_total>1 & oha_total==0, insulin_therapy:="Combination Insulin"]
antidia[insulin_count_total>0 & oha_total!=0, insulin_therapy:="Insulin with OHA"]
#antidia[insulin_count_total>1 & oha_total!=0, insulin_therapy:="Combination Insulin with OHA"]
antidia[is.na(insulin_therapy),insulin_therapy:='OHA only']

insulin_acting <- gs_title('Copy of drug_master')
insulin_acting <- insulin_acting %>% gs_read(ws='insulin', range = cell_cols(1:3))
insulin_acting<-as.data.table(insulin_acting)
a<-merge(insulin_acting,a,by='clean_brand',all.y = T)

latest_brand_molecule<-merge(prescription_brand_molecules,prescription_header_latest,by='pph_id')
latest_brand_molecule<-merge(latest_brand_molecule,prescription_header,by='pph_id')

setwd('E:\\girdhar\\Lifespan_Dia\\latest\\output')


dia_0<-prescription_investigation_history[duration_diabetes=='0',.N,patient_id][,N:=NULL]
dia_0<-as.data.table(dia_0)

dia_0<-merge(dia_0,merge(prescription_brand_molecules[purpose=='antidiabetic'],prescription_header,by='pph_id'),by='patient_id')
dia_0<-dia_0[order(patient_id,created_date,molecule)]
dia_0[,molecule:=toString(molecule),.(patient_id,pph_id)]
dia_0<-unique(dia_0[,.(patient_id,molecule,pph_id,created_date)])
dia_0_1<-dia_0[,head(.SD,1),patient_id]
dia_0[,change:=mapply(grepl,molecule,data.table::shift(molecule)),.(patient_id)]
dia_0<-dia_0[pph_id %ni% dia_0_1[,pph_id]]
dia_0_2<-dia_0[change=='FALSE',head(.SD,1),patient_id]

dia_0_1[,molecule_1:=molecule]
dia_0_2[,molecule_2:=molecule]
dia_0_1<-merge(dia_0_1[,.(patient_id,pph_id,molecule_1)],prescription_brand_molecules,by='pph_id')
dia_0_2<-merge(dia_0_2[,.(patient_id,pph_id,molecule_2)],prescription_brand_molecules,by='pph_id')

write.csv(dia_0_1[patient_id %in% dia_id$patient_id],'1st_line_0yr.csv')
write.csv(dia_0_2[patient_id %in% dia_id$patient_id],'2st_line_0yr.csv')

trip<-prescription_investigation_history
trip[,year:=year(created_date)]
trip[,total_year_count:=.N,.(patient_id,year)]
trip2<-trip[,.N,.(year,total_year_count,patient_id)][,.N,.(total_year_count,year)][,mean(N),.(total_year_count)]

write.csv(trip2,'trip2.csv',na = '')
write.csv(trip,'trip.csv',na = '')

prescription_investigation_history<-prescription_investigation_history[pph_id!='' & !is.na(pph_id)]
prescription_investigation_history[,Doctor_Category:=str_replace_all(Doctor_Category,'[^[:alnum:]]','')]
prescription_investigation_history[,`Diabetes Mapping_Abbott_11june19`:=str_replace_all(`Diabetes Mapping_Abbott_11june19`,'[^[:alnum:]]','')]

prescription_investigation_history[,duration_diabetes:=as.character(duration_diabetes)]
prescription_investigation_history[is.na(duration_diabetes),duration_diabetes:='unknown']


exer<-emr[grepl('exercise|diet|walk|cycle|garden|gym|physicalactivity|treadmill|yoga',value,ignore.case = T)]
exer[,patient_id:=paste('ascl_',uhid,sep = '')]
exer[grepl('no exercise|no physic',value,ignore.case = T),exercise:='no']
exer[is.na(exercise) & grepl('exercise|walk|cycle|garden|gym|physicalactivity|treadmill|yoga',value,ignore.case = T),exercise:='yes']

exer[grepl('no diet|not following diet',value,ignore.case = T),diet:='bad']
exer[is.na(diet) & grepl('following diet|follow diet|diet control',value,ignore.case = T),diet:='good']


diag_count<-diagnosis_latest_long[variable %ni% c('t1dm','t2dm')]
diag_count<-diag_count[,.N,.(patient_id,variable)][,.N,patient_id]
prescription_header<-merge(prescription_header,diag_count,by='patient_id',all.x = T)
prescription_header[,diagnosis_s:=N]
prescription_header[is.na(N),diagnosis_s:=0]

write.csv(merge(antidia,dia_id,by='patient_id'),'combi_therapy.csv')
write.csv(merge(latest_brand_molecule[,.N,.(purpose,patient_id)],dia_id,by='patient_id'),'patient_latest_theraphy_molecule.csv')

write.csv(merge(patient_all,dia_id,by='patient_id'),'db_patient.csv',row.names=F,na='')
write.csv(merge(prescription_header,dia_id,by='patient_id'),'db_prescription_header.csv',row.names=F,na='')
write.csv(merge(medical_history_header,dia_id,by='patient_id'),'db_medical_history_header.csv',row.names=F,na='')
write.csv(merge(merge(medical_history_all1,medical_history_header_latest, by='history_id'),dia_id,by='patient_id'),'db_medical_history.csv',row.names=F,na='')
write.csv(complaints,'db_complaints_final.csv',row.names=F,na='')
write.csv(complaints_final_csv,'db_complaints_final_unique.csv',row.names=F,na='')
# write.csv(diagnosis_final,'db_diagnosis.csv',row.names=F,na='')
#write.csv(prescription_others,'db_prescription_others.csv',row.names=F,na='')
write.csv(brand_switching,'db_brand_switching.csv',row.names=F,na='')
write.csv(brand_switching_dcast,'db_brand_switching_dcast.csv',row.names=F,na='')
write.csv(molecule_switching,'db_molecule_switching.csv',row.names=F,na='')
write.csv(molecule_switching_dcast,'db_molecule_switching_dcast.csv',row.names=F,na='')
write.csv(merge(investigation_header,dia_id,by='patient_id'),'db_investigation_header.csv',row.names=F,na='')
write.csv(investigation_all,'db_investigation.csv',row.names=F,na='')
write.csv(comorbidity_final,'db_comorbidity_final.csv',row.names=F,na='')
write.csv(comorbidity_final_csv,'db_comorbidity_final_unique.csv',row.names=F,na='')
write.csv(merge(investigation_latest,dia_id,by='patient_id'),'db_investigation_latest.csv',row.names=F,na='')
write.csv(medical_history_unique,'db_medical_history_unique.csv',row.names=F,na='')
write.csv(prescription_investigation_history,'db_prescription_investigation_history.csv',row.names = F,na='')
write.csv(merge(complaints_latest,dia_id,by='patient_id'),'db_complaints_latest.csv',row.names=F,na='')
write.csv(merge(diagnosis_latest,dia_id,by='patient_id'),'db_diagnosis_latest.csv',row.names=F,na='')
write.csv(merge(comorbidity_latest,dia_id,by='patient_id'),'db_comorbidity_latest.csv',row.names=F,na='') 
write.csv(prescription_all_latest,'db_prescription_all_latest.csv',row.names=F,na='')
write.csv(prescription_molecules_latest,'db_prescription_molecules_latest.csv',row.names=F,na='')
write.csv(prescription_all_csv,'db_prescription_all_unique.csv',row.names=F,na='')
write.csv(prescription_molecules_csv,'db_prescription_molecules_unique.csv',row.names=F,na='')
write.csv(merge(prescription_brand_molecules,prescription_header_latest,by='pph_id'),'db_prescription_brand_molecules.csv',row.names=F,na='')
write.csv(state_region_map,'db_state_region.csv',row.names=F,na='')
write.csv(drug_composition,'db_drug_composition.csv', row.names = F, na='')
write.csv(brand_switching_molecule, 'db_brand_switching_molecule.csv', row.names = F, na='')

write.csv(class_switching_dcast, 'db_class_switching_dcast.csv', row.names = F, na='')
write.csv(class_switching, 'db_class_switching.csv', row.names = F, na='')


write.csv(diagnosis_latest_long, 'diagnosis_long.csv', row.names = F, na='')
write.csv(diagnosis_latest_long[,diagnosis:=variable],'db_diagnosis_unique.csv',row.names=F,na='')
write.csv(net_gain_cat,'net_gain_category.csv')
write.csv(net_gain_mol,'net_gain_molecule.csv')
setwd('E:\\girdhar\\Lifespan_Dia\\latest')
getwd()
  
pac_ascl<-read.csv('Apollo Sugar - Package Data (1).csv')
pac_ascl<-as.data.table(pac_ascl)
pac_ascl[,billdate:=as.Date(billdate,format = "%m/%d/%Y")]

pac_ascl[,pac_end_date:=billdate+days]
pac_ascl[,patient_id:=paste('ascl_',uhid,sep = '')]

inv_pac<-merge(investigation_all,investigation_header, by='investigation_id')
pac_ascl<- merge(pac_ascl,inv_pac,by = 'patient_id')



pac_ascl[,pre_pac_days:=billdate-investigation_date]
pac_ascl[,pre_pac_days:=as.numeric(pre_pac_days)]
pac_ascl[pre_pac_days<181 & pre_pac_days>-5,status:='pre']
pac_ascl[between(pre_pac_days,-365,-20),status:='post']
setwd('E:\\girdhar\\Lifespan_Dia\\latest\\output')

pac_ascl_pre<-pac_ascl[status=='pre']
pac_ascl_post<-pac_ascl[status=='post']

pac_ascl_done<-merge(pac_ascl_pre,pac_ascl_post,by=c('patient_id','billdate','packagename'))

patient_all[patient_id %in% pac_ascl[,patient_id],pac_status:='yes']
patient_all[is.na(pac_status),pac_status:='no']



pac_ascl_temp<-pac_ascl[,.N,.(patient_id,billno,billdate,packagename,days)]
pac_ascl_temp<-pac_ascl_temp[order(patient_id,packagename,billdate)]
pac_ascl_temp<-pac_ascl_temp[,head(.SD,1),.(patient_id,packagename,billno)]
pac_ascl_temp[,start_date:=billdate]
pac_ascl_temp[,billdate:=NULL]
pac_ascl_temp[,N:=NULL]
pac_ascl_temp[,end_date:=start_date+days]

pac_ascl<-merge(pac_ascl,pac_ascl_temp,by=c('patient_id','packagename','billno','days'))

pac_ascl_temp<-pac_ascl[,.N,.(patient_id,billno,billdate,packagename,days)]
pac_ascl_temp<-pac_ascl_temp[,.N,.(patient_id,billno,packagename)]
pac_ascl_temp[,tot_visit:=N]
pac_ascl_temp[,N:=NULL]

pac_ascl<-merge(pac_ascl,pac_ascl_temp,by=c('patient_id','packagename','billno'))

write.csv(pac_ascl,'pac_ascl.csv')
####   pac2   ######################

pac_ascl<-read.csv('Apollo Sugar - Package Data (1).csv')
pac_ascl<-as.data.table(pac_ascl)
pac_ascl[,billdate:=as.Date(billdate,format = "%m/%d/%Y")]
pac_ascl[,patient_id:=paste('ascl_',uhid,sep = '')]
#pac_ascl<-pac_ascl[packagename!='Sugar 1']
pac_ascl<-pac_ascl[order(patient_id,packagename,billdate)]


pac_ascl1<-pac_ascl[,head(.SD,1),.(patient_id)]
pac_ascl1[,end_date:=days+billdate]
pac_ascl1<-merge(pac_ascl,pac_ascl1[,.(patient_id,end_date)],by='patient_id')
pac_ascl1[,one_diff:=end_date-billdate]
pac_ascl1[,visit_c:=1:.N,patient_id]
pac_ascl1[visit_c==1,new:=1]
pac_ascl1[new==1,pac_diff:=one_diff-days]
pac_ascl1[new==1,one_diff:=one_diff-days]




pac_ascl2<-pac_ascl1[is.na(new) & one_diff<0,head(.SD,1),.(patient_id)]
pac_ascl2[,end_date:=days+billdate]
pac_ascl2[,status_new:=one_diff]
pac_ascl2<-merge(pac_ascl1[,.(patient_id,billno,billdate,packagename,days,visit_c,new,pac_diff)],pac_ascl2[,.(patient_id,end_date,status_new)],by='patient_id',all.x = T)
pac_ascl2[,one_diff:=end_date-billdate]
pac_ascl2[one_diff==days,new:=1]
pac_ascl2[new==1 & is.na(pac_diff),pac_diff:=status_new]
pac_ascl2[new==1,one_diff:=one_diff-days]


pac_ascl1<-pac_ascl2

pac_ascl1[,status_new:=NULL]
pac_ascl1[,one_diff:=NULL]
pac_ascl1[,end_date:=NULL]

write.csv(unique(pac_ascl1[,`:=`(total=.N,mean_diff=mean(pac_diff)),.(visit_c,new,packagename)][,.(total,mean_diff,visit_c,new,packagename)]),'pac_ascl2.csv',na = '')

pac_ascl1<-pac_ascl1[new==1][order(patient_id,billdate)]
pac_ascl1[,last_pac:=data.table::shift(packagename),patient_id]
write.csv(pac_ascl1[,.(.N,mean(pac_diff)),.(last_pac,packagename)],'packageb.csv')



pac_ascl<-merge(pac_ascl,prescription_header,by='patient_id',all.y = T)
pac_ascl[,year_p:=year(as.Date(billdate))]
pac_ascl[,year_r:=year(created_date)]
write.csv(pac_ascl[,.N,.(patient_id,year_p,packagename)][,.N,.(year_p,packagename)],'pac_year.csv')
write.csv(pac_ascl[,.N,.(patient_id,year_r)][,.N,.(year_r)],'all_pac.csv')
#### compliance ############

pac_ascl1<-pac_ascl1[new==1 & packagename!='Sugar 1']
pac_ascl1[,bill_end:=billdate+days]
pac_ascl1<-merge(pac_ascl1,prescription_header[,.(patient_id,created_date,`Diabetes Mapping_Abbott_11june19`)],by='patient_id')
pac_ascl1[,pac_visit:=ifelse(created_date>billdate & created_date<bill_end,'1','0')]
#pac_ascl1<-pac_ascl1[pac_visit==1]
#pac_ascl1<-pac_ascl1[created_date!=billdate]
pac_ascl1[pac_visit==1,tot_pac_visit:=.N,.(patient_id,packagename)]
pac_ascl1[is.na(tot_pac_visit),tot_pac_visit:=0]
pac_ascl1[,tot_pac_visit:=tot_pac_visit+1]
write.csv(pac_ascl1[,.N,.(packagename,tot_pac_visit,patient_id)][,.N,.(packagename,tot_pac_visit)],'compliance_all.csv')

##### compliance lab ########################

pac_ascl1[,packagename:=tolower(packagename)]
pac_ascl1<-pac_ascl1[new==1 & packagename!='sugar 1']
pac_ascl1[,bill_end:=billdate+days]
pac_ascl1<-merge(pac_ascl1,merge(investigation_all,investigation_header,by='investigation_id'),by='patient_id')
pac_ascl1[,pac_visit:=ifelse(investigation_date>=billdate & investigation_date<=bill_end,'1','0')]
pac_ascl1<-pac_ascl1[pac_visit==1]
#pac_ascl1<-pac_ascl1[created_date!=billdate]
pac_ascl1<-melt.data.table(pac_ascl1,id.vars = c('patient_id','billno','billdate','packagename','days','visit_c','new','pac_diff','bill_end','investigation_id','pac_visit','source','investigation_date'))
pac_ascl1<-pac_ascl1[!is.na(value)]
pac_ascl1[,tot_lab_pac_visit:=.N,.(patient_id,packagename,variable)]


write.csv(pac_ascl1[,.N,.(packagename,tot_lab_pac_visit,patient_id,variable)][,.N,.(packagename,tot_lab_pac_visit,variable)],'compliance_lab.csv')




diet<-emr_tp[level1_n=='personalInformation',.(level2_n,patientid,value)]
diet[,patient_id:=paste('tp_',patientid,sep = '')]
diet[,value:=tolower(str_squish(value))]
write.csv(diet,'diet.csv')
