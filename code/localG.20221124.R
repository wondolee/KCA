rm(list=ls())
setwd("d:/GIT/KCA/")
card.index<-readRDS('data/index.card.monthly.rda')
card.index$code<-as.factor(card.index$code)

month<-plyr::count(card.index$date)
colnames(month)<-c("day","freq")
month$id<-rownames(month)

card.index<-left_join(card.index,month[c(1,3)],by=c("date"="day"))
card.index$id<-as.numeric(card.index$id)
month$id<-as.numeric(month$id)

require(sf)
require(dplyr)
require(rgeos)
require(sp)
require(spdep)
require(rgeoda)
admin.data<-st_read('data/sig.shp')
admin.data<-st_transform(admin.data,4326)
admin.data<-admin.data %>% select(-SIG_KOR_NM)

queen_w <- rgeoda::queen_weights(admin.data)

gstat.pro<-list()
gstat<-list()
for (i in 1:nrow(month)){
  list.card.pro<-data.frame()
  list.card.pro<-card.index %>% filter(id==i)
  admin.data.re<-left_join(admin.data,list.card.pro,
                        by=c("SIG_CD"="code"))
  gstat.pro[[i]]<-rgeoda::local_gstar(queen_w,admin.data.re["intensity"],
                                      permutations = 9999,
                                        permutation_method = "complete",
                                        significance_cutoff = 0.05)
  gstat[[i]]<-gstat.pro[[i]]$GetClusterIndicators()
}

tbl.gstat<-plyr::ldply(gstat)
tbl.gstat<-as.data.frame(t(tbl.gstat))
rownames(tbl.gstat)<-NULL
tbl.gstat$code<-admin.data.re$SIG_CD
tbl.gstat<-reshape2::melt(tbl.gstat,id.var="code",variable.name="id",
                          value.name="type")
tbl.gstat$id<-as.numeric(tbl.gstat$id)
tbl.gstat$code<-as.factor(tbl.gstat$code)
card.index$code<-as.factor(card.index$code)
tbl.gstat<-left_join(tbl.gstat,card.index %>% 
                       select(code,year,month,intensity,date,name,des.dep.area,id),
                     by=c("code","id"))
hotspot.output<-tbl.gstat
hotspot.output$type<-as.factor(hotspot.output$type)
levels(hotspot.output$type)<-c("Not sig.","Cold spot","Hot spot","Isolated")
hotspot.output$type<-factor(hotspot.output$type,
                           levels = c("Not sig.","Hot spot","Cold spot","Isolated"))
#0"Not significant",1High-High","2Low-Low","3Undefined","4Isolated"
saveRDS(hotspot.output,'data/card.hotspot.type.rda')