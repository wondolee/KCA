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
admin.data<-st_read('data/sig.shp')
admin.data<-st_transform(admin.data,4326)
admin.data<-admin.data %>% select(-SIG_KOR_NM)

coords<-gCentroid(as(admin.data,"Spatial"),byid=TRUE)
knn <-knearneigh(coords,k=4)
admin_knn <- knn %>% knn2nb() %>% nb2listw(., style="W",zero.policy = TRUE)

list.card<-list()
gstat.pro<-list()
for (i in 1:nrow(month)){
  list.card.pro<-data.frame()
  list.card.pro<-card.index %>% filter(id==i)
  admin.data.re<-left_join(admin.data,list.card.pro,
                        by=c("SIG_CD"="code"))
    gstat.pro[[i]]<-admin.data.re %>% pull(intensity) %>% as.vector()%>%
    localG(., admin_knn,zero.policy=TRUE,return_internals=TRUE)
  gstat.pro[[i]]<-attributes(gstat.pro[[i]])$internals
}

require(data.table)
gstat<-plyr::ldply(gstat.pro)
colnames(gstat)<-c("Gi","E","V","Z","Pr")
gstat$type<-0 #Insig.
gstat$type[gstat$Gi>=1.65&gstat$Pr<=0.05]<-1 #hot
gstat$type[gstat$Gi<=-1.65&gstat$Pr<=0.05]<-2 #cold

gstat<-cbind(gstat,card.index %>% select(code,year,month,date))
hotspot.output<-gstat %>% select(code,year,month,date,type)
saveRDS(hotspot.output,'data/card.hotspot.type.rda')