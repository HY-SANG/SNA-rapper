install.packages('GGally')
library(data.table)
library(dplyr)
library(plyr)
library(ggplot2)
library(MASS)
library(proxy)
library(igraph)
library(scales)
library(plm)
library(pglm)
library(gridExtra)
library(grid)
library(ergm)
library(GGally)
library(tidyr)
library(splitstackshape)
library('combinat')
library(lubridate)

setwd("C:/Users/Jacky Yang/Desktop/Social Network/final project/2016_rap_network_analysis-master/data")

city<-read.csv('album_and_City.csv')
top<-read.csv('top.csv')
albumrank<-read.csv('billboard_rap_chart_albums_cleaned.csv')
artist<-read.csv('artist_pair_counts.csv')
detail<-read.csv('rapworld.csv')
ryhme<-read.csv('complete_rhyme.csv')
ryhme<-na.omit(ryhme)
morealbum<-read.csv('top album more.csv')
artist_2017<-read.csv('2017.csv')
artist_2018<-read.csv('2018.csv')
edge_2017<-na.omit(read.csv('edges2017.csv'))
edge_2018<-na.omit(read.csv('edges2018.csv'))

artist_2016<-read.csv('2016.csv')
edge_2016<-na.omit(read.csv('edges2016.csv'))
artist_2015<-read.csv('2015.csv')
edge_2015<-na.omit(read.csv('edges2015.csv'))
# artist_2014<-read.csv('2014.csv')
# edge_2014<-na.omit(read.csv('edges2014.csv'))
artist_2013<-read.csv('2013.csv')
edge_2013<-na.omit(read.csv('edges2013.csv'))
artist_2012<-read.csv('2013.csv')
edge_2012<-na.omit(read.csv('edges2012.csv'))


artist_2017<-na.omit(artist_2017)
artist_2018<-na.omit(artist_2018)
artist_name<-unique(na.omit(as.data.frame(c(as.character(artist_2017$artist),as.character(artist_2018$artist),as.character(artist$source)))))
colnames(artist_name)<-c('artist')
write.csv(artist_name,'artist_name.csv')


# create edge list --------------------------------------------------------
#2012
edge_2012<-cSplit(edge_2012,"artist",",", "long")
edge_2012$artist<-sub('\\]$', '', edge_2012$artist) 
edge_2012$artist<-gsub("\\[", "", edge_2012$artist) 
edge_2012$artist<-gsub("\\'", "", edge_2012$artist) 

edge_2012[, min_two := .N > 1,by =edge_2012[,1]]
edge_2012<-edge_2012[!edge_2012$artist=='',]
edge_2012<-edge_2012[min_two==TRUE]
combn1<-edge_2012[,as.data.table(t(combn(artist,2))), .(`album`)]
colnames(combn1)<-c("album","source","target")
write.csv(combn1,"combn1.csv")
#2013
edge_2013<-cSplit(edge_2013,"artist",",", "long")
edge_2013$artist<-sub('\\]$', '', edge_2013$artist) 
edge_2013$artist<-gsub("\\[", "", edge_2013$artist) 
edge_2013$artist<-gsub("\\'", "", edge_2013$artist) 

edge_2013[, min_two := .N > 1,by =edge_2013[,1]]
edge_2013<-edge_2013[!edge_2013$artist=='',]
edge_2013<-edge_2013[min_two==TRUE]
combn2<-edge_2013[,as.data.table(t(combn(artist,2))), .(`album`)]
colnames(combn2)<-c("album","source","target")
write.csv(combn2,"combn2.csv")
# #2014
# edge_2014<-cSplit(edge_2014,"artist",",", "long")
# edge_2014$artist<-sub('\\]$', '', edge_2014$artist) 
# edge_2014$artist<-gsub("\\[", "", edge_2014$artist) 
# edge_2014$artist<-gsub("\\'", "", edge_2014$artist) 
# 
# edge_2014[, min_two := .N > 1,by =edge_2014[,1]]
# edge_2014<-edge_2014[!edge_2014$artist=='',]
# edge_2014<-edge_2014[min_two==TRUE]
# combn1<-edge_2014[,as.data.table(t(combn(artist,2))), .(`album`)]
# colnames(combn1)<-c("album","source","target")

#2015
edge_2015<-cSplit(edge_2015,"artist",",", "long")
edge_2015$artist<-sub('\\]$', '', edge_2015$artist) 
edge_2015$artist<-gsub("\\[", "", edge_2015$artist) 
edge_2015$artist<-gsub("\\'", "", edge_2015$artist) 

edge_2015[, min_two := .N > 1,by =edge_2015[,1]]
edge_2015<-edge_2015[!edge_2015$artist=='',]
edge_2015<-edge_2015[min_two==TRUE]
combn3<-edge_2015[,as.data.table(t(combn(artist,2))), .(`album`)]
colnames(combn3)<-c("album","source","target")
write.csv(combn3,"combn3.csv")
#2016
edge_2016<-cSplit(edge_2016,"artist",",", "long")
edge_2016$artist<-sub('\\]$', '', edge_2016$artist) 
edge_2016$artist<-gsub("\\[", "", edge_2016$artist) 
edge_2016$artist<-gsub("\\'", "", edge_2016$artist) 

edge_2016[, min_two := .N > 1,by =edge_2016[,1]]
edge_2016<-edge_2016[!edge_2016$artist=='',]
edge_2016<-edge_2016[min_two==TRUE]
combn4<-edge_2016[,as.data.table(t(combn(artist,2))), .(`album`)]
colnames(combn4)<-c("album","source","target")
write.csv(combn4,"combn4.csv")
#2017
edge_2017<-cSplit(edge_2017,"artist",",", "long")
edge_2017$artist<-sub('\\]$', '', edge_2017$artist) 
edge_2017$artist<-gsub("\\[", "", edge_2017$artist) 
edge_2017$artist<-gsub("\\'", "", edge_2017$artist) 

edge_2017[, min_two := .N > 1,by =edge_2017[,1]]
edge_2017<-edge_2017[!edge_2017$artist=='',]
edge_2017<-edge_2017[min_two==TRUE]
combn5<-edge_2017[,as.data.table(t(combn(artist,2))), .(`album`)]
colnames(combn5)<-c("album","source","target")
write.csv(combn5,"combn5.csv")
#2018
edge_2018<-cSplit(edge_2018,"artist",",", "long")
edge_2018$artist<-sub('\\]$', '', edge_2018$artist) 
edge_2018$artist<-gsub("\\[", "", edge_2018$artist) 
edge_2018$artist<-gsub("\\[", "", edge_2018$artist) 
edge_2018$artist<-gsub("\\'", "", edge_2018$artist) 

edge_2018[, min_two := .N > 1,by =edge_2018[,1]]
edge_2018<-edge_2018[min_two==TRUE]
edge_2018<-edge_2018[!edge_2018$artist=='',]
combn6<-edge_2018[,as.data.table(t(combn(artist,2))), .(`album`)]
colnames(combn6)<-c("album","source","target")
write.csv(combn6,"combn6.csv")

# all years edge ----------------------------------------------------------
alledge<-rbind(edge_2012,edge_2013,edge_2015,edge_2016,edge_2017,edge_2018)
alledge_combn<-alledge[,as.data.table(t(combn(artist,2))), .(`album`)]
alledge_combn[,]
write.csv(alledge_combn,"alledge_combn.csv")

colnames(alledge_combn)<-c("album","source","target")
g<-graph.data.frame(alledge_combn[,1:2],directed = FALSE)

#degree
degree<-as.data.frame(degree(g))
#get the name of 
id=V(g)$name
degree<-as.data.table(list(artist=id,eigen = degree))
#betweenness
betweenness<-as.data.frame(betweenness(g))
#get the name 
id=V(g)$name
between<-as.data.table(list(artist=id,between = betweenness))
#close
close<-as.data.frame(closeness(g))
#get the name 
id=V(g)$name
close<-as.data.table(list(artist=id,close = close))
#calculate eigen vector
eigen<-as.data.table(eigen_centrality(g)$vector)
#get the name of the investor
id=V(g)$name
eigen<-as.data.table(list(artist=id,eigen = eigen))
#join centrality tables
merge1<-left_join(top,degree, by = c("source"="artist"))
merge1<-left_join(merge1,between, by = c("source"="artist"))
merge1<-left_join(merge1,close, by = c("source"="artist"))
merge1<-left_join(merge1,eigen, by = c("source"="artist"))
colnames(merge1)<-c("source","target","weight","type","top.x","top.y","both_top","degree","between","close","eigen")
merge1<-as.data.table(merge1)
merge1[,one_top:= ifelse(top.x==1|top.y==1,1,0)]
#join artist details 2016
colnames(detail)
merge2<-left_join(merge1,detail, by = c("source"="ï..Name"))
merge2<-left_join(merge2,detail, by = c("target"="ï..Name"))
merge2<-unique(merge2[-which(is.na(merge2$years_in_op.x)),])
merge2<-as.data.table(merge2)
merge2[,same_city:= ifelse(City.x==City.y, 1, 0)]
merge2_short<-merge2[,c("source","target","top.x","top.y","years_in_op.x","years_in_op.y","same_city","City.x","City.y","both_top",
                        "degree","close","between","eigen")]
merge2_short<-merge2_short[-which(is.na(merge2_short$same_city)),]
merge2_short[,one_top:= ifelse(top.x==1|top.y==1,1,0)]
merge2_short<-unique(merge2_short)

glm6<-glm(top.x~degree+between+close+one_top+eigen+years_in_op.x+years_in_op.y+factor(City.y),data=twoyears,family = "binomial")
summary(glm6)
# the rest ----------------------------------------------------------------

#get the top song from the dirty excel
for (row in 1:nrow(morealbum)){
  if (row %% 3 ==0){
    artist<-as.character(morealbum[row,1])
    artist_list_2013<-c(artist_list_2013,artist)
  }
}
artist_list_2015<-c()
for (row in 1:nrow(morealbum)){
  if (row %% 3 ==0){
    artist<-as.character(morealbum[row,2])
    artist_list_2015<-c(artist_list_2015,artist)
  }
}
artist_list_2016<-c()
for (row in 1:nrow(morealbum)){
  if (row %% 3 ==0){
    artist<-as.character(morealbum[row,3])
    artist_list_2016<-c(artist_list_2016,artist)
  }
}

artist_list_2017<-c()
for (row in 1:nrow(morealbum)){
  if (row %% 3 ==0){
    artist<-as.character(morealbum[row,4])
    artist_list_2017<-c(artist_list_2017,artist)
  }
}

artist_list_2018<-c()
for (row in 1:nrow(morealbum)){
  if (row %% 3 ==0){
    artist<-as.character(morealbum[row,5])
    artist_list_2018<-c(artist_list_2018,artist)
  }
}

topalbum2013<-data.frame(artist_list_2013)
topalbum2015<-data.frame(artist_list_2015)
topalbum2016<-data.frame(artist_list_2016)
topalbum2017<-data.frame(artist_list_2017)
topalbum2018<-data.frame(artist_list_2018)
topalbum2016$top<-1
topalbum2017$top<-1
topalbum2018$top<-1
topalbum2013$top<-1
topalbum2015$top<-1


# detail<-detail[-which(detail$Years.Active.End<2016),]
# detail$years_in_op<-2016-detail$Years.Active.Start
# detail<-detail[-which(is.na(detail$years_in_op)),]

# twoway<-as.data.frame(cbind(as.character(artist$target),as.character(artist$source),artist$weight,as.character(artist$type)))
# colnames(twoway)<-c("source","target","weight","type")
# artist<-rbind(artist,twoway)

#join album info with edge lists
top1<-left_join(combn2,topalbum2013, by=c("source"="artist_list_2013"))
top1<-left_join(top1,topalbum2013, by=c("target"="artist_list_2013"))

top2<-left_join(combn3,topalbum2015, by=c("source"="artist_list_2015"))
top2<-left_join(top2,topalbum2015, by=c("target"="artist_list_2015"))

top3<-left_join(combn4,topalbum2016, by=c("source"="artist_list_2016"))
top3<-left_join(top3,topalbum2016, by=c("target"="artist_list_2016"))

top4<-left_join(combn5,topalbum2017, by=c("source"="artist_list_2017"))
top4<-left_join(top4,topalbum2017, by=c("target"="artist_list_2017"))

top5<-left_join(combn6,topalbum2018, by=c("source"="artist_list_2018"))
top5<-left_join(top5,topalbum2018, by=c("target"="artist_list_2018"))

top1[is.na(top1)]<-0
top1<-as.data.table(top1)
top1[,both_top:=ifelse(top.x==1 & top.y==1, 1,0)]
top1[,one_top:=ifelse(top.x==1|top.y==1,1,0)]
percent<-sum(top$both_top)/2136

top2[is.na(top2)]<-0
top2<-as.data.table(top2)
top2[,both_top:=ifelse(top.x==1 & top.y==1, 1,0)]
top2[,one_top:=ifelse(top.x==1|top.y==1,1,0)]

top3[is.na(top3)]<-0
top3<-as.data.table(top3)
top3[,both_top:=ifelse(top.x==1 & top.y==1, 1,0)]
top3[,one_top:=ifelse(top.x==1|top.y==1,1,0)]
top3<-top3[,1:7]

top4[is.na(top4)]<-0
top4<-as.data.table(top4)
top4[,both_top:=ifelse(top.x==1 & top.y==1, 1,0)]
top4[,one_top:=ifelse(top.x==1|top.y==1,1,0)]
top4<-top4[,1:7]

top5[is.na(top5)]<-0
top5<-as.data.table(top5)
top5[,both_top:=ifelse(top.x==1 & top.y==1, 1,0)]
top5[,one_top:=ifelse(top.x==1|top.y==1,1,0)]
top5<-top5[,1:7]

fouryears<-rbind(top,top2,top3,top4)
fiveyears<-rbind(top,top2,top3,top4,top5)

g<-graph.data.frame(fouryears[,2:3],directed=FALSE)
g2<-graph.data.frame(fiveyears[,2:3],directed=FALSE)
g3<-graph.data.frame(top5[,2:3],directed=FALSE)
#degree
degree<-as.data.frame(degree(g))
#get the name of 
id=V(g)$name
degree<-as.data.table(list(artist=id,eigen = degree))
#betweenness
betweenness<-as.data.frame(betweenness(g))
#get the name 
id=V(g)$name
between<-as.data.table(list(artist=id,between = betweenness))
#close
close<-as.data.frame(closeness(g))
#get the name 
id=V(g)$name
close<-as.data.table(list(artist=id,close = close))
#calculate eigen vector
eigen<-as.data.table(eigen_centrality(g)$vector)
#get the name of the investor
id=V(g)$name
eigen<-as.data.table(list(artist=id,eigen = eigen))

#degree
degree<-as.data.frame(degree(g2))
#get the name of 
id=V(g2)$name
degree<-as.data.table(list(artist=id,eigen = degree))
#betweenness
betweenness<-as.data.frame(betweenness(g2))
#get the name 
id=V(g2)$name
between<-as.data.table(list(artist=id,between = betweenness))
#close
close<-as.data.frame(closeness(g2))
#get the name 
id=V(g2)$name
close<-as.data.table(list(artist=id,close = close))
#calculate eigen vector
eigen<-as.data.table(eigen_centrality(g2)$vector)
#get the name of the investor
id=V(g2)$name
eigen<-as.data.table(list(artist=id,eigen = eigen))

#degree
degree<-as.data.frame(degree(g3))
#get the name of 
id=V(g3)$name
degree<-as.data.table(list(artist=id,eigen = degree))
#betweenness
betweenness<-as.data.frame(betweenness(g3))
#get the name 
id=V(g3)$name
between<-as.data.table(list(artist=id,between = betweenness))
#close
close<-as.data.frame(closeness(g3))
#get the name 
id=V(g3)$name
close<-as.data.table(list(artist=id,close = close))
#calculate eigen vector
eigen<-as.data.table(eigen_centrality(g3)$vector)
#get the name of the investor
id=V(g3)$name
eigen<-as.data.table(list(artist=id,eigen = eigen))
#join centrality tables
merge1<-left_join(fouryears,degree, by = c("source"="artist"))
merge1<-left_join(merge1,between, by = c("source"="artist"))
merge1<-left_join(merge1,close, by = c("source"="artist"))
merge1<-left_join(merge1,eigen, by = c("source"="artist"))
colnames(merge1)<-c("album","source","target","top.x","top.y","both_top","one_top","degree","between","close","eigen")
merge1<-as.data.table(merge1)
merge1[,out:=ifelse(source==target,1,0)]
merge1<-merge1[out==0]
merge1_join<-left_join(merge1,city, by =c("source"="source"))
merge1_join<-merge1[!is.na(merge1$City),]

oneyear<-left_join(top5,degree, by = c("source"="artist"))
oneyear<-left_join(oneyear,between, by = c("source"="artist"))
oneyear<-left_join(oneyear,close, by = c("source"="artist"))
oneyear<-left_join(oneyear,eigen, by = c("source"="artist"))
colnames(oneyear)<-c("album","source","target","top.x","top.y","both_top","one_top","degree","between","close","eigen")
oneyear<-as.data.table(oneyear)
oneyear[,out:=ifelse(source==target,1,0)]
oneyear<-oneyear[out==0]
oneyear_join<-left_join(oneyear,city, by =c("source"="source"))
oneyear_join<-oneyear_join[!is.na(oneyear_join$City),]

fouryear<-left_join(fouryears,degree, by = c("source"="artist"))
fouryear<-left_join(fouryear,between, by = c("source"="artist"))
fouryear<-left_join(fouryear,close, by = c("source"="artist"))
fouryear<-left_join(fouryear,eigen, by = c("source"="artist"))
colnames(fouryear)<-c("album","source","target","top.x","top.y","both_top","one_top","degree","between","close","eigen")
fouryear<-as.data.table(fouryear)
fouryear[,out:=ifelse(source==target,1,0)]
fouryear<-fouryear[out==0]

fiveyear<-left_join(fiveyears,degree, by = c("source"="artist"))
fiveyear<-left_join(fiveyear,between, by = c("source"="artist"))
fiveyear<-left_join(fiveyear,close, by = c("source"="artist"))
fiveyear<-left_join(fiveyear,eigen, by = c("source"="artist"))
colnames(fiveyear)<-c("album","source","target","top.x","top.y","both_top","one_top","degree","between","close","eigen")
fiveyear<-as.data.table(fiveyear)
fiveyear[,out:=ifelse(source==target,1,0)]
fiveyear<-unique(fiveyear[out==0])
fiveyear_join<-left_join(fiveyear,city, by =c("source"="source"))
fiveyear_join<-fiveyear_join[!is.na(fiveyear_join$City),]
fiveyear_join2<-left_join(fiveyear,ryhme,by=c("source"="name"))
fiveyear_join2<-fiveyear_join2[!is.na(fiveyear_join2$score),]
#regression
glm6<-glm(top.x~degree+between+close+eigen,data=merge1,family = "binomial"(link="logit"))
summary(glm6)


#lagged effect analysis
lagged<-cbind(fouryear[,5:12],fiveyear$top.x)
lagged2<-cbind(fouryear[,5:12],oneyear$top.x)
glm6<-glm(V2~degree+between+close+eigen,data=lagged,family = "binomial"(link="logit"))
summary(glm6)

glm6<-glm(V2~degree+between+close+eigen,data=lagged2,family = "binomial"(link="logit"))
summary(glm6)

oneyear$pre_2016<-predict(glm6,oneyear[,8:11], type="response")
oneyear[,pre_classify:= ifelse(pre_2016>0.9999996,1,0)]
oneyear[,right:=ifelse(pre_classify==one_top,1,0)]
oneyear<-unique(oneyear)
#accuracy rate
sum(oneyear$right)/255

n<-network(oneyear[,2:3])
n %v% "degree" <-oneyear$degree
n %v% "close" <-oneyear$close
n %v% "between" <-oneyear$between
n %v% "eigen" <-oneyear$eigen
ergm<-ergm(n~edges+triangle+nodecov("degree")+nodecov("close")+nodecov("between")+nodecov("eigen"))
summary(ergm)
colnames(fiveyears)
n<-network(fiveyears[,2:3],directed = FALSE)
n %v% "degree" <-fiveyear$degree
n %v% "close" <-fiveyear$close
n %v% "between" <-fiveyear$between
n %v% "eigen" <-fiveyear$eigen
n %v% "top"   <-fiveyear$top.x
ergm<-ergm(n~edges+triangle)
ergm<-ergm(n~edges+triangle+nodecov("top"))
summary(ergm)

ergm<-ergm(n~edges+triangle)
#individual position analysis
close10<-close[order(`close.closeness(g2)`,decreasing =TRUE),][1:10,]
between10<-between[order(`between.betweenness(g2)`,decreasing =TRUE),][1:10,]
degree10<-degree[order(`eigen.degree(g2)`,decreasing =TRUE),][1:10,]
eigen10<-eigen[order(eigen.V1,decreasing =TRUE),][1:10,]

close10<-unique(left_join(close10,fiveyear_join2,by=c("artist"="source")))
between10<-na.omit(unique(left_join(between10,fiveyear_join2,by=c("artist"="source"))))
degree10<-unique(left_join(degree10,fiveyear_join2,by=c("artist"="source")))
eigen10<-unique(na.omit(left_join(eigen10,fiveyear_join2,by=c("artist"="source"))))

sum(close10$top.x)/140
sum(between10$top.x)/147
sum(degree10$top.x)/139
sum(eigen10$top.x)/84

mean(close10$score)
mean(between10$score)
mean(degree10$score)
mean(eigen10$score)

close10<-close[order(`close.closeness(g2)`,decreasing =FALSE),][1:10,]
between10<-between[order(`between.betweenness(g2)`,decreasing =FALSE),][1:10,]
degree10<-degree[order(`eigen.degree(g2)`,decreasing =FALSE),][1:10,]
eigen10<-eigen[order(eigen.V1,decreasing =FALSE),][1:10,]

close10<-na.omit(unique(left_join(close10,fiveyear_join2,by=c("artist"="source"))))
between10<-na.omit(unique(left_join(between10,fiveyear_join2,by=c("artist"="source"))))
degree10<-na.omit(unique(left_join(degree10,fiveyear_join2,by=c("artist"="source"))))
eigen10<-unique(na.omit(left_join(eigen10,fiveyear_join2,by=c("artist"="source"))))

sum(close10$top.x)/140
sum(between10$top.x)/147
sum(degree10$top.x)/139
sum(eigen10$top.x)/84

mean(close10$score)
mean(between10$score)
mean(degree10$score)
mean(eigen10$score)

#-------------------------------------------------------------------


close[which.max(close$`close.closeness(g2)`)]
degree[which.max(degree$`eigen.degree(g2)`)]
between[which.max(between$`between.betweenness(g2)`)]
eigen[which.max(eigen$eigen.V1)]

close[which.min(close$`close.closeness(g2)`)]
degree[which.min(degree$`eigen.degree(g2)`)]
between[which.min(between$`between.betweenness(g2)`)]
eigen[which.min(eigen$eigen.V1)]

coreness <- graph.coreness(g2) 
maxCoreness <- max(coreness)
verticesHavingMaxCoreness <- which(coreness == maxCoreness) 
kcore <- induced.subgraph(graph=g,vids=verticesHavingMaxCoreness)

#clique analysis----------------------------------------------------------------------------
clique<-cliques(g2,2)
summary(clique)
#find out the largest clique g-4 years, g2-5 years, g3-1 year
largest_cliques(g2)
plot(g3,vertex.label=NA,vertex.size=5)
#create a DT with just rapper pairs and city and weight
edges_for_city <- top[,c('source', 'target', 'weight', 'City.x')]

#top list
largeclique<-fiveyear[fiveyear$source=="Don Q"|fiveyear$source=="Chris Brown"|fiveyear$source=="21 Savage"|fiveyear$source=="Desiigner"]
smallclique<-fiveyear[fiveyear$source=="Anderson .Paak"|fiveyear$source=="Candice Pillay"]

#rhytme factor


#make a graph for edgesforcity
rapper_graph_city <-graph.data.frame(edges_for_city,directed = FALSE)
rapper_graph_city <-set_edge_attr(rapper_graph_city,"weight",value = edges_for_city$weight)
#rapper_graph_city <- set_vertex_attr(rapper_graph_city, "color", value=edges_for_city$City)

n_city<-network(edges_for_city)
plot(n_city)
graph_of_rappers <- plot.igraph(rapper_graph_city, vertex.label=NA, vertex.color.label=edges_for_city$City.x,layout=layout.fruchterman.reingold, vertex.size=5, vertex.color=edges_for_city$City)



g<-graph.data.frame(top[,1:2],directed = FALSE)
#g<-set_edge_attr(g,"weight",value = top$weight)

plot(g,vertex.label=NA)

g2<-graph.data.frame(top2[,1:2],directed = FALSE)


# Function 1 : For ploting missing value
plot_missing <- function(data, title = NULL, ggtheme = theme_gray(), theme_config = list("legend.position" = c("bottom"))) {
  ## Declare variable first to pass R CMD check
  feature <- num_missing <- pct_missing <- group <- NULL
  ## Check if input is data.table
  is_data_table <- is.data.table(data)
  ## Detect input data class
  data_class <- class(data)
  ## Set data to data.table
  if (!is_data_table) data <- data.table(data)
  ## Extract missing value distribution
  missing_value <- data.table(
    "feature" = names(data),
    "num_missing" = sapply(data, function(x) {sum(is.na(x))})
  )
  missing_value[, feature := factor(feature, levels = feature[order(-rank(num_missing))])]
  missing_value[, pct_missing := num_missing / nrow(data)]
  missing_value[pct_missing < 0.05, group := "Good"]
  missing_value[pct_missing >= 0.05 & pct_missing < 0.4, group := "OK"]
  missing_value[pct_missing >= 0.4 & pct_missing < 0.8, group := "Bad"]
  missing_value[pct_missing >= 0.8, group := "Remove"][]
  ## Set data class back to original
  if (!is_data_table) class(missing_value) <- data_class
  ## Create ggplot object
  output <- ggplot(missing_value, aes_string(x = "feature", y = "num_missing", fill = "group")) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0(round(100 * pct_missing, 2), "%"))) +
    scale_fill_manual("Group", values = c("Good" = "#1a9641", "OK" = "#a6d96a", "Bad" = "#fdae61", "Remove" = "#d7191c"), breaks = c("Good", "OK", "Bad", "Remove")) +
    scale_y_continuous(labels = comma) +
    coord_flip() +
    xlab("Features") + ylab("Number of missing rows") +
    ggtitle(title) +
    ggtheme + theme_linedraw()+
    do.call(theme, theme_config)
  ## Print plot
  print(output)
  ## Set return object
  return(invisible(missing_value))
}
plot_missing(join)

join[is.na(join)]<-0
colnames(join)<-c("source","target","weight","type","Album","topalbum","topsong")

#degree
degree<-as.data.frame(degree(g))
#get the name of 
id=V(g)$name
degree<-as.data.table(list(artist=id,eigen = degree))
#betweenness
betweenness<-as.data.frame(betweenness(g))
#get the name 
id=V(g)$name
between<-as.data.table(list(artist=id,between = betweenness))
#close
close<-as.data.frame(closeness(g))
#get the name 
id=V(g)$name
close<-as.data.table(list(artist=id,close = close))
#calculate eigen vector
eigen<-as.data.table(eigen_centrality(g)$vector)
#get the name of the investor
id=V(g)$name
eigen<-as.data.table(list(artist=id,eigen = eigen))


#degree
degree<-as.data.frame(degree(g2))
#get the name of 
id=V(g2)$name
degree<-as.data.table(list(artist=id,eigen = degree))
#betweenness
betweenness<-as.data.frame(betweenness(g2))
#get the name 
id=V(g2)$name
between<-as.data.table(list(artist=id,between = betweenness))
#close
close<-as.data.frame(closeness(g2))
#get the name 
id=V(g2)$name
close<-as.data.table(list(artist=id,close = close))
#calculate eigen vector
eigen<-as.data.table(eigen_centrality(g2)$vector)
#get the name of the investor
id=V(g2)$name
eigen<-as.data.table(list(artist=id,eigen = eigen))

unique(artist$source)
#get the highest closeness
close[which.max(close$close)]
#distance
distance<-shortest.paths(g,mode="all")
distance2<-longest.paths(g,mode="all")
distance[!is.finite(distance)]<-1034
min(rowMeans(distance))
mean(distance["ScHoolboy Q",])#verify if the artist with highest closeness also have the lowest path
diameter(g)

#join centrality tables
merge1<-left_join(top,degree, by = c("source"="artist"))
merge1<-left_join(merge1,between, by = c("source"="artist"))
merge1<-left_join(merge1,close, by = c("source"="artist"))
merge1<-left_join(merge1,eigen, by = c("source"="artist"))
colnames(merge1)<-c("source","target","weight","type","top.x","top.y","both_top","degree","between","close","eigen")
merge1<-as.data.table(merge1)
merge1[,one_top:= ifelse(top.x==1|top.y==1,1,0)]

#join centrality tables
merge11<-left_join(top2,degree, by = c("source"="artist"))
merge11<-left_join(merge11,between, by = c("source"="artist"))
merge11<-left_join(merge11,close, by = c("source"="artist"))
merge11<-left_join(merge11,eigen, by = c("source"="artist"))
colnames(merge11)<-c("album","source","target","top.x","top.y","both_top","one_top","degree","between","close","eigen")
merge11<-as.data.table(merge11)
merge11[,one_top:= ifelse(top.x==1|top.y==1,1,0)]

#join artist details 2016
colnames(detail)
merge2<-left_join(merge1,detail, by = c("source"="ï..Name"))
merge2<-left_join(merge2,detail, by = c("target"="ï..Name"))
merge2<-unique(merge2[-which(is.na(merge2$years_in_op.x)),])
merge2<-as.data.table(merge2)
merge2[,same_city:= ifelse(City.x==City.y, 1, 0)]
merge2_short<-merge2[,c("source","target","top.x","top.y","years_in_op.x","years_in_op.y","same_city","City.x","City.y","both_top",
                        "degree","close","between","eigen")]
merge2_short<-merge2_short[-which(is.na(merge2_short$same_city)),]
merge2_short[,one_top:= ifelse(top.x==1|top.y==1,1,0)]
merge2_short<-unique(merge2_short)

#join artist details 2017
colnames(detail)
merge22<-left_join(merge11,detail, by = c("source"="ï..Name"))
merge22<-left_join(merge22,detail, by = c("target"="ï..Name"))
merge22<-unique(merge22[-which(is.na(merge22$years_in_op.x)),])
merge22<-as.data.table(merge22)
merge22[,same_city:= ifelse(City.x==City.y, 1, 0)]
merge22_short<-merge22[,c("source","target","top.x","top.y","years_in_op.x","years_in_op.y","same_city","City.x","City.y","both_top",
                        "degree","close","between","eigen")]
merge22_short<-merge22_short[-which(is.na(merge22_short$same_city)),]
merge22_short[,one_top:= ifelse(top.x==1|top.y==1,1,0)]
merge22_short<-unique(merge22_short)

colnames(merge22)
twoyears<-rbind(merge2_short,merge22_short)

#regression
glm6<-glm(top.x~degree+between+close+one_top+eigen+years_in_op.x+years_in_op.y+factor(City.y),data=twoyears,family = "binomial")
summary(glm6)

glm6<-glm(top.x~degree+between+close+one_top+eigen,data=merge1,family = "binomial")
summary(glm6)

merge1$pre_2016<-predict(glm6,merge1, type="response")
merge1[,pre_classify:= ifelse(pre_2016>0.8,1,0)]
merge1[,right:=ifelse(pre_classify==one_top,1,0)]
merge1$pre_2017<-top2$top.x

glm7<-glm(pre_2016~degree+between+close+one_top+eigen,data=merge1,family = "binomial")
summary(glm6)
merge1[,right_2017:=ifelse(pre_==one_top,1,0)]

cor(merge2_short$top.x,merge2_short$degree)
cor(merge2_short$top.x,merge2_short$close)
cor(merge2_short$top.x,merge2_short$between)

merge2_short$pre_2016<-predict(glm6,merge2_short, type="response")
merge2_short[,pre_classify:= ifelse(pre_2016>0.8,1,0)]
merge2_short[,right:=ifelse(pre_classify==one_top,1,0)]
merge2_short[,same_city:=ifelse(City.x==City.y,1,0)]
#accuracy rate
sum(merge2_short$right)/111

n<-network(twoyears[,1:2])

n %v% "city" <-twoyears$same_city
n %v% "years1" <-twoyears$years_in_op.x
n %v% "years2" <-twoyears$years_in_op.y

write.csv(merge2_short,"merge2_short.csv")

ergm1<-ergm(n~edges+triangle+nodematch("city")+nodecov("years1")+nodecov("years2"))

#merge2_short_join<-left_join(merge2_short,ryhme, by =c("source"="Artist"))
#source<-merge2[-which(is.na(merge2_short$years_in_op.y)),]

#define RMSE function
RMSE <- function(x,y){
  a <- sqrt(sum((x-y)^2)/length(y))
  return(a)
}

plot_missing(merge2_short_join)

#clique analysis----------------------------------------------------------------------------
g<-graph.data.frame(merge2_short[,1:2],directed = FALSE)
clique<-cliques(g,2)
summary(clique)
#find out the largest clique
largest_cliques(g)

merge1<-as.data.table(merge1)
large_clique<-rbind(merge1[which(source=="Problem"),],merge1[which(source=="Ty Dolla $ign"),],merge1[which(source=="The Game"),],merge1[which(source=="YG"),])
small_clique1<-rbind(merge1[which(source=="Jadakiss"),],merge1[which(source=="Akon"),]) 
small_clique2<-rbind(merge1[which(source=="Yo Gotti"),],merge1[which(source=="Pusha T"),]) 
small_clique3<-rbind(merge1[which(source=="Wiz Khalifa"),],merge1[which(source=="Curren$y"),]) 

clique_num(g)
summary(clique)

#coreness analysis-----------------------------------------------------
g2<-graph.data.frame(merge2[,1:2],directed = FALSE)
V(g2)$kCore = graph.coreness(g2)

CorenessLayout <- function(g) {
  coreness <- graph.coreness(g);
  xy <- array(NA, dim=c(length(coreness), 2));
  
  shells <- sort(unique(coreness));
  for(shell in shells) {
    v <- 1 - ((shell-1) / max(shells));
    nodes_in_shell <- sum(coreness==shell);
    angles <- seq(0,360,(360/nodes_in_shell));
    angles <- angles[-length(angles)]; # remove last element
    xy[coreness==shell, 1] <- sin(angles) * v;
    xy[coreness==shell, 2] <- cos(angles) * v;
  }
  return(xy);
}

# g2 is the network
# compute coreness
coreness <- graph.coreness(g2);
# assign colors
colbar <- rainbow(max(coreness));
# create layout
ll <- CorenessLayout(g2);
# plot
plot(g2,vertex.size=3,vertex.label=NA,vertex.color=colbar[coreness], vertex.frame.color=colbar[coreness], main='Coreness Analysis');

#plot city
V(g2)$city<-merge2$City.x
plot(g2,vertex.color=city,vertex.label=NA)

#get the year function
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#regression--------------------------------------------------------------------------------------
# glm1<-glm(top_song~degree+between+close+eigen,data=merge1,family = "binomial")
# summary(glm1)
# 
# glm2<-glm(top_album~degree+between+close+eigen,data=merge1,family = "binomial")
# summary(glm2)
#top album list
par(mfrow=c(1,1))

glm1<-glm(top_album~degree,data=merge1,family = "binomial")
glm2<-glm(top_album~between,data=merge1,family = "binomial")
glm3<-glm(top_album~close,data=merge1,family = "binomial")
glm4<-glm(top_album~eigen,data=merge1,family = "binomial")
plot(glm1)
#add city and years in business;top album
glm5<-glm(top_album~degree+between+close+eigen+years_in_op.x+factor(City.x),data=merge2,family = "binomial")
summary(glm5)
#top song
glm6<-glm(top_song~degree+between+close+eigen+years_in_op.x+factor(City.x),data=merge2,family = "binomial")
summary(glm6)
#get edge list for ergm
edge<-unique(merge2[,c("source","target")])

edge_join<-left_join(edge,merge2_short,by=c("source"="source"))
edge_join2<-left_join(edge_join,merge2_short,by=c("target"="ï..Name"))
#edge_nodup<-unique(left_join(edge_nodup,merge2_short,by=c("source"="source","target"="target")))
#edge_nodup2<-unique(left_join(edge_nodup,merge2_short, by = c("target"="source","source"="target")))
edge2<-unique(na.omit(edge_nodup[,c("source","target")]))
n<-network(merge2_short[,1:2],directed = FALSE)

#ergm---------------------------------------------------------------------------------------------
n %v% "city" <- merge2_short$same_city
n %v% "years" <- merge2_short$years_in_op.x
n %v% "years2" <- merge2_short$years_in_op.y

ergm1<-ergm(n~edges+triangle+nodematch("city",diff=TRUE)+nodecov("years")+nodecov("years2"))

summary(ergm1)

merge1$album_predict_degree<-predict(glm1,merge1, type="response")
merge1$album_predict_between<-predict(glm2,merge1, type="response")
merge1$album_predict_close<-predict(glm3,merge1, type="response")
merge1$album_predict_eigen<-predict(glm4,merge1, type="response")

(p1<-ggplot(data=merge1,aes(y=merge1$album_predict_degree,x=degree))+geom_point()+
  geom_smooth(method="glm", method.args = list(family = "binomial"), se = FALSE)+ylab("Probabilities")+xlab("Degree"))
(p2<-ggplot(data=merge1,aes(y=merge1$album_predict_between,x=between))+geom_point()+
    geom_smooth(method="glm", method.args = list(family = "binomial"), se = FALSE)+ylab("Probabilities")+xlab("Betweenness"))
(p3<-ggplot(data=merge1,aes(y=merge1$album_predict_close,x=close))+geom_point()+
    geom_smooth(method="glm", method.args = list(family = "binomial"), se = FALSE)+ylab("Probabilities")+xlab("Closeness"))
#(p4<-ggplot(data=merge1,aes(y=merge1$album_predict_eigen,x=eigen))+geom_point()+
#    geom_smooth(method="glm", method.args = list(family = "binomial"), se = FALSE))
grid.arrange(p1,p2,p3,nrow=2,top = textGrob("Top Album Models",gp=gpar(fontsize=15,font=3)))

#top song list
glm1<-glm(top_song~degree,data=merge1,family = "binomial")
glm2<-glm(top_song~between,data=merge1,family = "binomial")
glm3<-glm(top_song~close,data=merge1,family = "binomial")
glm4<-glm(top_song~eigen,data=merge1,family = "binomial")

merge1$song_predict_degree<-predict(glm1,merge1, type="response")
merge1$song_predict_between<-predict(glm2,merge1, type="response")
merge1$song_predict_close<-predict(glm3,merge1, type="response")
merge1$song_predict_eigen<-predict(glm4,merge1, type="response")

(p1<-ggplot(data=merge1,aes(y=merge1$song_predict_degree,x=degree))+geom_point()+
    geom_smooth(method="glm", method.args = list(family = "binomial"), se = FALSE)+ylab("Probabilities")+xlab("Degree"))
(p2<-ggplot(data=merge1,aes(y=merge1$song_predict_between,x=between))+geom_point()+
    geom_smooth(method="glm", method.args = list(family = "binomial"), se = FALSE)+ylab("Probabilities")+xlab("Betweenness"))
(p3<-ggplot(data=merge1,aes(y=merge1$song_predict_close,x=close))+geom_point()+
    geom_smooth(method="glm", method.args = list(family = "binomial"), se = FALSE)+ylab("Probabilities")+xlab("Closeness"))
#(p4<-ggplot(data=merge1,aes(y=merge1$song_predict_eigen,x=eigen))+geom_point()+
#    geom_smooth(method="glm", method.args = list(family = "binomial"), se = FALSE))
grid.arrange(p1,p2,p3,nrow=2,top = textGrob("Top Song Models",gp=gpar(fontsize=15,font=3)))

#correlation analysis
cor(merge1$top_song,merge1$degree)
cor(merge1$top_song,merge1$between)
cor(merge1$top_song,merge1$close)
cor(merge1$top_song,merge1$eigen)

cor(merge1$top_album,merge1$degree)#high correlation
cor(merge1$top_album,merge1$between)#high correlation
cor(merge1$top_album,merge1$close)
cor(merge1$top_album,merge1$eigen)

plot(merge1$top_album,merge1$degree)









