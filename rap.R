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


setwd("C:/Users/Jacky Yang/Desktop/Social Network/final project/2016_rap_network_analysis-master/data")

top<-read.csv('top.csv')
albumrank<-read.csv('billboard_rap_chart_albums_cleaned.csv')
artist<-read.csv('artist_pair_counts.csv')
detail<-read.csv('rapworld.csv')
ryhme<-read.csv('rhyme factor.csv')
morealbum<-read.csv('top album more.csv')

artist_list_2016<-c()
for (row in 1:nrow(morealbum)){
  if (row %% 3 ==0){
    artist<-as.character(morealbum[row,1])
    artist_list_2016<-c(artist_list_2016,artist)
  }
}

artist_list_2017<-c()
for (row in 1:nrow(morealbum)){
  if (row %% 3 ==0){
    artist<-as.character(morealbum[row,2])
    artist_list_2017<-c(artist_list_2017,artist)
  }
}

artist_list_2018<-c()
for (row in 1:nrow(morealbum)){
  if (row %% 3 ==0){
    artist<-as.character(morealbum[row,3])
    artist_list_2018<-c(artist_list_2018,artist)
  }
}

topalbum2016<-data.frame(artist_list_2016)
topalbum2017<-data.frame(artist_list_2017)
topalbum2018<-data.frame(artist_list_2018)
topalbum2016$top<-1
topalbum2017$top<-1
topalbum2018$top<-1

detail<-detail[-which(detail$Years.Active.End<2016),]
detail$years_in_op<-2016-detail$Years.Active.Start
detail<-detail[-which(is.na(detail$years_in_op)),]

song_d<-data.frame()
for (row in 1:nrow(top)){
  if (row %% 3 == 0){
    song<-as.data.frame(as.character(top[row,1]))
    song_d<-rbind(song_d,song)
  }
}
colnames(song_d)<-c("artist")

twoway<-as.data.frame(cbind(as.character(artist$target),as.character(artist$source),artist$weight,as.character(artist$type)))
colnames(twoway)<-c("source","target","weight","type")
artist<-rbind(artist,twoway)

#join album info with edge lists
top<-left_join(artist,topalbum2016, by=c("source"="artist_list_2016"))
top<-left_join(top,topalbum2016, by=c("target"="artist_list_2016"))
top<-left_join(top,topalbum2017, by=c("target"="artist_list_2016"))
top<-unique(top)
top[is.na(top)]<-0
top<-as.data.table(top)
top[,both_top:=ifelse(top.x==1 & top.y==1, 1,0)]
percent<-sum(top$both_top)/2136


#create a DT with just rapper pairs and city and weight
edges_for_city <- top[,c('source', 'target', 'weight', 'City.x')]

#make a graph for edgesforcity
rapper_graph_city <-graph.data.frame(edges_for_city,directed = FALSE)
rapper_graph_city <-set_edge_attr(rapper_graph_city,"weight",value = edges_for_city$weight)
#rapper_graph_city <- set_vertex_attr(rapper_graph_city, "color", value=edges_for_city$City)

n_city<-network(edges_for_city)
plot(n_city)
graph_of_rappers <- plot.igraph(rapper_graph_city, vertex.label=NA, vertex.color.label=edges_for_city$City.x,layout=layout.fruchterman.reingold, vertex.size=5, vertex.color=edges_for_city$City)


g<-graph.data.frame(top[,1:2],directed = FALSE)
g<-set_edge_attr(g,"weight",value = top$weight)

plot(g,vertex.label=NA)


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

#join artist details

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

merge2_short$source
#regression
glm6<-glm(top.x~degree+between+close+one_top+eigen+years_in_op.x+years_in_op.y+factor(City.x),data=merge2_short,family = "binomial")
summary(glm6)

cor(merge2_short$top.x,merge2_short$degree)
cor(merge2_short$top.x,merge2_short$close)
cor(merge2_short$top.x,merge2_short$between)

merge2_short$pre_2016<-predict(glm6,merge2_short, type="response")
merge2_short[,pre_classify:= ifelse(pre_2016>0.8,1,0)]
merge2_short[,right:=ifelse(pre_classify==one_top,1,0)]
merge2_short[,same_city:=ifelse(City.x==City.y,1,0)]
#accuracy rate
sum(merge2_short$right)/111

n<-network(merge2_short[,1:2])

n %v% "city" <-merge2_short$same_city
n %v% "years1" <-merge2_short$years_in_op.x
n %v% "years2" <-merge2_short$years_in_op.y

ergm1<-ergm(n~edges+triangle+nodematch("city",diff=TRUE)+nodecov("years1")+nodecov("years2"))

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









