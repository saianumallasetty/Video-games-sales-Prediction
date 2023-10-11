library(tidyverse) #most datascience related libraries
install.packages('FNN')
library(FNN) #knn regression
library(corrplot) # correlation plot
install.packages("tree")
library(tree) # Decision Tree
install.packages('caret')
library(caret) #train control

library(ggplot2)



vgsales<-read.csv(file.choose(),sep=",",na.strings=c(""," ","NA","N/A"), stringsAsFactors = TRUE)
head(vgsales)
summary(vgsales)

str(vgsales)

vgsales <- na.omit(vgsales)
str(vgsales)


na_count <-sapply(vgsales, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count


gamesales <- cbind(vgsales[,-c(6:10)],vgsales$Global_Sales)
gamesales$Year_of_Release <- 2016 - gamesales$Year_of_Release
names(gamesales)[3] <- "Game_Age"
names(gamesales)[12] <- "Global_Sales"
str(gamesales)


summary(gamesales)



#EDA
#platform graph
platformplot <- ggplot(gamesales, aes(x=Platform,fill =Platform)) + geom_bar() + theme(text = element_text(size=10))  
platformplot

#game age bar
gameAgeplot <- ggplot(gamesales, aes(x=Game_Age)) + geom_bar(fill = "forestgreen") + theme(text = element_text(size=10))  
gameAgeplot

#genre bar
Genreplot <- ggplot(gamesales, aes(x=Genre,fill =Genre)) + geom_bar() + theme(text = element_text(size=10),axis.text.x=element_text(vjust = 0.4,size=8))  
Genreplot

#critic score 
criticplot<-ggplot(gamesales,aes(Critic_Score))+geom_histogram(binwidth = 4,color ="black",fill = "green")
criticplot

#critic_count_
CriticCountplot <- ggplot(gamesales, aes(Critic_Count)) + geom_histogram(binwidth = 4, color = "black",fill = "blue") + theme(text = element_text(size=10))
CriticCountplot

#user_count
UserCountplot <- ggplot(gamesales, aes(User_Count)) + geom_histogram(color = "black",fill = "pink",bins = 40) + theme(text = element_text(size=10))
UserCountplot


#rating
Ratingplot <- ggplot(gamesales, aes(x=Rating,fill =Rating)) + geom_bar() + theme(text = element_text(size=10))  
Ratingplot

#global sales
GlobalSalesplot <- ggplot(gamesales, aes(Global_Sales)) + geom_histogram(binwidth = 2, color = "black",fill = "orange") + theme(text = element_text(size=10))
GlobalSalesplot

#global sales w.r.t platform
salesbyplatform <- ggplot(gamesales, aes(Platform,Global_Sales,fill =Platform)) +geom_bar(stat = "identity") + 
theme(text = element_text(size=10),legend.position="right",axis.text.x=element_text(vjust = 0.5,hjust = 1,size=10))+labs(x="Platform",y="Global Sales",title="global sales w.r.t platform")
salesbyplatform

#critic score and global sales
Criticscore <- ggplot(gamesales, aes(Critic_Score,Global_Sales))+geom_jitter(color = "blue") + theme(text = element_text(size = 15))
Criticscore 

#game age and global sales
Globalsales <- ggplot(gamesales, aes(Game_Age,Global_Sales)) + geom_jitter(color = "darkgreen") + theme(text = element_text(size = 15))
Globalsales

#usercount and global sales
Usercount <- ggplot(gamesales, aes(User_Count,Global_Sales))+geom_jitter(color = "orange") + theme(text = element_text(size = 15))
Usercount

#top 10 selling developers
gamesales %>% select(Name,Global_Sales) %>% arrange(desc(Global_Sales))%>% head(10)%>%
  ggplot(aes(x=Name,y=Global_Sales,fill=Name))+geom_bar(stat="identity")+
  theme(text = element_text(size=10),legend.position="right",axis.text.x=element_text(angle = 90,vjust = 0.5,hjust = 1,size=10))+labs(x="Developer",y="Total Sales",title="Top 10 selling Developers")+labs(x="Game",y="Global Sales",title="Top 10 selling games")+scale_fill_brewer(palette="Spectral")

#top 10 publishers
gamesales%>% select(Publisher,Global_Sales)%>%group_by(Publisher)%>%
  summarise(Total_sales=sum(Global_Sales))%>%arrange(desc(Total_sales))%>% head(10)%>%
  ggplot(aes(x=Publisher,y=Total_sales,fill=Publisher))+geom_bar(stat="identity")+
  theme(text = element_text(size=10),legend.position="right",axis.text.x=element_text(angle = 90,vjust = 0.5,hjust = 1,size=15))+labs(x="Publisher",y="Global Sales",title="Top 10 Publishers")+scale_fill_brewer(palette="Paired")

#top 10 games
gamesales %>% select(Name,Global_Sales) %>% arrange(desc(Global_Sales))%>% head(10)%>%
  ggplot(aes(x=Name,y=Global_Sales,fill=Name))+geom_bar(stat="identity")+
  theme(text = element_text(size=10),legend.position="right",axis.text.x=element_text(angle = 90,vjust = 0.5,hjust = 1,size=15))+labs(x="Developer",y="Total Sales",title="Top 10 selling Developers")+labs(x="Game",y="Global Sales",title="Top 10 selling games")+scale_fill_brewer(palette="Spectral")

View(vgsales_df)
#knn
gamesales <- gamesales[,-1]
knn.df <- gamesales
head(knn.df)


for (i in 1:dim(knn.df)[2])
{
  knn.df[,i] <- as.numeric(knn.df[,i])
}
head(knn.df)

knn.df1 <- sapply(knn.df[,1:(dim(knn.df)[2]-1)],scale)
knn.df1 <- as.data.frame(knn.df1)
knn.df <- cbind(knn.df1,knn.df$Global_Sales)
names(knn.df)[11] = "Global_Sales"
head(knn.df)

set.seed(5)
train.size <- floor(0.7*nrow(knn.df))
train.index <- sample(1:nrow(knn.df),train.size, replace = F)
train.set <- knn.df[train.index,]
test.set <- knn.df[-train.index,]
train.x <- train.set[,-11]
train.y <- train.set[,11]
test.x <- test.set[,-11]
test.y <- test.set[,11]

library(FNN)
pred_003 <- FNN::knn.reg(train = train.x, test = test.x, y = train.y, k = 3)
diff3 = test.y-pred_003$pred
test_mse = mean(diff3^2)
test_mse


#linear regression
lr.df <- gamesales
str(lr.df)


set.seed(101)
train.size <- floor(0.7*nrow(lr.df))
train.index <- sample(1:nrow(lr.df),train.size, replace = F)
train.set <- lr.df[train.index,]
test.set <- lr.df[-train.index,]


reg1 <- lm(Global_Sales ~., data = test.set)
summary(reg1)
predicted1 = predict(reg1, newdata = test.set)
Mse = mean((test.set$Global_Sales - predicted1)^2)
cat("Mse = ",Mse)

predicted1<-predict(test.set)

table(test.set$Global_Sales)

#model improvement
reg2 <- lm(Global_Sales ~Critic_Score + Critic_Count + User_Count, data = train.set)
summary(reg2)

predicted2 = predict(reg2, newdata = test.set)
Mse = mean((test.set$Global_Sales - predicted2)^2)
cat("Mse = ",Mse)


reg3 = lm(Global_Sales ~Platform + Game_Age + Genre + Critic_Score +Critic_Count + User_Count + Rating, data = train.set)
summary(reg3)

predicted = predict(reg3, newdata = test.set)
Mse = mean((test.set$Global_Sales - predicted)^2)
cat("Mse = ",Mse)

#decision tree
dt.df <- lr.df

 
set.seed(5)
tree.vgs=tree(Global_Sales~Critic_Score + Critic_Count + User_Count,dt.df,subset=train.index)
summary(tree.vgs)

dtplot<-plot(tree.vgs)
dtplot
text(tree.vgs,pretty=0, cex = 1.5)



View(vgsales_df)

dm <- dist(vgsales_df, method = "euclidean")
dm

model <- hclust(dm, method = "ward.D")
model

plot(model, labels=rownames(vgsales_df))

clusterGroups <- cutree(model, k=10)


#logistic regression
library(caTools)

set.seed(101)
train.size <- floor(0.7*nrow(lr.df))
train.index <- sample(1:nrow(lr.df),train.size, replace = F)
train.set <- lr.df[train.index,]
test.set <- lr.df[-train.index,]

logistic<-glm(Global_Sales~., family = binomial(link='logit'),data = train.set)

gamesales$g1<-as.factor(gamesales$Global_Sales)
View(gamesales)

lr2.df<-gamesales[,-11]
View(lr2.df)


set.seed(101)
train.size <- floor(0.3*nrow(lr2.df))
train.index <- sample(1:nrow(lr2.df),train.size, replace = F)
train.set <- lr2.df[train.index,]
test.set <- lr2.df[-train.index,]


#random forest
rf<-randomForest(factor(Global_Sales)~ Game_Age+Critic_Score,data = train.set)
rf

predict(rf,test.set)

rf.pred<-predict(rf,test.set)

head(rf.pred)



.#kmeans
#hclust
#logistic reg
#

#######################################

Videogame <- read.csv(file.choose(),sep=",",na.strings=c(""," ","NA","N/A"), stringsAsFactors = TRUE)
Videogame <- na.omit(Videogame)
Videogame$User_Count<-as.numeric(as.character(Videogame$User_Count))
Videogame$User_Score<-as.numeric(as.character(Videogame$User_Score))

Videogame$Publisher2=0
Videogame$Publisher2[(Videogame$Publisher=="Nintendo")|(Videogame$Publisher=="Activision")|(Videogame$Publisher=="Sony Computer Entertainment")|(Videogame$Publisher=="Electronic Arts")|(Videogame$Publisher=="Take-Two Interactive")|(Videogame$Publisher=="Ubisoft")]=1
Videogame_headpub <- Videogame[Videogame$Publisher2==1,]

Videogame_headpub$year2[Videogame_headpub$Year_of_Release<"2010"]=0
Videogame_headpub$year2[(Videogame_headpub$Year_of_Release=="2010")|(Videogame_headpub$Year_of_Release>"2000")]=1

Videogame_ms <- Videogame_headpub[,c(2,4,5,10,11,13,16,18)]

