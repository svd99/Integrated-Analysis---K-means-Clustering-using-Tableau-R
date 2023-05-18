titanic=read.csv('titanic.csv')
dim(titanic)
meanage=sum(na.omit(titanic$Age))/length(na.omit(titanic$Age))
meanage
titanic$Age[is.na(titanic$Age)] = meanage
titanic$Age = round(titanic$Age)

titanic$AgeCat[titanic$Age>=0 & titanic$Age<=16]="0-16"
titanic$AgeCat[titanic$Age>=17 & titanic$Age<=32]="17-32"
titanic$AgeCat[titanic$Age>=33 & titanic$Age<=48]="33-48"
titanic$AgeCat[titanic$Age>=49  & titanic$Age<=64]="49-64"
titanic$AgeCat[titanic$Age>=65]="65 and above"

titanic$Survived[titanic$Survived==0] = "Not Survived"
titanic$Survived[titanic$Survived==1] = "Survived"

titanic$Pclass = factor(titanic$Pclass)
titanic$AgeCat = factor(titanic$AgeCat)
titanic$Survived = factor(titanic$Survived)
titanic$Embarked = as.character(titanic$Embarked)
titanic$Embarked[titanic$Embarked == "S"] = "Southhampton"
titanic$Embarked[titanic$Embarked == "C"] = "Cherbourg"
titanic$Embarked[titanic$Embarked == "Q"] = "Queenstown"
titanic$Embarked = factor(titanic$Embarked)

titanic = titanic[c(-9,-11)]
View(titanic)

write.csv(titanic, file = "titanicNew.csv")

titanicNew = read.csv("titanicNew.csv")
titanicUpdated = titanicNew

SurvivedNum = ifelse(titanicUpdated$Survived=="Not Survived",0,1)
titanicUpdated = data.frame(titanicUpdated, SurvivedNum)

SexN = ifelse(titanicUpdated$Sex=="male",0,1)
titanicUpdated = data.frame(titanicUpdated, SexN)

EmbarkedN = ifelse(titanicUpdated$Embarked=="Southampton",1, ifelse(titanicUpdated$Embarked=="Cherbourg",2,0))
titanicUpdated <- data.frame(titanicUpdated, EmbarkedN)

write.csv(titanicUpdated,file = "titanicUpdated.csv")
titanic.scaled = scale(data.frame(titanic$Age, titanic$Parch, titanic$SibSp, titanic$Fare))
colnames(titanic.scaled)
totwss=vector()
btwss=vector()
for(i in 2:25){
  set.seed(1234)
  temp=kmeans(titanic.scaled, centers = i)
  totwss[i] = temp$tot.withinss
  btwss[i] = temp$betweenss
}

plot(totwss,xlab = "Number of Cluster", type = "b", 
     ylab="Total within sum of squares")
plot(btwss,xlab = "Number of Cluster", type = "b", 
     ylab="Total between sum of squares")

install.packages('Rserve',,'http://www.rforge.net/')
library(Rserve)
Rserve(args = "--save")




