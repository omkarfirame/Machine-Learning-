### MEAN DECREASE IN GINI ###

#data import

mydata <- read.csv("file:///D:/ML/Datasets/data.csv")
str(mydata)
mydata <- mydata[,-c(1,33)]

#data partition

shuff=mydata[sample(1:nrow(mydata)),]
train=shuff[(1:456),]
test=shuff[c(457:570),]

# random forest 

library(randomForest)

library(caret)
set.seed(1234)
rf <-  randomForest(diagnosis~. , data = train,ntree=400,importance = TRUE,proximity = TRUE)

pd <- predict(rf,test)
cm=confusionMatrix(pd,test$diagnosis)

#sorting attributes according their importance

im=importance(rf)
sr=sort(im[,4],decreasing = T)
nn=seq(2,30,by=2)

#finding important attributes and eliminating less important attributes

acc1=NULL
for(n in nn)
{
  var.predict<-paste(names(sr)[1:n],collapse="+")
  rnft.formula <- as.formula(paste(names(mydata)[1], var.predict, sep = " ~ "))
  rnft <- randomForest(rnft.formula ,data = train, ntree=400,importance = TRUE, proximity = TRUE)
  pred2 <- predict(rnft,test)
  cm2 <- confusionMatrix(pred2,test$diagnosis)
  acc=as.numeric(cm2$overall['Accuracy'])
  acc1=c(acc1,acc)
}
mtr=seq(2,30,by=2)
mx.ac=which.max(acc1)
bst.mtr=mtr[mx.ac]                   # best number of attributes
bst=seq(2,bst.mtr,by=2)

# Applying CV fold on best attributes

CV<-cut(1:nrow(mydata),breaks=5, labels=FALSE)
ntre=seq(50,400,by=50)
c4=NULL
c5=NULL
ntr=NULL

for(i in bst)
{
  c3=NULL
  for(j in ntre)
  {
    
    c1=NULL
    
    for( m in 1:5) 
    {
      
      test1<- shuff[which(m==CV,arr.ind=T),]
      train1<- shuff[-(which(m==CV,arr.ind=T)),]
      
      var.predict1<-paste(names(sr)[1:length(bst)],collapse="+")
      rnft.formula1 <- as.formula(paste(names(mydata)[1], var.predict1, sep = " ~ "))
      rf1 <- randomForest(rnft.formula1 ,data = train1,ntree=j,mtry=i, importance = TRUE, proximity = TRUE)
      pred1 <- predict(rf1,test1)
      cm1 <- confusionMatrix(pred1,test1$diagnosis)
      over1=as.numeric(cm1$overall['Accuracy'])
      
      c1=c(c1,over1)
    }  
    
    c2=mean(c1)
    
    c3=c(c3,c2)
  }  
  c4=c(c4,max(c3))
  ntr=c(ntr,ntre[which.max(c3)])
}

zz=data.frame(bst,c4,ntr)
zx=which.max(zz[,2])
MTRY=zz[zx,1]
ACCURACY=zz[zx,2]
NTREE=zz[zx,3]
print("Best accuracy and its respective Mtry and Ntree are:")
print(data.frame(MTRY,ACCURACY,NTREE))               
print("No of important attributes are  :")
print(bst.mtr)
print("Important attributes are :")
print(data.frame(names(sr)[1:bst.mtr]))