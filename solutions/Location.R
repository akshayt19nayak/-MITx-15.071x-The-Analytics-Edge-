boston<-read.csv(file.choose())
str(boston)

#EDA
plot(boston$LON,boston$LAT)
points(boston$LON[boston$NOX>0.538],boston$LAT[boston$NOX>0.538],pch=19,col='green')
points(boston$LON[boston$TRACT==3531],boston$LAT[boston$TRACT==3531],pch=19,col='red')
points(boston$LON[boston$CHAS==1],boston$LAT[boston$CHAS==1],pch=19,col='blue')
points(boston$LON[boston$MEDV>=21.2],boston$LAT[boston$MEDV>=21.2],pch=19,col='red')
summary(boston$MEDV)

#Model 
latlontree=rpart(MEDV~LAT+LON,data=boston)
prp(latlontree)
fittedvalues=predict(latlontree)
points(boston$LON[fittedvalues>=21.2],boston$LAT[fittedvalues>=21.2],pch='$',col='blue')

#Linear Regression vs Decision Tree
library('caTools')
split=sample.split(boston$MEDV,SplitRatio = 0.7)
train=subset(boston,split==TRUE)
test=subset(boston,split==FALSE)
linreg=lm(MEDV~LAT+LON+CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO,data = train)
pred=predict(linreg,newdata=test)
RMSE=sqrt(sum((test$MEDV-pred)^2)/nrow(test))

tree=rpart(MEDV~LAT+LON+CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO,data = train)
predtree=predict(tree,newdata=test)
RMSEtree=sqrt(sum((test$MEDV-predtree)^2)/nrow(test))
prp(tree)

#Cross Validation
library('caret')
library('e1071')
tr.control=trainControl(method='cv',number=10)
cp.grid=expand.grid(.cp=seq(0,0.01,0.001))
tr=train(MEDV~LAT+LON+CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO,data = train,
         method='rpart',trControl=tr.control,tuneGrid=cp.grid)
best.tree=tr$finalModel
prp(best.tree)
predcv=predict(best.tree,newdata=test)
RMSEcv=sqrt(sum((test$MEDV-predcv)^2)/nrow(test))
