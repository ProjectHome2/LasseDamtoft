lprice = home$LogPrice
library("glmnet")
library("foreach")
library("iterators")
data.matrix = home[-c(3, 4)]
head(data.matrix)
x = model.matrix (lprice~.,data = data.matrix)[,-1]
y = lprice

library("glmnet")
grid =10^ seq (10,-2, length =100)
ridge.mod = glmnet(x, y, alpha = 0, lambda = grid)
dim(coef(ridge.mod ))
ridge.mod$lambda [50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[ -1 ,50]^2) )
ridge.mod$lambda [60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[ -1 ,60]^2) )

#Definer traning og test set
set.seed (1)
train=sample (1: nrow(x), nrow(x)/2)
test=(- train )
y.test=y[test]
#run ridge on the training set
ridge.mod =glmnet (x[train ,],y[train],alpha =0, lambda =grid ,thresh =1e-12)
#defines the ridge predicted values
ridge.pred=predict (ridge.mod ,s=4, newx=x[test,])
#calcs the MSE for this predictiong
mean(( ridge.pred -y.test)^2)
#calcs the MSE for only a mean as our model
mean(( mean(y[train ])-y.test)^2)

#Bruger CV til at finde det optimale lambda
cv.out =cv.glmnet (x[train ,],y[train],alpha =0)
plot(cv.out)
bestlam =cv.out$lambda.min
log(bestlam)
log(cv.out$lambda.1se)
# OLS estimatet
ridge.pred=predict (ridge.mod ,s=0, newx=x[test ,], exact=T)
mean(( ridge.pred -y.test)^2)
#Ridge med optimalt lambda
ridge.pred=predict (ridge.mod ,s=bestlam ,newx=x[test ,])
mean((ridge.pred -y.test)^2)

#runing ridge on whole data set
out=glmnet(x,y,alpha =0)
predict(out ,type="coefficients",s=bestlam )[1:28 ,]


cv.out

# LASSO

lasso.mod =glmnet (x[train ,],y[train],alpha =1, lambda =grid)
plot(lasso.mod)
#Best lambda for lasso
cv.out =cv.glmnet (x[train ,],y[train],alpha =1)
plot(cv.out)
bestlam =cv.out$lambda.min
#Lasso MSE
lasso.pred=predict (lasso.mod ,s=bestlam ,newx=x[test ,])
mean(( lasso.pred -y.test)^2)

out=glmnet (x,y,alpha =1, lambda =grid)
lasso.coef=predict (out ,type ="coefficients",s=bestlam )[1:28,]
lasso.coef
lasso.coef[lasso.coef !=0]



as = seq(0, 1, length.out = 20)
bestlams = c()
best1ses = c()
for (alp in as) {
  tral = cv.glmnet(x[train ,], y[train], alpha = alp)
  mod.lol = glmnet(x[train ,], y[train], alpha = alp, lambda = grid)
  bestlam = tral$lambda.min
  best1se = tral$lambda.1se
  pred.best = predict (mod.lol ,s=bestlam ,newx=x[test ,])
  pred.1se = predict (mod.lol ,s=best1se ,newx=x[test ,])
  bestlams = c(bestlams, mean((pred.best - y.test)^2))
  best1ses = c(best1ses, mean((pred.1se - y.test)^2))
}
plot(as, best1ses, ylim = c(0.046, 0.0525), type = "l")
lines(as, bestlams, type = "l")






