lprice = home$LogPrice
require("glmnet")
require("foreach")
require("iterators")
data.matrix = home[-c(3, 4)]
head(data.matrix)
x = model.matrix (lprice~.,data = data.matrix)[,-1]
y = lprice
head(x)
lambdas =10^ seq (10,-2, length =100)

ridge.mod = glmnet(x, y, alpha = 0, lambda = lambdas)
dim(coef(ridge.mod ))
ridge.mod$lambda [50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[ -1 ,50]^2) )
ridge.mod$lambda [60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[ -1 ,60]^2) )

#Definer traning og test set
set.seed (1)
train=sample (1: nrow(x), 0.75* nrow(x))
test=(- train )
y.test=y[test]
#run ridge on the training set
ridge.mod =glmnet (x[train ,],y[train],alpha =0, lambda =lambdas ,thresh =1e-12)
#defines the ridge predicted values

ridge.pred=predict (ridge.mod ,s=4, newx=x[test,])
#calcs the MSE for this predictiong
mean(( ridge.pred -y.test)^2)
#calcs the MSE for only a mean as our model
mean(( mean(y[train ])-y.test)^2)

#Bruger CV til at finde det optimale lambda
cv.out =cv.glmnet (x[train ,],y[train],alpha =0, lambda = lambdas)
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

lasso.mod =glmnet (x[train ,],y[train],alpha =1, lambda =lambdas)
plot(lasso.mod)
#Best lambda for lasso
cv.out =cv.glmnet (x[train ,],y[train],alpha =1)
plot(cv.out)
bestlam =cv.out$lambda.min
#Lasso MSE
lasso.pred=predict (lasso.mod ,s=bestlam ,newx=x[test ,])
mean(( lasso.pred -y.test)^2)

out=glmnet (x,y,alpha =1, lambda =lambdas)
lasso.coef=predict (out ,type ="coefficients",s=bestlam )[1:28,]
lasso.coef
lasso.coef[lasso.coef !=0]



as = seq(0, 1, length.out = 20)
bestlams = c()
best1ses = c()
for (alp in as) {
  tral = cv.glmnet(x[train ,], y[train], alpha = alp)
  mod.lol = glmnet(x[train ,], y[train], alpha = alp, lambda = lambdas)
  bestlam = tral$lambda.min
  best1se = tral$lambda.1se
  pred.best = predict (mod.lol ,s=bestlam ,newx=x[test ,])
  pred.1se = predict (mod.lol ,s=best1se ,newx=x[test ,])
  bestlams = c(bestlams, mean((pred.best - y.test)^2))
  best1ses = c(best1ses, mean((pred.1se - y.test)^2))
}

alphas[which.min(bestlams)]
out=glmnet (x,y,alpha =as[2], lambda =lambdas)
lasso.coef=predict (out ,type ="coefficients",s=bestlams[2] )[1:28,]
lasso.coef
lasso.coef[lasso.coef !=0]



set.seed (3) # In order to reproduce the results
train = sample (1: nrow(x), (0.75 * nrow(x)))
test = (- train )
y.train = y[train]
y.test = y[test]

alphas = seq(0, 1, length.out = 20)
bestlams = c()
best1ses = c()
for (alp in alphas) {
  cv.mod = cv.glmnet(x[train,], y[train], alpha = alp)
  mod.lol = glmnet(x[train,], y[train], alpha = alp, lambda = lambdas)
  bestlam = cv.mod$lambda.min
  best1se = cv.mod$lambda.1se
  pred.best = predict (mod.lol, s = bestlam, newx = x[train,])
  pred.1se = predict (mod.lol, s=best1se, newx = x[train,])
  bestlams = c(bestlams, mean((pred.best - y.train)^2))
  best1ses = c(best1ses, mean((pred.1se - y.train)^2))
}
xmin = min(range(best1ses, bestlams))


xmax = max(range(best1ses, bestlams))
plot(as, best1ses, ylim = c(0.9 * xmin, 1.1 * xmax), type = "l", ylab = "MSE")
lines(as, bestlams, type = "l")



set.seed (420) # In order to reproduce the results
train = sample (1: nrow(x), (0.75 * nrow(x)))
test = (- train )
y.test = y[test]
y.train = y[train]

# Initialise alphas and empty vectors
alphas = seq(0, 1, length.out = 20)
bestlams = c()
lams.1se = c()
bestcv = c()
cv.1se = c()

for (alp in alphas) {
  cv.mod = cv.glmnet(x[train,], y[train], alpha = alp)
  
  # Add Best lamda (that minimise CV error) and lambda.1se
  bestlams = c(bestlams, cv.mod$lambda.min)
  lams.1se = c(lams.1se, cv.mod$lambda.1se)
  
  # Add the corresponding CV and CV.1se
  bestcv = c(bestcv, min(cv.mod$cvm))
  cv.1se = c(cv.1se, min(cv.mod$cvup))
}

top.20.models = cbind(alphas, bestlams, bestcv)
top.20.1se.models = cbind(alphas, lams.1se, cv.1se)

# 
x.min.alpha = which.min(bestcv)
best.mod = c(alphas[x.min.alpha], bestlams[x.min.alpha], bestcv[x.min.alpha])
names(best.mod) = c("Best alpha", "Best lambda", "CV-error")        
best.mod

x.min.alpha.1se = which.min(cv.1se)
best.mod.1se = c(alphas[x.min.alpha.1se], 
                 lams.1se[x.min.alpha.1se], cv.1se[x.min.alpha.1se])
names(best.mod.1se) = c("Best alpha.1se", "Best lambda.1se", "CV-error.1se")        
best.mod.1se

elastic.mod = glmnet(x[train,], y[train], alpha = best.mod["Best alpha"], lambda = best.mod["Best lambda"])
model = predict(elastic.mod, s = best.mod["Best lambda"], type = "coefficients")[1:10000]
model[model != 0]

elastic.mod.1se = glmnet(x[train,], y[train], alpha = 
                           best.mod.1se["Best alpha.1se"], lambda = best.mod.1se["Best lambda.1se"])
model.1se = predict(elastic.mod.1se, lam)


x = model.matrix (y~. + LivingArea:NumberOfBedrooms + LivingArea:PostalCode + Condition:PostalCode + LivingArea:BasementArea + log(LivingArea) + sqrt(BasementArea),data = data.matrix)[,-1]





















predict(mod.lol, s = bestlam, newx = x[train,])
model.1se[model.1se != 0]
