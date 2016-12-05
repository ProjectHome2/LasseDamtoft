home
head(home)
price = home$Price
lprice = home$LogPrice
par(mfrow = c(1,1))
i = 5
head(home)
fisk = 1
for (i in c(5, 6, 7, 8, 10, 13)) {
  plot(home[,i], price, xlab = names(home)[i])
  lm.fit = lm(price~home[,i], data = home)
  abline(lm.fit, col = "red")
  sk = coef(lm.fit)[1]
  h = coef(lm.fit)[2]
  xtralala = mean(range(home[,i]))
  text(xtralala, 1e+7, paste("intercept =", sk, "\n", "slope =", h), pos = 1)
  fisk
}

lm.fit = lm(price~home$PostalCode, data = home)

summary(lm.fit)
lm.fit_1 = lm(lprice~home$Condition, data = home)
summary(lm.fit)
lm.fit_2 = lm(price~home$LivingArea, data = home)
summary(lm.fit_2)
lm.fit_3 = lm(lprice~home$BasementArea, data = home)
summary(lm.fit)
lm.fit_4 = lm(lprice~home$SalesPeriod, data = home)
summary(lm.fit)
lm.fit_5 = lm(lprice~home$YearOfSale, data = home)
summary(lm.fit)
coef(lm.fit)[1]
lm.fit_6 = lm(lprice~home$Quarter, data = home)
summary(lm.fit)
lm.fit_7 = lm(lprice~home$Age, data = home)
summary(lm.fit)
lm.fit_8 = lm(lprice~home$Balcony, data = home)
summary(lm.fit)
lm.fit_9 = lm(lprice~home$Renovation, data = home)
summary(lm.fit)
lm.fit_10 = lm(lprice~home$NumberOfBedrooms, data = home)
summary(lm.fit)

lm.fit = lm(lprice~home$Condition + home$LivingArea + home$YearOfSale + home$BasementArea + home$SalesPeriod+ home$Age + home$Renovation + home$NumberOfBedrooms, data = home)
summary(lm.fit)


plot(coef(lm.fit_2)[2],coef(lm.fit)[3], xlim = c(0, 0.4), ylim = c(0, 0.5))
points(coef(lm.fit_5)[2],coef(lm.fit)[4])
points(coef(lm.fit_3)[2],coef(lm.fit)[5])
points(coef(lm.fit_4)[2],coef(lm.fit)[6])
points(coef(lm.fit_7)[2],coef(lm.fit)[7])
points(coef(lm.fit_9)[2],coef(lm.fit)[8])
points(coef(lm.fit_10)[2],coef(lm.fit)[9])


k = 5

k = k + 1
lm.nice = lm(price ~ home[,k] + I(home[,k]^2) + I(home[,k]^3), data = home)
lm.nice_2 = lm(price ~ home[,k], data = home)
plot(home[,k], price, xlab = names(home)[k])
beta_3 = coef(lm.nice)
beta_1 = coef(lm.nice_2)
first = polynom::polynomial(coef = beta_1)
third = polynom::polynomial(coef = beta_3)
show(first)
show(third)
lines(first, col = "red", lwd = 3)
lines(third, col = "blue", lwd = 3)


summary(lm.nice)

summary(lm.nice_2)

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

















