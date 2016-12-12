load("home_imputated.Rdata")
y = home$LogPrice 
price = home$Price
home$Price = NULL
home$LogPrice = NULL
home$SalesPeriod = NULL
data.matrix = home
x = model.matrix (y~. + LivingArea:NumberOfBedrooms + LivingArea:PostalCode + Condition:PostalCode + LivingArea:BasementArea +log(LivingArea) + sqrt(BasementArea) ,data = data.matrix)[,-1]

lambdas = 10^ seq(10,-2, length =100)
ridge.mod = glmnet(x, y, alpha = 0, lambda = lambdas)
dim(coef(ridge.mod))

cv.out =cv.glmnet(x,y,alpha =0, lambda = lambdas)

set.seed (420) # In order to reproduce the results
train = sample (1: nrow(x), (0.75 * nrow(x)))
xs = 1:nrow(x)
test = xs[-train]
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

# Find best pair (alpha, lambda)
best.mod.index = which.min(bestcv)
best.mod = c(alphas[best.mod.index], bestlams[best.mod.index],
             bestcv[best.mod.index])
names(best.mod) = c("Best alpha", "Best lambda", "CV-error")        
best.mod

# Find best pair (alpha, lambda) for 1se
best.mod.1se.index = which.min(cv.1se)
best.mod.1se = c(alphas[best.mod.1se.index], 
                 lams.1se[best.mod.1se.index], cv.1se[best.mod.1se.index])
names(best.mod.1se) = c("Best alpha.1se", "Best lambda.1se", "CV-error.1se")        
best.mod.1se

# Make the final model
elastic.mod = glmnet(x[train,], y[train], alpha = best.mod["Best alpha"],
                     lambda = best.mod["Best lambda"])
# Return non-zero coefficient
index = which(coef(elastic.mod) != 0)
mod.coef = data.frame(Coef = coef(elastic.mod)[which(coef(elastic.mod) != 0)], row.names = dimnames(coef(elastic.mod))[[1]][index])
mod.coef

# Final model for 1se
elastic.mod.1se = glmnet(x[train,], y[train], alpha = 
                           best.mod.1se["Best alpha.1se"], lambda = best.mod.1se["Best lambda.1se"])
# Return non-zero coefficient
index = which(coef(elastic.mod.1se) != 0)
mod.1se.coef = data.frame(Coef = coef(elastic.mod.1se)[which(coef(elastic.mod.1se) != 0)], row.names = dimnames(coef(elastic.mod.1se))[[1]][index])
mod.1se.coef

par(mfrow = c(1,2))
pred.best = predict (elastic.mod, s = best.mod["Best lambda"], newx = x[train,])
pred.1se = predict (elastic.mod.1se, s = best.mod.1se["Best lambda.1se"], newx = x[train,])

plot(pred.best, y.train, xlab = "Predicted y's from the best lambda")
plot(pred.1se, y.train, xlab = "Predicted y's from the lambda.1se")

res.best = y.train - pred.best
res.1se = y.train - pred.1se

plot(pred.best, res.best, xlab = "log(price)", ylab = "Residual")
abline(a = 0, b = 0)
plot(pred.1se, res.1se, xlab = "log(price)", ylab = "Residual")
abline(a = 0, b = 0)

bin.width = 100
hist(res.best, bin.width, xlim = c(-1.5, 1.5), probability = T, xlab = "Residuals", ylab = "Estimated probability")
x.lol <- seq(-1.5, 1.5, length.out = 500)
y.lol <- dnorm(x.lol, 0, sqrt(var(res.best)))
lines(x.lol, y.lol)

hist(res.1se, bin.width, xlim = c(-1.5, 1.5), probability = T, xlab = "Residuals", ylab = "Estimated probability")
y.lol <- dnorm(x.lol, 0, sqrt(var(res.1se)))
lines(x.lol, y.lol, lwd =3)


plot(train, res.best, xlab = "index", ylab = "Redidual")
abline(a = 0, b = 0)
plot(train, res.1se)
abline(a = 0, b = 0, xlab = "index", ylab = "Redidual")


pred.log.test = predict (elastic.mod, s = best.mod["Best lambda"], newx = x[test,])
var.log.test = sum((y.train - pred.best)^2)/(nrow(x[train,]) - ncol(x[train,]))

pred.test = exp(pred.log.test + var.log.test/2)
price.test = price[test]
sqrt(mean((pred.test - price.test)^2))
