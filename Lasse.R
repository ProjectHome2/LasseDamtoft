head(home)
post = home[,"PostalCode"]
post
hist(post)
table(post)
vand = home[, "CloseToWater"]
vand
which(vand == 1)
vand2 = home[which(vand == 1),]
head(vand2)
vand2

plot(home[, "PostalCode"], home[, "Price"])

postpris = home[, c("PostalCode", "Price")]
postsnit = c()
for (i in 8000:8350){
  postsnit = c(postsnit, mean(postpris[which(postpris[, "PostalCode"] == i), "Price"]))
}
xs = 8000:8350
A = matrix(c(xs, postsnit), 351, 2, byrow = FALSE)
post2 = as.data.frame(A)
post2 = post2[which(post2[, 2] > 0), ]
plot(post2)
post2
sort(post2)
post3 = post2[order(-post2[,2]),]
post3

pris = home[, "Price"]
hist(pris)

library(ggplot2)
ggplot(home, aes(x=as.factor(PostalCode), y=as.integer(SalesPeriod), colour = Condition)) + geom_point()

names(home)
ggplot(home, aes(x=LivingArea, y=(Price))) + geom_point()
ggplot(home, aes(x=YearOfSale, y=(Price))) + geom_point()
ggplot(home, aes(x=Levels, y=(Price))) + geom_point()
ggplot(home, aes(x=ConstructionYear, y=log(Price))) + geom_point()

ggplot(home, aes(x=(Price), color = Condition)) + geom_histogram(binwidth = 1e05)
ggplot(home, aes(x=NewWindows, y=(Price), color = Condition)) + geom_point()
table(home[, "NewWindows"])
       
ggplot(home, aes(x=Age, y=(Renovation), color = Condition)) + geom_point()





















