home=HOME2
head(home)
post = home[,"Postnr"]
post
hist(post)
table(post)
vand = home[, "TaetVedVand"]
vand
which(vand == 1)
vand2 = home[which(vand == 1),]
head(vand2)
vand2
awe a
plot(home[, "Postnr"], home[, "Kontantpris"])

postpris = home[, c("Postnr", "Kontantpris")]
postsnit = c()
for (i in 8000:8350){
  postsnit = c(postsnit, mean(postpris[which(postpris[, "Postnr"] == i), "Kontantpris"]))
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

pris = home[, "Kontantpris"]
hist(pris)

library(ggplot2)
ggplot(home, aes(x=as.factor(Postnr), y=as.integer(Liggetid), colour = Boligtilstand)) + geom_point()
