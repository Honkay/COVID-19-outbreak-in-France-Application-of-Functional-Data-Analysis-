library(fda)
library(readxl)

THIRDCASEDV <- read_excel("Downloads/THIRDCASEDV.xlsx")
View(THIRDCASEDV)  
mat=as.matrix(THIRDCASEDV)
quartz()
matplot(mat,type="l")
THIRDCASEDV = mat
dim(THIRDCASEDV)

FranceCovid = as.matrix(THIRDCASEDV)

dimnames(FranceCovid)[[2]] <- paste('b', 1:101, sep='')

Fig10.10data = cbind(FranceCovid[, c('b38', 'b79', 'b100')])

FranceTime = 0:185;
FranceRng = c(0,185);
quartz()
matplot(FranceTime, Fig10.10data,
        type='l',lwd=2,xlab='Days',ylab='French Dept',col=1,
        cex.lab=1.5,cex.axis=1.5)

#  smooth the data observations


FranceBasis = create.fourier.basis(FranceRng, 185)

D2fdPar = fdPar(FranceBasis, lambda=1e6)

FranceCovidfd = smooth.basis(FranceTime, FranceCovid, D2fdPar)$fd
quartz()
par(mfrow=c(1,1),mar = c(8, 8, 4, 2))
plot(FranceCovidfd,xlab='day',ylab='Deceased cases during vaccination',cex.lab=1.5,cex.axis=1.5)
# calculate the variance-covariance of the functional data
tempvar = var.fd(FranceCovidfd)
tvvals = eval.bifd(1:185,1:185,tempvar)
quartz()
par(mfrow=c(1,1),mar = c(8, 8, 4, 2))
contour(1:185,1:185,tvvals,xlab='day',ylab='day',cex.lab=1.5,cex.axis=1.5)
image.plot(1:185,1:185,tvvals,xlab='day',ylab='day',cex.lab=1.5,cex.axis=1.5)
# Correlation Coefficient
temp.cor = cor.fd(1:185,FranceCovidfd)
quartz()
par(mfrow=c(1,1),mar = c(8, 8, 4, 2))
contour(1:185,1:185,temp.cor,xlab='day',ylab='day',cex.lab=1.5,cex.axis=1.5)
image.plot(1:185,1:185,temp.cor,xlab='day',ylab='day',cex.lab=1.5,cex.axis=1.5)

Francepca = pca.fd(FranceCovidfd,nharm=4)
colnames(Francepca$scores)<-c("Hospi1","Hospi2","Hospi3","Hospi4")
MatScores<-data.frame(Francepca$scores)
#Fitting the models

Model.Icu1<-lm(Hospi1~Hospi2+Hospi3+Hospi4,data=MatScores)
summary(Model.Icu1)
plot(Model.Icu1)

names(Francepca)
Francepca$varprop
#Francepca$values are the eigenvalues
quartz()
par(mfrow=c(1,1),mar = c(8, 8, 4, 2))
plot(Francepca$values[1:8],xlab='component',ylab='variance',col="red",
     cex.lab=1.5,cex.axis=1.5,cex=2)
# plot the cumulative percentage explained total variations
# It shows that the top 3 FPCs explains more than 99% of total variations
quartz()
par(mfrow=c(1,1),mar = c(8, 8, 4, 2))
plot(cumsum(Francepca$values[1:10])/sum(Francepca$values),xlab='Number of Components',
     ylab='cumulative variance explained',col=2,cex.lab=2,
     cex.axis=2,cex=2)
abline(h=0.99)
# Show the mean curves - temppca$meanfd
quartz()
par(mfrow=c(1,1),mar = c(8, 8, 4, 2))
plot(FranceCovidfd,xlab='day',ylab='Deceased cases during vaccination',cex.lab=1.5,cex.axis=1.5,col=4)
lines(Francepca$meanfd,lwd=2.5,col=2)
# functional principal components
harmfd = Francepca$harmonics
harmvals = eval.fd(1:185,harmfd)
dim(harmvals) # The top 4 FPCs
# plot the second FPC
quartz()
par(mfrow=c(1,1),mar = c(8, 8, 4, 2))
plot(1:185,harmvals[,2],xlab='day',ylab='PCs',
     lwd=4,lty=1,cex.lab=2,cex.axis=2,type='l')
# plot all 4 FPCs
quartz()
par(mfrow=c(1,1),mar = c(8, 8, 4, 2))
matplot(1:185,harmvals,xlab='day',ylab='PCs',
        lwd=4,lty=1,cex.lab=2.5,cex.axis=2.5,type='l')
legend(0,-0.07,c('PC1','PC2','PC3','PC4'),col=1:4,lty=1,lwd=5)
title('Deceased cases during vaccination Principle Component Functions')
# plot the first FPC scores vs. the second FPC scores 
quartz()
par(mfrow=c(1,1),mar = c(8, 8, 4, 2))
plot(Francepca$scores[,1:2],xlab='PC Score 1',ylab='PC Score 2',col=4,
     cex.lab=1.5,cex.axis=1.5,cex=1)
text(Francepca$scores[,1],Francepca$scores[,2], cex=1)
quartz()
par(mfrow=c(1,1),mar = c(8, 8, 4, 2))
plot(FranceCovidfd[60],xlab='day',ylab='Deceased cases during vaccination',cex.lab=2.5,cex.axis=2.5,col="black",lwd=4,ylim=c(-40,20))
lines(FranceCovidfd[76],xlab='day',ylab='Deceased   cases during vaccination',cex.lab=2.5,cex.axis=2.5,col="red",lwd=4)
lines(FranceCovidfd[92],xlab='day',ylab='Deceased   cases during vaccination',cex.lab=2.5,cex.axis=2.5,col="blue",lwd=4)
legend(-1, 15, c("Nord", "Paris","Essonne"),col = c("black","red","blue"),lty=1,lwd=4)
# Remove the mean function

quartz()
par(mfrow=c(1,1),mar = c(4, 4, 2, 1))
plot(FranceCovidfd[60]-Francepca$meanfd,xlab='day',ylab='Deceased   cases during vaccination',cex.lab=2.5,cex.axis=2.5,col="black",lwd=4,ylim=c(-40,20))
lines(FranceCovidfd[76]-Francepca$meanfd,xlab='day',ylab='Deceased  cases during vaccination',cex.lab=2.5,cex.axis=2.5,col="red",lwd=4)
lines(FranceCovidfd[92]-Francepca$meanfd,xlab='day',ylab='Deceased   cases during vaccination',cex.lab=2.5,cex.axis=2.5,col="blue",lwd=4)
legend(-1, 15, c("Nord", "Paris","Essonne"),col = c("black","red","blue"),lty=1,lwd=4)