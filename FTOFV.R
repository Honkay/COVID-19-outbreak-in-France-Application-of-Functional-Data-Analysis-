library(fda)
library(readxl)

FIRSTCASEV <- read_excel("Downloads/FIRSTCASEV.xlsx")
View(FIRSTCASEV)  
mat=as.matrix(FIRSTCASEV)
quartz()
matplot(mat,type="l")
FIRSTCASEV = mat
dim(FIRSTCASEV)

FranceCovid = as.matrix(FIRSTCASEV)

dimnames(FranceCovid)[[2]] <- paste('b', 1:101, sep='')

Fig10.10data = cbind(FranceCovid[, c('b60', 'b76', 'b92')])

FranceTime = 0:185;
FranceRng = c(0,185);
quartz()
matplot(FranceTime, Fig10.10data,
        type='l',lwd=2,xlab='Days',ylab='French Dept',col=1,
        cex.lab=1.5,cex.axis=1.5)

#  smooth the log hazard observations

nbasis = 85
norder = 6
FranceBasis = create.bspline.basis(FranceRng, nbasis, norder)

D2fdPar = fdPar(FranceBasis, lambda=1e-7)

FranceCovidfd = smooth.basis(FranceTime, FranceCovid, D2fdPar)$fd

# The following requires manually clicking on the plot
# for each of 1O1 French dept

plotfit.fd(FranceCovid,FranceTime,FranceCovidfd)


# Set up for the list of regression coefficient fdPar objects

nbasis     = 23
FranceRng   = c(0,185)
FranceBetaBasis = create.bspline.basis(FranceRng,nbasis)

FranceBeta0Par = fdPar(FranceBetaBasis, 2, 1e-5)

FranceBeta1fd  = bifd(matrix(0,23,23), FranceBetaBasis, FranceBetaBasis)

FranceBeta1Par = bifdPar(FranceBeta1fd, 2, 2, 1e3, 1e3)

FranceBetaList = list(FranceBeta0Par, FranceBeta1Par)

#  Define the dependent and independent variable objects

NextYear = FranceCovidfd[2:101]
LastYear = FranceCovidfd[1:100]

#  Do the regression analysis

France.linmod = linmod(NextYear, LastYear, FranceBetaList)

France.Days = seq(0, 185, 2)
France.beta1mat = eval.bifd(France.Days, France.Days, France.linmod$beta1estbifd)
range(France.beta1mat)
quartz()
persp(France.Days, France.Days, France.beta1mat,
      xlab="Days", ylab="Days",zlab="beta(s,t)",
      cex.lab=1.5,cex.axis=1.5)
library(fields)
quartz()
image.plot(France.Days, France.Days, France.beta1mat,
           xlab="Days", ylab="Days",zlab="beta(s,t)",
           cex.lab=1.5,cex.axis=1.5)
abline(a = -1, b = 1, col = 2)
abline(a = 1, b = 1, col = 2)
abline(a = -2, b = 1, col = 2, lty=2)
abline(a = 2, b = 1, col = 2, lty=2)