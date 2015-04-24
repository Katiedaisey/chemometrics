data<-read.csv("data/octane_cal.csv")
require(pls)
source("C:/Users/Beebe Group/Desktop/chemo/scripts/eval_resids.R")
source("C:/Users/Beebe Group/Desktop/chemo/chemometrics/snippets/mvr_ext.R")
spec<-data[,-c(1,228)]
prop<-data[,228]

cal_spec<-as.matrix(spec[1:34,],nrow=34)
cal_prop<-as.matrix(prop[1:34],col=1)
rownames(cal_spec)<-1:34
rownames(cal_prop)<-1:34
tst_spec<-as.matrix(spec[35:47,],ncol=226)
tst_prop<-as.matrix(prop[35:47],ncol=1)
rownames(tst_spec)<-35:47
rownames(tst_prop)<-35:47


wavelength<-seq(800,1700,length.out = 226)
matplot(wavelength,t(cal_spec),type="l")
text(1450,cal_spec[,164],labels=1:34)

outliers<-cal_spec[c(25,26),]
outliers_prop<-cal_prop[c(25,26)]

matplot(wavelength,t(cal_spec),type="l",col=2:5)
matlines(wavelength,t(tst_spec),col=1)
text(2004,tst_spec[,164],labels=1:13)
text(2004,cal_spec[,164],labels=1:34)

alcohol<-cal_spec[c(25,26),]
alcohol<-cbind(alcohol,cal_prop[c(25,26)])
tmp<-cbind(tst_spec[10:13,],tst_prop[10:13])
alcohol<-rbind(alcohol,tmp)



cal_spec<-cal_spec[-c(25,26),]
cal_prop<-cal_prop[-c(25,26),]
tst_spec<-tst_spec[-(10:13),]
tst_prop<-tst_prop[-(10:13),]


reg1<-pcr(cal_prop~cal_spec, ncomp=15,validation="CV", scale=T,x=T,y=T,model=T)
summary(reg1)

#keep 4 or 5 components
#okay to overfit a little bit with pcr
plot(reg1);abline(0,1,col=2)
yfitted<-cal_prop-reg1$fitted.values[,,5]
plot(cal_prop,yfitted)

ypred<-predict(reg1,tst_spec,ncomp=5,type="response")
rmsep<-sqrt((sum(ypred-tst_prop)^2)/8)

ypred2<-predict(reg1,tst_spec,ncomp=9,type="response")
rmsep2<-sqrt((sum(ypred2-tst_prop)^2)/8)



(reg1r<-eval_resids(reg1,5))

#leverage vs studentized residuals
plot(reg1r[[3]]~reg1r[[1]],col="blue",xlab="leverage",ylab="Studentized Residuals")
text(reg1r[[3]]~reg1r[[1]],labels=rownames(cal_prop))
abline(h=0,col="gray")
abline(h=c(-2,2),col="red")
abline(v=reg1r[[4]],col="red")


plot(reg1$scores[,1],reg1$Yscores[,1])




#pls
reg2<-plsr(cal_prop~cal_spec,ncomp=15,validation="CV",scale=T,x=T,y=T,method="simpls")
plot(reg2)
plot(reg2, "val")
plot(reg2, "loadings")

reg2r<-eval_resids(reg2,ncomp=)
plot(reg2r[[3]]~reg2r[[1]],col="blue",xlab="leverage",ylab="Studentized Residuals")
text(reg2r[[3]]~reg2r[[1]],labels=rownames(cal_spec))
abline(h=0,col="gray")
abline(h=c(-2,2),col="red")
abline(v=reg2r[[4]],col="red")

source("C:/Users/Beebe Group/Desktop/chemo/chemometrics/snippets/mvr_ext.R")
RMSEPCV(reg2)








##############################

data<-read.csv("data/tinxrfdata.csv")
prop<-as.matrix(data[,2],nrow=20)
spec<-as.matrix(data[,-c(1,2)],nrow=20)
rownames(prop)<-1:20
rownames(spec)<-1:20


matplot(t(spec),type="l")



a<-apply(spec,2,var)
matplot(a,type="l")

spec1<-spec[,a>1000]
matplot(t(spec1),type="l")


reg1<-pcr(prop~spec1,ncomp=10,validation="CV",scale=T,x=T,y=T,model=T)
summary(reg1)
plot(reg1);abline(0,1,col=2)

ypred<-predict(reg1,spec1,ncomp=4,type="response")
rmsec<-(sqrt(sum((ypred[1:20]-prop)^2)/19))
rmsec




#RMSEPCV from plsr/mvr/pcr
CV<-NULL
for(i in 1:reg1$ncomp){
CV<-append(CV,sqrt(sum((prop-reg1$validation$pred[,,i])^2)/20))
}
plot(CV,type="b",col=2,xlab="Number of Components",ylab="RMSEP - CV", main="Validation")
#%variance from plsr/mvr

#prop variance
prop_exp<-NULL
for(i in 1:reg1$ncomp){
a<-prop-reg1$fitted.values[,,i]
prop_exp<-append(prop_exp,100*(var(prop)-var(a))/var(prop))
}

#spec variance
a<-as.matrix(reg1$coefficients[,,1])
a<-prop%*%t(a)
a<-spec1-a

(sum(var(spec1))-sum(var(a)))/sum(var(spec1))



reg2<-plsr(prop~spec1,ncomp=15,validation="CV",scale=T,x=T,y=T,method="simpls")
plot(reg2)
plot(reg2, "val")
plot(reg2, "loadings",comps=1:6)

reg2r<-eval_resids(reg2,ncomp=2)
plot(reg2r[[3]]~reg2r[[1]],col="blue",xlab="leverage",ylab="Studentized Residuals")
text(reg2r[[3]]~reg2r[[1]],labels=rownames(spec),pos=1,col="grey")
abline(h=0,col="gray")
abline(h=c(-2,2),col="red")
abline(v=reg2r[[4]],col="red")







###############################33
#wheat#

require(pls)
source("C:/Users/Beebe Group/Desktop/chemo/scripts/eval_resids.R")
source("C:/Users/Beebe Group/Desktop/chemo/chemometrics/snippets/mvr_ext.R")

data<-read.csv("data/wheat.csv")
