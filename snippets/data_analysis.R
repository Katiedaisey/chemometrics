#data anaylsis




#principal components
#may wish to remove some variables
#may wish to scale=F or autoscale (scale=T) data
body<-read.csv("data/bodytype.csv")
body2<-as.matrix(body[,1:5])

boxplot(height~sex,data=body)


apply(body2,2,var)
body_a<-scale(body2)


body_a_pc<-princomp(body_a,cor=TRUE)
plot(body_a_pc$scores[,1],body_a_pc$scores[,2], col=as.factor(body$sex), xlab="PC#1 - 74.3% of Variance", ylab="PC#2 - 15.05% of Variance")
abline(h=0, col="gray")
abline(v=0, col="gray")
text(body_a_pc$scores[,1],body_a_pc$scores[,2],labels=rownames(body),cex=.5, pos=1)

plot(body_a_pc$loadings[,1],body_a_pc$loadings[,2])
abline(h=0, col="gray")
abline(v=0, col="gray")
text(body_a_pc$loadings[,1],body_a_pc$loadings[,2],labels=colnames(body[1:5]),cex=.5, pos=c(2,4,2,2,2))

plot(height~belly_size,data=body,col=as.factor(sex))
plot(belly_size~height,data=body,col=as.factor(sex))


library(scatterplot3d)


# First we scale the unknown data to the data in the PCA model
# This step is the most important- it avoids transmitting information
# on the unknown to the PCA when we build the model.
#
afgc<-scale(fgc) # scale the model data
# now use object attributes to get the centering and scaling data
scalef<- attributes(afgc)$"scaled:scale"
scalec<-attributes(afgc)$"scaled:center"
#and scale the unknown to the data in the PCA model
afgu<-scale(fgu,center=scalec,scale=scalef)








#NIPALS - located in chemometrics
pcnipals<- nipals(afg, a=3, it=50) # this only gives 3 PC axes
summary(pcnipals) #T-scores, P-loadings
plot(pcnipals$T[,1],pcnipals$T[,2],xlab="PC Score #1",ylab="PC Score #2",col=fglass[,10],main="NIPALS Scores Plot by Group")
abline(h=0,col="gray"); abline(v=0,col="gray")
plot(pcnipals$P[,1],pcnipals$P[,2],xlab="PC Loading #1",ylab="PC Loading #2",col="blue",main="NIPALS Loadings Plot by Variable")
abline(h=0,col="gray"); abline(v=0,col="gray")
text(pcnipals$P[,1],pcnipals$P[,2],label=colnames(fg), cex=0.6, pos=c(1,1,1,1,2,1,1,1))






variances <-ants_c_pc$sdev^2/(nrow(ants_c)-1)
totvariances<- sum(variances)
relvars<- variances/totvariances
vars<- 100*round(relvars, digits=3)

par(mfrow=c(1,1))
plot(ants_c_pc$scores[,1],ants_c_pc$scores[,2], col=as.factor(ants_out$Type), xlab=paste("PC#1 - ",vars[1],"% of Variance"), ylab=paste("PC#2 - ",vars[2],"% of Variance"), main="PC of Fire Ants")
abline(h=0, col="gray")
abline(v=0, col="gray")





#cls/ils
data<-read.csv("data/IR_spectra.csv")

#visably check what columns contain properities and spectra

data2<-data
data<-data[-7,]
prop<-as.matrix(data[,2:3],ncol=2)
spec<-as.matrix(data[,-(1:3)],ncol=601)

colnames(spec)[c(1,dim(spec)[[2]])]

wavelengths<-seq(1325.25,1475.25,length.out=dim(spec)[[2]])
matplot(wavelengths,t(spec),type="l",lty=1)

#CLS
kmat<-solve(t(prop)%*%prop)%*%t(prop)%*%spec
matplot(wavelengths,t(kmat),type="l",lty=1)

pred<-spec%*%t(kmat)%*%solve(kmat%*%t(kmat))



#ILS - must choose at most 6 channels
a<-c(195,395,411,435,585)
matplot(1:601,t(kmat),type="l",lty=1)
abline(v=a)


pick<-spec[,a]

kmatils<-solve(t(pick)%*%pick)%*%t(pick)%*%prop

predils<-pick%*%kmatils
plot(prop[,1],prop[,1]-predils[,1])
abline(h=0)
plot(prop[,2],prop[,2]-predils[,2])
abline(h=0)


#rmsresid
(a<-sqrt(apply(((prop-predils)^2),2,mean)))
(b<-sqrt(apply(((prop-pred)^2),2,mean)))


