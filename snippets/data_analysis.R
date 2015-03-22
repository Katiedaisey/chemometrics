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
afgu<-scale(fgu,center=scalec,scale=scalef