
RMSEPCV<-function(reg,plot=T){

#RMSEPCV from plsr/mvr/pcr
CV<-NULL
for(i in 1:reg$ncomp){
    CV<-append(CV,sqrt(sum((prop-reg$validation$pred[,,i])^2)/20))
}
#%variance from plsr/mvr
#prop variance
prop_exp<-NULL
for(i in 1:reg$ncomp){
    a<-prop-reg$fitted.values[,,i]
    prop_exp<-append(prop_exp,100*(var(prop)-var(a))/var(prop))
}

#spec variance
x_exp<-NULL
a<-0
for(i in 1: reg$ncomp){
    a<-a+(100*reg$Xvar[i]/reg$Xtotvar)
    x_exp<-append(x_exp,a)
}




plot1<-plot(CV,type="b",col="red",xlab="Number of Components", ylab="RMSEP CV", main="PLS")
plot2<-plot(prop_exp,type="b",col="green",
            xlab="Number of Components", ylab="% Variance Explained",main=paste0("Variance in " ,reg$terms[[2]] ))
plot3<-plot(x_exp,type="b",col="green",
            xlab="Number of Components", ylab="% Variance Explained",main=paste0("Variance in " ,reg$terms[[3]] ))

#usually not helpful
#plot4<-plot(reg$fitted.values[,,1] ~ get(as.character(reg$terms[[2]])), xlab="Measured Level",ylab="Predicted Level", main="PLS Regression- Scaled");abline(0,1,col="red")
#points(reg1$fitted.values[,,5] ~ get(as.character(reg1$terms[[2]])),col=6)




#adjust this - this will find smallest possible change
CVmin<-NULL
for(i in 1:(length(CV)-1)){
    a<-CV[i]-CV[(i+1)]
    CVmin<-append(CVmin,a)
}


#rownames(CVmin)<-2:(length(CV))
comp<-match(min(CVmin),CVmin)
comp2<-match(min(CV),CV)



Y<-get(as.character(reg$terms[[2]]))
index<-1:length(Y)
plot5<-matplot(index,Y,type="l",ylab="Property",xlab="Index",main="PLS Regression- Scaled")
matpoints(index,reg$fitted.values[,,comp2],type="p",col="blue", pch=1)

plot6<-matplot(index,reg$residuals[,,comp2],type="l",ylab="Residuals",col="blue",xlab="Index",main="Liquid-Fed Ceramic Melter: PLS Regression- Scaled")
abline(h=0, col="gray")

plot7<-plot((1:dim(reg$coefficients)[[1]]),reg$coefficients[,,comp],type="l",col="red",xlab="Variable No.", ylab="Coefficient", main="PLS Regression, Scaled",)
abline(h=0, col="gray")


#return
reg_RMSEP<-list(CV,prop_exp,x_exp,CVmin,comp,comp2)
names(reg_RMSEP)<-c("RMSEPCV",paste0("Var_explained_",reg$terms[[2]]),paste0("Var_explained_",reg$terms[[3]]),"CV_min","suggested_comp_pcr","suggested_comp_CV")


return(reg_RMSEP)
if(plot==T){
    return(plot1)
    return(plot2)
    return(plot3)
    return(plot5)
    return(plot6)
    return(plot7)
    }
}