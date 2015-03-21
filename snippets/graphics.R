##plotting and graphics




#plot layout
#central legend
#plots
par(mfrow=c(1,1),mar=c(2,2,2,2))
m<-matrix(c(1,2,3,4,5,5),3,2,byrow=TRUE)
layout(m,heights=c(.45,.45,.1))

#10 observations
hist(sample_meansu[,1],freq=F,main="Means of 10 uniform observations",xlab="mean")
lines(density(sample_meansu[,1]),col="navy",lwd=3)
lines(density(dataset2),col="red",lwd=3)

#50 observations
hist(sample_meansu[,2],freq=F,main="Means of 50 uniform observations",xlab="mean")
lines(density(sample_meansu[,2]),col="navy",lwd=3)
lines(density(dataset2),col="red",lwd=3)

#100 observations
hist(sample_meansu[,3],freq=F,main="Means of 100 uniform observations",xlab="mean",ylim=(c(0,8)))
lines(density(sample_meansu[,3]),col="navy",lwd=3)
lines(density(dataset2),col="red",lwd=3)

#200 observations
hist(sample_meansu[,4],freq=F,main="Means of 200 uniform observations",xlab="mean")
lines(density(sample_meansu[,4]),col="navy",lwd=3)
lines(density(dataset2),col="red",lwd=3)

par(mar=c(0,0,0,0))
plot(5, type = "n", axes=FALSE, xlab="", ylab="")
legend("center",inset=0,legend=c("Population","Sample means"),col=c("red","blue"), lty=1, lwd=5,cex=1,horiz=T,bty="n")


par(mfrow=c(1,1))








#custom axis
    #specify labels for tick marks
    xaxis.labels<-seq(0,2300,by=100)
    #specify where tick marks should go
    xaxis.at<-seq(0,288,length.out=(24))
    #automates display of tick marks to fit area of plot
    
    
    #xaxt='n' supresses standard axis
    plot(interval_means$interval,interval_means$interval_mean,xlab="Interval",ylab="Average Number of Steps",type="l",main="Average Number of Steps throughout the Day",xaxt='n')
    
    #side = 1-bottom,2-left,3-top,4-right
    axis(1,at=xaxis.at,labels=xaxis.labels)







#write graphic to file

#defaults w=h=480

...

dev.off()


#subplots and insets
# datasets
d0 <- data.frame(x = rnorm(150, sd=5), y = rnorm(150, sd=5))
d0_inset <- data.frame(x = rnorm(1500, sd=5), y = rnorm(1500, sd=5))

# ranges
xlim <- range(d0$x)
ylim <- range(d0$y)

# plot
plot(d0)

# calculate position of inset
plotdim <- par("plt")
xleft    = plotdim[2] - (plotdim[2] - plotdim[1]) * 0.25
xright   = plotdim[2]  #
ybottom  = plotdim[4] - (plotdim[4] - plotdim[3]) * 0.25  #
ytop     = plotdim[4]  #

# set position for inset
par(
    fig = c(xleft, xright, ybottom, ytop)
    , mar=c(0,0,0,0)
    , new=TRUE
)

# add inset
plot(d0_inset, col=2) # inset bottomright




#defults
par(mar=c(5,4,4,2)+0.1,fig=c(0,1,0,1))


#inset - top left

# calculate position of inset
plotdim <- par("plt")
xleft    = plotdim[1] + 0.02
xright   = plotdim[1] + (plotdim[2] - plotdim[1])*.30 + 0.02
ybottom  = plotdim[4] - (plotdim[4] - plotdim[3])*.40 - 0.03
ytop     = plotdim[4] - 0.03  #

# set position for inset
par(
    fig = c(xleft, xright, ybottom, ytop)
    , mar=c(0,0,0,0)
    , new=TRUE
)






#rotate axis labels
par(mfrow=c(1,2),srt=45)
fat<-danger$F.mean[order(danger$F.mean,decreasing=T)]
fat<-fat[1:6]
barplot(fat,axisnames=F, ylab="Mean number per event")
text(c(1:6)*1.2-.2,par("usr")[3]-.01,labels=rownames(fat),srt=45,xpd=T,cex=.5,adj=1)

inj<-danger$I.mean[order(danger$I.mean, decreasing=T)]
inj<-inj[1:6]
barplot(inj,axisnames=F, ylab="Mean number per event")
text(c(1:6)*1.2-.2,par("usr")[3]-0.01,labels=rownames(inj),srt=45,xpd=T,cex=.5,adj=1)
