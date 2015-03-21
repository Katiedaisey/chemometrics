#data manipulation



#Time and Date

a<-data[,c(1,2)]
a[,1]<-as.character(a[,1])
a[,2]<-as.character(a[,2])
a[,1]<-as.Date(a[,1],format="%d/%m/%Y")
a[,3]<-paste(a[,1],a[,2],sep=" ")
day<-strptime(a[,3],format="%Y-%m-%d %H:%M:%S")