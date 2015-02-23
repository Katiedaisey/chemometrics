
    set.seed(1)
    nums<-rnorm(1000,0,sqrt(10))
    matrixmeans<-matrix(0,1000,4)
    matrixvars <-matrix(0,1000,4)
   
    colnames<-c(10,50,100,200)
    colnames(matrixmeans)<-colnames
    colnames(matrixvars)<-colnames
    
    k=1
    for(j in c(10,50,100,200)){
        means_all<-c()
        vars_all<-c()
       print(j)
       print(k)
        
        for(i in 1:1000){
            a<-sample(nums,j)
            meana<-mean(a)
            vara<-var(a)
            means_all<-append(means_all,meana)
            
            vars_all<-append(vars_all,vara)
        }
       print(length(matrixmeans[,k]))
       print(length(means_all))
        matrixmeans[,k]<-means_all
        matrixvars[,k]<-vars_all
        k<-k+1
        
    }
    
