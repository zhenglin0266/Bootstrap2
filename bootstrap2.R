##Problem description: Write functions pivotalMean(x, conf=0.99, B=1e3) and StudentPivotalMean(x, conf=0.99, B=1e3) 
##that take in a numerical vector x and produce a bootstrap pivotal and a 
##bootstrap Studentized pivotal (respectively) conffidence interval for the mean.

Diff<-matrix(0,1000,12)
num<-matrix(0, 4, 3)
for(h in 1:1000){
  for(k in 1:4){
    n<-c(10, 20, 50, 100)
    x<-rnorm(n[k])
    ##Construct the function pivotalMean##
    pivotalMean<-function(x, conf, B){
      bound<-c(0,0)
      theta<-mean(x)
      boot <- numeric()
      quan<-c(0,0)
      for(i in 1: B){
        boot[i] <- mean(sample(x, size=length(x), replace=T))
      }
      quan[1]<-quantile(boot, 1-(1-conf)/2)
      quan[2]<-quantile(boot, (1-conf)/2)
      bound[1]<-2*theta-quan[1]
      bound[2]<-2*theta-quan[2]
      return(bound)
    }
    
    ##Construct the function StudentPivotalMean##
    StudentPivotalMean<-function(x, conf, B){
      bound <- c(0,0)
      theta <- mean(x)
      quan <- c(0,0)
      t<-rep(0, B)
      for(i in 1: B){
        xx <- sample(x, size=length(x), replace=T)
        t[i] <- (mean(xx)-theta)/(sd(xx)/(sqrt(n[k])))
      }
      quan[1] <- quantile(t, (1-conf)/2)
      quan[2] <- quantile(t, 1-(1-conf)/2)
      bound[1] <- theta + quan[1]*sd(x)/(sqrt(n[k]))
      bound[2] <- theta + quan[2]*sd(x)/(sqrt(n[k]))
      return(bound)
    }
    
    ##Construct regular student confidence interval##
    CI<-matrix(0,3,2)
    CI[3,1]<- mean(x)+qt(0.01/2, n[k]-1)*sd(x)/(sqrt(n[k]))
    CI[3,2]<- mean(x)-qt(0.01/2, n[k]-1)*sd(x)/(sqrt(n[k]))
    
    ##Simulation##
    CI[1,] <-pivotalMean(x, 0.99, 1000)
    CI[2,] <-StudentPivotalMean(x, 0.99, 1000)
    
    for(m in 1:3){
      if(0>CI[m,1] && 0<CI[m,2]){
        num[k, m] <-num[k, m]+1
      }
    }
    for(l in 1:3){
      Diff[h,(3*(k-1)+l)]<-CI[l,2]-CI[l,1]
    }
  }
  
}
##Output the table of the achieved confidence levels.
View(num)

##Draw the graphs of the interval lengths.
data<-data.frame(Diff[,1],Diff[,2],Diff[,3],Diff[,4],Diff[,5],Diff[,6],Diff[,7],Diff[,8],Diff[,9],Diff[,10],Diff[,11],Diff[,12])
box<-boxplot(data, col=c(1,2,3), at=c(1,2,3,6,7,8,11,12,13,16,17,18),xaxt="n",ylim=c(0,5),ylab="Hight")
axis(side=1, c(2,7,12,17), labels=c("n=10","n=20","n=50","n=100"))
bname<-rep(c("piv","stud","regu"),4)
text(c(1,2,3,6,7,8,11,12,13,16,17,18),rep(0,12), rep(bname[1:3],4))
##Firstly, for the achieved confidence levels, pivotal confidence intervals usually have the 
##lowest levels, and regular student confidence intervals usually have the highest levels.
##Secondly, for the achieved confidence levels, as n increases, the achieved
##confidence levels increase.
##Thirdly, for the lengths of the intervals, as n increases, the lengths of the
##intervals decrease.
##Fourthly, as n goes bigger, the differences of the interval lengths between 
##the three methods go smaller.
##Fifthly, when n is small, the lengths of studentized pivotal confidence intervals
##and regular student confidence intervals are usually larger than that of pivotal
##confidence intervals.

