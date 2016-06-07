#plot of kmeans for parameter number of logins, number of forum posts, number of Msg per user, 
#number of conversation requests, number of forum views, number of help views, number of page views web


kmdefine<-function() 
{
  Allpara<-cbind(members$numLogins,members$numForumPosts ,members$numMsgUser,members$convRequests,members$forumViews,members$helpViews,members$pageViewsWeb, members$pageViewsApp, members$activedays)
  Allpara_nazero<- Allpara[ apply(Allpara!=0, 1, any), , drop=FALSE] 
  rcorr(Allpara_nazero, type="pearson")
  sc <- kkmeans(Allpara_nazero, centers=6)
  a.cols <- rainbow(10,alpha=1)[sm9]
  X_for_column<-Allpara_nazero''
  X_for_column[, c(1] + 1
  X_for_column[,3]+1
  plot(X_for_column[,c(1,3)], xlab="Popularity", ylab="Activity", col=a.cols,log="xy")
  plot(Allpara_nazero[,c(3,9)], xlab="Activity", ylab="Loyality", col=a.cols)
  plot(Allpara_nazero[,c(9,1)],xlab="Loyality", ylab="Popularity",  col=a.cols)
  plot(Allpara_nazero[,c(1,2)], col=a.cols,log="xy")
  plot(Allpara_nazero[,c(2,3)], col=a.cols, log="xy")
  plot(Allpara_nazero[,c(3,4)], col=a.cols, log="xy")
  plot(Allpara_nazero[,c(4,5)], col=a.cols, log="xy")
  plot(Allpara_nazero[,c(5,6)], col=a.cols, log="xy")
  plot(Allpara_nazero[,c(6,7)], col=a.cols, log="xy")
  plot(Allpara_nazero[,c(1,3)], col=a.cols, log="xy")
  plot(Allpara_nazero[,c(3,9)], col=a.cols, log="xy")
  plot(Allpara_nazero[,c(1,5)], col=a.cols, log="xy")
  plot(Allpara_nazero[,c(1,6)], col=a.cols, log="xy")
  plot(Allpara_nazero[,c(1,7)], col=a.cols, log="xy")
  plot(Allpara_nazero[,c(2,3)], col=a.cols, log="xy")
  plot(Allpara_nazero[,c(2,4)], col=a.cols, log="xy")
  plot(Allpara_nazero[,c(2,5)], col=a.cols, log="xy")
  plot(Allpara_nazero[,c(2,6)], col=a.cols, log="xy")
  plot(Allpara_nazero[,c(2,7)], col=a.cols, log="xy")
  plot(Allpara_nazero[,c(3,5)], col=a.cols, log="xy")
  plot(Allpara_nazero[,c(3,6)], col=a.cols, log="xy")
  plot(Allpara_nazero[,c(3,7)], col=a.cols, log="xy")
  plot(Allpara_nazero[,c(4,6)], col=a.cols, log="xy")
  plot(Allpara_nazero[,c(4,7)], col=a.cols, log="xy")
  plot(Allpara_nazero[,c(5,6)], col=a.cols, log="xy")
  plot(Allpara_nazero[,c(5,7)], col=a.cols, log="xy")
  plot(Allpara_nazero[,c(6,7)], col=a.cols, log="xy")
  plot(Allpara_nazero[,c(1,3)], col=a.cols, log="xy")
  points(km.out$centers[,c(1,3)], col=1:3, pch=8, cex=2)
  hist(Allpara_nazero)
  utils::str(hist(Allpara_nazero, col = "black",prob=TRUE))
  km.out <- kmeans(Allpara_nazero[0:10000,c(1,3)], 6, nstart=100)
  plot.dist <- dist(Allpara_nazero[0:10000, c(1,3)]) #for converting multivariate scale in to two dimension
  plot.mds <- cmdscale(plot.dist)
  a.cols <- rainbow(7,alpha=1)[km.out$cluster]
  plot(plot.mds, col = a.cols,xlab = "X", ylab = "Y", log="xy")
  plot(sort(Allpara_nazero[,1]), 1-f(sort(Allpara_nazero[,1])), type="s", lwd=1,log="xy", ylab="P(X < x)",xlab="x",main="CDF")
  p <- ppoints(603765)
  x11(width=9, height=9, pointsize=12)
  par(fin=(4,2))
  
  par(fig=c(0, 1, 0, 1))
  par(pin=c(2.8,2.8))
  plot(log(quantile(Allpara_nazero[,1],p=p)),log(1-p),ylab="Pr(X > x)",xlab="x",main="Account Logins",col="blue",pch="+",ps=".5")
  plot(log(quantile(Allpara_nazero[,2],p=p)),log(1-p),ylab="Pr(X > x)",xlab="x",main="Forum Posts",col="blue",pch="+",ps=".5")
  plot(log(quantile(Allpara_nazero[,3],p=p)),log(1-p),ylab="Pr(X > x)",xlab="x",main="Messages exchanged",col="blue",pch="+",ps=".5")
  plot(log(quantile(Allpara_nazero[,4],p=p)),log(1-p),ylab="Pr(X > x)",xlab="x",main="Forum views",col="blue",pch="+",ps=".5")
  plot(log(quantile(Allpara_nazero[,5],p=p)),log(1-p),ylab="Pr(X > x)",xlab="x",main="Help views",col="blue",pch="+",ps=".5")
  plot(log(quantile(Allpara_nazero[,6],p=p)),log(1-p),ylab="Pr(X > x)",xlab="x",main="Page views from web",col="blue",pch="+",ps=".5")
  plot(log(quantile(Allpara_nazero[,7],p=p)),log(1-p),ylab="Pr(X > x)",xlab="x",main="Page views from App",col="blue",pch="+",ps=".5")
  plot(log(quantile(Allpara_nazero[,8],p=p)),log(1-p),ylab="Pr(X > x)",xlab="x",main="Active days",col="blue",pch="+",ps=".5")

  
  
}

