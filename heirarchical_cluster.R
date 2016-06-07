#plot of kmeans for all parameters agasint one another


kheiarchical<-function() {
  Allpara<-cbind(members$numLogins,members$numForumPosts ,members$numMsgUser,members$convRequests,members$forumViews,members$helpViews,members$pageViewsWeb)
  Allpara_nazero<- Allpara[ apply(Allpara!=0, 1, any), , drop=FALSE] 
  hc <- hclust(dist(Allpara_nazero[1000:1200,]), "average")
  plot(hc ,main="average", xlab="", sub="", cex=.9)
  cutree(hc,6)
 }


