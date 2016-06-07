set.seed(25)
Allpara<-cbind(members$numLogins,members$numForumPosts ,members$numMsgUser,members$convRequests,members$forumViews,members$helpViews,members$pageViewsWeb)
Allpara_nazero<- Allpara[ apply(Allpara!=0, 1, any), , drop=FALSE] 
Login_Posts<-cbind(members$numLogins,members$numForumPosts)
km.out=kmeans(Login_Posts,6,nstart =20)
a.cols <- rainbow(6)[km.out$cluster]
plot(Login_Posts, col=(km.out$cluster +1), main="K-Means Clustering Results with K=6", xlab="number of logins", ylab="number of Forum posts", pch=20, cex=2)
Posts_Msguser<-cbind(members$numForumPosts, members$numMsgUser)
km.out=kmeans(Posts_Msguser,6,nstart =20)
a.cols <- rainbow(6)[km.out$cluster]
plot(Posts_Msguser, col=(km.out$cluster +1), main="K-Means Clustering Results with K=6", xlab="number of Forum posts", ylab="number of msg per user", pch=20, cex=2, log="y")
Msguser_Requests<-cbind(members$numMsgUser, members$convRequests)
km.out=kmeans(Msguser_Requests,6,nstart =20)
a.cols <- rainbow(6)[km.out$cluster]
plot(Msguser_Requests, col=(km.out$cluster +1), main="K-Means Clustering Results with K=6", xlab="number of msg per user", ylab="number of conversation requests", pch=20, cex=2, log="y")
Requests_ForumViews<-cbind(members$convRequests,members$forumViews )
km.out=kmeans(Requests_ForumViews,6,nstart =20)
a.cols <- rainbow(6)[km.out$cluster]
plot(Requests_ForumViews, col=(km.out$cluster +1), main="K-Means Clustering Results with K=6", xlab="number of conversation requests", ylab="number of forum views", pch=20, cex=2, log="y")
Logins_PageViews<-cbind(members$numLogins,members$pageViewsWeb)
km.out=kmeans(Logins_PageViews,6,nstart =20)
a.cols <- rainbow(6)[km.out$cluster]
plot(Logins_PageViews, col=(km.out$cluster +1), main="K-Means Clustering Results with K=6", xlab="number of logins", ylab="number of page views", pch=20, cex=2, log="y")



