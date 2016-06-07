
#plot of users
pieplot<-c(10956, 9339, 4191, 2762, 3858, 14671, 21308)
lbls <- c("I (16.33%)", " II (13.92%)", "III (6.24%)", " IV (4.11%)", " V (5.75%) ","VI (21.87%)", "VII (31.6%)")
pie(pieplot, labels = lbls, main="Cluster of Users")

#plot of users
pieplot<-c(4165,5689,3378,2027,1512)# 0.005 1.507739 k=6: 1.681246
lbls <- c("I (16.33%)", " II (13.92%)", "III (6.24%)", " IV (4.11%)", " V (5.75%)")
pie(pieplot, labels = lbls, main="Cluster of Users")

#plot of users 
pieplot<-c(2652,1215,5868,3104,3932)# 0.007 1.501541 k=6: 1.673561
lbls <- c("I (16.33%)", " II (13.92%)", "III (6.24%)", " IV (4.11%)", " V (5.75%)")
pie(pieplot, labels = lbls, main="Cluster of Users")



#plot of users
pieplot<-c(2011,3883,1460,3280,1313,4824) #0.009  1.682574 k=5 1.457518
lbls <- c("I (16.33%)", " II (13.92%)", "III (6.24%)", " IV (4.11%)", " V (5.75%)", "VI")
pie(pieplot, labels = lbls, main="Cluster of Users")

pieplot<-



pieplot<-c(1131,5786,4033,1221,2076,2524) #0.01 1.626085 k=5 1.488955
lbls <- c("I (16.33%)", " II (13.92%)", "III (6.24%)", " IV (4.11%)", " V (5.75%)","VIdfdf")
pie(pieplot, labels = lbls, main="Cluster of Users")


pieplot2<-c(0.05479346 ,0.05730722, 0.07649877,6.17530411,0.26762625)
lbls2 <- c("1st ", "2nd", "3rd", "4th Cluster", "5th"pie(pieplot2, labels = lbls2, main="Visits to the website")

pieplot3<-c(17.847334,34.905631 , 56.224561 ,374.972734 ,1.121493)
lbls3 <- c("1st ", "2nd", "3rd", "4th Cluster", "5th")
pie(pieplot3, labels = lbls3, main="Message exchanges")

pieplot4<-c(0.06327065 ,0.06814913, 0.09638554,10.03121414,0.36954135)
lbls4 <- c("----------------------1st", "----2nd", "----------3rd", "4th Cluster", "5th")
pie(pieplot4, labels = lbls4, main="Active Days")
#interaction plot
barplot(centers(sm4),horiz=TRUE, names.arg=c("I","II","III","IV","V","VI","VII" ), cex.names=0.4)
variance<-c(3.294835,3.322186,3.346879,3.749639, 4.041171,10.4248,71.82049)
barplot(variance, horiz=TRUE,names.arg=c("I","II","III","IV","V","VI","VII" ), cex.names=0.4, col="blue", main="percent initation form clust")

library(corrplot)
M <- cor(Allpara_nazero)
corrplot(M, method = "circle")
c<-as.data(Allpara_nazero)
library(scatterplot3d)
attach(Allpara_nazero)
scatterplot3d(wt,disp,mpg, pch=16, highlight.3d=TRUE,
              type="h", main="3D Scatterplot") 
persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue")
persp3d(Allpara_nazero[,1], Allpara_nazero[,3], Allpara_nazero[,9], aspect=c(1, 1, 0.5), col = "lightblue",
        xlab = "X", ylab = "Y", zlab = "Sinc( r )")
colnames(Allpara_nazero) = c("A", "B", "C","D","E","F","G","H","I")
alldata<-as.data.frame(Allpara_nazero)
library(scatterplot3d)
alldata[,c("A")]
attach(alldata)
a.cols <- rainbow(4,alpha=1)[sm3]
plot3d(Allpara_nazero[,c(1,3,9)],col=a.cols)
s3d<-scatterplot3d(A,I,C,pch=16, highlight.3d=TRUE,
              type="h",col=a.cols, main="3D Scatterplot")
library(Rcmdr)
fit <- lm(C ~ A+I) 
s3d$plane3d(fit)
scatter3d(A,I,C)
