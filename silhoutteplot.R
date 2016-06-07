library(ggplot2)
library(cluster)
library(kernlab)
nc<-9

clusts <- sort(unique(sm))
coeffs <- rep(-2,length(clusts))
meanCoeff<-rep(-2,length(clusts))
for(i in 1:length(sm)){
  cdist <- rep(0,length(clusts))
  for(j in clusts){
    cdist[j] <- dist(rbind((centers(sm)[j,]),Allpara_nazero[i,]))
  }
  a <- cdist[sm[i]]
  b <- min(cdist[clusts != sm[i]])
  coeffs[i] <- (b-a) / max(b,a)
}
meanCoeff[1]<-mean(coeffs)

clusts <- sort(unique(sm2))


for(i in 1:length(sm2)){
  cdist <- rep(0,length(clusts))
  for(j in clusts){
    cdist[j] <- dist(rbind((centers(sm2)[j,]),Allpara_nazero[i,]))
  }
  a <- cdist[sm2[i]]
  b <- min(cdist[clusts != sm2[i]])
  coeffs[i] <- (b-a) / max(b,a)
  
}
meanCoeff[2]<-mean(coeffs)

clusts <- sort(unique(sm3))

for(i in 1:length(sm3)){
  cdist <- rep(0,length(clusts))
  for(j in clusts){
    cdist[j] <- dist(rbind((centers(sm3)[j,]),Allpara_nazero[i,]))
  }
  a <- cdist[sm3[i]]
  b <- min(cdist[clusts != sm3[i]])
  coeffs[i] <- (b-a) / max(b,a)
  
}


meanCoeff[3]<-mean(coeffs)
clusts <- sort(unique(sm4))

for(i in 1:length(sm4)){
  cdist <- rep(0,length(clusts))
  for(j in clusts){
    cdist[j] <- dist(rbind((centers(sm4)[j,]),Allpara_nazero[i,]))
  }
  a <- cdist[sm4[i]]
  b <- min(cdist[clusts != sm4[i]])
  coeffs[i] <- (b-a) / max(b,a)
  
}

meanCoeff[4]<-mean(coeffs)

clusts <- sort(unique(sm5))

for(i in 1:length(sm5)){
  cdist <- rep(0,length(clusts))
  for(j in clusts){
    cdist[j] <- dist(rbind((centers(sm5)[j,]),Allpara_nazero[i,]))
  }
  a <- cdist[sm5[i]]
  b <- min(cdist[clusts != sm5[i]])
  coeffs[i] <- (b-a) / max(b,a)
}
meanCoeff[5]<-mean(coeffs)

clusts <- sort(unique(sm6))

for(i in 1:length(sm6)){
  cdist <- rep(0,length(clusts))
  for(j in clusts){
    cdist[j] <- dist(rbind((centers(sm6)[j,]),Allpara_nazero[i,]))
  }
  a <- cdist[sm6[i]]
  b <- min(cdist[clusts != sm6[i]])
  coeffs[i] <- (b-a) / max(b,a)
  
}
meanCoeff[6]<-mean(coeffs)

clusts <- sort(unique(sm7))

for(i in 1:length(sm7)){
  cdist <- rep(0,length(clusts))
  for(j in clusts){
    cdist[j] <- dist(rbind((centers(sm7)[j,]),Allpara_nazero[i,]))
  }
  a <- cdist[sm7[i]]
  b <- min(cdist[clusts != sm7[i]])
  coeffs[i] <- (b-a) / max(b,a)
  
}
meanCoeff[7]<-mean(coeffs)

clusts <- sort(unique(sm8))

for(i in 1:length(sm8)){
  cdist <- rep(0,length(clusts))
  for(j in clusts){
    cdist[j] <- dist(rbind((centers(sm8)[j,]),Allpara_nazero[i,]))
  }
  a <- cdist[sm8[i]]
  b <- min(cdist[clusts != sm8[i]])
  coeffs[i] <- (b-a) / max(b,a)
}
meanCoeff[8]<-mean(coeffs)

clusts <- sort(unique(sm9))

for(i in 1:length(sm9)){
  cdist <- rep(0,length(clusts))
  for(j in clusts){
    cdist[j] <- dist(rbind((centers(sm9)[j,]),Allpara_nazero[i,]))
  }
  a <- cdist[sm9[i]]
  b <- min(cdist[clusts != sm9[i]])
  coeffs[i] <- (b-a) / max(b,a)
  
}
meanCoeff[9]<-mean(coeffs)

plot(1:nc, meanCoeff, type="b", xlab="Number of Clusters",ylab="Silhouette Coefficient ")



