
library(cluster)
library(HSAUR)
library(kernlab)

library(cluster)
library(HSAUR)
data(pottery)
k   <- kmeans(Allpara_nazero,7)
dissE <- daisy(Allpara_nazero[50000:65000,]) 
dE2   <- dissE^2
sk2   <- silhouette(sm4[50000:60000], dE2)
plot(sk2)
aa=silhouette(sc,dist(Allpara_nazero[1:1000,])) 

clusts <- sort(unique(k$cluster))
coeffs <- rep(-2,length(clusts))
for(i in 1:length(k$cluster)){
  cdist <- rep(0,length(clusts))
  for(j in clusts){
    cdist[j] <- dist(rbind(k$centers[j,],Allpara_nazero[i,]))
  }
  a <- cdist[k$cluster[i]]
  b <- min(cdist[clusts != k$cluster[i]])
  coeffs[i] <- (b-a) / max(b,a)
}
return(coeffs)
}

