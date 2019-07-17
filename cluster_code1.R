###Clustering clustering

##Correlation clustering
for (i in 1:length(countries)){
  state <- scaled_imp[countries[i]]
  
  
  state <- t(as.matrix(state))
  #print(countries[i])
  #print(statsNA(as.vector(state)))
  #correlation dissimilarity
  cor_rel <- diss(state, "COR")
  #cluster
  corr_clust <- hclust(cor_rel)
  plot(corr_clust)
}


##Time series clusting
#convert to ts, and then impute data
fiveYears <- ts(as.matrix(fiveYears), frequency = 4)
interpolate <- na.interpolation(fiveYears)
locf <- na.locf(fiveYears)
seasadj <- na.seadec(fiveYears)

fiveYears <- t(as.matrix(locf))
#corelllation dissimilarity
corr_dis <- diss(fiveYears, "COR")
summary(corr_dis)
##which are the most different
sort(rowMeans(as.matrix(corr_dis)))
#cluster
C1 <- hclust(corr_dis, "median")
plot(C1)

#disimmilarity by "frECHET" DISTANCE
frechet_dis <- diss(fiveYears, "FRECHET")
#cluster 
C2 <- hclust(frechet_dis)
plot(C2)

##Dynamic time warpiong
dtwarp <- diss(fiveYears, "DTWARP")
#cluster
C3 <- hclust(dtwarp)
plot(C3)


##dissimmilarity by Integrated Periodgram Distance
int.perp <- diss(fiveYears, "INT.PER")
#cluster
C4 <- hclust(int.perp)
plot(C4)






##DTW clustering 
for (i in 1:length(countries)){
  state <- ts(as.matrix(places[countries[i]]))
  
  inter_state <- na.interpolation(state)
  locf_state <- na.locf(state)
  seasadj <- na.seadec(state)
  
  state <- t(as.matrix(inter_state))
  
  #disimilarity
  dtw_dis <- diss(state, "DTWARP")
  #cluster
  plot(hclust(dtw_dis, "average"))
  
}

##Intergrated Periodgram Distance
for (i in 1:length(countries)){
  state <- ts(as.matrix(places[countries[i]]))
  
  inter_state <- na.interpolation(state)
  locf_state <- na.locf(state)
  seasadj <- na.seadec(state)
  
  state <- t(as.matrix(locf_state))
  
  ##disimilarity
  int_per <- diss(state, "INT.PER")
  ##cluster
  plot(hclust(int_per, "mcquitty"))
}