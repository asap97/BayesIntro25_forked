days <- 1000*7
islands <- data.frame(island = c(1:10), popu = sample(1:500, 10, replace=FALSE),
                      visits = rep(0, 10))
current <- sample(1:10, 1)


for (day in c(1:days)){
  #add a visit to the current position
  islands[current,"visits"] <- islands[current,"visits"]+1
  #generate a proposal island
  proposal <- sample(c(1,-1), 1) + current
  #print(proposal)
  #make sure the proposal makes sense if it is at an "edge"
  if (proposal == 0){
    proposal <- 10
  }else if(proposal == 11){
    proposal <- 1
  }
  #calculate the ratio 
  ratio <- islands[proposal,"popu"]/islands[current,"popu"]
  #print(ratio)
  #update the island based on the ratio
  if(ratio>runif(1)){
    current <- proposal
  }
}
  
barplot(t(as.matrix(islands[, c("popu", "visits")])),
        beside = TRUE,
        col = c("skyblue", "orange"),
        names.arg = 1:nrow(islands),
        legend.text = c("Population", "Visits"),
        args.legend = list(x = "topright"),
        main = "Comparison of Population vs Visits per Island",
        xlab = "Island",
        ylab = "Count")
