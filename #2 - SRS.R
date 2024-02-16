############################# Echantillon par SRS ##############################

n<- 10000

creation_ech_SRS <- function(n,data){
  ech <- data[sample(nrow(data), size = n)]
  ech$poids <- nrow(data)/n
  return(ech)
}

ech_test <- creation_ech_SRS(n,data)
