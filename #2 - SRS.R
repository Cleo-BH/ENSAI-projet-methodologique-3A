############################# Echantillon par SRS ##############################
# Principe : On tire un échantillon de n individus aléatoirement et sans remise,
# puis on définit les poids (ici N/n).

n<- 10000

creation_ech_SRS <- function(n,data){
  ech <- data[sample(nrow(data), size = n)]
  ech$poids <- nrow(data)/n
  return(ech)
}

ech_test <- creation_ech_SRS(n,data)
