############# Prédiction de Y par X - modèle homogène par strate ###############

# Principe : On construit un modèle homogène par strate sur l'échantillon à 
# partir des X puis on l'applique sur la base de sondage. La seule manière de 
# construire les strates et de prendre les fractiles d'une unique variable 
# X_strate (à coder).

# Hypothèse : Non réponse aléatoire
# Besoin de connaître X pour tout le monde : Oui
# Autres : Nécessite d'avoir codé X_strate dans la base de sondage et soit dans 
# data (pour erreur_methode) soit dans l'échantillon.

modele_par_strate <- function(ech,base,n_strates){
  
  #Calcul des fractiles :
  probs <- c()
  for(i in 0:n_strates){
    probs <- c(probs,i/n_strates)
  }
  limites <- quantile(base$X_strate,probs)
  
  #Construction des strates sur les deux tables :
  base$Strate <- 0
  ech$Strate <- 0
  for(i in 1:n_strates){
    borne_inf <- limites[i]
    borne_sup <- limites[i+1]
    ech$Strate[borne_inf<=ech$X_strate & ech$X_strate<=borne_sup] <- i
    base$Strate[borne_inf<=base$X_strate & base$X_strate<=borne_sup] <- i
  }
  
  #Partie générique pour connaître la moyenne par strate :
  Y_hat <- 0
  for(i in 1:n_strates){
    ech_stratei <- ech %>% 
      filter(!is.na(mode)) %>% 
      filter(Strate==i)
    nb_stratei <- nrow(base %>% filter(Strate==i))
    Y_hat <- Y_hat + nb_stratei * mean(ech_stratei$Y)
  }
  return(Y_hat)
}

# Total estimé sur l'échantillon test :
ech_test$X_strate<-ech_test$X
base$X_strate <- base$X
modele_par_strate(ech_test,base,2)
