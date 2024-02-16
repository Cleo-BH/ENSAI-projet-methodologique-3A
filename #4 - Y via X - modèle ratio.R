#################### Prédiction de Y par X - modèle ratio ######################

# Principe : On construit un modèle ratio sur l'échantillon à partir des X puis 
# on l'applique sur la base de sondage.

# Hypothèse : Non réponse aléatoire 
# Besoin de connaître X pour tout le monde : Oui
# Autres : Le modèle est Y~Ratio avec Ratio une variable qu'il faut construire 
# sur l'échantillon et dans la base de sondage.

modele_ratio <- function(ech, base){
  #On traite Ratio pour avoir seulement des valeurs strictement positives.
  controle <- -min(min(ech$Ratio),0)+0.1
  ech_controle <- ech %>% mutate(Ratio = Ratio+controle)
  
  regression_ratio <- lm(Y~Ratio, data=ech_controle %>% filter(!is.na(mode)),weights = Ratio^{-1})
  Y_hat <- predict(regression_ratio, newdata = base)
  return(sum(Y_hat))
}