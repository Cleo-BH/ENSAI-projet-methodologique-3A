################### Prédiction de Y par X - modèle linéaire ####################

# Principe : On construit un modèle linéaire sur l'échantillon à partir des X
# puis on l'applique sur la base de sondage.

# Hypothèse : Non réponse aléatoire
# Besoin de connaître X pour tout le monde : Oui

modele_lineaire <- function(ech, base, formula){
  regression_lineaire <- lm(formula, data=ech %>% filter(!is.na(mode)))
  Y_hat <- predict(regression_lineaire, newdata = base)
  return(sum(Y_hat))
}

#Total estimé sur l'échantillon test :
modele_lineaire(ech_test, base, Y~X1)
modele_lineaire(ech_test, base, Y~X2)
modele_lineaire(ech_test, base, Y~X1+0)
modele_lineaire(ech_test, base, Y~X1+X2)