######################### Comparaisons méthodes cas 1 ##########################

# 1) Définition du nombre de simulations #######################################

n_simul <- 1000

# 2) Evaluations par méthode ###################################################

rm(ech_test)

## a) HT sur les répondants ####################################################

HT_sur_répondants <- erreur_methode(
  data,
  function(taille_ech,data){creation_ech_SRS(taille_ech,data)},
  function(ech){simulation_nrprop(ech)},
  n,
  n_simul,
  function(ech){HT_repondants(ech)}
  )

## b) Prédiction de Y via X - modèle linéaire ##################################

Y_selon_X_linéaire <- erreur_methode(
  data,
  function(taille_ech,data){creation_ech_SRS(taille_ech,data)},
  function(ech){simulation_nrprop(ech)},
  n,
  n_simul,
  function(ech){modele_lineaire(ech,base,Y~X)}
)

## c) Prédiction de Y via X - modèle homogène 2 strates ########################

data$X_strate <- data$X
base$X_strate <- base$X
Y_selon_X_2_strates <- erreur_methode(
  data,
  function(taille_ech,data){creation_ech_SRS(taille_ech,data)},
  function(ech){simulation_nrprop(ech)},
  n,
  n_simul,
  function(ech){modele_par_strate(ech,base,2)}
)

# 3) Mise en forme des résultats ###############################################

base_resultats <- as.data.frame(rbind(HT_sur_répondants,
                                      Y_selon_X_linéaire,
                                      Y_selon_X_2_strates))

colnames(base_resultats) <- c("Biais","Variance","MSE","RMSE")
base_resultats
