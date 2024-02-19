######################### Comparaison des méthodes TIC #########################

# 1) Définition du nombre de simulations #######################################

n_simul <- 1000

# 2) Evaluations par méthode ###################################################

rm(ech_test)

## a) HT sur les répondants ####################################################

set.seed(194)
HT_sur_répondants <- erreur_methode(
  data,
  function(taille_ech,data){creation_ech_SRS(taille_ech,data)},
  function(ech){simulation_nrTIC(ech)},
  n,
  n_simul,
  function(ech){HT_repondants(ech)}
)

## b) Prédiction de Y via X - modèle linéaire ##################################

set.seed(194)
Y_selon_X_linéaire <- erreur_methode(
  data,
  function(taille_ech,data){creation_ech_SRS(taille_ech,data)},
  function(ech){simulation_nrTIC(ech)}, 
  n,
  n_simul,
  function(ech){modele_lineaire(ech,base,Y~X1+X3)}
)


## c) Prédiction de Y via X - modèle homogène 2 strates ########################

data$X_strate <- data$X1
base$X_strate <- base$X1
Y_selon_X_2_strates <- erreur_methode(
  data,
  function(taille_ech,data){creation_ech_SRS(taille_ech,data)},
  function(ech){simulation_nrTIC(ech)},
  n,
  n_simul,
  function(ech){modele_par_strate(ech,base,2)}
)

## d) Re pondération sans les modes ###############################################

set.seed(194)
Y_repondéré_sans_modes <- erreur_methode(
  data,
  function(taille_ech,data){creation_ech_SRS(taille_ech,data)},
  function(ech){simulation_nrTIC(ech)},
  n,
  n_simul,
  function(ech){sans_mode_GHR_scores(ech,4,R~X1+X3,"CAH")}
)

## e) Re pondération des deux modes ############################################

set.seed(194)
Y_repondéré_2_modes <- erreur_methode(
  data,
  function(taille_ech,data){creation_ech_SRS(taille_ech,data)},
  function(ech){simulation_nrTIC(ech)},
  n,
  n_simul,
  function(ech){modes_separes_GHR_scores(ech,4,Ri~X1+X3,Rt_noni~X1+X3,"CAH")}
)

## f) Re pondération que pour le téléphone #####################################

set.seed(194)
Y_repondéré_que_telephone <- erreur_methode(
  data,
  function(taille_ech,data){creation_ech_SRS(taille_ech,data)},
  function(ech){simulation_nrTIC(ech)},
  n,
  n_simul,
  function(ech){que_telephone_GHR_score(ech,4,Rt_noni~X1+X3,"CAH")}
)

## g) Re pondération avec les modes ############################################

set.seed(194)
Y_repondéré_avec_modes <- erreur_methode(
  data,
  function(taille_ech,data){creation_ech_SRS(taille_ech,data)},
  function(ech){simulation_nrTIC(ech)},
  n,
  n_simul,
  function(ech){avec_mode_GHR_score(ech,4,Ri~X1+X3, Rt~X1+X3,"CAH")}
)

## h) Heckman 1 étape ##########################################################

set.seed(194)
Heckman_1_etape <- erreur_methode(
  data,
  function(taille_ech,data){creation_ech_SRS(taille_ech,data)},
  function(ech){simulation_nrTIC(ech)},
  n,
  n_simul,
  function(ech){Heckman_reponderation(ech, 
                                      function(ech){selection(selection~X1+X3+instrument,
                                                              Y~X1+X3,
                                                              data=ech,
                                                              method="ml")})}
)

## i) Heckman 2 étapes #########################################################

set.seed(194)
Heckman_2_etapes <- erreur_methode(
  data,
  function(taille_ech,data){creation_ech_SRS(taille_ech,data)},
  function(ech){simulation_nrTIC(ech)},
  n,
  n_simul,
  function(ech){Heckman_reponderation(ech, 
                                      function(ech){selection(selection~X1+X3+instrument,
                                                              Y~X1+X3,
                                                              data=ech,
                                                              method="2step")})}
)

# 3) Mise en forme des résultats ###############################################

base_resultats <- as.data.frame(rbind(HT_sur_répondants,
                                      Y_selon_X_linéaire,
                                      Y_repondéré_sans_modes,
                                      Y_repondéré_2_modes,
                                      Y_repondéré_que_telephone,
                                      Y_repondéré_avec_modes,
                                      Heckman_1_etape,
                                      Heckman_2_etapes))

colnames(base_resultats) <- c("Biais","Variance","MSE","RMSE")
base_resultats
