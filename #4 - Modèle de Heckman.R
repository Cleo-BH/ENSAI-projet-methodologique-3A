############################## Modèle de Heckman ###############################

# Principe : On utilise la fonction selection du package sampleSelection pour 
# mettre en place un modèle de Heckman et repondérer l'échantillon. Le modèle de
# Heckman peut être estimé en une étape (méthode "ml") ou en deux étapes 
# (méthode "2step"). Ce code s'appuie sur les travaux de L. Castell et de 
# P. Sillard, accessible via le lien suivant :
# https://github.com/InseeFrLab/NRC-heck-model

# Instrument utilisé : On sépare l'échantillon en deux échantillons aléatoires : 
# dans l'un des cas on propose de répondre uniquement par internet, et dans le 
# second cas on propose le multimode séquentiel. L'instrument est alors
# l'indicatrice du mode de collecte proposé.

# Hypothèse : Non réponse non ignorable
# Besoin de connaître X pour tout le monde : Oui

# Autres : La fonction selection ne permet pas de renseigner une formule sous la
# forme d'un argument. Afin de pouvoir modifier les formules utilisées on doit 
# donc passer en argument la fonction déjà paramétrée... Elle doit avoir la forme
# suivante : function(ech){selection(selection~X+instrument,Y~X,ech,methode)} où
# X peut prendre la forme souhaitée et où méthode vaut soit "ml" soit "2step". 

Heckman_reponderation <- function(ech, fonction_heckman, methode){
  
  #Création de l'instrument et définition des deux sous échantillons :
  ech$instrument <- ifelse(runif(nrow(ech),0,1)<=0.5,1,0)
  ech <- ech %>% 
    mutate(selection  = case_when(
      instrument==0 ~ Ri,
      instrument==1 ~ R))
  
  #Mise en place du modèle de Heckman :
  modele_heckman <- fonction_heckman(ech)
  
  #On calcule les probabilités de non-réponse :
  selection_pred <- predict(modele_heckman, part="selection", type="link", newdata=ech)
  res_Y <- residuals(modele_heckman, part="outcome")
  ech$proba_nr <- pnorm((selection_pred+coef(modele_heckman)["rho"]/coef(modele_heckman)["sigma"]*res_Y)/
                          ((1-coef(modele_heckman)["rho"]^2)^(0.5)))
  ech[ech$proba_nr<0.1]$proba_nr <- 0.1
  
  #On en déduit les poids associés à la non-réponse et on estime :
  ech <- ech %>% 
    mutate(poids_nr = case_when(
      is.na(proba_nr)~0,
      TRUE ~ 1/proba_nr)) %>% 
    mutate(Y_pond = case_when(
      is.na(proba_nr)~0,
      TRUE ~ Y * poids * poids_nr
    ))
  return(sum(ech$Y_pond))
}

#Total estimé sur l'échantillon test :
Heckman_reponderation(ech_test, 
                      function(ech){selection(selection~X1+X2+instrument,
                                              Y~X1+X2,
                                              data=ech,
                                              method="ml")})

Heckman_reponderation(ech_test, 
                      function(ech){selection(selection~X1+instrument,
                                              Y~X1+X2,
                                              data=ech,
                                              method="ml")})
Heckman_reponderation(ech_test, 
                      function(ech){selection(selection~X1+instrument,
                                              Y~X1+X2,
                                              data=ech,
                                              method="2step")})
