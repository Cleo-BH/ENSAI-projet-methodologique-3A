################### Non-réponse dans le cas de l'exemple TIC ################### 

# On reprend le code principal auquel on ajoute les conditions suivantes :
# - Si U1 (défiance vis-à-vis d'internet, suit une N(0,1)) est supérieur à 
# 1.2816 (si parmi les 10% les plus méfiants) aucune réponse par internet n'est
# possible.
# - Si U2 (score lié aux équipements, suit une N(0,1)) est inférieur à -1.2816
# (si parmi les 10% les  moins bien équipés) aucune réponse par internet n'est 
# possible.


simulation_nrTIC<- function(ech){
  ech <- ech %>% 
    #On tire 2 valeurs aléatoires pour tout le monde.
    mutate(
      seuili = runif(nrow(ech),0,1),
      seuilt = runif(nrow(ech),0,1)) %>% 
    #On en déduit si la personne répondrait par internet et/ou par téléphone.
    #(Si supérieur au seuil répond, si inférieur ne répond pas + CONTRAINTES)
    mutate(
      Ri = case_when(
        U1 > 1.2816 ~ 0,
        U2 < -1.2816 ~ 0,
        Pi > seuili ~ 1,
        TRUE ~ 0),
      Rt = case_when(
        Pt > seuilt ~ 1,
        TRUE ~ 0)) %>% 
    #On est en multimode séquentiel internet puis téléphone donc on en déduit le
    #mode de collecte.
    mutate(mode = case_when(
      Ri == 1 ~ "I",
      Rt == 1 ~ "T",
      TRUE ~ NA)) %>% 
    #On définit également l'indicatrice de réponse totale.
    mutate(R = case_when(
      is.na(mode)~0,
      TRUE ~1)) %>% 
    #On supprime Y pour les non-répondants.
    mutate(mode = as.factor(mode),
           Y = case_when(
             is.na(mode)~NA,
             TRUE~Y
           ))
}

ech_test <- simulation_nrTIC(ech_test)

#Description des modes de collecte et proportion de non répondants :
summary(ech_test$mode)
nrow(ech_test[is.na(ech_test$mode),])/nrow(ech_test)
