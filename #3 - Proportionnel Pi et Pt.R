#################### Non-réponse proportionnelle à Pi et Pt #################### 

simulation_nrprop <- function(ech){
  ech <- ech %>% 
    #On tire 2 valeurs aléatoires pour tout le monde.
    mutate(
      seuili = runif(nrow(ech),0,1),
      seuilt = runif(nrow(ech),0,1)) %>% 
    #On en déduit si la personne répondrait par internet et/ou par téléphone.
    #(Si supérieur au seuil répond, si inférieur ne répond pas)
    mutate(
      Ri = case_when(
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

ech_test <- simulation_nrprop(ech_test)

#Description des modes de collecte et proportion de non répondants :
summary(ech_test$mode)
nrow(ech_test[is.na(ech_test$mode),])/nrow(ech_test)
