################### Re pondération séparée des deux modes ######################

# Principe : Pour chaque mode, on estime la probabilité de réponse des individus
# (variable Ri et Rt_noni) à partir de X, on constitue des groupes homogènes de 
# réponse avec la méthode des scores et on utilise le taux de réponse par groupe
# pour changer les poids. Pour ne représenter qu'une fois la population totale, 
# on traite les poids de non-réponse pour que chaque mode de collecte soit 
# représenté à la hauteur de sa fréquence dans l'échantillon.

# Les groupes peuvent être constitués à l'aide des quantiles (méthode 
# "quantiles"), par CAH (méthode "CAH") ou en utilisant l'algorithme des kmeans 
# (méthode "kmeans"). La variable Rt_noni est créée dans la fonction à partir de
# Ri et Rt.

# Hypothèse : Non réponse ignorable
# Besoin de connaître X pour tout le monde : Oui

#Fonction annexe qui permet de générer n_groupes sur la variable score :
GHR_score <- function(ech,n_groupes,methode){
  if(methode=="quantiles"){
    probs <- c()
    for(i in 0:n_groupes){
      probs <- c(probs,i/n_groupes)
    }
    limites <- quantile(ech$score,probs)
    ech$groupe <- 0
    for(i in 1:n_groupes){
      borne_inf <- limites[i]
      borne_sup <- limites[i+1]
      ech$groupe[borne_inf<=ech$score & ech$score<=borne_sup] <- i
    }
  }
  if(methode=="CAH"){
    score <- scale(ech$score,center=T,scale=T)
    d.score <- dist(score)
    cah <- hclust(d.score)
    ech$groupe <- cutree(cah,k=n_groupes)
  }
  if(methode=="kmeans"){
    score <- scale(ech$score,center=T,scale=T)
    ech$groupe <- kmeans(score,centers=n_groupes)$cluster
  }
  return(ech$groupe)
}

modes_separes_GHR_scores <- function(ech, n_groupes, formula_i, formula_t, methode){
  
  #On calcule le poids de non réponse par internet (poids_nri):
  modele_logit_internet <- glm(formula_i, data=ech, family=binomial(link = "logit"))
  ech$score <- predict(modele_logit_internet, newdata=ech)
  ech$groupei <- GHR_score(ech,n_groupes,methode) 
  ech$poids_nri <- 1
  for (i in 1:n_groupes){
    ech$poids_nri[ech$groupei == i] <- 1/mean(ech[ech$groupei==i,Ri])
  }
  
  #On crée la variable Rt_noni indicatrice des répondants téléphone :
  ech <- ech %>% 
    mutate(Rt_noni = case_when(
      mode == "T" ~ 1,
      TRUE ~ 0
    ))
  
  #On calcule le poids de non-réponse par téléphone (poids_nrt) :
  modele_logit_telephone <- glm(formula_t, data=ech, family=binomial(link = "logit"))
  ech$score_i <- ech$score #On sauvegarde l'ancienne valeur du score
  ech$score <- predict(modele_logit_telephone, newdata=ech)
  ech$groupet <- GHR_score(ech,n_groupes,methode)
  ech$poids_nrt <- 1
  for (i in 1:n_groupes){
    ech$poids_nrt[ech$groupet == i] <- 1/mean(ech[ech$groupet==i,Rt_noni])
  }
  
  #On en déduit le poids de non-réponse de tous les répondants et on estime :
  proportion_internet <- sum(ech$Ri)/sum(ech$R)
  
  ech <- ech %>% 
    mutate(poids_nr = case_when(
      mode == "I" ~ poids_nri*proportion_internet,
      mode == "T" ~ poids_nrt*(1-proportion_internet),
      TRUE ~NA
    )) %>% 
    mutate(Y_pond = case_when(
      is.na(Y)~0,
      TRUE ~ Y * poids * poids_nr
    ))
  return(sum(ech$Y_pond))
}

#Total estimé sur l'échantillon test :
modes_separes_GHR_scores(ech_test,4,Ri~X1+X2,Rt_noni~X1+X2,"quantiles")
modes_separes_GHR_scores(ech_test,4,Ri~X1+X2,Rt_noni~X1,"quantiles")
modes_separes_GHR_scores(ech_test,4,Ri~X1,Rt_noni~X1,"quantiles")
modes_separes_GHR_scores(ech_test,4,Ri~X1,Rt_noni~X1,"CAH")
modes_separes_GHR_scores(ech_test,4,Ri~X1,Rt_noni~X1,"kmeans")