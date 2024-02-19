################### Re pondération en tenant compte du mode ####################

# Principe : On estime la probabilité de réponse globale des individus à partir 
# de leur probabilité de réponse par internet estimée et de leur probabilité de
# réponse par téléphone estimée. On estime la probabilité de réponse par 
# internet pour tout l'échantillon à partir de Ri et de X, et en utilisant des 
# groupes homogènes de réponse. On estime la probabilité de réponse par téléphone 
# pour les non-répondants internet à partir de Rt et X. On en déduit une
# estimation pour les répondants internet et on utilise à nouveau des groupes
# homogènes de réponse. 

# On constitue des groupes homogènes de réponse avec la méthode des scores. Les 
# groupes peuvent être constitués à l'aide des quantiles (méthode "quantiles"), 
# par CAH (méthode "CAH") ou en utilisant l'algorithme des kmeans (méthode 
# "kmeans").

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

avec_mode_GHR_score <- function(ech, n_groupes, formula_i, formula_t, methode){
  
  #On estime la probabilité de réponse par internet pour tout le monde :
  modele_logit_internet <- glm(formula_i, data=ech, family=binomial(link = "logit"))
  ech$score <- predict(modele_logit_internet, newdata=ech)
  ech$groupei <- GHR_score(ech,n_groupes,methode)
  ech$proba_nri <- 1
  for (i in 1:n_groupes){
    ech$proba_nri[ech$groupei == i] <- mean(ech[ech$groupei==i,Ri])
  }
  
  #On estime la probabilité de réponse par téléphone pour tout le monde (en 
  #construisant le modèle seulement sur les non-répondants internet) :
  modele_logit_telephone <- glm(formula_t, data=ech%>% filter(mode!="I" | is.na(mode)), family=binomial(link = "logit"))
  ech$score_i <- ech$score #On sauvegarde l'ancienne valeur du score
  ech$score <- predict(modele_logit_telephone, newdata=ech)
  ech$groupet <- GHR_score(ech,n_groupes,methode)
  ech$proba_nrt <- 1
  for (i in 1:n_groupes){
    ech$proba_nrt[ech$groupet == i] <- mean(ech[ech$groupet==i & (ech$mode=="T"|is.na(ech$mode)),Rt])
  }
  
  #On en déduit la probabilité de réponse globale et on estime :
  ech <- ech %>% 
    mutate(proba_nr = proba_nri + (1-proba_nri)*proba_nrt) %>% 
    mutate(poids_nr = 1/proba_nr) %>% 
    mutate(Y_pond = case_when(
      is.na(Y)~0,
      TRUE ~ Y * poids * poids_nr
    ))
  return(sum(ech$Y_pond))
}

#Total estimé sur l'échantillon test :
avec_mode_GHR_score(ech_test,4,Ri~X1+X2, Rt~X1+X2,"quantiles")
avec_mode_GHR_score(ech_test,4,Ri~X1+X2, Rt~X1,"quantiles")
avec_mode_GHR_score(ech_test,4,Ri~X1, Rt~X1,"quantiles")
avec_mode_GHR_score(ech_test,4,Ri~X1, Rt~X1,"CAH")
avec_mode_GHR_score(ech_test,4,Ri~X1, Rt~X1,"kmeans")
