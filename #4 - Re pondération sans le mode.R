################## Re pondération sans tenir compte du mode ####################

# Principe : On estime la probabilité de réponse globale des individus (variable
# R) à partir de X. On constitue des groupes homogènes de réponse avec la 
# méthode des scores puis on utilise le taux de réponse par groupe pour changer
# les poids. Les groupes peuvent être constitués à l'aide des quantiles (méthode
# "quantiles"), par CAH (méthode "CAH") ou en utilisant l'algorithme des kmeans
# (méthode "kmeans").

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

sans_mode_GHR_scores <- function(ech,n_groupes,formula,methode){
  #On modélise la probabilité de réponse à partir des variables choisies :
  modele_logit <- glm(formula, data=ech, family=binomial(link = "logit"))
  
  #On crée des groupes homogènes de réponse :
  ech$score <- predict(modele_logit, newdata=ech)
  ech$groupe <- GHR_score(ech,n_groupes,methode)
  
  #On calcule le poids de non réponse de chaque groupe :
  ech$poids_nr <- 1
  for (i in 1:n_groupes){
    ech$poids_nr[ech$groupe == i] <- 1/mean(ech[ech$groupe==i,R])
  }
  
  #On en déduit l'estimateur re pondéré :
  ech <- ech %>% 
    mutate(Y_pond = case_when(
      is.na(Y)~0,
      TRUE ~ ech$Y*ech$poids*ech$poids_nr))
  return(sum(ech$Y_pond))
}

#Total estimé sur l'échantillon test :
sans_mode_GHR_scores(ech_test,4,R~X1+X2,"quantiles")
sans_mode_GHR_scores(ech_test,4,R~X1,"quantiles")
sans_mode_GHR_scores(ech_test,4,R~X1,"CAH")
sans_mode_GHR_scores(ech_test,4,R~X1,"kmeans")
