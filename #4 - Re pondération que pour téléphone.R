############## Non réponse portée par les répondants téléphone #################

# Principe : Les répondants internet se représentent eux même tandis que les 
# répondants téléphone vont représenter l'ensemble des non-répondants internet.
# La re pondération des non-répondants internet estime la probabilité de réponse
# Rt_noni à partir de X puis utilise des groupes de réponse homogène calculés 
# selon la méthode des scores.
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

que_telephone_GHR_score <- function(ech, n_groupes, formula_t, methode){
  #On code Rt_noni l'indicatrice de réponse par téléphone avant de séparer 
  #l'échantillon (il est prévu de les réunir une fois les poids_nr calculés):
  ech <- ech %>% 
    mutate(Rt_noni = case_when(
      mode == "T" ~ 1,
      TRUE ~ 0
    ))
  
  #On traite à part les répondants internet (poids de non réponse égal à 1) :
  echi <- ech %>% filter(mode=="I")
  echi$poids_nr <- 1
  
  #On traite à part les répondants téléphone et les non-répondants :
  ech_noni <- ech %>% filter(mode!="I" | is.na(mode))
  modele_logit_telephone <- glm(formula_t, data=ech_noni, family=binomial(link = "logit"))
  ech_noni$score <- predict(modele_logit_telephone, newdata=ech_noni)
  ech_noni$groupe <- GHR_score(ech_noni,n_groupes,methode)
  ech_noni$poids_nr <- 1
  for (i in 1:n_groupes){
    ech_noni$poids_nr[ech_noni$groupe == i] <- 1/mean(ech_noni[ech_noni$groupe==i,Rt_noni])
  }
  
  #On réunit les deux sous-échantillons:
  ech_noni$score <- NULL
  ech_noni$groupe<- NULL
  ech <- rbind(echi,ech_noni)
  
  #On estime :
  ech <- ech %>% 
    mutate(poids_nr = case_when(
      is.na(mode)~NA,
      TRUE ~poids_nr
    )) %>% 
    mutate(Y_pond = case_when(
      is.na(Y)~0,
      TRUE ~ Y * poids * poids_nr
    ))
  return(sum(ech$Y_pond))
}

#Total estimé sur l'échantillon test :
que_telephone_GHR_score(ech_test,4,Rt_noni~X1+X2,"quantiles")
que_telephone_GHR_score(ech_test,4,Rt_noni~X1,"quantiles")
que_telephone_GHR_score(ech_test,4,Rt_noni~X1,"CAH")
que_telephone_GHR_score(ech_test,4,Rt_noni~X1,"kmeans")
