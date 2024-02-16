######################## Fonction générique évaluation #########################

erreur_methode <- function(data, methode_ech, methode_nr, taille_ech, nombre_ech,methode){
  
  #On initialise le vecteur resultat, qui contiendra les estimations :
  resultats <- c()
  
  #Pour chaque itération on crée un nouvel échantillon, on utilise la méthode à 
  #évaluer et on récupère le résultat.
  for(i in 1:nombre_ech){
    ech <- methode_ech(taille_ech,data)
    ech <- methode_nr(ech)
    resultats <- c(resultats,methode(ech))
  }
  
  #Le biais empirique transforme l'espérance de l'estimateur en la moyenne des
  #estimations :
  biais <- mean(resultats) - sum(data$Y)
  
  #La variance empirique est la variance observée sur les estimations :
  variance <- var(resultats)
  
  #On en déduit l'erreur quadratique moyenne :
  mse <- biais^2 + variance
  rmse <- sqrt(mse)
  return(c(biais,variance,mse,rmse))
}

#Exemple : On utilise beaucoup les fonctions anonymes pour pouvoir paramétrer la
#fonction peu importe les méthodes utilisées et la manière de créer les 
#échantillons. Il en résulte une syntaxe assez complexe mais la deuxième façon 
#de procéder (en mettant les paramètres des fonctions sous forme de liste pour 
#permettre plusieurs longueurs de liste) n'est pas la plus interessante puisquon
#ne peut alors plus s'aider de l'autocomplétion.

erreur_methode(data,
               function(taille_ech,data){creation_ech_SRS(taille_ech,data)},
               function(ech){simulation_nrprop(ech)},
               10000,
               10,
               function(ech){HT_repondants(ech)})
