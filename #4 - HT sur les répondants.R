############################ HT sur les répondants #############################

# Principe : On estime Y par Horvitz-Thompson sans repondérer, on adapte 
# seulement la taille de l'échantillon.

# Hypothèse : Non réponse aléatoire
# Besoin de connaître X pour tout le monde : Non

HT_repondants <- function(ech){
  nouveau_n <- nrow(ech %>% filter(!is.na(mode)))
  
  #On modifie le poids pour le lier au nombre de répondants
  ech <- ech %>% 
    mutate(Y_pond = case_when(
      is.na(Y)~0,
      TRUE ~ Y * poids * nrow(ech) / nouveau_n
    ))
  
  return(sum(ech$Y_pond))
}

#Total estimé sur l'échantillon test :
HT_repondants(ech_test)
