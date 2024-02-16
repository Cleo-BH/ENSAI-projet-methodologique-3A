############################ Données exemple cas 1 #############################

# 1) Nombre d'individus ########################################################

N <- 2000000

# 2) Définition des variables ##################################################

def <- defData(varname="Y",
               formula=10,
               variance = 2,
               dist="normal")

def <- defData(def,
               varname="X",
               formula=0,
               variance = 1,
               dist="normal")

def <- defData(def,
               varname="U",
               formula=0,
               variance = 1,
               dist="normal")

def <- defData(def,
               varname="Pi",
               formula=0.5,
               variance = 0.19^2,
               dist="normal")

def <- defData(def,
               varname="Pt",
               formula=0.5,
               variance = 0.19^2,
               dist="normal")

# 3) Liens entre les variables #################################################

correlation <- matrix(c(1, 0.5, 0.5, 0, 0, 
                        0.5, 1, 0.5, 0, 0, 
                        0.5, 0.5, 1, 0, 0, 
                        0, 0, 0, 1, 0.5, 
                        0, 0, 0, 0.5, 1), nrow = 5)

# 4) Génération du jeu de données et contrôles #################################

data <- genCorFlex(N,
                   defs = def,
                   corMatrix = correlation)

#Matrice de corrélation du jeu de données simulé :
cor(data[,-1])

#Distribution des variables :
hist(data$Y)
hist(data$X)
hist(data$U)
hist(data$Pi)
hist(data$Pt)

#Base de sondage disponible pour les redressements :
base <- data %>% 
  select(id,X)
