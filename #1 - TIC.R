############################# Données exemple TIC ##############################

# 1) Nombre d'individus ########################################################

N <- 2000000

# 2) Définition des variables ##################################################

# Y : indicatrice d'illectronisme
def <- defData(varname="Y",
               formula=0.15,
               dist="binary")

# X1 : age
def <- defData(def,
               varname="X1",
               formula="15;85",
               dist="uniform")

# X2 : sexe
def <- defData(def,
               varname="X2",
               formula=0.5,
               dist="binary")

# X3 : revenu
def <- defData(def,
               varname="X3",
               formula=23160,
               variance = 8500^2,
               dist="normal")

# X4 : niveau de diplôme
def <- defData(def,
               varname="X4",
               formula=0,
               variance = 1,
               dist="normal")

# U1 : défiance vis à vis d'internet
def <- defData(def,
               varname="U1",
               formula=0,
               variance = 1,
               dist="normal")

# U2 : score lié aux équipements (connexion haut débit, équipements personnels, 
# équipements professionnels, présence de médiathèques, etc)
def <- defData(def,
               varname="U2",
               formula=0,
               variance = 1,
               dist="normal")

# Pi : propension à répondre par internet
def <- defData(def,
               varname="Pi",
               formula=0.4,
               variance = 0.19^2,
               dist="normal")

# Pt : propension à répondre par téléphone
def <- defData(def,
               varname="Pt",
               formula=0.7,
               variance = 0.19^2,
               dist="normal")

# 3) Liens entre les variables #################################################

correlation <- matrix(c(1, 0.8, 0.1, 0.7, 0.7, 0.5, 0.9,-0.9, 0, 
                        0.8, 1, 0.1, 0.6, -0.3, 0.6, -0.7, -0.8, -0.7,
                        0.1, 0.1, 1, -0.2, 0, 0, 0, 0, 0,
                        0.7, 0.6, -0.2, 1, 0.6, 0, 0.5, 0.5, 0, 
                        0.7, -0.3, 0, 0.6, 1, 0, 0.6, 0.5, 0,
                        0.5, 0.6, 0, 0, 0, 1, -0.7, -0.9, 0,
                        0.9, -0.7, 0, 0.5, 0.6, -0.7, 1, 0.2, 0,
                        -0.9, -0.8, 0, 0.5, 0.5,-0.9, 0.2, 1, 0.5,
                        0, -0.7, 0, 0, 0, 0,0, 0.5, 1), nrow = 9)

#La matrice n'est pas définie positive donc on prend la matrice de corrélation
#définie positive la plus proche : 
library(Matrix)
matrice_alternative <- nearPD(correlation, corr=TRUE)

# 4) Génération du jeu de données et contrôles #################################

data <- genCorFlex(N,
                   defs = def,
                   corMatrix = as.matrix(matrice_alternative$mat))

#Matrice de corrélation du jeu de données simulé :
cor(data[,-1])

#Distribution des variables :
hist(data$Y)
hist(data$X1)
hist(data$X2)
hist(data$X3)
hist(data$X4)
hist(data$U1)
hist(data$U2)
hist(data$Pi)
hist(data$Pt)

#Base de sondage disponible pour les redressements :
base <- data %>% 
  select(id,starts_with("X"))
