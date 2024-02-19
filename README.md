Le code est associé au projet "Correction du biais de sélection dans les enquêtes multimodes" réalisé en troisième année à l'ENSAI par Cléo BOREL HERBERT, Fanny DURAND et Maha DAO. 
Nous étions encadrées alors par Stephane LEGLEYE.

Les fichiers sont organisés en six catégories afin que l’utilisateur puisse personnaliser son étude et/ou
ajouter de nouvelles fonctionnalités facilement. Il paraît important de les utiliser dans l’ordre :
- Catégorie #0 : Le fichier "Requirements" contient l’ensemble des packages utilisés par la suite et qu’il
convient de charger dès le départ.

- Catégorie #1 : Les codes définissent le contexte de la simulation (taille de la base de sondage, nombre
de variables et lois associées, matrice de corrélation) et permettent quelques contrôles. Le rendu initial
contient les points de départ des trois exemples théoriques ainsi que les codes utilisés pour les démonstrations sur TIC
et Epicov.

- Catégorie #2 : Elle contient les fonctions permettant de définir un échantillon. Pour l’instant il n’y a
qu’une seule option qui est le plan de sondage aléatoire simple sans remise.

- Catégorie #3 : Elle contient les fonctions permettant de mettre en place la non-réponse. Nous four-
nissons le code général ainsi que la variante utilisée pour la démonstration sur TIC.

- Catégorie #4 : Il y a un code pour chaque méthode présentée dans le rapport.
  
- Catégorie #5 : Le fichier "Evaluation d’une méthode.R" contient la méthode générique permettant de
connaître le biais empirique, la variance empirique et l’erreur quadratique commise par une certaine
fonction dans un certain contexte. Il est accompagné de trois exemples théoriques ainsi que des codes utilisés
pour les démonstrations.
