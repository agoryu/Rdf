# Chargement de la base de noms d'animaux
source ("rdfAnimaux.txt")
source ("rdfFonction.R")

n <- length(noms)

indice <- getIndice(noms)
print(indice)