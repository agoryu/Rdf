# Chargement de la base de noms d'animaux
source ("rdfAnimaux.txt")
source ("rdfFonction.R")

n <- length(noms)
calcArbre(noms, 0)
#print(!is.na(match("pou", noms)))
#jouer(noms)
# indice <- getIndice(noms)
# 
# print(indice["ensembleA"])
# print(indice["pos"])