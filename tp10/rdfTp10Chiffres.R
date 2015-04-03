# Chargement des fonctions externes
library ("EBImage")
library( abind );
source ("rdfTools.R")

# -----------------------------------------------
# Images des chiffres:
# width : 528px
# height: 544px
# colonne de chiffre: 32 complètes + 1 non complète
# ligne de chiffre: 34
# Soit 32col * 34row + 1col(12 chiffre)
#
# Les chiffres dans les images:
# width: 16px
# heigth: 16px
# -----------------------------------------------



nnumber <- 1100

# largeur et hauteur d'une image d'un chiffre
width <- 16
heigth <- 16

# lecture des images binaires
output <- readUSPSdata("usps")

data <- output[[1]]
labels <- output[[2]]

# la moitié des données sont d'apprentissage
napp <- 1100/2

pc <- probabilityPixelOfClass(data, labels, nnumber, napp)


