# -----------------------------------------------
# Chargement des fonctions externes
# -----------------------------------------------

library ("EBImage")
library( abind );
source ("rdfTools.R")




# -----------------------------------------------
# Images des chiffres:
# width : 528px
# height: 544px
# colonne de chiffre: 32 completes + 1 non complete
# ligne de chiffre: 34
# Soit 32col * 34row + 1col(12 chiffres)
#
# Les chiffres dans les images:
# width: 16px
# heigth: 16px
# -----------------------------------------------


# -----------------------------------------------
# DATA ET VARIABLES UTILIES
# -----------------------------------------------

# lecture des images binaires
output <- readUSPSdata("usps")

data <- output[[1]]
labels <- output[[2]]

# nombre d'images par classe
nnumber <- 1100

# la moitie des images sont d'apprentissage (550 sur 1100)
napp <- 550
ntest <- nnumber - napp

# largeur et hauteur d'une image d'un chiffre
width <- 16
heigth <- 16





# -----------------------------------------------
# TEST SUR UNE IMAGE DE TEST
# -----------------------------------------------

# >> ------------ >
#
# IMAGE TEST
#
# Tous les 1100 images on passe a une autre classe (autre chiffre),
# Les images de 1 à 550 sont d'apprentissage.
#
# Par exemple de 1 à 550 c'est l'apprentissage des images de 0 et 
# de 4401 à 4950 c'est l'apprentissage des images de 4
#
# -----------------

imgtest <- data[,,851] # 0
#imgtest <- data[,,1902] # 1
#imgtest <- data[,,3008] # 2
#imgtest <- data[,,4120] # 3
#imgtest <- data[,,5207] # 4
#imgtest <- data[,,6487] # 5
#imgtest <- data[,,7409] # 6
#imgtest <- data[,,8888] # 7
#imgtest <- data[,,9609] # 8
#imgtest <- data[,,10996] # 9

# < ------------ <<

# image considere pour le calcul de l'entropie
nappMask <- getNAppMask(nnumber, napp)

foundClass <- findClassFromImgtest( imgtest, data, labels, nnumber, napp, nappMask )

print(paste("L'image testé appartient probablement à la classe", foundClass))





# -----------------------------------------------
# TAUX DE BONNE CLASSIFICATION
# (tests sur toutes les images de test)
# -----------------------------------------------

vecBC <- rep(FALSE, ntest*10)

for (c in 0:9){
    print(paste("Test sur la classe", c))
    for (i in 1:(ntest)){
        idData <- (c*nnumber) + napp + i
        idVecBC <- (c*ntest) + i
        foundClass <- findClassFromImgtest( data[,,idData], data, labels, nnumber, napp, nappMask )
        if(foundClass == labels[idData]){
            vecBC[idVecBC] = TRUE   
        }
    }
}

tauxBC <- sum(vecBC) / length(vecBC)

print(paste("Taux de bonne clasification", tauxBC))
