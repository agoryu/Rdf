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


# nombre d'image par classe
nnumber <- 1100

# largeur et hauteur d'une image d'un chiffre
width <- 16
heigth <- 16

# lecture des images binaires
output <- readUSPSdata("usps")

data <- output[[1]]
labels <- output[[2]]

# la moitié des images sont d'apprentissage
napp <- 1100/2

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# IMAGE TESTE
# attention il faut prendre une image dont l'indice n'est pas dans napp
# Tous les 1100 images on passe a une autre classe (autre chiffre),
# Les images de 1 à 550 sont d'apprentissage.
#
# Par exemple de 1 à 550 c'est l'apprentissage des images de 0 et 
# de 4401 à 4950 c'est l'apprentissage des images de 4
#
# ----------------------

#imgtest <- data[,,850] # 0
#imgtest <- data[,,1700] # 1
#imgtest <- data[,,2769] # 2
#imgtest <- data[,,3820] # 3
#imgtest <- data[,,4777] # 4
imgtest <- data[,,10996] # 9

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# image considere pour le calcul de l'entropie
nappMask <- getNAppMask(nnumber, napp)
curentMask <- nappMask


# /!\ pour le moment c'est du bricolage
# a mi-chemin entre respect du cours et fait maison


repeat {

    pc <- probabilityPixelOfClassInNApp(data, labels, nnumber, napp, curentMask)

    entropy <- computeEntropy(pc)

    # condition d'arret
    if ( is.nan(sum(entropy)) ){
        # si il y a des nan dans la matrix entropy on s'arrete    
        break
    }

    bestPixel <- which.max(entropy)

    best.row <- bestPixel %% 16 
    if( best.row==0 ){
        best.row <- 16
    }

    # on arroundit au supérieur car les indices commence à 1
    best.col <- ceiling( bestPixel/16 )

    imagesWithBest <- whichImgContain(best.row, best.col, data, nnumber)

    if(imgtest[best.row,best.col] == 1) {
        curentMask <- imagesWithBest & curentMask
    } else {
        curentMask <- !imagesWithBest & curentMask
    }

    print(paste("Nombre de pixels restant", (sum(curentMask))))

}

proba <- getProbaComeFromClass(curentMask, napp)
# on fait -1 car le chiffre 0 commence a l'indice 1
print(paste("L'image testé appartient probablement à la classe", (which.max(proba)-1)))
#print(paste("Probabilité: ", (max(proba))))

