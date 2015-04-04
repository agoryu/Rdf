sizePicture <- 33 * 40
nbApprentissage <- 8
nbFace <- 40

# Chargement d'une image en niveaux de gris
rdfReadGreyImage <- function (nom) {
  image <- readImage (nom)
  if (length (dim (image)) == 2) {
    image
  } else {
    channel (image, 'red')
  }
}

#recuperation et segmentation des donne
#row = 40
#col = 33
splitImageArray <- function( images, nbImage, row, col, nbImage ) {
  result <- array( , dim = c( row, col, nbImage ) );
  for ( i in 1:20 ) {
    for ( j in 1:20 ) {
      result[ , , (j-1)*20+i ] <- images[((i-1)*row+1):(i*row), ((j-1)*col+1):(j*col)];
    }
  }
  return (result);
}

#probabilite qu'un pixel soit dans une image
probabilityOfPixel <- function(ens, present, row, col, nbClasse, nbExemplaire) {
  
  nbPicture <- length(ens[,,1])
  result <- array( , dim = c( row, col ) )
  result[,] <- 0
  
  for(i in 1:nbClasse) {
    
    for(j in 1:nbExemplaire) {
      if(present[i,j] == 1) {
        imgID <- ((i-1) * nbExemplaire) + j
        #somme de pixel
        result <- result + ens[,,imgID]
      }
    }
  }
  
  #moyenne
  result <- result / 400
  return(result)
}

#calcul de l'entropie
getEntropie <- function(ens) {
  
  p <- ens
  pInv <- 1 - ens
  return(-(log(p ^ p) + log(pInv ^ pInv)))
}

#retourne le nombre de classe encore presente
checkNbClasse <- function(ens) {
  
  cpt <- 0
  for(i in 1:40) {
    if(sum(ens[i,]) > 0) {
      cpt <- cpt +1
    }
  }
  return(cpt)
}

#contruit l'arbre permettant de savoir si une image est présente ou non
constructBooleanTab <- function() {
  stackedFacesI <- array( , dim = c( 40, 10 ) );
  for(i in 1:40) {
    for(j in 1:10) {
      if(j <= 8)
        stackedFacesI[i,j] <- 1
      else 
        stackedFacesI[i,j] <- 0
    }
  }
  return(stackedFacesI)
}

getTree <- function(pos, ens, present, response) {
  
  x <- pos %/% 33
  y <- pos %% 33
  cpt <- 0
  
  if(x == 0) x = 1
  if(y == 0) y = 1
  
  for (i in 1:40)
  {
    for(j in 1:10) {
      cpt <- cpt+1
      
      if(present[i,j] == 1) {
        if(ens[x, y, cpt] != response[x,y]){
          present[i,j] <- 0
        }
      }
    }
  }
  return(present)
}

findFace <- function(stackedFaces, stackedFacesI, response) {
  
  cpt <- 0
  
  #while(checkNbClasse(stackedFacesI) > 1) {
  while(cpt < 39) {
    
    print(paste("passage ", cpt))
    cpt <- cpt + 1
    
    #probabilite qu'un pixel soit dans une image
    proba <- probabilityOfPixel(stackedFaces, stackedFacesI)
    
    #recuperation de l'entropie
    entropie <- getEntropie(proba)
    
    #meilleur position
    position <- which.max(entropie)
    
    stackedFacesI <- getTree(position, stackedFaces, stackedFacesI, response)
    print(stackedFacesI)
  }
  
  if(checkNbClasse(stackedFacesI) == 1) {
    for(i in 1:40) {
      if(sum(ens[i,]) > 0) {
        print(paste("le visage selectionne appartient a la classe ", i))
        return(i)
      }
    } 
  } else {
    print("il n'y a pas de solution")
  }
}