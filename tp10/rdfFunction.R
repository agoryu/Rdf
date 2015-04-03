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

splitImageArray <- function( images, rows, cols, height, width ) {
  result <- array( , dim = c( height, width, rows * cols ) );
  for ( i in 1:rows ) {
    for ( j in 1:cols ) {
      result[ , , (j-1)*rows+i ] <- images[((i-1)*height+1):(i*height), ((j-1)*width+1):(j*width)];
    }
  }
  return (result);
}

countPixel <- function(ens) {
  
  classCount <- c(sizePicture * nbFace)
  nbPixel <- 0
  
  #pixel
  for (i in 1:sizePicture) {
    
    #visage
    for(j in 1:nbFace) {
      position <- j * sizePicture + i
      classCount[position] = 0
      
      #exemplaire visage
      for(k in 1:nbApprentissage) {
        nbPixel = nbPixel + ens[i + sizePicture * (k*j)]
        classCount[position] = classCount[position] + ens[i + sizePicture * (k*j)]
      }
    }
  }
  
  return(classCount)
}
  
entropieVariation <- function(ens) {
  
  sizePicture <- 33 * 40
  entropie <- c(sizePicture)
  for(j in 1:sizePicture) {
    entropie[j] = 0
    for(i in 1:40) {
      p <- classCount[sizePicture * i + j] / nbPixel
      entropie[j] <- entropie[j] + p * log(p)
    }
  }

  print(entropie)
  #position de la lettre la plus interessante a choisir
  position <- which.min(entropie)
  return(position)

} 

createTree <- function(ens) {
  nbPixel <- countPixel(ens)
  print(nbPixel)
}