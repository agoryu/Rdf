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

bestAttribut <- function(ens) {
  
  #taille de l'ensemble
  size <- 33 * 40
  p <- c(40 * size)
  
  #pixel
  for (i in 1:size) {
    #visage
    for(j in 1:40) {
      p[j * size + i] = 0
      #exemplaire visage
      for(k in 1:8) {
        p[j * size + i] = p[j * size + i] + ens[i + size * (k*j)]
      }
      #calcul moyenne
      p[j * size + i] = p[j * size + i] / 8
    }
  }
  
  entropie <- c(size)
  for(j in 1:size) {
    for(i in 1:40) {
      entropie[j] <- entropie[j] + p[size * i + j] * log(p[size * i + j])
    }
  }
  
  print(entropie)
  #position de la lettre la plus interessante a choisir
  position <- which.min(entropie)
  return(position)
#   
#   ensA <- c()
#   ensS <- c()
#   
#   for (i in 1:n)
#   {
#     if(mat[position,i] == 1 ){
#       ensA = c(ensA, ens[i])
#     } else {
#       ensS = c(ensS, ens[i])
#     }
#   }
#   
#   return(list(pos = position, ensembleA = ensA, ensembleS = ensS))
} 

createArbre <- function(ens) {
  
  mot <- scan("",what="character",nlines=1);
  size <- nchar(mot)
  tmpEns <- ens
  
  while(length(tmpEns) != 1) {
    res <- getIndice(tmpEns)
    
    if(containLetter(res$pos, mot) == TRUE) {
      tmpEns <- res$ensembleA
    } else {
      tmpEns <- res$ensembleS
    }
  }
}