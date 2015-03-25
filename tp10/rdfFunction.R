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
  n <- 400
  size <- 33 * 40
  h <- c(size)
  for (i in 1:size) {
    h[i] = 0
    for(j in 1:400) {
      h[i] = h[i] + ens[(j-1)*size+i]
    }
  }
  
  #probabilite qu'une lettre apparaisse dans un mot
  p <- h / n
  print(p)
#   #inverse de la probabilite precedente
#   pInv <- 1 - p
#   
#   #calcul de l entropie
#   entropie <- -(log2(p ^ p) + log2(pInv ^ pInv))
#   
#   #position de la lettre la plus interessante a choisir
#   position <- which.max(entropie)
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