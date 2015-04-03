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

splitImageArray <- function( images ) {
  #result <- array( , dim = c( height, width, rows * cols ) );
  result <- array( , dim = c( 40, 33, 400 ) );
  for ( i in 1:20 ) {
    for ( j in 1:20 ) {
      result[ , , (j-1)*20+i ] <- images[((i-1)*40+1):(i*40), ((j-1)*33+1):(j*33)];
    }
  }
  return (result);
}

countPixel <- function(ens) {
  
#   classCount <- array( , dim = c( 40, 33, 40 * 30 ) )
#   
#   for(x in 1:33) {
#     for(y in 1:40) {
#       for(n in 1:nbFace) {
#         classCount[ x, y, n] <- 0
#         for(i in 1:nbApprentissage) {
#           classCount[x, y, n] <- classCount[x, y, n] + ens[x, y, i+(n-1)*9]
#         }
#       }
#     }
#   }
  
  pc <- array(, dim=c(40, 33, 0) );
  # pour chaque classe
  for ( c in 0:40 ) {
    csum <- matrix(rep(0, 33*40), nrow=33,ncol=40, byrow=TRUE)
    # pour chaque image de la classe, dans le nombre pour l'apprentissage
    for ( i in 1:8 ){
      imgID <- (c * 10) + i
      csum <- csum + ens[1,1,imgID]
    }
    csum <- csum / 8
    pc <- abind( pc, csum, along=3 )
  }
  return ( pc )
}

createTree <- function(ens) {
  nbPixel <- countPixel(ens)
  print(length(ens[1,,]))
  print(length(ens[,1,]))
  print(length(ens[,,1]))
  
  #print(nbPixel)
}