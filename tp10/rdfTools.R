# ------------------------------------------------------
# Ce fichier contient les fonctions utile pour le tp 10.
# ------------------------------------------------------

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

# read USPS digits files
# Files must be stored in a single folder and their names must have the
# following format : usps_digit.jpg, where digit is in [0;9].
# Image data is stored in a 16x16x11000 array.
# Image matrices are non-transposed 16x16 matrices.
# Image labels is a vector of integers
#
# params: folder - the folder where the files are stored
#
# returns: a list containing image data and image labels (in [0;9])
#
readUSPSdata <- function( folder ) {

	# output data: image matrix and labels vectors
	data <- array(, dim=c(16, 16, 0) );
	labels <- vector( , length=0 );

	# read the image files 
	for ( i in 0:9 ) {
		file <- sprintf( "%s/usps_%d.png", folder, i );
		image <- rdfReadGreyImage( file );
		image <- t( imageData( image ) );
		images <- splitImageArray( image, 34, 33, 16, 16 );
		images <- images[ , , 1:1100 ];
		data <- abind( data, images, along=3 );
		labels <- c( labels, rep ( i, 1100 ) );
	}

	return ( list(data, labels) );
}

# Retourne un vecteur de booléen de taille le nombre total d'images.
# Mettant à vrai les images d'apprentissage et à faux les images de test.
getNAppMask <- function(nnumber, napp) {

    vec <- rep(FALSE, nnumber*10)

    # pour chaque classe
    for ( c in 0:9 ) {

        # pour chaque image de la classe, dans le nombre pour l'apprentissage 
        for ( i in 1:napp ){
            imgID <- (c * nnumber) + i
            vec[imgID] <- TRUE
        }
    }
    
    return ( vec )
}

# Retourne la probabilité pour chaque pixel, s'il appartient aux classes, 
# parmi les images du masque `mask`
probabilityPixelOfClassInNApp <- function( data3D, labels, nnumber, napp, mask ) {

    # width
    W <- 16
    # heigth
    H <- 16

    pc <- array(, dim=c(W, H, 0) );

    # pour chaque classe
    for ( c in 0:9 ) {
        csum <- matrix(rep(0, W*H), nrow=H,ncol=W, byrow=TRUE)
        cpt <- 0

        # pour chaque image de la classe, dans les images d'apprentissage 
        for ( i in 1:napp ){
            imgID <- (c * nnumber) + i
            if (mask[imgID]){
                csum <- csum + data3D[,,imgID]
                cpt <- cpt + 1
            }
        }

        csum <- csum / cpt
        pc <- abind( pc, csum, along=3 )
    }

    return ( pc )
}

# Retourne l'entropie calculée avec la formule du cours, avec prise 
# en compte des classes.
# L'entropie maximale donne le pixels qui sépare les images au mieux.
computeEntropy <- function( pc ) {
 
    # width
    W <- 16
    # heigth
    H <- 16

    sum <- matrix(rep(0, W*H), nrow=H,ncol=W, byrow=TRUE)

    for ( c in 0:9 ){
        sum <- sum + ( log2(pc[,,c+1]^pc[,,c+1]) )
    }

    return ( - sum )
}

# Retourne un vecteur de booléen de taille, le nombre total d'image.
# TRUE:  le pixel de position `row`, `col` est blanc (à 1)
# FALSE: le pixel de position `row`, `col` est noir (à 0)
whichImgContain <- function( row, col, data, nnumber ) {

    vec <- rep(FALSE, nnumber*10)

    for ( i in 1:(nnumber*10) ) {
        if ( data[row,col,i] == 1 ) {
            vec[i] <- TRUE
        }
    }

    return ( vec )
}

# Retourne la probabilité que l'image fait partie de la classe 
# Image dont on parle est celle testé, les calculs on produit 
# le mask ici en param, en testant l'image
getProbaComeFromClass <- function( mask, napp) {
    
    
    vsum <- rep(0,10)

    # pour chaque classe
    for ( c in 0:9 ) {

        # pour chaque image de la classe, dans les images d'apprentissage 
        for ( i in 1:napp ){

            imgID <- (c * nnumber) + i
            if (mask[imgID]){
                vsum[c+1] <- vsum[c+1] + 1 
            }
        }

        vsum[c+1] <- vsum[c+1] / napp
    }

    return ( vsum )

}

# --------------------------------

# Fonction principale qui utile une grande partie des autres fonctions
# Retourne la classe probable de l'image de test `imgtest`
findClassFromImgtest <- function( imgtest, data, labels, nnumber, napp, mask ) {

    curentMask <- mask

    repeat {

        pc <- probabilityPixelOfClassInNApp(data, labels, nnumber, napp, curentMask)

        entropy <- computeEntropy(pc)

        # condition d'arret
        if ( is.nan(sum(entropy)) || sum(curentMask)<250 ){
            # on s'arrete: 
            # si il y a des nan dans la matrice entropy
            # ou 
            # si on a déja fait beaucoup d'itération
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

    }

    proba <- getProbaComeFromClass(curentMask, napp)

    # on fait -1 car le chiffre 0 commence a l'indice 1
    return  ( which.max(proba) - 1 )
}

