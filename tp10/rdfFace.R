library ("EBImage")
source ("rdfFunction.R")

nom <- "allFaces.png";
allFaces <- rdfReadGreyImage (nom)

nbFace <- 40
widthFace <- 660 / 20
heightFace <- 800 / 20

allFaces <- t( imageData( allFaces ) );
stackedFaces = splitImageArray(allFaces)

createTree(stackedFaces)