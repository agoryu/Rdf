library ("EBImage")
source ("rdfFunction.R")

nom <- "allFaces.png";
allFaces <- rdfReadGreyImage (nom)

allFaces <- t( imageData( allFaces ) )

#recuperation des images
stackedFaces <- splitImageArray(allFaces)

#tableau permettant de savoir si une image doit etre pris en compte
stackedFacesI <- constructBooleanTab()

response <- stackedFaces[,,9]

findFace(stackedFaces, stackedFacesI, response)

