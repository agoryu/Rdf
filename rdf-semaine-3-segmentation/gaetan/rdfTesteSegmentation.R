# -----------------------------------------------------------------------
# Extraction d'attributs de pixels pour la classification,
# Module RdF, reconnaissance de formes
# Copyleft (C) 2014, Universite Lille 1
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
# -----------------------------------------------------------------------

# Chargement des fonctions externes
library ("EBImage")
source ("rdfSegmentation.R")


# Chargement d'une image
nom <- "rdf-2-classes-texture-3.png"
nomref <- "rdf-masque-ronds.png"
image <- rdfReadGreyImage (nom)
ref <- rdfReadGreyImage (nomref)

o <- rdfTextureEcartType(image, 2)

if (interactive ()) {
  display (o, "image ecart type")
}

# Calcul et affichage de son histogramme
nbins <- 256
h <- hist (as.vector (o), breaks = seq (0, 1, 1 / nbins))

# histogramme 2D
h2d <- rdfCalculeHistogramme2D(image, nbins, o, nbins);
if (interactive ()) {
  display (h2d, "histogramme 2D")
}


# Segmentation par binarisation
# avec hist niveau de gris
seuil0g <- 0.5
seuil1g <- 0.58
seuil2g <- 0.39
seuil3g <- 0.42
seuil4g <- 0.5
# avec hist niveau de texture
seuil0t <- 0.5
seuil1t <- 0.32
seuil2t <- 0.42
seuil3t <- 0.36
seuil4t <- 0.35

seuil <- seuil4g
binaire <- (image - seuil) >= 0
# image 2 image 3
#binaire <- (image - seuil) < 0

# Affichage des deux images
if (interactive ()) {
#  display (image, nom)
#  display (binaire, "image binaire")
}

#imgerr <- xor(binaire, ref)
imgerr <- binaire != ref
sumerr <- sum(imgerr)
len <- length(image)

taux <- (sumerr*100)/len
print(taux)

#if (interactive ()) {
#  display (imgerr, "image erreur")
#}
