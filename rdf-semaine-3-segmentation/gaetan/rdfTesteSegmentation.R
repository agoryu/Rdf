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
nom <- "rdf-2-classes-texture-2.png"
nomref <- "rdf-masque-ronds.png"
image <- rdfReadGreyImage (nom)
ref <- rdfReadGreyImage (nomref)

o <- rdfTextureEcartType(image, 2)

if (interactive ()) {
#  display (o, "image ecart type")
}

# Calcul et affichage de son histogramme
nbins <- 256
#h <- hist (as.vector (o), breaks = seq (0, 1, 1 / nbins))

# histogramme 2D
h2d <- rdfCalculeHistogramme2D(image, nbins, o, nbins);
if (interactive ()) {
#  display (h2d, "histogramme 2D")
}

# z(x,y) = a*x + b*y +c
# image 0 -> meilleur solution hist gris
#z = 1*image + 0*o
# image 1 -> meilleur solution hist gris
#z = 1*image + 0*o
# image 2 -> meilleur solution équation
z = 1*image + 1*o
# image 3 -> meilleur solution texture
#z = 0*image + 1*o
# image 4 -> meilleur solution texture
#z = 0*image + 1*o

# Normaliser
z = z / max(z)

if (interactive ()) {
  display (z, "équation")
}

h <- hist (as.vector (z), breaks = seq (0, 1, 1 / nbins))

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

# histogramme seuil équation
seuil0e <- 0.5
seuil1e <- 0.58
seuil2e <- 0.43
seuil3e <- 0.36
seuil4e <- 0.35

seuil <- seuil2e
#binaire <- (z - seuil) >= 0
# gris2 girs3 tex1 tex2 tex3 tex4 eq2
binaire <- (z - seuil) < 0

# Affichage des deux images
if (interactive ()) {
#  display (image, nom)
  display (binaire, "image binaire")
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
