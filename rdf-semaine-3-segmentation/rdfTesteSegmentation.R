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
nom <- "rdf-2-classes-texture-0.png"
nom1 <- "rdf-2-classes-texture-1.png"
nom2 <- "rdf-2-classes-texture-2.png"
nom3 <- "rdf-2-classes-texture-3.png"
nom4 <- "rdf-2-classes-texture-4.png"
reference <- "rdf-masque-ronds.png"
image <- rdfReadGreyImage (nom)
image1 <- rdfReadGreyImage (nom1)
image2 <- rdfReadGreyImage (nom2)
image3 <- rdfReadGreyImage (nom3)
image4 <- rdfReadGreyImage (nom4)
imRef <- rdfReadGreyImage (reference)

# Calcul et affichage de son histogramme
nbins <- 256
#h <- hist (as.vector (image), breaks = seq (0, 1, 1 / nbins))
h1 <- hist (as.vector (image1), breaks = seq (0, 1, 1 / nbins))
#h2 <- hist (as.vector (image2), breaks = seq (0, 1, 1 / nbins))
#h3 <- hist (as.vector (image3), breaks = seq (0, 1, 1 / nbins))
#h4 <- hist (as.vector (image4), breaks = seq (0, 1, 1 / nbins))

# Segmentation par binarisation
seuil <- 0.5
binaire <- (image1 - seuil) >= 0
#binaire <- (image1 - seuil) < 0

rdfPourcentage(binaire, imRef)

# Affichage des deux images
if (interactive ()) {
  #display (image1, nom1)
  display (binaire, "image binaire")
}
