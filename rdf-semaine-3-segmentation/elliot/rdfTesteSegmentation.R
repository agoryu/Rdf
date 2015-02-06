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
#nom <- "rdf-2-classes-texture-0.png"
#nom <- "rdf-2-classes-texture-1.png"
#nom <- "rdf-2-classes-texture-2.png"
#nom <- "rdf-2-classes-texture-3.png"
nom <- "rdf-2-classes-texture-4.png"
reference <- "rdf-masque-ronds.png"
image <- rdfReadGreyImage (nom)
imRef <- rdfReadGreyImage (reference)

# Calcul et affichage de son histogramme
nbins <- 256
#h <- hist (as.vector (image), breaks = seq (0, 1, 1 / nbins))

# Segmentation par binarisation
seuil <- 0.5
binaire <- (image - seuil) >= 0
#binaire <- (image - seuil) < 0

#rdfPourcentage(binaire, imRef)

texture <- rdfTextureEcartType(image, 2)

h2 <- rdfCalculeHistogramme2D(image, nbins, texture, nbins)

# Affichage des deux images
if (interactive ()) {
  #display (image, nom)
  #display (binaire, "image binaire")
  #display (texture, "texture")
  #h <- hist (as.vector (texture), breaks = seq (0, 1, 1 / nbins))
  display(h2, "test")
}
