# -----------------------------------------------------------------------
# Extraction d'attributs de contours,
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
source ("rdfContours.R")

# Chargement d'un contour
#nom <- "rdf-cercle-80.txt"
#cont <- rdfChargeFichierContour (nom)
nom <- "rdf-rectangle-horizontal.png"
cont <- rdfContour (rdfReadGreyImage (nom))
size <- length(cont)

# fourier
fourier <- rdfTransformeFourier(cont)
fourierAnn <- rdfAnnuleDescFourier(fourier, 0.9)
fourierInv <- fft(fourierAnn, TRUE)

#corde
corde <- rdfAlgorithmeCorde(cont, 0.1)
#corde2 <- rdfAlgorithmeCorde(cont, 0.8)
#corde3 <- rdfAlgorithmeCorde(cont, 1)

# Afficher le contour
plot (corde, main = nom, type = "o", asp = 1, col = "red",
      ylim = rev (range (Im (cont))))

lines(fourierInv, type = "o", asp = 1, col = "blue")
