# -----------------------------------------------------------------------
# Extraction d'attributs de forme,
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

# Chargement d'une image en niveaux de gris
rdfReadGreyImage <- function (nom) {
  image <- readImage (nom)
  if (length (dim (image)) == 2) {
    image
  } else {
    channel (image, 'red')
  }
}

# Calcul de la surface d'une forme
rdfSurface <- function (im) {
  sum (im)
}

# Calcul d'un moment geometrique
rdfMoment <- function (im, p, q) {
  x <- (1 : (dim (im)[1])) ^ p
  y <- (1 : (dim (im)[2])) ^ q
  as.numeric (rbind (x) %*% im %*% cbind (y))
}

# Calcul d'un moment centre
rdfMomentCentre <- function (im, p, q) {
  # Barycentre
  s <- rdfSurface (im)
  cx <- rdfMoment (im, 1, 0) / s
  cy <- rdfMoment (im, 0, 1) / s
  # Initialiser les vecteurs x et y
  x <- (1 : (dim (im)[1]) - cx) ^ p
  y <- (1 : (dim (im)[2]) - cy) ^ q
  # Calcul du moment centre
  as.numeric (rbind (x) %*% im %*% cbind (y))
}

rdfMatriceInertie <-function (im) {
  u20 <- rdfMomentCentre(im, 2, 0)
  u11 <- rdfMomentCentre(im, 1, 1)
  u02 <- rdfMomentCentre(im, 0, 2)
  mat <- matrix(data = c(u20, u11, u11, u02), nrow = 2)
}

# moment principal d'inertie
rdfValeurPropre <- function(im) {
  mat <-rdfMatriceInertie(im)
  vpropre <- eigen(mat)
  vpropre$values
}

# axe principal d'inertie
rdfVecteurPropre <- function(im) {
  mat <-rdfMatriceInertie(im)
  vpropre <- eigen(mat)
  vpropre$vectors
}

# 3e partie
rdfMomentCentreNormalise <- function(im, p, q) {
  uij <- rdfMomentCentre(im, p, q)
  u00 <- rdfMomentCentre(im, 0, 0)
  nij <- uij / (u00 ^ (1 + (p+q) / 2))
}

rdfMatriceInertieN <-function (im) {
  u20 <- rdfMomentCentreNormalise(im, 2, 0)
  u11 <- rdfMomentCentreNormalise(im, 1, 1)
  u02 <- rdfMomentCentreNormalise(im, 0, 2)
  mat <- matrix(data = c(u20, u11, u11, u02), nrow = 2)
}

rdfValeurPropreN <- function(im) {
  mat <-rdfMatriceInertieN(im)
  vpropre <- eigen(mat)
  vpropre$values
}

rdfVecteurPropreN <- function(im) {
  mat <-rdfMatriceInertieN(im)
  vpropre <- eigen(mat)
  vpropre$vectors
}

# 4e partie
rdfMomentsInvariants <- function(im) {
  n30 <- rdfMomentCentreNormalise(im, 3, 0)
  n12 <- rdfMomentCentreNormalise(im, 1, 2)
  n21 <- rdfMomentCentreNormalise(im, 2, 1)
  n03 <- rdfMomentCentreNormalise(im, 0, 3)
  n20 <- rdfMomentCentreNormalise(im, 2, 0)
  n02 <- rdfMomentCentreNormalise(im, 0, 2)
  n11 <- rdfMomentCentreNormalise(im, 1, 1)
  
  inv1 <- n20 + n02
  print("inv1 = ")
  print(inv1)
  
  inv2 <- (n20 - n02)^2 + (n11)^2
  print("inv2 = ")
  print(inv2)
  
  inv3 <- (n30 - 3 * n12)^2 + (3 * n21 - n03)^2
  print("inv3 = ")
  print(inv3)
  
  inv4 <- (n30 + n12)^2 + (n21 + n03)^2
  print("inv4 = ")
  print(inv4)
  
  inv5 <- (n30 - 3 * n12) * (n30 + n12) * ((n30 + n12)^2 - 3*(n21 + n03)^2) + 
    (3 * n21 - n03) * (n21 + n03) * (3 * (n30 + n12)^2 - (n21 + n03)^2)
  print("inv5 = ")
  print(inv5)
  
}