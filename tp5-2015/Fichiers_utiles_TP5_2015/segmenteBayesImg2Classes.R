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
#source ("fonction.R")

# Chargement de l'image
#nom <- "rdf-2-classes-texture-0.png"
nom <- "2classes_100_100_8bits.png"
image <- rdfReadGreyImage (nom)



# Question 1
# ----------

# Calcul et affichage de son histogramme
nbins <- 256
#h <- hist (as.vector (image), freq=FALSE, breaks = seq (0, 1, 1 / nbins))

# Segmentation par binarisation 0.3
seuil <- 0.36
binaire35 <- (image - 0.35) >= 0
binaire36 <- (image - 0.36) >= 0
binaire37 <- (image - 0.37) >= 0


# Affichage des deux images
if (interactive ()) {
  #display (binaire35, "image binaire 0.35")
  #display (binaire36, "image binaire 0.36")
  #display (binaire37, "image binaire 0.37")
}



# Question 2
# ----------

# Chargement de l'image omega1
nom <- "2classes_100_100_8bits_omega1.png"
omega1 <- rdfReadGreyImage (nom)

# Calcul et affichage de son histogramme
#h1 <- hist (as.vector (omega1), freq=FALSE, breaks = seq (0, 1, 1 / nbins))

# Chargement de l'image omega2
nom <- "2classes_100_100_8bits_omega2.png"
omega2 <- rdfReadGreyImage (nom)

# Calcul et affichage de son histogramme
#h2 <- hist (as.vector (omega2), freq=FALSE, breaks = seq (0, 1, 1 / nbins))

#  Calcul des probas a priori des classes
p_omega1= sum(h1$counts[0:255])/ sum(h$counts[0:255])
p_omega2= sum(h2$counts[0:255])/ sum(h$counts[0:255])
 


# Question 3
# ----------

#  Calcul des probas conditionnelles
#h$counts[80]
#h1$counts[80]
#h$density[80]
#h1$density[80]

X = 91;# gris à 79

p79_I  = h$density[X]
p79_o1 = h1$density[X] / p_omega1
p79_o2 = h2$density[X] / p_omega2
      


# Question 4
# ----------

#  pour le seuil X calcul de l'erreur d'assignation
somme1 = 0:255
somme2 = 0:255
erreur = 0:255


stop()

# recherche du minimum
minimum_erreur = 1;
seuil_minimum_erreur = 0;

for (X in 1:255) {
    # (\sum_{\mathbf{X} \in \hat{\omega_2}} P(\mathbf{X} / \omega_1). P(\omega_1)  
    somme1[X+1] = sum( h1$density[(X+1):256]) / sum(h1$density[1:256] )
    somme1[X+1] = somme1[X+1] * p_omega1
    #\sum_{\mathbf{X} \in \hat{\omega_1}} P(\mathbf{X}  / \omega_2). P(\omega_2)    
    somme2[X+1] = sum( h2$density[(X+1):256]) / sum(h2$density[1:256] )
    somme2[X+1] = somme2[X+1] * p_omega2

    erreur[X+1] = somme1[X+1] + somme2[X+1]
    
	# seuil corrrespondant à l'erreur minimale
  	if (erreur[X+1] < minimum_erreur ) seuil_minimum_erreur = X
  	if (erreur[X+1] < minimum_erreur ) minimum_erreur = erreur[X+1]
}
print(erreur)

seuil = seuil_minimum_erreur/255 
binaire_Bayes <- (image - seuil) >= 0
display (binaire_Bayes, "image binaire Bayes")
