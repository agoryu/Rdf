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

# Chargement de l'image

nom <- "3classes_100_156_8bits.png"
image <- rdfReadGreyImage (nom)



# Question 1
# ----------

# Calcul et affichage de son histogramme
nbins <- 256
h <- hist (as.vector (image), freq=FALSE, breaks = seq (0, 1, 1 / nbins))



# Question 2
# ----------

# Chargement de l'image omega1
#nom <- "2classes_100_100_8bits_omega1.png"
nom <- "3classes_100_156_8bits_omega1.png"
omega1 <- rdfReadGreyImage (nom)
# Calcul et affichage de son histogramme
h1 <- hist (as.vector (omega1), freq=FALSE, breaks = seq (0, 1, 1 / nbins))

# Chargement de l'image omega2
nom <- "3classes_100_156_8bits_omega2.png"
omega2 <- rdfReadGreyImage (nom)
# Calcul et affichage de son histogramme
h2 <- hist (as.vector (omega2), freq=FALSE, breaks = seq (0, 1, 1 / nbins))

# Chargement de l'image omega3
nom <- "3classes_100_156_8bits_omega3.png"
omega3 <- rdfReadGreyImage (nom)
# Calcul et affichage de son histogramme
h3 <- hist (as.vector (omega3), freq=FALSE, breaks = seq (0, 1, 1 / nbins))

#  Calcul des probas a priori des classes
p_omega1 = sum(h1$counts[0:255])/ sum(h$counts[0:255])
p_omega2 = sum(h2$counts[0:255])/ sum(h$counts[0:255])
p_omega3 = sum(h3$counts[0:255])/ sum(h$counts[0:255])
print(paste("P(omega1) = ", p_omega1, ""))
print(paste("P(omega2) = ", p_omega2, ""))
print(paste("P(omega3) = ", p_omega3, ""))



# Question 6
# ----------

# Premier passage

#  pour le seuil X calcul de l'erreur d'assignation
somme1 = 0:255
somme2 = 0:255
erreur = 0:255

# recherche du minimum
minimum_erreur_classes_1_2 = 1;
seuil_minimum_erreur_classes_1_2 = 0;

for (X in 1:255) {
    # (\sum_{\mathbf{X} \in \hat{\omega_2}} P(\mathbf{X} / \omega_1). P(\omega_1)  
    somme1[X+1] = sum( h1$density[(X+1):256]) / sum(h1$density[1:256] )
    somme1[X+1] = somme1[X+1] * p_omega1
    #\sum_{\mathbf{X} \in \hat{\omega_1}} P(\mathbf{X}  / \omega_2). P(\omega_2)    
    somme2[X+1] = sum( h2$density[1:(X+1)]) / sum(h2$density[1:256] )
    somme2[X+1] = somme2[X+1] * p_omega2

    erreur[X+1] = somme1[X+1] + somme2[X+1]
    
	# seuil corrrespondant à l'erreur minimale
  	if (erreur[X+1] < minimum_erreur_classes_1_2 ) seuil_minimum_erreur_classes_1_2 = X
  	if (erreur[X+1] < minimum_erreur_classes_1_2 ) minimum_erreur_classes_1_2 = erreur[X+1]
}

# Deuxième passage

#  pour le seuil X calcul de l'erreur d'assignation
somme1 = 0:255
somme2 = 0:255
erreur = 0:255

# recherche du minimum
minimum_erreur_classes_2_3 = 1;
seuil_minimum_erreur_classes_2_3 = 0;

for (X in 1:255) {
    # (\sum_{\mathbf{X} \in \hat{\omega_2}} P(\mathbf{X} / \omega_1). P(\omega_1)  
    somme1[X+1] = sum( h2$density[(X+1):256]) / sum(h2$density[1:256] )
    somme1[X+1] = somme1[X+1] * p_omega2
    #\sum_{\mathbf{X} \in \hat{\omega_1}} P(\mathbf{X}  / \omega_2). P(\omega_2)    
    somme2[X+1] = sum( h3$density[1:(X+1)]) / sum(h3$density[1:256] )
    somme2[X+1] = somme2[X+1] * p_omega3

    erreur[X+1] = somme1[X+1] + somme2[X+1]
    
	# seuil corrrespondant à l'erreur minimale
  	if (erreur[X+1] < minimum_erreur_classes_2_3 ) seuil_minimum_erreur_classes_2_3 = X
  	if (erreur[X+1] < minimum_erreur_classes_2_3 ) minimum_erreur_classes_2_3 = erreur[X+1]
}
print(paste("Seuil erreur minimum classes 1 et 2: ", seuil_minimum_erreur_classes_1_2, ""))
print(paste("Taux d'erreur classes 1 et 2: ", minimum_erreur_classes_1_2, ""))
print(paste("Seuil erreur minimum classes 2 et 3: ", seuil_minimum_erreur_classes_2_3, ""))
print(paste("Taux d'erreur classes 2 et 3: ", minimum_erreur_classes_2_3, ""))

seuil_1_2 = seuil_minimum_erreur_classes_1_2/255
seuil_2_3 = seuil_minimum_erreur_classes_2_3/255

width = length(image[,1])
height = length(image[1,])

# mettre tout en noir
binaire_Bayes = (image*0)

for (y in 1:height) {
    #print(paste("RAW: ", y, ""))
    for (x in 1:width) {
        if(image[x,y]>seuil_1_2){
            binaire_Bayes[x,y] = 0.5
        }
        if(image[x,y]>seuil_2_3){
            binaire_Bayes[x,y] = 1
        }
    }
}

# Question 7
# ----------
#il y a des valeurs négatives
finErreur = ((binaire_Bayes - image) + 1) / 2
print(finErreur)
#histerr <- hist (as.vector (finErreur), freq=FALSE, breaks = seq (0, 1, 1 / nbins))
display (finErreur, "erreur")
accumul1 = histerr$density[2]
accumul2 = 0

for (i in 3:255) {
  accumul1 = accumul1 + histerr$density[i]
  accumul2 = accumul2 + histerr$density[i]
}  
print(accumul1)
print(accumul2)
print(accumul1/accumul2)
#display (binaire_Bayes, "image binaire Bayes")
