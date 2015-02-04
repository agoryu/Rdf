1) nbin => nombre de gris pris en compte pour l'histogramme
2) sueil => valeur du gris à partir duquel on arrete de prendre en
compte les gris dans l'histogramme

image 0 => 0.5 -> 0.5
image 1 => 0.6 -> 0.4
image 2 (inverse) => 0.5 -> 0.55
image 3 (inverse) => 0.5 -> 0.47
image 4 (inverse) => 0.38 -> 0.6

image => 0.12
image1 => 3.30
image2 => 16.44
image3 => 66.85 -> revoir
image4 => 45.89 -> revoir

pas binarisable avec seulement le niveau de gris

a*texture + b*gris + c = 0
a*texture + b*gris + c < 0
a*texture + b*gris + c > 0
