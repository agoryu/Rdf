## Code R

### Q1

*bins* correspond aux nombres de division sur l'axe horizontal de l'histogramme, 
càd les niveau de gris. On prend une valeur de 256 car il y a 256 niveaux de gris 
dans une image numérique.

### Q2

*seuil* correspond au moment où un pixel de fond devient pixel objet.

### Conclusion segmentation par binarisation

Conclure sur la possibilité ou pas de binariser toutes les images fournies 
en associant à chaque pixel son niveau de gris comme seul attribut pour la 
segmentation. 

Lorsque l'image à binariser est trop détériorer, avec un bruit de forte 
amplitude par exemple (bruit sur toute les nuances de gris). Il est 
difficile de déterminer les pixels du fond et ce de l'objet par un 
simple seuillage pixel par pixel.

### Histogramme des niveaux de texture

#### rdfMoyenneImage

Cette fonction applique un filtre moyenneur avec un masque qui est une matrice 
N\*N où N vaut taille\*2+1.


#### rdfTextureEcartType

Calcul de l'ecart type de la texture de l'image (la différence entre l'image 
et l'image moyenné).


#### Comment fixe-t-on la dimension du voisinage de calcul grâce à l'argument taille passé à la fonction?

On informe du voisinage qui nous intéresse grace au paramètre taille passé 
aux fonctions. Ce paramètre, tel un rayon, indique le nombre de pixel à prendre 
en compte autour du pixel cible. Si le taille = 2 alors masque sera une matrice 
de 5*5 pixels.
