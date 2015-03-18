str2int <- function(x) { 
  strtoi(charToRaw(x),16L)-96 
}

getIndice <- function(ens) {
  #creation d'une matrice de taille 26 sur nb mot de booleen
  #indique quel lettre sont presentent pour chaque mot
  mat = matrix(rep(0,26*n),nrow=26, ncol=n);
  for (i in 1:n)
  {
    c = str2int(noms[i]);
    mat[c,i] <- 1;
  }
  
  h <- vector(length=26)
  for (i in 1:26)
  {
    h[i] = sum(mat[i,])
  }
  
  totalLettre <- length(ens)
  p <- h / totalLettre
  pInv <- 1 - p
  entropie <- - (log2(p ^ p) + log2(pInv ^ pInv))
  print(entropie)
  which.max(entropie)
} 



#affichage arbre
#int2st <- function(x) {
#x <- x+96
#mode(x) <- "new"
#return(rawToChar(x))
#}