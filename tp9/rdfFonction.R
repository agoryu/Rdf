str2int <- function(x) { 
  strtoi(charToRaw(x),16L)-96 
}

getIndice <- function(ens) {
  
  #taille de l'ensemble
  n <- length(ens)
  
  #creation d'une matrice de taille 26 sur nb mot de booleen
  #indique quel lettre sont presentent pour chaque mot
  mat = matrix(rep(0,26*n),nrow=26, ncol=n);
  for (i in 1:n)
  {
    c = str2int(ens[i]);
    mat[c,i] <- 1;
  }
  
  #creation d'une liste de taille 26 avec le nombre de mot contenant chaque lettre
  h <- vector(length=26)
  for (i in 1:26)
  {
    h[i] = sum(mat[i,])
  }
  
  #probabilite qu'une lettre apparaisse dans un mot
  p <- h / n
  #inverse de la probabilite precedente
  pInv <- 1 - p
  
  #calcul de l entropie
  entropie <- -(log2(p ^ p) + log2(pInv ^ pInv))
  
  #position de la lettre la plus interessante a choisir
  position <- which.max(entropie)
  
  ensA <- c()
  ensS <- c()
  
  for (i in 1:n)
  {
    if(mat[position,i] == 1 ){
      ensA = c(ensA, ens[i])
    } else {
      ensS = c(ensS, ens[i])
    }
  }
  
  return(list(pos = position, ensembleA = ensA, ensembleS = ensS))
} 

jouer <- function(ens) {
  
  mot <- scan("",what="character",nlines=1);
  size <- nchar(mot)
  tmpEns <- ens
  
  while(length(tmpEns) != 1) {
    res <- getIndice(tmpEns)
    
     print(paste("lettre = ", int2st(res$pos)))
    
    if(containLetter(res$pos, mot) == TRUE) {
      tmpEns <- res$ensembleA
    } else {
      tmpEns <- res$ensembleS
    }
  }

  print(paste("mot = ", tmpEns))
}

containLetter <- function(letter, word) {
  
  size <- nchar(word)
  chaine <- unlist(strsplit(word, split=""))
  
  for(i in 1:size) {
    if(str2int(chaine[i]) == letter) {
      return(TRUE);
    }
  }
  return(FALSE);
}

# partage <- function(ens) {
#   
#   if(length(ens) == 0)
#     return ()
#   
#   attribut <- getIndice(ens)
#   
#   lettre1 <- partage(attribut["ensembleA"])
#   lettre2 <- partage(attribut["ensembleS"])
#   
#   return c(lettre1, lettre2)
# }

#affichage arbre
int2st <- function(x) {
  x <- x+96
  mode(x) <- "raw"
  return(rawToChar(x))
}