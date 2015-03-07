#Elliot Vanegue
#Gaetant Deflandre

library ("EBImage")
library("MASS")
library("lattice")

(load(file='x_app.data'))
(load(file='classe_app.data'))
(load(file='x_test.data'))
(load(file='classe_test.data'))

n_app <- 300

couleur<-rep('red',n_app)
couleur[classe_app==2]='green'
couleur[classe_app==3]='blue'

plot(x_test, col = couleur, main="donnees de test")

# moyenne classe1
mean <- colMeans(x_app)
mean1 <- colMeans(x_app[classe_app==1,])
mean2 <- colMeans(x_app[classe_app==2,])
mean3 <- colMeans(x_app[classe_app==3,])

print(paste("mean : ", mean))
print(paste("mean 1 : ", mean1))
print(paste("mean 2 : ", mean2))
print(paste("mean 3 : ", mean3))
# covariance intra-classe classe 1
S1 <- cov(x_app[classe_app==1,])
S2 <- cov(x_app[classe_app==2,])
S3 <- cov(x_app[classe_app==3,])

Sw=S1+S2+S3
# covariance inter-classe
Sb=(mean1-mean)%*%t(mean1-mean)+
  (mean2-mean)%*%t(mean2-mean)+
  (mean3-mean)%*%t(mean3-mean)

# Resolution equation
invSw= solve(Sw)
invSw_by_Sb= invSw %*% Sb
Vp<- eigen(invSw_by_Sb)

# Affichage de la droite correspondant au vecteur propre
# dont la valeur propre la plus elevee
pente <- Vp$vectors[2,1]/Vp$vectors[1,1]
abline(a = 0, b = pente, col = "blue")

#produit scalaire
ScalarProduct_test <- x_test
ScalarProduct_test <- x_test %*% (Vp$vectors[,1] / sqrt(sum(Vp$vectors[,1]*Vp$vectors[,1])))

#projection des points
x_test_ACP <- x_test
x_test_ACP[,1] = ScalarProduct_test * Vp$vectors[1,1]
x_test_ACP[,2] = ScalarProduct_test * Vp$vectors[2,1]

#affichage des points projetes
points(x_test_ACP[classe_test==1,], col="red")
points(x_test_ACP[classe_test==2,], col="green")
points(x_test_ACP[classe_test==3,], col="blue")

#////////////////// ALD ///////////////////////
x_app_ACP.lda<-lda(ScalarProduct_test, classe_app)
assigne_app<-predict(x_app_ACP.lda, newdata = ScalarProduct_test)
# Estimation des taux de bonnes classifications
table_classification_app <-table(classe_app, assigne_app$class)
print("matrice de confusion :")
print(table_classification_app)

# table of correct class vs. classification
diag(prop.table(table_classification_app, 1))
# total percent correct
taux_bonne_classif_app <-sum(diag(prop.table(table_classification_app)))
print(paste("taux de bonne classification", taux_bonne_classif_app))

# forme : les classe d'assignation fournie par l'ALD
shape<-rep(1,n_app)
shape[assigne_app$class==2]=2
shape[assigne_app$class==3]=3
# Affichage des projections apprentissage classees
plot(x_test,col=couleur,pch=shape,xlab = "X1", ylab = "X2")

# #produit scalaire
# ScalarProduct_app <- x_app
# ScalarProduct_app <- x_app %*% (Vp$vectors[,1] / sqrt(sum(Vp$vectors[,1]*Vp$vectors[,1])))
# 
# #projection des points
# x_app_ACP <- x_app
# x_app_ACP[,1] = ScalarProduct_app * Vp$vectors[1,1]
# x_app_ACP[,2] = ScalarProduct_app * Vp$vectors[2,1]
# 
# #affichage des points projetes
# points(x_app_ACP[classe_app==1,], col="red")
# points(x_app_ACP[classe_app==2,], col="green")
# points(x_app_ACP[classe_app==3,], col="blue")
# 
# #////////////////// ALD ///////////////////////
# x_app_ACP.lda<-lda(ScalarProduct_app, classe_app)
# assigne_app<-predict(x_app_ACP.lda, newdata = ScalarProduct_app)
# # Estimation des taux de bonnes classifications
# table_classification_app <-table(classe_app, assigne_app$class)
# print("matrice de confusion :")
# print(table_classification_app)
# 
# # table of correct class vs. classification
# diag(prop.table(table_classification_app, 1))
# # total percent correct
# taux_bonne_classif_app <-sum(diag(prop.table(table_classification_app)))
# print(paste("taux de bonne classification", taux_bonne_classif_app))
# 
# # forme : les classe d'assignation fournie par l'ALD
# shape<-rep(1,n_app)
# shape[assigne_app$class==2]=2
# shape[assigne_app$class==3]=3
# # Affichage des projections apprentissage classees
# plot(x_app,col=couleur2,pch=shape,xlab = "X1", ylab = "X2")