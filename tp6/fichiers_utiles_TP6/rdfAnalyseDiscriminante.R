#Elliot Vanegue
#Gaetan Deflandre

library ("EBImage")
library("MASS")
library("lattice")

#(load(file='simul-2015.Rdata'))
(load(file='Iris_x2_x4.Rdata'))

#//////////// APPRENTISSAGE ////////////////
couleur<-rep('red',n_app)
couleur[classe_app==2]='blue'
couleur[classe_app==3]='green'

plot(x_app, col = couleur, main="apprentissage")

couleur2<-rep('red',n_test)
couleur2[classe_test==2]='blue'
couleur2[classe_test==3]='green'

plot(x_test, col = couleur2, main="test")

#classe 1
M1<-seq(1,2)
M1[1] = mean(x_app[classe_app==1,1])
M1[2] = mean(x_app[classe_app==1,2])
print(paste("vecteur moyenne classe rouge, attribut 1 : ", M1[1]))
print(paste("vecteur moyenne classe rouge, attribut 2 : ", M1[2]))

#classe 2
M2<-seq(1,2)
M2[1] = mean(x_app[classe_app==2,1])
M2[2] = mean(x_app[classe_app==2,2])
print(paste("vecteur moyenne classe bleu, attribut 1 : ", M2[1]))
print(paste("vecteur moyenne classe bleu, attribut 2 : ", M2[2]))

#classe 3
M3<-seq(1,2)
M3[1] = mean(x_app[classe_app==3,1])
M3[2] = mean(x_app[classe_app==3,2])
print(paste("vecteur moyenne classe verte, attribut 1 : ", M3[1]))
print(paste("vecteur moyenne classe verte, attribut 2 : ", M3[2]))

#covariance
Sigma1<-matrix(1,2,2)
Sigma2<-matrix(1,2,2)
Sigma3<-matrix(1,2,2)
s<-matrix(1,2,2)
for(i in 1:2) {
  for(j in 1:2) {
    Sigma1[i,j]=cov(as.vector(x_app[classe_app==1,i]), as.vector(x_app[classe_app==1,j]))
    Sigma2[i,j]=cov(as.vector(x_app[classe_app==2,i]), as.vector(x_app[classe_app==2,j]))
    Sigma3[i,j]=cov(as.vector(x_app[classe_app==3,i]), as.vector(x_app[classe_app==3,j]))
    s[i,j] = cov(as.vector(x_app[classe_app,i]), as.vector(x_app[classe_app,j]))
  }
}
print("covariance classe 1 : ")
print(Sigma1)

print("covariance classe 2 : ")
print(Sigma2)

print("covariance classe 3 : ")
print(Sigma3)

print("ecart type : ")
print(s)

#//////////// Analyse linéaire discriminante /////////////
# Grille d estimation de la densite de probabilite en 50 intervalles selon 1er attribut
xp1<-seq(min(x_app[,1]),max(x_app[,1]),length=50)
# Grille d estimation de la densite de probabilite en 50 intervalles selon 2eme attribut
xp2<-seq(min(x_app[,2]),max(x_app[,2]),length=50)
grille<-expand.grid(x1=xp1,x2=xp2)
x_app.lda<-lda(x_app,classe_app)
grille = cbind(grille[,1], grille[,2])
Zp<-predict(x_app.lda,grille)
assigne_test<-predict(x_app.lda, newdata=x_test)
# Estimation des taux de bonnes classifications
table_classification_test <-table(classe_test, assigne_test$class)
# table of correct class vs. classification
diag(prop.table(table_classification_test, 1))
# total percent correct
taux_bonne_classif_test <-sum(diag(prop.table(table_classification_test)))
print(paste("taux de bonne classification : ", taux_bonne_classif_test))

# Creation du vecteur contenant le code de la forme des donnees test assignees aux classes - code initialise a 1
shape<-rep(1,n_test)
# forme des donnees assignees a la classe 2
shape[assigne_test$class==2]=2
shape[assigne_test$class==3]=3
# Affichage avec code couleur et forme adaptees
plot(x_test,col=couleur2,pch=shape,xlab = "X1", ylab = "X2", main="Analyse linéaire discriminante")

xp1<-seq(min(x_app[,1]),max(x_app[,1]),length=50)
xp2<-seq(min(x_app[,2]),max(x_app[,2]),length=50)
grille<-expand.grid(x1=xp1,x2=xp2)
grille=cbind(grille[,1],grille[,2])
Zp<-predict(x_app.lda,grille)
zp<-Zp$post[,3]-pmax(Zp$post[,2],Zp$post[,1])
contour(xp1,xp2,matrix(zp,50),add=TRUE,levels=0,drawlabels=FALSE)

zp2<-Zp$post[,2]-pmax(Zp$post[,3],Zp$post[,1])
contour(xp1,xp2,matrix(zp2,50),add=TRUE,levels=0,drawlabels=FALSE)

print("")
#//////////// TEST ////////////////

#classe 1
m1<-seq(1,2)
m1[1] = mean(x_test[classe_app==1,1])
m1[2] = mean(x_test[classe_app==1,2])
print(paste("vecteur moyenne classe 1, attribut 1 : ", m1[1]))
print(paste("vecteur moyenne classe 1, attribut 2 : ", m1[2]))

#classe 2
m2<-seq(1,2)
m2[1] = mean(x_test[classe_app==2,1])
m2[2] = mean(x_test[classe_app==2,2])
print(paste("vecteur moyenne classe 2, attribut 1 : ", m2[1]))
print(paste("vecteur moyenne classe 2, attribut 2 : ", m2[2]))

#classe 3
m3<-seq(1,2)
m3[1] = mean(x_test[classe_app==3,1])
m3[2] = mean(x_test[classe_app==3,2])
print(paste("vecteur moyenne classe 3, attribut 1 : ", m3[1]))
print(paste("vecteur moyenne classe 3, attribut 2 : ", m3[2]))

#//////////////// quadratique ///////////////////////////

# Grille d estimation de la densite de probabilite en 50 intervalles selon 1er attribut
xp1<-seq(min(x_app[,1]),max(x_app[,1]),length=50)
# Grille d estimation de la densite de probabilite en 50 intervalles selon 2eme attribut
xp2<-seq(min(x_app[,2]),max(x_app[,2]),length=50)
grille<-expand.grid(x1=xp1,x2=xp2)
x_app.qda<-qda(x_app,classe_app)
grille = cbind(grille[,1], grille[,2])
Zp<-predict(x_app.qda,grille)
assigne_test<-predict(x_app.qda, newdata=x_test)
# Estimation des taux de bonnes classifications
table_classification_test <-table(classe_test, assigne_test$class)
# table of correct class vs. classification
diag(prop.table(table_classification_test, 1))
# total percent correct
taux_bonne_classif_test <-sum(diag(prop.table(table_classification_test)))
print(paste("taux de bonne classification : ", taux_bonne_classif_test))

# Creation du vecteur contenant le code de la forme des donnees test assignees aux classes - code initialise a 1
shape<-rep(1,n_test)
# forme des donnees assignees a la classe 2
shape[assigne_test$class==2]=2
shape[assigne_test$class==3]=3
# Affichage avec code couleur et forme adaptees
plot(x_test,col=couleur2,pch=shape,xlab = "X1", ylab = "X2", main="Analyse quadratique disciminante")

xp1<-seq(min(x_app[,1]),max(x_app[,1]),length=50)
xp2<-seq(min(x_app[,2]),max(x_app[,2]),length=50)
grille<-expand.grid(x1=xp1,x2=xp2)
grille=cbind(grille[,1],grille[,2])
Zp<-predict(x_app.qda,grille)
zp<-Zp$post[,3]-pmax(Zp$post[,2],Zp$post[,1])
contour(xp1,xp2,matrix(zp,50),add=TRUE,levels=0,drawlabels=FALSE)

zp2<-Zp$post[,2]-pmax(Zp$post[,3],Zp$post[,1])
contour(xp1,xp2,matrix(zp2,50),add=TRUE,levels=0,drawlabels=FALSE)



tmp = x_app
x_app = x_test
x_test = tmp


# Grille d estimation de la densite de probabilite en 50 intervalles selon 1er attribut
xp1<-seq(min(x_app[,1]),max(x_app[,1]),length=50)
# Grille d estimation de la densite de probabilite en 50 intervalles selon 2eme attribut
xp2<-seq(min(x_app[,2]),max(x_app[,2]),length=50)
grille<-expand.grid(x1=xp1,x2=xp2)
x_app.lda<-lda(x_app,classe_app)
grille = cbind(grille[,1], grille[,2])
Zp<-predict(x_app.lda,grille)
assigne_test<-predict(x_app.lda, newdata=x_test)
# Estimation des taux de bonnes classifications
table_classification_test <-table(classe_test, assigne_test$class)
# table of correct class vs. classification
diag(prop.table(table_classification_test, 1))
# total percent correct
taux_bonne_classif_test <-sum(diag(prop.table(table_classification_test)))
print(paste("taux de bonne classification : ", taux_bonne_classif_test))

# Creation du vecteur contenant le code de la forme des donnees test assignees aux classes - code initialise a 1
shape<-rep(1,n_test)
# forme des donnees assignees a la classe 2
shape[assigne_test$class==2]=2
shape[assigne_test$class==3]=3
# Affichage avec code couleur et forme adaptees
plot(x_test,col=couleur2,pch=shape,xlab = "X1", ylab = "X2", main="Analyse linéaire discriminante")

xp1<-seq(min(x_app[,1]),max(x_app[,1]),length=50)
xp2<-seq(min(x_app[,2]),max(x_app[,2]),length=50)
grille<-expand.grid(x1=xp1,x2=xp2)
grille=cbind(grille[,1],grille[,2])
Zp<-predict(x_app.lda,grille)
zp<-Zp$post[,3]-pmax(Zp$post[,2],Zp$post[,1])
contour(xp1,xp2,matrix(zp,50),add=TRUE,levels=0,drawlabels=FALSE)

zp2<-Zp$post[,2]-pmax(Zp$post[,3],Zp$post[,1])
contour(xp1,xp2,matrix(zp2,50),add=TRUE,levels=0,drawlabels=FALSE)

print("")