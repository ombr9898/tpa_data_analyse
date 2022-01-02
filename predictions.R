
#jointure

jointure = (inner_join(immatriculation,client10,by=NULL,copy = FALSE))
copie_jointure=jointure


jointure=jointure[!is.na(jointure),]


#suppression de champs 
copie_jointure <- subset(copie_jointure, select = -immatriculation)
copie_jointure <- subset(copie_jointure, select = -marque)
copie_jointure <- subset(copie_jointure, select = -nom)
copie_jointure <- subset(copie_jointure, select = -puissance)
copie_jointure <- subset(copie_jointure, select = -longueur)
copie_jointure <- subset(copie_jointure, select = -nbPlaces)
copie_jointure <- subset(copie_jointure, select = -nbPortes)
copie_jointure <- subset(copie_jointure, select = -couleur)
copie_jointure <- subset(copie_jointure, select = -occasion)
copie_jointure <- subset(copie_jointure, select = -prix)

# 
copie_jointure$categorie <- as.factor(copie_jointure$categorie)
copie_jointure$age <- as.integer(copie_jointure$age)
copie_jointure$sexe<- as.factor(copie_jointure$sexe)
copie_jointure$taux <- as.integer(copie_jointure$taux)
copie_jointure$situationFamiliale<- as.factor(copie_jointure$situationFamiliale)
copie_jointure$X2eme.voiture <- as.factor(copie_jointure$X2eme.voiture)
copie_jointure$nbEnfantsAcharge <- as.factor(copie_jointure$nbEnfantsAcharge)

#marketing factor
marketing$age <- as.integer(marketing$age)
marketing$sexe<- as.factor(marketing$sexe)
marketing$taux <- as.integer(marketing$taux)
marketing$situationFamiliale<- as.factor(marketing$situationFamiliale)
marketing$X2eme.voiture <- as.factor(marketing$X2eme.voiture)
marketing$nbEnfantsAcharge <- as.factor(marketing$nbEnfantsAcharge)
#verif

NbrNA=c()
for (i in names(copie_jointure)){
  NbrNA=append(NbrNA,sum(is.na(copie_jointure[i])))
}
copie_jointure=copie_jointure[!is.na(copie_jointure$age),]
copie_jointure=copie_jointure[!is.na(copie_jointure$sexe),]
copie_jointure=copie_jointure[!is.na(copie_jointure$taux),]
copie_jointure=copie_jointure[!is.na(copie_jointure$situationFamiliale),]
copie_jointure=copie_jointure[!is.na(copie_jointure$nbEnfantsAcharge),]
copie_jointure=copie_jointure[!is.na(copie_jointure$X2eme.voiture),]
copie_jointure=copie_jointure[!is.na(copie_jointure$categorie),]


# Construction des ensembles d'apprentissage et de test
jointure_EA <- copie_jointure[1:65538,]
jointure_ET <- copie_jointure[65539:98308,]




-------------------- arbres de décision------------------------------
library(C50)
library(rpart)
library(tree)

---C5.0-------------------------------------------------------------

vars <- names(subset(copie_jointure, select = -categorie))
time0=Sys.time()
tree_c50 <- C5.0(x = jointure_EA[, vars], y = jointure_EA$categorie)
time_c50=as.double(Sys.time()-time0)
plot(tree_c50, type="simple")

# Test du classifieur : classe prédite
result.tree_C50_1 <- predict(tree_c50, jointure_ET, type="class")
result.tree_C50_1 
jointure_ET$result=result.tree_C50_1

# Matrice de confusion (avec le package caret)
matrix_c50=confusionMatrix(jointure_ET$categorie, result.tree_C50_1)

# Test du classifieur : probabilites pour chaque prediction
p.tree_C50_1 <- predict(tree_C50_1, jointure_ET)

---rpart----------------------------------------------------------

time0=Sys.time()
tree_rpart <- rpart(categorie~., jointure_EA)
time_rpart=as.double(Sys.time()-time0)
plot(tree_rpart)
text(tree_rpart, pretty=0)


# Test du classifieur : classe prédite
result.tree_rpart <- predict(tree_rpart, jointure_ET, type="class")
jointure_ET$result2=result.tree_C50_2

# Matrice de confusion (avec le package caret)
matrix_rpart=confusionMatrix(jointure_ET$categorie, result.tree_rpart)

---tree----------------------------------------------------------
  
time0=Sys.time()
tree_tree <- tree(categorie~., jointure_EA)
time_tree=as.double(Sys.time()-time0)
plot(tree_tree)
text(tree_tree, pretty=0)


# Test du classifieur : classe prédite
result.tree_tree <- predict(tree_tree, jointure_ET, type="class")
jointure_ET$result3=result.tree_tree

# Matrice de confusion (avec le package caret)
matrix_tree=confusionMatrix(jointure_ET$categorie, result.tree_tree)


------------------------------- k-nearest neighbors------------------------ 
library(kknn)
#pour k=9
time0=Sys.time()
kknn <- kknn(categorie~., jointure_EA, jointure_ET, k=9)
time_knn=as.double(Sys.time()-time0)

matrix_knn=confusionMatrix(jointure_ET$categorie, kknn$fitted.values)


#pour k=20
time0=Sys.time()
kknn <- kknn(categorie~., jointure_EA, jointure_ET, k=20)
time_knn20=as.double(Sys.time()-time0)
# Matrice de confusion
matrix_knn20=confusionMatrix(jointure_ET$categorie, kknn$fitted.values)


-----------------------------Ramdom_Forest---------------------
library(randomForest)
time0=Sys.time()
rf<- randomForest(categorie~., jointure_EA ,ntree=300,mtry=3)
time_rf=as.double(Sys.time()-time0)
# Test du classifieur : classe prédite

rf_classifieur <- predict(rf,jointure_ET, type="response")

# Matrice de confusion 
matrix_RF=confusionMatrix(jointure_ET$categorie, rf_classifieur)




-----------------------------------support_vecteur_machine
library(e1071)
# Apprentissage du classifeur de type 
time0=Sys.time()
svm <- svm(categorie~., jointure_EA, probability=TRUE, cost=9)
time_svm=as.double(Sys.time()-time0)
# Test du classifieur : classe prédite
length(levels(jointure_ET))
length(levels(marketing))
result.svm <- predict(svm, jointure_ET, type="response")

# Matrice de confus
matrix_svm=confusionMatrix(jointure_ET$categorie, result.svm)

# Test du classifieur : probabilités pour chaque prédiction
svm_prob <- predict(svm, jointure_ET, probability=TRUE)

# Recuperation des probabilités associées aux prédictions
svm_prob <- attr(svm_prob, "probabilities")

# Conversion en un data frame  
svm_prob <- as.data.frame (svm_prob)


----------------------------------réseaux de neurones--------------
#Commentaire : le réseaux de neurones ne marche pas 
# Apprentissage du classifeur de type nn
library(nnet)
  
ll=list()
for (i in 1:10){
    rn=nnet(categorie~., jointure_EA, size=i)
    result.rn=predict(rn, jointure_ET, type="class")
    ll=append(ll,list(table(result.rn)))
}
names(ll)= 1:10

#  regroupement des données
time_element=c(time_c50,time_rpart,time_tree,time_knn,time_knn20,time_rf,time_svm)
element_conf=c(matrix_c50[["overall"]][["Accuracy"]],
        matrix_rpart[["overall"]][["Accuracy"]],
        matrix_tree[["overall"]][["Accuracy"]],
        matrix_knn[["overall"]][["Accuracy"]],
        matrix_knn20[["overall"]][["Accuracy"]],
        matrix_RF[["overall"]][["Accuracy"]],
        matrix_svm[["overall"]][["Accuracy"]])
name_conf=c('tree_C50','tree_rpart','tree_tree','knn_9','knn_20','Random forest','svm')

tableconf=data.frame(name_conf,element_conf,time_element)
write.csv(tableconf,'tableconf.csv')

#marketing prediction
jointure_ET_2=subset(jointure_ET,select = -categorie)
jj=rbind(jointure_ET_2,marketing)
jj=jj[32771:32790,]
rf_classifieur_marketing <- predict(rf,jj, type="response", proximity=TRUE)
result.tree_C50_marketing <- predict(tree_c50, jj, type="class")
marketing$categorie_predict_RF=rf_classifieur_marketing[["predicted"]]
marketing$categorie_predict_C50=result.tree_C50_marketing
write.csv(marketing,"marketing_pred.csv")
marketing <- subset(marketing, select = -categorie_predict)
