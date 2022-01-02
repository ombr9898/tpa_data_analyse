library(rpart)
library(cluster)
library(ggplot2)
library(tidyverse)


-------------------catalogue
catalogue=read.csv("catalogue_refractor.csv",header = TRUE,sep = ",",dec = "." ,na.strings="NA")
catalogue$longueur=case_when(
startsWith(catalogue$longueur, 't') ~ 'très longue',
  TRUE ~ catalogue$longueur
)

catalogue$marque=case_when(
  startsWith(catalogue$marque, 'Hy') ~ 'Hyundai',
  TRUE ~ catalogue$marque
)
catalogue_element=append({},list(table(catalogue[,1], exclude = NULL)))

j=2
for (i in names(catalogue)){
  if(j<length(catalogue)+1){
    x=list(table(catalogue[,j], exclude = NULL))
    catalogue_element=append(catalogue_element,x,after=j)
    j=j+1
  }
}

names(catalogue_element)=names(catalogue)
------------------------client2
client2=read.csv("Clients_2_refractor.csv",header = TRUE,sep = ",",dec = "." ,na.strings="NA" )

client2$situationFamiliale=case_when(
  startsWith(client2$situationFamiliale, 'C') ~ 'célibataire',
  startsWith(client2$situationFamiliale, 'D') ~ 'divorcé',
  startsWith(client2$situationFamiliale, 'M') ~ 'marié(e)',
  startsWith(client2$situationFamiliale, 'S') ~ 'seul(e)',
  TRUE ~ client2$situationFamiliale
)
client2_element=append({},list(table(client2[,1], exclude = NULL)))
j=2
for (i in names(client2)){
  if(j<length(client2)+1){
    x=list(table(client2[,j], exclude = NULL))
    client2_element=append(client2_element,x,after=j)
    j=j+1
  }
}
client2$situationFamiliale=case_when(startsWith(client2$situationFamiliale,'s')~'Seul'
                                     ,TRUE~client2$situationFamiliale)
names(client2_element)=names(client2)


------------------------client10
client10=read.csv("Clients_10_refractor.csv",header = TRUE,sep = ",",dec = "." ,na.strings="NA")

client10$situationFamiliale=case_when(
  startsWith(client10$situationFamiliale, 'C') ~ 'célibataire',
  startsWith(client10$situationFamiliale, 'D') ~ 'divorcé',
  startsWith(client10$situationFamiliale, 'M') ~ 'marié(e)',
  startsWith(client10$situationFamiliale, 'S') ~ 'seul(e)',
  
  TRUE ~ client10$situationFamiliale
)

client10['sexe'=='NA']=NA

client10_element=append({},list(table(client10[,1], exclude = NULL)))
j=2
for (i in names(client10)){
  if(j<length(client10)+1){
    x=list(table(client10[,j], exclude = NULL))
    client10_element=append(client10_element,x,after=j)
    j=j+1
  }
}

names(client10_element)=names(client10)

------------------------immatricualtion

immatriculation=read.csv("Immatriculations.csv",header = TRUE,sep = ",",dec = "." ,na.strings="NA" )
immatriculation_element=append({},list(table(immatriculation[,1], exclude = NULL)))
j=2
for (i in names(immatriculation)){
  if(j<length(immatriculation)+1){
    print(1)
    x=list(table(immatriculation[,j], exclude = NULL))
    immatriculation_element=append(immatriculation_element,x,after=j)
    j=j+1
  }
}
names(immatriculation_element)=names(immatriculation)


------------------------Marketing
marketing=read.csv("Marketing_refractor.csv",header = TRUE,sep = ",",dec = "." ,na.strings="NA" )
marketing$situationFamiliale=case_when(
  startsWith(marketing$situationFamiliale, 'C') ~ 'célibataire',
  TRUE ~ marketing$situationFamiliale
)
marketing_element=append({},list(table(marketing[,1], exclude = NULL)))
--------------------total_nettoyage 
element= list(catalogue_element,client10_element,client2_element,immatriculation_element,marketing_element)
names(element)=c("catalogue","client10","client2","immatriculation","marketing")


*********************Missing_Data********************
  
-----------------marketing
NbrNA_ma=c()
          
for (i in names(marketing)){
NbrNA_ma=append(NbrNA_ma,sum(is.na(marketing[i])))}
barplot(NbrNA_ma,names.arg = names(marketing))
title(ylab="Number of NA value", line=5, family="Calibri Light")
title(main="Bar chart with Number of NA value of each fields of Marketing")



------------------catalogue
NbrNA_ca=c()

for (i in names(catalogue)){
  NbrNA_ca=append(NbrNA_ca,sum(is.na(catalogue[i])))}
barplot(NbrNA_ca,names.arg = names(catalogue))
title(ylab="Number of NA value", line=5, family="Calibri Light")
title(main="Bar chart with Number of NA value of each fields of catalogue")


------------------client10
NbrNA_cl10=c()

for (i in names(client10)){
  NbrNA_cl10=append(NbrNA_cl10,sum(is.na(client10[i])))}
barplot(NbrNA_cl10,names.arg = names(client10),las=2)
title(main="Bar chart with Number of NA value of each fields of Client10")


--------------------client2
NbrNA_cl2=c()

for (i in names(client2)){
  NbrNA_cl2=append(NbrNA_cl2,sum(is.na(client2[i])))}

barplot(NbrNA_cl2,names.arg = names(client2),las=2)
title(ylab="Number of NA value", line=5, family="Calibri Light")
title(main="Bar chart with Number of NA value of each fields of Client2")

--------------------immatriculation
NbrNA_im=c()
for (i in names(immatriculation)){
  NbrNA_im=append(NbrNA_im,sum(is.na(immatriculation[i])))}
barplot(NbrNA_im,names.arg = names(immatriculation))
title(ylab="Number of NA value", line=5, family="Calibri Light")
title(main="Bar chart with Number of NA value of each fields of Immatriculation")


------------------

-----------------------test valeurs liées 
unique(catalogue$nbPortes)
catcat=list()
for (i in unique(catalogue$nbPortes)){
catcat <- append(catcat,list(table(catalogue[catalogue$nbPortes==i ,]$longueur)))
}
names(catcat)=unique(catalogue$nbPortes)


--------------------valeurs_doubles
duplicated(immatriculation)
dup=c()
dup=append(dup,sum(duplicated(client10)))
dup=append(dup,sum(duplicated(client2)))
dup=append(dup,sum(duplicated(immatriculation)))
dup=append(dup,sum(duplicated(immatriculation$immatriculation)))
dup=append(dup,sum(duplicated(marketing)))
dup=append(dup,sum(duplicated(catalogue)))
par(mar = rep(2, 4))
barplot(dup,names.arg = c("client10","client2","immatriculation","immatriculation fields","marketing","catalogue"))
title(main="Bar chart with Number of duplicated value of each table")




--------------------------
  
--------------doublont supprimer
client2=client2[!duplicated(client2$immatriculation),]
client10=client10[!duplicated(client10$immatriculation),]
immatriculation=immatriculation[!duplicated(immatriculation),]
immatriculation=immatriculation[!duplicated(immatriculation$immatriculation),]



write.csv(immatriculation,'immatriculation_net.csv')
write.csv(marketing,'marketing_net.csv')
write.csv(client10,'client10_net.csv')
write.csv(client2,'client2_net.csv')
write.csv(catalogue,'catalogue_net.csv')





