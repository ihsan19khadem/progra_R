#Exercicce 1  Importer les données
#importation du fichier fao.csv

#OUVRIR UN CSV :
getwd()
#ctrl shift h 
#dfnomcsv=read.csv("nom.csv",header=TRUE,sep=";",dec=",")


setwd(dir= "L:/BUT/SD/Promo 2023/ikhadem/Progra_statistique")
getwd()
#lecture du fichier csv
fao =read.csv(file="fao.csv", header =TRUE, sep = ";",dec = "," )

View(fao)# 186 pays en tout
summary(fao) #un résumé statistique des données dans un objet



#Exercice 2  Statistiques descriptives 

# la disponibilité alimentaire moyenne mondiale en Kcal/personne/jour
mean(fao$Dispo_alim, na.rm = TRUE)#moyenne
sum(fao$Dispo_alim, na.rm = TRUE)#addition

#le nombre d’habitant dans le monde
sum(fao$Population, na.rm = TRUE)#addition

#l’écart-type du volume des exportations de viande ? Et des importations de viande ?
sd(fao$Export_viande, na.rm = TRUE)
sd(fao$Import_viande, na.rm = TRUE)

#la médiane du volume de production de viande
median(fao$Prod_viande, na.rm = TRUE)

#les quartiles du nombre de Kcal de disponibilité alimentaire
quantile(fao$Dispo_alim, probs = c(0.25, 1, 0.75))

#les centiles du volume d’importation de viande
quantile(fao$Import_viande, probs = seq(0,1,0.01))

         
#Exercice 3   Tris et filtres

#1 extraire les lignes du dataset avec les 5 pays les moins peuplés
pays5 =head(fao[order(fao$Population),],5)
print(pays5)
View(pays5)

#2 extraire les lignes du dataset avec les 5 pays les plus peuplés
rang = order(fao$Population, decreasing = TRUE)
resultat = head(fao[ rang , ], n = 5)
View(resultat)

#3 les 5 pays qui produisent le plus de viande
rang = order(fao$Prod_viande, decreasing = TRUE)
resultat = head(fao[ rang , ], n = 5)
View(resultat)

#4 les 5 pays qui importent le plus de viande
rang = order(fao$Import_viande, decreasing = TRUE)
resultat = head(fao[ rang , ], n = 5)
View(resultat)

#5  les pays qui ont une disponibilité alimentaire supérieure ou égale à 2300 kcal. Combien de pays sont concernés ?
resultat = subset(fao, Dispo_alim>=2300)
View(resultat)

#6 les pays qui ont une disponibilité alimentaire strictement supérieure à 3500 kcal et qui importe un volume de viande supérieure ou égale à 1 000 000 tonnes par an. Combien de pays sont concernés ?
resultat = subset(fao, Dispo_alim > 3500  & Import_viande > 1000)
View(resultat)

#7 une requête pour extraire les lignes du dataset avec la France et la Belgique.
resultat = subset(fao,Nom %in% c("France","Belgique"))
View(resultat)

#Exercice 4 Modifier le dataframe

#1 Ajouter une colonne nommée part_export qui correspond à la part des exportations de viande par rapport à la production de viande.
fao$Part_export<-df$Export_viande/df$Prod_viande

#2 Ajouter une colonne nommée dispo_alim_pays qui correspond à la disponibilité total du pays en Kcal/jour
fao$Dispo_alim_pays<-df$Dispo_alim*df$Population

#3 Exporter le nouveau dataframe dans un fichier csv nommé ExportTp2.csv avec la fonction write.table()
write.table(x = df, file = "ExportTp2.csv")

#4 Calculer la somme de la disponibilité alimentaire mondiale
dispo_alim_mondiale = sum(fao$Dispo_alim_pays, na.rm=TRUE)
dispo_alim_mondiale

#5 le besoin énergétique moyen d’une adulte est de 2300 kcal par jour. Combien d’adulte pourrait-on nourrir avec la disponibilité alimentaire mondiale ?
dispo_alim_mondiale/2300


#Exercice 5 Corrélation

#1 Représenter graphiquement dans un nuage de points le lien entre Prod_viande et Export_viande. Commenter le lien entre ces deux variables ?
plot(x = fao$Prod_viande,
     y = fao$Export_viande, 
     main = "Pays : Prod_viande / Export_viande")

#2 Calculer le coefficient de corrélation de cette relation avec la fonction cor()
cor(x = fao$Prod_viande,
    y = fao$Export_viande)

#3 Construire la matrice des corrélations des variables quantitatives avec la fonction cor(). Afficher cette matrice dans une vue et arrondisser les valeurs avec deux décimales uniquements. Commenter la relation la plus forte, la plus faible.
matriceCor = cor(fao[ , - 1] , use = "complete.obs")
matriceCor = round(matriceCor , 2)
View(matriceCor)

#4 Pour mieux visualiser ces corrélations, nous allons utiliser un package qui ne fait pas parti des packages par défaut. Installer le package corrplot avec la fonction install.packages() sauf s'il est déjà installé.
#commande à executer qu'une seule fois
install.packages("corrplot")

#5 Construire une Corrélogramme avec la fonction corrplot()
library(corrplot) #je charge mon package pour pouvoir utiliser ses fonctionalités
corrplot(matriceCor, method="circle")




