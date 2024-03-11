##Exercice 1, importer des données 
#Les fonctions getwd(),setwd, et read.csv

setwd(dir= "L:/BUT/SD/Promo 2023/ilunion/Programmation de la statistique/dataset" )#montre l'emplacement du fichier csv 
getwd()
bodies_karts=read.csv(file="bodies_karts.csv",header =TRUE,sep = ";",dec = "," )#lire le fichier csv 
drivers=read.csv(file="drivers.csv",header =TRUE,sep = ";",dec = "," )
gliders=read.csv(file="gliders.csv",header =TRUE,sep = "|",dec = "." )
tires=read.csv(file="tires.csv",header =TRUE,sep = "\t",dec = "," )
dim(bodies_karts)#affiche la dimension (ligne/colonne) du dataframe 
dim(drivers)
dim(gliders)
dim(tires)

##Exercice 2- statistique 
summary(bodies_karts)#	Produit un résumé statistique des données dans un objet
summary(drivers)
summary(gliders)
summary(tires)
plot(x=drivers$Weight,
     y=drivers$Acceleration,
     main="Drivers:Weight/Acceleration")
# le $ permet d'accéder à une colonne d'un dataframe  
#la commande plot permet elle de te tracer le nuage de point *
#Réponse à la question posé: 
#Il semble que les deux variables soient corrélées négativement
#Il y a autant de points mais ils sont superposés car certains drivers ont les mêmes statistiques

cor(x=drivers$Weight,
    y=drivers$Acceleration)
#calcul le coefficient de coorélation une variable par rapport à une autre 

#calcul de vérification avec la covarience et sa formuule en décomposant les calculs sinon il voit flou R 
#formule: cov(X,Y)/ecrat-type de X* ecart-type de Y
CovXY=cov(x=drivers$Weight,
          y=drivers$Acceleration)
sX=sd(drivers$Weight) #sd fonction qui calcule l'ecart-type 
sY=sd(drivers$Acceleration)
print(CovXY/(sX*sY))#résultat de la formule 

#calcul du coefficient de détermination 
coefcorr=cor(x=drivers$Weight,
             y=drivers$Acceleration)#déclaration de la variable qui calcule la coorrélation 
coefdeter=coefcorr^2 #calcul du coefficient de détermination; la coorrélation à la puissance 2
print(coefdeter)

matriceCor = cor(drivers[ , - 1])
matriceCor = round(matriceCor , 2)
#la fonction round arrondie les décimal et après la virgule on spécifie le nombre de chiffres qu'on veut après la virgule 
View(matriceCor)#la fonction view nous permet de voir la matrice qu'on a crée
#Toutes les variables semblent fortement corrélées entre elles.

#commande à executer qu'une seule fois
install.packages("corrplot")