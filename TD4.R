#Exercice 1

#1
setwd("L:/BUT/SD/Promo 2023/ikhadem/Progra_statistique/TD4")
getwd()
velo = read.csv(file = "velov.csv",
              header = TRUE,
              sep = ";", dec=",")

 #2
 summary(velo)
 class(velo$status)
 class(velo$CodePostal)
              
 #3 passer en factor
 velo$status = as.factor(velo$status)
 velo$CodePostal = as.factor(velo$CodePostal)
 print(velo$status)             
 print( velo$CodePostal)
 
 #4
 velo$bornes=ifelse(velo$capacity !=(velo$bikes + velo$stands),"KO","OK")
 table(velo$bornes)
 
#Exercice 2
 
#1 fonction hist pour un histogramme

 hist(velo$stands, main = "distribution des capacity")
 
#2  graphique mais avec 6 classes.

hist(x = velo$capacity,
    main = "Distribution de \n la capacité des stations",
    breaks = 6)
    
#3 même graphique mais en rouge.
 
 hist(x = velo$capacity, 
    main = "Distribution de \n la capacité des stations",
    breaks = 6,
    col = "red")
    
#4 Renommer l axe des abscisses par Capacity.

 hist(x = velo$capacity, 
    main = "Distribution de \n la capacité des stations",
    breaks = 6,
    col = "red",
    xlab = "Capacity")
    
#La fonction abline()

#5 ajouter une ligne horizontale bleue qui à pour ordonné la valeur 100 / personnaliser le trait de la ligne avec l argument lty

abline(h = 100, 
col = "blue", 
lty = 100)

#Les fonctions hist(), lines() et density()  

#6 Construire le même graphique mais avec la densité plutôt que les effectifs. Supprimer l argument break pour rétablir les classes par défaut.
 
hist(x = velo$capacity, 
     main = "Distribution de \n la capacité des stations",
     col = "red",
     probability = TRUE, # si TRUE, les hauteurs des barres représentent des densités de probabilité plutôt que des effectifs
     xlab = "Capacity")

#7 Ajouter la courbe densité de cette distribution à l aide des fonctions lines() et density(). On peut mettre cette courbe en bleu en changeant la taille de la courbe avec l argument lwd

lines(density(velo$capacity),
      lty = 2,
      col = "grey",
      lwd = 4) #changer la taille de la courbe
      
#8 Pour voir la courbe density en entier, modifier les bornes de l axe des ordonnées de l histogramme avec l argument ylim

hist(x = velo$capacity, 
     main = "Distribution de \n la capacité des stations",
     col = "red",
     probability = TRUE,
     xlab = "Capacity",
     ylim = c(0,0.08)) #modifier les bornes de l axe des ordonnées

lines(density(velo$capacity),
      lty = 2,
      col = "blue",
      lwd = 2)

#Exercice 3

# Le boxplot permet aussi d analyser les variables quantitatives avec une vue univariée ou bivariée avec une variable qualitative. Il permet d apporter d autres informations telles que les quartiles et éventuelles valeurs atypiques.

#1 une boîte à moustache de la distribution des capacity

boxplot(x = velo$capacity, 
        main = "Boxplot de \n la capacité des stations")
        
#2 Construire le même graphique mais pivoter horizontalement

boxplot(x = velo$capacity, 
        main = "Boxplot de \n la capacité des stations",
        horizontal = TRUE)
        
#3 Construire le même graphique en le remettant à la verticale et en n affichant pas les valeurs atypiques.

boxplot(x = velo$capacity, 
        main = "Boxplot de \n la capacité des stations",
        horizontal = FALSE,
        outline = FALSE) # n affichant pas les valeurs atypiques.
        
#4 Ajouter un point supplémentaire qui correspond à la moyenne de la série avec la fonction points(). On souhaite que ce point soit un gros carré rouge
points(moy, col = "red", pch = 15, cex = 2)
# cex = spécifié la taille relative des textes
# pch = 

#La fonction par()

#5 comparer les vélos disponibles sur le 7ème et le 8ème arrondissement. Diviser la fenêtre graphique en deux puis constuire un boxplot pour ces deux arrondissement. 

par(mfrow=c(1,2)) #fenêtre sur 1 ligne et 2 colonnes

#(7ème)
df7 = subset(velo, CodePostal == "69007")
boxplot(x = df7$bikes, 
        main = "Boxplot nb vélos \n 69007",
        ylim = c(0,40))

#(8ème)
df8 = subset(df, CodePostal == "69008")
boxplot(x = df8$bikes, 
        main = "Boxplot nb vélos \n 69008",
        ylim = c(0,40))

#C est plus simple d analyser les deux graphiques si la borne des ordonnées est la même.
# On remarque que la disponibilité des stations est plus homogènes sur le 8ème.


#6 on souhaite analyser le nombre de vélos disponibles en fonction de la variable bonus

par(mfrow=c(1,1))  #fenêtre sur 1 ligne et 1 colonne

# Tracer le graphique boxplot
boxplot(formula = bikes ~ bonus,
        data = df, 
        main = "Dispo vélos vs Stations Bonus")

#7 Ajouter les moyennes de chaque groupes sur le graphique à l'aide de la fonction tapply() et points()

# Calculer les moyennes de chaque groupe
means <- tapply(X = df$bikes, 
                INDEX = df$bonus, 
                FUN = function(X) mean(X))
print(means)
# Ajouter les moyennes de chaque groupe au graphique
points(means, col = "red", pch = 19)

# Exercice 4 : DIAGRAMME

#Les fonctions barplot()et table().

#1 diagramme en barre de la répartition du nombre de station 
effectif = table(df$bonus)
barplot(height = effectif,
        main = "Répartition du nombre \n de station bonus")#mettre un titre

#2 Construire le même graphique mais pivoter horizontalement.
barplot(height = effectif,
        main = "Répartition du nombre \n de station bonus",
        horiz = TRUE)

#Les fonctions barplot(), prop.table()et legend().

#3 Construire le même graphique mais en pourcentage.
frequence = prop.table(effectif)
barplot(height = frequence,
        main = "Répartition en % du nombre \n de station bonus",
        horiz = TRUE)

#4  un diagramme bivarié avec la répartition du nombre de station bonus en fonction du nombre de station avec un terminal de paiement. Les deux variables ayant les mêmes modalités TRUE/ FALSE, il est important de définir le nom de l'axe des abscisses
effectif = table(df$banking, df$bonus)
print(effectif)
barplot(height = effectif,
        main = "Bonus vs Banking",
        xlab = "Station Bonus ?")
#On remarque qu'on ne sait pas distinguer les deux modalités car il n'y a pas de légende.


#5 Afficher une légende pour pouvoir distinguer les couleurs associées aux modalités avec vert pour TRUEet rouge pour FALSE. On peut vérifier si le graphique est cohérent en vérifiant avec l'objet frequence.

#Calcul des pourcentages
frequence = prop.table(x = effectif) #s pourcentages en utilisant la fonction prop.table() pour l'objet effectif.
barplot(height = frequence,
        main = "Bonus vs Banking",
        xlab = "Station Bonus ?", #spécifier l'étiquette de l'axe des abscisses (axe horizontal) sur le graphique. 
        col = c("red","green"))

#Préparer les labels
legend_labels <- colnames(frequence) # Stocke les noms des colonnes de l'objet frequence dans la variable legend_labels.
#Ajouter une légende
legend(x = "topright", 
       legend = legend_labels, 
       fill  = c("red","green")) #Ajoute une légende dans le coin supérieur droit du graphique avec les labels stockés dans legend_labels et utilise les couleurs rouge et verte.

#Afficher les fréquences pour vérifier le graphique
print(frequence)

#6 Même question mais en pourcentage colonne.

#prop.table() = Calculez les proportions d'un tableau de contingence.
#Calcul des pourcentages colonnes
frequence = prop.table(x = effectif, margin = 2) #calcule les proportions des valeurs dans effectif par colonne, ce qui est souvent utile pour calculer les proportions par rapport aux totaux par colonne dans une table de données.
barplot(height = frequence,
        main = "Bonus vs Banking",
        xlab = "Station Bonus ?",
        col = c("red","green"))

#Préparer les labels
legend_labels <- colnames(frequence)
#Ajouter une légende
legend(x = "topright", 
       legend = legend_labels, 
       fill  = c("red","green"))

#Afficher les fréquences pour vérifier le graphique
print(frequence)

#7 Même question mais avec un diagramme bivarié non empilé à l'aide de l'argument beside.

      #Calcul des pourcentages colonnes
frequence = prop.table(x = effectif, margin = 2)
barplot(height = frequence,
        main = "Bonus vs Banking",
        xlab = "Station Bonus ?",
        col = c("red","green"),
        beside = TRUE)#non empilé

      #Préparer les labels
legend_labels <- colnames(frequence)
    #Ajouter une légende
legend(x = "topright", 
       legend = legend_labels, 
       fill  = c("red","green"))

    #Afficher les fréquences pour vérifier le graphique
print(frequence)


#Les fonctions pie()et paste().

#8 Créer un diagramme circulaire de la répartition du nombre de station bonusà l'aide de la fonction pie()en différenciant les deux catégories avec la couleur jaune et vert.
pie(x = effectif, #diagramme circulaire pie
    main = "Répartition du nombre \n de station bonus",
    col = c("yellow","green"))

#9 Construire le même graphique à l'aide de l'argument labelset la fonction paste()afin d'ajouter les étiquettes de données avec les effectifs.
etiquette = paste(rownames(effectif),"\n",effectif)
pie(x = effectif,
    main = "Répartition du nombre \n de station bonus",
    col = c("yellow","green"),
    labels = etiquette)

#Les fonctions palette()et colors().

#10 Construire dans un diagramme en barre le top 10 des codes postaux avec le plus de station velo'v. On peut faire pivoter les étiquettes à l'aide de l'argument las pour une meilleure lecture du graphique. Utilisez la fonction palette()comme couleur pour les barres.
effectif = table(df$CodePostal)
top10 = sort(effectif, decreasing = TRUE)[1:10]
barplot(height = top10,
        main = "Top 10 sur le \n nombre de station",
        col = palette(),
        las = 2)  # Rotation des étiquettes à 90 degrés
#On remarque que les deux premières couleurs se répetent.
print(palette()) # la fonction `palette()` ne dispose que de 8 couleurs

#11 Même question mais avec la fonction colors(). Elle donne accès à plus de 650 couleurs.
barplot(height = top10,
        main = "Top 10 sur le \n nombre de station",
        col = colors(),
        las = 2)  # Rotation des étiquettes à 90 degrés

print(colors())

#12 Exporter ce graphique dans un format .PNG à l'aide de la fonction dev.print().

dev.print(device = png, file = "export.png", width = 600)

#Exercice 5 - Nuage de points

#La fonction plot() = Crée un nuage de points

#1 A l'aide de la fonction plot(), construisez un nuage de point pour étudier la corrélation entre le nombre de places disponibles sur les stations et leur capacité.
plot(x = df$stands, y = df$capacity,
     main = "Place disponible vs Capacité")

#2 Construisez le même graphique en zoomant avec des abscisses et ordonnées qui vont de 0 à 60 et avec des points avec un fond noir.
plot(x = df$stands, y = df$capacity,
     main = "Place disponible vs Capacité",
     xlim = c(0,60),
     ylim = c(0,60),
     pch=19)

#Les fonctions plot()et levels().

#3 Construire le même graphique en affichant deux couleurs différentes selon la colonne bornescréée précédemment. La colonne bornesdoit avoir le type factor.
df$bornes = as.factor(df$bornes)
plot(x = df$stands, y = df$capacity,
     main = "Place disponible vs Capacité",
     xlim = c(0,60),
     ylim = c(0,60),
     col = df$bornes,
     pch=19) # spécifie que des cercles remplis seront utilisés comme marqueurs 

# Ajouter une légende
legend("topright", legend = levels(df$bornes),
       col = palette(), pch = 19)

#4 Pour pouvoir choisir ses couleurs, il suffit d'utiliser un vecteur avec les couleurs qu'on souhaite.
myColors <- c("red", "blue", "green")  
# Ajoutez plus de couleurs si nécessaire avec le code HTML des couleurs à la place des noms

# Tracer le graphique
plot(x = df$stands, y = df$capacity,
     main = "Place disponible vs Capacité",
     xlim = c(0, 60),
     ylim = c(0, 60),
     col = myColors[df$bornes],
     pch = 19)

# Ajouter une légende
legend("topright", legend = levels(df$bornes),
       col = myColors, pch = 19)

#5 Ajouter un carré vert sur le graphique représentant la moyenne du nombre de places disponibles et la capacité des stations.
moy_stands = mean(df$stands)
moy_capacity = mean(df$capacity)
points(x = moy_stands,y = moy_capacity, 
       pch = 15,
       col = myColors[3],
       cex = 2) # cex est un paramètre utilisé pour contrôler la taille des symboles ou du texte dans les graphiques.

#Exercice 6 - Cartographie (spoil sur le SD2)

#Exécuter le code suivant pour créer une carte à partir des colonnes position_longitudeet position_latitude.
# Librairies nécessaires
library(leaflet)
library(dplyr)
library(ggplot2)

# Créer une carte Leaflet
maCarte <- leaflet(df) %>%  # crée une carte interactive en utilisant les données fournies dans le dataframe df
  addTiles() %>%  #tuiles de fond de carte sont ajoutées avec addTiles()
  addMarkers(~position_longitude, 
             ~position_latitude, 
             popup = ~address)

# Afficher la carte
maCarte
