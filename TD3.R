#Exercice 1-exportation d'un fichier excel 
install.packages("readxl")
library(readxl)
setwd("C:/Users/admin/Documents/IUT LUMIERE/Programmation/progra_stat/POKEMON") #faire CTRL+SHIFT+H
pokemon=read_excel(path="pokemon.xlsx",sheet="pokemon")

#2

dim(pokemon) # nb de lignes et de colonnes
ncol(pokemon) #nb colonne
nrow(pokemon) #nb ligne

#3

summary(pokemon) # résumé des données

#4
# Modification du type de ces variables pour les transformateurs en type factor
pokemon$is_legendary <-as.factor(pokemon$is_legendary)
pokemon$generation <-as.factor(pokemon$generation)
pokemon$type <-as.factor(pokemon$type)

#5
summary(pokemon)


#Exercice 2

#La fonction ifelse() : Retourne des valeurs en fonction d'une condition.

#1 
#Créer une colonne attack_groupavec la valeur Attack+ si la valeur d' attackest supérieure ou égale à la médiane, 
#sinon Attack- . Convertir cette variable en factorpuis effectuer un résumé de cette colonne avec la fonction
med = median(pokemon$attack)
pokemon$attack_group = ifelse(pokemon$attack >= med, "attack+","attack-")
pokemon$attack_group <-as.factor(pokemon$attack_group)
summary(pokemon$attack_group)

summary(pokemon)

#2
#Créer une colonne water_fireavec la valeur yes si le type est eau ou feu , sinon renseigner la valeur no 
pokemon$water_fire = ifelse(pokemon$type %in% c("water","fire"), "yes","no")
pokemon$water_fire <-as.factor(pokemon$water_fire)
summary(pokemon$water_fire)

summary(pokemon)

#3
#Créer une colonne bestavec la valeur yes si la valeur de attackfait partie du troisième quartile et 
#si la valeur de defensefait partie du troisième quartile et si la valeur de speedfait partie du troisième quartile, sinon renseigner la valeur no
q3_attack = quantile(pokemon$attack, probs = 0.75)
q3_defense = quantile(pokemon$defense, probs = 0.75)
q3_speed = quantile(pokemon$speed, probs = 0.75)
pokemon$best = ifelse(pokemon$attack > q3_attack &
                        pokemon$defense > q3_defense &
                        pokemon$speed > q3_speed , "yes","no")
pokemon$best <-as.factor(pokemon$best)
summary(pokemon$best)
View(pokemon)

#La fonction is.na() : Vérifiez si les valeurs sont manquantes (NA).

#1
# Filtrer les données dans un objet nommé requete avec les pokemons ayant des valeurs manquantes sur la colonne weight_kg
requete = subset(pokemon, is.na(weight_kg))
View(requete)

#2
#Filtrer les données dans un objet nommé requeteavec les pokemons n'ayant pas des valeurs manquantes sur la colonne
requete = subset(pokemon, !is.na(weight_kg))
View(requete)

#3
#Créer des nouvelles variables nommées weight_kgNaet height_mNAavec les mêmes valeurs pour les valeurs déjà renseignées mais en remplaçant 
#les valeurs manquantes NApar leurs valeurs médianes
med_weight_kg = median(pokemon$weight_kg, na.rm = TRUE)
pokemon$weight_kgNa = ifelse(is.na(pokemon$weight_kg) , 
                             med_weight_kg ,
                             pokemon$weight_kg)

med_height_m = median(pokemon$height_m, na.rm = TRUE)
pokemon$height_mNA = ifelse(is.na(pokemon$height_m) , 
                            med_height_m ,
                            pokemon$height_m)

# Les fonctions cut() : Diviser les valeurs numériques en intervalles.

#1
#Créer une nouvelle variable nommée weight_groupen regroupant en 3 tranches avec les labels léger / moyen / lourd .
pokemon$weight_group = cut(pokemon$weight_kg,#Cette ligne crée une nouvelle variable appelée weight_group dans le jeu de données pokemon. Cette nouvelle variable sera utilisée pour stocker les catégories de poids.
                           breaks = 3, # L'argument breaks spécifie le nombre de points de rupture pour diviser les données en intervalles. Dans ce cas, il y aura trois points de rupture, ce qui signifie que les données seront divisées en trois groupes de poids.
                           labels = c("léger","moyen","lourd"))
View(pokemon)

#2
#une nouvelle variable nommée height_m_groupen regroupant en 4 tranches telles que : ]0,1] / ]1,2] / ]2,3] / ]3,max]
pokemon$height_m_group = cut(pokemon$height_m,#nouvelle variable appelée height_m_group  Cette nouvelle variable sera utilisée pour stocker les catégories de taille des Pokémon.
                             breaks = c(0,1,2,3,
                                        max(pokemon$height_m,
                                            na.rm = TRUE)))

#3
#une nouvelle variable nommée defense_groupen regroupant en 5 tranches avec les min, max et quartiles telle que : [min,Q1] / (Q1,Q2] / (Q2,Q3] / (Q3,max] . Calculer un résumé de la nouvelle colonne.
pokemon$defense_group = cut(pokemon$defense,
                            breaks = quantile(pokemon$defense,
                                              na.rm = TRUE),
                            include.lowest = TRUE)#include.lowest = TRUE: Indique que le premier intervalle doit inclure la valeur la plus basse rencontrée dans la colonne defense
summary(pokemon$defense_group)


#La fonction aggregate() : Effectuer une opération d'agrégation sur les données groupées.

#1
#Calculer la moyenne d' attack par type
aggregate(x = attack ~ type, 
          data = pokemon,
          FUN = function(x) mean(x))

#2
#Calculer la médiane d' attack par generation et type
aggregate(x = attack ~ generation + type,
          data = pokemon, 
          FUN = function(x) median(x))
#3
#alculer l'effectif par type
aggregate(x = pokedex_number ~ type,
          data = pokemon,
          FUN = function(x) length(x))
#4
#Calculer la moyenne et la médiane de la statistique speedpour chaque generationet type. Afficher également les effectifs de chaque paire
aggregate(speed ~ generation + type,
          data = pokemon, 
          FUN = function(x) c(moy = mean(x),
                              med = median(x),
                              eff = length(x) ) )
