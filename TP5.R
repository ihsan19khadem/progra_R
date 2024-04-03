setwd("L:/BUT/SD/Promo 2023/ikhadem/Progra_statistique/TP5/nba")
getwd()

library(tools)

#2 La commande list.files()  #Liste les fichiers et/ou répertoires dans un dossier spécifié.
fichiers <- list.files(path = getwd(),
                       pattern = ".csv",
                       full.names = TRUE)
print(fichiers)

#Lister tous les fichiers du dossier nba

fichiers <- list.files(path = getwd(),
                       pattern = ".csv$",
                       full.names = TRUE)


#3 Les commandes basename() et file_path_sans_ext()   
#Extrait le nom de fichier de chaque chemin dans un vecteur de chemins.

library(tools)
print(fichiers[1])
nom_fichier = basename(path = fichiers[1])
nom_fichier_sans_extension = file_path_sans_ext(x = nom_fichier)
print(nom_fichier_sans_extension)

#4 La commande assign() = Affecte une valeur à un objet spécifié par un nom.

# Lire le fichier CSV et l'affecter à une variable avec le nom du fichier
assign(x = nom_fichier_sans_extension, 
       value = read.csv(fichiers[1],
                        sep = ",",
                        dec = "."))
#un dataframe vient d'être créé avec comme nom d'objet le nom du fichier sans extension.

#5 Utiliser ce même procéder dans une boucle for pour importer toutes la liste de fichiers.


# Boucle pour lire chaque fichier CSV
for (fichier in fichiers) {
  # Extraire le nom du fichier sans extension
  nom_objet <- file_path_sans_ext(basename(fichier))
  
  # Lire le fichier CSV et l'affecter à une variable avec le nom du fichier
  start_time <- Sys.time()
  assign(nom_objet, read.csv(fichier, 
                             sep = ",",
                             dec = "."))
  end_time <- Sys.time()
  # Calcul du temps écoulé
  execution_time <- end_time - start_time
  cat("Importation : ",nom_objet, "=" , execution_time , "\n")
}


#Exercice 2 : Les jointures

#1 Combien de match se sont dérouler à Los Angeles depuis la création de la NBA ?

df_x = subset(team, city == "Los Angeles", select = c("id", "city"))
df_y = subset(game, select = c("game_id", "team_id_home"))
dfJoin = merge(x = df_x, y = df_y, 
               by.x = "id", 
               by.y = "team_id_home", 
               all.x = TRUE)
nrow(dfJoin)
View(dfJoin)

# subset = donne l'heure actuelle du systeme ex: pc

#2 Quelle est l'affluence moyenne de spectacteur durant ces matchs joués à Los Angeles.

df_x = dfJoin
df_y = subset(game_info, select = c("game_id", "attendance"))
dfJoin = merge(x = df_x, y = df_y, 
               by = "game_id",
               all.x = TRUE)
mean(dfJoin$attendance, na.rm = TRUE)
View(dfJoin)

#3 Combien d'arbitres différents ont officié durant la saison 2020.

df_x = subset(game_summary, season == 2020,
              select = c("game_id", "season"))
dfJoin = merge(x = df_x, y = officials, 
               by = "game_id",
               all.x = TRUE)
length(unique(dfJoin$official_id))
View(dfJoin)

#4 Combien de matchs à officié Dick Bavetta par saison ?

df_x = subset(game_summary,
              select = c("game_id", "season"))
df_y = subset(officials, first_name == "Dick" & last_name == "Bavetta")
dfJoin = merge(x = df_x, y = df_y, 
               by = "game_id",
               all.y = TRUE)
View(dfJoin)
table(dfJoin$season)

#Exercice 3 - GOAT : Connexion à une database SQLite

#Connexion avec R

#1 Installer le package DBIet RSQLite. Puis créer une connexion vers votre fichier nba.sqliteà l'aide de la fonction dbConnect().
library(DBI)
library(RSQLite)
mydb <- dbConnect(SQLite(), "nbaDb.sqlite")

#2 Lister les tables de la base de données SQLite à l'aide de la fonction dbListTables().
dbListTables(mydb)

#3 A l'aide de la fonction dbGetQuery()sélectionnez les 5 premières lignes du tableau team.
dbGetQuery(mydb, 'SELECT * FROM team LIMIT 5')

#4 Refaire une des jointures de l'exercice 2 à l'aide de la fonction dbGetQuery().
dfJoin = dbGetQuery(mydb, '....')

#5 Stocker la table issue de la jointure précédente dans une nouvelle table de la base de données SQLlite à l'aide de la fonction dbWriteTable(). Vérifiez que la table a été correctement créée.
dbWriteTable(mydb, "nom_table", dfJoin)
dbListTables(mydb)

#6 Fermer la connexion avec la base de données SQLite.
dbDisconnect(mydb)






























































