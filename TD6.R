#Exercice 1  Importer les données

setwd("L:/BUT/SD/Promo 2023/ikhadem/Progra_statistique/TD6")
getwd()

df <- read.csv(file = "nba2014_2015.csv", 
               sep = ",", #mettre la , al la place de ; (ouvrir blocnote)
               header = TRUE, #mettre true
               dec = ";")
View(df)



nrow(df) #fallait mettre df et pas nba
ncol(df) #fallait mettre df et pas nba
colnames(df)  # fallait rajouter un s a colname
Period <- as.factor(df$Period) # enlever le df$ au debut
PTSTYPE <- as.factor(df$PTSTYPE) #remplacer les signes tjrs mettre <- et enlever le df$ au debut
SHOOTER <- as.factor(df$shooter) #remplacer les signes tjrs mettre <- et enlever le df$ au debut


#EXERCICE 2 Statistiques descriptives

length(df$Period) #inverser th de length
length(df$PTSTYPE) #inverser th de length
length(df$SHOOTER) #inverser th de length
summary(df) #enlever un d ds ddf
sd(df$SHOT_DIST) # df en minuscule et fermer le ()
sd(df$SHOT_CLOCK) # enlever les[] et mettre les ()
     
     #combien de tirs manqués/réussis
     table(df$SHOT_RESULT) #enlever le s a RESULT et la ,et [] et mettre $
     
     #les quartiles
     quantile(df$SHOT_CLOCK, probs = seq(0.25,0.5,0.75), na.rm = TRUE) #ajouter seq et na.rm
    
      #les déciles
     quantile(df$CLOSE_DIST, probs = seq(0.1,0.3), na.rm = TRUE) #ajouter seq et na.rm et enlever s a quantile
    
 #nombre de matches différents
liste_game <- unique(df$GAME_ID) #enlever une ()
length(liste_game) #mettre un tiret a liste game

#nombre de joueurs différents
SHOOTER <- as.factor(df$SHOOTER) #mettre . apres as et enlever df$
levels(df$SHOOTER) #enlever n et mettre un s a level

# BLOC       
#conversion de la variable SHOT_DIST en mètre pour que les européens comprennent nos chiffres
df$SHOT_DIST_METRE<- df$SHOT_DIST * 0.30 # mette df a la place nba ,enlever les == et mettre df$
       
#nombre de points qu'a rapporté la tentative (0,2 ou 3)  
df$PTS_MARQUES <- ifelse(df$SHOT_RESULT == "made", yes = df$PTS_TYPE, 0)
       
#On supprime la variable GAME_RESULT car elle n'est pas utile
df$GAME_RESULT <- NULL
       
#création d'un objet sans la première colonne GAME_ID
df2 <- df[ , -1 ]


#Exercice 3 Extractions a faire 

#Les 100 tirs réussis ou manqués les plus loin
rang <- order(df$SHOT_DIST, decreasing = FALSE)
df3 <- df[, rang]
df3 <- df[ 1 : 100 , ]

#Les 100 tirs réussis les plus loin
df4 <- subset(df3, SHOT_RESULT = made) #changer symbole
df4 <- df[ 1 : 100 ; ]

#Combien de tirs à 3 points a réussi Kobe Bryant ?
df_kobe = subset(df,SHOT_RESULT = made &
                   PTS_TYPE = 3 & 
                   SHOOTER = "Kobe BRYANT")

dim(df_kobe)

#Le TOP5 des joueurs qui ont marqués le plus de points dans la saison
df_total <- aggregate(PTS_MARQUES ~ SHOOTER, data = df, FUN = sum)
df_total_tri <- df_total[-order(df_total$PTS_MARQUES)]
df_top5 <-  df_total_tri[  5  ,  ]


#Exercice 4 Automatisation a faire 

#Des graphiques adaptés selon le type de variable

#construction de la fonction
build_graph <- function(une_colonne, nom_colonne) {
  if(is.numeric(une_colonne)) {
    print(boxplot(une_colonne, main = nom_colonne))
  }
  else if (as.factor(une_colonne)) {
    tri <- table(une_colonne)
    print(barplot(tri, main = nom_colonne))
  }
  
  #on déroule la fonction sur chaque colonne du data frame.
  
  for (colonne in colnames(df) {
    build_graph(une_colonne = df[colonne , ] , nom_colonne = colone)
  }
}




