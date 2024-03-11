#afficher la classe d un jeu de donnée :
rm() #delete
iris
class(iris)
View(iris)
nrow(iris)
ncol(iris)
colnames(iris) #afficher le noms des colonnes
summary(iris)
iris[,c("Sepal.Length","Species")] #afficher que certaines colonnes
iris[c(100,103,105),] #afficher que certaines lignes
iris[c(50:100),] #afficher les lignes de x à y
median(iris$Sepal.Length)
mean(iris$Sepal.Length)
sd(iris$Sepal.Length) #calcul de l ecart type
quantile(iris$Sepal.Length, probs =seq(from=0.1, to=0.9, by=0.1))#calcul de deciles


#OUVERTURE FICHIER CSV :

dfmanga= read.csv("C:/Users/isaaq/Downloads/manga.csv", header = TRUE, sep = ",", dec = ".")
dfanime= read.csv("C:/Users/isaaq/Downloads/anime.csv", header = TRUE, sep = ",", dec = ".")

dim(dfmanga)
dim(dfanime)
mean(dfmanga$Score)
mean(dfanime$Score)
sum(dfmanga$Vote)
sum(dfanime$Vote)
sd(dfmanga$Score)
sd(dfanime$Score)
quantile(dfmanga$Score, probs =seq(from=0.1, to=0.9, by=0.1))
quantile(dfanime$Score, probs =seq(from=0.1, to=0.9, by=0.1))



#extraction
extraction1=subset(dfmanga,Score> 9)
nrow(extraction1)
extraction2=subset(dfmanga,Vote >= 200000)
nrow(extraction2)
extraction3=subset(dfmanga,Vote >= 200000 & Score> 8 )
nrow(extraction3)
extraction4=subset(dfmanga, Score >= 7 & Score <= 8)
nrow(extraction4)


#Calculer les effectifs de RatinG. Combien de modalité de la variable existe-t-il ?
#Calculer ensuite les effectifs en pourcentage.
effectifrating = table(dfanime$Rating) 
print(effectifrating)
length(effectifrating)
prop.table(effectifrating)


#cmb anime rating == truc
extraction5=subset(dfanime,Rating=="R - 17+ (violence & profanity)")
nrow(extraction5)
extraction6=subset(dfanime,Rating=="R - 17+ (violence & profanity)"& Score> 8)
nrow(extraction6)
extraction7=subset(dfanime,Rating!="R - 17+ (violence & profanity)")
nrow(extraction7)

#Cmb anime ont un score sup 9 OU vote sup truc:
extraction8=subset(dfanime,Score >= 9 |Vote > 400000)
nrow(extraction7)





#MODIFIER UN DATAFRAM
dfanime= dfanime[  ,c("Title","Score","Vote","Ranked")]
dfmanga=dfmanga[,c("Title","Score","Vote","Ranked")]

#Creer une colonne dans un DATAFRAM
dfanime$type="anime"
dfmanga$type="manga"

#compiler deux DATAFRAM
dfconcat=rbind(dfmanga,dfanime)


#exporter un fichier csv
write.table(x=dfconcat,file="Bureau/SD/R/exportTp1.csv",
            sep=",",
            row.names=FALSE)
rm(list = ls())

