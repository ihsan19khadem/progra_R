# rnorm() = 	Génère des échantillons aléatoires suivant une distribution normale N(μ,σ) 
#qnorm() = Calcule les quantiles de la distribution normale spécifiée.
#pnorm() = Calcule la fonction de distribution cumulative (CDF) pour la distribution normale spécifiée.


# Exercice 1 - Comprendre la loi normale

#1 Créer une graphique vide avec comme borne d'abscisse [-5,5] et d'ordonnées [0,1].

# Créer une toile de fond vide pour le graphique
par(mfrow = c(1,1))
plot(NA, xlim=c(-5, 5), ylim=c(0, 1), xlab="X", 
     ylab="Densité de probabilité", 
     main="Densités de probabilité \n de lois normales")


#2 Programmer une boucle for qui à chaque itération, ajoute une courbe densité issue d'une des 4 combinaisons de paramètres de loi normale suivant 

# Tracer la densité de probabilité pour chaque simulation
moyennes <- c(0, 0, 0, -2)
sigmas <- c(0.45, 1, 2.25, 0.7)
colors <- c("red", "blue", "green", "grey")
legend_labels <- c()
for (i in 1:length(moyennes)) {
  serie = rnorm(n = 1000, 
                mean = moyennes[i], 
                sd = sigmas[i]) # ecart type
  lines(density(serie), col = colors[i])
  legend_labels <- c(legend_labels, paste("m =", moyennes[i], ",", "s =", sigmas[i]))
}

# Ajouter une légende
legend("topright", legend=legend_labels, col=colors, lwd=2, cex=0.8)

#3 Simuler une loi normale N(μ=0, σ=1) de taille 10 000.

serie = rnorm(n = 1e4, mean = 0, sd = 1)

#4 Contruire l'histograme de la distribution de la série avec sa courbe densité.

hist(serie, main = "loi normal centrée-réduite",
     probability = TRUE)
lines(density(serie))

#5 Calculer la médiane de la série.

median(serie)

#6 Calculer les quartiles de la série.

quantile(serie)

#7 Calculer les centiles de la série. Quelle valeur de la série correspond au centile 0.95 ?

quantile(serie, 
         probs = seq(from = 0, 
                     to = 1, by = 0.01))

quantile(serie, 
         probs = 0.95)
# environ 1,64                  

#Les commandes pnorm() et qnorm().

#8 Calculer la valeur théorique à l'aide de la fonction qnorm()

qnorm(p = 0.95, mean = 0, sd = 1)
pnorm(q = 1.644854, mean = 0, sd = 1)

#9 Quelle est la valeur théorique pour P(X < x) = 0.975.

qnorm(p = 0.975, mean = 0, sd = 1)

#10 Quelle est la probabilité théorique pour P(X >= 1.96) = p.

pnorm(q = 1.96, mean = 0, sd = 1)


# Exercice 2 - Construire la table de loi normale

#1 Construire une vecteur avec les probabilités de la première colonne de la table de loi normale. On souhaite une précision avec uniquement 4 décimales.

indices_lignes = seq(from = 0, to = 3.9, by = 0.1)

#on crée un vecteur vide pour ajouter les probas au fur et à mesure
all_probas = c()
#On parcourt les indices lignes
for (i in indices_lignes){
  proba = pnorm(q = i, mean = 0, sd = 1)
  #on ajoute la nouvelle proba au vecteur existant
  all_probas = c(all_probas,proba)
  all_probas = round(all_probas,digits = 4)
}

#2 Modifier ce code pour construire la table de loi normale.

indices_colones = seq(from = 0.00, to = 0.09, by = 0.01)
indices_lignes = seq(from = 0, to = 3.9, by = 0.1)

#On crée un objet résultat vide.
resultat = NULL
#On parcourt les indices colonnes
for (j in indices_colones) {
  #on crée un vecteur vide pour ajouter les probas au fur et à mesure
  all_probas = c()
  #On parcourt les indices lignes
  for (i in indices_lignes){
    quantile = i + j
    proba = pnorm(q = quantile, mean = 0, sd = 1)
    #on ajoute la nouvelle proba au vecteur existant
    all_probas = c(all_probas,proba)
    all_probas = round(all_probas,digits = 4)
  }
  #On ajoute une colonne au resultat
  resultat = cbind(resultat,all_probas)
}


#3 Modifier le nom des lignes et colonnes.

class(resultat)
table = data.frame(resultat)
colnames(table) = indices_colones
rownames(table) = indices_lignes
View(table)


#Exercice 4 - Simulation d'échantillon

#La commande sample()

#1 Tirez un échantillon de taille 100 dans la population initiale, à l'aide de la fonction sample. Quelle est la taille moyenne dans l'échantillon ? Quelle est l'écart-type dans l'échantillon ? Ces deux valeurs sont-elles proches de celles de la population ?

taille_ech<-100
echantillon<-sample(x = population, 
                    size = taille_ech, 
                    replace = TRUE)
mean(echantillon)
sd(echantillon)

#2 A partir de l'écart-type estimé, calculez la largeur du demi-intervalle de confiance, puis les bornes inférieures et supérieures de l'intervalle de confiance (toujours à 95%)

largeur<-qnorm(p = 0.975,mean=0,sd=1)*sd_pop/sqrt(taille_ech)
borne_inf<-moyenne_pop-largeur
borne_sup <-moyenne_pop+largeur

#Les commandes sample() = Sélectionne un échantillon aléatoire à partir d'un vecteur.et 
#apply() = Appliquer une fonction sur les marges d'un tableau (matrice ou data frame).

#3 A l'aide de la fonction replicate(), tirez 1000 échantillons de taille 100. Stockez dans un dataframe la moyenne et l'écart-type de chaque échantillon à l'aide de la fonction apply().
taille_ech<-100
nb_replicat<-1000
echantillons<-replicate(n = nb_replicat,
                        expr =  sample(population,
                                       taille_ech, 
                                       replace = TRUE))

moyennes<-apply(X = echantillons,
                MARGIN = 2,
                FUN = function(x) mean(x))
ecart_types<-apply(echantillons,
                   MARGIN = 2,
                   FUN = function(x) sd(x))
#4 Tracer l'histogramme des moyennes des échantillons. Retrouver-t-on une forme connue ?
hist(moyennes)

#5 Calculez la moyenne des moyennes des échantillons, ainsi que l'écart-type des moyennes des échantillons.
mean(moyennes)
sd(moyennes) #ecart type 

#6 Combien d'échantillons ont une moyenne supérieure à 172,8cm ? Quel est le nombre théorique ?
#observé
moy172 = moyennes[moyennes > 172]
length(moy172)
length(moy172) / length(moyennes)

#en théorie
#proba de P( X < 172cm)
proba_inf_172 = pnorm(q = 172, 
                      mean=moyenne_pop, 
                      sd=sd_pop/sqrt(taille_ech))
#proba de P( X >= 172cm)
1 - proba_inf_172

#7 Pour chaque échantillon, calculez la largeur du demi-intervalle de confiance en utilisant l'estimation de l'écart-type calculé pour chaque échantillon, puis calculez les bornes inférieures et supérieures des intervalles de confiance (variables à rajouter dans votre dataframe).
largeur<-apply(X = echantillons,
               MARGIN = 2,
               FUN = function(x) pnorm(0.975)*sd(x)/taille_ech)

borne_inf_IC<-moyennes-largeur
borne_sup_IC<-moyennes+largeur

#8 Construisez un dataframe avec ces 3 vecteurs.
resultat = data.frame(largeur,borne_inf_IC,borne_sup_IC)
View(resultat)

#Exercice 5 - L'effet de la taille de l'échantillon

# runif()	= Génère des échantillons aléatoires suivant une distribution uniforme.

#1 Créer une fonction moyenne_echantillon()qui prend en entrée le vecteur Vvariable d'une population et une taille nd'échantillon et qui donne en sortie la moyenne d'un échantillon aléatoire de taille ntiré dans la population tels que : moyenne_echantillon <- function(V, n).
moyenne_echantillon<-function(V,n) {
  return(mean(sample(x = V,size = n, replace=TRUE)))
}

#2 A l'aide de la fonction replicate, tirer 1000 échantillons pour chacun des tailles d'échantillons suivantes : 20, 30, 50, 100, 500, 1000 toujours à partir de la population initiale de 10.000.000 d'individus. .
moyennes_20<-replicate(n = nb_replicat, 
                       expr = moyenne_echantillon(V = population,
                                                  n = 20))
moyennes_30<-replicate(n = nb_replicat, 
                       expr = moyenne_echantillon(V = population,
                                                  n = 30))
moyennes_50<-replicate(n = nb_replicat, 
                       expr = moyenne_echantillon(V = population,
                                                  n = 50))
moyennes_100<-replicate(n = nb_replicat, 
                        expr = moyenne_echantillon(V = population,
                                                   n = 100))
moyennes_500<-replicate(n = nb_replicat, 
                        expr = moyenne_echantillon(V = population,
                                                   n = 500))

#3 Représentez les histogrammes des moyennes pour chaque taille d'échantillon en gardant les mêmes échelles des axes des abcisses et ordonnées.
par(mfrow=c(2,3))
hist(moyennes_20, xlim=c(161,181), main="20")
hist(moyennes_30, xlim=c(161,181), main="30")
hist(moyennes_50, xlim=c(161,181), main="50")
hist(moyennes_100, xlim=c(161,181), main="100")
hist(moyennes_500, xlim=c(161,181), main="500")


#4 Reprendre les 3 questions précédentes avec une nouvelle population de 10.000.000 individus tirés à partir d'une loi uniforme sur [0,1].
population<-runif(n = 1e7, min = 0, max = 1)
moyennes_20<-replicate(nb_replicat, moyenne_echantillon(population,20))
moyennes_30<-replicate(nb_replicat, moyenne_echantillon(population,30))
moyennes_50<-replicate(nb_replicat, moyenne_echantillon(population,50))
moyennes_100<-replicate(nb_replicat, moyenne_echantillon(population,100))
moyennes_500<-replicate(nb_replicat, moyenne_echantillon(population,500))
par(mfrow=c(2,3))
hist(moyennes_20, xlim=c(0,1), main="20")
hist(moyennes_30, xlim=c(0,1), main="30")
hist(moyennes_50, xlim=c(0,1), main="50")
hist(moyennes_100, xlim=c(0,1), main="100")
hist(moyennes_500, xlim=c(0,1), main="500")











