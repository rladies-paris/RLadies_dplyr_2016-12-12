
## Charger les données ====

library(readr)
jardins <- read_csv2("http://opendata.paris.fr/explore/dataset/parcsetjardinsparis2010/download/?format=csv&timezone=Europe/Berlin&use_labels_for_header=true")

## Vérifier l'import ====

summary(jardins)
head(jardins)

# et à propos de la classe ?

class(jardins)

# attention ! 
iris_tbl<-as.tbl(iris)
class(iris)# [1] "data.frame"
class(iris_tbl)  #[1] "tbl_df" "tbl" "data.frame " 
class(iris[,1]) # "numeric"
class(iris[,1,drop=FALSE]) # "data.frame"
class(iris_tbl[,1])# "tbl_df" "data.frame"

## Changer les noms de variables ====

names(jardins) <- make.names(names(jardins))
names(jardins)[10] <- "SURFACE.ADMINISTRATIVE_m2" #



## Chargement du package ====

library(dplyr)

# à noter que au chargement de dplyr : 

# Attachement du package : ‘dplyr’
# 
# The following objects are masked from ‘package:stats’:
#   
#   filter, lag
# 
# The following objects are masked from ‘package:base’:
#   
#   intersect, setdiff, setequal, union


## Supprimons les 2 premières variables avec select() ====

# Les variables "Geo.point" et "Geo.Shape",  
# qui permettent de localiser les jardins,
# ne seront pas utilisées...

jardins <- select(jardins, -Geo.point, -Geo.Shape)

## Expérimentons les fonctions de sélections dans dplyr ====

head(select(jardins, starts_with("annee", ignore.case=TRUE)),3) # 3 pour 3 entrées à voir dans la console
head(select(jardins, ends_with("MENT")), 3)
head(select(jardins, -contains(".")), 3)

# Si elles manquent de flexibilité, il est toujours possible d'utiliser les expressions régulières :
head(select(jardins, matches("._.")))
select(jardins, NOM_EV:ARRONDISSEMENT)


## Filtrer les parcs et jardins qui ont ouvert après l'année 2000 ====

filter(jardins, ANNEE.OUVERTURE > 2000)


## Filtrer les parcs et jardins pour qui l'année de rénovation est "0" ====
# ainsi que l'année de changement ====

# ces 2 écritures sont identiques :
filter(jardins, ANNEE.RENOVATION == 0 & ANNEE.CHANGEMENT == 0)
filter(jardins, ANNEE.RENOVATION == 0 , ANNEE.CHANGEMENT == 0)

## Filtrer les parcs et jardins pour qui l'année de rénovation est "0" OU l'année de changement est "0" ====

# le symbole | s'utilise comme en base :
filter(jardins, ANNEE.RENOVATION ==0 | ANNEE.CHANGEMENT ==0)
filter(jardins, ANNEE.OUVERTURE > 2000 | ARRONDISSEMENT == 75010)
filter(jardins, ANNEE.OUVERTURE > 2000 & ARRONDISSEMENT == 75010)

## Trier les parcs et jardins par ordre croissant d'arrondissement ====

arrange(jardins, ARRONDISSEMENT)

## Trier les parcs et jardins par ordre décroissant d'arrondissement ====

arrange(jardins, desc(ARRONDISSEMENT))
select(arrange(jardins, desc(ARRONDISSEMENT)),ARRONDISSEMENT, NOM_EV)

## Trier les parcs et jardins par ordre décroissant d'arrondissement et par ordre croisant d'année d'ouverture ====

arrange(jardins, desc(ARRONDISSEMENT), ANNEE.OUVERTURE) #
arrange(jardins, desc(ARRONDISSEMENT), desc(ANNEE.OUVERTURE)) #


## Transformons la variable "SURFACE.ADMINISTRATIVE_m2" ====

head(jardins$SURFACE.ADMINISTRATIVE_m2)

jardins <- mutate(jardins, 
                  SURFACE.ADMINISTRATIVE_m2= as.numeric(as.character(SURFACE.ADMINISTRATIVE_m2)))

summary(jardins$SURFACE.ADMINISTRATIVE_m2) 


## Résumons la variable de surface administrative par sa moyenne ====

summarise(jardins, surface_moyenne = mean(SURFACE.ADMINISTRATIVE_m2))


## Calculons moyenne et écart-type pour cette variable ====

summarise(jardins, 
          surface_moyenne = mean(SURFACE.ADMINISTRATIVE_m2),
          ecart_type = sd(SURFACE.ADMINISTRATIVE_m2)) #


## Remplacer les zéros des variables "ANNEE.*", "NUMERO", "SURFACE.ADMINISTRATIVE_m2" ====
# et "ARRONDISSEMENT" par des NA ====

# construire une fonction...
remplacer_les_zeros <- function(vecteur){vecteur[vecteur==0] <- NA ; return(vecteur)}

#...pour l'appliquer à toutes ces variables : 
mutate(jardins, 
       ANNEE.OUVERTURE = remplacer_les_zeros(ANNEE.OUVERTURE),
       ANNEE.RENOVATION = remplacer_les_zeros(ANNEE.RENOVATION),
       ANNEE.CHANGEMENT = remplacer_les_zeros(ANNEE.CHANGEMENT), 
       NUMERO = remplacer_les_zeros(NUMERO),
       SURFACE.ADMINISTRATIVE_m2 = remplacer_les_zeros(SURFACE.ADMINISTRATIVE_m2), 
       ARRONDISSEMENT = remplacer_les_zeros(ARRONDISSEMENT))


# ou utiliser mutate_each() !
jardins <- mutate_each( jardins, funs(remplacer_les_zeros), 
                       contains("ANNEE"),
                       NUMERO,
                       SURFACE.ADMINISTRATIVE_m2, 
                       ARRONDISSEMENT)



## Calculer la médiane et la moyenne de toutes les variables d'année ====

summarise_each( jardins, funs(median,mean), ANNEE.OUVERTURE:ANNEE.CHANGEMENT)

## Calculer la médiane et la moyenne de toutes les variables d'année, sans les données manquantes ! ====

summarise_each(jardins, 
                funs(median(.,na.rm=TRUE),mean(.,na.rm=TRUE)),
                ANNEE.OUVERTURE:ANNEE.CHANGEMENT)


## En utilisant %>%, filter les 10ème, 11ème et 12ème arrondissement, ====
## faire le total de la surface des parcs et jardins,
## créer 2 variables de surface : une en ha et l'autre en km²


jardins %>% 
  filter(ARRONDISSEMENT %in% c(75010, 75011, 75012)) %>%
  summarise(surface_totale = sum(SURFACE.ADMINISTRATIVE_m2, na.rm = TRUE)) %>% 
  mutate(surface_totale_ha = surface_totale/10000,
         surface_totale_km2= surface_totale_ha/100) 

# idée : se faciliter la vie et la lecture avec des retours à la ligne après chaque %>%

jardins %>% 
  #filter(ARRONDISSEMENT %in% c(75010, 75011, 75012)) %>%
  summarise(surface_totale = sum(SURFACE.ADMINISTRATIVE_m2, na.rm = TRUE )) %>% 
  mutate(surface_totale_ha = surface_totale/10000,
         surface_totale_km2=round(surface_totale_ha)) 
  


## Calculer par arrondissement pour chaque parc/jardin ====
# la proportion de surface occupée par rapport à l'ensemble des espaces verts ====

jardins %>%
  filter(!is.na(SURFACE.ADMINISTRATIVE_m2)) %>% 
  group_by(ARRONDISSEMENT) %>% 
  mutate(prop_arrondissement = SURFACE.ADMINISTRATIVE_m2/sum(SURFACE.ADMINISTRATIVE_m2)) %>% 
  select(NOM_EV,prop_arrondissement)



## Résumer, par arrondissement, la somme des surfaces totales d'espaces verts ====

jardins %>%
  filter(!is.na(SURFACE.ADMINISTRATIVE_m2)) %>% 
  group_by(ARRONDISSEMENT) %>% 
  summarise(surface_admin = sum(SURFACE.ADMINISTRATIVE_m2))

jardins %>% head()
head(jardins)

# en faire un plot ?

library(ggplot2)

jardins %>%
  filter(!is.na(SURFACE.ADMINISTRATIVE_m2),ARRONDISSEMENT != 94300, 
         !is.na(ARRONDISSEMENT)) %>% 
  group_by(ARRONDISSEMENT) %>% 
  summarise(surface_admin = sum(SURFACE.ADMINISTRATIVE_m2)) %>% 
  ggplot(aes(x = as.factor(ARRONDISSEMENT), y = surface_admin)) + geom_bar(stat = "identity")







