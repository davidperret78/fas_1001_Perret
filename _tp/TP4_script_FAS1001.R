install.packages("rvest")
library("rvest")
library("dplyr")
url <-  "https://fr.wikipedia.org/wiki/Indice_de_perception_de_la_corruption"
page <-  read_html(url)
tableaux <- page %>% html_nodes("table")
tableau_corruption <- tableaux %>% .[1] %>% html_table(fill = T)
print(tableau_corruption)
tableau_corruption <-  tableau_corruption[[1]]
tableau_corruption <- tableau_corruption %>% rename(Pays = `Pays ou territoire`)
Europe <- c("France", " Finlande", "Danemark", "Suisse", "Allemagne", "Royaume-Uni","Suède","Autriche", "Belgique","Islande","Lituanie","Pologne")
Amérique_Nord <- c("États-Unis", "Canada", "Mexique")
Amérique_Latine <-  c("Brésil","Argentine","Colombie","Uruguay","Chili","Paraguay","Honduras","Équateur","Guatemala")
Afrique <-  c("Tunisie","Maroc","Mali","Libye", "Soudan","Congo-Kinshasa","Cameroun","Nigeria","Guinée-Bissau","Côte d'Ivoire")
Asie <- c("Chine","Inde","Japon","Pakistan","Kazakhstan","Indonésie","Liban","Arabie saoudite","Malaisie","Vietnam","Laos","Iran","Corée du Sud")

associations <- c(Europe="Europe", Amérique_Nord="Amérique du Nord", Amérique_Latine="Amérique Latine", Afrique="Afrique", Asie="Asie")

tableau_corruption$Continent <- NA

for(continent in names(associations)) {
 
  pays_continent <- get(continent)
  tableau_corruption$Continent[tableau_corruption$Pays %in% pays_continent] <- associations[continent]
}

tableau_corruption_filtré <- tableau_corruption %>%
  filter(!is.na(Continent))

print(tableau_corruption_filtré)

tableau_long <- pivot_longer(
       tableau_long,
       cols = !c(Continent, Pays), # Exclut Continent et Pays du pivot
       names_to = "Année",
       values_to = "Score"
   )
 
moyennes_par_continent_et_annee <- tableau_long %>%
       group_by(Continent, Année) %>%
       summarise(Moyenne_Score = mean(Score, na.rm = TRUE))


ggplot(moyennes_par_continent_et_annee, aes(x = as.factor(Année), y = Moyenne_Score, group = Continent, color = Continent)) +
  geom_line(size = 1.5) +
  theme_minimal() +
  labs(title = "Niveau Moyen de Corruption par Continent au Fil des Années",
       x = "Année",
       y = "Score Moyen de Corruption") +
  theme(plot.title = element_text(hjust = 0.5),  # Centre le titre
        panel.grid.major = element_blank(),  # Enlève le quadrillage
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 15),
        axis.title = element_text(size = 12),
        axis.line = element_line())  # Affiche les axes

