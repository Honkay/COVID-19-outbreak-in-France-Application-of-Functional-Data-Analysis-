library(readxl)
Daily_death_at_each_department_as_functional_data_since_vaccination_started <- read_excel("Downloads/Daily death at each department as functional data since vaccination started.xlsx")
View(Daily_death_at_each_department_as_functional_data_since_vaccination_started)

X = Daily_death_at_each_department_as_functional_data_since_vaccination_started[,c('Ain', 'Aisne', 'Allier', 'Alpes-de-Haute-Provence', 'Hautes-Alpes','Alpes-Maritimes','Ardèche','Ardennes','Ariège','Aube','Aude','Aveyron','Bouches-du-Rhône','Calvados','Cantal',
                                          'Charente', 'Charente-Maritime', 'Cher', 'Corrèze', 'Côte-d-Or', 'Côtes-d-Armor','Creuse','Dordogne','Doubs','Drôme','Eure','Eure-et-Loir','Finistère','Corse-du-Sud','Haute-Corse','Gard',
                                          'Haute-Garonne', 'Gers', 'Gironde', 'Hérault', 'Ille-et-Vilaine', 'Indre','Indre-et-Loire','Isère','Jura','Landes','Loir-et-Cher','Loire','Haute-Loire','Loire-Atlantique','Loiret','Lot',
                                          'Lot-et-Garonne', 'Lozère', 'Maine-et-Loire', 'Manche', 'Marne', 'Haute-Marne','Mayenne','Meurthe-et-Moselle','Meuse','Morbihan','Moselle','Nièvre','Nord','Oise','Orne','Pas-de-Calais',
                                          'Puy-de-Dôme', 'Pyrénées-Atlantiques', 'Hautes-Pyrénées', 'Pyrénées-Orientales', 'Bas-Rhin', 'Haut-Rhin','Rhône','Haute-Saône','Saône-et-Loire','Sarthe','Savoie','Haute-Savoie','Paris','Seine-Maritime','Seine-et-Marne','Yvelines',
                                          'Deux-Sèvres', 'Somme', 'Tarn', 'Tarn-et-Garonne', 'Var', 'Vaucluse','Vendée','Vienne','Haute-Vienne','Vosges','Yonne','Territoire de Belfort','Essonne','Hauts-de-Seine','Seine-Saint-Denis','Val-de-Marne',
                                          'Val-d-Oise', 'Guadeloupe', 'Martinique', 'Guyane', 'La Réunion', 'Mayotte')]
Y = Daily_death_at_each_department_as_functional_data_since_vaccination_started[,c('Infected','Vaccination','Vaccination per 1000', 'Recovered','Test')]
model <- cancor(X,Y)
print(model)
barplot(model$cor, xlab = "Dimension", ylab = "Canonical correlations", ylim = c(0,1))

library(CCA)

model2 <- cc(X, Y)
plt.cc(model2, var.label = TRUE)
plt.var(model2, 1, 3, var.label = TRUE)
plt.var(model2, 2, 3, var.label = TRUE)
library(yacca)
cca.fit <- cca(X, Y)
#View the results
cca.fit
summary(cca.fit)
plot(cca.fit)
#Test the canonical correlations (see also summary(cca.fit))
F.test.cca(cca.fit)
#Show loadings on first canonical variate
helio.plot(cca.fit, x.name="French Dept",
           y.name="Epidemiology Variables")
#Show variances on second canonical variate
helio.plot(cca.fit, cv=2, x.name="French Dept",
           y.name="Epidemiology Variables", type="variance")
