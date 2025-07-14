data = read.csv("covid_edytowane.csv")#, fileEncoding = 'WINDOWS-1250')
head(data)

sort(table(data$sczepionka_nazwa_edycja), decreasing = TRUE)
barplot(sort(table(data$sczepionka_nazwa_edycja)), col='lightblue')

summary(data$wiek)
wiek = data$wiek
hist(wiek, col='lightblue')

names(data)
hist(as.numeric(data$przeciwciala_1_bau_ml_edycja))


dane = read.csv("covid_edytowane_ponownie.csv")
dane


names(dane)

sum(is.na(dane$przeciwciala_1_bau_ml_nowe))
is.na(dane$przeciwciala_2_bau_ml_edycja)
is.na(dane$przeciwciala_3_bau_ml_nowe)

is.numeric(dane$przeciwciala_1_bau_ml_nowe)
is.numeric(dane$przeciwciala_2_bau_ml_edycja)
is.numeric(dane$przeciwciala_3_bau_ml_nowe)

przeciwciala_badanie_1 = dane$przeciwciala_1_bau_ml_nowe
przeciwciala_badanie_2 = dane$przeciwciala_2_bau_ml_edycja
przeciwciala_badanie_3 = dane$przeciwciala_3_bau_ml_nowe

hist(przeciwciala_badanie_1, col='lightblue')
hist(przeciwciala_badanie_2, col='lightblue')
hist(przeciwciala_badanie_3, col='lightblue')

sum(przeciwciala_badanie_3 < 1000, na.rm=TRUE)

boxplot(przeciwciala_badanie_1)
boxplot(przeciwciala_badanie_2)
boxplot(przeciwciala_badanie_3)


# Kryterium kwartylowe (Tukeya)

zakres = 3 
boxplot(przeciwciala_badanie_1, range=zakres)
boxplot.stats(przeciwciala_badanie_1, coef=zakres)$out # wartości obserwacji odstających
min(boxplot.stats(przeciwciala_badanie_1, coef=zakres)$out)
length(boxplot.stats(przeciwciala_badanie_1, coef=zakres)$out) # liczba obserwacji odstających
# 7 obserwacji skrajnych

zakres = 1.5
boxplot(przeciwciala_badanie_1, range=zakres)
boxplot.stats(przeciwciala_badanie_1, coef=zakres)$out # wartości obserwacji odstających
min(boxplot.stats(przeciwciala_badanie_1, coef=zakres)$out)
length(boxplot.stats(przeciwciala_badanie_1, coef=zakres)$out) # liczba obserwacji odstających
# 25 obserwacji odstajacych


# Kryterium kwartylowe (Tukeya)

zakres = 3 
boxplot(przeciwciala_badanie_2, range=zakres)
boxplot.stats(przeciwciala_badanie_2, coef=zakres)$out # wartości obserwacji odstających
min(boxplot.stats(przeciwciala_badanie_2, coef=zakres)$out)
length(boxplot.stats(przeciwciala_badanie_2, coef=zakres)$out) # liczba obserwacji odstających
# 12 obserwacji skrajnych


zakres = 1.5
boxplot(przeciwciala_badanie_2, range=zakres)
boxplot.stats(przeciwciala_badanie_2, coef=zakres)$out # wartości obserwacji odstających
min(boxplot.stats(przeciwciala_badanie_2, coef=zakres)$out)
length(boxplot.stats(przeciwciala_badanie_2, coef=zakres)$out) # liczba obserwacji odstających
# 22 obserwacje odstajace



# Kryterium kwartylowe (Tukeya)

zakres = 3 
boxplot(przeciwciala_badanie_3, range=zakres)
boxplot.stats(przeciwciala_badanie_3, coef=zakres)$out # wartości obserwacji odstających
min(boxplot.stats(przeciwciala_badanie_3, coef=zakres)$out)
length(boxplot.stats(przeciwciala_badanie_3, coef=zakres)$out) # liczba obserwacji odstających
# 23 obserwacje skrajne

zakres = 1.5
boxplot(przeciwciala_badanie_3, range=zakres)
boxplot.stats(przeciwciala_badanie_3, coef=zakres)$out # wartości obserwacji odstających
min(boxplot.stats(przeciwciala_badanie_3, coef=zakres)$out)
length(boxplot.stats(przeciwciala_badanie_3, coef=zakres)$out) # liczba obserwacji odstających
# 30 obserwacji odstajacych

########################################################################
#Proporcje osób z objawami w zalezności od grupy wiekowej
dane$grupa_wiekowa = ifelse(dane$wiek < 30, "20-29", ifelse(dane$wiek < 40, "30-39",
                        ifelse(dane$wiek < 50, "40-49",ifelse(dane$wiek < 60, "50-59",
                          ifelse(dane$wiek < 70, "60-69","70-79")))))

wiek_objawy = table(dane$grupa_wiekowa, dane$czy_objawy)
wiek_objawy = wiek_objawy[, c("TAK", "NIE")]
wiek_objawy
prop.test(wiek_objawy)
fisher.test(wiek_objawy)
##### Proporcje osób z objawami w zalezności od rodzaju szczepionki

szczepionka_objawy = table(dane$sczepionka_nazwa_edycja, dane$czy_objawy)
szczepionka_objawy = szczepionka_objawy[, c("TAK", "NIE")] #zamiana kolejnością kolumn
szczepionka_objawy

prop.test(szczepionka_objawy) #test proporcji wyrzuca ostrzeżenie - istnieją wartości poniżej 5 w tabeli
fisher.test(szczepionka_objawy) #dokładny test Fishera - błędów nie wywala, ale czy on nie był tylko dla tabel 2x2?
#p < 0.05, zatem istnieje zależność między rodzajem szczepienia a występowaniem objawów
#może jakoś bootstrap pod to podpiąć?

##### Iloraz szans występowania objawów w grupie szczepionych do grupy nieszczepionych
library(epitools)
a = length(dane$czy_objawy[dane$czy_objawy == 'TAK' & dane$sczepionka_nazwa_edycja != 'NIE'])
b = length(dane$czy_objawy[dane$czy_objawy == 'NIE' & dane$sczepionka_nazwa_edycja != 'NIE'])
c = length(dane$czy_objawy[dane$czy_objawy == 'TAK' & dane$sczepionka_nazwa_edycja == 'NIE'])
d = length(dane$czy_objawy[dane$czy_objawy == 'NIE' & dane$sczepionka_nazwa_edycja == 'NIE'])
OR_tabela = matrix(data = c(a,b,c,d), nrow = 2, byrow = T, dimnames=list("Szczepiony"=c("tak", "nie"), "Objawy"=c("tak", "nie")))
OR_tabela
oddsratio(OR_tabela) #OR = 0.1755, co oznacza, że ryzyko wystapienia objawów
#u osób nieszczepionych jest ponad 5.5-krotnie wieksze niż u osób szczepionych
chisq.test(OR_tabela, correct = F)
#### rozkład antygenów w zalezności od występowania objawów
#objawy tak - 69 obserwacji
#objawy nie - 242 obserwacje
antygen_objawy1_T = dane$przeciwciala_1_bau_ml_nowe[dane$czy_objawy == "TAK"] #9 braków
antygen_objawy1_N = dane$przeciwciala_1_bau_ml_nowe[dane$czy_objawy == "NIE"] #63 braki
antygen_objawy2_T = dane$przeciwciala_2_bau_ml_edycja[dane$czy_objawy == "TAK"] #28 braków
antygen_objawy2_N = dane$przeciwciala_2_bau_ml_edycja[dane$czy_objawy == "NIE"] #83 braki
antygen_objawy3_T = dane$przeciwciala_3_bau_ml_nowe[dane$czy_objawy == "TAK"] #22 braki
antygen_objawy3_N = dane$przeciwciala_3_bau_ml_nowe[dane$czy_objawy == "NIE"] #75 braków

# wykresy
library(ggplot2)
ggplot(dane, aes(x = przeciwciala_1_bau_ml_nowe, fill = czy_objawy)) + 
  geom_density(alpha = 0.5) +
  xlab('Poziom przeciwciał (BAU/ml)') +
  scale_fill_manual(values = c('#1f77b4', '#d62728')) 
#ggsave("density1.png", dpi = 3000)
ggplot(dane, aes(x = przeciwciala_2_bau_ml_edycja, fill = czy_objawy)) + 
  geom_density(alpha = 0.5) +
  xlab('Poziom przeciwciał (BAU/ml)') +
  scale_fill_manual(values = c('#1f77b4', '#d62728'))
#ggsave("density2.png", dpi = 3000)
ggplot(dane, aes(x = przeciwciala_3_bau_ml_nowe, fill = czy_objawy)) + 
  geom_density(alpha = 0.5) +
  xlab('Poziom przeciwciał (BAU/ml)') +
  scale_fill_manual(values = c('#1f77b4', '#d62728'))
#ggsave("density3.png", dpi = 3000)

# test t-Studenta nie spełnia założeń - rozkład nie jest normalny oraz liczbność za mała (69 < 100)
# wykonany zostanie nieparametryczny test Wilcoxona

median(antygen_objawy1_N, na.rm = T) #390.5
median(antygen_objawy1_T, na.rm = T) #426.635
median(antygen_objawy2_N, na.rm = T) #529.6
median(antygen_objawy2_T, na.rm = T) #2175.1
median(antygen_objawy3_N, na.rm = T) #214.6
median(antygen_objawy3_T, na.rm = T) #1304.5


wilcox.test(antygen_objawy1_T, antygen_objawy1_N) #p = 0.6684
wilcox.test(antygen_objawy2_T, antygen_objawy2_N) #p = 9.778e-10
wilcox.test(antygen_objawy3_T, antygen_objawy3_N) #p = 8.398e-14

sum(is.na(antygen_objawy1_N))

dane_as = dane[!is.na(dane$przeciwciala_3_bau_ml_nowe) & dane$szczepienie_2_data_edycja != 'NIE',]
dane_as$dni11 = as.Date(dane_as$badanie_1_data) - as.Date(dane_as$szczepienie_1_data_edycja)
dane_as$dni21 = as.Date(dane_as$badanie_2_data) - as.Date(dane_as$szczepienie_1_data_edycja)
dane_as$dni31 = as.Date(dane_as$badanie_3_data) - as.Date(dane_as$szczepienie_1_data_edycja)
dane_as$dni12 = as.Date(dane_as$badanie_1_data) - as.Date(dane_as$szczepienie_2_data_edycja)
dane_as$dni22 = as.Date(dane_as$badanie_2_data) - as.Date(dane_as$szczepienie_2_data_edycja)
dane_as$dni32 = as.Date(dane_as$badanie_3_data) - as.Date(dane_as$szczepienie_2_data_edycja)

dane_as2 = dane_as[dane_as$covid_data_edycja != 'NIE',]
plot(dane_as2$dni31, dane_as2$przeciwciala_1_bau_ml_nowe, col = as.factor(dane_as2$sczepionka_nazwa_edycja), cex = 2.5, pch = 20)
plot(dane_as2$dni31, dane_as2$przeciwciala_2_bau_ml_edycja, col = as.factor(dane_as2$sczepionka_nazwa_edycja), cex = 2.5, pch = 20)
plot(dane_as2$dni31, dane_as2$przeciwciala_3_bau_ml_nowe, col = as.factor(dane_as2$sczepionka_nazwa_edycja), cex = 2.5, pch = 20)
legend('topleft',unique(dane_as2$sczepionka_nazwa_edycja),col=1:length(dane_as2$sczepionka_nazwa_edycja),pch=20, pt.cex = 5)


###########3
choroba_objawy = rbind(colSums(dane2[dane2$czy_objawy == 'TAK', c(10,11,12)]), colSums(dane2[dane2$czy_objawy == 'NIE', c(10,11,12)]))
rownames(choroba_objawy) = c("Objawy", 'Brak objawów')
choroba_objawy = cbind(choroba_objawy, c(length(dane2$nr[dane2$choroby_wspolistniejace_edycja == 'NIE' & dane2$czy_objawy == 'TAK']), 
                                         length(dane2$nr[dane2$choroby_wspolistniejace_edycja == 'NIE' & dane2$czy_objawy == 'NIE'])))
choroba_objawy
length(dane2$nr[dane2$choroby_wspolistniejace_edycja == 'NIE' & dane2$czy_objawy == 'TAK'])
length(dane2$nr[dane2$choroby_wspolistniejace_edycja == 'NIE' & dane2$czy_objawy == 'NIE'])
chisq.test(choroba_objawy)
fisher.test(choroba_objawy) #Przyjmujemy H0 - niezależność między objawami a chorobami współistniejącymi

#############
choroba_covid = rbind(colSums(dane2[dane2$covid_data_edycja != 'NIE', c(10,11,12)]), colSums(dane2[dane2$covid_data_edycja == 'NIE', c(10,11,12)]))
rownames(choroba_covid) = c("chory", 'zdrowy')
choroba_covid = cbind(choroba_covid, c(length(dane2$nr[dane2$choroby_wspolistniejace_edycja == 'NIE' & dane2$covid_data_edycja != 'NIE']), 
                                         length(dane2$nr[dane2$choroby_wspolistniejace_edycja == 'NIE' & dane2$covid_data_edycja == 'NIE'])))
choroba_covid
chisq.test(choroba_covid)
fisher.test(choroba_covid) #Przyjmujemy H0 - niezależność między COVID a chorobami współistniejącymi
prop.test(t(choroba_covid))
