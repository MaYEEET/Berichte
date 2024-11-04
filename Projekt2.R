
read.delim("Konzentrationsdaten.txt")
data = Konzentrationsdaten

# Entfernen des Ausreißers mit ID = 14
data <- data[data$id != 14, ]

# Aufgabe 1
# Aufteilen der Konzentrationsdaten nach Gruppe
datag1 = subset(data, gruppe == 1)
datag2 = subset(data, gruppe == 2)

# Aufteilen der gruppierten Daten nach Durchgang
datag1d1 = subset(datag1, durchgang == 1)
datag2d1 = subset(datag2, durchgang == 1)

mean(datag1d1$KL)
# 11.255
mean(datag2d1$KL)
# 10.84
median(datag1d1$KL)
# 11.15
median(datag2d1$KL)
# 10.4

sd(datag1d1$KL)
# 4.02773
sd(datag2d1$KL)
# 3.655335

install.packages("car")
library(car)

# QQ-Plot mit Konfidenzintervallen fuer Gruppe 1, Durchgang 1
qqPlot(datag1d1$KL)
# Da alle Punkte innerhalb der Konfidenzintervalle liegen, ist die Annahme der
# Normalverteilung plausibel

# QQ-Plot mit Konfidenzintervallen fuer Gruppe 2, Durchgang 1
qqPlot(datag2d1$KL)
# Ebenfalls Annahme der Normalverteilung plausibel

# Shapiro-Wilk-Test fuer Normalverteilung
shapiro.test(datag1d1$KL)  # Gruppe 1
# p-Wert: 0.5617
shapiro.test(datag2d1$KL)  # Gruppe 2
# p-Wert: 0.3286
# p-Werte deutlich ueber dem Signifikanzniveau 0.05 => Werte weichen nicht von
# einer Normalverteilung ab

# Levene-Test zur ueberpruefung der Varianz-Homogenitaet
leveneTest(c(datag1d1$KL, datag2d1$KL), 
           group = factor(c(rep(1, nrow(datag1d1)), rep(2, nrow(datag2d1)))))
# p-Wert: 0.4317 > 0.05 => kein signifikanter Unterschied in den Varianzen der
# beiden Gruppen

# t-test zur ueberpruefung der Mittelwert-Unterschiede
t.test(datag1d1$KL, datag2d1$KL, alternative = "two.sided", var.equal = TRUE)
# p-Wert: 0.7348
# => kein statistisch signifikanter Unterschied in der KL zwischen den Gruppen
# Testart hat keinen wesentlichen Einfluss bezueglich der KL auf den ersten
# Durchgang




# Aufgabe 2

# Aufteilen der Daten nach Durchgang
datad1 = subset(data, durchgang == 1)
datad2 = subset(data, durchgang == 2)

# Berechnung der Differenzen fuer Konzentrationsleistung und Bearbeitungszeit
diff_KL <- datad2$KL - datad1$KL
diff_B <- datad2$B - datad1$B


# Shapiro-Wilk-Test fuer Normalverteilung der Differenzen in
# Konzentrationsleistung und Bearbeitungszeit
shapiro.test(diff_KL)
shapiro.test(diff_B)





# Vergleich der Konzentrationsleistung zwischen Durchgang 1 und Durchgang 2
t.test(diff_KL)
t.test(datad1$KL, datad2$KL, paired = TRUE)
# p-Wert: 4.066e-06 => KL deutlich hoeher im 2. Durchgang

# Vergleich der Bearbeitungszeit zwischen Durchgang 1 und Durchgang 2
t.test(diff_B)
t.test(datad1$B, datad2$B, paired = TRUE)
# p-Wert: 0.000142 => B deutlich kuerzer im 2. Durchgang

# Mittelwerte, Mediane und Standardabweichungen für beide Durchgänge
summary_stats <- data.frame(
  Durchgang = c(1, 2),
  Mittelwert_KL = c(mean(datad1$KL), mean(datad2$KL)),
  Median_KL = c(median(datad1$KL), median(datad2$KL)),
  SD_KL = c(sd(datad1$KL), sd(datad2$KL)),
  Mittelwert_B = c(mean(datad1$B), mean(datad2$B)),
  Median_B = c(median(datad1$B), median(datad2$B)),
  SD_B = c(sd(datad1$B), sd(datad2$B))
)
summary_stats

# Laden von ggplot2
library(ggplot2)

# Scatterplot für Konzentrationsleistung (Durchgang 1 vs. Durchgang 2)
ggplot() +
  geom_point(aes(x = datad1$KL, y = datad2$KL), color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Konzentrationsleistung (Durchgang 1)",
       y = "Konzentrationsleistung (Durchgang 2)") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20)
  )

# Scatterplot für Bearbeitungszeit (Durchgang 1 vs. Durchgang 2)
ggplot() +
  geom_point(aes(x = datad1$B, y = datad2$B), color = "green") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Bearbeitungszeit (Durchgang 1)",
       y = "Bearbeitungszeit (Durchgang 2)") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20)
  )




# wahrscheinlich unnoetig:
# Boxplot für Konzentrationsleistung
boxplot(datad1$KL, datad2$KL,
        names = c("Durchgang 1", "Durchgang 2"),
        main = "Konzentrationsleistung in beiden Durchgängen",
        ylab = "Konzentrationsleistung (KL)")

# Boxplot für Bearbeitungszeit
boxplot(datad1$B, datad2$B,
        names = c("Durchgang 1", "Durchgang 2"),
        main = "Bearbeitungszeit in beiden Durchgängen",
        ylab = "Bearbeitungszeit (B)")


library(ggplot2)
# Boxplot für Konzentrationsleistung (KL)
ggplot(data, aes(x = as.factor(durchgang), y = KL)) +
  geom_boxplot(fill = "skyblue", color = "darkblue") +
  labs(title = "Konzentrationsleistung in beiden Durchgängen",
       x = "Durchgang",
       y = "Konzentrationsleistung (KL)") +
  theme_minimal()

# Boxplot für Bearbeitungszeit (B)
ggplot(data, aes(x = as.factor(durchgang), y = B)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen") +
  labs(title = "Bearbeitungszeit in beiden Durchgängen",
       x = "Durchgang",
       y = "Bearbeitungszeit (B)") +
  theme_minimal()





# Effektstärke für Konzentrationsleistung und Bearbeitungszeit
install.packages("effsize")
library(effsize)

# Cohen's d für Konzentrationsleistung zwischen Durchgang 1 und Durchgang 2
cohen.d(datad1$KL, datad2$KL, paired = TRUE)
# d estimate: -0.6139877 (medium)
# 95 percent confidence interval:
# lower      upper 
# -0.8628432 -0.3651321 

# Die Effektstärke von −0.6139 zeigt, dass der Wiederholungseffekt in der
# Konzentrationsleistung einen mittleren Einfluss hat.
# Das bedeutet, dass die Wiederholung des Tests eine moderate Verbesserung
# der Konzentrationsleistung bewirkt.
# Ein negativer Wert deutet darauf hin, dass die Konzentrationsleistung im
# zweiten Durchgang höher ist als im ersten Durchgang
# [−0.8628,−0.3651] zeigt, dass der Effektbereich bei einem mittleren Effekt 
# liegt und der Unterschied relativ konsistent ist.
# Da das Konfidenzintervall den Nullwert nicht umfasst, 
# ist der Effekt statistisch signifikant.


# Cohen's d für Bearbeitungszeit zwischen Durchgang 1 und Durchgang 2
cohen.d(datad1$B, datad2$B, paired = TRUE)
# d estimate: 0.513063 (medium)
# 95 percent confidence interval:
#   lower     upper 
# 0.2554555 0.7706705 
# Die Effektstärke von 0.5131 zeigt, dass der Wiederholungseffekt einen
# mittleren Einfluss auf die Verkürzung der Bearbeitungszeit hat, was bedeutet,
# dass die Teilnehmer im zweiten Durchgang signifikant schneller wurden.
# Ein positiver Wert bedeutet, dass die Bearbeitungszeit im zweiten Durchgang
# kürzer war als im ersten, was auf einen positiven Wiederholungseffekt
# hindeutet.
# [0.2555,0.7707], was den Effektbereich abdeckt und zeigt,
# dass der Unterschied relativ konsistent ist.
# Da das Konfidenzintervall den Nullwert nicht umfasst,
# ist der Effekt statistisch signifikant.




# Aufgabe 3
datag1d2 = subset(datag1, durchgang == 2)
datag2d2 = subset(datag2, durchgang == 2)

diff_g1_KL = datag1d2$KL - datag1d1$KL
diff_g2_KL = datag2d2$KL - datag2d1$KL
diff_g1_B = datag1d2$B - datag1d1$B
diff_g2_B = datag2d2$B - datag2d1$B

data.frame(
  Gruppe = rep(c("GU-GU", "UG-GU"), each = 2),
  Variable = rep(c("Konzentrationsleistung", "Bearbeitungszeit"), times = 2),
  Mean = c(mean(diff_g1_KL), mean(diff_g1_B), mean(diff_g2_KL), 
           mean(diff_g2_B)),
  Median = c(median(diff_g1_KL), median(diff_g1_B), median(diff_g2_KL),
             median(diff_g2_B)),
  SD = c(sd(diff_g1_KL), sd(diff_g1_B), sd(diff_g2_KL), sd(diff_g2_B))
)
# Konzentrationsleistung:
# Beide Gruppen zeigen eine Verbesserung der Konzentrationsleistung
# (positive Mittelwerte der Differenzen), wobei die Gruppe UG-GU einen etwas
# höheren mittleren Anstieg von 3.12 im Vergleich zu 2.31 bei GU-GU aufweist.
# Die Standardabweichungen sind relativ niedrig (3.15 für GU-GU und 3.28 für
# UG-GU), was darauf hinweist, dass die Verbesserung der Konzentrationsleistung
# bei beiden Gruppen relativ konstant ist.
# Bearbeitungszeit:
# Beide Gruppen zeigen eine Verkürzung der Bearbeitungszeit (negative
# Mittelwerte der Differenzen), wobei die Verkürzung in der Gruppe UG-GU
# (-19.37) stärker ausfällt als in der Gruppe GU-GU (-14.27).
# Die Standardabweichung der Bearbeitungszeit ist jedoch höher, insbesondere in
# der Gruppe UG-GU (28.88), was auf eine größere Variabilität in den
# Verbesserungen der Bearbeitungszeit hinweist.


# Shapiro-Wilk-Test für Normalverteilung der Differenzen in Konzentrationsleistung und Bearbeitungszeit
shapiro.test(diff_g1_KL)
# p-Wert: 0.2586
shapiro.test(diff_g2_KL)
# p-Wert: 0.6598
# Beide p-Werte > 0.05 => weist darauf hin, dass die Differenzen in der KL für 
# beide Gruppen normalverteilt sind

shapiro.test(diff_g1_B)
# 0.05012
# NV kann knapp noch akzeptiert werden
shapiro.test(diff_g2_B)
# 0.7823
# NV für g2 klar gegeben

# Somit kann t-Test durchgeführt werden:

# t-Test für Konzentrationsleistung zwischen GU-GU und UG-GU
t.test(diff_g1_KL, diff_g2_KL, var.equal = TRUE)
# p-Wert: 0.434 => kein statistisch signifikanter Unterschied in der 
# Verbesserung der KL zwischen beider Gruppen
# beide Gruppen (GU-GU und UG-GU) zeigen eine Verbesserung in der 
# Konzentrationsleistung, aber die Größenordnung dieser Verbesserung ist
# zwischen den beiden Gruppen statistisch gesehen ähnlich.

# t-Test für Bearbeitungszeit zwischen GU-GU und UG-GU
t.test(diff_g1_B, diff_g2_B, var.equal = TRUE)
# p-Wert: 0.5291 => kein statistisch signifikanter Unterschied in der 
# Verbesserung der B zwischen beider Gruppen
# deutet darauf hin, dass Wiederholung beider Tests zu einer Verkürzung der B 
# führt, die Größenordnung dieses Effekts jedoch zwischen den beiden Gruppen 
# statistisch gesehen nicht unterschiedlich ist.


cohen.d(diff_g1_KL, diff_g2_KL)  # Für Konzentrationsleistung
# d estimate: -0.2500621 (small)
# 95 percent confidence interval:
#   lower      upper 
# -0.8927288  0.3926046 

cohen.d(diff_g1_B, diff_g2_B)    # Für Bearbeitungszeit
# d estimate: 0.2008897 (small)
# 95 percent confidence interval:
#   lower      upper 
# -0.4408926  0.8426720 

# Boxplot der Differenzen in Konzentrationsleistung für beide Gruppen

data_diff <- data.frame(
  Gruppe = rep(c("GU-GU", "UG-GU"), each = length(diff_g1_KL)),
  Diff_KL = c(diff_g1_KL, diff_g2_KL),
  Diff_B = c(diff_g1_B, diff_g2_B)
)

# Boxplot für Konzentrationsleistung
ggplot(data_diff, aes(x = Gruppe, y = Diff_KL)) +
  geom_boxplot(fill = "skyblue", color = "darkblue") +
  labs(title = "Differenzen in der Konzentrationsleistung zwischen GU-GU und
       UG-GU",
       x = "Gruppe",
       y = "Differenz in der Konzentrationsleistung (KL)") +
  theme_minimal()

# Boxplot für Bearbeitungszeit
ggplot(data_diff, aes(x = Gruppe, y = Diff_B)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen") +
  labs(title = "Differenzen in der Bearbeitungszeit zwischen GU-GU und UG-GU",
       x = "Gruppe",
       y = "Differenz in der Bearbeitungszeit (B)") +
  theme_minimal()

# Berechnung der Korrelation zwischen den Differenzen in Konzentrationsleistung
# und Bearbeitungszeit
cor(data_diff$Diff_KL, data_diff$Diff_B)

