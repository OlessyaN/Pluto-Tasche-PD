#   Ziele:     Berechnung der Konservativen PD nach der Methode von K.Pluto und D.Tasche;
#              Anpassung mithilfe von Scaling Factor 
#
#   Ausgangsituation: Die Berechnung der Ausfallwahrscheinlichkeit von Kreditportfolios spielt eine wichtige 
#                     Rolle bei der Bewertung des Kreditrisikos. Es gibt jedoch Situationen, in denen die Bestimmung der 
#                     Ausfallwahrscheinlichkeit aufgrund einer geringen Anzahl von Ausfällen im Portfolio oder deren vollständiger
#                     Abwesenheit unmöglich ist oder falsche Ergebnisse liefert. Dies tritt häufig auf, zum Beispiel bei der
#                     Finanzierung von Fonds, öffentlichen Dienst usw, wo Ausfälle sehr selten auftreten, aber für Kreditinstitute 
#                     ist die Berechnung der Ausfallwahrscheinlichkeit bei jeder Form der Finanzierung notwendig.
#                     In solchen Fällen werden verschiedene Methoden zur Berechnung der Ausfallwahrscheinlichkeit in 
#                     Low-Default-Portfolios verwendet. Eine davon ist die Methode von Pluto-Tasche, die die Wahrscheinlichkeit 
#                     des Ausfalls anhand der oberen Grenze des berechneten Konfidenzintervalls schätzt. 


rm()


library(dplyr)
library(xlsx)
library(ggplot2)
library(ggpubr)


# Anzahl der Ratings und der Ausfälle pro Ratingklasse von schlechteren zu den besseren Ratingklasse

portfolio <- c(500,450,300,400,100)
defaults<-c(2,1,1,2,0)

#Funktion, die konservative PD nach der Methode von Pluto-Tasche berechnet.
#
# One Period PD estimation Pluto & Tasche model
# Args:
#portf.uncond:unconditional portfolio distribution from the worst to the best credit
# quality
#   portf.def: number of defaults in a given rating class
#   conf.interval:  condifence interval of PD estimates
# Returns:
#  estimated conditional PDs

PTOnePeriodPD <- function(portf.uncond, portf.def, conf.interval = listpd) {
  r.num <- length(portf.uncond)
  r.PD <- rep(0, r.num)
  portf.CNum <- rev(cumsum(portf.uncond))
  portf.CDef <- rev(cumsum(portf.def))
  for (r in seq_len(r.num)) {
    if (portf.CDef[r] == portf.CNum[r]) {
      r.PD[r] <- 1
    } else {
      f <- function(x) pbinom(portf.CDef[r], portf.CNum[r], x) - 1 + conf.interval
      r.PD[r] <- uniroot(f, c(0, 1))$root
    }
  }
  return(rev(r.PD))
}

#im Datensatz hinzuzufügen und abzuspeichern

df<-as.data.frame(cbind(portfolio,defaults))

#Konfidenzintervalls definieren
list_ci<-c(0.50,0.75,0.90,0.95)

for (i in list_ci){
  result<-PTOnePeriodPD(portfolio, defaults, conf.interval = i)
  df[paste0('ci_',i)]<-result
}

#Konservative PD im Vergleich mit der tatsächlichen Ausfallrate 
df$ausfallrate<-df$defaults/df$portfolio #Ausfallrate berechnen

#zu jedem Konfidenzintervall einen Datensatz erstellen
df5.0<-as.data.frame(df %>% select(1:3,7))  
df7.5<-as.data.frame(df %>% select(1,2,4,7))
df9.0<-as.data.frame(df %>% select(1,2,5,7))
df9.5<-as.data.frame(df %>% select(1,2,6,7))

#Ratingklasse zuordnen

fuc1<-function (df){
  df[paste0('klasse')]<-c(5,4,3,2,1)
  return(df%>%arrange(klasse))
}

df5<-fuc1(df5.0)
df7<-fuc1(df7.5)
df9<-fuc1(df9.0)
df9.5<-fuc1(df9.5)
df5$ki<-50
df7$ki<-75
df9$ki<-90
df9.5$ki<-95


fuc2<-function (df){
  names(df)[3]<-"Konservative_PD"
  return(df)
}

df5<-fuc2(df5)
df7<-fuc2(df7)
df9<-fuc2(df9)
df9.5<-fuc2(df9.5)

df_gesamt<-rbind(df5,df7)   #alle Tabelle zusammenführen
df_gesamt<-rbind(df_gesamt,df9)
df_gesamt<-rbind(df_gesamt,df9.5)
df_gesamt$ki <- as.factor(df_gesamt$ki)

fuc3<-function (df){
  df[paste0('klasse')]<-c(5,4,3,2,1)
  return(df%>%arrange(klasse))
}
df<-fuc3(df)


#Die Darstellung dazu
g<-ggplot() + 
  geom_line( data=df_gesamt,aes(x = klasse, y = Konservative_PD*100, color = ki)) +
  geom_line(size = 1.5) +  
  labs(x = "Ratingklasse", y = "Konservative Pluto Tasche PD (%)", color = "Für Konfidenzintervalle (%)") +  # Изменяем название легенды
  scale_fill_manual(values = c("50" = "red", "75" = "blue", "90" = "green", "95" = "orange")) +
  geom_line( data=df,aes(x = klasse, y = ausfallrate*100)) +
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 12, face = "bold"))+
  scale_x_continuous(breaks = c(1, 2, 3,4,5), labels = c("1", "2", "3","4","5")) +
scale_y_continuous (breaks=seq(0,1.7,0.1))

g1<-g + 
  annotate("text", x = 2.1, y = 0.10,     label ="Tatsächliche Ausfallrate", colour = "black",size=3,alpha=0.3,angle = 0)
g1


rm(df5.0,df7.5,df9.0,df9.5,df7,df5,df9)


#Benjamin et al (2006) proposed the margin of conservatism approach which, in part, employed the most prudent
#estimation methodology in Pluto and Tasche (2006). However, rather than estimating the upper bound PD for
#each grade, this method begins with using the best grade estimated PD as the conservative portfolio PD. Then,
#one calculates the corresponding scaling factor to scale-up the initial estimated PD for each rating grade. As a
#result, factors that drive the the margin of conservatism PDs come from both the obligor distribution as well as
#the choice of method used in the initial PD estimation


df$we<-df$portfolio/sum(df$portfolio) #Berechnung der Gewichtung für den Scaling Factor 

#zu jedem Konfidenzintervall einen Datensatz erstellen
df5.0<-as.data.frame(df %>% select(1:3,7:9))
df7.5<-as.data.frame(df %>% select(1,2,4,7:9))
df9.0<-as.data.frame(df %>% select(1,2,5,7:9))
df9.50<-as.data.frame(df %>% select(1,2,6,7:9))

# PDs mit dem SF berechnen
fuc<-function (df){
  x<-df[3]*df[6]   #initial PD = Gewicht* konservative PD (Pluto Tasche)
  df[paste0('inPD')]<-x
  cons_PD<-sum(df$defaults)/sum(df$portfolio) # portfolio PD
  df[paste0('cons_PD')]<-cons_PD
  init_PD<-sum(df$inPD)
  df[paste0('init_PD')]<-init_PD
  SF<-cons_PD /init_PD      #Scaling Factor = conservative portfolio PD/initial portfolio PD 
  #Scaling Factor = Ausfallrate Portfolio / Konservative Pluto-Tasche PD*Anteil der Ratings pro Klasse
  df[paste0('SF')]<-SF
  PD_scaled<- SF*df[3]      #PD von Scaling Factor angepasst
  df[paste0('PD_scaled')]<-PD_scaled
  return (df)
}

#Zu jedem Konfidenzintervall die Berechnungen durchführen
df50<-fuc(df5.0)
df75<-fuc(df7.5)
df90<-fuc(df9.0)
df95<-fuc(df9.50)


fuc4<-function (df){
  df<-df[,c("klasse","PD_scaled")]
  return (df)
}

df5<-fuc4(df50)
df7<-fuc4(df75)
df9<-fuc4(df90)
df9.5<-fuc4(df95)
df5$ki<-50
df7$ki<-75
df9$ki<-90
df9.5$ki<-95

df_gesamt1<-rbind(df5,df7)   #alle Tabelle zusammenführen
df_gesamt1<-rbind(df_gesamt1,df9)
df_gesamt1<-rbind(df_gesamt1,df9.5)
df_gesamt1$ki <- as.factor(df_gesamt1$ki)

g2<-ggplot() + 
  geom_line( data=df_gesamt1,aes(x = klasse, y = PD_scaled*100, color = ki)) +
  geom_line(size = 1.5) +  
  labs(x = "Ratingklasse", y = "PD_scaled (%)", color = "Für Konfidenzintervalle (%)") +  # Изменяем название легенды
  scale_fill_manual(values = c("50" = "red", "75" = "blue", "90" = "green", "95" = "orange")) +
  geom_line( data=df,aes(x = klasse, y = ausfallrate*100)) +
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 12, face = "bold"))+
  scale_x_continuous(breaks = c(1, 2, 3,4,5), labels = c("1", "2", "3","4","5")) +  # Настроим ось x
  scale_y_continuous (breaks=seq(0,1.7,0.03))

g3<-g2 + 
  annotate("text", x = 2.2, y = 0.15,     label = "Tatsächliche Ausfallrate", colour = "black",size=3,alpha=0.3,angle =0)
g3


g_ges<-(ggarrange(g1,g3, 
                  nrow = 1,  ncol = 2));g_ges 


#Tabelle erstellen und abspeichern
df50<-df50%>%select(klasse,everything()) #"klasse" - erster Kolumn
df50.1<-df50[,c(1:4,10:11)]
names(df50.1)[6]<-"PD_scaled_ci_0.5"
func1<-function(df){
  df<-df[,c(3,10:11)]
  for (i in df[3]){
    names(df)[3]<-paste0(names(df)[3],"_",names(df)[1])
  }
  return (df)
}

df75.1<-func1(df75)
df90.1<-func1(df90) 
df95.1<-func1(df95) 


df_ges<-cbind(df50.1,df75.1)   #alle Tabelle zusammenführen
df_ges<-cbind(df_ges,df90.1)
df_ges<-cbind(df_ges,df95.1)

df_ges<-round(df_ges,4)


rm(df50,df75,df90,df95,df7,df5,df9,df50.1,df75.1,df90.1,df95.1,df_gesamt1)


#Scaling Factor -Berechnung nach den oberen Rand der konservativen PD von Pluto-Tasche

fuc<-function (df){
  x<-df[3]*df[6]   #initial PD = Gewicht* konservative PD (Pluto Tasche)
  df[paste0('inPD')]<-x
  cons_PD<-min(df[3]) #Oberer Rand
  df[paste0('cons_PD')]<-cons_PD
  init_PD<-sum(df$inPD)
  df[paste0('init_PD')]<-init_PD
  SF<-cons_PD /init_PD      #Scaling Factor = conservative portfolio PD/initial portfolio PD 
  df[paste0('SF')]<-SF
  PD_scaled<- SF*df[3]      #PD von Scaling Factor angepasst
  df[paste0('PD_scaled')]<-PD_scaled
  return (df)
}

df50<-fuc(df5.0)
df75<-fuc(df7.5)
df90<-fuc(df9.0)
df95<-fuc(df9.50)


df5<-fuc4(df50)
df7<-fuc4(df75)
df9<-fuc4(df90)
df9.5<-fuc4(df95)
df5$ki<-50
df7$ki<-75
df9$ki<-90
df9.5$ki<-95

df_gesamt1<-rbind(df5,df7)   #alle Tabelle zusammenführen
df_gesamt1<-rbind(df_gesamt1,df9)
df_gesamt1<-rbind(df_gesamt1,df9.5)
df_gesamt1$ki <- as.factor(df_gesamt1$ki)

g2.1<-ggplot() + 
  geom_line( data=df_gesamt1,aes(x = klasse, y = PD_scaled*100, color = ki)) +
  geom_line(size = 1.5) +  # Задаем толщину линий
  labs(x = "Ratingklasse", y = "PD_scaled nach oberen Rand (%)", color = "Für Konfidenzintervalle (%)") +  # Изменяем название легенды
  scale_fill_manual(values = c("50" = "red", "75" = "blue", "90" = "green", "95" = "orange")) +
  geom_line( data=df,aes(x = klasse, y = ausfallrate*100)) +
  theme_minimal()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 12, face = "bold"))+
  scale_x_continuous(breaks = c(1, 2, 3,4,5), labels = c("1", "2", "3","4","5")) +  # Настроим ось x
  scale_y_continuous (breaks=seq(0,1.7,0.05))

g3.1<-g2.1 + 
  annotate("text", x = 2.1, y = 0.1,     label = "Tatsächliche Ausfallrate", colour = "black",size=3,alpha=0.3,angle =0)
g3.1


g_ges1<-(ggarrange(g3,g3.1, 
                   nrow = 1,  ncol = 2));g_ges1 


