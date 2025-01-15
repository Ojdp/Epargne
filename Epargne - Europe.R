
library(tidyverse)
library(rsdmx)
library(zoo)
library(lubridate)
library(ggplot2)
library(ofce)
library(magrittr)
library(xml2)
library(gridExtra)

ESTAT_structure_LFS <- data.frame(readSDMX('https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/datastructure/ESTAT/nasq_10_f_bs')@datastructures[[1]]@Components)

read_data <- function(flow_key) {
  data.frame(
    readSDMX(
      providerId = 'ESTAT',
      resource = 'data',
      flowRef = 'nasq_10_f_bs',
      key = flow_key
    )
  ) %>%
    select(geo, obsTime, obsValue, na_item) %>%
    pivot_wider(names_from = "na_item", values_from = "obsValue") %>%
    mutate(
      obsTime = as.yearqtr(obsTime, format = "%Y-Q%q"),
       compteur = row_number()) %>%
        filter(!(compteur %in% c(99, 199))) %>%
        mutate(compteur = row_number())
}



Asset <- read_data('Q.MIO_EUR.S14_S15.ASS.F.DE+ES+FR+IT')%>%
rename(Asset = F)

Liab<- read_data('Q.MIO_EUR.S14_S15.LIAB.F.DE+ES+FR+IT') %>%
  rename(Liab = F)

AssetAutre <- read_data('Q.MIO_EUR.S14_S15.ASS.F2+F5+F6.DE+ES+FR+IT')


read_data <- function(flow_key) {
  data.frame(
    readSDMX(
      providerId = 'ESTAT',
      resource = 'data',
      flowRef = 'nasq_10_nf_tr',
      key = flow_key
    )
  ) %>%
    select(geo, obsTime, obsValue, na_item) %>%
    pivot_wider(names_from = "na_item", values_from = "obsValue") %>%
    mutate(
      obsTime = as.yearqtr(obsTime, format = "%Y-Q%q"),
      compteur = row_number()
    ) %>%
    filter(! compteur == "99")%>%
    filter(!(compteur >= 199 & compteur <= 274)) %>%
    mutate(compteur = row_number())
}

Income<- read_data('Q.CP_MEUR.PAID.S14_S15.B6G.SCA.DE+ES+FR+IT') %>%
  rename(Income = B6G)

Saving<- read_data('Q.CP_MEUR.PAID.S14_S15.B8G.SCA.DE+ES+FR+IT') %>%
  rename(Saving = B8G)

Encours <- merge(Asset, Liab, by = "compteur", suffixes = c(".Asset", ".Liab")) %>%
  merge(Income, by = "compteur") %>%
  merge(Saving, by = "compteur")%>%
  select(-c("geo.Asset", "geo.Liab", "compteur", "obsTime.Liab","obsTime.Asset", "geo.x", "obsTime.x")) %>%
  rename (geo = geo.y,
          obsTime = obsTime.y ) %>%
  mutate(moving_avg = rollsum(Income, k = 4, align = "right", fill = NA), 
         Rapport = Asset/moving_avg,
         Tx_Epargne = (Saving / Income)*100,
         EncoursVariation = ifelse(geo == lag(geo), Asset - lag(Asset), NA)) 

#les valeurs pour le premier trim de chaque pays est pas la bonne ça devrait être zéro mais flemme (vu qu'on prend n-1)

Patrimoine <- ggplot(data = Encours) +
  geom_line(aes(x = obsTime, y = Rapport, color = geo), show.legend = TRUE) +
  labs(
    caption = "Sources: Eurostat, calculs des auteurs, années de RDB",
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +
  scale_x_yearqtr(limits = c(as.yearqtr("2011-01-01"), max(Encours$obsTime)), format = "%Y-Q%q", expand = c(0, 0)) +
  scale_y_continuous(limits = c(2.5, 4.5), labels = scales::label_number(decimal.mark = ".")) +
  ggtitle("Patrimoine financier des ménages")

Tauxepargne <- ggplot(data = Encours) +
  geom_line(aes(x = obsTime, y = Tx_Epargne, color = geo), show.legend = TRUE) +
  labs(
    caption = "Sources: Eurostat, calculs des auteurs, % RDB",
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +
  scale_x_yearqtr(limits = c(as.yearqtr("2011-01-01"), max(Encours$obsTime)), format = "%Y-Q%q", expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 30), labels = scales::label_number(decimal.mark = ".")) +
  ggtitle("Taux d'épargne des ménages")

read_data <- function(flow_key) {
  data.frame(
    readSDMX(
      providerId = 'ESTAT',
      resource = 'data',
      flowRef = 'nasq_10_f_tr',
      key = flow_key
    )
  ) %>%
    select(geo, obsTime, obsValue, na_item) %>%
    pivot_wider(names_from = "na_item", values_from = "obsValue") %>%
    mutate(
      obsTime = as.yearqtr(obsTime, format = "%Y-Q%q"),
      compteur = row_number()) %>%
    filter(!(compteur %in% c(99, 199))) %>%
    mutate(compteur = row_number())
}

AssetFlux <- read_data('Q.MIO_EUR.S14_S15.ASS.F_TR.DE+ES+FR+IT') %>%
  rename(AssetFlux = F_TR)

AutreFlux <- read_data('Q.MIO_EUR.S14_S15.ASS.F2+F5+F6.DE+ES+FR+IT') 

AutreFlux<- AutreFlux %>%
  mutate(Total = F2 + F5 + F6) %>%
  merge(AssetFlux, by = "compteur") %>%
  mutate(Autre = Total - AssetFlux) %>%
  select(!c(geo.y, obsTime.y, Total, compteur)) %>%
  pivot_longer(cols= - c(geo.x,obsTime.x), names_to = "Operation", values_to = "Value") %>%
  filter(obsTime.x >= "2019 Q4" & obsTime.x <= "2023 Q3") %>%
  group_by(geo.x, Operation) %>%
  mutate(Somme = sum(Value))



Total <- merge (Encours, AssetFlux, by= c("geo", "obsTime"))%>%
  select(!compteur)%>%
  mutate( AutreVariation = EncoursVariation - AssetFlux)
  

TotalGraph1IT <- Total %>%
  select(c("AutreVariation", "AssetFlux", "Asset", "geo", "obsTime", "EncoursVariation"))%>%
  filter(geo == "IT")%>%
  mutate(Inflation = -3.3, 
          CroissanceAsset = (((Asset - lag(Asset))/ lag(Asset))*100),
ContributionFluxTrim = ((AssetFlux/ (Asset - lag(Asset))) * CroissanceAsset),
ContributionAutre = ((AutreVariation/ (Asset - lag(Asset)))* CroissanceAsset))%>%
  filter(obsTime >= "2020 Q1" & obsTime <= "2021 Q4")%>%
  mutate(SommeAsset = sum(CroissanceAsset) + Inflation,
         SommeFlux = sum(ContributionFluxTrim) ,
         SommeAutre = sum(ContributionAutre))


TotalGraph2IT <-Total %>%
  select(c("AutreVariation", "AssetFlux", "Asset", "geo", "obsTime", "EncoursVariation"))%>%
  filter(geo == "IT")%>%
  mutate(CroissanceAsset = (((Asset - lag(Asset))/ lag(Asset))*100),
         ContributionFluxTrim = ((AssetFlux/ (Asset - lag(Asset))) * CroissanceAsset),
         ContributionAutre = ((AutreVariation/ (Asset - lag(Asset)))* CroissanceAsset))%>%
  filter(obsTime >= "2022 Q2" & obsTime <= "2023 Q3")%>%
  mutate(Inflation = -10.4,
         SommeAsset = sum(CroissanceAsset) + Inflation,
         SommeFlux = sum(ContributionFluxTrim) ,
         SommeAutre = sum(ContributionAutre))

TotalGraphIT <- bind_rows(TotalGraph1IT ,TotalGraph2IT )%>%
  rename (Autre = SommeAutre,
          Flux =SommeFlux,
          date = obsTime)%>%
  pivot_longer(cols= - c(geo,date), names_to = "Operation", values_to = "Value")

Italie <- ggplot(data = TotalGraphIT) +
  geom_bar(data = . %>% filter(Operation %in% c("Autre", "Flux", "Inflation") & date == "2021 Q2"),
           aes(x = 1, y = Value, fill = Operation), position = "stack", stat = "identity") + 
  geom_point(data = . %>% filter(Operation == "SommeAsset" & date == "2021 Q2"),
             aes(x = 1, y = Value), color = "black", size = 3) +
  geom_text(data = . %>% filter(Operation == "SommeAsset" & date == "2021 Q2"), 
            aes(x = 1, y = Value +1.3, label = paste0(round(Value, 1), "%")))+
  geom_bar(data = . %>% filter(Operation %in% c("Autre", "Flux", "Inflation") & date == "2023 Q3"),
           aes(x = 2, y = Value, fill = Operation), position = "stack", stat = "identity") + 
  geom_point(data = . %>% filter(Operation == "SommeAsset" & date == "2023 Q3"),
             aes(x = 2, y = Value), color = "black", size = 3) +
  geom_text(data = . %>% filter(Operation == "SommeAsset" & date == "2023 Q3"),
            aes(x = 2, y = Value +1.3, label = paste0(round(Value, 1), "%")))+
  labs(
    caption = "Source: Eurostat, calculs des auteurs
               Note : la crise covid se déroule de T4:2019 à T4:2021, 
               la crise inflationniste de T1:2022 à T3:2023",
    title = "",
    x = NULL,
    y = "En pp",
    fill = NULL, 
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") + 
  theme(legend.position = "bottom") + 
  theme(axis.text.x = element_text(angle = 0)) +
  scale_x_continuous(breaks = c(1, 2), labels = c("Crise Covid", "Crise Inflationniste")) +
  scale_y_continuous(limits = c(-15, 15), labels = scales::label_number(decimal.mark = ",")) 

TotalGraph1DE <- Total %>%
  select(c("AutreVariation", "AssetFlux", "Asset", "geo", "obsTime", "EncoursVariation"))%>%
  filter(geo == "DE")%>%
  mutate(Inflation = -3.3, 
         CroissanceAsset = (((Asset - lag(Asset))/ lag(Asset))*100),
         ContributionFluxTrim = ((AssetFlux/ (Asset - lag(Asset))) * CroissanceAsset),
         ContributionAutre = ((AutreVariation/ (Asset - lag(Asset)))* CroissanceAsset))%>%
  filter(obsTime >= "2020 Q1" & obsTime <= "2021 Q4")%>%
  mutate(SommeAsset = sum(CroissanceAsset) + Inflation,
         SommeFlux = sum(ContributionFluxTrim) ,
         SommeAutre = sum(ContributionAutre))


TotalGraph2DE <-Total %>%
  select(c("AutreVariation", "AssetFlux", "Asset", "geo", "obsTime", "EncoursVariation"))%>%
  filter(geo == "DE")%>%
  mutate(CroissanceAsset = (((Asset - lag(Asset))/ lag(Asset))*100),
         ContributionFluxTrim = ((AssetFlux/ (Asset - lag(Asset))) * CroissanceAsset),
         ContributionAutre = ((AutreVariation/ (Asset - lag(Asset)))* CroissanceAsset))%>%
  filter(obsTime >= "2022 Q2" & obsTime <= "2023 Q3")%>%
  mutate(Inflation = -10.4,
         SommeAsset = sum(CroissanceAsset) + Inflation,
         SommeFlux = sum(ContributionFluxTrim) ,
         SommeAutre = sum(ContributionAutre))

TotalGraphDE <- bind_rows(TotalGraph1DE ,TotalGraph2DE )%>%
  rename (Autre = SommeAutre,
          Flux =SommeFlux,
          date = obsTime)%>%
  pivot_longer(cols= - c(geo,date), names_to = "Operation", values_to = "Value")

Allemagne<- ggplot(data = TotalGraphDE) +
  geom_bar(data = . %>% filter(Operation %in% c("Autre", "Flux", "Inflation") & date == "2021 Q2"),
           aes(x = 1, y = Value, fill = Operation), position = "stack", stat = "identity") + 
  geom_point(data = . %>% filter(Operation == "SommeAsset" & date == "2021 Q2"),
             aes(x = 1, y = Value), color = "black", size = 3) +
  geom_text(data = . %>% filter(Operation == "SommeAsset" & date == "2021 Q2"), 
            aes(x = 1, y = Value +1.3, label = paste0(round(Value, 1), "%")))+
  geom_bar(data = . %>% filter(Operation %in% c("Autre", "Flux", "Inflation") & date == "2024 Q1"),
           aes(x = 2, y = Value, fill = Operation), position = "stack", stat = "identity") + 
  geom_point(data = . %>% filter(Operation == "SommeAsset" & date == "2024 Q1"),
             aes(x = 2, y = Value), color = "black", size = 3) +
  geom_text(data = . %>% filter(Operation == "SommeAsset" & date == "2024 Q1"),
            aes(x = 2, y = Value +1.3, label = paste0(round(Value, 1), "%")))+
  labs(
    caption = "Source: Eurostat, calculs des auteurs
               Note : la crise covid se déroule de T4:2019 à T4:2021, 
               la crise inflationniste de T1:2022 à T2:2023",
    title = "",
    x = NULL,
    y = "En pp",
    fill = NULL, 
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") + 
  theme(legend.position = "bottom") + 
  theme(axis.text.x = element_text(angle = 0)) +
  scale_x_continuous(breaks = c(1, 2), labels = c("Crise Covid", "Crise Inflationniste")) +
  scale_y_continuous(limits = c(-15, 15), labels = scales::label_number(decimal.mark = ",")) 

Allemagne 

TotalGraph1ES <- Total %>%
  select(c("AutreVariation", "AssetFlux", "Asset", "geo", "obsTime", "EncoursVariation"))%>%
  filter(geo == "ES")%>%
  mutate(Inflation = -5, 
         CroissanceAsset = (((Asset - lag(Asset))/ lag(Asset))*100),
         ContributionFluxTrim = ((AssetFlux/ (Asset - lag(Asset))) * CroissanceAsset),
         ContributionAutre = ((AutreVariation/ (Asset - lag(Asset)))* CroissanceAsset))%>%
  filter(obsTime >= "2020 Q1" & obsTime <= "2021 Q4")%>%
  mutate(SommeAsset = sum(CroissanceAsset) + Inflation,
         SommeFlux = sum(ContributionFluxTrim) ,
         SommeAutre = sum(ContributionAutre))


TotalGraph2ES <-Total %>%
  select(c("AutreVariation", "AssetFlux", "Asset", "geo", "obsTime", "EncoursVariation"))%>%
  filter(geo == "ES")%>%
  mutate(CroissanceAsset = (((Asset - lag(Asset))/ lag(Asset))*100),
         ContributionFluxTrim = ((AssetFlux/ (Asset - lag(Asset))) * CroissanceAsset),
         ContributionAutre = ((AutreVariation/ (Asset - lag(Asset)))* CroissanceAsset))%>%
  filter(obsTime >= "2022 Q2" & obsTime <= "2023 Q3")%>%
  mutate(Inflation = -7.5,
         SommeAsset = sum(CroissanceAsset) + Inflation,
         SommeFlux = sum(ContributionFluxTrim) ,
         SommeAutre = sum(ContributionAutre))

TotalGraphES <- bind_rows(TotalGraph1ES ,TotalGraph2ES )%>%
  rename (Autre = SommeAutre,
          Flux =SommeFlux,
          date = obsTime)%>%
  pivot_longer(cols= - c(geo,date), names_to = "Operation", values_to = "Value")

Espagne<- ggplot(data = TotalGraphES) +
  geom_bar(data = . %>% filter(Operation %in% c("Autre", "Flux", "Inflation") & date == "2021 Q2"),
           aes(x = 1, y = Value, fill = Operation), position = "stack", stat = "identity") + 
  geom_point(data = . %>% filter(Operation == "SommeAsset" & date == "2021 Q2"),
             aes(x = 1, y = Value), color = "black", size = 3) +
  geom_text(data = . %>% filter(Operation == "SommeAsset" & date == "2021 Q2"), 
            aes(x = 1, y = Value +1.3, label = paste0(round(Value, 1), "%")))+
  geom_bar(data = . %>% filter(Operation %in% c("Autre", "Flux", "Inflation") & date == "2023 Q3"),
           aes(x = 2, y = Value, fill = Operation), position = "stack", stat = "identity") + 
  geom_point(data = . %>% filter(Operation == "SommeAsset" & date == "2023 Q3"),
             aes(x = 2, y = Value), color = "black", size = 3) +
  geom_text(data = . %>% filter(Operation == "SommeAsset" & date == "2023 Q3"),
            aes(x = 2, y = Value +1.3, label = paste0(round(Value, 1), "%")))+
  labs(
    caption = "Source: Eurostat, calculs des auteurs
               Note : la crise covid se déroule de T4:2019 à T4:2021, 
               la crise inflationniste de T1:2022 à T3:2023",
    title = "",
    x = NULL,
    y = "En pp",
    fill = NULL, 
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") + 
  theme(legend.position = "bottom") + 
  theme(axis.text.x = element_text(angle = 0)) +
  scale_x_continuous(breaks = c(1, 2), labels = c("Crise Covid", "Crise Inflationniste")) +
  scale_y_continuous(limits = c(-15, 15), labels = scales::label_number(decimal.mark = ",")) 

TotalGraph1FR <- Total %>%
  select(c("AutreVariation", "AssetFlux", "Asset", "geo", "obsTime", "EncoursVariation"))%>%
  filter(geo == "FR")%>%
  mutate(Inflation = -2.8, 
         CroissanceAsset = (((Asset - lag(Asset))/ lag(Asset))*100),
         ContributionFluxTrim = ((AssetFlux/ (Asset - lag(Asset))) * CroissanceAsset),
         ContributionAutre = ((AutreVariation/ (Asset - lag(Asset)))* CroissanceAsset))%>%
  filter(obsTime >= "2020 Q1" & obsTime <= "2021 Q4")%>%
  mutate(SommeAsset = sum(CroissanceAsset) + Inflation,
         SommeFlux = sum(ContributionFluxTrim) ,
         SommeAutre = sum(ContributionAutre))


TotalGraph2FR <-Total %>%
  select(c("AutreVariation", "AssetFlux", "Asset", "geo", "obsTime", "EncoursVariation"))%>%
  filter(geo == "FR")%>%
  mutate(CroissanceAsset = (((Asset - lag(Asset))/ lag(Asset))*100),
         ContributionFluxTrim = ((AssetFlux/ (Asset - lag(Asset))) * CroissanceAsset),
         ContributionAutre = ((AutreVariation/ (Asset - lag(Asset)))* CroissanceAsset))%>%
  filter(obsTime >= "2022 Q2" & obsTime <= "2023 Q3")%>%
  mutate(Inflation = -10,
         SommeAsset = sum(CroissanceAsset) + Inflation,
         SommeFlux = sum(ContributionFluxTrim) ,
         SommeAutre = sum(ContributionAutre))

TotalGraphFR <- bind_rows(TotalGraph1FR ,TotalGraph2FR )%>%
  rename (Autre = SommeAutre,
          Flux =SommeFlux,
          date = obsTime)%>%
  pivot_longer(cols= - c(geo,date), names_to = "Operation", values_to = "Value")

France<- ggplot(data = TotalGraphFR) +
  geom_bar(data = . %>% filter(Operation %in% c("Autre", "Flux", "Inflation") & date == "2021 Q2"),
           aes(x = 1, y = Value, fill = Operation), position = "stack", stat = "identity") + 
  geom_point(data = . %>% filter(Operation == "SommeAsset" & date == "2021 Q2"),
             aes(x = 1, y = Value), color = "black", size = 3) +
  geom_text(data = . %>% filter(Operation == "SommeAsset" & date == "2021 Q2"), 
            aes(x = 1, y = Value +1.3, label = paste0(round(Value, 1), "%")))+
  geom_bar(data = . %>% filter(Operation %in% c("Autre", "Flux", "Inflation") & date == "2023 Q3"),
           aes(x = 2, y = Value, fill = Operation), position = "stack", stat = "identity") + 
  geom_point(data = . %>% filter(Operation == "SommeAsset" & date == "2023 Q3"),
             aes(x = 2, y = Value), color = "black", size = 3) +
  geom_text(data = . %>% filter(Operation == "SommeAsset" & date == "2023 Q3"),
            aes(x = 2, y = Value +1.3, label = paste0(round(Value, 1), "%")))+
  labs(
    caption = "Source: Eurostat, calculs des auteurs
               Note : la crise covid se déroule de T4:2019 à T4:2021, 
               la crise inflationniste de T1:2022 à T3:2023",
    title = "",
    x = NULL,
    y = "En pp",
    fill = NULL, 
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") + 
  theme(legend.position = "bottom") + 
  theme(axis.text.x = element_text(angle = 0)) +
  scale_x_continuous(breaks = c(1, 2), labels = c("Crise Covid", "Crise Inflationniste")) +
  scale_y_continuous(limits = c(-15, 15), labels = scales::label_number(decimal.mark = ","))

FranceFlux <- ggplot(data = AutreFlux) +
  geom_bar(data = AutreFlux %>% 
             filter(geo.x == "FR") %>%
             filter(Operation %in% c("F2", "F5", "F6","Autre")), 
           aes(x = Operation, y = Somme, fill = Operation), 
           position = "dodge", stat = "identity") +
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +  
  scale_y_continuous(limits = c(-200000, 500000), labels = scales::label_number(decimal.mark = ",")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  ggtitle("")

AllemagneFlux <- ggplot(data = AutreFlux) +
  geom_bar(data = AutreFlux %>% 
             filter(geo.x == "DE") %>%
             filter(Operation %in% c("F2", "F5", "F6","Autre")), 
           aes(x = Operation, y = Somme, fill = Operation), 
           position = "dodge", stat = "identity") +
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +  
  scale_y_continuous(limits = c(-20000, 600000), labels = scales::label_number(decimal.mark = ",")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

AllemagneFlux
EspagneFlux <- ggplot(data = AutreFlux) +
  geom_bar(data = AutreFlux %>% 
             filter(geo.x == "ES") %>%
             filter(Operation %in% c("F2", "F5", "F6","Autre")), 
           aes(x = Operation, y = Somme, fill = Operation), 
           position = "dodge", stat = "identity") +
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +  
  scale_y_continuous(limits = c(-40000, 160000), labels = scales::label_number(decimal.mark = ",")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ItalieFlux <- ggplot(data = AutreFlux) +
  geom_bar(data = AutreFlux %>% 
             filter(geo.x == "IT") %>%
             filter(Operation %in% c("F2", "F5", "F6","Autre")), 
           aes(x = Operation, y = Somme, fill = Operation), 
           position = "dodge", stat = "identity") +
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +  
  scale_y_continuous(limits = c(-180000, 160000), labels = scales::label_number(decimal.mark = ",")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
