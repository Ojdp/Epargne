
install.packages("ggsci")
install.packages("dplyr")
install.packages("svglite")
install.packages("ofce")
install.packages("patchwork")

library(ggsci)
library(rwebstat)
library(svglite)
library(openxlsx)
library(data.table)
library(dplyr)
library(wCorr)
library(magrittr)
library(WeightIt)
library(stringr)
library(ggplot2)
library(ggpubr)
library(pins)
library(fst)
library(fs)
library(kableExtra)
library(tidyr)
library(ofce)
library(patchwork)
library(zoo)
#MIR1: indentifiant du jeu de données, nom du dataset et ce qui suit c la clé 
#Crédit = Mensuelle, France, Etablissements de crédit et autres institutions financières, crédit, Toutes maturités, flux mensuels cumulés sur un an , Tous montants, SNF résidentes, euro, contrats nouveaux
#Endettement = Mensuel, France, Brut, Endettement, Total, Indices notionnels des stocks, Résidents, Sociétés non financières (S11), Toutes monnaies confondues, Taux de croissance annuel

webstat_client_ID <- "523de456-638c-4ac1-a31e-7c466d1329d0"





#Par Déciles : France, Ménages, Patrimoine Net, Déciles de Patrimoine Net, Par ménage

TotalMoyen<- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.N.LE.NWA._Z.EUR_R_NH.S.N")
TotalD10<- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.N.LE.NWA.D10.EUR_R_NH.S.N")
TotalD9 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.N.LE.NWA.D9.EUR_R_NH.S.N")
TotalD8<- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.N.LE.NWA.D8.EUR_R_NH.S.N")
TotalD7 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.N.LE.NWA.D7.EUR_R_NH.S.N")
TotalD6 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.N.LE.NWA.D6.EUR_R_NH.S.N")
TotalInf50 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.N.LE.NWA.B50.EUR_R_NH.S.N")

FinancierD10<- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.A.LE.F51M.D10.EUR_R_NH.S.N")
FinancierD9 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.A.LE.F51M.D9.EUR_R_NH.S.N")
FinancierD8<- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.A.LE.F51M.D8.EUR_R_NH.S.N")
FinancierD7 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.A.LE.F51M.D7.EUR_R_NH.S.N")
FinancierD6 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.A.LE.F51M.D6.EUR_R_NH.S.N")
FinancierInf50 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.A.LE.F51M.B50.EUR_R_NH.S.N")

ImmobilierD10<- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.A.LE.NUN.D10.EUR_R_NH.S.N")
ImmobilierD9 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.A.LE.NUN.D9.EUR_R_NH.S.N")
ImmobilierD8<- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.A.LE.NUN.D8.EUR_R_NH.S.N")
ImmobilierD7 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.A.LE.NUN.D7.EUR_R_NH.S.N")
ImmobilierD6 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.A.LE.NUN.D6.EUR_R_NH.S.N")
ImmobilierInf50 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.A.LE.NUN.B50.EUR_R_NH.S.N")

AssuranceVieD10 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.A.LE.F62.D10.EUR_R_NH.S.N")
AssuranceVieD9 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.A.LE.F62.D9.EUR_R_NH.S.N")
AssuranceVieD8<- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.A.LE.F62.D8.EUR_R_NH.S.N")
AssuranceVieD7 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.A.LE.F62.D7.EUR_R_NH.S.N")
AssuranceVieD6 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.A.LE.F62.D6.EUR_R_NH.S.N")
AssuranceVieInf50 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.A.LE.F62.B50.EUR_R_NH.S.N")

ActionscoteesD10 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.A.LE.F511.D10.EUR_R_NH.S.N")
ActionscoteesD9 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.A.LE.F511.D9.EUR_R_NH.S.N")
ActionscoteesD8<- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.A.LE.F511.D8.EUR_R_NH.S.N")
ActionscoteesD7 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.A.LE.F511.D7.EUR_R_NH.S.N")
ActionscoteesD6 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.A.LE.F511.D6.EUR_R_NH.S.N")
ActionscoteesInf50 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.A.LE.F511.B50.EUR_R_NH.S.N")

OPCD10 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.A.LE.F52.D10.EUR_R_NH.S.N")
OPCD9 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.A.LE.F52.D9.EUR_R_NH.S.N")
OPCD8<- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.A.LE.F52.D8.EUR_R_NH.S.N")
OPCD7 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.A.LE.F52.D7.EUR_R_NH.S.N")
OPCD6 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.A.LE.F52.D6.EUR_R_NH.S.N")
OPCInf50 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.A.LE.F52.B50.EUR_R_NH.S.N")

DepotD10 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.A.LE.F2M.D10.EUR_R_NH.S.N")
DepotD9 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.A.LE.F2M.D9.EUR_R_NH.S.N")
DepotD8<- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.A.LE.F2M.D8.EUR_R_NH.S.N")
DepotD7 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.A.LE.F2M.D7.EUR_R_NH.S.N")
DepotD6 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.A.LE.F2M.D6.EUR_R_NH.S.N")
DepotInf50 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.A.LE.F2M.B50.EUR_R_NH.S.N")

TotalPassifD10 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.L.LE.F_NNA.D10.EUR_R_NH.S.N")
TotalPassifD9 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.L.LE.F_NNA.D9.EUR_R_POP.S.N")
TotalPassifD8<- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.L.LE.F_NNA.D8.EUR_R_POP.S.N")
TotalPassifD7 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.L.LE.F_NNA.D7.EUR_R_POP.S.N")
TotalPassifD6 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.L.LE.F_NNA.D6.EUR_R_NH.S.N")
TotalPassifInf50 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.L.LE.F_NNA.B50.EUR_R_NH.S.N")

TotalActifD10 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.A.LE.F_NNA.D10.EUR_R_POP.S.N")
TotalActifD9 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.A.LE.F_NNA.D9.EUR_R_POP.S.N")
TotalActifD8<- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.A.LE.F_NNA.D8.EUR_R_POP.S.N")
TotalActifD7 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.A.LE.F_NNA.D7.EUR_R_POP.S.N")
TotalActifD6 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.A.LE.F_NNA.D6.EUR_R_POP.S.N")
TotalActifInf50 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.A.LE.F_NNA.B50.EUR_R_POP.S.N")

TitredeCreanceD10 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.A.LE.F3.D10.EUR_R_POP.S.N")
TitredeCreanceD9 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.A.LE.F3.D9.EUR_R_POP.S.N")
TitredeCreanceD8<- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.A.LE.F3.D8.EUR_R_POP.S.N")
TitredeCreanceD7 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.A.LE.F3.D7.EUR_R_POP.S.N")
TitredeCreanceD6 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.A.LE.F3.D6.EUR_R_POP.S.N")
TitredeCreanceInf50 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.A.LE.F3.B50.EUR_R_POP.S.N")

EmpruntImmobilierD10 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.L.LE.F4B.D10.EUR_R_NH.S.N")
EmpruntImmobilierD9 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.L.LE.F4B.D9.EUR_R_POP.S.N")
EmpruntImmobilierD8<- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.L.LE.F4B.D8.EUR_R_NH.S.N")
EmpruntImmobilierD7 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.L.LE.F4B.D7.EUR_R_POP.S.N")
EmpruntImmobilierD6 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.L.LE.F4B.D6.EUR_R_NH.S.N")
EmpruntImmobilierInf50 <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.L.LE.F4B.B50.EUR_R_NH.S.N")

#par tete
Salarie <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.N.LE.NWA.WSE.EUR_R_POP.S.N")
Retraité <- w_data(dataset_name = "DWA1", series_name = "DWA1.Q.FR.S14.N.LE.NWA.WSR.EUR_R_POP.S.N")


data_frames_list <- list(
  TotalD10,TotalD9,TotalD8,TotalD7,TotalD6,TotalInf50,TotalMoyen,
  FinancierD10,FinancierD6,FinancierD7,FinancierD8,FinancierD9,FinancierInf50,
  ImmobilierD10, ImmobilierD9, ImmobilierD8,ImmobilierD7,ImmobilierD6,ImmobilierInf50,
  AssuranceVieD10,AssuranceVieD9,AssuranceVieD8,AssuranceVieD7,AssuranceVieD6,AssuranceVieInf50,
  ActionscoteesD10, ActionscoteesD9,ActionscoteesD8,ActionscoteesD7,ActionscoteesD6,ActionscoteesInf50,
  OPCD10,OPCD9,OPCD8,OPCD7,OPCD6,OPCInf50,
  DepotD10,DepotD9,DepotD8,DepotD7,DepotD6,DepotInf50,
  TotalPassifD10,TotalPassifD9,TotalPassifD8,TotalPassifD7,TotalPassifD6,TotalPassifInf50,
  TotalActifD10,TotalActifD9,TotalActifD8,TotalActifD7,TotalActifD6,TotalActifInf50,
  TitredeCreanceD10,TitredeCreanceD9,TitredeCreanceD8,TitredeCreanceD7,TitredeCreanceD6,TitredeCreanceInf50,
  EmpruntImmobilierD10,EmpruntImmobilierD9,EmpruntImmobilierD8,EmpruntImmobilierD7,EmpruntImmobilierD6,EmpruntImmobilierInf50)


# Get the names of the dataframes
df_names <- c("TotalD10","TotalD9","TotalD8","TotalD7","TotalD6","TotalInf50","TotalMoyen",
              "FinancierD10","FinancierD6","FinancierD7","FinancierD8","FinancierD9","FinancierInf50",
              "ImmobilierD10", "ImmobilierD9", "ImmobilierD8","ImmobilierD7","ImmobilierD6","ImmobilierInf50",
              "AssuranceVieD10","AssuranceVieD9","AssuranceVieD8","AssuranceVieD7","AssuranceVieD6","AssuranceVieInf50",
              "ActionscoteesD10", "ActionscoteesD9","ActionscoteesD8","ActionscoteesD7","ActionscoteesD6","ActionscoteesInf50",
              "OPCD10","OPCD9","OPCD8","OPCD7","OPCD6","OPCInf50", 
              "DepotD10","DepotD9","DepotD8","DepotD7","DepotD6","DepotInf50",
              "TotalPassifD10","TotalPassifD9","TotalPassifD8","TotalPassifD7","TotalPassifD6","TotalPassifInf50",
              "TotalActifD10","TotalActifD9","TotalActifD8","TotalActifD7","TotalActifD6","TotalActifInf50",
              "TitredeCreanceD10","TitredeCreanceD9","TitredeCreanceD8","TitredeCreanceD7","TitredeCreanceD6","TitredeCreanceInf50",
              "EmpruntImmobilierD10","EmpruntImmobilierD9","EmpruntImmobilierD8","EmpruntImmobilierD7","EmpruntImmobilierD6","EmpruntImmobilierInf50")


data_EpargneParDec <- Reduce(function(x, y) merge(x, y, by = "date", all = TRUE), data_frames_list)
colnames(data_EpargneParDec)[-1] <- df_names

data_EpargneParDec <- data_EpargneParDec %>%
  mutate(
    TotalMenage = rowSums(select(., c("TotalD10","TotalD9","TotalD8","TotalD7","TotalD6","TotalInf50")), na.rm = TRUE)
  )

data_EpargneParDec <- data_EpargneParDec %>%
  pivot_longer(cols = -date, names_to = "Operation", values_to = "Value") %>%
  mutate(date = as.POSIXct(date))

data_EpargneParDec <- data_EpargneParDec %>%
  group_by(Operation) %>%
  mutate(
    CroissanceTrim = (Value - lag(Value, n = 1)) / lag(Value, n = 1) * 100,
    CroissanceAnnuel = (Value - lag(Value, n = 4)) / lag(Value, n = 4) * 100,
    Covid = Value - Value[date == "2019-10-01"])%>%
  group_by(date)%>%
  mutate(Part = (Value / sum(Value[Operation == "TotalMenage"])) * 100)


PatrimoineTotal <- ggplot(data = data_EpargneParDec) +
  geom_line(data = data_EpargneParDec|> filter(Operation %in% c("TotalD10","TotalD9","TotalD8","TotalD7","TotalD6","TotalInf50")), aes(x = as.Date(date), y = Value, color = Operation), show.legend = TRUE) +
  labs(
    caption = "Source: Banque de France",
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2013-01-01", max(data_EpargneParDec$date))), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 2700), labels = scales::label_number(decimal.mark = ",")) +
  ggtitle("Patrimoine des ménages, en Milliards ")


ggsave(filename = "C:/Users/153003/Documents/Epargne/PatrimoineTotal.png", plot = PatrimoineTotal, width = 8, height = 6, units = "in")


TotalCroissance<- ggplot(data = data_EpargneParDec) +
  geom_line(data = data_EpargneParDec|> filter(Operation %in% c("TotalD10","TotalD9","TotalD8","TotalD7","TotalD6","TotalInf50")), aes(x = as.Date(date), y = CroissanceAnnuel, color = Operation), show.legend = TRUE) +
  labs(
    caption = "Source: Banque de France",
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2013-01-01", max(data_EpargneParDec$date))), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-3, 11), labels = scales::label_number(decimal.mark = ",")) +
  ggtitle("Patrimoine des ménages, Taux de croissance annuel")

ggsave(filename = "C:/Users/153003/Documents/Epargne/TotalCroissance.png", plot = TotalCroissance, width = 8, height = 6, units = "in")


ggplot(data = data_EpargneParDec) +
  geom_bar(data = data_EpargneParDec %>%
             filter(Operation %in% c("TotalD10","TotalD9","TotalD8","TotalD7","TotalD6","TotalInf50")),
           aes(x = as.Date(date), y = Value, fill = Operation), position = "stack", stat = "identity") + 
  geom_line(data = data_EpargneParDec %>% filter(Operation == "Total"),
            aes(x = as.Date(date), y = Value), show.legend = TRUE) +
  geom_point(data = data_EpargneParDec %>% filter(Operation == "Total"),
             aes(x = as.Date(date), y = Value), color = "black", size = 3) +
  labs(
    title = "Contributions to Growth",
    x = "Date",
    y = "En %"
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +  
  scale_x_date(limits = as.Date(c("2018-04-01", "2023-07-01")), date_labels = "%Y-%m-%d") +
  scale_y_continuous(limits = c(0, 4500), labels = scales::label_number(decimal.mark = ",")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  ggtitle("Patrimoine des français par décile, par ménage")

graph_5<- ggplot(data = data_EpargneParDec) +
  geom_bar(data = data_EpargneParDec %>%
             filter(Operation %in% c("TotalD10","TotalD9","TotalD8","TotalD7","TotalD6","TotalInf50")),
           aes(x = as.Date(date), y = Covid, fill = Operation), position = "stack", stat = "identity") + 
  geom_line(data = data_EpargneParDec %>% filter(Operation == "Total"),
            aes(x = as.Date(date), y = Covid), show.legend = TRUE) +
  geom_point(data = data_EpargneParDec %>% filter(Operation == "Total"),
             aes(x = as.Date(date), y = Covid), color = "black", size = 3) +
  labs(
    title = "Contributions to Growth",
    x = "",
    y = "En Milliards",
    caption = "Source: Banque de France",
    
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 11.25, face = "bold"))  +  
  scale_x_date(limits = as.Date(c("2020-01-01", "2023-10-01")), date_labels = "%Y-%m-%d") +
  scale_y_continuous(limits = c(0, 550), labels = scales::label_number(decimal.mark = ",")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  ggtitle("Patrimoine des français par décile, par ménage, en écart à T4:2019")

ggsave(filename = "C:/Users/153003/Documents/Epargne/graph_5.png", plot = graph_5, width = 8, height = 6, units = "in")

ggplot(data = data_EpargneParDec) +
  geom_line(data = data_EpargneParDec|> filter(Operation %in% c("TotalD10")), aes(x = as.Date(date), y = Part, color = Operation), show.legend = TRUE) +
  labs(
    caption = "Source: Banque de France",
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2013-01-01", max(data_EpargneParDec$date))), expand = c(0, 0)) +
  scale_y_continuous(limits = c(50, 57), labels = scales::label_number(decimal.mark = ",")) +
  ggtitle("Patrimoine des ménages, Taux de croissance annuel")