install.packages("svglite")

install.packages("ggsci")
install.packages("palmerpenguins")
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

webstat_client_ID <- "523de456-638c-4ac1-a31e-7c466d1329d0"

PELencours <- w_data(dataset_name = "BSI1", series_name = "BSI1.M.FR.N.A.L22FRPL.A.1.U6.2251.Z01.E")
PELvariation <- w_data(dataset_name = "BSI1", series_name = "BSI1.M.FR.N.A.L22FRPL.A.4.U6.2251.Z01.E")
CELencours<- w_data(dataset_name = "BSI1", series_name = "BSI1.M.FR.N.A.L23FRCL.A.1.U6.2251.Z01.E")
CELvariation <- w_data(dataset_name = "BSI1", series_name = "BSI1.M.FR.N.A.L23FRCL.A.4.U6.2251.Z01.E")
LEPencours <- w_data(dataset_name = "BSI1", series_name = "BSI1.M.FR.N.A.L23FRLP.A.1.U6.2251.Z01.E")
LEPvariation <- w_data(dataset_name = "BSI1", series_name = "BSI1.M.FR.N.A.L23FRLP.A.4.U6.2251.Z01.E")

Epargne1 <- inner_join(inner_join(PELencours, PELvariation, by ="date"), CELencours, by = "date")%>%
  rename( PELencours = "BSI1.M.FR.N.A.L22FRPL.A.1.U6.2251.Z01.E",
          PELvariation = "BSI1.M.FR.N.A.L22FRPL.A.4.U6.2251.Z01.E",
          CELencours = "BSI1.M.FR.N.A.L23FRCL.A.1.U6.2251.Z01.E")

Epargne2 <- inner_join(inner_join(CELvariation, LEPencours, by ="date"), LEPvariation, by = "date")%>%
  rename( CELvariation = "BSI1.M.FR.N.A.L23FRCL.A.4.U6.2251.Z01.E",
          LEPencours = "BSI1.M.FR.N.A.L23FRLP.A.1.U6.2251.Z01.E",
          LEPvariation = "BSI1.M.FR.N.A.L23FRLP.A.4.U6.2251.Z01.E")

Epargne <- inner_join(Epargne1, Epargne2, by ="date")%>%
  mutate(
    PEL_avg = mean(ifelse(year(date) == 2019, PELencours, NA), na.rm = TRUE),
    LEP_avg = mean(ifelse(year(date) == 2019, LEPencours, NA), na.rm = TRUE),
    CEL_avg = mean(ifelse(year(date) == 2019, CELencours, NA), na.rm = TRUE),
    PEL_index = (PELencours / PEL_avg) * 100,  # Convert to percentage scale
    LEP_index = (LEPencours / LEP_avg) * 100,
    CEL_index = (CELencours / CEL_avg) * 100
  )


Epargne$date <- as.Date(Epargne$date)
GraphEpargne <- ggplot(data = Epargne) +
  geom_line(aes(x = date, y = PEL_index, color = "PEL_index"), show.legend = TRUE) +
  geom_line(aes(x = date, y = LEP_index, color = "LEP_index"), show.legend = TRUE) +
  geom_line(aes(x = date, y = CEL_index, color = "CEL_index"), show.legend = TRUE) +
  labs(
    caption = "Source: Banque de France",
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  theme(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("1999-01-01", max(Epargne$date))), expand = c(0, 0)) +
  scale_y_continuous(limits = c(40, 160))+ #labels = scales::label_number(scale = 1e-3, suffix = "k")) +
  scale_color_manual(
    name = "Variables",
    values = c("green", "red", "blue"), # Adjust colors as needed
    labels = c("CEL", "LEP", "PEL") # Names for the legend
  ) +
  ggtitle("Encours d'Epargne selon le type, Indice Base 100 = mpyenne de l'année 2019")


GraphEpargne

GraphEpargne<- ggplot() +
  geom_bar(data=EndettementGraph, aes(x = Variable1, y = Contribution, fill = Variable),position = "stack", stat = "identity") +  
  geom_point(data = EndettementGraphTotal,  aes(x = Variable1, y = Contribution, color = "Total"), size = 4.5) + 
  geom_text(data = EndettementGraphTotal,  aes(x = Variable1, y = Contribution, label = signif(Contribution, 2)), size = 2, color = "white") +# Create a stacked bar chart
  labs(
    caption = "Source: Banque de France
              Note: La période correspondant à la Grande Crise Financière (GFC) va du mois de septembre 2009 à celui de décembre 2012, la période Covid est celle comprise entre février 2020 et mars 2021. Enfin la période post Covid correspond aux mois entre celui d'avril 2021 et la dernière donnée disponible en août 2023.",
    title = "",
    x = "",
    y = "Contribution en pourcentage à la croissance totale"
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial", size= 8.5)) +
  theme(legend.position = "bottom") +  
  scale_y_continuous(limits = c(-4, 13), labels = scales::label_number(decimal.mark = ",")) +
  scale_color_manual(name = "", values = c("Total" = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_manual(values = c("#DADAEB", "#6A51A3","#9E9AC8", "#E7298A", "#66A61E" ))+ # Rotate x-axis labels for better readability
  ggtitle("Variation de l'endettement des Sociétés Non Financières pendant les crises financières et Covid")+
  guides(fill = guide_legend(title = NULL))


ggsave(filename = "C:/Users/153003/Documents/Entreprise/GraphEndettement.svg", plot = GraphEndettement, width = 8, height = 6, units = "in")
