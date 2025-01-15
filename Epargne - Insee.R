

library(ofce)
library(tidyverse)
library(readxl)
library(insee)
library(zoo) 
library(openxlsx)
library(gt)




#Paramètres séries à garder : Epargne non finanicère des ménages 


dataEpargnenonFi <-get_insee_dataset("CNA-2014-PAT-NF")%>%
  filter(SECT_INST == "S14")%>%
  filter(UNIT_MEASURE == "EUROS_COURANTS") %>%
  split_title()%>%
  select(DATE,OBS_VALUE,OPERATION_label_fr, TYPE_FLUX_label_fr)%>%
  arrange(DATE)%>%
  pivot_wider(names_from = TYPE_FLUX_label_fr, values_from = OBS_VALUE, names_sep = "_")%>%
  select(-`Encours brut en fin de période (valeur de marché)`)%>%
  rename('Autres changements de volume'= 'Autres changements de volume et ajustements')




#faut rajouter l'année 2022 à la main avec le fichier Excel

#Paramètres séries à garder : Epargne finanicère des ménages 

source<-"CNA-2014-TOF"

bases<-get_dataset_list()

idbank_list <- get_idbank_list(source)

variable<-"S14"
naf<-"EUROS_COURANTS"
place <-c("EA","RP")

testCommerce<-idbank_list%>%
  filter(SECT_INST %in% variable,
         UNIT_MEASURE==naf,
         COMPTE %in% place)%>%
  select(idbank)%>%
  pull(idbank)


dataEpargneFi =
  get_insee_idbank(testCommerce) %>%
  split_title()%>%
  add_insee_metadata()%>%
  select(DATE,OBS_VALUE,OPERATION_label_fr, TYPE_FLUX_label_fr, COMPTE_label_fr)%>%
  arrange(DATE)%>%
  pivot_wider(names_from = TYPE_FLUX_label_fr, values_from = OBS_VALUE, names_sep = "_")%>%
  mutate('Consommation de capital fixe' = NA)

dataEpargne <- bind_rows (dataEpargneFi, dataEpargnenonFi)%>%
  arrange(DATE)%>%
  rename(`Réévaluation` = `Réévaluations (gains ou pertes de détention)`,
         `Patrimoine en fin d'année` =`Encours en fin de période (valeur de marché)`,
         `Flux d'actifs`=`Acquisitions moins cessions` )

file_path <- "C:/Users/153003/Documents/Classeur2.xlsx"

excel_data <- read_excel(file_path, col_names = TRUE)%>%
  mutate(OPERATION_label_fr = paste(IANSTR_ASSET, "-", `Classe d'actifs passifs`), 
         DATE = as.Date("2022-01-01"))%>%
  select(-c(IANSTR_ASSET, `Classe d'actifs passifs`))

diff_values <- setdiff(excel_data$OPERATION_label_fr, dataEpargne$OPERATION_label_fr)
print(diff_values)


EpargneUp <- bind_rows (dataEpargne, excel_data)



# Replace "local_file.xlsx" with the actual path to your downloaded file


data.raw <- readxl::read_xlsx("immo/T_8220.xlsx", skip=4, sheet="T_8220")

items <- data.raw |> slice(1) |> unlist() |> map(~case_when(
  str_detect(.x , "Patrimoine") ~ "w", 
  str_detect(.x , "Flux") ~ "i",
  str_detect(.x , "Consommation") ~ "ccf",
  str_detect(.x , "Réévaluation") ~ "reev",
  str_detect(.x , "Autres") ~ "aut",))

nn <- names(items) |> str_sub(1, 4)
items <- str_c(items, "_", nn)

data <- data.raw
names(data) <-c("code", "label", tail(items, -2))

data <- data |> 
  slice(-1, -2) |> 
  filter(!is.na(code), !str_detect(code, "Source")) |> 
  group_by(code) |> 
  mutate(
    n = n(),
    type = ifelse(n==1, "actif", c("actif", "passif"))) |> 
  ungroup() |> 
  relocate(type, code, label) |> 
  select(-n)

t8220 <- data |> pivot_longer(cols = -c(code, label, type)) |> 
  separate(col=name, into = c("op", "y"), sep = "_") |> 
  mutate(value = replace_na(as.numeric(value), 0),
         y = as.Date(str_c(y, "-12-31"))) |> 
  pivot_wider(id_cols = c(code, y, label, type), names_from = op, values_from = value)

pat_nf_20 <- t8220 |> 
  rename(enc = w, flux = i, acvaj = aut) |> 
  mutate(across(c(enc, ccf, flux, reev, acvaj), ~.x*1000)) |>
  # note : N111 : logement, N2111 : terrains résidentiels, N13 : objets de valeur, N1131 : matériel et équipement
  filter(code %in% c("N111", "N2111", "N13", "N1131") ) |> 
  mutate(code  = case_match(code,
                            "N111" ~ "lgt",
                            "N2111" ~ "ter",
                            "N13" ~ "objv",
                            "N1131" ~ "mat")) |> 
  select(-label, -type) |> 
  pivot_wider(names_from = c(code), values_from = c(enc, flux, reev, acvaj, ccf), names_glue = "{code}_{.value}") |> 
  rename(DATE=y) |> 
  arrange(desc(DATE)) 

print("---------------------------")
# Extract the base columns
base <- excel_data[c(1, 2)]

# Initialize df_long with the base columns and the columns for the first year

df_long <- cbind(base, excel_data[4:8])

# Loop to append data for each subsequent year
for (i in 2:45) {
  iLeft <- 4 + (i - 1) * 5
  iRight <- 4 + i * 5 - 1
  year <- cbind(base, excel_data[iLeft:iRight])
  colnames(year) <- colnames(df_long)  # Ensure column names match
  df_long <- rbind(df_long, year)
}

titles <- c("Index",
  "Operation",
  "Flux d’actifs",
  "Consommation de capital fixe",
  "Réévaluation",
  "Autres changements de volume et ajustements",
  "Patrimoine en fin d'année"
)

colnames(df_long)[1:7] <- titles
df_long <- df_long[!(df_long$Operation %in% c("Opération comptable", "Annuel","Classe d'actifs passifs")), , drop = FALSE]

df_long <- df_long %>%
  mutate(DATE = as.Date("1979-01-01") + lubridate::years(floor((row_number() - 1) / 53))) %>%
  select(DATE, everything())

columns_to_check <- c(
  "Flux d’actifs",
  "Consommation de capital fixe",
  "Réévaluation",
  "Autres changements de volume et ajustements",
  "Patrimoine en fin d'année"
)

na_count_per_row <- rowSums(is.na(df_long[, columns_to_check]))
df_long <- df_long[na_count_per_row != 5, ]
  
filtre.produit<-c("Total des actifs financiers", "Total des actifs non financiers")


EpargneTotal <-ggplot(df_long %>%
         filter(Operation %in% filtre.produit, DATE == as.Date("2023-01-01")) %>%
         rename(Patrimoine = `Patrimoine en fin d'année`) %>%
         mutate(Patrimoine = as.numeric(as.character(Patrimoine))),
       aes(x = 2, y = Patrimoine, fill = Operation)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0) +
  geom_text(aes(x = 2, y = sum(Patrimoine), label = sum(Patrimoine)), color = "black", size = 4) +  # Centered label
  geom_text(aes(x = 2, y = Patrimoine, label = round(Patrimoine, digit = 1)), color = "black", position = position_stack(vjust = 0.5)) +  # Centered label
  scale_fill_manual(values = c("Total des actifs financiers" = "red", "Total des actifs non financiers" = "blue")) +
  theme_void() +
  xlim(0.5, 2.5) +  # Adjusted x-axis limits for better visualization
  ggtitle("Epargne des ménages en 2022")

EpargneTotal

Epargnefinancière <- ggplot(df_long %>%
         rename(Patrimoine = `Patrimoine en fin d'année`) %>%
        filter(Operation %in% c("Numéraire et dépôts", "Actions et parts de fonds d’investissement", "Systèmes d’assurance, de pension et de garanties standard"), DATE == as.Date("2022-01-01")) %>%
      mutate(Patrimoine = as.numeric(as.character(Patrimoine))),
            aes(x = 2, y = Patrimoine, fill = Operation)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0) +
  geom_text(aes(x = 2, y = sum(Patrimoine), label = sum(Patrimoine)), color = "black", size = 4) +  # Centered label
  geom_text(aes(x = 2, y = Patrimoine, label = round(Patrimoine, digit = 1)), color = "black", position = position_stack(vjust = 0.5)) +  # Centered label
  scale_fill_manual(values = c("Numéraire et dépôts" = "red", "Actions et parts de fonds d’investissement" = "blue","Systèmes d’assurance, de pension et de garanties standard" = "grey" )) +
  theme_void() +
  xlim(0.5, 2.5) +  # Adjusted x-axis limits for better visualization
  ggtitle("Epargne financière des ménages en 2022")


Epargnenonfinancière <-ggplot(df_long %>%
         filter(Operation %in% c("    Logements", "      Terrains supportant des bâtiments et des ouvrages de génie civil"), DATE == as.Date("2022-01-01")) %>%
         rename(Patrimoine = `Patrimoine en fin d'année`) %>%
         mutate(Patrimoine = as.numeric(as.character(Patrimoine)),
                Operation = ifelse(Operation == "      Terrains supportant des bâtiments et des ouvrages de génie civil", "Terrains bâtis", Operation),
                Operation = ifelse(Operation == "    Logements", "Logements", Operation)),
       aes(x = 2, y = Patrimoine, fill = Operation)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0) +
  geom_text(aes(x = 2, y = sum(Patrimoine), label = sum(Patrimoine)), color = "black", size = 4) +  # Centered label
  geom_text(aes(x = 2, y = Patrimoine, label = round(Patrimoine, digit = 1)), color = "black", position = position_stack(vjust = 0.5)) +  # Centered label
  scale_fill_manual(values = c("Logements" = "red", "Terrains bâtis" = "blue")) +
  theme_void() +
  xlim(0.5, 2.5) +  # Adjusted x-axis limits for better visualization
  ggtitle("Epargne non financière des ménages en 2022")


Epargnetotal2022 <-ggplot(df_long %>%
         filter(Operation %in% c("    Logements", "      Terrains supportant des bâtiments et des ouvrages de génie civil","Numéraire et dépôts", "Actions et parts de fonds d’investissement", "Systèmes d’assurance, de pension et de garanties standard"), DATE == as.Date("2022-01-01")) %>%
         rename(Patrimoine = `Patrimoine en fin d'année`) %>%
         mutate(Patrimoine = as.numeric(as.character(Patrimoine)),
                Operation = ifelse(Operation == "      Terrains supportant des bâtiments et des ouvrages de génie civil", "Terrains bâtis", Operation),
                Operation = ifelse(Operation == "    Logements", "Logements", Operation)),
       aes(x = 2, y = Patrimoine, fill = Operation)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0) +
  geom_text(aes(x = 2, y = sum(Patrimoine)/10, label = sum(Patrimoine)), color = "black", size = 4) +  # Centered label
  geom_text(aes(x = 2, y = Patrimoine, label = round(Patrimoine, digit = 1)), color = "black", position = position_stack(vjust = 0.5)) +  # Centered label
  scale_fill_manual(values = c("Logements" = "red", "Terrains bâtis" = "blue", "Numéraire et dépôts" = "pink", "Actions et parts de fonds d’investissement" = "orange","Systèmes d’assurance, de pension et de garanties standard" = "grey")) +
  theme_void() +
  xlim(0.5, 2.5) +  # Adjusted x-axis limits for better visualization
  ggtitle("Epargne des ménages en 2022")

EpargneTotal2019<- ggplot(df_long %>%
         filter(Operation %in% filtre.produit3, DATE == as.Date("2019-01-01")) %>%
         rename(Patrimoine = `Patrimoine en fin d'année`) %>%
         mutate(Patrimoine = as.numeric(as.character(Patrimoine)),
                Operation = ifelse(Operation == "      Terrains supportant des bâtiments et des ouvrages de génie civil", "Terrains bâtis", Operation),
                Operation = ifelse(Operation == "    Logements", "Logements", Operation)),
       aes(x = 2, y = Patrimoine, fill = Operation)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0) +
  geom_text(aes(x = 2, y = sum(Patrimoine)/10, label = sum(Patrimoine)), color = "black", size = 4) +  # Centered label
  geom_text(aes(x = 2, y = Patrimoine, label = round(Patrimoine, digit = 1)), color = "black", position = position_stack(vjust = 0.5)) +  # Centered label
  scale_fill_manual(values = c("Logements" = "red", "Terrains bâtis" = "blue", "Numéraire et dépôts" = "pink", "Actions et parts de fonds d’investissement" = "orange","Systèmes d’assurance, de pension et de garanties standard" = "grey")) +
  theme_void() +
  xlim(0.5, 2.5) +  # Adjusted x-axis limits for better visualization
  ggtitle("Epargne des ménages en 2019")

#Investissement logement des ménages trim


dataEpargneTotal <- get_insee_dataset("CNT-2020-CSI") %>%
  filter(SECT_INST == "S14")%>%
  filter(OPERATION %in% c("B8G","B6", "B9NF"))%>%
  split_title() %>%
  add_insee_metadata() %>%
  select(DATE, OBS_VALUE, OPERATION_label_fr) %>%
  arrange(DATE) %>%
  pivot_wider(names_from = OPERATION_label_fr, values_from = OBS_VALUE, names_sep = "_") %>%
  rename(Epargne = `B8G - Épargne`, Revenudispo = `B6 - Revenu disponible`) %>%
  mutate(Taux = (Epargne / Revenudispo) * 100,
         EpargnenonfinancièreT = ((Epargne - `B9NF - Capacité (+) ou besoin (-) de financement, approche non financière`) / Revenudispo) * 100,
         EpargnefinancièreT = (`B9NF - Capacité (+) ou besoin (-) de financement, approche non financière` / Revenudispo) * 100,
         Epargnenonfinancière = (Epargne - `B9NF - Capacité (+) ou besoin (-) de financement, approche non financière`),
         Epargnefinancière = (`B9NF - Capacité (+) ou besoin (-) de financement, approche non financière`),
         trendPreCovid = mean(Taux[DATE >= "2010-01-01" & DATE <= "2019-10-01"]),
         trendPostCovid = mean(Taux[DATE >= "2020-01-01" & DATE <= "2024-04-01"]),
         EpargneMoyenne = (trendPreCovid * Revenudispo) / 100) %>%
  mutate(Surepargne = Epargne - EpargneMoyenne) %>%
  pivot_longer(cols = -DATE, names_to = "Operation", values_to = "Value") 
  
  
SurEpargne<- dataEpargneTotal %>% 
  filter(Operation == "Surepargne", DATE >= "2020-01-01", DATE <= "2024-04-01") %>%
  summarise(SurEpargneTotale = sum(Value))


dataOther <- dataEpargneTotal%>%
  filter(Operation %in% c("EpargnenonfinancièreT","EpargnefinancièreT")) %>%
  mutate(Operation = case_when(
    Operation == "EpargnefinancièreT" ~ "Epargne financière",
    Operation == "EpargnenonfinancièreT" ~ "Epargne non financière",
    TRUE ~ as.character(Operation)
  ))

dataOther$Operation <- factor(dataOther$Operation, levels = c("Epargne non financière", "Epargne financière"))

dataTaux <- dataEpargneTotal %>%
  filter(Operation == "Taux")


# Plotting
graphAlterEco <- ggplot() +
  geom_bar(data = dataOther, aes(x = as.Date(DATE), y = Value, fill = Operation), position = "stack", stat = "identity") +
  geom_line(data = dataTaux, aes(x = as.Date(DATE), y = Value, color = Operation), color="black",show.legend = FALSE) +
  geom_point(data = dataTaux, aes(x = as.Date(DATE), y = Value), color = "black", size = 2) +
  guides(colour="none")+
  
  labs(
    caption = "Source: INSEE, calculs des auteurs",
    y = NULL,
    x = NULL,
    color = NULL,  
    fill = NULL 
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2012-11-01", max(dataEpargneTotal$DATE))), expand = c(0, 0)) +
  annotate("text", x = as.Date("2017-01-01"), y = 17.5, label = "Tendance Pre Covid", size = 3.5, color = "red") +
  annotate("text", x = as.Date("2017-01-01"), y = 15, label = round(mean(dataTaux$Value[as.Date(dataTaux$DATE) >= "2010-10-01" & as.Date(dataTaux$DATE) <= "2019-07-01"]), digit = 1),
           size = 3, color = "red", vjust = -1.5, hjust = 0) +
  annotate("text", x = as.Date("2021-04-01"), y = 20, label = "Tendance Post Covid", size = 3.5, color = "red",
           vjust = -1, hjust = 0) +
  annotate("text", x = as.Date("2022-04-01"), y = 19, label = round(mean(dataTaux$Value[as.Date(dataTaux$DATE) >= "2020-01-01" & as.Date(dataTaux$DATE) <= "2024-04-01"]), digit = 1),
           size = 3, color = "red", vjust = -1, hjust = 0) +
  scale_y_continuous(limits = c(0, 27), labels = scales::label_number(decimal.mark = ",")) +
  ggtitle("Taux d'épargne des ménages, en % du revenu disponible")

ggsave(filename = "C:/Users/153003/Documents/Epargne/graphAlterEco.svg", plot = graphAlterEco, width = 15, height = 8, units = "in")
ggsave(filename = "C:/Users/153003/Documents/Epargne/graphAlterEco.png", plot = graphAlterEco, width = 8, height = 6, units = "in")

source<-"CNT-2014-OPERATIONS"

bases<-get_dataset_list()

idbank_list <- get_idbank_list(source)

variable<-"P51M"
place <-"A17-FZ"
prout <- "V"

testCommerce<-idbank_list%>%
  filter(OPERATION %in% variable,
         CNA_PRODUIT %in%  place,
         VALORISATION == prout)%>%
  select(idbank)%>%
  pull(idbank)

dataConstruction =
  get_insee_idbank(testCommerce) %>%
  split_title()%>%
  add_insee_metadata()%>%
  select(DATE,OBS_VALUE,OPERATION_label_fr)%>%
  arrange(DATE)%>%
  pivot_wider(names_from = OPERATION_label_fr, values_from = OBS_VALUE, names_sep = "_")


ggplot(data = dataConstruction) +
  geom_line(data = dataEpargneTotal, aes(x = as.Date(DATE), y = Taux), show.legend = TRUE) +
  labs(
    caption = "Source: INSEE",
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2013-01-01", max(dataEpargneTotal$DATE))), expand = c(0, 0)) +
  annotate("text", x = as.Date("2017-01-01"), y = 16, label = "Trend Pre Covid", size = 4, color = "red") +
  annotate("text", x = as.Date("2017-01-01"), y = 15, label = round(dataEpargneTotal$trendPreCovid, digit = 1), size = 4, color = "red") +
  annotate("text", x = as.Date("2022-04-01"), y = 20, label = "Trend Post Covid", size = 4, color = "red") +
  annotate("text", x = as.Date("2022-04-01"), y = 19, label = round(dataEpargneTotal$trendPostCovid, digit = 1), size = 4, color = "red") +
  scale_y_continuous(limits = c(12, 27), labels = scales::label_number(decimal.mark = ",")) +
  ggtitle("Taux d'épargne des ménages, en % du revenu disponible")

