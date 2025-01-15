#API Banque de France
#clé : 7MyyvJc-4YAD-cr
#valeur confidentielle : a2ff8f02132cd59c97bc2d082dfa5806


install.packages("ggsci")
install.packages("dplyr")
install.packages("svglite")
install.packages("ofce")
install.packages("patchwork")
install.packages("WeightIt")
install.packages("httr")  
install.packages("ggplot2")
# Install if you haven't already
library(httr)

library(ggsci)
library(svglite)
library(openxlsx)
library(data.table)
library(dplyr)
library(WeightIt)
library(stringr)
library(tidyr)
library(ofce)
library(patchwork)
library(zoo)
library(jsonlite)
library(insee)
library(ggplot2)

#MIR1: indentifiant du jeu de données, nom du dataset et ce qui suit c la clé 
#Crédit = Mensuelle, France, Etablissements de crédit et autres institutions financières, crédit, Toutes maturités, flux mensuels cumulés sur un an , Tous montants, SNF résidentes, euro, contrats nouveaux
#Endettement = Mensuel, France, Brut, Endettement, Total, Indices notionnels des stocks, Résidents, Sociétés non financières (S11), Toutes monnaies confondues, Taux de croissance annuel

headers <- add_headers(
  Authorization = "Apikey 82ce51865859143598789064e4b7bc60e0ba0307b5ee69ad79c7ba44",  
  accept = "application/json"
)


fetch_and_process_data <- function(url, headers = headers) {
  
  headers <- add_headers(
    Authorization = "Apikey 82ce51865859143598789064e4b7bc60e0ba0307b5ee69ad79c7ba44",  
    accept = "application/json"
  )
  
  # Make the HTTP request with SSL verification disabled
  response <- GET(url, headers, config(ssl_verifypeer = FALSE))
  
  # Check if the request was successful
  if (status_code(response) == 200) {
    # Extract JSON content
    content_json <- content(response, "text", encoding = "UTF-8")
    
    # Convert JSON to dataframe, select required columns, and sort by time_period in descending order
    data <- fromJSON(content_json, flatten = TRUE) %>%
      select(c(title_fr, time_period, obs_value)) %>%
      mutate(title_fr = sub(",.*", "", title_fr)) %>%
      mutate(time_period = case_when(
        grepl("-Q1", time_period) ~ paste0(sub("-Q1", "-01-01", time_period)),
        grepl("-Q2", time_period) ~ paste0(sub("-Q2", "-04-01", time_period)),
        grepl("-Q3", time_period) ~ paste0(sub("-Q3", "-07-01", time_period)),
        grepl("-Q4", time_period) ~ paste0(sub("-Q4", "-10-01", time_period))
      )) %>%
      
      # Convert to Date type
      mutate(time_period = as.Date(time_period)) %>%
      # Sort time_period in descending order
      arrange(time_period)
    new_column_name <- unique(data$title_fr)
    data <- data %>%
      rename(!!new_column_name := obs_value) %>%
      select(-title_fr) %>%
      rename(date = time_period)# Drop 'title_fr' column after renaming
    
    
    return(data)
  } else {
    # Handle errors
    cat("Request failed with status:", status_code(response), "\n")
    return(NULL)
  }
}




generate_url <- function(dataset_id, series_key = NULL) {
  base_url <- "https://webstat.banque-france.fr/api/explore/v2.1/catalog/datasets/observations/exports/json/"
  
  # Construct the query parameters
  if (!is.null(series_key)) {
    where_clause <- paste0("series_key+IN+%28%22", series_key, "%22%29")
  } else {
    stop("Either series_key or series_name must be provided.")
  }
  
  url <- paste0(base_url, "?where=", where_clause, "&order_by=-time_period_start")
  return(url)
}

# Generate URLs


url_AutreEpargne <- generate_url(dataset_id = "CFT", series_key = "CFT.Q.N.FR.W0.S1M.S1.N.A.LE.F29O.T._Z.XDC._T.S.V.N._T")
url_EpargneReglementee <- generate_url(dataset_id = "CFT", series_key = "CFT.Q.N.FR.W0.S1M.S1.N.A.LE.F29R.T._Z.XDC._T.S.V.N._T")
url_DepotBancairesRemunerees <- generate_url(dataset_id = "CFT", series_key = "CFT.Q.N.FR.W0.S1M.S1.N.A.LE.F29Z.T._Z.XDC._T.S.V.N._T")
url_NumerairesDepotsAVue <- generate_url(dataset_id = "CFT", series_key = "CFT.Q.N.FR.W0.S1M.S1.N.A.LE.F2A.T._Z.XDC._T.S.V.N._T")
url_Titresdecreancesdetenuesdirectement <- generate_url(dataset_id = "CFT", series_key = "CFT.Q.N.FR.W0.S1M.S1.N.A.LE.F3.T._Z.XDC._T.S.V.N._T")
url_Actionscotees <- generate_url(dataset_id = "CFT", series_key = "CFT.Q.N.FR.W0.S1M.S1.N.A.LE.F511._Z._Z.XDC._T.S.V.N._T")
url_Actionsnoncotees <- generate_url(dataset_id = "CFT", series_key = "CFT.Q.N.FR.W0.S1M.S1.N.A.LE.F51M._Z._Z.XDC._T.S.V.N._T")
url_OPCmonetaires <- generate_url(dataset_id = "CFT", series_key = "CFT.Q.N.FR.W0.S1M.S1.N.A.LE.F521._Z._Z.XDC._T.S.V.N._T")
url_AutresPlacements <- generate_url(dataset_id = "CFT", series_key = "CFT.Q.N.FR.W0.S1M.S1.N.A.LE.F522A._Z._Z.XDC._T.S.V.N._T")
url_Assuranceviesupport <- generate_url(dataset_id = "CFT", series_key = "CFT.Q.N.FR.W0.S1M.S1.N.A.LE.F62B._Z._Z.XDC._T.S.V.N._T")
url_Assurancevieunitésdecompte <- generate_url(dataset_id = "CFT", series_key = "CFT.Q.N.FR.W0.S1M.S1.N.A.LE.F62A._Z._Z.XDC._T.S.V.N._T")
url_Titresdecreancesdetenuesnondirectement <- generate_url(dataset_id = "CFT", series_key = "CFT.Q.N.FR.W2.S1M.S124.N.A.LE.F5222._Z._Z.XDC._T.S.V.N._T")
url_Produitsdefondspropes <- generate_url(dataset_id = "CFT", series_key = "CFT.Q.N.FR.W0.S1M.S1.N.A.LE.PDFP._Z._Z.XDC._T.S.V.N._T")
url_Produitsdetaux <- generate_url(dataset_id = "CFT", series_key = "CFT.Q.N.FR.W0.S1M.S1.N.A.LE.PDTX._Z._Z.XDC._T.S.V.N._T")
url_PrincipauxPlacementsfinanciers <- generate_url(dataset_id = "CFT", series_key = "CFT.Q.N.FR.W0.S1M.S1.N.A.LE.PPFI._Z._Z.XDC._T.S.V.N._T")

url_Reevaluation <- generate_url(dataset_id = "CFT", series_key = "CFT.Q._Z.FR.W0.S1M.S1.N.A.K.PPFI._Z._Z.XDC._T.S.V.N._T")
url_ContributionReevaluation <- generate_url(dataset_id = "CFT", series_key = "CFT.Q._Z.FR.W0.S1M.S1.N.A.K.PPFI._Z._Z.PC._T.S.V.GO1._T")
url_FluxTrim <- generate_url(dataset_id = "CFT", series_key = "CFT.Q.S.FR.W0.S1M.S1.N.A.F.PPFI._Z._Z.XDC._T.S.V.N._T")
url_CroissancePPF <- generate_url(dataset_id = "CFT", series_key = "CFT.Q.N.FR.W0.S1M.S1.N.A.LE.PPFI._Z._Z.PC._T.S.V.G1._T")
url_ContributionFluxTrim <- generate_url(dataset_id = "CFT", series_key = "CFT.Q.S.FR.W0.S1M.S1.N.A.F.PPFI._Z._Z.PC._T.S.V.GO1._T")


# For flux
url_DepotBancairesRemunereesF <- generate_url(dataset_id = "CFT", series_key = "CFT.Q.S.FR.W0.S1M.S1.N.A.F.F29Z.T._Z.XDC._T.S.V.N._T")
url_NumerairesDepotsAVueF <- generate_url(dataset_id = "CFT", series_key = "CFT.Q.S.FR.W0.S1M.S1.N.A.F.F2.T._Z.XDC._T.S.V.N._T")
url_ActionscoteesF <- generate_url(dataset_id = "CFT", series_key = "CFT.Q.S.FR.W0.S1M.S1.N.A.F.F511._Z._Z.XDC._T.S.V.N._T")
url_ActionsnoncoteesF <- generate_url(dataset_id = "CFT", series_key = "CFT.Q.S.FR.W0.S1M.S1.N.A.F.F51M._Z._Z.XDC._T.S.V.N._T")
url_AssuranceviesupportF <- generate_url(dataset_id = "CFT", series_key = "CFT.Q.S.FR.W0.S1M.S1.N.A.F.F62._Z._Z.XDC._T.S.V.N._T")


AutreEpargne <- fetch_and_process_data(url_AutreEpargne)
EpargneReglementee <- fetch_and_process_data(url_EpargneReglementee)
DepotBancairesRemunerees <- fetch_and_process_data(url_DepotBancairesRemunerees)
NumerairesDepotsAVue <- fetch_and_process_data(url_NumerairesDepotsAVue)
Titresdecreancesdetenuesdirectement <- fetch_and_process_data(url_Titresdecreancesdetenuesdirectement)
Actionscotees <- fetch_and_process_data(url_Actionscotees)
Actionsnoncotees <- fetch_and_process_data(url_Actionsnoncotees)
OPCmonetaires <- fetch_and_process_data(url_OPCmonetaires)
AutresPlacements <- fetch_and_process_data(url_AutresPlacements)
Assuranceviesupport <- fetch_and_process_data(url_Assuranceviesupport)
Assurancevieunitésdecompte <- fetch_and_process_data(url_Assurancevieunitésdecompte)
Titresdecreancesdetenuesnondirectement <- fetch_and_process_data(url_Titresdecreancesdetenuesnondirectement)
Produitsdefondspropes <- fetch_and_process_data(url_Produitsdefondspropes)
Produitsdetaux <- fetch_and_process_data(url_Produitsdetaux)
PrincipauxPlacementsfinanciers <- fetch_and_process_data(url_PrincipauxPlacementsfinanciers)

# For flux
DepotBancairesRemunereesF <- fetch_and_process_data(url_DepotBancairesRemunereesF)
NumerairesDepotsAVueF <- fetch_and_process_data(url_NumerairesDepotsAVueF)
ActionscoteesF <- fetch_and_process_data(url_ActionscoteesF)
ActionsnoncoteesF <- fetch_and_process_data(url_ActionsnoncoteesF)
AssuranceviesupportF <- fetch_and_process_data(url_AssuranceviesupportF)

Reevaluation <- fetch_and_process_data(url_Reevaluation)%>%
  rename(Reevaluation = 'Principaux placements financiers')

ContributionReevaluation <- fetch_and_process_data(url_ContributionReevaluation)%>%
  rename(ContributionReevaluation = 'Principaux placements financiers')

FluxTrim <- fetch_and_process_data(url_FluxTrim)%>%
  rename(FluxTrim = 'Principaux placements financiers')

CroissancePPF <- fetch_and_process_data(url_CroissancePPF)%>%
  rename(CroissancePPF = 'Principaux placements financiers')

ContributionFluxTrim <- fetch_and_process_data(url_ContributionFluxTrim)%>%
  rename(ContributionFluxTrim = 'Principaux placements financiers')


# List of data frames
data_frames_list <- list(
  AutreEpargne, EpargneReglementee, DepotBancairesRemunerees, NumerairesDepotsAVue,
  Titresdecreancesdetenuesdirectement,Actionscotees, Actionsnoncotees, OPCmonetaires, AutresPlacements,
  Assuranceviesupport, Assurancevieunitésdecompte, Titresdecreancesdetenuesnondirectement,
  Produitsdefondspropes, Produitsdetaux, PrincipauxPlacementsfinanciers)


# Get the names of the dataframes
df_names <- c("AutreEpargne", "EpargneReglementee", "DepotBancairesRemunerees",
              "NumerairesDepotsAVue", "Titresdecreancesdetenuesdirectement", "Actioncotees",
              "Actionsnoncotees", "OPCmonetaires", "AutresPlacements",
              "Assuranceviesupport", "Assurancevieunitésdecompte",
              "Titresdecreancesdetenuesnondirectement", "Produitsdefondspropes",
              "Produitsdetaux", "PrincipauxPlacementsfinanciers")

# Merge the data frames and set the column names

data_EpargneFi <- Reduce(function(x, y) merge(x, y, by = "date", all = TRUE), data_frames_list)
colnames(data_EpargneFi)[-1] <- df_names

data_EpargneFi <- data_EpargneFi %>%
  mutate(
    Autre = PrincipauxPlacementsfinanciers - rowSums(select(., c(NumerairesDepotsAVue, Actioncotees, Actionsnoncotees, Assuranceviesupport, DepotBancairesRemunerees)), na.rm = TRUE),
    Actions = Actioncotees+  Actionsnoncotees,
    NumerairesetDepot =  NumerairesDepotsAVue +  DepotBancairesRemunerees)
  

PPF <- inner_join(inner_join(inner_join(ContributionReevaluation, ContributionFluxTrim, by = "date"), CroissancePPF, by = "date"), FluxTrim, by = "date") %>%
  inner_join(Reevaluation, by = "date") %>%
  inner_join(PrincipauxPlacementsfinanciers, by = "date") %>%
  rename(PrincipauxPlacementsfinanciers = 'Principaux placements financiers')

PPF1 <- PPF %>%
  filter(date >= "2019-04-01" & date <= "2020-04-01")%>%
  mutate(CroissancePPF = (((PrincipauxPlacementsfinanciers - lag(PrincipauxPlacementsfinanciers))/ lag(PrincipauxPlacementsfinanciers)))*100,
         ContributionFluxTrim = ((FluxTrim / (PrincipauxPlacementsfinanciers - lag(PrincipauxPlacementsfinanciers))) * CroissancePPF),
         ContributionReevaluation = ((Reevaluation / (PrincipauxPlacementsfinanciers - lag(PrincipauxPlacementsfinanciers)))* CroissancePPF))%>%
  filter(date >= "2020-01-01" & date <= "2020-04-01")%>%
        mutate(SommePPF = sum(CroissancePPF),
         SommeFlux = sum(ContributionFluxTrim),
         SommeReevaluation = sum(ContributionReevaluation),
         Inflation = -2.8)

#inflation = moyenne de l'inflation sur le trim
PPF2 <- PPF %>%
  filter(date >= "2020-07-01" & date <= "2021-10-01")%>%
  mutate(
    Inflation = -2.8,
    SommePPF = sum(CroissancePPF)-0.1573929 + Inflation,
         SommeFlux = sum(ContributionFluxTrim) + 2.089727,
         SommeReevaluation = sum(ContributionReevaluation) -2.24712 + Inflation)

PPF3<- PPF %>%
  filter(date >= "2022-01-01" & date <= "2023-10-01")%>%
  mutate(Inflation = -10,
         SommePPF = sum(CroissancePPF)+ Inflation,
         SommeFlux = sum(ContributionFluxTrim),
         SommeReevaluation = sum(ContributionReevaluation))

PPF4<- PPF %>%
  filter(date == "2024-01-01")%>%
  mutate(Inflation = - 2.8,
         SommePPF = sum(CroissancePPF)+ Inflation,
         SommeFlux = sum(ContributionFluxTrim),
         SommeReevaluation = sum(ContributionReevaluation))

PPF1bis <- bind_rows(PPF1,PPF2,PPF3, PPF4)%>%
rename (Réevaluation = SommeReevaluation,
        Flux =SommeFlux)%>%
pivot_longer(cols= - date, names_to = "Operation", values_to = "Value")

Flux <- inner_join(inner_join(inner_join(DepotBancairesRemunereesF, ActionscoteesF, by = "date"), ActionsnoncoteesF, by = "date"), FluxTrim, by = "date") %>%
  rename(
    DepotBancairesRemunereesF = "Dépôts bancaires rémunérés",
    ActionscoteesF = "Actions cotées",
    ActionsnoncoteesF = "Actions non cotées et autres participations") %>%
  inner_join(NumerairesDepotsAVueF, by = "date") %>%
  rename(
    NumerairesDepotsAVueF = "Numéraire et dépôts") %>%
  inner_join(AssuranceviesupportF, by = "date") %>%
  rename(
    AssuranceviesupportF = "Assurance vie et épargne retraite") %>%
  filter(date >= "2012-01-01") %>%
  mutate(AutreF = FluxTrim - rowSums(select(., c(NumerairesDepotsAVueF, ActionscoteesF, ActionsnoncoteesF, AssuranceviesupportF, DepotBancairesRemunereesF)), na.rm = TRUE),
         Actions = ActionscoteesF+  ActionsnoncoteesF,
         NumerairesetDepotF =  NumerairesDepotsAVueF +  DepotBancairesRemunereesF) %>%
  pivot_longer(cols = -date, names_to = "Operation", values_to = "Value") %>% 
  group_by(Operation) %>%  # Group by date
  mutate(CovidTotal = sum(Value[date >= as.Date("2020-01-01") & date <= as.Date("2023-07-01")]))  
  
#on merge le RNB de l'INSEE

RDB <- get_insee_dataset("CNT-2020-CSI") %>%
  filter(OPERATION == "B6")%>%
  filter(SECT_INST == "S14")%>%
  split_title() %>%
  select(DATE, OBS_VALUE, OPERATION) %>%
  arrange(DATE) %>%
  pivot_wider(names_from = OPERATION, values_from = OBS_VALUE, names_sep = "_")%>%
  rename(date= DATE)

  
Rapport<- inner_join(RDB, PrincipauxPlacementsfinanciers, by = "date")%>%
  rename(PPF = "Principaux placements financiers")%>%
  mutate(B6 = (B6/1000)*4,
         Rapport = PPF/B6,
         trendPreCovid = mean(Rapport[date >= "2012-01-01" & date <= "2019-10-01"]),
         trendPostCovid = mean(Rapport[date>= "2020-01-01" & date <= "2024-01-01"]))
 

rapport_2019_avg <- Rapport %>%
  filter(date >= as.Date("2019-01-01") & date <= as.Date("2019-10-01")) %>%
  summarize(avg_rapport_2019 = mean(Rapport, na.rm = TRUE)) %>%
  pull(avg_rapport_2019)

graph4PB_A<-ggplot(data = Rapport) +
  geom_line(aes(x = as.Date(date), y = Rapport), show.legend = TRUE) +
  geom_hline(yintercept = rapport_2019_avg, linetype = "dashed", color = "grey", size = 1) + 
  annotate("text", x = as.Date("2014-03-01"), y = rapport_2019_avg, label = "moyenne 2019", vjust = -1, hjust = -0.1, color = "grey") +  # Adding "2019" label# Horizontal line at 2019 value  # Vertical line at 2019
  labs(
    caption = "Sources: Banque de France, INSEE, calculs des auteurs",
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2012-01-01", max(Rapport$date))), expand = c(0, 0)) +
  scale_y_continuous(limits = c(3,4), labels = scales::label_number(decimal.mark = ",")) +  # Adjusted x-axis limits for better visualization
  ggtitle("Patrimoine financier des ménages (ratio encours / RDB)")

graph4PB_A

ggsave(filename = "C:/Users/153003/Documents/Epargne/graph4PB_A.png", plot = graph4PB_A, width = 8, height = 6, units = "in")
ggsave(filename = "C:/Users/153003/Documents/Epargne/graph4PB_A.svg", plot = graph4PB_A, width = 15, height = 8, units = "in")
 

#Patrimoine financier des ménages
        
data_Epargne <- data_EpargneFi %>%
  pivot_longer(cols = -date, names_to = "Operation", values_to = "Value") %>%
  filter(date >= ("2012-01-01"))%>%
  group_by(Operation) %>%
  mutate(CroissanceTrim = (Value - lag(Value, n = 1)) / lag(Value, n = 1) * 100,
  CroissanceAnnuel = (Value - lag(Value, n = 4)) / lag(Value, n = 4) * 100,
  CovidTotal  = (Value[date == "2023-04-01"] - Value[date == "2019-10-01"]), 
  Covid = ((Value[date == ("2023-04-01")] - Value[date == ("2019-10-01")]) / Value[date == ("2019-10-01")]) * 100,
  Premier = ((Value[date == "2021-07-01"] - Value[date == "2019-10-01"]) / Value[date == "2019-10-01"]) * 100,
  Deuxieme = ((Value[date == "2022-04-01"] - Value[date == "2021-10-01"]) / Value[date == "2021-10-01"]) * 100,
  Troisieme = ((Value[date == "2023-04-01"] - Value[date == "2022-10-01"]) / Value[date == "2022-10-01"]) * 100) %>%
  group_by(date)%>%
  mutate(Part = (Value / Value[Operation == "PrincipauxPlacementsfinanciers"]) * 100)%>%
  group_by(Operation) %>%
  mutate(Contribution = CroissanceAnnuel * lag(Part, n = 4) /100,
         ContributionCovid = Covid * (Part[date=="2019-10-01"])/100,
         ContributionPremier = Premier * (Part[date=="2019-10-01"])/100,
         ContributionDeuxieme = Deuxieme * (Part[date=="2021-10-01"])/100,
         ContributionTroisieme = Troisieme * (Part[date=="2022-10-01"])/100)

            
ggplot(data = data_Epargne %>%
         filter(Operation == "PrincipauxPlacementsfinanciers")) +
  geom_line(aes(x = as.Date(date), y = Value, color = Operation), show.legend = TRUE) +
  labs(
    caption = "Source: Banque de France",
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  #theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2011-01-01", max(data_Epargne$date))), expand = c(0, 0)) +
  scale_y_continuous(limits = c(3700, 6500), labels = scales::label_number(decimal.mark = ",")) +  # Adjusted x-axis limits for better visualization
  ggtitle("Epargne financière des ménages, Encours,Milliards d'euros ")


ggplot(data = data_Epargne %>%
      filter(Operation == "PrincipauxPlacementsfinanciers")) +
  geom_line(aes(x = as.Date(date), y = CroissanceAnnuel, color = Operation), show.legend = TRUE) +
  labs(
    caption = "Source: Banque de France",
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2013-01-01", max(data_EpargneFi$date))), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-7, 15), labels = scales::label_number(decimal.mark = ",")) +
  ggtitle("Epargne financière des ménages, Taux de croissance annuel")


ggsave(filename = "C:/Users/153003/Documents/Epargne/Principauxplacementsfinanciers.png", plot = PlacementTotal, width = 8, height = 6, units = "in")

ggplot(data = data_Epargne %>%
                 filter(Operation %in% c("PrincipauxPlacementsfinanciers", "DepotBancairesRemunerees","Actionsnoncotees", "Assuranceviesupport","NumerairesDepotsAVue","Actionscotees"))) +
  geom_line(aes(x = as.Date(date), y = CroissanceAnnuel, color = Operation), show.legend = TRUE) +
  labs(
    caption = "Source: Banque de France",
    y = NULL,
    x = NULL,
    color = NULL
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +
  scale_x_date(limits = as.Date(c("2014-01-01", max(data_Epargne$date))), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-15, 30), labels = scales::label_number(decimal.mark = ",")) +  # Adjusted x-axis limits for better visualization
  ggtitle("Epargne financière des ménages, Taux de croissance annuel des encours")

 
graph4PB<- data_Epargne %>%
  filter(Operation %in% c("NumerairesetDepot", "Assuranceviesupport", "Actions", "Autre"),
         date == ("2019-10-01")) %>%
  ggplot(aes(x = 2, y = Value, group= Operation)) +
  geom_bar(aes(fill = Operation), stat = "identity", color="white" ,width = 1) +
  coord_polar(theta = "y", start = 0) +
  geom_text(
    aes( label = round(Value)),
    position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c(
    "NumerairesetDepot" = "#F59C00",
    "Actions" = "#008D36",
    "Assuranceviesupport" = "#C51315",
    "Autre"= "grey"),
    labels = c("NumerairesetDepot" = "Numéraires et dépôts à vue", "Assuranceviesupport" = "Assurance Vie")) +
  theme_void() +
  xlim(0.5, 2.5) +
  ggtitle("") +
  labs(
    caption = "",
    fill = "") +
  theme(legend.position = "right")+
guides(fill = FALSE)

graph4PB

ggsave(filename = "C:/Users/153003/Documents/Epargne/graph4PB.png", plot = graph4PB, width = 8, height = 6, units = "in")

graph4PBbis<- data_Epargne %>%
  filter(Operation %in% c("NumerairesetDepot", "Assuranceviesupport", "Actions", "Autre"),
         date == ("2024-01-01")) %>%
  ggplot(aes(x = 2, y = Value, group= Operation)) +
  geom_bar(aes(fill = Operation), stat = "identity", color="white" ,width = 1) +
  coord_polar(theta = "y", start = 0) +
    geom_text(
      aes( label = round(Value)),
      position = position_stack(vjust = 0.5)) +
    scale_fill_manual(values = c(
      "NumerairesetDepot" = "#F59C00",
      "Actions" = "#008D36",
      "Assuranceviesupport" = "#C51315",
      "Autre"= "grey"),
      labels = c("NumerairesetDepot" = "Numéraires et dépôts à vue", "Assuranceviesupport" = "Assurance Vie")) +
  theme_void() +
  xlim(0.5, 2.5) +
  ggtitle("") +
  labs(
    caption = "",
    fill = "") +
 theme(legend.position = "bottom", legend.key.size = unit(0.35, "cm"))

graph4PBbis

ggsave(filename = "C:/Users/153003/Documents/Epargne/graph4PBbis.png", plot = graph4PBbis, width = 8, height = 6, units = "in")



graph4PBter<- Flux %>%
  filter(Operation %in% c("NumerairesetDepotF", "AssuranceviesupportF", "Actions", "AutreF"),
         date == ("2024-01-01")) %>%
  ggplot(aes(x = factor(Operation), y = CovidTotal, fill = Operation)) +
  geom_bar(stat = "identity", width = 0.8) +
  geom_text(aes(label = round(CovidTotal)), position = position_stack(vjust = 0.5), color = "black", size = 3) +
  scale_fill_manual(values = c(
    "NumerairesetDepotF" = "#F59C00",
    "Actions" = "#008D36",
    "AssuranceviesupportF" = "#C51315",
    "AutreF" = "grey"),
    labels = c("Numéraires et dépôts à vue", "Actions", "Assurance Vie", "Autre")) +
  ggtitle("Flux entre les 2 dates") +
  labs(
    caption = "",
    fill = NULL,
    x = NULL,
    y = "") +
  theme(plot.title = element_text(size = 10),
        legend.position = "right",
        panel.background = element_blank(),
        text = element_text(family = "Arial"),
        axis.text.x = element_blank(),
        axis.line.y = element_line()) +
  geom_hline(yintercept = 0, size = 0.1)+
  guides(fill = FALSE)

# Combine the plots with the legend between graph4PB and graph4PBbis

plot_grid(
  plot_grid(graph4PB, graph4PBbis, ncol = 2, rel_widths = c(0.65, 0.8)),
  graph4PBter,
  nrow = 2,
  rel_heights = c(1, 0.5)
)

ggdraw() +
  draw_plot(
    graph5PB,
    width = 1,
    height = 1,
    x = 0,
    y = 0
  ) +
  draw_label(
    "Epargne financière des ménages au T4:2019 et au T1:2024, Encours d'actifs en Milliards€",
    x = 0.5,
    y = 0.98,
    size = 10.5,
    fontface = "bold"
  ) +
draw_label("Source: Banque de France, calculs des auteurs", x = 0.2, y = 0.01, size = 7.5, color = "black")

ggsave(filename = "C:/Users/153003/Documents/Epargne/graph6PB.svg", plot = graph5PB, width = 15, height = 8, units = "in")
ggsave(filename = "C:/Users/153003/Documents/Epargne/graph6PB.png", plot = graph5PB, width = 8, height = 6, units = "in")


ggplot(data = data_Epargne %>%
                    filter(date == max(date), 
                           Operation %in% c("NumerairesDepotsAVue", "Actioncotees", "Actionsnoncotees", "Assuranceviesupport", "DepotBancairesRemunerees", "Autre"))) +
geom_bar(aes(x = 1, y = ContributionPremier, fill = Operation), position = "stack", stat = "identity") +
geom_bar(aes(x = 2, y = ContributionDeuxieme, fill = Operation), position = "stack", stat = "identity") +
geom_bar(aes(x = 3, y = ContributionTroisieme, fill = Operation), position = "stack", stat = "identity") +
geom_point(data = . %>%
               filter(Operation == "PrincipauxPlacementsfinanciers"), aes(x = 1, y = ContributionPremier), color = "black", size = 3) +
geom_point(data = . %>%
               filter(Operation == "PrincipauxPlacementsfinanciers"), aes(x = 2, y = ContributionDeuxieme), color = "black", size = 3) +
geom_point(data = . %>%
               filter(Operation == "PrincipauxPlacementsfinanciers"), aes(x = 3, y = ContributionTroisieme), color = "black", size = 3) +
geom_text(data = . %>%
              filter(Operation == "PrincipauxPlacementsfinanciers"), aes(x = 1, y = ContributionPremier, label = round(ContributionPremier, 1)), color = "black", size = 3, vjust = -1) +
geom_text(data = . %>%
              filter(Operation == "PrincipauxPlacementsfinanciers"), aes(x = 2, y = ContributionDeuxieme, label = round(ContributionDeuxieme, 1)), color = "black", size = 3, vjust = -1) +
geom_text(data = . %>%
              filter(Operation == "PrincipauxPlacementsfinanciers"), aes(x = 3, y = ContributionTroisieme, label = round(ContributionTroisieme, 1)), color = "black", size = 3, vjust = -1) +
  labs(
    title = "Contributions to Growth",
    caption = "Source: Banque de France",
    x = "",
    y = "Contribution (pp)"
  ) +
  scale_x_continuous(breaks = c(1, 2, 3), labels = c("2019:T4-2021:T3", "2021:T4-2022:T2", "2022:T3-2024:T1")) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.text = element_text(size = 8)) +  # Adjust the size as needed
  coord_cartesian(ylim = c(-7, 12)) +
  ggtitle("Contribution à la croissance de l'épargne financière, en pp")

ggsave(filename = "C:/Users/153003/Documents/Epargne/CroissanceEpargne.png", plot = CroissanceEpargne, width = 8, height = 6, units = "in")

ggplot(data = PPF) +
geom_bar(data = PPF|> filter(Operation %in% c("ContributionReevaluation", "ContributionFluxTrim")),aes(x = as.Date(date), y = Value, fill = Operation), position = "stack", stat = "identity") + 
  geom_line(data = PPF|> filter(Operation== "CroissancePPF"), aes(x = as.Date(date), y = Value), show.legend = TRUE) +
  geom_point(data = PPF|> filter(Operation== "CroissancePPF"), aes(x = as.Date(date), y = Value), color = "black", size = 3)+
  labs(
    title = "Contributions to Growth",
    x = "Date",
    y = "En pp"
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") +  
  scale_x_date(limits = as.Date(c("2020-04-01", "2023-07-01")), date_labels = "%Y-%m-%d") +
  scale_y_continuous(limits = c(-5, 4), labels = scales::label_number(decimal.mark = ",")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  ggtitle("Taux de croissance trimestrielle et contributions")


graphPB5_bis<- ggplot(data = PPF1bis) +
    geom_bar(data = . %>% filter(Operation %in% c("Réevaluation", "Flux", "Inflation") & date == "2021-04-01"),
             aes(x = 1, y = Value, fill = Operation), position = "stack", stat = "identity") + 
    geom_point(data = . %>% filter(Operation == "SommePPF" & date == "2021-04-01"),
               aes(x = 1, y = Value), color = "black", size = 3) +
    geom_text(data = . %>% filter(Operation == "SommePPF" & date == "2021-04-01"), 
              aes(x = 1, y = Value +1.2, label = paste0(round(Value, 1), "%")))+
    geom_bar(data = . %>% filter(Operation %in% c("Réevaluation", "Flux", "Inflation") & date == "2023-04-01"),
             aes(x = 2, y = Value, fill = Operation), position = "stack", stat = "identity") + 
    geom_point(data = . %>% filter(Operation == "SommePPF" & date == "2023-10-01"),
               aes(x = 2, y = Value), color = "black", size = 3) +
    geom_text(data = . %>% filter(Operation == "SommePPF" & date == "2023-10-01"),
              aes(x = 2, y = Value +1.2, label = paste0(round(Value, 1), "%")))+
  geom_bar(data = . %>% filter(Operation %in% c("Réevaluation", "Flux", "Inflation") & date == "2024-01-01"),
           aes(x = 3, y = Value, fill = Operation), position = "stack", stat = "identity") + 
  geom_point(data = . %>% filter(Operation == "SommePPF" & date == "2024-01-01"),
             aes(x = 3, y = Value), color = "black", size = 3) +
  geom_text(data = . %>% filter(Operation == "SommePPF" & date == "2024-01-01"),
            aes(x = 3, y = Value +1.2, label = paste0(round(Value, 1), "%")))+
    labs(
      caption = "Source: Banque de France, calculs des auteurs
Note : la crise covid se déroule de T4:2019 à T4:2021, la crise inflationniste de T1:2022 à T1:2024,
      et la normalisation se déroule depuis le T1:2024",
      title = "Contributions to Growth",
      x = NULL,
      y = "En pp",
      fill = NULL, 
    ) +
    theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
    theme(legend.position = "bottom") + 
    theme(legend.position = "bottom") + 
    theme(axis.text.x = element_text(angle = 0)) +
    scale_x_continuous(breaks = c(1, 2,3), labels = c("Crise Covid", "Crise Inflationniste", "Normalisation")) +
    scale_y_continuous(limits = c(-15, 15), labels = scales::label_number(decimal.mark = ",")) +
    ggtitle("Variation du patrimoine financier réel et contributions")
  
graphPB5_bis
ggsave(filename = "C:/Users/153003/Documents/Epargne/graphPB5_bis.svg", plot = graphPB5_bis, width = 15, height = 6, units = "in")
ggsave(filename = "C:/Users/153003/Documents/Epargne/graphPB5_bis.png", plot = graphPB5_bis, width = 8, height = 6, units = "in")


graphPB5_bis<- ggplot(data = PPF1bis) +
  geom_bar(data = . %>% filter(Operation %in% c("Réevaluation", "Flux") & date == "2021-04-01"),
           aes(x = 1, y = Value, fill = Operation), position = "stack", stat = "identity") + 
  geom_point(data = . %>% filter(Operation == "SommePPF" & date == "2021-04-01"),
             aes(x = 1, y = Value), color = "black", size = 3) +
  geom_text(data = . %>% filter(Operation == "SommePPF" & date == "2021-04-01"), 
            aes(x = 1, y = Value +1.2, label = paste0(round(Value, 1), "%")))+
  geom_bar(data = . %>% filter(Operation %in% c("Réevaluation", "Flux") & date == "2023-04-01"),
           aes(x = 2, y = Value, fill = Operation), position = "stack", stat = "identity") + 
  geom_point(data = . %>% filter(Operation == "SommePPF" & date == "2023-10-01"),
             aes(x = 2, y = Value), color = "black", size = 3) +
  geom_text(data = . %>% filter(Operation == "SommePPF" & date == "2023-10-01"),
            aes(x = 2, y = Value +1.2, label = paste0(round(Value, 1), "%")))+
  geom_bar(data = . %>% filter(Operation %in% c("Réevaluation", "Flux") & date == "2024-01-01"),
           aes(x = 3, y = Value, fill = Operation), position = "stack", stat = "identity") + 
  geom_point(data = . %>% filter(Operation == "SommePPF" & date == "2024-01-01"),
             aes(x = 3, y = Value), color = "black", size = 3) +
  geom_text(data = . %>% filter(Operation == "SommePPF" & date == "2024-01-01"),
            aes(x = 3, y = Value +1.2, label = paste0(round(Value, 1), "%")))+
  labs(
    caption = "Source: Banque de France, calculs des auteurs
Note : la crise covid se déroule de T4:2019 à T4:2021, la crise inflationniste de T1:2022 à T1:2024,
      et la normalisation se déroule depuis le T1:2024",
    title = "Contributions to Growth",
    x = NULL,
    y = "En pp",
    fill = NULL, 
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial")) +
  theme(legend.position = "bottom") + 
  theme(legend.position = "bottom") + 
  theme(axis.text.x = element_text(angle = 0)) +
  scale_x_continuous(breaks = c(1, 2,3), labels = c("Crise Covid", "Crise Inflationniste", "Normalisation")) +
  scale_y_continuous(limits = c(-15, 15), labels = scales::label_number(decimal.mark = ",")) +
  ggtitle("Variation du patrimoine financier réel et contributions")

save(list = c("graph_0", "graph_0bis","graph_0ter", "graph_1", "graph_2", "graph_3", "graph_4", "graph_5","graph_6"),
     data_EpargneFi,
     file = "graphiques_epargne.rda")


     

