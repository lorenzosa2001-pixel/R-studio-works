# Carica le librerie necessarie
library(readxl)
library(dplyr)
library(tidyr)
setwd("C:\\Users\\Lorenzo\\Documents\\Uni\\Ultimo semestre\\De Arcangelis\\2°lavoro\\C\\Per R")

# 1. Leggi i dati dai file Excel
# ----------------------------------------------------------------------
# Bilateral trade
trade_data <- read_excel("Bilateral_trade.xlsx", sheet = "Sheet1") %>%
  rename(bilateral_trade = Bilateral_Trade)

# Distanze
distance_data <- read_excel("Distanze_r.xlsx", sheet = "Foglio1") %>%
  rename(distance = Distanza)

# GDP - NOTA: qui usiamo il nome esatto della colonna che contiene i valori del PIL
gdp_data <- read_excel("GDP_r.xlsx", sheet = "Foglio1") %>%
  select(COUNTRY, `2024`) %>%  # Seleziona le colonne COUNTRY e 2024
  rename(gdp = `2024`)  # Rinomina la colonna 2024 in gdp

# 2. Pulisci e prepara dati
# ----------------------------------------------------------------------
# Separa la colonna "Pair" in due colonne
trade_pairs <- trade_data %>%
  separate(Pair, into = c("country1", "country2"), sep = "-", remove = FALSE)
gdp_data$gdp <- gdp_data$gdp * 1e3

# 3. Unisci i dati
# ----------------------------------------------------------------------
# Aggiungi il GDP del primo paese
merged_data <- trade_pairs %>%
  left_join(gdp_data, by = c("country1" = "COUNTRY")) %>%
  rename(gdp1 = gdp)

# Aggiungi il GDP del secondo paese
merged_data <- merged_data %>%
  left_join(gdp_data, by = c("country2" = "COUNTRY")) %>%
  rename(gdp2 = gdp)
# Sostituisci manualmente NA specifici (esempio)
merged_data <- merged_data %>%
  mutate(
    gdp1 = ifelse(is.na(gdp1) & country1 == "indonesia", 1396300000, gdp1),
    gdp2 = ifelse(is.na(gdp2) & country2 == "Russia", 2161205000, gdp2),
    gdp2 = ifelse(is.na(gdp2) & country2 == "South Korea", 
  )



  # Correggi i nomi nei dati GDP per farli combaciare
  merged_data <- merged_data %>%
    mutate(country1 = case_when(
      country1 == "Inonesia" ~ "Indonesia",
      TRUE ~ country1
    ))
  
  
  
  
  
# Aggiungi le distanze
final_data <- merged_data %>%
  left_join(distance_data, by = c("country1" = "Reporter", "country2" = "Partner"))

# 4. Pulisci i dati finali
final_data <- na.omit(final_data)

# 5. Aggiungi trasformazioni logaritmiche
final_data <- final_data %>%
  mutate(
    ln_trade = log(bilateral_trade),
    ln_gdp1 = log(gdp1),
    ln_gdp2 = log(gdp2),
    ln_distance = log(distance)
  )

# 6. Verifica il dataframe finale
head(final_data)
str(final_data)

# 7. Regressione
model <- lm(ln_trade ~ ln_gdp1 + ln_gdp2 + ln_distance, data = final_data)
summary(model)



# Se non l'hai caricato: carica i pacchetti
library(ggplot2)

# Supponiamo che il tuo modello si chiami "model"
# Residui vs fitted values
ggplot(data = data.frame(fitted = fitted(model), residuals = resid(model)), aes(x = fitted, y = residuals)) +
  geom_point(color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Grafico Residui vs Valori Predetti",
       x = "Valori predetti",
       y = "Residui") +
  theme_minimal()

# Supponiamo che i dati siano in final_data
ggplot(final_data, aes(x = ln_distance, y = ln_trade)) +
  geom_point(color = "darkgreen", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Relazione log(Trade) vs log(Distance)",
    x = "log(Distance)",
    y = "log(Bilateral Trade)"
  ) +
  theme_minimal()






Riprova





getwd()
setwd("C:\\Users\\Lorenzo\\Documents\\Uni\\Ultimo semestre\\De Arcangelis\\2°lavoro\\C\\Per R - 2")

library(dplyr)
library(tidyr)

# Leggi il dataset


trade_data <- trade_data %>%
  rename(
    Year = refMonth ,
    Reporter = reporterISO, 
    flowdesc = flowCode,
    Partner = partnerISO,
    Value = fobvalue
      )


# Seleziona solo le colonne essenziali
essential_cols <- c("Year", "Reporter", "flowdesc", "Partner", 
                    "Value")

trade_clean <- trade_data %>% 
  select(all_of(essential_cols)) %>%
  filter(!is.na(Value)) %>%  # Rimuovi righe con valori mancanti
  mutate(Value = as.numeric(Value))  # Converti in numerico

#Sostituisce "T�rkiye" con "Turkey" nella colonna 'Country'
trade_clean$Reporter <- gsub("T�rkiye", "Turkey", trade_clean$Reporter)
# Supponendo che il tuo dataset si chiami 'data' e la colonna contenente "Türkiye" si chiami 'country'
trade_clean$Reporte <- gsub("Türkiye", "Turkey", trade_clean$Reporter)
trade_clean$Partner <- gsub("T\xfcrkiye", "Turkey", trade_clean$Partner, useBytes = TRUE)

# Se invece vuoi sostituire in tutto il dataset (in tutte le colonne di tipo carattere)
data[] <- lapply(data, function(x) if(is.character(x)) gsub("Türkiye", "Turkey", x) else x)



# Calcola il commercio bilaterale (somma di import ed export tra ogni coppia di paesi)
bilateral_trade <- trade_clean %>%
  group_by(Reporter, Partner) %>%
  summarize(
    total_trade = sum(Value, na.rm = TRUE),
    exports = sum(Value[flowdesc == "Export"], na.rm = TRUE),
    imports = sum(Value[flowdesc == "Import"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(Reporter != Partner) %>%  # Rimuovi commercio con se stessi
  arrange(desc(total_trade))  # Ordina per volume commerciale decrescente

# Aggiungi colonna per il saldo commerciale
bilateral_trade <- bilateral_trade %>%
  mutate(trade_balance = exports - imports)

# Uniforma i nomi dei paesi per coerenza con altri dataset
bilateral_trade <- bilateral_trade %>%
  mutate(
    Reporter = case_when(
      Reporter == "Rep. of Korea" ~ "South Korea",
      Reporter == "Türkiye" ~ "Turkey",
      TRUE ~ Reporter
    ),
    Partner = case_when(
      Partner == "Rep. of Korea" ~ "South Korea",
      Partner == "Türkiye" ~ "Turkey",
      TRUE ~ Partner
    )
  )



summary(model)



# Visualizza le prime righe del risultato
head(bilateral_trade)

# Opzionale: salva il dataset pulito
write.csv(bilateral_trade, "bilateral_trade_clean.csv", row.names = FALSE)
