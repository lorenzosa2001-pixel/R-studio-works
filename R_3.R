
library(tidyverse)
library(readr)
setwd("C:\\Users\\Lorenzo\\Documents\\Uni\\Ultimo semestre\\De Arcangelis\\2°lavoro\\C\\R_3")

df <- read_csv("Trade_data.csv")


df_clean <- df %>%
  select(reporterISO, partnerISO, flowCode, fobvalue) %>%
  rename(from = reporterISO, to = partnerISO, flow = flowCode, trade_value = fobvalue) %>%
  filter(!is.na(trade_value), from != to)


df_clean <- df_clean %>%
  mutate(pair = pmap_chr(list(from, to), ~ paste(sort(c(..1, ..2)), collapse = "_")))


# Crea una coppia ordinata per ogni direzione
df_clean <- df %>%
  filter(!is.na(fobvalue), reporterISO != partnerISO) %>%
  mutate(
    pair = pmap_chr(list(reporterISO, partnerISO), ~ paste(sort(c(..1, ..2)), collapse = "_"))
  )

# Somma i valori per ogni direzione
bilateral_full <- df_clean %>%
  group_by(pair, reporterISO, partnerISO) %>%
  summarise(value = sum(fobvalue, na.rm = TRUE), .groups = "drop")

# Calcola la media tra le due direzioni (se entrambe presenti)
bilateral_avg <- bilateral_full %>%
  group_by(pair) %>%
  summarise(bilateral_trade = mean(value), .groups = "drop") %>%
  separate(pair, into = c("country1", "country2"), sep = "_")


trade_pairs <- df_clean %>%
  group_by(pair) %>%
  summarise(bilateral_trade = sum(trade_value, na.rm = TRUE)) %>%
  separate(pair, into = c("country1", "country2"), sep = "_") %>%
  ungroup()


print(trade_pairs)
write.csv(trade_pairs, "trade_pairs.csv")

GDP

library(dplyr)
library(tidyr)
library(countrycode)


gdp_data <- read_csv("Gdp.csv") %>%
  select(COUNTRY, `2022`) %>%  
  rename(Gdp = `2022`)

library(dplyr)
library(readr)


gdp_data <- read_csv("Gdp.csv") %>%
  select(COUNTRY, `2022`) %>%
  rename(GDP_2022 = `2022`) %>%
  mutate(COUNTRY = case_when(
    COUNTRY == "Egypt, Arab Republic of" ~ "Egypt",
    COUNTRY == "Ethiopia, The Federal Democratic Republic of" ~ "Ethiopia",
    COUNTRY == "Korea, Republic of" ~ "South Korea",
    COUNTRY == "Türkiye, Republic of" ~ "Turkey",
    TRUE ~ COUNTRY
  )) %>%
  filter(!is.na(GDP_2022)) %>%
  mutate(ISO3C = countrycode(COUNTRY, "country.name", "iso3c"))

print("Cleaned GDP Data for 2022:")
print(gdp_data)


gdp_data <- read_csv("Gdp.csv") %>%
  select(COUNTRY, `2022`) %>%
  rename(GDP_billions = `2022`) %>%
  mutate(
  
    GDP_absolute = GDP_billions * 1000000000,  # 1 billion = 1,000,000,000
    COUNTRY = case_when(
      COUNTRY == "Egypt, Arab Republic of" ~ "Egypt",
      COUNTRY == "Ethiopia, The Federal Democratic Republic of" ~ "Ethiopia",
      COUNTRY == "Korea, Republic of" ~ "South Korea",
      COUNTRY == "Türkiye, Republic of" ~ "Turkey",
      TRUE ~ COUNTRY
    )
  ) %>%
  filter(!is.na(GDP_billions)) %>%
  mutate(ISO3C = countrycode(COUNTRY, "country.name", "iso3c"))


print("GDP Data with Absolute Values (2022):")
print(gdp_data %>% select(COUNTRY, GDP_billions, GDP_absolute))
gdp_data <- gdp_data %>%
  select(COUNTRY, GDP_absolute,ISO3C ) %>%
  rename(gdp = GDP_absolute)

write.csv(gdp_data, "gdp_data.csv")



distanze

install.packages("readxl")

library(tidyverse)
library(readr)
library(readxl)
distances <- read_excel("dist_cepii.xls", sheet = 1)


distances <- read.csv("distances_celan.csv")


unique_pairs <- distances_clean[distances_clean$iso_o != distances_clean$iso_d, ]


unique_pairs <- unique_pairs %>%
  mutate(pair = ifelse(iso_o < iso_d, 
                       paste(iso_o, iso_d, sep = "-"), 
                       paste(iso_d, iso_o, sep = "-"))) %>%
  distinct(pair, .keep_all = TRUE)


unique_pairs <- unique_pairs %>%
  select(iso_o, iso_d, dist) %>%
  arrange(iso_o, iso_d)


print(unique_pairs, n = 105)


write.csv(unique_pairs, "unique_country_pairs.csv", row.names = FALSE)








UNIONE DATI





library(dplyr)
library(tidyr)

gdp_data <- read.csv("gdp_data.csv") %>%
  select(ISO3C, gdp) %>%
  rename(country = ISO3C)

trade_pairs <- bilateral_avg

distances <- read.csv("unique_country_pairs.csv") %>%
  rename(country1 = iso_o, country2 = iso_d, distance = dist)


merged_data <- trade_pairs %>%
  left_join(distances, by = c("country1", "country2"))

merged_data <- merged_data %>%
  left_join(gdp_data, by = c("country1" = "country")) %>%
  rename(gdp_country1 = gdp)


merged_data <- merged_data %>%
  left_join(gdp_data, by = c("country2" = "country")) %>%
  rename(gdp_country2 = gdp)


merged_data <- merged_data %>%
  mutate(gdp_product = gdp_country1 * gdp_country2)


final_data <- merged_data %>%
  mutate(
    log_trade = log(bilateral_trade),
    log_gdp1 =  log(gdp_country1),
    log_gdp2 = log(gdp_country2),
    log_distance = log(distance),
  ) %>%
  select(country1, country2, bilateral_trade, distance, 
         gdp_country1, gdp_country2, gdp_product,
         log_trade, log_distance, log_gdp1, log_gdp2)


str(final_data)


model <- lm(log_trade ~ log_gdp1 + log_gdp2 + log_distance, data = final_data)
summary(model)

write.csv(final_data, "gravity_model_data.csv", row.names = FALSE)









grafico



ggplot(final_data, aes(x = log_distance, y = log_trade)) +
  geom_point(color = "darkgreen", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Relazione log(Trade) vs log(Distance)",
    x = "log(Distance)",
    y = "log(Bilateral Trade)"
  ) +
  theme_minimal()



install.packages("gridExtra")
library(ggplot2)
library(gridExtra)


plot1 <- ggplot(final_data, aes(x = log(gdp_country1), y = log_trade)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", color = "firebrick", se = FALSE) +
  labs(x = "Log GDP Country 1", y = "Log Bilateral Trade", 
       title = "Trade vs GDP Country 1") +
  theme_minimal()

plot2 <- ggplot(final_data, aes(x = log(gdp_country2), y = log_trade)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", color = "firebrick", se = FALSE) +
  labs(x = "Log GDP Country 2", y = "Log Bilateral Trade",
       title = "Trade vs GDP Country 2") +
  theme_minimal()

plot3 <- ggplot(final_data, aes(x = log_distance, y = log_trade)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", color = "firebrick", se = FALSE) +
  labs(x = "Log Distance", y = "Log Bilateral Trade",
       title = "Trade vs Distance") +
  theme_minimal()


grid.arrange(plot1, plot2, plot3, ncol = 2,
             top = "Relazioni nel Modello di Gravità Naive")


final_data$predicted <- predict(model)

ggplot(final_data, aes(x = predicted, y = log_trade)) +
  geom_point(color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, color = "firebrick", linetype = "dashed") +
  labs(x = "Valori Predetti dal Modello", y = "Valori Osservati (Log Trade)",
       title = "Bontà del Modello: Osservato vs Predetto",
       subtitle = paste("R-quadro:", round(summary(model)$r.squared, 3))) +
  theme_minimal()
