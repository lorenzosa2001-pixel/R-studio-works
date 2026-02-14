
library(tidyverse)
library(ggplot2)
library(broom)


setwd("C:\\Users\\Lorenzo\\Documents\\Uni\\Ultimo semestre\\De Arcangelis\\2°lavoro\\A")

gdp_data <- read_csv("GDP 2.csv") %>%
  select(COUNTRY, `2019`) %>%
  rename(gdp_2019 = `2019`) %>%
  mutate(COUNTRY = case_when(
    COUNTRY == "Slovak Republic" ~ "Slovakia",
    COUNTRY == "Czech Republic" ~ "Czechia",
    COUNTRY == "Croatia, Republic of" ~ "Croatia",
    COUNTRY == "Estonia, Republic of" ~ "Estonia",
    COUNTRY == "Lithuania, Republic of" ~ "Lithuania",
    COUNTRY == "Slovenia, Republic of" ~ "Slovenia",
    COUNTRY == "Netherlands, The" ~ "Netherlands",
    COUNTRY == "Latvia, Republic of" ~ "Latvia",
    COUNTRY == "Poland, Republic of" ~ "Poland",
    TRUE ~ COUNTRY
  )) 


gdp_data_ordered <- gdp_data %>%
  arrange(gdp_2019)


median_row <- gdp_data_ordered[14, ]

median_country <- median_row$COUNTRY
median_gdp <- median_row$gdp_2019


trade_data <- read_csv("Trade data 2.csv") %>%
  filter(refYear == 2019) %>%
  select(partnerDesc, flowDesc, primaryValue) %>%
  pivot_wider(names_from = flowDesc, values_from = primaryValue) %>%
  rename(country = partnerDesc, 
         export_to_kor = Export,
         import_from_kor = Import) %>%
  mutate(country = str_replace(country, "Rep. of Korea", "South Korea"))

romania_export <- trade_data %>%
  filter(country == "Romania") %>%
  pull(export_to_kor)

romania_import <- trade_data %>%
  filter(country == "Romania") %>%
  pull(import_from_kor)

full_data <- full_data %>%
  mutate(
    norm_export = export_to_kor / romania_export,
    norm_import = import_from_kor / romania_import,
    log_norm_export = log(norm_export),
    log_norm_import = log(norm_import)
  )




full_data <- gdp_data %>%
  inner_join(trade_data, by = c("COUNTRY" = "country")) %>%
  mutate(
    norm_gdp = gdp_2019 / median_gdp,
    norm_export = export_to_kor / romania_export,
    norm_import = import_from_kor / romania_import,
    log_norm_gdp = log(norm_gdp),
    log_norm_export = log(norm_export),
    log_norm_import = log(norm_import)
  )


fit_export <- lm(log_norm_export ~ log_norm_gdp, data = full_data)
fit_import <- lm(log_norm_import ~ log_norm_gdp, data = full_data)


ggplot(full_data, aes(x = log_norm_gdp, y = log_norm_export)) +
  geom_point(aes(color = COUNTRY), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_text(aes(label = paste("y =", round(coef(fit_export)[1], 2), "+", 
                              round(coef(fit_export)[2], 2), "x")),
            x = -1, y = 4, color = "red") +
  labs(title = "Relazione GDP-Export (UE→Corea del Sud 2019)",
       subtitle = paste("Paese mediano:", median_country, "- GDP:", round(median_gdp, 2), "miliardi USD"),
       x = "log(GDP normalizzato)",
       y = "log(Export normalizzato)",
       caption = "Fonte: IMF WEO & UN Comtrade") +
  theme_minimal() +
  theme(legend.position = "bottom")


ggplot(full_data, aes(x = log_norm_gdp, y = log_norm_import)) +
  geom_point(aes(color = COUNTRY), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_text(aes(label = paste("y =", round(coef(fit_import)[1], 2), "+", 
                              round(coef(fit_import)[2], 2), "x")),
            x = -1, y = 6, color = "blue") +
  labs(title = "Relazione GDP-Import (UE←Corea del Sud 2019)",
       subtitle = paste("Paese mediano:", median_country, "- GDP:", round(median_gdp, 2), "miliardi USD"),
       x = "log(GDP normalizzato)",
       y = "log(Import normalizzato)",
       caption = "Fonte: IMF WEO & UN Comtrade") +
  theme_minimal() +
  theme(legend.position = "bottom")


summary(fit_export)
summary(fit_import)



library(ggplot2)


if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)


gdp_data <- read.csv("GDP 2.csv", stringsAsFactors = FALSE)


print(colnames(gdp_data)) 


gdp_data <- data.frame(
  country = gdp_data$COUNTRY,
  gdp_2019 = gdp_data$X2019  # Modifica "X2019" in base al nome reale
)

gdp_data$country <- ifelse(gdp_data$country == "Slovak Republic", "Slovakia",
                           ifelse(gdp_data$country == "Czech Republic", "Czechia",
                                  ifelse(gdp_data$country == "Croatia, Republic of", "Croatia",
                                         ifelse(gdp_data$country == "Estonia, Republic of", "Estonia", 
                                                ifelse(gdp_data$country == "Netherlands, The", "Netherlands",
                                                       gdp_data$country)))))


gdp_data <- subset(gdp_data, !country %in% c("Luxembourg", "Cyprus"))


median_gdp <- median(gdp_data$gdp_2019, na.rm = TRUE)
median_country <- gdp_data$country[which.min(abs(gdp_data$gdp_2019 - median_gdp))]

print(paste("Mediana GDP:", median_gdp, "Paese mediano:", median_country))

library(readr)
trade_data <- read_csv("Trade data 2.csv")
trade_data <- read.csv("Trade data 2.csv", stringsAsFactors = FALSE)
trade_data <- subset(trade_data, refYear == 2019, 
                     select = c("partnerDesc", "flowDesc", "primaryValue"))
exports <- subset(trade_data, flowDesc == "Export", select = c("partnerDesc", "primaryValue"))
imports <- subset(trade_data, flowDesc == "Import", select = c("partnerDesc", "primaryValue"))

colnames(exports) <- c("country", "export_to_kor")
colnames(imports) <- c("country", "import_from_kor")


full_data <- merge(gdp_data, merge(exports, imports, by = "country"), by = "country")

full_data$norm_gdp <- full_data$gdp_2019 / median_gdp
full_data$norm_export <- full_data$export_to_kor / median(full_data$export_to_kor, na.rm = TRUE)
full_data$norm_import <- full_data$import_from_kor / median(full_data$import_from_kor, na.rm = TRUE)
full_data$log_norm_gdp <- log(full_data$norm_gdp)
full_data$log_norm_export <- log(full_data$norm_export)
full_data$log_norm_import <- log(full_data$norm_import)


fit_export <- lm(log_norm_export ~ log_norm_gdp, data = full_data)
fit_import <- lm(log_norm_import ~ log_norm_gdp, data = full_data)

summary(fit_export)
summary(fit_import)

library(ggplot2)
library(dplyr)


slope_exp <- round(coef(fit_export)[2], 2)
intercept_exp <- round(coef(fit_export)[1], 2)
r2_exp <- round(summary(fit_export)$r.squared, 2)


ggplot(full_data, aes(x = log_norm_gdp, y = log_norm_export)) +
  geom_point(size = 3, color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1) +
  geom_text(aes(label = COUNTRY), vjust = -0.8, size = 3) +
  annotate("text", x = min(full_data$log_norm_gdp), y = max(full_data$log_norm_export),
           hjust = 0, vjust = 1,
           label = paste0("y = ", intercept_exp, " + ", slope_exp, "x\nR² = ", r2_exp),
           color = "red", size = 4, fontface = "italic") +
  labs(
    title = "Export dalla Corea verso UE (2019)",
    subtitle = "log(Export normalizzato) ~ log(GDP normalizzato)",
    x = "log(GDP normalizzato)",
    y = "log(Export normalizzato)"
  ) +
  theme_minimal()


ggplot(full_data, aes(x = log_norm_gdp, y = log_norm_export)) +
  geom_point(size = 3, color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1) +
  geom_text(aes(label = COUNTRY), vjust = -0.8, size = 3, check_overlap = TRUE) +
  annotate("text", x = min(full_data$log_norm_gdp), y = max(full_data$log_norm_export),
           hjust = 0, vjust = 1,
           label = paste0("y = ", intercept_exp, " + ", slope_exp, "x\nR² = ", r2_exp),
           color = "red", size = 4, fontface = "italic") +
  labs(
    title = "Korea's Export to EU, 2019",
    x = "GDP (ROU = 1)",
    y = "Korea's 2019 Export "
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )








slope_imp <- round(coef(fit_import)[2], 2)
intercept_imp <- round(coef(fit_import)[1], 2)
r2_imp <- round(summary(fit_import)$r.squared, 2)


ggplot(full_data, aes(x = log_norm_gdp, y = log_norm_import)) +
  geom_point(size = 3, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 1) +
  geom_text(aes(label = COUNTRY), vjust = -0.8, size = 3) +
  annotate("text", x = min(full_data$log_norm_gdp), y = max(full_data$log_norm_import),
           hjust = 0, vjust = 1,
           label = paste0("y = ", intercept_imp, " + ", slope_imp, "x\nR² = ", r2_imp),
           color = "blue", size = 4, fontface = "italic") +
  labs(
    title = "Korea's Import from EU, 2019",
    x = "GDP (ROU = 1)",
    y = "Korea's 2019 Impoprt (ROU = 1)"
  ) +
  theme_minimal()

ggplot(full_data, aes(x = log_norm_gdp, y = log_norm_import)) +
  geom_point(size = 3, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkblue", linewidth = 1) +
  geom_text(aes(label = COUNTRY), vjust = -0.8, size = 3, check_overlap = TRUE) +
  annotate("text", x = min(full_data$log_norm_gdp), y = max(full_data$log_norm_import),
           hjust = 0, vjust = 1,
           label = paste0("y = ", intercept_imp, " + ", slope_imp, "x\nR² = ", r2_imp),
           color = "darkblue", size = 4, fontface = "italic") +
  labs(
    title = "Korea's Import from EU, 2019",
    x = "GDP (ROU = 1)",
    y = "Korea's Import from EU (normalized)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )


ggplot(full_data, aes(x = log_norm_gdp, y = log_norm_import)) +
  geom_point(size = 3, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkblue", linewidth = 1) +
  geom_text(aes(label = COUNTRY), vjust = -0.8, size = 3, check_overlap = TRUE) +
  annotate("text", x = min(full_data$log_norm_gdp), y = max(full_data$log_norm_import),
           hjust = 0, vjust = 1,
           label = paste0("y = ", intercept_imp, " + ", slope_imp, "x\nR² = ", r2_imp),
           color = "darkblue", size = 4, fontface = "italic") +
  labs(
    title = "Korea's Export from EU, 2019",
    x = "GDP (normalized, Romania = 1)",
    y = "Imports from Korea (normalized)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )






