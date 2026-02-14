library(readxl)
library(ggplot2)
library(dplyr)
ggplot(Plot_domanda_1, aes(x = `GDP normalized`, y = `Export normalized`, label = Country)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_text(vjust = -0.5, size = 3) +
  labs(
    title = "Export dalla Corea verso Paesi UE (log normalizzati)",
    x = "log(PIL normalizzato)",
    y = "log(Export normalizzato)"
  ) +
  theme_minimal()
ls()
class(Plot_domanda_1)
df <- as.data.frame(Plot_domanda_1)
names(df)
# Forziamo la conversione se serve
df <- as.data.frame(nome_dataset)  # sostituisci nome_dataset col tuo

# Eseguiamo il modello
mod <- lm(`Export normalized` ~ `GDP normalized`, data = df)

# Mostriamo i risultati
summary(mod)
slope <- round(coef(mod)[2], 3)
fit <- round(summary(mod)$r.squared, 2)
library(ggplot2)

ggplot(df, aes(x = `GDP normalized`, y = `Export normalized`, label = Country)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_text(vjust = -0.6, size = 3) +
  labs(
    title = "Export dalla Corea verso UE (log-log)",
    x = "log(PIL normalizzato)",
    y = "log(Export normalizzato)"
  ) +
  annotate("text", x = min(df$`GDP normalized`), y = max(df$`Export normalized`),
           label = paste0("slope = ", slope, "\nfit = ", fit), 
           hjust = 0, vjust = 1, size = 4, fontface = "italic") +
  theme_minimal()
# Regressione lineare su dati import
mod_import <- lm(`Import normalized` ~ `GDP normalized`, data = df)

# Estrai slope e fit (R-squared)
slope_import <- round(coef(mod_import)[2], 3)
fit_import <- round(summary(mod_import)$r.squared, 2)

# Crea grafico
library(ggplot2)

ggplot(df, aes(x = `GDP normalized`, y = `Import normalized`, label = Country)) +
  geom_point(color = "darkgreen", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "orange") +
  geom_text(vjust = -0.6, size = 3) +
  labs(
    title = "Import in Corea da Paesi UE (log-log)",
    x = "log(PIL normalizzato)",
    y = "log(Import normalizzato)"
  ) +
  annotate("text", 
           x = min(df$`GDP normalized`, na.rm = TRUE), 
           y = max(df$`Import normalized`, na.rm = TRUE),
           label = paste0("slope = ", slope_import, "\nfit = ", fit_import), 
           hjust = 0, vjust = 1, size = 4, fontface = "italic") +
  theme_minimal()

