# Carica librerie necessarie
library(dplyr)
library(ggplot2)
library(moments)  # Per skewness e curtosi

# 1. Carica dati
data <- read.csv("realestate_texas.csv")

# 1. Analisi delle variabili
str(data)
summary(data)
# City è categorica, year/month quantitative discrete, le altre quantitative continue o discrete

# 2. Indici di posizione, variabilità e forma

statistiche <- data %>%
  summarise(
    mean_year = mean(year),
    sd_year = sd(year),
    skew_year = skewness(year),
    kurt_year = kurtosis(year),
    
    mean_month = mean(month),
    sd_month = sd(month),
    skew_month = skewness(month),
    kurt_month = kurtosis(month),
    
    mean_sales = mean(sales),
    sd_sales = sd(sales),
    skew_sales = skewness(sales),
    kurt_sales = kurtosis(sales),
    
    mean_volume = mean(volume),
    sd_volume = sd(volume),
    skew_volume = skewness(volume),
    kurt_volume = kurtosis(volume),
    
    mean_median_price = mean(median_price),
    sd_median_price = sd(median_price),
    skew_median_price = skewness(median_price),
    kurt_median_price = kurtosis(median_price),
    
    mean_listings = mean(listings),
    sd_listings = sd(listings),
    skew_listings = skewness(listings),
    kurt_listings = kurtosis(listings),
    
    mean_months_inventory = mean(months_inventory),
    sd_months_inventory = sd(months_inventory),
    skew_months_inventory = skewness(months_inventory),
    kurt_months_inventory = kurtosis(months_inventory)
  )

print(statistiche)

# Frequenze city
freq_city <- table(data$city)
print(freq_city)

# 3. Variabili con maggiore variabilità e asimmetria
# Maggiore variabilità: confronto sd
sd_vars <- c(
  sales = sd(data$sales),
  volume = sd(data$volume),
  median_price = sd(data$median_price),
  listings = sd(data$listings),
  months_inventory = sd(data$months_inventory)
)
print(sd_vars)
max_var <- names(which.max(sd_vars))

# Maggiore asimmetria: confronto skewness assoluto
skew_vars <- c(
  sales = skewness(data$sales),
  volume = skewness(data$volume),
  median_price = skewness(data$median_price),
  listings = skewness(data$listings),
  months_inventory = skewness(data$months_inventory)
)
max_skew_var <- names(which.max(abs(skew_vars)))

cat("Variabile con maggiore variabilità:", max_var, "\n")
cat("Variabile con maggiore asimmetria:", max_skew_var, "\n")

# 4. Creazione classi per variabile quantitativa (sales)
breaks <- seq(min(data$sales), max(data$sales), length.out = 6)  # 5 classi
data$sales_class <- cut(data$sales, breaks = breaks, include.lowest = TRUE)
freq_sales_class <- table(data$sales_class)
print(freq_sales_class)

# Istogramma con ggplot2
ggplot(data, aes(x = sales_class)) + 
  geom_bar(fill = "steelblue") + 
  theme_minimal() + 
  labs(title = "Distribuzione delle vendite per classi", x = "Classe di vendite", y = "Frequenza")

# Calcolo Indice di Gini (eterogeneità)
gini_index <- function(x) {
  x <- sort(x)
  n <- length(x)
  G <- sum((2*(1:n) - n -1)*x) / (n*sum(x))
  return(G)
}
gini_sales <- gini_index(as.numeric(data$sales_class))
cat("Indice di Gini sulle classi di 'sales' (approssimativo):", gini_sales, "\n")

# 5. Calcolo probabilità

# P(city = "Beaumont")
p_beaumont <- mean(data$city == "Beaumont")

# P(month = 7)
p_july <- mean(data$month == 7)

# P(month = 12 & year = 2012)
p_dec_2012 <- mean(data$month == 12 & data$year == 2012)

cat("P(city=Beaumont):", p_beaumont, "\n")
cat("P(month=7):", p_july, "\n")
cat("P(month=12, year=2012):", p_dec_2012, "\n")

# 6. Creazione di una nuova variabile 

data <- data %>%
  mutate(
    prezzo_medio = volume * 1e6 / sales, # volume in milioni quindi moltiplico per 1e6
    efficacia_annunci = sales / listings
  )

summary(data$prezzo_medio)
summary(data$efficacia_annunci)

# 7. Analisi condizionata con dplyr
summary_cond <- data %>%
  group_by(city, year, month) %>%
  summarise(
    media_sales = mean(sales),
    sd_sales = sd(sales),
    media_volume = mean(volume),
    sd_volume = sd(volume),
    media_prezzo_medio = mean(prezzo_medio),
    sd_prezzo_medio = sd(prezzo_medio),
    media_efficacia = mean(efficacia_annunci),
    sd_efficacia = sd(efficacia_annunci),
    .groups = "drop"
  )
print(head(summary_cond))

# 8. Visualizzazioni con ggplot2

# Boxplot prezzi mediani per città
ggplot(data, aes(x = city, y = median_price)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Boxplot Prezzo Mediano per Città", x = "Città", y = "Prezzo Mediano") +
  theme_minimal()

# Boxplot volume vendite per città e anno
ggplot(data, aes(x = as.factor(year), y = volume, fill = city)) +
  geom_boxplot(position=position_dodge()) +
  labs(title = "Boxplot Volume Vendite per Anno e Città", x = "Anno", y = "Volume (milioni)") +
  theme_minimal()

# Grafico a barre sovrapposte vendite per mese e città
ggplot(data, aes(x = factor(month), y = sales, fill = city)) +
  geom_bar(stat="identity", position="stack") +
  labs(title = "Totale Vendite per Mese e Città", x = "Mese", y = "Vendite") +
  theme_minimal()

# Grafico a barre normalizzato
ggplot(data, aes(x = factor(month), y = sales, fill = city)) +
  geom_bar(stat="identity", position="fill") +
  labs(title = "Distribuzione Percentuale Vendite per Mese e Città", x = "Mese", y = "Percentuale") +
  theme_minimal()

# Line chart andamento vendite per città e anno mediane per mese (aggregato)
agg_line <- data %>%
  group_by(city, year, month) %>%
  summarise(sales_mediana = median(sales), .groups = "drop")

ggplot(agg_line, aes(x = month, y = sales_mediana, color = city)) +
  geom_line() +
  facet_wrap(~year) +
  labs(title = "Andamento mediano delle vendite per mese, anno e città",
       x = "Mese", y = "Vendite Median") +
  theme_minimal()

