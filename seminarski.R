library(dplyr)
library(tidyverse)
library(ggplot2)
library(scales)
library(nortest)
library(glmnet)
library(Metrics)
library(sf)
library(tigris)
library(randomForest)
library(caret)
data = read.csv("nyc-rolling-sales.csv")

nrow(data)
ncol(data)

str(data)
summary(data)
names(data)

#Mapa
#U R postoji jedan lep nacin da se prikazu mape sa korisnim podacima a to je uz pomoc paketa sf i tigris
data_pomocna = read.csv("nyc-rolling-sales.csv")
data_pomocna$SALE.PRICE <- as.numeric(gsub(",", "", data_pomocna$SALE.PRICE))
ny_counties <- counties(state = "NY", cb = TRUE, year = 2022)
ny_naselja <- ny_counties %>%
  filter(NAME %in% c("New York", "Bronx", "Kings", "Queens", "Richmond"))

nasa_naselja<- data.frame(
  BOROUGH = c(1,2,3,4,5),
  NAME = c("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island")
)
#ne poklapaju se imena tako da moramo da ih promenimo kao sto su u paketu
data_pomocna <- data_pomocna %>%
  mutate(
    BOROUGH = as.numeric(as.character(BOROUGH)),
    novo_ime = case_when(
      BOROUGH == 1 ~ "New York",
      BOROUGH == 2 ~ "Bronx",
      BOROUGH == 3 ~ "Kings",
      BOROUGH == 4 ~ "Queens",
      BOROUGH == 5 ~ "Richmond"
    )
  )

prosecna_cena_naselja <- data_pomocna %>%
  group_by(novo_ime) %>%
  summarise(prosek = mean(SALE.PRICE, na.rm = TRUE))

mapa <- ny_naselja

indeks <- match(ny_naselja$NAME, prosecna_cena_naselja$novo_ime)

avg_vrednost <- prosecna_cena_naselja$prosek[indeks]

mapa$prosek <- avg_vrednost

ggplot(mapa) +
  geom_sf(aes(fill = prosek)) +
  scale_fill_viridis_c(labels = comma) +
  labs(
    title = "Prosečna cena nekretnina po borough-u (NYC)",
    fill = "Prosečna cena"
  ) +
  theme_minimal()
#Ovim grafikom mozemo da vidimo da methetn prednjaci sa cenama nekretnina


summary(data$SALE.PRICE)

#1
data <- data[, -1]

#2 plus output
colSums(is.na(data))


#3
data$EASE.MENT <- NULL


head(data)


#4 Provera za " " i sto je na istim redovima
colSums(data == " ")

#Provera da li je na istim redovima
isti_redovi <- (data$TAX.CLASS.AT.PRESENT == " ") & (data$BUILDING.CLASS.AT.PRESENT == " ")
sum(isti_redovi)

#imamo at time of sale za svaki

#provera da li su koncentrisani u jednoj oblasti
data_fali <- data[isti_redovi, ]
table(data_fali$BOROUGH)
sort(table(data_fali$NEIGHBORHOOD), decreasing = TRUE)
#nisu koncentrisani

#da li je ista building class kategorija
head(sort(table(data_fali$BUILDING.CLASS.CATEGORY), decreasing = TRUE), 15)
#veliki procenat pripada CONDOS


#apartment number missing vrednosti
sum(data$APARTMENT.NUMBER == " ")


#5 apartment number po klasama nekretnina
sort(table(data$BUILDING.CLASS.CATEGORY[data$APARTMENT.NUMBER == " "]),decreasing = TRUE)


#uklanjanje prethodno obradjenih kolona sa nedostajucim vrednostima
data <- data %>% 
  select(-TAX.CLASS.AT.PRESENT, -BUILDING.CLASS.AT.PRESENT, -APARTMENT.NUMBER)

#pronalazenje missing vrednosti u vidu "-"
head(data$SALE.PRICE, 10)

colSums(data == "-")

crtica=data$SALE.PRICE[2]
colSums(data == crtica)

#ponovni ispis tipova kolona
str(data[, c("LAND.SQUARE.FEET", "GROSS.SQUARE.FEET", "SALE.PRICE")])

#Hendlovanje - u SALE.PRICE
data$SALE.PRICE[data$SALE.PRICE == crtica] <- NA
colSums(data == crtica, na.rm = TRUE)

#Hendlovanje - u ostale dve kolone LAND.SQUARE.FEET i GROSS.SQUARE.FEET
data$LAND.SQUARE.FEET[data$LAND.SQUARE.FEET == crtica] <- NA
data$GROSS.SQUARE.FEET[data$GROSS.SQUARE.FEET == crtica] <- NA

colSums(data == crtica, na.rm = TRUE)

#Konverzija iz char u numeric tip
data$SALE.PRICE <- as.numeric(data$SALE.PRICE)
data$LAND.SQUARE.FEET <- as.numeric(data$LAND.SQUARE.FEET)
data$GROSS.SQUARE.FEET <- as.numeric(data$GROSS.SQUARE.FEET)
str(data[, c("LAND.SQUARE.FEET", "GROSS.SQUARE.FEET", "SALE.PRICE")])

#Uklanjanje kolone ADDRESS
data$ADDRESS <- NULL

#Uklanjanje redova sa NA vrednoscu u SALE.PRICE koloni
brojNA_SP <- sum(is.na(data$SALE.PRICE))
(brojNA_SP / nrow(data)) * 100

data <- data[!is.na(data$SALE.PRICE), ]
sum(is.na(data$SALE.PRICE))

#Promena tipova kolona

str(data)

#nominalne kolone
data$NEIGHBORHOOD <- as.factor(data$NEIGHBORHOOD)
data$BUILDING.CLASS.CATEGORY <- as.factor(data$BUILDING.CLASS.CATEGORY)
data$BUILDING.CLASS.AT.TIME.OF.SALE <- as.factor(data$BUILDING.CLASS.AT.TIME.OF.SALE)
data$BOROUGH <- as.factor(data$BOROUGH)
data$ZIP.CODE <- as.factor(data$ZIP.CODE)

#ordinalne kolone
data$TAX.CLASS.AT.TIME.OF.SALE <- factor(
  as.character(data$TAX.CLASS.AT.TIME.OF.SALE),
  levels = c("1", "2", "3", "4"),
  ordered = TRUE
)

#date kolona
data$SALE.DATE <- as.Date(data$SALE.DATE)

#numericke kolone
data$RESIDENTIAL.UNITS <- as.numeric(data$RESIDENTIAL.UNITS)
data$COMMERCIAL.UNITS  <- as.numeric(data$COMMERCIAL.UNITS)
data$TOTAL.UNITS       <- as.numeric(data$TOTAL.UNITS)
data$YEAR.BUILT        <- as.numeric(data$YEAR.BUILT)

#analiza SALE.PRICE
summary(data$SALE.PRICE)

ggplot(data, aes(x = SALE.PRICE)) +
  geom_histogram(binwidth = 50000,  
                 fill = "blue",
                 color = "black",
                 alpha = 0.7) +
  scale_x_continuous(labels = comma, limits = c(0, 5000000)) +
  coord_cartesian(ylim = c(0, 4000)) +
  labs(
    title = "Distribucija prodajne cene",
    x = "Prodajna cena",
    y = "Broj nekretnina"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#Uklanjanje duplih redova
nrow(data)
sum(duplicated(data))

key <- with(data, paste(BOROUGH, BLOCK, LOT, SALE.DATE, SALE.PRICE, sep = "_"))
sum(duplicated(key))

data <- distinct(data, BOROUGH, BLOCK, LOT, SALE.DATE, SALE.PRICE, .keep_all = TRUE)
nrow(data)

#Univarijantna analiza i analiza outliera


#Izbacivanje nelogicnih vrednosti

data <- data %>%
  filter(!is.na(SALE.PRICE), SALE.PRICE > 0)

data <- data %>%
  filter(is.na(GROSS.SQUARE.FEET) | GROSS.SQUARE.FEET > 0) %>%
  filter(is.na(LAND.SQUARE.FEET)  | LAND.SQUARE.FEET  > 0)

data <- data %>%
  filter(is.na(TOTAL.UNITS)        | TOTAL.UNITS        >= 0) %>%
  filter(is.na(RESIDENTIAL.UNITS)  | RESIDENTIAL.UNITS  >= 0) %>%
  filter(is.na(COMMERCIAL.UNITS)   | COMMERCIAL.UNITS   >= 0)

#YEAR.BUILT
ggplot(data, aes(y = YEAR.BUILT)) +
  geom_boxplot(outlier_alpha = 0.5) +
  labs(
    title = "Boxplot za YEAR.BUILT",
    y = "YEAR.BUILT",
    x = ""
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

trenutna_godina <- as.integer(format(Sys.Date(), "%Y"))

data <- data %>%
  filter(is.na(YEAR.BUILT) | (YEAR.BUILT >= 1800 & YEAR.BUILT <= trenutna_godina))

#TOTAL.UNITS

ggplot(data, aes(x = TOTAL.UNITS)) +
  geom_histogram(binwidth = 1, boundary = 0, color = "black", alpha = 0.7) +
  coord_cartesian(xlim = c(0, 20)) +
  labs(title = "Raspodela TOTAL.UNITS") +
  theme_minimal()

#RESIDENTIAL.UNITS

ggplot(data, aes(x = RESIDENTIAL.UNITS)) +
  geom_histogram(binwidth = 1, boundary = 0, color = "black", alpha = 0.7) +
  coord_cartesian(xlim = c(0, 15)) +
  labs(title = "Raspodela RESIDENTIAL.UNITS") +
  theme_minimal()

#COMMERCIAL.UNITS

ggplot(data, aes(x = COMMERCIAL.UNITS)) +
  geom_histogram(binwidth = 1, boundary = 0, color = "black", alpha = 0.7) +
  coord_cartesian(xlim = c(0, 10)) +
  labs(title = "Raspodela COMMERCIAL.UNITS") +
  theme_minimal()

#LAND.SQUARE.FEET

#raspodela pre logaritamske transformacije
ggplot(data, aes(x = LAND.SQUARE.FEET)) +
  geom_histogram(bins = 60, color = "black", alpha = 0.7) +
  scale_x_log10() +
  labs(title = "LAND.SQUARE.FEET",
       x = "LAND.SQUARE.FEET (log10 skala)", y = "Broj") +
  theme_minimal()


#GROSS.SQUARE.FEET

#raspodela pre logaritamske transformacije
ggplot(data, aes(x = GROSS.SQUARE.FEET)) +
  geom_histogram(bins = 60, color = "black", alpha = 0.7) +
  scale_x_log10() +
  labs(title = "GROSS.SQUARE.FEET",
       x = "GROSS.SQUARE.FEET (log10 skala)", y = "Broj") +
  theme_minimal()

data <- data %>%
  mutate(
    log_sale_price = log10(SALE.PRICE),
    log_gross_sqft = ifelse(is.na(GROSS.SQUARE.FEET), NA, log10(GROSS.SQUARE.FEET + 1)),
    log_land_sqft  = ifelse(is.na(LAND.SQUARE.FEET),  NA, log10(LAND.SQUARE.FEET + 1)))

ggplot(data %>% filter(!is.na(log_sale_price)), aes(x = log_sale_price)) +
  geom_histogram(bins = 40, fill = "blue", color = "black", alpha = 0.7) +
  scale_x_continuous(
    breaks = pretty_breaks(n = 10)
  ) +
  labs(
    title = "Distribucija log_sale_price",
    x = "log10(SALE.PRICE)",
    y = "Broj nekretnina"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#raspodele posle logaritamske transformacije
ggplot(data, aes(x = log_land_sqft)) +
  geom_histogram(bins = 50, color = "black", alpha = 0.7) +
  labs(title = "log_land_sqft", x = "log10(LAND.SQUARE.FEET + 1)", y = "Broj") +
  theme_minimal()

ggplot(data, aes(x = log_gross_sqft)) +
  geom_histogram(bins = 50, color = "black", alpha = 0.7) +
  labs(title = "log_gross_sqft", x = "log10(GROSS.SQUARE.FEET + 1)", y = "Broj") +
  theme_minimal()

#BOROUGH

ggplot(data, aes(x = BOROUGH, fill = BOROUGH)) +
  geom_bar(alpha = 0.8) +
  labs(title = "Broj zapisa po BOROUGH", x = "Borough", y = "Broj") +
  theme_minimal() +
  theme(legend.position = "none")

#BUILDING.CLASS.CATEGORY

top10 <- data %>%
  count(BUILDING.CLASS.CATEGORY, sort = TRUE) %>%
  head(10)

ggplot(top10, aes(x = reorder(BUILDING.CLASS.CATEGORY, n), y = n, fill = BUILDING.CLASS.CATEGORY)) +
  geom_col(alpha = 0.8) +
  coord_flip() +
  labs(title = "BUILDING.CLASS.CATEGORY (Top 10)", x = "", y = "Broj zapisa") +
  theme_minimal() +
  theme(legend.position = "none")

#NEIGHBORHOOD

top10_nbh <- data %>%
  count(NEIGHBORHOOD, sort = TRUE) %>%
  head(10)

ggplot(top10_nbh, aes(x = reorder(NEIGHBORHOOD, n), y = n, fill = NEIGHBORHOOD)) +
  geom_col(alpha = 0.8) +
  coord_flip() +
  labs(title = "NEIGHBORHOOD (Top 10)", x = "", y = "Broj zapisa") +
  theme_minimal() +
  theme(legend.position = "none")

#Odnosi SALE.PRICE sa ostalim kolonama

#Numericke kolone

#1 GROSS.SQUARE.FEET

#log_sale_price vs log_gross_sqft
ggplot(data, aes(x = log_gross_sqft, y = log_sale_price)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title="Grafik odnosa SALE.PRICE i GROSS.SQUARE.FEET",
       x="log10(GROSS.SQUARE.FEET+1)", y="log10(SALE.PRICE)") +
  theme_minimal()

#Uklanjanje administrativnih prodaja i ostavljanje validnih vrednosti za SALE.PRICE
data <- data %>%
  filter(SALE.PRICE > 10000)

#Pregledniji grafik
ggplot(data, aes(x = log_gross_sqft, y = log_sale_price)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title="Grafik odnosa SALE.PRICE i GROSS.SQUARE.FEET",
       x="log10(GROSS.SQUARE.FEET+1)", y="log10(SALE.PRICE)") +
  theme_minimal()

data <- data %>%
  filter(log_gross_sqft <= 6.2 | is.na(log_gross_sqft))


#2 LAND.SQUARE.FEET

ggplot(data, aes(x = log_land_sqft, y = log_sale_price)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title="Grafik odnosa SALE.PRICE i LAND.SQUARE.FEET",
       x="log10(LAND.SQUARE.FEET+1)", y="log10(SALE.PRICE)") +
  theme_minimal()

# Uklanjanje samo ekstremno velikih vrednosti 
data <- data %>%
  filter(log_land_sqft <= 5.3 | is.na(log_land_sqft))

#3 YEAR.BUILT

ggplot(data, aes(x = YEAR.BUILT, y = log_sale_price)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title="Grafik odnosa SALE.PRICE i YEAR.BUILT",
       x="YEAR.BUILT", y="log10(SALE.PRICE)") +
  theme_minimal()

#Brisanje starih zgrada
data <- data %>%
  filter(is.na(YEAR.BUILT) | YEAR.BUILT >= 1900)

#4 TOTAL.UNITS
ggplot(data, aes(x = TOTAL.UNITS, y = log_sale_price)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title="SALE.PRICE vs TOTAL.UNITS",
       x="TOTAL.UNITS", y="log10(SALE.PRICE)") +
  theme_minimal()

#brisanje jedinstvene tacke
data <- data %>%
  filter(is.na(TOTAL.UNITS) | TOTAL.UNITS <= 2000)

#Kategorijske kolone

#1 BOROUGH
ggplot(data, aes(x = BOROUGH, y = SALE.PRICE)) +
  geom_boxplot(outlier_alpha = 0.3) +
  scale_y_log10(labels = comma) +
  labs(title="SALE.PRICE po BOROUGH", x="BOROUGH", y="SALE.PRICE") +
  theme_minimal()

#2 BUILDING.CLASS.CATEGORY

#Top 10 kategorija
top10_kat <- data %>%
  count(BUILDING.CLASS.CATEGORY, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(BUILDING.CLASS.CATEGORY)

ggplot(filter(data, BUILDING.CLASS.CATEGORY %in% top10_kat),
       aes(x = BUILDING.CLASS.CATEGORY, y = SALE.PRICE)) +
  geom_boxplot(outlier_alpha = 0.25) +
  scale_y_log10(labels = comma) +
  labs(title="SALE.PRICE po BUILDING.CLASS.CATEGORY",
       x="Kategorija", y="SALE.PRICE") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#3 BUILDING.CLASS.AT.TIME.OF.SALE
top10_klasa_nekretnine <- names(
  sort(table(data$BUILDING.CLASS.AT.TIME.OF.SALE), decreasing = TRUE)[1:10]
)

data_top10_pomocna <- data %>%
  filter(BUILDING.CLASS.AT.TIME.OF.SALE %in% top10_klasa_nekretnine)

ggplot(data_top10_pomocna,
       aes(x = BUILDING.CLASS.AT.TIME.OF.SALE, y = SALE.PRICE)) +
  geom_boxplot(outlier_alpha = 0.3) +
  scale_y_log10(labels = comma) +
  labs(
    title = "SALE.PRICE BUILDING.CLASS.AT.TIME.OF.SALE",
    x = "BUILDING.CLASS.AT.TIME.OF.SALE",
    y = "SALE.PRICE"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#4 TAX.CLASS.AT.TIME.OF.SALE

ggplot(data, aes(x = TAX.CLASS.AT.TIME.OF.SALE, y = SALE.PRICE)) +
  geom_boxplot(outlier_alpha = 0.3) +
  scale_y_log10(labels = comma) +
  labs(title="SALE.PRICE po TAX.CLASS.AT.TIME.OF.SALE",
       x="TAX.CLASS.AT.TIME.OF.SALE", y="SALE.PRICE") +
  theme_minimal()


#Multivarijantna analiza

#SALE.PRICE, GROSS.SQUARE.FEET i BOROUGH

ggplot(data, aes(x = log_gross_sqft, 
                 y = log_sale_price, 
                 color = BOROUGH)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Odnos povrsine i cene po opštinama",
    x = "log GROSS.SQUARE.FEET",
    y = "log SALE.PRICE",
    color = "BOROUGH"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

#SALE.PRICE, LAND.SQUARE.FEET i BOROUGH

ggplot(data, aes(x = log_land_sqft, 
                 y = log_sale_price, 
                 color = BOROUGH)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Odnos povrsine i cene po opštinama",
    x = "log LAND.SQUARE.FEET",
    y = "log SALE.PRICE",
    color = "BOROUGH"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )



#Imputacija vrednosti

#Broj NA vrednosti u kolonama pre imputacije
sum(is.na(data$LAND.SQUARE.FEET))
sum(is.na(data$GROSS.SQUARE.FEET))

#Pokusaj Shapiro testa normalnosti
#shapiro.test(data$LAND.SQUARE.FEET)

#Koriscenje Anderson-Darling testa normalnosti
ad.test(na.omit(data$LAND.SQUARE.FEET))
ad.test(na.omit(data$GROSS.SQUARE.FEET))

# flag (da model zna da je bila imputacija)
data <- data %>%
  mutate(
    imp_land = as.integer(is.na(LAND.SQUARE.FEET)),
    imp_gross = as.integer(is.na(GROSS.SQUARE.FEET))
  )

# medijane po grupi (BOROUGH + BUILDING.CLASS.CATEGORY)
grp_medians <- data %>%
  group_by(BOROUGH, BUILDING.CLASS.CATEGORY) %>%
  summarise(
    med_land  = median(LAND.SQUARE.FEET,  na.rm = TRUE),
    med_gross = median(GROSS.SQUARE.FEET, na.rm = TRUE),
    .groups = "drop"
  )

# global fallback medijane (ako neka grupa nema nijednu poznatu vrednost)
global_med_land  <- median(data$LAND.SQUARE.FEET,  na.rm = TRUE)
global_med_gross <- median(data$GROSS.SQUARE.FEET, na.rm = TRUE)

data <- data %>%
  left_join(grp_medians, by = c("BOROUGH", "BUILDING.CLASS.CATEGORY")) %>%
  mutate(
    LAND.SQUARE.FEET  = ifelse(is.na(LAND.SQUARE.FEET),
                               ifelse(is.na(med_land),  global_med_land,  med_land),
                               LAND.SQUARE.FEET),
    GROSS.SQUARE.FEET = ifelse(is.na(GROSS.SQUARE.FEET),
                               ifelse(is.na(med_gross), global_med_gross, med_gross),
                               GROSS.SQUARE.FEET)
  ) %>%
  select(-med_land, -med_gross)

#Broj NA vrednosti u kolonama posle imputacije
sum(is.na(data$LAND.SQUARE.FEET))
sum(is.na(data$GROSS.SQUARE.FEET))

#EDA

#gsf i sale
ggplot(data, aes(x = GROSS.SQUARE.FEET, y = SALE.PRICE)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE)+
  scale_y_continuous(labels = comma) +
  labs(title = "GROSS.SQUARE.FEET i SALE.PRICE",
       x = "Bruto površina",
       y = "Prodajna cena") +
  theme_minimal()

ggplot(data, aes(x = GROSS.SQUARE.FEET, y = SALE.PRICE)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(labels = comma) +
  coord_cartesian(xlim = c(0, 20000),
                  ylim = c(0, 5000000)) +
  labs(title = "GROSS.SQUARE.FEET i SALE.PRICE",
       x = "Bruto površina",
       y = "Prodajna cena") +
  theme_minimal()

cor.test(data$log_gross_sqft, data$log_sale_price)
#p je manje od 0.05 tako da je znacajno obelezje

#lsf i sale
ggplot(data, aes(x = log_land_sqft, y = log_sale_price)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "log(LAND.SQUARE.FEET) vs log(SALE.PRICE)",
    x = "log(1 + LAND.SQUARE.FEET)",
    y = "log(1 + SALE.PRICE)"
  ) +
  theme_minimal()

#lsf i sale zumiran
xlim <- quantile(data$log_land_sqft, probs = c(0.01, 0.99), na.rm = TRUE)
ylim <- quantile(data$log_sale_price, probs = c(0.01, 0.99), na.rm = TRUE)

ggplot(data, aes(x = log_land_sqft, y = log_sale_price)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  coord_cartesian(xlim = xlim, ylim = ylim) +
  labs(
    title = "log(LAND.SQUARE.FEET) vs log(SALE.PRICE) (1–99% zoom)",
    x = "log(1 + LAND.SQUARE.FEET)",
    y = "log(1 + SALE.PRICE)"
  ) +
  theme_minimal()

cor.test(data$log_land_sqft, data$log_sale_price)
#p je manje od 0.05 tako da je znacajno obelezje

#opstina i sale
ggplot(data, aes(x = BOROUGH, y = log_sale_price)) +
  geom_boxplot() +
  scale_y_continuous(labels = comma) +
  labs(title = "SALE.PRICE po BOROUGH",
       x = "BOROUGH",
       y = "Prodajna cena") +
  theme_minimal()

anova_borough <- aov(log_sale_price ~ BOROUGH, data = data)
summary(anova_borough)

#klasa zgrade
top10_kategorije <- names(
  sort(table(data$BUILDING.CLASS.CATEGORY), decreasing = TRUE)[1:10]
)

data_top10_pomocna <- data %>%
  filter(BUILDING.CLASS.CATEGORY %in% top10_kategorije)

ggplot(data_top10_pomocna,
       aes(x = BUILDING.CLASS.CATEGORY, y = log_sale_price)) +
  geom_boxplot(outlier_alpha = 0.3) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "SALE.PRICE po BUILDING.CLASS.CATEGORY (Top 10)",
    x = "BUILDING.CLASS.CATEGORY",
    y = "SALE.PRICE"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#klasa pri kupovini i sale

top10_kalsa_nekretnine <- names(
  sort(table(data$BUILDING.CLASS.AT.TIME.OF.SALE), decreasing = TRUE)[1:10]
)

data_top10_pomocna <- data %>%
  filter(BUILDING.CLASS.AT.TIME.OF.SALE %in% top10_kalsa_nekretnine)

ggplot(data_top10_pomocna,
       aes(x = BUILDING.CLASS.AT.TIME.OF.SALE, y = log_sale_price)) +
  geom_boxplot(outlier_alpha = 0.3) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "SALE.PRICE po BUILDING.CLASS.AT.TIME.OF.SALE (Top 10)",
    x = "BUILDING.CLASS.AT.TIME.OF.SALE",
    y = "SALE.PRICE"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#dodavanje dve nove kolone za starost i mesec
data <- data %>%
  mutate(
    starost = as.integer(format(SALE.DATE, "%Y")) - YEAR.BUILT,
    mesec = factor(as.integer(format(SALE.DATE, "%m"))),
    godina = as.factor(format(SALE.DATE, "%Y"))
  )

#Broj transakcija po mesecu
ggplot(data, aes(x = mesec)) +
  geom_bar(alpha = 0.7) +
  labs(
    title = "Broj transakcija po mesecu",
    x = "Mesec",
    y = "Broj prodaja"
  ) +
  theme_minimal()

#Broj transakcija po godini
ggplot(data, aes(x = godina)) +
  geom_bar(alpha = 0.7) +
  labs(
    title = "Broj transakcija po godini",
    x = "Godina",
    y = "Broj prodaja"
  ) +
  theme_minimal()

#mesec i sale
ggplot(data, aes(x = mesec, y = log_sale_price)) +
  geom_boxplot(outlier_alpha = 0.3) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "SALE.PRICE po mesecu",
    x = "Mesec",
    y = "SALE.PRICE"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#starost i sale
ggplot(data, aes(x = starost, y = log_sale_price)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(labels = comma) +
  labs(title = "SALE.PRICE po starosti",
       x = "Starost",
       y = "Prodajna cena") +
  theme_minimal()

#stambene jedinice i cena
ggplot(data, aes(x = RESIDENTIAL.UNITS, y = SALE.PRICE)) +
  geom_point(alpha = 0.3) +
  coord_cartesian(xlim = c(0, 50),
                  ylim = c(0, 5000000)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "SALE.PRICE po broju stambenih jedinica",
       x = "Broj stambenih jedinica",
       y = "Prodajna cena") +
  theme_minimal()

#komercijalne jedinice i cena
ggplot(data, aes(x = COMMERCIAL.UNITS, y = SALE.PRICE)) +
  geom_point(alpha = 0.3) +
  coord_cartesian(xlim = c(0, 50),
                  ylim = c(0, 5000000)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "SALE.PRICE po broju komercijalnih jedinica",
       x = "Broj komercijalnih jedinica",
       y = "Prodajna cena") +
  theme_minimal()

#ukupan broj jedinica i cena
ggplot(data, aes(x = TOTAL.UNITS, y = SALE.PRICE)) +
  geom_point(alpha = 0.3) +
  coord_cartesian(xlim = c(0, 100),
                  ylim = c(0, 5000000)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "SALE.PRICE po ukupnom broju jedinica",
       x = "Ukupan broj jedinica",
       y = "Prodajna cena") +
  theme_minimal()


#Feature engineering

#Agregacija BUILDING.CLASS.CATEGORY u sire grupe

#Podrazumevana grupa
data$grupa_nekretnine <- "OTHER"  

#Porodicne kuce
data$grupa_nekretnine[grepl("ONE FAMILY", data$BUILDING.CLASS.CATEGORY)]   <- "ONE_FAMILY"
data$grupa_nekretnine[grepl("TWO FAMILY", data$BUILDING.CLASS.CATEGORY)]   <- "TWO_FAMILY"
data$grupa_nekretnine[grepl("THREE FAMILY", data$BUILDING.CLASS.CATEGORY)] <- "THREE_FAMILY"

#Condos i coops
data$grupa_nekretnine[grepl("CONDO", data$BUILDING.CLASS.CATEGORY)] <- "CONDO"
data$grupa_nekretnine[grepl("COOP",  data$BUILDING.CLASS.CATEGORY)] <- "COOP"
data$grupa_nekretnine[grepl("COOPS", data$BUILDING.CLASS.CATEGORY)] <- "COOP"

#Rentals
data$grupa_nekretnine[grepl("RENTAL",  data$BUILDING.CLASS.CATEGORY)] <- "RENTAL"
data$grupa_nekretnine[grepl("RENTALS", data$BUILDING.CLASS.CATEGORY)] <- "RENTAL"

#Commercial (store, office, hotels, warehouses, factories...)
data$grupa_nekretnine[grepl("STORE", data$BUILDING.CLASS.CATEGORY)]      <- "COMMERCIAL"
data$grupa_nekretnine[grepl("OFFICE", data$BUILDING.CLASS.CATEGORY)]     <- "COMMERCIAL"
data$grupa_nekretnine[grepl("HOTEL", data$BUILDING.CLASS.CATEGORY)]      <- "COMMERCIAL"
data$grupa_nekretnine[grepl("WAREHOUSE", data$BUILDING.CLASS.CATEGORY)]  <- "COMMERCIAL"
data$grupa_nekretnine[grepl("FACTORY", data$BUILDING.CLASS.CATEGORY)]    <- "COMMERCIAL"
data$grupa_nekretnine[grepl("INDUSTRIAL", data$BUILDING.CLASS.CATEGORY)] <- "COMMERCIAL"
data$grupa_nekretnine[grepl("COMMERCIAL", data$BUILDING.CLASS.CATEGORY)] <- "COMMERCIAL"

#Pretvaramo u factor za model
data$grupa_nekretnine <- as.factor(data$grupa_nekretnine)

#Provera raspodele novih grupa
table(data$grupa_nekretnine)

#Grupe nekretnine
ggplot(data_fs, aes(x = grupa_nekretnine, y = log_sale_price)) +
  geom_boxplot(alpha = 0.3) +
  labs(
    title = "SALE.PRICE po grupa_nekretnine",
    x = "Grupa nekretnine",
    y = "log SALE.PRICE"
  ) +
  theme_minimal() 

anova_grupan <- aov(log_sale_price ~ grupa_nekretnine, data = data_fs)
summary(anova_grupan)

#Ponovno logovanje zbog imputed vrednosti za GROSS.SQUARE.FEET i LAND.SQUARE.FEET
data <- data %>%
  mutate(
    log_gross_sqft = log10(GROSS.SQUARE.FEET + 1),
    log_land_sqft  = log10(LAND.SQUARE.FEET + 1))

#Iskoriscenost parcele = bruto povrsina / povrsina zemljista
data <- data %>%
  mutate(isk_parcele = log_gross_sqft / log_land_sqft)

#Iskoriscenost parcele grafik
ggplot(data_fs, aes(x = isk_parcele, y = log_sale_price)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Odnos iskoriscenosti parcele i prodajne cene",
    x = "Iskoriscenost parcele",
    y = "log SALE.PRICE"
  ) 

#Kombinacija BOROUGH i grupa_nekretnine
data <- data %>%
  mutate(borough_group = interaction(BOROUGH, grupa_nekretnine, drop = TRUE))

#BOROUGH i grupa_nekretnine grafik
ggplot(data_fs, aes(x = borough_group, y = log_sale_price)) +
  geom_boxplot(outlier_alpha = 0.2) +
  labs(
    title = "SALE.PRICE po borough_group",
    x = "BOROUGH × grupa_nekretnine",
    y = "log10(SALE.PRICE)"
  ) +
  theme_minimal() 
anova_bg <- aov(log_sale_price ~ borough_group, data = data_fs)
summary(anova_bg)

#RESIDENTIAL.UNITS grupisanje
data$res_units_grupa <- NA

data$res_units_grupa[data$RESIDENTIAL.UNITS == 0] <- "0"
data$res_units_grupa[data$RESIDENTIAL.UNITS == 1] <- "1"
data$res_units_grupa[data$RESIDENTIAL.UNITS %in% c(2, 3)] <- "2-3"
data$res_units_grupa[data$RESIDENTIAL.UNITS >= 4] <- "4+"

# Faktor transformacija
data$res_units_grupa <- factor(data$res_units_grupa, levels = c("0", "1", "2-3", "4+"))

#grupa po jedinicama grafik
ggplot(data, aes(x = res_units_grupa, y = log_sale_price)) +
  geom_boxplot(alpha = 0.6) +
  labs(
    title = "Prodajna cena u odnosu na broj stambenih jedinica",
    x = "Broj stambenih jedinica",
    y = "log SALE.PRICE"
  ) +
  theme_minimal() 

#COMMERCIAL.UNITS binarni indikator
data$ima_komercijalne <- ifelse(is.na(data$COMMERCIAL.UNITS), 0,
                                ifelse(data$COMMERCIAL.UNITS > 0, 1, 0))
data$ima_komercijalne <- factor(data$ima_komercijalne, levels = c(0, 1))

#komericjalne jedinice grafik
ggplot(data, aes(x = ima_komercijalne, y = log_sale_price)) +
  geom_boxplot(alpha = 0.6) +
  labs(
    title = "Uticaj komercijalnih jedinica na cenu",
    x = "Ima komercijalne jedinice (0 = Ne, 1 = Da)",
    y = "log SALE.PRICE"
  ) +
  theme_minimal()

#Feature selection

data_fs <- data %>%
  select(
    SALE.PRICE, log_sale_price, GROSS.SQUARE.FEET, log_gross_sqft, LAND.SQUARE.FEET, BUILDING.CLASS.CATEGORY ,log_land_sqft,
    TOTAL.UNITS, RESIDENTIAL.UNITS, COMMERCIAL.UNITS, BOROUGH,
    grupa_nekretnine, isk_parcele, borough_group,ima_komercijalne,res_units_grupa
  )

#Rad sa modelima

#Train test split
train_index <- sample(seq_len(nrow(data_fs)), size = 0.8 * nrow(data_fs))
train <- data_fs[train_index, ]
test  <- data_fs[-train_index, ]

numericki <- train %>%
  select(where(is.numeric))

cor_matrix <- cor(numericki, use = "complete.obs")
round(cor_matrix, 2)
#Prvi model uzimamo vrednost koja ima najvecu korelaciju 
model1 <- lm(log_sale_price ~ log_gross_sqft, data = train)
pred1 <- predict(model1, newdata = test)
rmse(test$log_sale_price, pred1)
summary(model1)

#drugi model uzimamo sve numericke koje nismo dobili preko FE

model2 <- lm(
  log_sale_price ~ 
    log_gross_sqft +
    log_land_sqft +
    TOTAL.UNITS,
  data = train
)
summary(model2)
pred2 <- predict(model2, newdata = test)
rmse(test$log_sale_price, pred2)

#Treci model uzecemo neke od nasih FE vrednosti i probati da dobijemo bolji model
model3 <- lm(
  log_sale_price ~
    isk_parcele * grupa_nekretnine +
    factor(ima_komercijalne) +
    factor(res_units_grupa) 
  ,
  data = train
)
summary(model3)
pred3 <- predict(model3, newdata = test)
rmse(test$log_sale_price, pred3)

#Model koji se najbolje pokazao 

model4 <- lm(
  log_sale_price ~ 
    log_gross_sqft * BOROUGH + 
    log_land_sqft + 
    factor(ima_komercijalne) + 
    factor(res_units_grupa) + 
    factor(BUILDING.CLASS.CATEGORY),
  data = train
)

pred4 <- predict(model4, newdata = test)
#Metrike najboljeg modela 
rmse(test$log_sale_price, pred4)
mae(test$log_sale_price, pred4)
summary(model4)


#ridge i lasso

data_pomocna_regularizacija  <- data_fs %>%
  select(
    log_sale_price, log_gross_sqft, BUILDING.CLASS.CATEGORY ,log_land_sqft,
    RESIDENTIAL.UNITS, COMMERCIAL.UNITS, BOROUGH
  )

data_pomocna_regularizacija <- drop_na(data_pomocna_regularizacija)

train <- data_pomocna_regularizacija[train_index, ]
test  <- data_pomocna_regularizacija[-train_index, ]

x_train <- model.matrix(log_sale_price ~ ., data = train)[, -1]
y_train <- train$log_sale_price

x_test <- model.matrix(log_sale_price ~ ., data = test)[, -1]
x_test <- x_test[, colnames(x_train)]
y_test <- test$log_sale_price

ridge_cv <- cv.glmnet(x_train, y_train, alpha = 0)
lasso_cv <- cv.glmnet(x_train, y_train, alpha = 1)

ridge_cv$lambda.min

lasso_cv$lambda.min

ridge_preds = predict(ridge_cv, newx = x_test, s = "lambda.min")
lasso_preds = predict(lasso_cv, newx = x_test, s = "lambda.min")

rmse(y_test, lasso_preds)
mae(y_test, lasso_preds)

tss <- sum((y_test - mean(y_test))^2)
rss_lasso <- sum((y_test - lasso_preds)^2)
r2_lasso <- 1 - rss_lasso/tss
r2_lasso

rmse(y_test, ridge_preds)
mae(y_test, ridge_preds)

rss_ridge <- sum((y_test - ridge_preds)^2)
r2_ridge <- 1 - rss_ridge/tss
r2_ridge

#random forest
data_rf <- data_pomocna_regularizacija 

train <- data_rf[train_index, ]
test  <- data_rf[-train_index, ]
dummies <- dummyVars(~ ., data = train[, !names(train) %in% "log_sale_price"])

data.train.num <- data.frame(predict(dummies, newdata = train))
data.train.num$log_sale_price <- train$log_sale_price

data.test.num <- data.frame(predict(dummies, newdata = test))
data.test.num$log_sale_price <- test$log_sale_price

rf_model <- randomForest(
  log_sale_price ~ ., 
  data = data.train.num,
  ntree = 500,      
  mtry = 3,         
  importance = TRUE
)

rf_preds <- predict(rf_model, newdata = data.test.num)

rmse_val <- rmse(data.test.num$log_sale_price, rf_preds)
mae_val  <- mae(data.test.num$log_sale_price, rf_preds)
rmse_val
mae_val

rss_rf <- sum((data.test.num$log_sale_price - rf_preds)^2)
tss_rf <- sum((data.test.num$log_sale_price - 
                 mean(data.test.num$log_sale_price))^2)

r2_rf <- 1 - rss_rf/tss_rf
r2_rf
