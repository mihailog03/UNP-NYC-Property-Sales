library(dplyr)
library(tidyverse)
library(ggplot2)
library(scales)
library(nortest)
library(glmnet)
library(Metrics)
data = read.csv("nyc-rolling-sales.csv")

nrow(data)
ncol(data)

str(data)
summary(data)
names(data)

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

#Analiza outliera



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

#Odnosi SALE.PRICE sa ostalim kolonama

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


#Imputacija vrednosti

#Broj NA vrednosti u kolonama pre imputacije
sum(is.na(data$LAND.SQUARE.FEET))
sum(is.na(data$GROSS.SQUARE.FEET))

#Pokusaj Shapiro testa normalnosti
#shapiro.test(data$LAND.SQUARE.FEET)

#Koriscenje Anderson-Darling testa normalnosti
ad.test(na.omit(data$LAND.SQUARE.FEET))
ad.test(na.omit(data$GROSS.SQUARE.FEET))

head(sort(table(data$BUILDING.CLASS.CATEGORY),decreasing=TRUE),10)

#Neophodna transformacija da bi preskoci_land kolona imala ispravne vrednosti, kasnije vracena u factor
data <- data %>%
  mutate(
    BUILDING.CLASS.CATEGORY = as.character(BUILDING.CLASS.CATEGORY),
    BUILDING.CLASS.CATEGORY = trimws(BUILDING.CLASS.CATEGORY)
  )

#Kategorije za koje na osnovu domenskog znanja nema smisla vrsiti imputaciju
preskoci_land <- c(
  "10 COOPS - ELEVATOR APARTMENTS",
  "09 COOPS - WALKUP APARTMENTS",
  "13 CONDOS - ELEVATOR APARTMENTS",
  "17 CONDO COOPS",
  "04 TAX CLASS 1 CONDOS"
)

data <- data %>%
  mutate(preskoci_land = BUILDING.CLASS.CATEGORY %in% preskoci_land)

#Imputacija po grupama
data <- data %>%
  group_by(BUILDING.CLASS.CATEGORY) %>%
  mutate(
    LAND.SQUARE.FEET = ifelse(
      preskoci_land & is.na(LAND.SQUARE.FEET),
      median(LAND.SQUARE.FEET, na.rm = TRUE),
      LAND.SQUARE.FEET
    ),
    GROSS.SQUARE.FEET = ifelse(
      is.na(GROSS.SQUARE.FEET),
      median(GROSS.SQUARE.FEET, na.rm = TRUE),
      GROSS.SQUARE.FEET
    )
  ) %>%
  ungroup()

#Broj NA vrednosti u kolonama posle imputacije
sum(is.na(data$LAND.SQUARE.FEET))
sum(is.na(data$GROSS.SQUARE.FEET))

#Dodatna imputacija za one redove iz kategorija koje nisu imale ni jednu vrednost
data <- data %>%
  mutate(
    LAND.SQUARE.FEET = ifelse(
      is.na(LAND.SQUARE.FEET) & !preskoci_land,
      median(LAND.SQUARE.FEET, na.rm = TRUE),
      LAND.SQUARE.FEET
    ),
    GROSS.SQUARE.FEET = ifelse(
      is.na(GROSS.SQUARE.FEET),
      median(GROSS.SQUARE.FEET, na.rm = TRUE),
      GROSS.SQUARE.FEET
    )
  )

# Osiguravanje da LAND ostane NA u preskoci_land kategorijama
data <- data %>%
  mutate(
    LAND.SQUARE.FEET = ifelse(preskoci_land, NA, LAND.SQUARE.FEET)
  ) 

#Broj NA vrednosti u kolonama posle finalne imputacije
sum(is.na(data$LAND.SQUARE.FEET))
head(data[, c("BUILDING.CLASS.CATEGORY", "LAND.SQUARE.FEET")], 20)

sum(is.na(data$GROSS.SQUARE.FEET))

data$BUILDING.CLASS.CATEGORY <- as.factor(data$BUILDING.CLASS.CATEGORY)



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

#lsf i sale
ggplot(data, aes(x = LAND.SQUARE.FEET, y = SALE.PRICE)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(labels = comma) +
  labs(title = "LAND.SQUARE.FEET vs SALE.PRICE",
       x = "Površina zemljišta",
       y = "Prodajna cena") +
  theme_minimal()

#lsf i sale zumiran
ggplot(data, aes(x = LAND.SQUARE.FEET, y = SALE.PRICE)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(labels = comma) +
  coord_cartesian(xlim = c(0, 20000),
                  ylim = c(0, 5000000)) +
  labs(title = "LAND.SQUARE.FEET vs SALE.PRICE",
       x = "Površina zemljišta",
       y = "Prodajna cena") +
  theme_minimal()

#opstina i sale
ggplot(data, aes(x = BOROUGH, y = log_sale_price)) +
  geom_boxplot() +
  scale_y_continuous(labels = comma) +
  labs(title = "SALE.PRICE po BOROUGH",
       x = "BOROUGH",
       y = "Prodajna cena") +
  theme_minimal()

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
    mesec = factor(as.integer(format(SALE.DATE, "%m")))
  )

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

#Ponovno logovanje zbog imputed vrednosti za GROSS.SQUARE.FEET i LAND.SQUARE.FEET
data <- data %>%
  mutate(
    log_gross_sqft = log10(GROSS.SQUARE.FEET + 1),
    log_land_sqft  = log10(LAND.SQUARE.FEET + 1))

#Iskoriscenost parcele = bruto povrsina / povrsina zemljista
data <- data %>%
  mutate(isk_parcele = (GROSS.SQUARE.FEET + 1) / (LAND.SQUARE.FEET + 1))

#Kombinacija BOROUGH i grupa_nekretnine
data <- data %>%
  mutate(borough_group = interaction(BOROUGH, grupa_nekretnine, drop = TRUE))

#Feature selection

data_fs <- data %>%
  select(
    SALE.PRICE, log_sale_price, GROSS.SQUARE.FEET, log_gross_sqft, LAND.SQUARE.FEET, log_land_sqft,
    TOTAL.UNITS, RESIDENTIAL.UNITS, COMMERCIAL.UNITS, BOROUGH,
    grupa_nekretnine, isk_parcele, borough_group
  )

#Rad sa modelima

#Train test split
train_index <- sample(seq_len(nrow(data)), size = 0.8 * nrow(data))
train <- data[train_index, ]
test  <- data[-train_index, ]

numericki <- train %>%
  select(where(is.numeric))

cor_matrix <- cor(numericki, use = "complete.obs")
round(cor_matrix, 2)

model1 <- lm(log_sale_price ~ log_gross_sqft, data = train)
summary(model1)

model2 <- lm(
  log_sale_price ~ 
    GROSS.SQUARE.FEET +
    LAND.SQUARE.FEET +
    TOTAL.UNITS +
    COMMERCIAL.UNITS +
    RESIDENTIAL.UNITS,
  data = train
)
summary(model2)

model3 <- lm(
  log_sale_price ~ 
    log_gross_sqft +
    log_land_sqft +
    TOTAL.UNITS +
    COMMERCIAL.UNITS +
    RESIDENTIAL.UNITS +
    factor(BOROUGH) +
    factor(grupa_nekretnine),
  data = train
)
summary(model3)

#ridge i lasso

data_pomocna_regularizacija <- data_fs %>%
  select(-SALE.PRICE)
data_pomocna_regularizacija <- drop_na(data_pomocna_regularizacija)
train_index <- sample(seq_len(nrow(data_pomocna_regularizacija)), 
                      size = 0.8 * nrow(data_pomocna_regularizacija))

train <- data_pomocna_regularizacija[train_index, ]
test  <- data_pomocna_regularizacija[-train_index, ]

x_train <- model.matrix(log_sale_price ~ ., data = train)[, -1]
y_train <- train$log_sale_price

x_test <- model.matrix(~ ., data = test)[, -1]
x_test <- x_test[, colnames(x_train)]
y_test <- test$log_sale_price

ridge_cv <- cv.glmnet(x_train, y_train, alpha = 0)
lasso_cv <- cv.glmnet(x_train, y_train, alpha = 1)

ridge_cv$lambda.min

lasso_cv$lambda.min

ridge_preds = predict(ridge_cv, newx = x_test, s = "lambda.min")
lasso_preds = predict(lasso_cv, newx = x_test, s = "lambda.min")


rmse(y_test, ridge_preds)

rmse(y_test, lasso_preds)
