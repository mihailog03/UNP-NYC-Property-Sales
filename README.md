# NYC Property Sales — Predikcija prodajne cene

Cilj ovog projekta je da se na osnovu podataka o prodajama nekretnina u Njujorku (NYC Property Sales dataset sa Kaggle platforme) napravi model koji predviđa prodajnu cenu. Projekat obuhvata kompletan tok rada: od pripreme i čišćenja podataka, preko EDA analize i rada sa nedostajućim vrednostima, do feature engineering-a i treniranja modela.

---

## Motivacija i problem
Prodajne cene nekretnina imaju izraženu asimetriju (mnogo manjih vrednosti i mali broj ekstremno velikih), a dataset sadrži i administrativne prodaje i nekonzistentne zapise (npr. cena 0). Zbog toga je potrebno pažljivo:
- očistiti podatke,
- obraditi nedostajuće vrednosti,
- analizirati i ublažiti uticaj outliera,
- napraviti informativne izvedene promenljive,
kako bi modeli davali stabilnije i realnije predikcije.

---

## Skup podataka
**Dataset:** NYC Rolling Sales (`nyc-rolling-sales.csv`)  
Dataset opisuje transakcije prodaje nekretnina (lokacija, tip objekta, jedinice, površine, godina izgradnje, datum prodaje, poreske klase itd.).

**Target promenljiva:**
- originalno: `SALE.PRICE`
- za modeliranje: `log_sale_price = log10(SALE.PRICE)`  
Log-transformacija se koristi jer stabilizuje varijansu i smanjuje uticaj ekstremnih cena, što olakšava regresiono modelovanje.

---

## Opis i priprema podataka

### Čišćenje i obrada
Urađeni su tipični koraci pripreme:
- uklanjanje neinformativnih kolona (identifikatori, adresa, pomoćne kolone),
- konverzija tipova (numeričke kolone u numeric, kategorijske u factor, datum prodaje u Date),
- uklanjanje duplikata na osnovu ključnih atributa transakcije,
- uklanjanje nelogičnih vrednosti (`SALE.PRICE <= 0`) i filtriranje administrativnih prodaja (`SALE.PRICE <= 10000`),
- osnovne provere realnih opsega (`YEAR.BUILT` u smislenim granicama).

### Nedostajuće vrednosti
U datasetu nedostajuće vrednosti nisu uvek eksplicitno `NA`, već se pojavljuju i kao “blank” ili posebni simboli (`-`). Nakon mapiranja na `NA`, analizirano je:
- u kojim kolonama se javljaju missing vrednosti,
- da li su missing vrednosti povezane (da li se pojavljuju u istim redovima),
- da li su koncentrisane u određenim opštinama i naseljima (BOROUGH i NEIGHBORHOOD).

### Imputacija
Za ključna numerička obeležja (posebno površine):
- `GROSS.SQUARE.FEET` i `LAND.SQUARE.FEET` su imputirani medianom **po kategoriji objekta** (`BUILDING.CLASS.CATEGORY`) tamo gde to ima smisla,
- za kategorije gde površina parcele nije smislen koncept (određene vrste stanova), `LAND.SQUARE.FEET` se ne imputira već ostaje `NA`.

---

## EDA i analiza outliera
U EDA fazi analizirane su distribucije i odnosi sa targetom:
- distribucija `SALE.PRICE` i opravdanost log-transformacije (`log_sale_price`),
- veze između cene i površina (`GROSS.SQUARE.FEET`, `LAND.SQUARE.FEET`),
- razlike u ceni po lokaciji (`BOROUGH`) i po tipu objekta (`BUILDING.CLASS.CATEGORY`),
- outlieri (ekstremne površine i ekstremne cene) i njihovo filtriranje kako bi model bio stabilniji.

---

## Feature engineering / Feature selection

### Izvedene promenljive
Uvedene su promenljive koje bolje hvataju relacije u podacima:
- `log_sale_price` (target promenljiva u log skali),
- `log_gross_sqft`, `log_land_sqft`,
- starost objekta (`starost = godina_prodaje - YEAR.BUILT`),
- mesec prodaje,
- odnos iskorišćenosti parcele (`isk_parcele`).

### Grupisanje kategorija
Pošto `BUILDING.CLASS.CATEGORY` ima veliki broj različitih kategorija, napravljeno je grupisanje u šire klase (one-family, two-family, condo, coop, rental, commercial, other). Time se:
- smanjuje dimenzionalnost,
- zadržava značenje (tip objekta),
- olakšava modeliranje i interpretacija.

Takođe je korišćena kombinacija lokacije i tipa (`borough_group`) radi hvatanja specifičnih obrazaca (“tip objekta u određenom borough-u”).

---

## Modelovanje i evaluacija

### Linearni modeli
Krenuto je od jednostavnih linearnih modela, zatim su modeli proširivani dodatnim obeležjima i kategorijskim faktorima (BOROUGH, grupa nekretnine, itd.).

Za jedan od prihvatljivih linearnih modela dobijeno je približno:
- **Adjusted R² ≈ 0.489**
- **Residual standard error ≈ 0.291** (u log skali)

Ovo pokazuje da model objašnjava značajan deo varijanse u logaritmovanom prostoru, ali da i dalje postoji prostor za poboljšanje.

### Ridge i Lasso
Korišćeni su regularizovani modeli nad one-hot enkodiranim obeležjima.

**RMSE (test, log skala):**
- Ridge: **0.2876**
- Lasso: **0.2860**

Razlika je mala, uz blagu prednost Lasso-a.

> Napomena: pošto je target u log skali, RMSE je takođe u log skali. Rezultati se interpretiraju kao greška u logaritmovanom prostoru, jer su originalne cene jako asimetrične.

---

## Tehnologije
- `R`
- `tidyverse`, `dplyr`
- `ggplot2`, `scales`
- `nortest`
- `glmnet`
- `Metrics`

---

## Pokretanje projekta
1. Postavite `nyc-rolling-sales.csv` u isti folder kao i R skriptu.
2. Instalirajte pakete (ako nedostaju).
3. Pokrenite skriptu:

```r
source("seminarski.R")