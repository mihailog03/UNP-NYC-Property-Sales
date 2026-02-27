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
- identifikovani su i uklonjeni duplikati (519 zapisa) definisanjem ključa transakcije: `BOROUGH + BLOCK + LOT + SALE.DATE + SALE.PRICE`.
- uklanjanje nelogičnih vrednosti (`SALE.PRICE <= 0`) i filtriranje administrativnih prodaja (`SALE.PRICE <= 10000`),
- osnovne provere realnih opsega (`YEAR.BUILT` u smislenim granicama).

### Nedostajuće vrednosti
U datasetu nedostajuće vrednosti nisu uvek eksplicitno `NA`, već se pojavljuju i kao “blank” ili posebni simboli (`-`). Nakon mapiranja na `NA`, analizirano je:
- u kojim kolonama se javljaju missing vrednosti,
- da li su missing vrednosti povezane (da li se pojavljuju u istim redovima),
- da li su koncentrisane u određenim opštinama i naseljima (BOROUGH i NEIGHBORHOOD).

### Imputacija
Za ključna numerička obeležja (posebno površine):
- `GROSS.SQUARE.FEET` i `LAND.SQUARE.FEET` su imputirani medianom **po kategoriji objekta** (`BUILDING.CLASS.CATEGORY`).

---

## EDA i analiza outliera
U EDA fazi analizirane su distribucije i odnosi sa targetom:
- distribucija `SALE.PRICE` i opravdanost log-transformacije (`log_sale_price`),
- veze između cene i površina (`GROSS.SQUARE.FEET`, `LAND.SQUARE.FEET`),
- razlike u ceni po lokaciji (`BOROUGH`) i po tipu objekta (`BUILDING.CLASS.CATEGORY`),
- outlieri (ekstremne površine i ekstremne cene) i njihovo filtriranje kako bi model bio stabilniji.
- napomena o outlier-ima: ekstremne vrednosti `SALE.PRICE` često predstavljaju realne transakcije na tržištu nekretnina, pa se uklanjaju samo kada postoje jasni indikatori greške (npr. nelogične vrednosti), kako se ne bi uvela pristrasnost.

---

## Feature engineering / Feature selection

### Izvedene promenljive
Uvedene su promenljive koje bolje hvataju relacije u podacima:
- `log_sale_price` (target promenljiva u log skali),
- `log_gross_sqft`, `log_land_sqft`,
- starost objekta (`starost = godina_prodaje - YEAR.BUILT`),
- mesec prodaje,
- odnos iskorišćenosti parcele (`isk_parcele`)
- `borough_group`, `res_units_grupa` i `ima_komercijalne`.

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

> Kada se u `lm()` uključe kategorijske promenljive (faktori), R interno radi odgovarajuće kodiranje (dummy/one-hot), tako da nije potreban ručni encoding.

Za jedan od prihvatljivih linearnih modela dobijeno je približno:
- **Adjusted R² ≈ 0.489**
- **Residual standard error ≈ 0.291** (u log skali)

Ovo pokazuje da model objašnjava značajan deo varijanse u logaritmovanom prostoru, ali da i dalje postoji prostor za poboljšanje.

### Najbolji linearni model (model4)
U novijoj verziji rada kao najstabilniji linearni model izdvojen je model koji uključuje interakciju kvadrature i lokacije, uz dodatne kategorijske faktore:

- **RMSE (test, log skala): 0.296491**
- **MAE (test, log skala): 0.205712**

Korišćena forma (opisno): `log_gross_sqft * BOROUGH + log_land_sqft + ima_komercijalne + res_units_grupa + BUILDING.CLASS.CATEGORY`.

### Ridge i Lasso
Korišćeni su regularizovani modeli nad one-hot enkodiranim obeležjima.

**RMSE (test, log skala):**
- Ridge: **0.2876**
- Lasso: **0.2860**

Razlika je mala, uz blagu prednost Lasso-a.

> Napomena: pošto je target u log skali, RMSE je takođe u log skali. Rezultati se interpretiraju kao greška u logaritmovanom prostoru, jer su originalne cene jako asimetrične.

## Zaključak

| Model          |   RMSE |    MAE |    R² |
|---------------|-------:|-------:|------:|
| LM – model4    | 0.2956 | 0.2060 | 0.5182 |
| Lasso          | 0.2960 | 0.2064 | 0.5208 |
| Ridge          | 0.3036 | 0.2117 | 0.4961 |
| Random Forest  | 0.3200 | 0.2223 | 0.4401 |

Na osnovu poređenja četiri modela na test skupu (RMSE, MAE i R²), najbolji rezultat daju linearna regresija sa interakcijom log_gross_sqft * BOROUGH + log_land_sqft + ima_komercijalne + res_units_grupa + BUILDING.CLASS.CATEGORY (model4) i Lasso regresija, pri čemu su njihove greške praktično iste (RMSE ≈ 0.296, MAE ≈ 0.206), a i objašnjena varijansa je oko ~0.52. Ridge je nešto slabiji (veći RMSE/MAE i niži R²), dok je Random Forest u korišćenoj konfiguraciji pokazao najlošije performanse, što verovatno znači da je potrebna dodatna optimizacija hiperparametara ili bogatiji skup prediktora. Zaključno, za ovaj dataset i izabrane feature, Lasso i pažljivo definisan linearni model sa interakcijama predstavljaju najbolji kompromis između tačnosti i interpretabilnosti.


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

---

## Zaključak
Log-transformacija prodajne cene pokazala se kao ključna za stabilnije modelovanje i fer poređenje modela, dok su površine i lokacija ostali najinformativniji prediktori. Regularizovani modeli (Ridge/Lasso) daju vrlo slične rezultate, a najbolji linearni model sa interakcijama i kategorijskim faktorima postiže najnižu grešku u okviru linearnih pristupa. Dalji napredak bi verovatno došao iz naprednijih nelinearnih modela i finije obrade lokacionih informacija, uz oprezno rukovanje outlier-ima koji mogu biti legitimne transakcije.

