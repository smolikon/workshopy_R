library(janitor)
library(dplyr)
library(ggplot2)
library(datasets)
library(tidyverse)
library(tibble)
library(purrr) # # https://purrr.tidyverse.org/ 
# library(furrr) # https://furrr.futureverse.org/


# doporučené učební zdroje ------------------------------------------------

# https://adv-r.hadley.nz/ # sekce 9-11
# https://www.youtube.com/watch?v=EGAs7zuRutY&ab_channel=PositPBC 
# https://www.youtube.com/watch?v=nd-Y8b22YaQ&ab_channel=EquitableEquations 
# https://www.youtube.com/watch?v=UvopClD98LQ&ab_channel=RProgramming101 
# záznam s Petřem Bouchalem dostupný v Teamsech 



# vektory, listy, dataframy --------------------------------------------------------

a <- c(1, 2, 8)

c <- c(3, 10, 15)

d <- c(54, 1, 87, 57)


class(a)

# V R je list objekt, který může obsahovat různé typy prvků – čísla, texty, logické hodnoty, dokonce i jiné vektory nebo listy. Na rozdíl od atomic vektoru (např. numeric vector jako c(1, 2, 8)), kde všechny prvky musí být stejného typu, v listu to není nutné.

b <- list(1, "text", TRUE)
class(b)

df <- data.frame(hodnota = I(list(1, "text", TRUE)))
class(df)

df_tibble <- df |> 
  as_tibble()

class(df_tibble)

# sečteme hodnoty opdovídající stejné pozici (1. apod) 
soucet <- a + c

# co když chceme např sečíst největší čísla z obou vektorů? Zde se nám už hodí funkce



# Psaní funkcí jak na to? -------------------------------------------------

# hranaté závorky []                            AltGr + F / AltGr + G
# složené závorky {}                            AltGr + B / AltGr + N


## Klasická funkce 
secti_max <- function(x, y) { 
  max_x <- max(x)  # najde největší hodnotu v x (respektive prvním vektoru)
  max_y <- max(y)  # najde největší hodnotu v y (respektive v druhém vektoru)
  return(max_x + max_y)  # co se vrací jako výsledek
}

soucet_maxim <- secti_max(a, c)

# TODO uprav funkci, aby odečítala průměrné hodnoty z vektoru d - a




# ukázka naší funkce na ověřování validity Rodných čísel ------------------
# pomáhá nám následně odfiltrovat osoby s neplatným RČ

validate_rc <- function(df, rc_column) {
  df %>%
    mutate(
      rc = as.character(.data[[rc_column]]),
      is_numeric = grepl("^[0-9]+$", rc) %>% as.logical(),  # Ověření, že obsahuje pouze čísla
      valid_length = nchar(rc) %in% c(9, 10),
      year = as.integer(substr(rc, 1, 2)),
      month = as.integer(substr(rc, 3, 4)),
      day = as.integer(substr(rc, 5, 6)),
      ecp = ifelse(day > 40, TRUE, FALSE),
      month = ifelse(month > 50, month - 50, month),
      day = ifelse(day > 40, day - 40, day),
      full_year = case_when(
        nchar(rc) == 9  ~ 1900 + year,
        nchar(rc) == 10 & year < 54  ~ 2000 + year,
        nchar(rc) == 10 & year >= 54 ~ 1900 + year,
        TRUE ~ NA_real_
      ),
      birth_date = suppressWarnings(as.Date(sprintf("%d-%02d-%02d", full_year, month, day))),
      divisible_by_11 = ifelse(nchar(rc) == 10, as.numeric(rc) %% 11 == 0, TRUE),
      valid_rc_ecp = is_numeric & valid_length & !is.na(birth_date) & divisible_by_11
    ) |>  select(-c(is_numeric, valid_length, year, month, day, full_year, birth_date, divisible_by_11, rc))
}


## Tzv. Anonymní funkce
# Použití anonymní funkce pro sečítání maximálních hodnot
soucet_maxim <- (function(x, y) max(x) + max(y)) (a, c)


# jiný příklad anonymní funkce a představení/opakování funkcí z rodiny apply
data(mtcars)

head(mtcars)

# chceme zjistit počet unikátních hodnot pro všechny sloupce datasetu mtcars

pocet_unikatnich_hodnot_sloupce1 <- lapply(mtcars, function(x) n_distinct(x))

pocet_unikatnich_hodnot_sloupce1

# Výstup: lapply() vrací list. Každý prvek listu bude odpovídat počtu unikátních hodnot pro každý sloupec v mtcars.
# 
# Využití: Tato funkce je vhodná pro aplikování funkce na každý sloupec (nebo prvek v seznamu) a vrací výsledek jako seznam.

pocet_unikatnich_hodnot_sloupce2 <- sapply(mtcars, function(x) n_distinct(x))

pocet_unikatnich_hodnot_sloupce2


# Výstup: sapply() se chová podobně jako lapply(), ale pokusí se automaticky zjednodušit výstup do vektoru nebo matice, pokud je to možné. V tomto případě, protože n_distinct() vrací numerické hodnoty, sapply() vrátí numerický vektor.

# Využití: Pokud chceš jednodušší výstup než seznam (například vektor), použij sapply().

pocet_unikatnich_hodnot_sloupce3 <- apply(mtcars, function(x) n_distinct(x))

pocet_unikatnich_hodnot_sloupce2


# Chyba: Při použití apply() s datasetem jako mtcars dostaneš chybu argument "FUN" is missing, with no default, protože argument MARGIN chybí a apply() neví, jestli má funkci aplikovat po řádcích nebo sloupcích.

# Správné použití apply(): Funkce apply() vyžaduje 2. argument MARGIN, který určuje, zda funkci aplikovat po řádcích (MARGIN = 1) nebo po sloupcích (MARGIN = 2). Pokud chceš aplikovat funkci po sloupcích, musíš to specifikovat.

pocet_unikatnich_hodnot_sloupce3 <- apply(mtcars, 2, function(x) n_distinct(x))



# tapply
# Funkce tapply() umožňuje aplikovat funkci na podskupiny dat na základě kombinace úrovní faktorů. Jinými slovy, dělí data do skupin podle faktorů (například podle hodnoty nějaké proměnné) a na každou skupinu pak použije požadovanou funkci.

# tapply(X, INDEX, FUN, ...)
# X: Vstupní vektor dat (např. mtcars$mpg).

# INDEX: Kategorické proměnné, podle kterých budou data rozdělena do podskupin (např. mtcars$cyl a mtcars$gear).
# 
# FUN: Funkce, která bude aplikována na každou podskupinu (např. mean pro výpočet průměru).
# 
# ...: Další argumenty, které můžeš předat funkci.

tapply(mtcars$mpg, list(mtcars$cyl, mtcars$gear), mean)
# mtcars$mpg je vektor hodnot spotřeby paliva (miles per gallon) pro různé automobily v datasetu mtcars.
# 
# list(mtcars$cyl, mtcars$gear) jsou dvě kategorické proměnné (faktory), které tvoří kombinované faktory. Tento seznam určuje, jak budou data rozdělena do podskupin:
#   
# mtcars$cyl obsahuje hodnoty 4, 6 a 8, což je počet válců v automobilech.
# 
# mtcars$gear obsahuje hodnoty 3, 4 a 5, což je počet převodových stupňů v automobilech.
# 
# Takto tapply() rozdělí data do podskupin podle kombinací hodnot cyl a gear. Funkce, kterou na každou podskupinu aplikujeme, je mean, tedy spočítáme průměrnou hodnotu mpg pro každou kombinaci počtu válců a počtu převodových stupňů.

# Tento příklad tedy spočítá průměrnou spotřebu pro různé kombinace počtu válců a počtu převodových stupňů.

# Rodina apply (base R)
# Funkce	Použití	Pro co je vhodná	Výstup

# apply()	Aplikuje funkci na řádky/sloupce matice/data framu	Pro číselné matice nebo datové rámce	Vektor, matice nebo seznam
# lapply()	Aplikuje funkci na každý prvek seznamu nebo sloupcích data framu	Vrací seznam	Seznam
# sapply()	Stejné jako lapply(), ale pokusí se zjednodušit výstup	Chceš vektor/matici místo seznamu	Vektor, matice nebo seznam
# vapply()	Jako sapply(), ale bezpečnější – předem definuješ typ výstupu	Bezpečné aplikace, např. v produkčním kódu	Vektor, matice nebo seznam
# tapply()	Aplikuje funkci na skupiny definované faktory	Skupinové operace (agregace)	Vektor/matice/tabulka
# mapply()	„Multivariate apply“ – aplikuje funkci na více vstupů současně	Vektorizace funkcí s více argumenty	Vektor/seznam/matic



# purrr -------------------------------------------------------------------

# Rodina map (tidyverse - balíček purrr)

# Funkce	Použití	Pro co je vhodná	Výstup
# map()	Jako lapply() – aplikuje funkci na každý prvek seznamu/vektoru	Čitelný tidyverse styl	Seznam
# map_lgl()	Vrací logické hodnoty	Např. testy podmínek	Logický vektor
# map_int()	Vrací celá čísla	Např. délky, počty	Číselný vektor (integer)
# map_dbl()	Vrací čísla (double)	Např. průměry	Číselný vektor (double)
# map_chr()	Vrací texty	Např. extrakce názvů, typů	Textový vektor
# pmap()	Jako mapply() – pro víc argumentů	Aplikace funkce na více vstupů z tibble/sloupce	Seznam nebo specifický typ
# imap()	Jako map(), ale poskytuje i index	Např. když chceš znát pozici	Seznam/vektor

# map z balíčku purrr 
result <- map(mtcars, function(x) n_distinct(x))

result <- map(mtcars, ~n_distinct(.x))

result

# map(mtcars, ...): Funkce map() iteruje přes každý sloupec nebo prvek datasetu mtcars (v tomto případě jsou to jednotlivé sloupce, protože mtcars je datový rámec).
# 
# .x: Tento symbol označuje aktuální prvek (v tomto případě sloupec z data frame mtcars), na který je aplikována funkce. V tomto případě se na každý sloupec aplikuje funkce n_distinct(), která vrací počet unikátních hodnot v daném sloupci.


# můžeme definovat jaký datový class chceme vracet
result_map_dbl <- map_dbl(mtcars, function(x) n_distinct(x))

result_map_dbl


result_map_chr <- map_chr(mtcars, function(x) as.character(n_distinct(x)))
result_map_chr 

# TODO podívej se na videa o map(ech)
# https://www.youtube.com/watch?v=nd-Y8b22YaQ&ab_channel=EquitableEquations

# velmi podobně funguje balíček # furrr


# Possibly, safely, quietly -----------------------------------------------

# Funkce possibly(), safely() a quietly() z knihovny purrr jsou užitečné pro ošetření potenciálních chyb nebo pro zajištění tichého výstupu, když pracuješ s funkcemi, které mohou selhat nebo generovat varování. Každá z těchto funkcí má specifický účel:

# possibly(): Umožňuje aplikovat funkci, která může selhat, a místo chyby vrátí definovaný výstup.

# safely(): Vrací dvě hodnoty: výsledek funkce a informace o chybě.

# quietly(): Potlačí varování nebo zprávy generované funkcí, kterou aplikujeme

# Vektor obsahující hodnoty, některé jsou NULL nebo NA
data <- list(1, 2, "NULL", 4, Inf, 6)

add_one <- possibly(function(x) {
  if (is.infinite(x)) {
    return(NA)  # Pokud je hodnota Inf nebo -Inf, vrátí NA
  }
  x + 1  # Jinak přičte 1
}, otherwise = NA)

result <- map(data, add_one)

# Výstup výsledku
print(result)

# TODO vyzkoušej totéž se safely, co se změnilo?



# for loops ---------------------------------------------------------------
# nevýhoda for loopů že mohou déle trvat než funkce z balíčku purrr 

# příklad 1
for (i in 1:5) {  # i bude postupně 1, 2, 3, 4, 5
  print(i * 2)     # Vytiskne 2, 4, 6, 8, 10
}


# příklad 2
jmena <- c("Alice", "Bob", "Charlie")
for (jmeno in jmena) {
  print(jmeno)
}

# příklad 3
cisla <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)

for (cislo in cisla) {
  if (cislo %% 3 == 0) {  # Zkontrolujeme, jestli je číslo dělitelné 3
    print(paste(cislo, "je dělitelné 3"))
  } else {
    print(paste(cislo, "není dělitelné 3"))
  }
}

# TODO podívej se na video o foor loopech 
# https://www.youtube.com/watch?v=UvopClD98LQ&ab_channel=RProgramming101 


# tvorba a uložení grafů pro všech 14 krajů -------------------------------

# Generování náhodných dat pro 14 krajů
kraje <- c("Hlavní město Praha", "Středočeský", "Jihočeský", "Plzeňský", "Karlovarský", 
           "Ústecký", "Liberecký", "Královéhradecký", "Pardubický", "Vysočina", 
           "Jihomoravský", "Olomoucký", "Zlínský", "Moravskoslezský")

# Vytvoření datového rámce s náhodnými podíly
set.seed(123)  # Pro reprodukovatelnost
data_kraj <- tibble(
  kraj = rep(kraje, each = 2),
  pohlavi = rep(c("Muži", "Ženy"), times = length(kraje)),
  podil = runif(28, 0.4, 0.6)  # Generování náhodného podílu mezi 0.4 a 0.6 pro všechny řádky
)


# Upravit podíly tak, aby jejich součet byl vždy 100 %
data <- data_kraj %>%
  group_by(kraj) %>%
  mutate(podil = ifelse(pohlavi == "Muži", podil, 1 - podil))


# Funkce pro uložení grafu pro každý kraj
uloz_graf <- function(data, slozka) {
  # Pokud složka neexistuje, vytvoříme ji
  if (!dir.exists(slozka)) {
    dir.create(slozka)
  }
  
  # Smyčka pro generování a ukládání grafů pro každý kraj
  for (k in unique(data$kraj)) {
    plot <- ggplot(data %>% filter(kraj == k), aes(x = pohlavi, y = podil, fill = pohlavi)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("Muži" = "lightblue", "Ženy" = "darkorange")) +
      labs(title = paste("Podíl podle pohlaví v kraji", k), x = "Pohlaví", y = "Podíl") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    # Uložení grafu do složky
    ggsave(paste0(slozka, "/", gsub(" ", "_", gsub("á", "a", k)), ".png"), plot = plot, bg = "white", height = 10, width = 15.98, unit = "cm", dpi = 300)
  }
}

# Složka, kam se grafy uloží
slozka_grafy <- "grafy"

# Volání funkce pro uložení grafů
uloz_graf(data_kraj, slozka_grafy)





# Úkoly na příště  --------------------------------------------------------
# V rámci přípravy na nadcházející workshop Automatizace v Quartu 

# 1. Zkouknout záznam workshopu k automatizaci s Petrem Bouchalem (viz Teamsy)
# 2. Zkouknout záznam úvodu do Quarto s Vojtou Kunou (viz Teamsy)
