library(readr)
library(dplyr)
library(openxlsx)
library(sf)
library(tmap)
library(tmaptools)
library(ggplot2)
library(shinyjs)
library(janitor)
library(extrafont)
library(reschola)
library(RColorBrewer)
library(ragg) #in-bildt písma


# barvy -------------------------------------------------------------------

Blues5 <- c("#c3cbdb", "#9ba9c4", "#637599", "#3b5486", "#263268")

mf5 <- c("#edf8fb", "#b3cde3", "#8c96c6", "#8856a7", "#810f7c")

zm5 <- c("#f0f9e8", "#bae4bc", "#7bccc4", "#43a2ca", "#0868ac")

zm5_0 <- c("#FFFFFF", "#f0f9e8", "#bae4bc", "#7bccc4", "#43a2ca", "#0868ac")


# tmaptools::palette_explorer()

modra <- get_brewer_pal("Blues", n = 6)

# # zobrazení všech palet 
# display.brewer.all()

# # Zobrazení palety "Blues" s pěti barvami
# display.brewer.pal(n = 5, name = "Blues")
# blues7 <- get_brewer_pal("Blues", n = 7)



# úprava reálných dat na cvičení -------------------------------------------
# df_kraj$pocet_respondentu <- sample(0:150, nrow(df_kraj), replace = TRUE)
# df_orp$pocet_respondentu <- sample(0:200, nrow(df_orp), replace = TRUE)



# načtení dat -------------------------------------------------------------

df_kraj <- readRDS("data/processed/kraje_pocty_projektu_cvicna_data.rds")
df_orp <- readRDS("data/processed/orp_pocty_projektu_cvicna_data.rds")



# mapy --------------------------------------------------------------------

# načtení polygonů a sjednocení názvů proměnné  
kraj <- RCzechia::kraje()
orp <- RCzechia::orp_polygony()
# mc <- RCzechia::casti()

kraj <- clean_names(kraj)
orp <- clean_names(orp)
# mc <- clean_names(mc)


kraj <- kraj %>% 
  rename("kraj" ="naz_cznuts3")

orp <- orp %>% 
  rename("orp" ="naz_orp", 
         "kraj" = "naz_cznuts3")


kraj_mapy <-  kraj %>% 
  left_join(df_kraj, by ="kraj") %>% 
  filter(probiha_je_vas_projekt == "Ano" & kraj != "Celá ČR")

orp_bezi <-  df_orp %>% 
  filter(probiha_je_vas_projekt == "Ano")

orp_mapy <-  orp %>% 
  left_join(orp_bezi, by ="orp") %>% 
  mutate(pocet_respondentu = if_else(is.na(pocet_respondentu), 0, pocet_respondentu)) 

rm(orp_bezi)
rm(orp)


# počty -------------------------------------------------------------------
colnames(kraj_mapy)

# kraje  
pocty_kraj1 <- tm_shape(kraj_mapy) +
  tm_polygons("pocet_respondentu", palette = mf5, style="pretty", n=4, title = "Celkem projektů \na soc. služeb v krajích", 
              as.count=TRUE, legend.show=FALSE, border.col = "red") 
pocty_kraj1 


pocty_orp1 <- tm_shape(orp_mapy) +
  tm_polygons("pocet_respondentu", palette = zm5, style="pretty", title = "Celkem projektů \na soc. služeb v krajích", 
              as.count=TRUE, legend.show=FALSE, border.col = "black") 
pocty_orp1



# další styly
# An alternative is the style argument. This allows the user to automatically create breaks by specifying algorithms.
# Among others, the following styles can be passed:
# style = equal: Splits the variable into intervals of equal length. Should only be used if the variable follows an uniform distribution.
# style = quantile: Splits the variable into quantiles. Consequently there are the same number of observations in each interval.
# style = jenks: Identifies groups with similar values and maximizes the difference between them.
# style = cont: Displays many colors over a continuous palette.
# style = cat: Colors each category individually for categorical data.
# style = fixed - pevně dané hranice kategorií tzv. breaks
# style = order - order style uses a smooth gradient with a large number of colors, but the values on the legend do not change linearly
# https://r-tmap.github.io/tmap-book/visual-variables.html


# kraje  
pocty_kraj <- tm_shape(kraj_mapy) +
  tm_polygons("pocet_respondentu", palette = zm5, style="order", title = "Celkem projektů \na soc. služeb v krajích", 
              as.count=FALSE, legend.show=TRUE, border.col = "black") +
  tm_text("pocet_respondentu", size = 0.6, xmod = 0, ymod = -0.25, remove.overlap = TRUE) + 
  tm_credits(text = "Cvičná data pro účely workshopu.", size = 0.9, position=c(0.0, 0.05)) +  
  tm_layout(frame = FALSE,
            legend.outside = FALSE, legend.format = list(text.separator = "až"),
            legend.title.size = 1.1, legend.text.size = 0.8, legend.title.fontface = "bold",
            legend.position = c(0.85, 0.65), 
            inner.margins = c(0.12,0.0,0.05,0.0), # Vector of four values specifying the bottom, left, top, and right margin
            outer.margins = c(0.0,-0.2,0.07,-0.2),
            main.title = "Počty běžících projektů a sociálních služeb v krajích pro osoby s dočasnou ochranou",
            main.title.size = 1,
            main.title.color = "#0868ac",
            main.title.fontface = "bold",  
            fontfamily = "Arial")  

pocty_kraj

tmap_save(pocty_kraj, "mapy/kraj_pocty_uprchliku.png", height = 9,  width = 15.98, units = "cm")


# instalace fontů 
# install.packages("extrafont")
# library(extrafont)
# font_import()  # Toto může chvíli trvat
# loadfonts(device = "win")  # Načte fonty pro Windows



# počty ORP 
kraj_shape <- tm_shape(kraj_mapy) + 
  tm_borders(lwd = 2, col = "black")  

pocty_orp <- tm_shape(orp_mapy) + 
  tm_polygons("pocet_respondentu", palette = zm5_0, style = "pretty", breaks=c(0, 30, 80, 120, 180, 250), 
              title = "Celkem projektů \na soc. služeb v ORP", as.count=TRUE, 
              # showNA = TRUE, textNA = "Bez projektů",
              # legend.format = list(fun = function(x) gsub("\\.", ",", paste0(formatC(x, digits = 1, format = "f"), " %"))), # as.count musí být false
              legend.show=TRUE, border.col = "black") +
  # tm_text("pocet_respondentu", size = 0.6, xmod = 0, ymod = -0.25) +
  tm_credits(text = "Cvičná data pro účely workshopu.", size = 0.9, position=c(0.0, 0.05)) +  
  tm_layout(frame = FALSE,
            legend.outside = FALSE, legend.format = list(text.separator = "až"),
            legend.title.size = 1.0, legend.text.size = 0.8, legend.title.fontface = "bold",
            legend.position = c(0.85, 0.65), 
            main.title = "Počty běžících projektů a sociálních služeb pro osoby s dočasnou ochranou v ORP",
            main.title.size = 1,
            inner.margins = c(0.12,0.0,0.05,0.0), # Vector of four values specifying the bottom, left, top, and right margin
            outer.margins = c(0.0,-0.15,0.07,0.0)) + 
  kraj_shape  # Přidání vrstvy s hranicemi krajů

pocty_orp

tmap_save(pocty_orp, "mapy/orp_pocty_uprchliku.png", height = 9, width = 15.98, units = "cm")


# počty ORP, style = "cont"
kraj_shape <- tm_shape(kraj_mapy) + 
  tm_borders(lwd = 2, col = "black")  

# možnost mít speciální text pro NA a upravit formát legendy na %, Kč apod.
pocty_orp <- tm_shape(orp_mapy) + 
  tm_polygons("pocet_respondentu", palette = zm5_0, style = "cont", 
              title = "Celkem projektů \na soc. služeb v ORP", as.count=FALSE, 
              # showNA = TRUE, textNA = "Bez projektů",
              # legend.format = list(fun = function(x) gsub("\\.", ",", paste0(formatC(x, digits = 1, format = "f"), " %"))), # as.count musí být false
              legend.show=TRUE, border.col = "black") +
  # tm_text("pocet_respondentu", size = 0.6, xmod = 0, ymod = -0.25) +
  tm_credits(text = "Cvičná data pro účely workshopu.", size = 0.9, position=c(0.0, 0.05)) +  
  tm_layout(frame = FALSE,
            legend.outside = FALSE, legend.format = list(text.separator = "až"),
            legend.title.size = 1.0, legend.text.size = 0.8, legend.title.fontface = "bold",
            legend.position = c(0.85, 0.65), 
            main.title = "Počty běžících projektů a sociálních služeb pro osoby s dočasnou ochranou v ORP",
            main.title.size = 1,
            inner.margins = c(0.12,0.0,0.05,0.0), # Vector of four values specifying the bottom, left, top, and right margin
            outer.margins = c(0.0,-0.15,0.07,0.0)) + 
  kraj_shape  # Přidání vrstvy s hranicemi krajů

pocty_orp

tmap_save(pocty_orp, "mapy/orp_pocty_uprchliku_style_cont.png", height = 9, width = 15.98, units = "cm")





# počty ORP jen vybrané kraje: Ústecký a Karlovarský kraj -----------------
# příprava: vyfiltrujeme si data, i hrnaici krajů jen pro vybrané 2 kraje
kraj_mapy2kraje <- kraj_mapy %>% 
  filter(kraj %in% c("Ústecký kraj", "Karlovarský kraj"))

kraj_shape_2_kraje <- tm_shape(kraj_mapy2kraje) + 
  tm_borders(lwd = 2, col = "black")  

orp_mapy_2kraje <- orp_mapy %>% 
  filter(kraj %in% c("Ústecký kraj", "Karlovarský kraj"))


pocty_orp_2kraje <- tm_shape(orp_mapy_2kraje) + 
  tm_polygons("pocet_respondentu", palette = zm5_0, style="fixed", breaks=c(0, 30, 80, 120, 180, 250), title = "Celkem projektů \na soc. služeb v ORP", as.count=TRUE, legend.show=TRUE, border.col = "black") +
  # tm_text("pocet_respondentu", size = 0.6, xmod = 0, ymod = -0.25) +
  tm_credits(text = "Cvičná data pro účely workshopu.", size = 0.9, position=c(0.0, 0.05)) +  
  tm_layout(frame = FALSE,
            legend.outside = FALSE, legend.format = list(text.separator = "až"),
            legend.title.size = 1.0, legend.text.size = 0.8, legend.title.fontface = "bold",
            legend.position = c("right", "bottom"),
            # legend.position = c(0.85, 0.1), 
            main.title.fontface = "bold",  
            main.title = "Počty běžících projektů a sociálních služeb pro osoby s dočasnou ochranou \nv ORP v Karlovarském a Ústeckém kraji",
            main.title.size = 1,
            inner.margins = c(0.12,0.0,0.05,0.0),  # Vector of four values specifying the bottom, left, top, and right margin
            outer.margins = c(0.0,-0.15,0.07,0.0)) + 
  kraj_shape_2_kraje  # Přidání vrstvy s hranicemi krajů

# Position of the legend. Vector of two values, specifying the x and y coordinates. Either this vector contains 
# "left", "LEFT", "center", "right", or "RIGHT" for the first value and "top", 
# "TOP", "center", "bottom", or "BOTTOM" for the second value, 
# or this vector contains two numeric values between 0 and 1 that specifies the x and y coordinates of the left bottom corner of the legend. 
# The uppercase values correspond to the position without margins (so tighter to the frame). 
# By default, it is automatically placed in the corner with most space based on the (first) shape object. 
# If legend.outside=TRUE, this argument specifies the legend position within the outside panel.

pocty_orp_2kraje

tmap_save(pocty_orp_2kraje, "mapy/orp_pocty_uprchliku_kvk_a_ulk.png", height = 9, width = 15.98, units = "cm")

