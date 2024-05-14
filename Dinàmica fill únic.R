library(readxl)
library(dplyr)
library(ggplot2)
library(deSolve)

dades_naixements <- read_excel("C:/Users/admin/Desktop/Inquietdus Programació/Dinàmica Fill únic/Dades natalitat xina.xlsx", 1)
dades_morts <- read_excel("C:/Users/admin/Desktop/Inquietdus Programació/Dinàmica Fill únic/Dades natalitat xina.xlsx", 2)
dades_població <- read_excel("C:/Users/admin/Desktop/Inquietdus Programació/Dinàmica Fill únic/Dades natalitat xina.xlsx", 3)

dades <- dades_naixements %>%
  left_join(dades_morts, by = "Fecha") %>%
  left_join(dades_població, by = "Fecha")
dades

# TRADUCCIÓ

dades <- rename(dades, Any = Fecha)
dades <- rename(dades, Taxa_Natalitat_x1000 = `Tasa Natalidad`)
dades <- rename(dades, Morts = Muertes)
dades <- rename(dades, Taxa_Mortalitat_x1000 = `Tasa mortalidad`)
dades <- rename(dades, Població = Población)

dades_netes <- dades [,c(1,2,4,5,9)]
dades_netes

dades_netes$Taxa_Natalitat_x1000 <- gsub("‰","", dades_netes$Taxa_Natalitat_x1000)
dades_netes$Taxa_Mortalitat_x1000 <- gsub("‰","", dades_netes$Taxa_Mortalitat_x1000)

dades_netes$Taxa_Natalitat_x1000 <- gsub(",",".", dades_netes$Taxa_Natalitat_x1000)
dades_netes$Taxa_Mortalitat_x1000 <- gsub(",",".", dades_netes$Taxa_Mortalitat_x1000)

dades_netes$Taxa_Natalitat_x1000 <- as.numeric(dades_netes$Taxa_Natalitat_x1000)
dades_netes$Taxa_Mortalitat_x1000 <- as.numeric(dades_netes$Taxa_Mortalitat_x1000)

dades_netes

increment_p <- function(x) {
  (x-lag(x))/lag(x)*1000
}

dades_transf. <- dades_netes %>%
  mutate(dades_netes, Increment_Pob_x1000 = increment_p(Població)) %>%
  mutate(dades_netes, Diferència_Taxes = Taxa_Natalitat_x1000 - Taxa_Mortalitat_x1000)

dades_transf.
dades.49_71 <- dades_transf.[dades_transf.$Any <1972,]
dades.49_71

dades.72_21 <- dades_transf.[dades_transf.$Any >1971,]
dades.72_21

mitjana_nat.49_71 <- mean(dades.49_71$Taxa_Natalitat_x1000)
mitjana_mort.49_71 <- mean(dades.49_71$Taxa_Mortalitat_x1000)
mitjana_Incr_Pob.49_71 <- mean(na.omit(dades.49_71$Increment_Pob_x1000))

mitjana_nat.72_21 <- mean(dades.72_21$Taxa_Natalitat_x1000)
mitjana_mort.72_21 <- mean(dades.72_21$Taxa_Mortalitat_x1000)
mitjana_Incr_Pob.71_21 <- mean(dades.72_21$Increment_Pob_x1000)

#experiment
mitjana_mortalitat <- mean(dades_transf.$Taxa_Mortalitat_x1000)
mitjana_mortalitat

# Com que el canvi de mortalitat no es massa gran i vull incorporar la relació
# entre pblació, pibpc i mortalitat, em quedo amb la dada del 12,63 per construir
# el contrafactual



# Crear model de dinàmica de sistemes, a partir de l'any 71 com a última
# Observació, amb els paràmetres de natalitat i mortalitat del període de 1949 fins
# a 1971

START<- 1971
FINISH <- 2021
STEP <- 1

simtime <- seq(START,FINISH,STEP)

stocks <- c(sPoblació = 852290000)



#Introdueixo un paràmetre d'ajustament de Nº de naixements per cada 1000 habitants 
#en cas que es vulgui modificar la mitja, que és de 33,74 naixements per defecte.

Ajustament <- 4 # Ajustament = 4 implica que la taxa de naixements era de 3 per cada 100 habitants
# Es pot utilizar la última sèrie històrica que construirem per calibrar aquesta taxa.
auxs <-c(aNatalitat = (mitjana_nat.49_71-Ajustament)/1000, aMortalitat = (mitjana_mort.49_71)/1000)


# Creem el model

model <- function(time, stocks, auxs) {
  with(as.list(c(stocks, auxs)),{
    fNaixements <- sPoblació*aNatalitat
    fDefuncions <- sPoblació *aMortalitat
    dP_dt <- fNaixements - fDefuncions
    
    return(list(c(dP_dt),
                Naixements = fNaixements, Defuncions = fDefuncions,
                Natalitat = aNatalitat, Mortalitat = aMortalitat))
  })
}

o <- data.frame(ode(y=stocks, times = simtime, func = model, parms = auxs, method = "euler"))
o

# Representem el model. De color vermell tenim les dades observables, de color blau
# tenim el contrafactual que hem construit.

ggplot()+
  geom_line(data=o, aes(x= time, o$sPoblació/1000000),colour="blue")+
  geom_line(data = dades_població, aes(x= Fecha, y= dades_població$Población/1000000), colour ="red")+
  geom_point(data=o, aes(x= time, o$sPoblació/1000000),colour="blue")+
  geom_point(data = dades_població, aes(x= Fecha, y= dades_població$Población/1000000), colour ="red")+
  ylab("Població (Milions)")+
  xlab("Any")


# (Taxes de naixement i de mortalitat) Sèrie històrica de calibració

ggplot()+
  geom_line(data = dades_transf., aes(x= Any, y=Taxa_Natalitat_x1000), colour = "green")+
  geom_point(data = dades_transf., aes(x= Any, y=Taxa_Natalitat_x1000), colour = "green")+
  geom_line(data = dades_transf., aes(x= Any, y=Taxa_Mortalitat_x1000), colour = "brown")+
  geom_point(data = dades_transf., aes(x= Any, y=Taxa_Mortalitat_x1000), colour = "brown")+
  ylab("Taxa per 1000 habitants")+
  xlab("Any")