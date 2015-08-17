#########################################
# TILM3558 Harjoitustyö, Osa 1, R-koodi #
# Lasse Rintakumpu, 63555               #
# 17.8.2015                             #
#########################################

# Asetetaan työhakemisto
wd <- "D:/Dropbox/Edu/Statistics/TILM 3558 Harjoitustyö" #Hipatlaptop 
setwd(wd)

# Funktio kirjastojen asentamiselle / lataamiselle
lataa_kirjasto <- function(kirjasto) {
  if(kirjasto %in% rownames(installed.packages()) == FALSE)
  {install.packages(kirjasto)} 
  library(kirjasto, character.only = TRUE)
}

# Ladataan/asennetaan käytetyt kirjastot
lapply(c("MASS", "MissMech", "tables"), lataa_kirjasto)

# Ladataan havaintoaineisto, 
ek2011 <- read.csv("https://raw.githubusercontent.com/rintakumpu/tilm3557/master/EK2011_filtered.csv", header=TRUE, row.names=NULL, fileEncoding = "UTF-8-BOM");

# TestMCARNormality(ek2011)
# Hypothesis of MCAR is rejected at  0.05 significance level.
# The multivariate normality test is inconclusive. 

# Hoidetaan puuttuvat havainnot poistamalla tilastoyksiköt joissa puuttuvia havaintoja
ek2011 <- na.omit(read.csv("https://raw.githubusercontent.com/rintakumpu/tilm3557/master/EK2011_filtered.csv", header=TRUE, row.names=NULL, fileEncoding = "UTF-8-BOM"));

# Tallennetaan käsiteltävä data omiin muuttujiinsa
sukupuoli <- ek2011$d2[ek2011$filter==1] # Sukupuoli
aanestys2007 <- ek2011$q23[ek2011$filter==1] #Äänestysaktiivisuus vuoden 2007 vaaleissa
omasupu <- ek2011$k23[ek2011$filter==1] # Oman sukupuolen äänestäminen

########################################
# 1. Frekvenssit ja ristiintaulukointi # 
########################################

# Luodaan frekvenssitaulut
sukupuoli_t<-table(sukupuoli)
sukupuoli_t<-addmargins(sukupuoli_t)
row.names(sukupuoli_t) <- c("Mies", "Nainen", "Yhteensä")
sukupuoli_t<-cbind( f=sukupuoli_t, Prosenttiosuus=prop.table(sukupuoli_t)*2)

#           f     Prosenttiosuus
#Mies     300      0.5016722
#Nainen   298      0.4983278
#Yhteensä 598      1.0000000

aanestys2007_t<-table(aanestys2007)
aanestys2007_t<-addmargins(aanestys2007_t)
row.names(aanestys2007_t) <- c("Kyllä", "Ei", "Ei äänioikeutta", "Ei halua sanoa", "Ei osaa sanoa","Yhteensä")
aanestys2007_t<-cbind( f=aanestys2007_t, Prosenttiosuus=prop.table(aanestys2007_t)*2)

#                  f  Prosenttiosuus
#Kyllä           525    0.877926421
#Ei               45    0.075250836
#Ei äänioikeutta  23    0.038461538
#Ei halua sanoa    1    0.001672241
#Ei osaa sanoa     4    0.006688963
#Yhteensä        598    1.000000000

omasupu_t<-table(omasupu)
omasupu_t<-addmargins(omasupu_t)
row.names(omasupu_t) <- c("Kyllä", "Ei", "Ei osaa sanoa", "Yhteensä")
omasupu_t<-cbind( f=omasupu_t, Prosenttiosuus=prop.table(omasupu_t)*2)

#               f   Prosenttiosuus
#Kyllä         347    0.580267559
#Ei            246    0.411371237
#Ei osaa sanoa   5    0.008361204
#Yhteensä      598    1.000000000

# Poistetaan pienifrekvenssiset luokat, tallennetaan data
# uusiin _mod-muuttujiin
aanestys2007_mod<-aanestys2007[!aanestys2007 %in% c(7,8)]
sukupuoli_mod<-sukupuoli[!aanestys2007 %in% c(7,8)]
omasupu_mod<-omasupu[!aanestys2007 %in% c(7,8)]

sukupuoli_mod<-sukupuoli_mod[!omasupu_mod %in% c(3)]
aanestys2007_mod<-aanestys2007_mod[!omasupu_mod %in% c(3)]
omasupu_mod<-omasupu_mod[!omasupu_mod %in% c(3)] # Vektori ylikirjoitetaan

ct<-xtabs(~aanestys2007_mod+omasupu_mod+sukupuoli_mod) # Luodaan kolmen muuttujan ristiintaulukko

#   Sukupuoli = Mies
#                             Oman sukupuolen äänestäminen
#   Äänestäminen vuonna 2007  Kyllä Ei
#                      Kyllä  170   91
#                         Ei  14    8
#            Ei äänioikeutta  7     4

#   Sukupuoli = Nainen
#                             Oman sukupuolen äänestäminen
#   Äänestäminen vuonna 2007  Kyllä Ei
#                      Kyllä  140   119
#                         Ei  12    11
#            Ei äänioikeutta  1     11

###############################################################
# 2. Muuttujien välisen riippuvuuden loglineaarinen mallinnus # 
###############################################################

# Askelletaan taaksepäin täydestä mallista
malli1 <- loglm(~aanestys2007_mod*omasupu_mod*sukupuoli_mod, data=ct)
drop1(malli1, scope = ~aanestys2007_mod*omasupu_mod*sukupuoli_mod, test="Chisq", trace=TRUE)

#Single term deletions
#Model:
#~aanestys2007_mod * omasupu_mod * sukupuoli_mod
#                                           Df    AIC    LRT Pr(>Chi)  
#<none>                                        24.000                  
#aanestys2007_mod                            0 24.000 0.0000           
#omasupu_mod                                 0 24.000 0.0000           
#sukupuoli_mod                               0 24.000 0.0000           
#aanestys2007_mod:omasupu_mod                0 24.000 0.0000           
#aanestys2007_mod:sukupuoli_mod              0 24.000 0.0000           
#omasupu_mod:sukupuoli_mod                   0 24.000 0.0000           
#aanestys2007_mod:omasupu_mod:sukupuoli_mod  2 25.564 5.5641  0.06191 .

# Poistetaan aanestys2007_mod:omasupu_mod:sukupuoli_mod ei-merkitsevänä
malli2 <- loglm(~aanestys2007_mod*omasupu_mod*sukupuoli_mod-aanestys2007_mod:omasupu_mod:sukupuoli_mod, data=ct)
drop1(malli2, scope = ~aanestys2007_mod*omasupu_mod*sukupuoli_mod-aanestys2007_mod:omasupu_mod:sukupuoli_mod, test="Chisq")

#Single term deletions
#Model:
#  ~aanestys2007_mod * omasupu_mod * sukupuoli_mod - aanestys2007_mod:omasupu_mod:sukupuoli_mod
#                               Df    AIC     LRT Pr(>Chi)   
#<none>                            25.564                    
#aanestys2007_mod                0 25.564  0.0000            
#omasupu_mod                     0 25.564  0.0000            
#sukupuoli_mod                   0 25.564  0.0000            
#aanestys2007_mod:omasupu_mod    2 27.038  5.4743  0.06476 . 
#aanestys2007_mod:sukupuoli_mod  2 21.592  0.0284  0.98590   
#omasupu_mod:sukupuoli_mod       1 33.668 10.1034  0.00148 **

#Poistetaan aanestys2007_mod:omasupu_mod ja aanestys2007_mod:sukupuoli_mod 
#mallista ei-merkitsevinä. omasupu_mod:sukupuoli_mod jää malliin merkitsevänä 
#p = 0.00148
malli3 <- loglm(~aanestys2007_mod+omasupu_mod+sukupuoli_mod+omasupu_mod:sukupuoli_mod, data=ct)
drop1(malli3, scope = ~aanestys2007_mod+omasupu_mod+sukupuoli_mod+omasupu_mod:sukupuoli_mod, test="Chisq")

#Single term deletions
#Model:
#  ~aanestys2007_mod + omasupu_mod + sukupuoli_mod + omasupu_mod:sukupuoli_mod
#                           Df    AIC    LRT  Pr(>Chi)    
#<none>                        23.11                     
#aanestys2007_mod           2 802.86 783.75 < 2.2e-16 ***
#omasupu_mod                0  23.11   0.00              
#sukupuoli_mod              0  23.11   0.00              
#omasupu_mod:sukupuoli_mod  1  31.26  10.15  0.001444 ** 

# Lopulliseen malliin jäävät omasupu_mod:sukupuoli_mod
# ja aanestys2007_mod

malli4 <- loglm(~aanestys2007_mod+omasupu_mod:sukupuoli_mod, data=ct)

# Standardoidut jäännökset
residuals(malli4)

#, , sukupuoli_mod = 1
#omasupu_mod
#aanestys2007_mod           1            2
#               1  0.08365801 -0.009267545
#               2 -0.16262825  0.041693017
#               3 -0.17421025 -0.014421100

#, , sukupuoli_mod = 2
#omasupu_mod
#aanestys2007_mod           1           2
#               1  0.40122738 -0.51385698
#               2  0.08463937  0.06347555
#               3 -2.52803825  2.05395004

# Yhteensopivuustesti
summary(malli4)

# Statistics:
#                       X^2 df P(> X^2)
# Likelihood Ratio 11.11174  6 0.08498371
# Pearson          10.10498  6 0.12030011

#####################################################
# 3. Mallin yhteyksien jatkotarkastelu ja tulkinta  #
#####################################################

# Mallin generoiva luokka on {omasupu_mod*sukupuoli_mod, aanestys2007_mod}  
# Mallissa on yksi yhteys, Tulkitaan yhteyttä ristiintaulukoimalla:

omasupu_mod <- factor(omasupu_mod, levels=c(1,2), labels=c("Kyllä","Ei"))
sukupuoli_mod <- factor(sukupuoli_mod, levels=c(1,2), labels=c("Mies", "Nainen"))
ct2 <- tabular((Sukupuoli=sukupuoli_mod)~(Heading(Oman_sukupuolen_aanestaminen)*omasupu_mod*(Percent("row")+ 1)))
# ct2_latex <- latex(ct2)

#           Oman sukupuolen äänestäminen
#           Kyllä    Ei         
# Sukupuoli Percent  All Percent All
# Mies      64.97    191 35.03   103
# Nainen    52.04    153 47.96   141

chisq.test(table(sukupuoli_mod, omasupu_mod))

# Pearson's Chi-squared test with Yates' continuity correction
# data:  table(sukupuoli_mod, omasupu_mod)
# X-squared = 9.5903, df = 1, p-value = 0.001956

# Taulukon perusteella miehet näyttäisivät äänestävän naisia
# useammin omaa sukupuolta edustavaa ehdokasta. Edellisissä
# vaaleissa äänestäminen ei vaikuta tähän käyttäytymiseen.
