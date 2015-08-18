#########################################
# TILM3558 Harjoitustyö, Osa 1, R-koodi #
# Lasse Rintakumpu, 63555               #
# 18.8.2015                             #
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

# Hoidetaan puuttuvat havainnot poistamalla tilastoyksiköt joissa puuttuvia havaintoja
ek2011 <- na.omit(read.csv("https://raw.githubusercontent.com/rintakumpu/tilm3558/master/EK2011_filtered.csv", header=TRUE, row.names=NULL, fileEncoding = "UTF-8-BOM"));

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
#Mies     341      0.4978102
#Nainen   344      0.5021898
#Yhteensä 685      1.0000000

aanestys2007_t<-table(aanestys2007)
aanestys2007_t<-addmargins(aanestys2007_t)
row.names(aanestys2007_t) <- c("Kyllä", "Ei", "Ei äänioikeutta", "Ei halua sanoa", "Ei osaa sanoa","Yhteensä")
aanestys2007_t<-cbind( f=aanestys2007_t, Prosenttiosuus=prop.table(aanestys2007_t)*2)

#                  f  Prosenttiosuus
#Kyllä           602    0.878832117
#Ei               49    0.071532847
#Ei äänioikeutta  27    0.039416058
#Ei halua sanoa    1    0.001459854
#Ei osaa sanoa     6    0.008759124
#Yhteensä        685    1.000000000

omasupu_t<-table(omasupu)
omasupu_t<-addmargins(omasupu_t)
row.names(omasupu_t) <- c("Kyllä", "Ei", "Ei osaa sanoa", "Yhteensä")
omasupu_t<-cbind( f=omasupu_t, Prosenttiosuus=prop.table(omasupu_t)*2)

#               f   Prosenttiosuus
#Kyllä         402     0.58686131
#Ei            278     0.40583942
#Ei osaa sanoa   5     0.00729927
#Yhteensä      685     1.00000000

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
#                      Kyllä  197   101
#                         Ei  15    8
#            Ei äänioikeutta  8     6

#   Sukupuoli = Nainen
#                             Oman sukupuolen äänestäminen
#   Äänestäminen vuonna 2007  Kyllä Ei
#                      Kyllä  165   134
#                         Ei  12    14
#            Ei äänioikeutta  1     12

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
#aanestys2007_mod:omasupu_mod:sukupuoli_mod  2 25.424 5.4243  0.06639 .

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
#aanestys2007_mod:omasupu_mod    2 29.850  8.4257 0.0148043 *  
#aanestys2007_mod:sukupuoli_mod  2 21.872  0.4480 0.7993029    
#omasupu_mod:sukupuoli_mod       1 35.487 12.0628 0.0005144 ***

#Poistetaan aanestys2007_mod:sukupuoli_mod 
#mallista ei-merkitsevänä. 
#omasupu_mod:sukupuoli_mod jää malliin merkitsevänä 
#p = 0.00148
#aanestys2007_mod:omasupu_mod jää malliin merkitsevänä
#p = 0.01480

malli3 <- loglm(~aanestys2007_mod+omasupu_mod+sukupuoli_mod+omasupu_mod:sukupuoli_mod+aanestys2007_mod:omasupu_mod, data=ct)
drop1(malli3, scope = ~aanestys2007_mod+omasupu_mod+sukupuoli_mod+omasupu_mod:sukupuoli_mod+aanestys2007_mod:omasupu_mod, test="Chisq")

#Single term deletions
#Model:
#  ~aanestys2007_mod + omasupu_mod + sukupuoli_mod + omasupu_mod:sukupuoli_mod + 
#aanestys2007_mod:omasupu_mod
#                           Df    AIC    LRT  Pr(>Chi)    
#<none>                        23.11                     
#aanestys2007_mod              0 21.872  0.0000              
#omasupu_mod                   0 21.872  0.0000              
#sukupuoli_mod                 0 21.872  0.0000              
#omasupu_mod:sukupuoli_mod     1 31.696 11.8239 0.0005848 ***
#aanestys2007_mod:omasupu_mod  2 26.059  8.1868 0.0166825 * 

# Lopulliseen malliin jäävät omasupu_mod:sukupuoli_mod
# ja aanestys2007_mod:omasupu_mod

malli4 <- loglm(~aanestys2007_mod:omasupu_mod+omasupu_mod:sukupuoli_mod, data=ct)

# Standardoidut jäännökset
residuals(malli4)

#, , sukupuoli_mod = 1

#                omasupu_mod
#aanestys2007_mod           1          2
#               1 -0.21975329  0.2738556
#               2  0.01949492 -0.4047331
#               3  1.24515522 -0.5772916

#, , sukupuoli_mod = 2

#               omasupu_mod
#aanestys2007_mod           1          2
#               1  0.24290246 -0.2340208
#               2 -0.02171404  0.3303641
#               3 -1.80696957  0.4611147

# Yhteensopivuustesti
summary(malli4)

# Statistics:
#                       X^2 df P(> X^2)
# Likelihood Ratio 5.872300  4 0.2088902
# Pearson          5.153074  4 0.2719487

#####################################################
# 3. Mallin yhteyksien jatkotarkastelu ja tulkinta  #
#####################################################

# Mallin generoiva luokka on {aanestys2007_mod:omasupu_mod, omasupu_mod:sukupuoli_mod}  
# Mallissa on kaksi yhteyttä, Tulkitaan yhteyttä ristiintaulukoimalla:

omasupu_mod <- factor(omasupu_mod, levels=c(1,2), labels=c("Kyllä","Ei"))
sukupuoli_mod <- factor(sukupuoli_mod, levels=c(1,2), labels=c("Mies", "Nainen"))
aanestys2007_mod <- factor(aanestys2007_mod, levels=c(1,2,3), labels=c("Kyllä", "Ei", "Ei äänioikeutta"))

ct2 <- tabular((Sukupuoli=sukupuoli_mod)~(Heading(Oman_sukupuolen_aanestaminen)*omasupu_mod*(Percent("row")+ 1)))

#           Oman sukupuolen äänestäminen
#           Kyllä                            Ei         
# Sukupuoli Percent                      All Percent All
# Mies      65.67                        220 34.33   115
# Nainen    52.66                        178 47.34   160

chisq.test(table(sukupuoli_mod, omasupu_mod))

# Pearson's Chi-squared test with Yates' continuity correction
# data:  table(sukupuoli_mod, omasupu_mod)
# X-squared = 11.2505, df = 1, p-value = 0.000796

ct2 <- tabular((Aanestys_2007=aanestys2007_mod)~(Heading(Oman_sukupuolen_aanestaminen)*omasupu_mod*(Percent("row")+ 1)))

#                Oman_sukupuolen_aanestaminen                
#                Kyllä                            Ei         
#Aanestys_2007   Percent                      All Percent All
#Kyllä           60.64                        362 39.36   235
#Ei              55.10                         27 44.90    22
#Ei äänioikeutta 33.33                          9 66.67    18

chisq.test(table(aanestys2007_mod, omasupu_mod))

# Pearson's Chi-squared test
# data:  table(aanestys2007_mod, omasupu_mod)
# X-squared = 8.3251, df = 2, p-value = 0.01557

# Taulukon perusteella miehet näyttäisivät äänestävän naisia
# useammin omaa sukupuolta edustavaa ehdokasta. 
# Samoin tekivät vuoden 2007 eduskuntavaaleissa äänestäneet.
