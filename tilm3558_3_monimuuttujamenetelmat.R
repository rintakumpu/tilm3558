#########################################
# TILM3558 Harjoitustyö, Osa 3, R-koodi #
# Lasse Rintakumpu, 63555               #
# 26.8.2015                             #
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
lapply(c("psych", "corrplot", "nFactors"), lataa_kirjasto)

# Ladataan havaintoaineisto ja valitaan tiedot satunnaisesti valituilta 1000 riviltä
pankkiotos <- read.csv("https://raw.githubusercontent.com/rintakumpu/tilm3558/master/pankkiotos_filtered.csv", sep=",", dec=".", header=TRUE, row.names=NULL, fileEncoding = "UTF-8-BOM")
pankkiotos <- as.data.frame(pankkiotos[pankkiotos$filter==1,])

# Tallennetaan pääkomponenttianalyysissa käytettävät muutujat omaan
# matriisiinsa
pankkiotos_pca <- pankkiotos[,69:109]

######################################################
# 1. Pääkomponenttianalyysin edellytysten tarkastelu # 
######################################################

# Tarkastellaan muuttujien välisten korrelaatioiden merkitsevyyttä
# Funktio korrelaatioiden läpikäyntiin
cor.mtest <- function(mat, conf.level = 0.95) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- sig.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], conf.level = conf.level)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      lowCI.mat[i, j] <- lowCI.mat[j, i] <- tmp$conf.int[1]
      uppCI.mat[i, j] <- uppCI.mat[j, i] <- tmp$conf.int[2]
      
      # Kuuluuko nolla luottamusvälille?
      if(tmp$conf.int[1]<=0 & tmp$conf.int[2]>=0) {
        sig.mat[i,j] <- sig.mat[j,i] <- FALSE
      } else { sig.mat[i,j] <- sig.mat[j,i] <- TRUE }
    }
  }
  # return(list(p.mat, lowCI.mat, uppCI.mat))
  # Palauttaa listan, jossa merkitsevät korrelaatiot TRUE
  return(sig.mat)
}

korrelaatiot_p005<-cor.mtest(pankkiotos_pca, 0.95)
sum(unlist(korrelaatiot_p005), na.rm=TRUE) # 816 / 1640 (41*40, eli korrelaatiot muuttujien itsensä kanssa korrelaatiot poistettu) 
# => 50% merkitseviä

korrelaatiot_p01<-cor.mtest(pankkiotos_pca, 0.90)
sum(unlist(korrelaatiot_p01), na.rm=TRUE) # 914 / 1640 => 56% merkitseviä

# Muuttujat juuri ja juuri sopivia pääkomponenttianalyysiin
# Tarkastellaan vielä korrelaatiota

korrelaatiot<-round(cor(pankkiotos_pca),2)
# Ainoastaan hyvin heikkoja korrelaatiot (kaikki korrelaatiot <0.2)
# sisältävät muuttujat
korrelaatiot[7,] # asuntolaina_b_kpl_luok
korrelaatiot[8,] # vakuutus_b_luok
korrelaatiot[9,] # vakuutus_c_luok
korrelaatiot[17,] # kayttotili_vel_luok
korrelaatiot[25,] # asuntolaina_d_kpl_luok
korrelaatiot[30,] # asuntolaina_e_kpl_luok
korrelaatiot[36,] # toimeksianto_a_kpl_luok
korrelaatiot[37,] # toimeksianto_b_kpl_luok
pudotettavat <- c("asuntolaina_b_kpl_luok", "vakuutus_b_luok", "vakuutus_c_luok", "kayttotili_vel_luok",  "asuntolaina_d_kpl_luok", "asuntolaina_e_kpl_luok", "toimeksianto_a_kpl_luok", "toimeksianto_b_kpl_luok")

# Pudotetaan nämä
pankkiotos_pca_edit <- pankkiotos_pca[,!(names(pankkiotos_pca) %in% pudotettavat)]

# Kaiser-Meyer-Olkin (KMO) Measure of Sampling Adequacy
KMO(pankkiotos_pca_edit)[[1]] # 0.718899 > 0.6
# KMO OK

# Bartlettin sfäärisyystesti on herkkä poikkeamille normaalijakaumista,
# normaalisuus datan luokitelluilla muuttujilla tuskin pätee,
# testataan kuitenkin satunnaisesti pari muuttujaa:

shapiro.test(pankkiotos_pca_edit[,sample(1:33,1)]) # W = 0.6501, p-value < 2.2e-16
shapiro.test(pankkiotos_pca_edit[,sample(1:33,1)]) # W = 0.309, p-value < 2.2e-16
shapiro.test(pankkiotos_pca_edit[,sample(1:33,1)]) # W = 0.6581, p-value < 2.2e-16

# Normaalisuusoletus ei päde, unohdetaan Bartlettin testi

#############################
# 2. Pääkomponenttianalyysi #
#############################

# Määritellään 
# Determine Number of Factors to Extract
ominaisarvot <- eigen(cor(pankkiotos_pca_edit)) # Haetaan ominaisarvot
ap <- parallel(subject=nrow(pankkiotos_pca_edit),var=ncol(pankkiotos_pca_edit),
               rep=100,cent=.05)
nScree(x=ominaisarvot$values, aparallel=ap$eigen$qevpea)

# noc naf nparallel nkaiser
# 9   1   9         11

# => Päädytään yhdeksään pääkomponenttiin
malli_pca <- principal(pankkiotos_pca, nfactors=9, rotate="promax")
# Fit based upon off diagonal values = 0.9

# Pääkomponenteille latautuneet muuttujat:


# # 2. Talleta havaintomatriisiin uusiksi muuttujiksi  
# pääkomponenttipistemäärät.     
# 3. Nimeä uudet muuttujat (pääkomponentteihin  
# latautuneiden muuttujien mukaisesti).   

#######################
# 3. Klusterianalyysi #
#######################

# 4. Käytä näitä uusia muuttujia klusterianalyysissä, 
# jossa muodostat asiakasryhmiä tilanteeseen sopivilla 
# menetelmillä. Kuvaile muodostamiasi ryhmiä. 

