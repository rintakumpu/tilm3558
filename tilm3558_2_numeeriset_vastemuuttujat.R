#########################################
# TILM3558 Harjoitustyö, Osa 2, R-koodi #
# Lasse Rintakumpu, 63555               #
# 19.8.2015                             #
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
lapply(c("moments", "car", "ARTool"), lataa_kirjasto)

# Ladataan havaintoaineisto
elinolo <- read.csv("https://raw.githubusercontent.com/rintakumpu/tilm3558/master/elinolo_filtered.csv", sep=",", dec=".", header=TRUE, row.names=NULL, fileEncoding = "UTF-8-BOM");

# Tallennetaan käsiteltävä data omiin muuttujiinsa,
# valitaan samalla tiedot vain niiltä 1000 riviltä, jotka on satunnaisesti
# valittu tarkasteluun koko datasta

pala <- elinolo$pala[elinolo$filter==1] # Asunnon pinta-ala, neliömetreinä
kumu <- as.factor(elinolo$kumu[elinolo$filter==1]) # Luokitteleva muuttuja kuntamuoto. 0: Kaupunki, 1: Muu kuntamuoto
supu  <- as.factor(elinolo$supu[elinolo$filter==1]) # Luokitteleva sukupuoli, 1: Mies, 2:Nainen

##############################################
# 1a. Kuntamuodon yhteys asunnon pinta-alaan # 
##############################################

# Tarkastellaan asunnon pinta-alaa kuntomuodoittain

length(pala[kumu==0]) # 645
length(pala[kumu==1]) # 355
boxplot(pala[kumu==0], pala[kumu==1],
        notch = T, xlab = "Kuntamuoto", ylab = expression(paste("Asunnon pinta-ala m  " ^{2})), xlab="ahtas", main="Asunnon koko kuntamuodoittain",
        col=rep(c("skyblue","lightgreen")), xaxt="n")
axis(side=1, labels=c("1. Kaupunki", "2. Muu kuntamuoto"), at=c(1,2))
pdf('boxplot_pala_kumu.pdf')
dev.off()

# Kuvan perusteella asunnon pinta-aloissa vaikuttaa olevan tilastollisesti
# merkitsevä (95%-CI) ero kuntamuotojen välillä

# Lasketaan kuntamuotojen piste-estimaatit

mean(pala[kumu==0], na.rm=T) # Kaupunki: 76.30698
mean(pala[kumu==1], na.rm=T) # Muu kuntamuoto: 90.27887

# Jakaumien vinous

skewness(pala[kumu==0]) # 1.298357
skewness(pala[kumu==1]) # 1.045394

# Testataan jakaumien normaalisuutta

shapiro.test(pala[kumu==0])
#data:  pala[kumu == 0]
#W = 0.9254, p-value < 2.2e-16

shapiro.test(pala[kumu==1])
#data:  pala[kumu == 1]
#W = 0.9379, p-value = 5.117e-11

# Jakaumia ei voida pitää normaaleina, siirrytään epäparametriseen testaukseen

# Käytetään testaamiseen Kruskal-Wallis-testiä
# h0: Luokkien välillä ei ole eroa sijainnissa.
# hv: Luokkien välillä on ero sijainnissa.

kruskal.test(pala, kumu)

# Kruskal-Wallis rank sum test
# data:  pala and kumu
# Kruskal-Wallis chi-squared = 28.4112, df = 1, p-value = 9.81e-08

#############################################
# 2b. Sukupuolen yhteys asunnon pinta-alaan # 
#############################################

# Tarkastellaan asuntojen pinta-alaa ryhmittelevänä tekijänä sukupuoli
par(mar=c(5,5,4,2), xpd=T, xaxt="s")
boxplot(pala[supu==1], pala[supu==2],
        notch = T, xlab = "Sukupuoli", ylab = expression(paste("Asunnon pinta-ala m  " ^{2})), xlab="ahtas", main="Asunnon koko sukupuolen mukaan luokiteltuna",
        xaxt="n", col=rep(c("skyblue","pink")),
        at=c(1,2))
axis(side=1, labels=c("1. Mies", "2. Nainen"), at=c(1,2))
pdf('boxplot_supu_pala.pdf')
dev.off()

# Kuvan perusteella asunnon pinta-aloissa vaikuttaa olevan tilastollisesti
# merkitsevä (95%-CI) ero sukupuolien välillä.

length(pala[supu==1]) # 477
length(pala[supu==2]) # 523

# Siirrytään tilastolliseen testaukseen. Testataan ensin jakaumien
# normaalisuutta.

shapiro.test(pala[supu==1])

# Shapiro-Wilk normality test
# data:  pala[supu == 1]
# W = 0.9315, p-value = 5.64e-14

shapiro.test(pala[supu==2])

# data:  pala[supu == 2]
# W = 0.9239, p-value = 1.294e-15

# Jakaumia ei voida pitää normaaleina, siirrytään epäparametriseen testaukseen

# Käytetään testaamiseen Kruskal-Wallis-testiä
# h0: Luokkien välillä ei ole eroa sijainnissa.
# hv: Luokkien välillä on ero sijainnissa.

kruskal.test(pala, supu)

# data:  pala and supu
# Kruskal-Wallis chi-squared = 12.8887, df = 1, p-value = 0.0003306

###################################################
# 3c. Asumisahtauden ja kuntamuodon yhdysvaikutus #
###################################################

interaction.plot(kumu, supu, pala, legend = FALSE, xlab = "Kuntamuoto", lty = 1:2,
                 ylab = expression(paste("Asunnon pinta-ala m  " ^{2})),
                 xtick=TRUE, main = "Pinta-alan keskiarvojen profiilikäyrät",
                 xaxt="n", type="b", pch=c(21,22), bg=c("skyblue","pink"))

axis(side=1, labels=c("1. Kaupunki", "2. Muu kuntamuoto"), at=c(1,2))

legend(x=0.9,y=95,legend=c("Mies", "Nainen"), inset=1,
       bg = par("bg"), bty = "n", lty = c(1,2))

pdf('interaction_plot_supu_kumu.pdf')
dev.off()

# Käytetään aligned rank transform -muunnosta dataan, jotta
# voidaan testata yhteisvaikutuksen merkitsevyyttä.

anova(art(pala~supu+kumu+supu:kumu))

# Aligned Rank Transform Anova Table (Type III tests)
# Response: art(pala)
# Sum Sq Df Df.res F value    Pr(>F)    
# supu       842829  1    996 10.2084  0.001442 ** 
# kumu      2312695  1    996 28.4415 1.195e-07 ***
# supu:kumu    4186  1    996  0.0501  0.822976 