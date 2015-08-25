#########################################
# TILM3558 Harjoitustyö, Osa 2, R-koodi #
# Lasse Rintakumpu, 63555               #
# 25.8.2015                             #
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
lapply(c("moments", "car", "ARTool", "corrplot", "MASS"), lataa_kirjasto)

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

#################################################
# 1b. Mallin oletusten ja muuttujien tarkastelu #
#################################################

# Ladataan data omiin muuttujiinsa
elinolo <- read.csv("https://raw.githubusercontent.com/rintakumpu/tilm3558/master/elinolo_filtered_b.csv", sep=",", dec=".", header=TRUE, row.names=NULL, fileEncoding = "UTF-8-BOM");
rkkot <- elinolo$rkkot[elinolo$filter==1] # Kotitalouden tulot, euroa per vuosi?
asmenot <- elinolo$asmenot[elinolo$filter==1] # Asumismenot, euroa per vuosi?
alaika <- elinolo$alaika[elinolo$filter==1] # Alueella asumisaika, kuukautta?

# Ladataan data matriisiin
elinolo_matrix <- matrix(c(pala,rkkot,asmenot,alaika), ncol=4, byrow=FALSE, 
                         dimnames=list(c(1:1000),c("pala", "rkkot", "asmenot", "alaika" )))

 
# Piirretään sirontakuvio
scatterplotMatrix(~pala+rkkot+asmenot+alaika, data=elinolo_matrix)
pdf ('scatterplot_pala_rkkot_asmenot_alaika.pdf')
dev.off()

# Piirretään korrelaatiomatrisi
corrplot(cor(elinolo_matrix, method="pearson"), method="number", col="black", type="upper", tl.pos="tl", diag=TRUE, cl.pos="n", tl.col="black")
corrplot(cor(elinolo_matrix, method="spearman"), col="#777777", add=TRUE, method="number", type="lower", tl.pos="n", cl.pos="n", tl.col="black", diag=FALSE)

pdf('corrplot_pala_rkkot_asmenot_alaika.pdf')
dev.off()

# Riippuvuus vain palan ja rkkotin välillä lineaarista, Rs > Rp. 
# Jakaumat eivät normaaleja, sovelletaan muunnoksia dataan

pala_pt <- powerTransform(pala) # 0.3128892
rkkot_pt <- powerTransform(rkkot) # 0.5585493

elinolo_matrix_transform <- matrix(c(pala^0.3128892,rkkot^0.5585493, asmenot,alaika), ncol=4, byrow=FALSE, 
                         dimnames=list(c(1:1000),c("pala", "rkkot", "asmenot", "alaika" ))

cor(elinolo_matrix_transform[,"pala"],elinolo_matrix_transform[,"rkkot"], method="pearson")
# 0.5640465
cor(elinolo_matrix_transform[,"pala"],elinolo_matrix_transform[,"rkkot"], method="spearman")
# 0.5567287

######################
# 2b. Mallin valinta #
######################

malli1 <- lm(pala~rkkot+asmenot+alaika, data=as.data.frame(elinolo_matrix_transform))
summary(stepAIC(malli1, direction="backward"))
malli2 <- lm(pala~rkkot+alaika, data=as.data.frame(elinolo_matrix_transform))
summary(stepAIC(malli2, direction="backward"))

# Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 3.1764747  0.0497292   63.88  < 2e-16 ***
#  rkkot       0.0017090  0.0000750   22.79  < 2e-16 ***
#  alaika      0.0053713  0.0007841    6.85 1.29e-11 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.4016 on 997 degrees of freedom
# Multiple R-squared:  0.3488,  Adjusted R-squared:  0.3475 
# F-statistic:   267 on 2 and 997 DF,  p-value: < 2.2e-16

# Eteenpäin askeltaen
malli3 <- lm(pala~rkkot, data=as.data.frame(elinolo_matrix_transform))
malli4 <- lm(pala~rkkot+asmenot, data=as.data.frame(elinolo_matrix_transform))
summary(stepAIC(malli3, direction="forward"))
summary(stepAIC(malli4, direction="forward"))
summary(stepAIC(malli2, direction="forward")) # Päädytään samaan malliin

summary(malli3)

# Residual standard error: 0.4107 on 998 degrees of freedom
# Multiple R-squared:  0.3181,  Adjusted R-squared:  0.3175 
# F-statistic: 465.7 on 1 and 998 DF,  p-value: < 2.2e-16

# Jatketaan mallilla 2
coefficients(malli2)
malli <- malli2
# 3.176474690 0.001708954 0.005371325 
# Regressioyhtälö:
# pala=rkkot*0.0017+alaika*0.0054+3.1765.

############################################
# 3b. Mallin jatkotarkastelu ja yhteenveto #
############################################

# Tarkastellaan valitun mallin (malli3) jäännöksiä
par(mfrow=c(2,2))

# Jäännösten histogrammi
hist(rstandard(malli), freq=FALSE, main="", ylab="", xlab="Standardized residuals")
xfit<-seq(min(rstandard(malli)),max(rstandard(malli)),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)
plot(malli)