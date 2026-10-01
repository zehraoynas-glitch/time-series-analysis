install.packages("forecast")
install.packages("readxl")
library(forecast) 
library(readxl)

data <- read_excel("C:/Users/zehra/OneDrive - hacettepe.edu.tr/Masaüstü/EVDS.xlsx")
summary(data)

# Zaman Serisi oluşturma
veri_vektor <- data[[1]] 
veri_ts <- ts(veri_vektor, start=c(2005, 6), frequency=12)

print(veri_ts)

# Orijinal Serinin Grafikleri
# Zaman Serisi Grafiği
ts.plot(veri_ts, gpars=list(xlab="Zaman", ylab="Değerler", main="EVDS Verisi Zaman Serisi"), 
        lwd=2, col="blue")
Acf(veri_ts, lag.max = 42, main="Orijinal Seri ACF Grafiği", ylim=c(-1,1), lwd=3)
Pacf(veri_ts, lag.max = 42, main="Orijinal Seri PACF Grafiği", ylim=c(-1,1), lwd=3)

# DURAĞANLAŞTIRMA (FARK ALMA İŞLEMLERİ) 
# Trent için 1. Dereceden Fark Alma
veri_diff <- diff(veri_ts)

# Farkı alınmış serinin grafiği
ts.plot(veri_diff, 
        gpars=list(xlab="Zaman", ylab="Fark Değerleri", main="1. Dereceden Farkı Alınmış Seri"), 
        lwd=2, col="red")
abline(h=0, col="blue", lty=2) 

# Trend farkı sonrası ACF/PACF 
Acf(veri_diff, lag.max = 42, main="1. Trent Farkı Alınmış Serinin ACF Grafiği", ylim=c(-1,1), lwd=3)
Pacf(veri_diff, lag.max = 42, main="1. Tren Farkı Alınmış Serinin PACF Grafiği", ylim=c(-1,1), lwd=3)

# Mevsimsellik İçin 1. Dereceden Fark Alma
veri_mevsim_fark_1 <- diff(veri_diff, lag = 12)

ts.plot(veri_mevsim_fark_1, 
        main="Trend + 1. Mevsimsel Farkı Alınmış Seri", 
        ylab="Değer", col="blue")
abline(h=0, col="red", lty=2)

Acf(veri_mevsim_fark_1, lag.max = 48, main="ACF (1. Trent + 1. Mevsimsel Fark)", lwd=3)
Pacf(veri_mevsim_fark_1, lag.max = 48, main="PACF (1. Trent + 1. Mevsimsel Fark)", lwd=3)

# Mevsimsellik İçin 2. Dereceden Fark Alma
veri_mevsim_fark_2 <- diff(veri_mevsim_fark_1, lag = 12)

ts.plot(veri_mevsim_fark_2, 
        main="Trend + 2 Kez Mevsimsel Fark Alınmış Seri", 
        ylab="Değer", col="darkgreen", lwd=2)
abline(h=0, col="red", lty=2)

Acf(veri_mevsim_fark_2, lag.max = 48, main="ACF (1. Trent + 2. Mevsimsel Fark)", lwd=3)
Pacf(veri_mevsim_fark_2, lag.max = 48, main="PACF (1. Trent + 2. Mevsimsel Fark)", lwd=3)

###### AYRIŞTIRMA YÖNTEMLERİ ######
# HAREKETLİ ORTALAMA (MA) YÖNTEMİ
data2_ts <- ma(veri_ts, order = 12, centre = TRUE) 

par(mar = c(5, 4, 6, 2), xpd = TRUE)
plot(window(veri_ts), 
     xlab="Zaman", 
     ylab="Değer", 
     lty=1, col=4, lwd=1, 
     main="Hareketli Ortalama")
lines(window(data2_ts), lty=3, col=2, lwd=2)
legend("topleft", 
       c("Gerçek Veri", "MA(12)"),
       lwd=c(2,2), 
       lty=c(1,3), 
       cex=0.7, 
       col=c(4,2),
       inset = c(0, -0.25), 
       bty = "n")

# TOPLAMSAL AYRIŞTIRMA MODELİ
# Trendi Bulma (tslm)
data_trent <- tslm(veri_ts ~ trend)
periyot <- veri_ts - data_trent[["fitted.values"]]

# Mevsimselliği Ayıklama (Veri - MA)
mevsim <- veri_ts - data2_ts

# Mevsimsel İndekslerin Hesabı (Matris - Aylık)
# Veri uzunluğunu güvenli hale getirme 
len <- length(mevsim)
if(len %% 12 != 0) {
  mevsim_vec <- c(mevsim, rep(NA, 12 - (len %% 12)))
} else {
  mevsim_vec <- mevsim
}

# 12 Aylık Matris
donemort <- t(matrix(data = mevsim_vec, nrow = 12))
endeks <- colMeans(donemort, na.rm = T) - mean(colMeans(donemort, na.rm = T))

# İndeksleri Seriye Yayma
indeks <- ts(rep(endeks, length.out = length(veri_ts)), start=start(veri_ts), frequency=12)

# Kontrol 
indeks_alternatif <- decompose(veri_ts, "additive")

# Saf Trend, Tahmin ve Hata
trenthata <- veri_ts - indeks
trent <- tslm(trenthata ~ trend)
tahmin <- indeks + trent[["fitted.values"]]
hata <- veri_ts - indeks - trent[["fitted.values"]]

# GRAFİK: Orijinal Seri vs Tahmin
par(mar = c(5, 4, 6, 2), xpd = TRUE)
plot(window(veri_ts), 
     xlab="Zaman", ylab="Değer", 
     lty=1, col=4, lwd=2, 
     main="Toplamsal Ayrıştırma Sonucu")
lines(window(tahmin), lty=3, col=2, lwd=3)
legend("topleft", 
       c("Gerçek Veri", "Tahmin"),
       lwd = c(2,2), 
       lty = c(1,3), 
       col = c(4,2), 
       cex = 0.7,
       inset = c(0, -0.25), 
       bty = "n")

# Akgürültü kontrolü
if(!is.null(dev.list())) dev.off()
Acf(hata, main="Toplamsal Model Hataları", lag.max = 42, ylim=c(-1,1), lwd=3)
Pacf(hata, main="Toplamsal Model Hataları", lag.max = 42, ylim=c(-1,1), lwd=3)
Box.test(hata, lag = 24, type = "Ljung-Box")

print(paste("Toplamsal HKO:", sqrt(mean(hata^2, na.rm=T))))

#ÇARPIMSAL AYRIŞTIRMA MODELİ
# Mevsimselliği Ayıklama (BÖLME İŞLEMİ)
mevsim1 <- veri_ts / data2_ts

# Mevsimsel İndekslerin Hesabı (Matris - Aylık)
if(length(mevsim1) %% 12 != 0) {
  mevsim1_vec <- c(mevsim1, rep(NA, 12 - (length(mevsim1) %% 12)))
} else {
  mevsim1_vec <- mevsim1
}

# 12 Aylık Matris
donemort1 <- t(matrix(data = mevsim1_vec, nrow = 12))

# (Çarpımsal Kuralı):
# İndekslerin ortalaması 1 olmalıdır. Bu yüzden ortalamaya bölüyoruz.
endeks1 <- colMeans(donemort1, na.rm = T) / mean(colMeans(donemort1, na.rm = T))

# İndeksleri Seriye Yayma
# Senin verinin uzunluğuna göre ayarlıyoruz
indeks1 <- ts(rep(endeks1, length.out = length(veri_ts)), start=start(veri_ts), frequency=12)

# Kontrol (decompose fonksiyonu ile)
indeks1_alternatif <- decompose(veri_ts, "multiplicative")

# Saf Trend, Tahmin ve Hata
# Trendi bulmak için veriyi indekse bölünür
trenthata1 <- veri_ts / indeks1
trent1 <- tslm(trenthata1 ~ trend)

# Tahmin = İndeks * Trend (ÇARPMA)
tahmin1 <- indeks1 * trent1[["fitted.values"]]

# Hata = Gerçek - Tahmin (Karşılaştırma için fark alıyoruz)
hata1 <- veri_ts - tahmin1

# Grafik çizimi
if(!is.null(dev.list())) dev.off()

par(mar = c(5, 4, 6, 2), xpd = TRUE)
plot(window(veri_ts), 
     xlab="Zaman", ylab="Değer", 
     lty=1, col=4, lwd=2, 
     main="Çarpımsal Ayrıştırma Sonucu")
lines(window(tahmin1), lty=3, col=2, lwd=3)
legend("topleft", 
       c("Gerçek Veri", "Tahmin"),
       lwd = c(2,2), 
       lty = c(1,3), 
       col = c(4,2), 
       cex = 0.7,
       inset = c(0, -0.25),
       bty = "n")

if(!is.null(dev.list())) dev.off()
# Hatalar Akgürültü mü?
Acf(hata1, main="Çarpımsal Model Hataları (ACF)", lag.max = 42, ylim=c(-1,1), lwd=3)
Pacf(hata1, main="Çarpımsal Model Hataları (PACF)", lag.max = 42, ylim=c(-1,1), lwd=3)
Box.test(hata1, lag = 24, type = "Ljung-Box")
print(paste("Çarpımsal HKO:", sqrt(mean(hata1^2, na.rm=T))))


###### Regresyon Analizi ######
#DÖNÜŞTÜRME (Box-Cox Transformation)
# Lambda parametresini otomatik buluyoruz
lambda_degeri <- BoxCox.lambda(veri_ts)
print(paste("Bulunan Lambda Değeri:", round(lambda_degeri, 4)))
veri_donusumlu <- BoxCox(veri_ts, lambda_degeri)
# Dönüşmüş verinin grafiği
plot(veri_donusumlu, main="Box-Cox Dönüşümü Yapılmış Seri", ylab="Dönüşümlü Değerler", col="darkblue")

# TOPLAMSAL MODEL #
n <- length(veri_donusumlu)
t <- 1:n 

sin1 <- sin(2 * pi * t / 12)
cos1 <- cos(2 * pi * t / 12)

sin2 <- sin(2 * pi * 2 * t / 12)
cos2 <- cos(2 * pi * 2 * t / 12)

veriseti_reg <- data.frame(y = as.numeric(veri_donusumlu), t = t, 
                           sin1 = sin1, cos1 = cos1, 
                           sin2 = sin2, cos2 = cos2)

# Model 1: Sadece t, sin1, cos1
regresyon_model1 <- lm(y ~ t + sin1 + cos1, data = veriseti_reg)
summary(regresyon_model1)

# Model 2: t, sin1, cos1 + sin2, cos2
regresyon_model2 <- lm(y ~ t + sin1 + cos1 + sin2 + cos2, data = veriseti_reg)
summary(regresyon_model2)

#TAHMİN VE GRAFİK (Model 2 İçin)
secilen_model <- regresyon_model2

# Tahmin ve Güven Aralıkları
tahmin_tum <- predict(secilen_model, interval="confidence", level=0.95)

# Geri Dönüştürme 
tahmin_gercek <- InvBoxCox(tahmin_tum[, "fit"], lambda_degeri)
alt_sinir <- InvBoxCox(tahmin_tum[, "lwr"], lambda_degeri)
ust_sinir <- InvBoxCox(tahmin_tum[, "upr"], lambda_degeri)

# Zaman serisi formatına çevirme
tahmin_serisi <- ts(tahmin_gercek, start=start(veri_ts), frequency=12)
alt_sinir_serisi <- ts(alt_sinir, start=start(veri_ts), frequency=12)
ust_sinir_serisi <- ts(ust_sinir, start=start(veri_ts), frequency=12)

if(!is.null(dev.list())) dev.off()
par(mar = c(5, 4, 7, 2), xpd = TRUE)
plot(veri_ts, 
     main="Regresyon (Model 2)", 
     ylab="Değer", lwd=1, col="black")
lines(alt_sinir_serisi, col="blue", lwd=1, lty=1) 
lines(ust_sinir_serisi, col="blue", lwd=1, lty=1) 
legend("topleft", 
       legend=c("Gerçek Veri", "%95 Güven Aralığı"), 
       col=c("black", "blue"), 
       lty=c(1,1), 
       lwd=c(1,1), 
       inset=c(0, -0.40), 
       bty="n", cex=0.8)

hata_reg <- veri_ts - tahmin_serisi
# Durbin-Watson Testi (Otokorelasyon Kontrolü)
install.packages("lmtest")
library(lmtest)
print("Durbin-Watson Testi")
dwtest(secilen_model)

library(forecast)
if(!is.null(dev.list())) dev.off() 
Acf(hata_reg, main="Model 2 Hataları (ACF)", lag.max=36, lwd=3)
Pacf(hata_reg, main="Model 2 Hataları (PACF)", lag.max=36, lwd=3)
 
print("--- Box-Ljung Testi ---")
Box.test(hata_reg, lag=24, type="Ljung-Box")

HKO_regresyon <- sqrt(mean(hata_reg^2, na.rm=T))
print(paste("Regresyon (Model 2) HKO Değeri:", round(HKO_regresyon, 4)))

# ÇarpıMSAL MODEL #
library(forecast)
library(lmtest)

n <- length(veri_donusumlu)
t <- 1:n

# 1. Harmonikler (Çarpımsal) 
sin1_c <- t * sin(2 * pi * t / 12)
cos1_c <- t * cos(2 * pi * t / 12)

veriseti_carpim1 <- data.frame(y = as.numeric(veri_donusumlu), t = t, 
                               sin1_c = sin1_c, cos1_c = cos1_c)

# Model 1'i Kurma
model_carpimsal1 <- lm(y ~ t + sin1_c + cos1_c, data = veriseti_carpim1)
print("ÇARPIMSAL MODEL 1 ÖZETİ")
summary(model_carpimsal1)

# 2. Harmonikler (Çarpımsal) 
sin2_c <- t * sin(2 * pi * 2 * t / 12)
cos2_c <- t * cos(2 * pi * 2 * t / 12)

veriseti_carpim2 <- data.frame(y = as.numeric(veri_donusumlu), t = t, 
                               sin1_c = sin1_c, cos1_c = cos1_c,
                               sin2_c = sin2_c, cos2_c = cos2_c)

model_carpimsal2 <- lm(y ~ t + sin1_c + cos1_c + sin2_c + cos2_c, data = veriseti_carpim2)

print("ÇARPIMSAL MODEL 2 ÖZETİ")
summary(model_carpimsal2)


secilen_model_carpim <- model_carpimsal2

# Tahmin ve güven aralıkları
tahmin_tum_c <- predict(secilen_model_carpim, interval="confidence", level=0.95)

# Geri dönüştürme
tahmin_gercek_c <- InvBoxCox(tahmin_tum_c[, "fit"], lambda_degeri)
alt_sinir_c <- InvBoxCox(tahmin_tum_c[, "lwr"], lambda_degeri)
ust_sinir_c <- InvBoxCox(tahmin_tum_c[, "upr"], lambda_degeri)

# Zaman Serisi Formatı
tahmin_serisi_c <- ts(tahmin_gercek_c, start=start(veri_ts), frequency=12)
alt_sinir_serisi_c <- ts(alt_sinir_c, start=start(veri_ts), frequency=12)
ust_sinir_serisi_c <- ts(ust_sinir_c, start=start(veri_ts), frequency=12)

if(!is.null(dev.list())) dev.off() 
par(mar = c(5, 4, 7, 2), xpd = TRUE) 
plot(veri_ts, 
     main="Çarpımsal Regresyon (Model 2)", 
     ylab="Değer", lwd=1, col="black")

# Güven Sınırları
lines(alt_sinir_serisi_c, col="blue", lwd=1, lty=1) 
lines(ust_sinir_serisi_c, col="blue", lwd=1, lty=1) 

legend("topleft", 
       legend=c("Gerçek Veri", "%95 Güven Aralığı"), 
       col=c("black", "blue"), 
       lty=c(1,1), lwd=c(1,1), 
       inset=c(0, -0.40), 
       bty="n", cex=0.8)

hata_carpim <- veri_ts - tahmin_serisi_c

# Durbin-Watson Testi
print("--- Durbin-Watson (Çarpımsal) ---")
dwtest(secilen_model_carpim)

if(!is.null(dev.list())) dev.off()
Acf(hata_carpim, main="Çarpımsal Model Hataları (ACF)", lag.max=36, lwd=3)
Pacf(hata_carpim, main="Çarpımsal Model Hataları (PACF)", lag.max=36, lwd=3)

Box.test(hata_carpim, lag=24, type="Ljung-Box")

HKO_carpim_reg <- sqrt(mean(hata_carpim^2, na.rm=T))

print(paste("Çarpımsal Regresyon HKO Değeri:", round(HKO_carpim_reg, 4)))

###### ÜSTEL DÜZLEŞTİRME YÖNTEMİ ######
## TOPLAMSAL WINTERS YÖNTEMİ ##
library(forecast)
winters_toplamsal <- ets(veri_ts, model = "AAA")
summary(winters_toplamsal)

tahmin_winters_top <- winters_toplamsal[["fitted"]]

if(!is.null(dev.list())) dev.off()
par(mar = c(5, 4, 4, 2))
plot(window(veri_ts), 
     xlab="Zaman", ylab="Değer", 
     main="Toplamsal Winters Metodu (AAA)",
     lty=1, col="black", lwd=2)
lines(window(tahmin_winters_top), lty=2, col="red", lwd=2)


hata_winters_top <- winters_toplamsal[["residuals"]]
Box.test(hata_winters_top, lag = 24, type = "Ljung-Box")

if(!is.null(dev.list())) dev.off()
Acf(hata_winters_top, main="Toplamsal Winters Hataları (ACF)", lag.max=36, lwd=3)
Pacf(hata_winters_top, main="Toplamsal Winters Hataları (PACF)", lag.max=36, lwd=3)

## ÇARPIMSAL WINTERS YÖNTEMİ ##
winters_carpimsal <- ets(abs(veri_ts), model = "MAM")
summary(winters_carpimsal)

tahmin_winters_c <- winters_carpimsal[["fitted"]]

if(!is.null(dev.list())) dev.off()
par(mar = c(5, 4, 4, 2)) 
plot(window(veri_ts), 
     xlab="Zaman", ylab="Değer", 
     main="Çarpımsal Winters Metodu (MAM)",
     lty=1, col="black", lwd=2)
lines(window(tahmin_winters_c), lty=2, col="red", lwd=2)


hata_winters_c <- winters_carpimsal[["residuals"]]
Box.test(hata_winters_c, lag = 24, type = "Ljung-Box")

if(!is.null(dev.list())) dev.off()
Acf(hata_winters_c, main="Çarpımsal Winters Hataları (ACF)", lag.max=36, lwd=3, ylim=c(-1,1))
Pacf(hata_winters_c, main="Çarpımsal Winters Hataları (PACF)", lag.max=36, lwd=3, ylim=c(-1,1))


##### BOX JENKINS MODELLERİ #####
install.packages("forecast")
install.packages("readxl")

library(forecast) 
library(readxl)
# seriyi Yükleme
data <- read_excel("C:/Users/zehra/OneDrive - hacettepe.edu.tr/Masaüstü/EVDS.xlsx")

# Zaman Serisi oluşturma
veri_vektor <- data[[1]] 
veri_ts <- ts(veri_vektor, start=c(2005, 6), frequency=12)

# Veriyi kontrol etme
print(veri_ts)

# Orijinal Serinin Grafikleri
ts.plot(veri_ts, gpars=list(xlab="Zaman", ylab="Değerler", main="EVDS Verisi Zaman Serisi"), 
        lwd=2, col="blue")

Acf(veri_ts, lag.max = 42, main="Orijinal Seri ACF Grafiği", ylim=c(-1,1), lwd=3)
Pacf(veri_ts, lag.max = 42, main="Orijinal Seri PACF Grafiği", ylim=c(-1,1), lwd=3)



# DURAĞANLAŞTIRMA (FARK ALMA İŞLEMLERİ) 
# Trent için 1. Dereceden Fark Alma
veri_diff <- diff(veri_ts)

# Farkı alınmış serinin grafiği
ts.plot(veri_diff, 
        gpars=list(xlab="Zaman", ylab="Fark Değerleri", main="1. Dereceden Farkı Alınmış Seri"), 
        lwd=2, col="red")
abline(h=0, col="blue", lty=2) 

# Trend farkı sonrası ACF/PACF 
Acf(veri_diff, lag.max = 42, main="1. Trent Farkı Alınmış Serinin ACF Grafiği", ylim=c(-1,1), lwd=3)
Pacf(veri_diff, lag.max = 42, main="1. Tren Farkı Alınmış Serinin PACF Grafiği", ylim=c(-1,1), lwd=3)


# Mevsimsellik İçin 1. Dereceden Fark Alma
veri_mevsim_fark_1 <- diff(veri_diff, lag = 12)

ts.plot(veri_mevsim_fark_1, 
        main="Trend + 1. Mevsimsel Farkı Alınmış Seri", 
        ylab="Değer", col="blue")
abline(h=0, col="red", lty=2)

# ACF ve PACF
Acf(veri_mevsim_fark_1, lag.max = 48, main="ACF (1. Trent + 1. Mevsimsel Fark)", lwd=3)
Pacf(veri_mevsim_fark_1, lag.max = 48, main="PACF (1. Trent + 1. Mevsimsel Fark)", lwd=3)


# Mevsimsellik İçin 2. Dereceden Fark Alma
veri_mevsim_fark_2 <- diff(veri_mevsim_fark_1, lag = 12)

ts.plot(veri_mevsim_fark_2, 
        main="Trend + 2 Kez Mevsimsel Fark Alınmış Seri", 
        ylab="Değer", col="darkgreen", lwd=2)
abline(h=0, col="red", lty=2)

# ACF ve PACF
Acf(veri_mevsim_fark_2, lag.max = 48, main="ACF (1. Trent + 2. Mevsimsel Fark)", lwd=3)
Pacf(veri_mevsim_fark_2, lag.max = 48, main="PACF (1. Trent + 2. Mevsimsel Fark)", lwd=3)

## ACF yavaş azalıyor. AR ##
library(forecast)
library(lmtest)

# Model:ARIMA(1,1,0)(1,2,0)
arima2 <- Arima(veri_ts, order=c(1,1,0), seasonal=c(1,2,0), include.constant=TRUE)
coeftest(arima2)
summary(arima2)
#anlamlı
hata2 <- residuals(arima2)
Acf(hata2, main="Hata ACF (Model 2)", lag.max=42, ylim=c(-1,1), lwd=3)
Pacf(hata2, main="Hata PACF (Model 2)", lag.max=42, ylim=c(-1,1), lwd=3)
Box.test(hata2, lag=24, type="Ljung-Box", fitdf=2)

n <- length(hata2)
sinir_degeri <- 1.96 / sqrt(n)
acf_data <- Acf(hata2, lag.max=20, plot=FALSE)
acf_degerleri <- acf_data$acf[2:21] # İlk 20 gecikme

# Sig. (P-Value) Hesaplama
# Formül: Z = ACF * sqrt(n) -> P = 2*(1-pnorm(|Z|))
z_skoru <- acf_degerleri * sqrt(n)
sig_degerleri <- 2 * (1 - pnorm(abs(z_skoru)))

tablo2 <- data.frame(
  Gecikme = 1:20,
  ACF = round(acf_degerleri, 3),
  Limit = round(sinir_degeri, 3),
  Sig_Degeri = round(sig_degerleri, 3), 
  Durum = ifelse(sig_degerleri < 0.05, 
                 "ANLAMLI", 
                 "Anlamsız")
)
print(tablo2)


# Farklı Kombinasyonlar
# Model: ARIMA(1,1,0)(2,2,0)
arima3 <- Arima(veri_ts, order=c(1,1,0), seasonal=c(2,2,0), include.constant=TRUE)
coeftest(arima3)
summary(arima3)
#anlamlı
hata3 <- residuals(arima3)
Acf(hata3, main="Hata ACF (Model 3)", lag.max=42, ylim=c(-1,1), lwd=3)
Pacf(hata3, main="Hata PACF (Model 3)", lag.max=42, ylim=c(-1,1), lwd=3)
Box.test(hata3, lag=24, type="Ljung-Box", fitdf=3)

n <- length(hata3)
sinir_degeri <- 1.96 / sqrt(n)
acf_data <- Acf(hata3, lag.max=20, plot=FALSE)
acf_degerleri <- acf_data$acf[2:21] # İlk 20 gecikme

# Sig. (P-Value) Hesaplama
# Formül: Z = ACF * sqrt(n) -> P = 2*(1-pnorm(|Z|))
z_skoru <- acf_degerleri * sqrt(n)
sig_degerleri <- 2 * (1 - pnorm(abs(z_skoru)))

tablo3 <- data.frame(
  Gecikme = 1:20,
  ACF = round(acf_degerleri, 3),
  Limit = round(sinir_degeri, 3),
  Sig_Degeri = round(sig_degerleri, 3), 
  Durum = ifelse(sig_degerleri < 0.05, 
                 "ANLAMLI", 
                 "Anlamsız")
)
print(tablo3)


# Model: ARIMA(1,1,0)(3,2,0)
arima4 <- Arima(veri_ts, order=c(1,1,0), seasonal=c(3,2,0), include.constant=TRUE)
coeftest(arima4)
summary(arima4)
#anlamlı
hata4 <- residuals(arima4)
Acf(hata4, main="Hata ACF (Model 4)", lag.max=42, ylim=c(-1,1), lwd=3)
Pacf(hata4, main="Hata PACF (Model 4)", lag.max=42, ylim=c(-1,1), lwd=3)
Box.test(hata4, lag=24, type="Ljung-Box")
checkresiduals(arima4)

n <- length(hata4)
sinir_degeri <- 1.96 / sqrt(n)
acf_data <- Acf(hata4, lag.max=20, plot=FALSE)
acf_degerleri <- acf_data$acf[2:21] # İlk 20 gecikme

# Sig. (P-Value) Hesaplama
# Formül: Z = ACF * sqrt(n) -> P = 2*(1-pnorm(|Z|))
z_skoru <- acf_degerleri * sqrt(n)
sig_degerleri <- 2 * (1 - pnorm(abs(z_skoru)))

tablo4 <- data.frame(
  Gecikme = 1:20,
  ACF = round(acf_degerleri, 3),
  Limit = round(sinir_degeri, 3),
  Sig_Degeri = round(sig_degerleri, 3), 
  Durum = ifelse(sig_degerleri < 0.05, 
                 "ANLAMLI", 
                 "Anlamsız")
)
print(tablo4)


# Model: ARIMA(1,1,0)(0,2,0)
arima1 <- Arima(veri_ts, order=c(1,1,0), seasonal=c(0,2,0), include.constant=TRUE)
coeftest(arima1)
summary(arima1)
#anlamlı
hata1 <- residuals(arima1)
Acf(hata1, main="Hata ACF (Model 1)", lag.max=42, ylim=c(-1,1), lwd=3)
Pacf(hata1, main="Hata PACF (Model 1)", lag.max=42, ylim=c(-1,1), lwd=3)
Box.test(hata1, lag=24, type="Ljung-Box", fitdf=1)

n <- length(hata1)
sinir_degeri <- 1.96 / sqrt(n)
acf_data <- Acf(hata1, lag.max=20, plot=FALSE)
acf_degerleri <- acf_data$acf[2:21] # İlk 20 gecikme

# Sig. (P-Value) Hesaplama
# Formül: Z = ACF * sqrt(n) -> P = 2*(1-pnorm(|Z|))
z_skoru <- acf_degerleri * sqrt(n)
sig_degerleri <- 2 * (1 - pnorm(abs(z_skoru)))

tablo1 <- data.frame(
  Gecikme = 1:20,
  ACF = round(acf_degerleri, 3),
  Limit = round(sinir_degeri, 3),
  Sig_Degeri = round(sig_degerleri, 3), 
  Durum = ifelse(sig_degerleri < 0.05, 
                 "ANLAMLI", 
                 "Anlamsız")
)
print(tablo1)


# Model: ARIMA(1,1,0)(1,2,1)[12]
arima7 <- Arima(veri_ts, order=c(1,1,0), seasonal=c(1,2,1), include.constant=TRUE)
coeftest(arima7)
summary(arima7)
#anlamsız

# Model: ARIMA(1,1,0)(0,2,1)[12]
arima5 <- Arima(veri_ts, order=c(1,1,0), seasonal=c(0,2,1), include.constant=TRUE)
coeftest(arima5)
summary(arima5)
#anlamlı
hata <- residuals(arima5)
if(!is.null(dev.list())) dev.off()
Acf(hata, main="Hata ACF", lag.max=42, ylim=c(-1,1), lwd=3)
Pacf(hata, main="Hata PACF", lag.max=42, ylim=c(-1,1), lwd=3)
Box.test(hata, lag=24, type="Ljung-Box")

#İLK 20 GECİKME
hata <- residuals(arima5)
n <- length(hata)
sinir_degeri <- 1.96 / sqrt(n) 

acf_data <- Acf(hata, lag.max=20, plot=FALSE)
acf_degerleri <- acf_data$acf[2:21] 

# 3. Sig. (P-Value) Hesaplama Formülü
# SPSS mantığı: Z = r * sqrt(n) üzerinden p-değeri hesaplanır.
z_skoru <- acf_degerleri * sqrt(n)
sig_degerleri <- 2 * (1 - pnorm(abs(z_skoru)))

tablo <- data.frame(
  Gecikme = 1:20,
  ACF = round(acf_degerleri, 3),
  Limit = round(sinir_degeri, 3),
  Sig_Degeri = round(sig_degerleri, 3),
  Durum = ifelse(sig_degerleri < 0.05, 
                 "ANLAMLI", 
                 "Anlamsız")
)

print(tablo)



# Model: ARIMA(1,1,0)(0,2,2)[12]
arima6 <- Arima(veri_ts, order=c(1,1,0), seasonal=c(0,2,2), include.constant=TRUE)
coeftest(arima6)
summary(arima6)
#anlamsız

# Model: ARIMA(1,1,0)(2,2,1)[12]
arima8 <- Arima(veri_ts, order=c(1,1,0), seasonal=c(2,2,1), include.constant=TRUE)
coeftest(arima8)
summary(arima8)
#anlamsız

## PACF yavaş azalıyor. MA ##
# Model: ARIMA(0,1,2)(0,2,1)[12]
arima10 <- Arima(veri_ts, order=c(0,1,2), seasonal=c(0,2,1), include.constant=TRUE)
coeftest(arima10)
summary(arima10)
#anlamlı
hata10 <- residuals(arima10)
Acf(hata10, main="Hata ACF (Model 10)", lag.max=42, ylim=c(-1,1), lwd=3)
Pacf(hata10, main="Hata PACF (Model 10)", lag.max=42, ylim=c(-1,1), lwd=3)
Box.test(hata10, lag=24, type="Ljung-Box")
checkresiduals(arima10)
library(forecast)

n <- length(hata10)
sinir_degeri <- 1.96 / sqrt(n)
acf_data <- Acf(hata10, lag.max=20, plot=FALSE)
acf_degerleri <- acf_data$acf[2:21] 

# Sig. (P-Value) Hesaplama
# Formül: Z = ACF * sqrt(n) -> P = 2*(1-pnorm(|Z|))
z_skoru <- acf_degerleri * sqrt(n)
sig_degerleri <- 2 * (1 - pnorm(abs(z_skoru)))

tablo10 <- data.frame(
  Gecikme = 1:20,
  ACF = round(acf_degerleri, 3),
  Limit = round(sinir_degeri, 3),
  Sig_Degeri = round(sig_degerleri, 3), 
  Durum = ifelse(sig_degerleri < 0.05, 
                 "ANLAMLI", 
                 "Anlamsız")
)

print(tablo10)


# Model: ARIMA(0,1,2)(0,2,2)[12]
arima11 <- Arima(veri_ts, order=c(0,1,2), seasonal=c(0,2,2), include.constant=TRUE)
coeftest(arima11)
summary(arima11)
#anlamsız

# Model: ARIMA(0,1,2)(1,2,1)[12]
arima13 <- Arima(veri_ts, order=c(0,1,2), seasonal=c(1,2,1), include.constant=TRUE)
coeftest(arima13)
summary(arima13)
#anlamsız

# Model: ARIMA(0,1,2)(1,2,0)[12]
arima12 <- Arima(veri_ts, order=c(0,1,2), seasonal=c(1,2,0), include.constant=TRUE)
coeftest(arima12)
summary(arima12)
#anlamlı
hata12 <- residuals(arima12)
Acf(hata12, main="Hata ACF (Model 12)", lag.max=42, ylim=c(-1,1), lwd=3)
Pacf(hata12, main="Hata PACF (Model 12)", lag.max=42, ylim=c(-1,1), lwd=3)
Box.test(hata12, lag=24, type="Ljung-Box", fitdf=3)

n <- length(hata12)
sinir_degeri <- 1.96 / sqrt(n)
acf_data <- Acf(hata12, lag.max=20, plot=FALSE)
acf_degerleri <- acf_data$acf[2:21] # İlk 20 gecikme

# Sig. (P-Value) Hesaplama
# Formül: Z = ACF * sqrt(n) -> P = 2*(1-pnorm(|Z|))
z_skoru <- acf_degerleri * sqrt(n)
sig_degerleri <- 2 * (1 - pnorm(abs(z_skoru)))

tablo12 <- data.frame(
  Gecikme = 1:20,
  ACF = round(acf_degerleri, 3),
  Limit = round(sinir_degeri, 3),
  Sig_Degeri = round(sig_degerleri, 3), 
  Durum = ifelse(sig_degerleri < 0.05, 
                 "ANLAMLI", 
                 "Anlamsız")
)
print(tablo12)


# Model: ARIMA(0,1,2)(2,2,0)[12]
arima14 <- Arima(veri_ts, order=c(0,1,2), seasonal=c(2,2,0), include.constant=TRUE)
coeftest(arima14)
summary(arima14)
#anlamlı
hata14 <- residuals(arima14)
Acf(hata14, main="Hata ACF (Model 14)", lag.max=42, ylim=c(-1,1), lwd=3)
Pacf(hata14, main="Hata PACF (Model 14)", lag.max=42, ylim=c(-1,1), lwd=3)
Box.test(hata14, lag=24, type="Ljung-Box", fitdf=4)

n <- length(hata14)
sinir_degeri <- 1.96 / sqrt(n)
acf_data <- Acf(hata14, lag.max=20, plot=FALSE)
acf_degerleri <- acf_data$acf[2:21] # İlk 20 gecikme

#Sig. (P-Value) Hesaplama
# Formül: Z = ACF * sqrt(n) -> P = 2*(1-pnorm(|Z|))
z_skoru <- acf_degerleri * sqrt(n)
sig_degerleri <- 2 * (1 - pnorm(abs(z_skoru)))

tablo14 <- data.frame(
  Gecikme = 1:20,
  ACF = round(acf_degerleri, 3),
  Limit = round(sinir_degeri, 3),
  Sig_Degeri = round(sig_degerleri, 3), 
  Durum = ifelse(sig_degerleri < 0.05, 
                 "ANLAMLI", 
                 "Anlamsız")
)
print(tablo14)


# Model: ARIMA(1,1,2)(2,2,0)[12]
arima17 <- Arima(veri_ts, order=c(1,1,2), seasonal=c(2,2,0), include.constant=TRUE)
coeftest(arima17)
summary(arima17)
#anlamlı
hata17 <- residuals(arima17)

if(!is.null(dev.list())) dev.off()
Acf(hata17, main="Hata ACF (tahmin modeli)", lag.max=42, ylim=c(-1,1), lwd=3)
Pacf(hata17, main="Hata PACF (tahmin modeli)", lag.max=42, ylim=c(-1,1), lwd=3)
print(Box.test(hata17, lag=24, type="Ljung-Box", fitdf=5))

##ilk 20 gecikme
n <- length(hata17)
sinir_degeri <- 1.96 / sqrt(n)
acf_data <- Acf(hata17, lag.max=20, plot=FALSE)
acf_degerleri <- acf_data$acf[2:21] 
z_skoru <- acf_degerleri * sqrt(n)
sig_degerleri <- 2 * (1 - pnorm(abs(z_skoru)))

tablo17 <- data.frame(
  Gecikme = 1:20,
  ACF = round(acf_degerleri, 3),
  Limit = round(sinir_degeri, 3),
  Sig_Degeri = round(sig_degerleri, 3),
  Durum = ifelse(sig_degerleri < 0.05, 
                 "ANLAMLI", 
                 "Anlamsız")
)
print(tablo17)

library(ggplot2)
model_uyum_degerleri <- fitted(arima17)
autoplot(veri_ts, series="Gerçek Veri") +
  autolayer(model_uyum_degerleri, series="Model Tahmini (Fitted)", lwd=1) +
  ggtitle("Model Uyumu: Gerçek Veriler ve Modelin Tahminleri") +
  xlab("Zaman") +
  ylab("Değerler") +
  theme_bw() +
  scale_color_manual(values=c("Gerçek Veri"="black", "Model Tahmini (Fitted)"="red")) +
  theme(legend.position="bottom")



# Model: ARIMA(0,1,2)(3,2,0)[12]
arima15 <- Arima(veri_ts, order=c(0,1,2), seasonal=c(3,2,0), include.constant=TRUE)
coeftest(arima15)
summary(arima15)
#anlamlı
hata15 <- residuals(arima15)
Acf(hata15, main="Hata ACF (Model 15)", lag.max=42, ylim=c(-1,1), lwd=3)
Pacf(hata15, main="Hata PACF (Model 15)", lag.max=42, ylim=c(-1,1), lwd=3)
Box.test(hata15, lag=24, type="Ljung-Box", fitdf=5)

n <- length(hata15)
sinir_degeri <- 1.96 / sqrt(n)
acf_data <- Acf(hata15, lag.max=20, plot=FALSE)
acf_degerleri <- acf_data$acf[2:21] # İlk 20 gecikme

# Sig. (P-Value) Hesaplama
# Formül: Z = ACF * sqrt(n) -> P = 2*(1-pnorm(|Z|))
z_skoru <- acf_degerleri * sqrt(n)
sig_degerleri <- 2 * (1 - pnorm(abs(z_skoru)))

tablo15 <- data.frame(
  Gecikme = 1:20,
  ACF = round(acf_degerleri, 3),
  Limit = round(sinir_degeri, 3),
  Sig_Degeri = round(sig_degerleri, 3), 
  Durum = ifelse(sig_degerleri < 0.05, 
                 "ANLAMLI", 
                 "Anlamsız")
)
print(tablo15)



# Model: ARIMA(0,1,2)(0,2,0)[12]
arima9 <- Arima(veri_ts, order=c(0,1,2), seasonal=c(0,2,0), include.constant=TRUE)
coeftest(arima9)
summary(arima9)
#anlamlı
hata9 <- residuals(arima9)
Acf(hata9, main="Hata ACF (Model 9)", lag.max=42, ylim=c(-1,1), lwd=3)
Pacf(hata9, main="Hata PACF (Model 9)", lag.max=42, ylim=c(-1,1), lwd=3)
Box.test(hata9, lag=24, type="Ljung-Box", fitdf=2)

n <- length(hata9)
sinir_degeri <- 1.96 / sqrt(n)
acf_data <- Acf(hata9, lag.max=20, plot=FALSE)
acf_degerleri <- acf_data$acf[2:21] 
# Sig. (P-Value) Hesaplama
# Formül: Z = ACF * sqrt(n) -> P = 2*(1-pnorm(|Z|))
z_skoru <- acf_degerleri * sqrt(n)
sig_degerleri <- 2 * (1 - pnorm(abs(z_skoru)))

tablo9 <- data.frame(
  Gecikme = 1:20,
  ACF = round(acf_degerleri, 3),
  Limit = round(sinir_degeri, 3),
  Sig_Degeri = round(sig_degerleri, 3), 
  Durum = ifelse(sig_degerleri < 0.05, 
                 "ANLAMLI", 
                 "Anlamsız")
)
print(tablo9)


## Hem ACF hem PACF yavaş azalıyor. ARMA ##
library(forecast)
library(lmtest)

# Model: ARIMA(1,1,2)(1,2,1)[12]
arima16 <- Arima(veri_ts, order=c(1,1,2), seasonal=c(1,2,1), include.constant=TRUE)
coeftest(arima16)
summary(arima16)
#anlamsız

#### arima17 modeli için öngörü ####
library(forecast)
# Gelecek 5 Ayın Tahmini 
ongoru <- forecast(arima17, h=5)
tahmin_degerleri <- as.numeric(ongoru$mean)

# Öngörü Tablosu 
tarihler_gelecek <- c("2018-01", "2018-02", "2018-03", "2018-04", "2018-05")

tablo_ongoru <- data.frame(
  Donem = tarihler_gelecek,
  Ongoru_Degeri = round(tahmin_degerleri, 2)
)

# Gerçek Verilerle Karşılaştırma Tablosu 
son_gercekler <- tail(veri_ts, 10)

tarihler_gecmis <- c("2017-03", "2017-04", "2017-05", "2017-06", "2017-07", 
                     "2017-08", "2017-09", "2017-10", "2017-11", "2017-12")
tablo_gercek <- data.frame(
  Donem_Gercek = tarihler_gecmis,
  Gercek_Deger = round(as.numeric(son_gercekler), 2)
)


print(tablo_ongoru)
print(tablo_gercek)

autoplot(ongoru) +
  ggtitle("Tekstil Ürünleri İmalatı: 5 Aylık Gelecek Tahmini (2018)") +
  xlab("Yıl") +
  ylab("Endeks Değeri") +
  theme_bw()









