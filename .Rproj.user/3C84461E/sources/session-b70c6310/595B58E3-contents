install.packages("lmtest")
install.packages("sandwich")
install.packages("tseries")
install.packages("olsrr")
library(olsrr)
library(tseries)
library(lmtest)
library(sandwich)

dataset <- read.csv("dairy_dataset.csv", header = TRUE, sep = ",")
head(dataset)
str(dataset)
colnames(dataset)
# Conversie a coloanei de date
dataset$Date <- as.Date(dataset$Date, format = "%Y-%m-%d")
# Filtrare pentru anul 2022
dataset_2022 <- subset(dataset, format(Date, "%Y") == "2022")
head(dataset_2022)
str(dataset_2022)
# Construirea modelului liniar folosind datele din 2022 (dataset_2022)
model <- lm(`Approx..Total.Revenue.INR.` ~ `Quantity.Sold..liters.kg.`, data = dataset_2022)

# Rezumatul modelului
summary(model)
#Concluziile modelului liniar
#Formula regresiei liniare : VenitulTotalAproximativ = -126.564 + 55.630 * CantitateaVanduta (l/kg)
#Interceptul (Beta0) = -126.564 . Cand cantitatea vanduta este 0, venitul total aproximativ
#este -126.564
#Variabila dependenta (Beta1) = 55.630. Pentru fiecare unitate vanduta (l/kg), venitul total
#aproximativ creste cu 55.630, caeteris paribus
#Termenul de interceptare nu este semnificativ, dar relatia dintre Cantitatea vanduta si
#venitul total este semnificativa dpdv statistic, p-value < 2e-16
#Calitatea modelului. R^2 = 0.6532 => modelul explica 65.32% din variatia veniturilor totale
#prin cantitatea vanduta 
#R^2 ajustat, similar, ajustat pentru numarul de observatii si de variabile
#Erorile reziduale au o abatere standard de 8722, variatia predictiilor in jurul valorilor reale
#F-statistic (p<2.2e-16) confirma ca modelul este semnficativ dpdv statistic (relatia dintre variabile este reala, nu aleatorie)

#Regresia simpla

# Calculul reziduurilor din model
residuals <- resid(model)

# Calculul mediei reziduurilor
mean_residuals <- mean(residuals)
mean_residuals
#media rezidurilor este -7.468573e-14, apropiata de 0 => ipoteza 2 ("Erorile au media nula") este satisfacuta

# Testul Breusch-Pagan pentru homoscedasticitate
bp_test <- bptest(model)
bp_test
#p-value < 0.05 => avem heteroscedasticitate => variatia rezidurilor nu este constanta

# Testul White
white_test <- bptest(model, ~ fitted(model) + I(fitted(model)^2), data = dataset_2022)
white_test
# p-value < 2.2e-16 => testul white confirma Breusch Pagan => heteroschedasticitate

#Pentru a rezolva probleme asociate cu varianta reziduurilor (heteroschedasticitate),
#vom aplica regresia cu valori robuste,
#modelul robust ajusteaza erorile standard, astfel incat sa fie mai precise in cazul
#heteroscedasticitatii
coeftest(model, vcov = vcovHC(model, type = "HC"))

bp_test <- bptest(model)
bp_test
#p-value < 0.05 => avem heteroscedasticitate => variatia rezidurilor nu este constanta

# Testul White
white_test <- bptest(model, ~ fitted(model) + I(fitted(model)^2), data = dataset_2022)
white_test

#Rulam testul Darbin-Watson pentru a testa autocorelarea erorilor
# Testul Durbin-Watson
dw_test <- dwtest(model)
dw_test
# DW este aproximativ 2 => absenta autocorelarii in reziduuri, iar p-value >0.05 =>
# acceptam ipoteza nula conform careia erorile nu sunt autocorelate

#Pentru testarea legaturii dintre regresor si erorile aleatoare vom folosi testul de corelatie in R
residuals <- resid(model) #extragem valorile reziduale
cor_test <- cor.test(dataset_2022$`Quantity.Sold..liters.kg.`, residuals)
cor_test
#valoarea r este extrem de apropiata de 0 => corelatie aproape inexistenta intre regresor si reziduuri
#p-value > 0.05 => acceptam ipoteza nula conform careia nu exista corelatie intre regresor (Quantity Sold) si reziduuri

#aplicam jarque-bera pentru a verifica normalitatea erorilor
# Calculul reziduurilor
residuals <- resid(model)

# Testul Jarque-Bera
jb_test <- jarque.bera.test(residuals)
jb_test
#p-value < 0.05 => respingem ipoteza nula conform careia rezidurile sunt distribuite normal

#analizam forma distributiei
# Histogramă a reziduurilor
hist(residuals, breaks = 30, main = "Histogram of Residuals", xlab = "Residuals")

# QQ plot
qqnorm(residuals)
qqline(residuals, col = "red")

ols_plot_cooksd_chart(model)
#incercam refacerea modelului pentru a obtine o forma normal distribuita a reziduurilor
hprice_model <- olsrr::ols_hprice_cook(model)
summary(hprice_model)

# Calculează distanțele Cook
cooks_dist <- cooks.distance(model)

# Pragul pentru observații influente
threshold <- 4 / nrow(dataset_2022)

# Observații influente
influential_points <- which(cooks_dist > threshold)

# Eliminare observații influente
dataset_cleaned <- dataset_2022[-influential_points, ]

model_cleaned <- lm(`Approx..Total.Revenue.INR.` ~ `Quantity.Sold..liters.kg.`, data = dataset_cleaned)
summary(model_cleaned)
# Testul Jarque-Bera
residuals <- resid(model_cleaned)
jb_test <- jarque.bera.test(residuals)
jb_test
#p-value < 0.05 => respingem ipoteza nula conform careia rezidurile sunt distribuite normal

#analizam forma distributiei
# Histogramă a reziduurilor
hist(residuals, breaks = 30, main = "Histogram of Residuals", xlab = "Residuals")

# QQ plot
qqnorm(residuals)
qqline(residuals, col = "red")

ols_plot_cooksd_chart(model_cleaned)

#dupa modificari, reziduurile tot nu au o distributie normala. Limitare a datelor.


#Prognoze
#impartim setul de date
set.seed(123)
train_indices <- sample(1:nrow(dataset_2022), 0.8 * nrow(dataset_2022))
train_data <- dataset_2022[train_indices, ]
test_data <- dataset_2022[-train_indices, ]

# Construirea modelului pe setul de antrenare
model_train <- lm(`Approx..Total.Revenue.INR.` ~ `Quantity.Sold..liters.kg.`, data = train_data)
summary(model_train)

# Prognoze și calcul MAPE
predictions <- predict(model_train, newdata = test_data)
errors <- abs(test_data$`Approx..Total.Revenue.INR.` - predictions)
mape <- mean(errors / test_data$`Approx..Total.Revenue.INR.`) * 100
print(mape)

# Definim valorile pentru prognoză
new_values <- data.frame(`Quantity.Sold..liters.kg.` = c(200, 250, 270, 300, 320))

# Prognozăm valorile folosind modelul curent
predictions <- predict(model, newdata = new_values, se.fit = TRUE, interval = "confidence", level = 0.90)

# Afișăm prognozele și intervalele de încredere
predictions

#am folsit un interval de incredere de 90%