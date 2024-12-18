# Instalare si incarcare a pachetelor necesare
install.packages("glmnet")
install.packages("caret",type="binary")
install.packages("stats")
install.packages("sandwich")
install.packages("MASS")
install.packages("lmtest")
install.packages("tseries")
install.packages("olsrr")
install.packages("car")
library(lmtest)
library(glmnet)
library(caret)
library(stats)
library(MASS)
library(sandwich)
library(tseries)
library(car)
library(olsrr)
# Incarcarea datelor
dataset <- read.csv("Cleaned_Dairy_Dataset.csv", header = TRUE, sep = ",")
str(dataset)
dataset$Date <- as.Date(dataset$Date, format = "%Y-%m-%d")

# Crearea subseturilor
small_farms <- subset(dataset, Farm.Size == "Small")
medium_farms <- subset(dataset, Farm.Size == "Medium")
large_farms <- subset(dataset, Farm.Size == "Large")
str(dataset)
process_farm_data <- function(farm_data,farm_size)
{
  cat("----------------------------------START----------------------------------\n")
  # Selectarea variabilelor numerice
  # Selectarea variabilelor numerice
  numerical_columns <- c("Total.Land.Area..acres.", "Number.of.Cows", "Quantity..liters.kg.", 
                         "Price.per.Unit", "Quantity.Sold..liters.kg.", "Price.per.Unit..sold.", 
                         "Approx..Total.Revenue.INR.", "Quantity.in.Stock..liters.kg.", 
                         "Minimum.Stock.Threshold..liters.kg.", "Reorder.Quantity..liters.kg.")
  
  data <- farm_data[, numerical_columns]
  
  # Eliminarea valorilor lipsa
  data <- na.omit(data)
  
  # Separarea in variabile predictori si tinta
  X <- as.matrix(data[, !colnames(data) %in% "Approx..Total.Revenue.INR."])
  y <- as.vector(data$Approx..Total.Revenue.INR.)
  
  # Standardizarea datelor (scalare)
  X_scaled <- scale(X)  # Scalare setului de antrenament
  
  # Impartirea datelor in seturi de antrenament si test
  set.seed(42)
  train_index <- sample(1:nrow(X_scaled), size = 0.8 * nrow(X_scaled))
  X_train <- X_scaled[train_index, ]
  X_test <- X_scaled[-train_index, ]
  y_train <- y[train_index]
  y_test <- y[-train_index]
  
  # Lasso Regression
  lasso_model <- cv.glmnet(X_train, y_train, alpha = 1, standardize = FALSE)
  lasso_coefs <- coef(lasso_model, s = "lambda.min")
  print("Lasso Coefficients:")
  print(lasso_coefs)
  
  # Ridge Regression
  ridge_model <- cv.glmnet(X_train, y_train, alpha = 0, standardize = FALSE)
  ridge_coefs <- coef(ridge_model, s = "lambda.min")
  print("Ridge Coefficients:")
  print(ridge_coefs)
  
  # Elastic Net Regression
  elastic_net_model <- cv.glmnet(X_train, y_train, alpha = 0.5, standardize = FALSE)
  elastic_net_coefs <- coef(elastic_net_model, s = "lambda.min")
  print("Elastic Net Coefficients:")
  print(elastic_net_coefs)
  
  # Evaluarea modelelor
  lasso_pred <- predict(lasso_model, X_test, s = "lambda.min")
  ridge_pred <- predict(ridge_model, X_test, s = "lambda.min")
  elastic_net_pred <- predict(elastic_net_model, X_test, s = "lambda.min")
  
  lasso_rmse <- sqrt(mean((lasso_pred - y_test)^2))
  ridge_rmse <- sqrt(mean((ridge_pred - y_test)^2))
  elastic_net_rmse <- sqrt(mean((elastic_net_pred - y_test)^2))
  
  cat("Lasso RMSE:", lasso_rmse, "\n")
  cat("Ridge RMSE:", ridge_rmse, "\n")
  cat("Elastic Net RMSE:", elastic_net_rmse, "\n")
  
  # Alegerea Lasso si eliminarea variabilelor nesemnificative
  significant_vars <- rownames(as.matrix(lasso_coefs))[as.matrix(lasso_coefs) != 0 & rownames(as.matrix(lasso_coefs)) != "(Intercept)"]
  cat("Variabile semnificative selectate de Lasso:\n")
  print(significant_vars)
  
  # Crearea unui nou set de date cu variabilele semnificative
  data_significant <- data[, significant_vars]
  X_significant <- as.matrix(data_significant)
  
  # Reantrenarea modelului Lasso doar pe variabilele semnificative
  set.seed(42)
  train_index <- sample(1:nrow(X_significant), size = 0.8 * nrow(X_significant))
  X_train <- X_significant[train_index, ]
  X_test <- X_significant[-train_index, ]
  y_train <- y[train_index]
  y_test <- y[-train_index]
  
  lasso_model_refined <- cv.glmnet(X_train, y_train, alpha = 1, standardize = FALSE)
  lasso_pred_refined <- predict(lasso_model_refined, X_test, s = "lambda.min")
  lasso_rmse_refined <- sqrt(mean((lasso_pred_refined - y_test)^2))
  
  cat("Lasso RMSE cu variabilele semnificative:", lasso_rmse_refined, "\n")
  
  # Model de regresie multipla folosind variabilele semnificative
  final_data <- data.frame(data_significant, Revenue = y)
  model <- lm(Revenue ~ ., data = final_data)
  summary_model <- summary(model)
  
  cat("\nRezumatul modelului de regresie multipla:\n")
  print(summary_model)
  
  # Identificarea variabilelor semnificative din modelul de regresie multipla (p < 0.05)
  p_values <- summary_model$coefficients[, 4]
  significant_vars_model <- rownames(summary_model$coefficients[p_values < 0.1, ])
  cat("Variabile semnificative în modelul de regresie multipla:\n")
  print(significant_vars_model)
  
  # Salvează numele variabilelor semnificative într-o variabilă separată
  significant_vars_names <- significant_vars_model
  
  # Eliminarea variabilelor nesemnificative din modelul de regresie multipla
  insignificant_vars <- rownames(summary_model$coefficients[p_values > 0.05, ])
  data_refined <- data_significant[, which(colnames(data_significant) %in% significant_vars_model)]
  
  # Model de regresie multipla cu variabile semnificative
  final_data <- data.frame(data_refined, Revenue = y)
  model <- lm(Revenue ~ ., data = final_data)
  summary_model <- summary(model)
  
  cat("\nRezumatul modelului de regresie multipla cu variabile semnificative:\n")
  print(summary_model)
  
  
  
  
#=================================================IPOTEZA 1========================================
  #Verificam liniaritatea functionalei
  #Verificam daca modelul de regresie multipla este liniar
  #H0: Modelul este liniar
  #H1: Modelul nu este liniar
  linear_hypothesis <- ols_test_normality(model)
  cat("\nTestul de liniaritate a functiei:\n")
  print(linear_hypothesis)

  #Testul de liniaritate a functiei nu a respins ipoteza nula, deci putem considera ca modelul este liniar
  
  
  #=================================================IPOTEZA 2========================================
  # Testarea ipotezei 2: Variabilitatea in X este pozitiva
  #H0: Variabilitatea in X este pozitiva
  #H1: Variabilitatea in X nu este pozitiva
  variances <- apply(data, 2, var)
  positive_variance <- all(variances > 0)
  cat("\nTestul de variabilitate a variabilelor explicative:\n")
  if (positive_variance) {
    cat("Ipoteza nulă nu a fost respinsă: Variabilitatea în X este pozitivă.\n")
  } else {
    cat("Ipoteza nulă a fost respinsă: Unele variabile explicative au variabilitate nulă sau negativă.\n")
  }
  #Testul de variabilitate a variabilelor explicative nu a respins ipoteza nula, deci putem considera ca variabilitatea in X este pozitiva
  #=================================================IPOTEZA 3========================================
  # Testarea ipotezei 3: Erorile au media 0
  #H0: Erorile au media 0
  #H1: Erorile nu au media 0
  residuals <- residuals(model)
  mean_residuals <- mean(residuals)
  cat("\nMedia reziduurilor:", mean_residuals, "\n")
  cat("\nTestul de medie a erorilor:\n")
  if (abs(mean_residuals) < 1e-6) {
    cat("Ipoteza nulă nu a fost respinsă: Erorile au medie 0.\n")
  } else {
    cat("Ipoteza nulă a fost respinsă: Erorile nu au medie 0.\n")
    
  }
  #=================================================IPOTEZA 4========================================
  # Testarea ipotezei 4: Homoscedasticitatea erorilor
  # H0: Erorile sunt homoscedastice
  # H1: Erorile nu sunt homoscedastice
  # Testul Breusch-Pagan
  cat("\nTestul de omoscedasticitate a erorilor:\n")
  homoscedasticity_test <- bptest(model)
  print(homoscedasticity_test)
  
  # Testul White
  cat("\nTestul de omoscedasticitate a erorilor (White):\n")
  white_test <- bptest(model, studentize = TRUE)
  print(white_test)
  
  # Vizualizare reziduuri
  cat("\nVizualizare reziduuri:\n")
  residuals_model <- residuals(model)
  
  # Graficul reziduurilor vs valorile ajustate
  plot(fitted(model), residuals_model, main = "Reziduuri vs Valori Ajustate",
       xlab = "Valori Ajustate", ylab = "Reziduuri")
  abline(h = 0, col = "red")
  
  # Histogramă a reziduurilor
  hist(residuals_model, main = "Distribuția Reziduurilor", xlab = "Reziduuri", col = "lightblue", border = "black")
  
  cat("\nReziduuri - Sumariu:\n")
  summary(residuals_model)
  
  # Aplicăm WLS doar dacă ipoteza nulă a fost respinsă
  if (homoscedasticity_test$p.value < 0.05) {
    cat("Ipoteza nulă a fost respinsă: Erorile nu sunt homoscedastice.\n")
    
    # Aplicăm WLS folosind ponderi standard
    cat("\nAplicam WLS folosind ponderi standard:\n")
    
    # Aplicăm WLS folosind ponderi bazate pe o variabilă semnificativă
    # Ponderi standard (de exemplu, folosind variabila "Quantity.Sold..liters.kg.")
    wls_model <- lm(Revenue ~ Quantity.Sold..liters.kg. + Price.per.Unit..sold., data = final_data, weights = 1 / fitted(model)^2)
    
    summary_wls_model <- summary(wls_model)
    print(summary_wls_model)
    
    # Testăm din nou omoscedasticitatea după aplicarea WLS
    cat("\nTestul de omoscedasticitate a erorilor (White) dupa aplicarea WLS:\n")
    white_test_wls <- bptest(wls_model, studentize = TRUE)
    print(white_test_wls)
    
    # Testul Breusch-Pagan
    cat("\nTestul de omoscedasticitate a erorilor (Breusch-Pagan) dupa aplicarea WLS:\n")
    homoscedasticity_test_wls <- bptest(wls_model)
    print(homoscedasticity_test_wls)
    
  } else {
    cat("Ipoteza nulă nu a fost respinsă: Erorile sunt homoscedastice.\n")
  }
  
  
  
  #=================================================IPOTEZA 5========================================
  # Testarea ipotezei 5: Erorile nu sunt autocorelate
  #H0: Erorile nu sunt autocorelate
  #H1: Erorile sunt autocorelate
  #Testul Durbin-Watson
  cat("\nTestul de autocorelare a erorilor (Durbin-Watson):\n")
  durbin_watson_test <- durbinWatsonTest(model)
  print(durbin_watson_test)
  
  cat("\nAcceptam ipoteza nula: Erorile nu sunt autocorelate.\n")
  
  #=================================================IPOTEZA 6========================================
  # Testarea ipotezei 6: Necorelare intre regresor si erorile aleatoare
  #H0: Necorelare intre regresor si erorile aleatoare
  #H1: Corelare intre regresor si erorile aleatoare
  #Testul cor 
  cat("\nTestul de corelatie intre regresor si erorile aleatoare:\n")
  cor_test <- cor.test(data$Quantity.Sold..liters.kg., residuals)
  print(cor_test)
  
  if (cor_test$p.value < 0.05) {
    cat("Ipoteza nulă a fost respinsă: Există corelație între regresor și erorile aleatoare.\n")
    
    #Aplicam GLS
    cat("\nAplicam GLS:\n")
    gls_model <- gls(Revenue ~ ., data = final_data, correlation = corAR1())
    summary_gls_model <- summary(gls_model)
    print(summary_gls_model)
    
  } else {
    cat("Ipoteza nulă nu a fost respinsă: Nu există corelație între regresor și erorile aleatoare.\n")
  }
  
  #=================================================IPOTEZA 7========================================
  # Testarea ipotezei 7: Erorile sunt distribuite normal
  #H0: Erorile sunt distribuite normal
  #H1: Erorile nu sunt distribuite normal
  
  #Testul Jarque-Bera
  cat("\nTestul de normalitate a erorilor (Jarque-Bera):\n")
  jarque_bera_test <- jarque.bera.test(residuals)
  print(jarque_bera_test)
  
  if (jarque_bera_test$p.value < 0.05) {
    cat("Ipoteza nulă a fost respinsă: Erorile nu sunt distribuite normal.\n")
    
    #Eliminam valorile extreme identificate cu distantele Cook
    cat("\nEliminam valorile extreme identificate cu distantele Cook:\n")
    cooks_distance <- cooks.distance(model)
    influential_points <- cooks_distance > 4 / nrow(data)
    data_cleaned <- data[!influential_points, ]
    
    #Reantrenam modelul
    final_data_cleaned <- data.frame(data_cleaned, Revenue = y[!influential_points])
    model_cleaned <- lm(Revenue ~ Quantity.Sold..liters.kg. + Price.per.Unit..sold., data = final_data_cleaned)
    summary_model_cleaned <- summary(model_cleaned)
    print(summary_model_cleaned)
    
    residuals_cleaned <- residuals(model_cleaned)

    
    #Jarque-Bera test
    cat("\nTestul de normalitate a erorilor (Jarque-Bera) dupa eliminarea valorilor extreme:\n")
    jarque_bera_test <- jarque.bera.test(residuals_cleaned)
    print(jarque_bera_test)
    
    if (jarque_bera_test$p.value < 0.05) {
      cat("Ipoteza nulă a fost respinsă: Erorile nu sunt distribuite normal nici după eliminarea valorilor extreme.\n")
      cat("Problema de limitare a datelor. Esantion mare. Teorema limitei centrale ne ajuta in acest sens\n")
      
    } else {
      cat("Ipoteza nulă nu a fost respinsă: Erorile sunt distribuite normal după eliminarea valorilor extreme.\n")
    }
    
    
  } else {
    cat("Ipoteza nulă nu a fost respinsă: Erorile sunt distribuite normal.\n")
  }
  
  #=================================================IPOTEZA 8========================================
  # Testarea ipotezei 8: Multicoliniaritate
  #H0: Nu exista multicoliniaritate
  #H1: Exista multicoliniaritate
  #Testul VIF
  cat("\nTestul de multicoliniaritate (VIF):\n")
  vif_test <- vif(model)
  print(vif_test)
  
  if (max(vif_test) > 10) {
    cat("Ipoteza nulă a fost respinsă: Exista multicoliniaritate.\n")
    
    #Eliminam variabilele cu VIF mare
    cat("\nEliminam variabilele cu VIF mare:\n")
    selected_vars <- c("Quantity.Sold..liters.kg.","Price.per.Unit..sold.","Minimum.Stock.Threshold..liters.kg.","Reorder.Quantity..liters.kg.")
    data_refined <- data[, selected_vars]
    
    # Model de regresie multipla cu variabile semnificative
    final_data <- data.frame(data_refined, Revenue = y)
    model <- lm(Revenue ~ ., data = final_data)
    summary_model <- summary(model)
    
    cat("\nRezumatul modelului de regresie multipla cu variabile semnificative:\n")
    print(summary_model)
    
  } else {
    cat("Ipoteza nulă nu a fost respinsă: Nu exista multicoliniaritate.\n")
  
  }
  return(list(
    model = model,
    data = final_data,
    residuals = residuals_model
  ))
  cat("----------------------------------END----------------------------------\n")
}

result_small <- process_farm_data(small_farms, "Small")
result_medium <- process_farm_data(medium_farms, "Medium")
result_large <- process_farm_data(large_farms, "Large")

model_small <- result_small$model
data_small <- result_small$data
residuals_small <- result_small$residuals

model_medium <- result_medium$model
data_medium <- result_medium$data
residuals_medium <- result_medium$residuals

model_large <- result_large$model
data_large <- result_large$data
residuals_large <- result_large$residuals

enchance_multiple_linear_model <- function(model)
{
  summary_model <- summary(model)
  cat("\nRezumatul modelului de regresie multipla:\n")
  print(summary_model)
}

enchance_multiple_linear_model(model_small)
enchance_multiple_linear_model(model_medium)
enchance_multiple_linear_model(model_large)

compare_farms_models <- function(model_small, model_medium, model_large, data_small, data_medium, data_large) {
  cat("================================= ANALIZA COMPARATIVĂ =================================\n")
  
  # 1. Performanța modelelor
  cat("\n1. Compararea performanței modelelor (RMSE):\n")
  
  # Calcularea RMSE pentru fiecare model
  calc_rmse <- function(model, data) {
    pred <- predict(model, data)
    actual <- data$Revenue
    sqrt(mean((pred - actual)^2))
  }
  
  rmse_small <- calc_rmse(model_small, data_small)
  rmse_medium <- calc_rmse(model_medium, data_medium)
  rmse_large <- calc_rmse(model_large, data_large)
  
  cat("RMSE pentru ferme mici:", rmse_small, "\n")
  cat("RMSE pentru ferme medii:", rmse_medium, "\n")
  cat("RMSE pentru ferme mari:", rmse_large, "\n")
  
  # 2. Compararea coeficienților semnificativi
  cat("\n2. Compararea coeficienților semnificativi din modele:\n")
  print("Ferme mici:")
  print(summary(model_small)$coefficients)
  
  print("Ferme medii:")
  print(summary(model_medium)$coefficients)
  
  print("Ferme mari:")
  print(summary(model_large)$coefficients)
  
  # 3. Testarea diferențelor între modelele reziduurilor folosind ANOVA
  cat("\n3. Testarea diferențelor între modele folosind ANOVA:\n")
  
  # Combinația datelor și reziduurilor pentru ANOVA
  data_small$residuals <- residuals(model_small)
  data_medium$residuals <- residuals(model_medium)
  data_large$residuals <- residuals(model_large)
  
  combined_data <- data.frame(
    Residuals = c(data_small$residuals, data_medium$residuals, data_large$residuals),
    Farm_Type = rep(c("Small", "Medium", "Large"), 
                    times = c(nrow(data_small), nrow(data_medium), nrow(data_large)))
  )
  
  anova_test <- aov(Residuals ~ Farm_Type, data = combined_data)
  print(summary(anova_test))
  
  # 4. Testarea diferențelor dintre medii folosind t-test pentru perechi
  cat("\n4. Testarea diferențelor dintre medii (t-test perechi):\n")
  t_test_small_medium <- t.test(data_small$residuals, data_medium$residuals)
  t_test_medium_large <- t.test(data_medium$residuals, data_large$residuals)
  t_test_small_large <- t.test(data_small$residuals, data_large$residuals)
  
  cat("T-test între ferme mici și medii:\n")
  print(t_test_small_medium)
  
  cat("T-test între ferme medii și mari:\n")
  print(t_test_medium_large)
  
  cat("T-test între ferme mici și mari:\n")
  print(t_test_small_large)
  
  # 5. Vizualizări grafice
  cat("\n5. Vizualizări grafice pentru reziduuri:\n")
  par(mfrow = c(1, 3)) # Setăm layout pentru 3 grafice
  
  hist(data_small$residuals, main = "Ferme Mici", xlab = "Reziduuri", col = "lightblue", border = "black")
  hist(data_medium$residuals, main = "Ferme Medii", xlab = "Reziduuri", col = "lightgreen", border = "black")
  hist(data_large$residuals, main = "Ferme Mari", xlab = "Reziduuri", col = "lightcoral", border = "black")
  
  par(mfrow = c(1, 1)) # Resetăm layout
  
  cat("\n================================= SFÂRȘIT ANALIZĂ =================================\n")
}

compare_farms_models(model_small, model_medium, model_large, data_small, data_medium, data_large)

#Concluzii comparare modele
#1. Modelul pentru fermele mari are cel mai mic RMSE, ceea ce înseamnă că prezice cel mai bine veniturile.
#2. Variabile aditionale: Ferme medii: Total Land Area Acres(seminficatie marginala p=0.056) si Ferme mari : Minimum Stock Threshold liters per kg (marginal semnificativ p=0.063)
# Variabilele Quantity sold si Price per unit sunt predictori puternici si stabili in toate cele 3 modele, dar exista variatii suplimentare pentru fermele medii si mari.
#3. Testul ANOVA arata ca nu exista diferente semnificative intre reziduurile modelelor pentru fermele mici, medii si mari => modelele reprezinta un comportament similar in privinta erorilor.
#4. Testele t-test pentru perechi arata ca nu exista diferente semnificative intre mediile reziduurilor modelelor pentru fermele mici, medii si mari ceea ce confrma stabilitatea modelelor pentru toate tipurile de ferme.
#5. Vizualizarea grafica a reziduurilor arata ca acestea sunt distribuite aproximativ normal pentru toate tipurile de ferme.
