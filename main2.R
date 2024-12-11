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
  # Selectarea variabilelor numerice
  numerical_columns <- c("Total.Land.Area..acres.","Number.of.Cows","Quantity..liters.kg.","Price.per.Unit","Quantity.Sold..liters.kg.",
                         "Price.per.Unit..sold.","Approx..Total.Revenue.INR.","Quantity.in.Stock..liters.kg.","Minimum.Stock.Threshold..liters.kg.","Reorder.Quantity..liters.kg.")
  
  data <- farm_data[, numerical_columns]
  
  # Eliminarea valorilor lipsa
  data <- na.omit(data)
  
  # Separarea in variabile predictori si tinta
  X <- as.matrix(data[, !colnames(data) %in% "Approx..Total.Revenue.INR."])
  y <- as.vector(data$Approx..Total.Revenue.INR.)
  
  # Standardizarea datelor
  X_scaled <- scale(X)
  
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
  #observam ca modelul s-a imbunatatit in urma eliminarii variabilelor nesemnificative
  
  # Model de regresie multipla folosind variabilele semnificative
  final_data <- data.frame(data_significant, Revenue = y)
  model <- lm(Revenue ~ ., data = final_data)
  summary_model <- summary(model)
  
  cat("\nRezumatul modelului de regresie multipla:\n")
  print(summary_model)
  
  #eliminam variabilele nesemnificative statistic din modelul de regresie multipla
  selected_vars <- c("Quantity.Sold..liters.kg.","Price.per.Unit..sold.","Minimum.Stock.Threshold..liters.kg.","Reorder.Quantity..liters.kg.")
  data_refined <- data[, selected_vars]
  
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
  linear_hypothesis <- linearHypothesis(model, c("Quantity.Sold..liters.kg.","Price.per.Unit..sold.","Minimum.Stock.Threshold..liters.kg.","Reorder.Quantity..liters.kg."))
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
  #H0: Erorile sunt homoscedastice
  #H1: Erorile nu sunt homoscedastice
  #Testul Breusch-Pagan
  cat("\nTestul de omoscedasticitate a erorilor:\n")
  homoscedasticity_test <- bptest(model)
  print(homoscedasticity_test)
  
  #Testul White
  cat("\nTestul de omoscedasticitate a erorilor (White):\n")
  white_test <- bptest(model, studentize = TRUE)
  print(white_test)
  
  if (homoscedasticity_test$p.value < 0.05) {
    cat("Ipoteza nulă a fost respinsă: Erorile nu sunt homoscedastice.\n")
    
    #Aplicam WLS
    cat("\nAplicam WLS:\n")
    wls_model <- lm(Revenue ~ Quantity.Sold..liters.kg. + Price.per.Unit..sold. + Minimum.Stock.Threshold..liters.kg. + Reorder.Quantity..liters.kg., data = final_data, weights = 1 / fitted(model)^2)
    summary_wls_model <- summary(wls_model)
    print(summary_wls_model)
    
    #Testam din nou homoscedasticitatea
    cat("\nTestul de omoscedasticitate a erorilor (White) dupa aplicarea WLS:\n")
    white_test_wls <- bptest(wls_model, studentize = TRUE)
    print(white_test_wls)
    
    #Testul Breusch-Pagan
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
    gls_model <- gls(Revenue ~ Quantity.Sold..liters.kg. + Price.per.Unit..sold. + Minimum.Stock.Threshold..liters.kg. + Reorder.Quantity..liters.kg., data = final_data, correlation = corAR1())
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
    model_cleaned <- lm(Revenue ~ Quantity.Sold..liters.kg. + Price.per.Unit..sold. + Minimum.Stock.Threshold..liters.kg. + Reorder.Quantity..liters.kg., data = final_data_cleaned)
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
  return (model)
}

model_small <- process_farm_data(small_farms, "Small")
model_medium <- process_farm_data(medium_farms, "Medium")
model_large <- process_farm_data(large_farms, "Large")

enchance_multiple_linear_model <- function(model)
{
  summary_model <- summary(model)
  cat("\nRezumatul modelului de regresie multipla:\n")
  print(summary_model)
}

enchance_multiple_linear_model(model_small)
enchance_multiple_linear_model(model_medium)
enchance_multiple_linear_model(model_large)






