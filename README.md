## **README.md**

# 📊 Econometrie

**Econometrie** is a repository that focuses on statistical modeling, hypothesis testing, and regression analysis using R programming. The primary goal is to analyze and compare datasets (e.g., farm data) using econometric models such as **Lasso Regression**, **Ridge Regression**, **Elastic Net**, and traditional multiple linear regression. The project also evaluates model assumptions using statistical tests.

---

## 🚀 **Features**

- **Data Cleaning and Preprocessing**:
  - Handles missing values and prepares numerical data for modeling.
  
- **Regression Models**:
  - **Lasso Regression** for feature selection.
  - **Ridge Regression** for regularization.
  - **Elastic Net** for combining L1 and L2 penalties.
  - Multiple Linear Regression for interpretable models.

- **Model Evaluation**:
  - Calculates Root Mean Square Error (**RMSE**) for model performance comparison.
  - Compares models across different subsets of data (e.g., small, medium, and large farms).

- **Statistical Testing**:
  - Tests for multicollinearity using **Variance Inflation Factor (VIF)**.
  - Checks for:
    - Homoscedasticity (**Breusch-Pagan Test**, **White Test**).
    - Linearity.
    - Normality of residuals (**Jarque-Bera Test**).
    - Autocorrelation of residuals (**Durbin-Watson Test**).
  - Applies **Weighted Least Squares (WLS)** and **Generalized Least Squares (GLS)** for heteroscedasticity correction.

- **Model Comparison**:
  - Compares performance between models for small, medium, and large datasets using:
    - **ANOVA** (analysis of variance).
    - **t-tests** for residual comparisons.

---

## 🔧 **Requirements**

- R (>= 4.0.0)
- Libraries:
  - `glmnet`
  - `caret`
  - `stats`
  - `sandwich`
  - `MASS`
  - `lmtest`
  - `tseries`
  - `olsrr`
  - `car`
  - `nlme`

To install the required libraries in R:

```R
install.packages(c("glmnet", "caret", "stats", "sandwich", "MASS", 
                   "lmtest", "tseries", "olsrr", "car", "nlme"))
```

---

## 📂 **Project Structure**

```
Econometrie/
│
├── Data/
│   └── Cleaned_Dairy_Dataset.csv      # Cleaned dataset for analysis
│
├── Scripts/
│   ├── process_farm_data.R            # Main script for model building and testing
│   └── compare_farm_models.R          # Comparison of farm models (ANOVA, t-tests, RMSE)
│
├── Results/
│   └── output_summary.txt             # Results from running the scripts
│
└── README.md                          # Project documentation
```

---

## 🔨 **How to Run**

1. **Clone the Repository**:
   ```bash
   git clone https://github.com/andrei2308/Econometrie.git
   cd Econometrie
   ```

2. **Prepare the Data**:
   - Ensure the cleaned dataset (`Cleaned_Dairy_Dataset.csv`) is in the `Data/` folder.

3. **Run the Main Script**:
   - Open R or RStudio and source the script:
     ```R
     source("Scripts/process_farm_data.R")
     ```
   - The script processes subsets of data (small, medium, large farms), builds models, evaluates assumptions, and outputs key results.

4. **Run the Model Comparison**:
   - After running the `process_farm_data.R`, use:
     ```R
     source("Scripts/compare_farm_models.R")
     ```

5. **Analyze Results**:
   - Outputs such as RMSE, residual tests, and model coefficients will be printed in the console or saved to the `Results/` folder.

---

## 📈 **Example Output**

### RMSE for Models
| Farm Type      | RMSE       |
|----------------|------------|
| Small Farms    | 5649.805   |
| Medium Farms   | 5681.074   |
| Large Farms    | 5611.518   |

### Statistical Tests
- **Homoscedasticity**: Breusch-Pagan Test, White Test.
- **Multicollinearity**: VIF values.
- **Normality of Residuals**: Jarque-Bera Test.

---

## 📝 **Notes**
- Ensure the dataset is preprocessed before running the scripts.
- The statistical tests assume standard regression assumptions:
  - Linearity
  - Independence of errors
  - Homoscedasticity
  - Normality of residuals

---

## 📧 **Contact**
For questions or feedback, feel free to reach out:

- **Author**: Andrei  
- **Email**: [chitoiu.andrei2@yahoo.com]  
- **GitHub**: [andrei2308](https://github.com/andrei2308)

---

## 📜 **License**
This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

---

Let me know if you need any adjustments or additions to the README! 😊
