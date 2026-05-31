# Heart Analysis and Prediction System
### Framingham Heart Study Analytics

A professional R Shiny application designed to analyze cardiovascular health data, identify high-risk patient clusters, and predict 10-year Coronary Heart Disease (CHD) risk using validated statistical methods.

## 🚀 Key Features
- **Health Dashboard:** Real-time summary statistics of the Framingham dataset.
- **Interactive Visualization:** Dynamic charts to explore relationships between age, blood pressure, and cardiac health.
- **Advanced Risk Prediction:** A Logistic Regression tool that predicts personal risk based on 8 clinical indicators.
- **Cluster Analysis:** Utilizes K-Means clustering to identify patient groups with similar health profiles.

## 🔬 Statistical Methodology
This application implements advanced data science techniques to ensure medical and analytical accuracy:
- **Standardized Coefficients:** Risk factors are ranked by "standardized beta weights," ensuring continuous variables (like Age) are compared fairly against binary ones (like Hypertension).
- **Outlier Management:** Automatically filters data points above the 99th percentile for blood pressure, cholesterol, and glucose to prevent model skew.
- **Class Imbalance Correction:** Uses case-weighting during model training to account for the minority of patients who experience CHD events.
- **Validation:** Utilizes an **80/20 Train-Test split** to provide honest performance metrics (Accuracy, Sensitivity, and Specificity) on unseen data.

## 📊 Clinical Indicators Used
The predictive model evaluates the following factors:
1. **Age** (Biological seniority)
2. **Gender** (Biological risk variations)
3. **Systolic BP** (Vascular pressure)
4. **Total Cholesterol** (Lipid profile)
5. **Blood Glucose** (Metabolic health)
6. **Cigarettes Per Day** (Smoking intensity)
7. **Prevalent Stroke** (Clinical history)
8. **Prevalent Hypertension** (Chronic condition)

## 🛠️ How to Run

### In RStudio (Recommended)
1. Open **RStudio**.
2. Go to `File` > `Open File...` and select `app.R`.
3. Locate the **Run App** button (with a blue play icon) at the top right of the script editor.
4. Ensure your working directory is set correctly via `Session` > `Set Working Directory` > `To Source File Location`.

### Via R Console
Alternatively, copy and paste this command into your R console to run the app directly:
```r
shiny::runApp("healthInside")
```

## 📦 Dependencies
Ensure the following libraries are installed before running:
```r
install.packages(c("shiny", "ggplot2", "dplyr", "DT", "corrplot"))
```


## 📝 Data Source
The application uses the **Framingham Heart Study** dataset, a landmark study that has provided primary insights into the epidemiology of cardiovascular disease.
