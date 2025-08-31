# Statistical Applications & Analysis

This repository is a curated collection of **statistical projects** that demonstrate how theoretical concepts in **statistics and data science** can be applied to real-world datasets for meaningful insights.

The focus is on **statistical thinking** and **analytical methods** rather than on any single programming language. Projects may be implemented in **R, Python, or other tools** as the collection grows.

## Projects

### 1. Retail Demand Forecasting & Inventory Optimization

- Modeled weekly product demand using **time series forecasting** (ARIMA).
- Calculated **optimal reorder quantities** to balance stockouts and overstocking.
- Dataset: ~49,000 simulated e-commerce transactions.
- Applications: inventory planning, purchasing decisions, warehouse optimization.

### 2. Sentiment Analysis of Product Reviews

- Preprocessed and tokenized product reviews.
- Applied **lexicon-based sentiment scoring** (AFINN).
- Classified reviews as **Positive, Negative, or Neutral**.
- Extracted insights on **customer satisfaction and opinion trends**.

### 3. Sales Data Analysis

- Cleaned and transformed e-commerce sales data.
- Conducted **descriptive statistics** and correlation studies (Pearson, Spearman).
- Analyzed the effect of **discount strategies** on **customer ratings**.

### 4. Exploratory Data Analysis on Iris Dataset

- Performed **summary statistics** on the classic Iris dataset.
- Implemented a custom function to calculate the **mode** for numeric columns.
- Provided **interactive visualizations** (histogram, boxplot, scatterplot) based on user input.
- Applications: basic EDA demonstration, understanding dataset exploration workflow.
- Generated **automated R markdown EDA Reports.**

### 5. Probability Distribution Simulation (Poisson & Normal)

- Simulated **Poisson** and **Normal** random variables.
- Compared simulated **mean, variance, and standard deviation** with theoretical values.
- Visualized distributions with **histograms** and **boxplots**.
- Applications: demonstrating how simulated data approximates theoretical distributions.
- Implemented a general **CLT Simulator.**
- Compared simulated sampling distributions against theoretical normal curves.

## Datasets

### 1. Retail Demand Forecasting & Inventory Optimization

- https://www.kaggle.com/datasets/yusufdelikkaya/online-sales-dataset

### 2. Sentiment Analysis of Product Reviews

- https://www.kaggle.com/datasets/arhamrumi/amazon-product-reviews

### 3. Sales Data Anlysis

### 4.Iris Dataset (EDA)

- Built-in **iris** dataset from R (no external download required).

### 5. Probability Distribution Simulation

- Simulated data generated using R’s random sampling functions (`rpois`, `rnorm`).

### 6. Theory of Estimation – Sampling & Density Visualization

- Generated a **Normal distribution** with user-defined sample size.
- Drew **random samples** from the population and computed their **sample mean** and **standard deviation**.
- Compared population statistics with sample statistics to demonstrate the **concept of estimation**.
- Visualized **population vs. sample density plots** (blue = full distribution, red = sample distribution).
- Applications: illustrating **estimator properties** (bias, variance, consistency) in a hands-on way.

## Statistical Concepts Applied

- **Descriptive statistics** (summary measures, distributions)
- **Correlation analysis** (Pearson, Spearman)
- **Text mining & sentiment analysis**
- **Time series analysis** (ARIMA forecasting)
- **Optimization in inventory control**
- **Exploratory data analysis (EDA)**
- **Probability distributions & simulation** (Poisson, Normal)
- **Estimation Theory** (Sample and Population Statistics, bias & variance)
- **Central Limit Theorem.**

## Technologies Used

- **R / Python** (depending on project folder)
- **tidyverse (dplyr, ggplot2, tidyr)** for data wrangling & visualization
- **tidytext** for text mining
- **forecast** for time series modeling
- Standard Python libraries will be added in future projects (`pandas`, `scikit-learn`, etc.)

## How to Use

1. Clone the repository:
   ```bash
   git clone https://github.com/Rithesh077/statistics_projects
   cd statistics_projects
   ```
2. Naviagate to the desired Folder directory.
3. Open the R or Python Script.
4. Run the code to produce the desired results.
5. Outputs are saved in output/figures or output/tables directories.

## Future Enhancements

- **Add Python Based Projects**: Extend repository with statistical applications implemented in Python for broader coverage.
- **Machine Learning Integration**: Incorporate predictive models (Random Forest, XGBoost) to complement classical statistical analysis.
- **Advanced Forecasting**: Apply multivariate time-series approaches (VAR, Prophet) to capture seasonality and external regressors.
- **Enhanced Text Analytics**: Support multiple sentiment lexicons (AFINN, NRC, Bing) for richer sentiment classification.
- **Interactive Dashboards**: Build Shiny apps or Plotly dashboards for dynamic data visualization and exploration.
- **Automated Data Pipelines**: Implement ETL workflows to integrate real-world streaming data from APIs or databases.
- **Reusable Templates**: Create modular project templates so this repo can serve as a public learning framework for applied statistics.
- **IRIS EDA extensions**:
  - Add **species-based visualizations** (scatterplots colored by species).
  - Incorporate **principal component analysis (PCA)** for dimensionality reduction.
- **Probability Simulation Extensions**:
  - Convergence checks, goodness-of-fit tests, Law of Large Numbers demos.
