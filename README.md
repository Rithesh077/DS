# Statistical Applications & Analysis

This repository is a curated collection of **statistical projects** that demonstrate how theoretical concepts in **statistics and data science** can be applied to real-world problems for meaningful insights.

The focus is on **statistical thinking** and **analytical methods** rather than on any single programming language. Projects are implemented primarily in **R**, with plans to extend to **Python**.

## Projects

### 1. Retail Demand Forecasting & Inventory Optimization

- Modeled weekly product demand using **time series forecasting (ARIMA)**.
- Optimized reorder quantities to balance stockouts and overstocking.
- **Dataset**: ~49,000 simulated e-commerce transactions.
- **Applications**: inventory planning, purchasing, warehouse optimization.

### 2. Sentiment Analysis of Product Reviews

- Preprocessed and tokenized customer reviews.
- Applied **lexicon-based sentiment scoring (AFINN)**.
- Classified reviews as **Positive, Negative, or Neutral**.
- **Applications**: understanding customer satisfaction & opinion trends.

### 3. Sales Data Analysis

- Cleaned and transformed e-commerce sales data.
- Conducted **descriptive statistics** and correlation studies (Pearson, Spearman).
- Explored the effect of **discount strategies** on **customer ratings**.
- **Applications**: evaluating pricing and promotion strategies.

### 4. EDA on Iris Dataset

- Performed **summary statistics** on the classic Iris dataset.
- Implemented a custom **mode function** for numeric columns.
- Generated **interactive visualizations** (histogram, boxplot, scatterplot).
- Added **automated R Markdown EDA reports**.
- **Applications**: demonstrating exploratory data analysis workflow.

### 5. Probability Distribution Simulation

- Simulated data from **Poisson, Normal, Binomial, Chi-squared, Gamma, and Exponential** distributions.
- Compared simulated vs. theoretical **mean, variance, and standard deviation**.
- Implemented a **Central Limit Theorem (CLT) simulator**.
- Visualized convergence via **Law of Large Numbers (LLN)** and **goodness-of-fit tests**.
- **Applications**: illustrating the connection between theoretical distributions and real simulations.

### 6. Theory of Estimation – Sampling & Density Visualization

- Generated a **Normal distribution** with user-defined sample sizes.
- Drew **random samples** and computed sample mean & standard deviation.
- Compared population vs. sample statistics to show estimation concepts.
- Visualized **population vs. sample density plots** (blue = full distribution, red = sample).
- **Applications**: demonstrating **bias, variance, and consistency** of estimators.

## Datasets

- **Retail Demand Forecasting**: [Online Sales Dataset (Kaggle)](https://www.kaggle.com/datasets/yusufdelikkaya/online-sales-dataset)
- **Sentiment Analysis**: [Amazon Product Reviews (Kaggle)](https://www.kaggle.com/datasets/arhamrumi/amazon-product-reviews)
- **Sales Data Analysis**: Provided in repo.
- **Iris Dataset (EDA)**: Built-in R dataset.
- **Probability Simulation**: Simulated using R random generators (`rpois`, `rnorm`, `rbinom`, etc.).
- **Theory of Estimation**: Simulated using `rnorm`.

## Statistical Concepts Applied

- **Descriptive statistics** (mean, variance, mode, distributions)
- **Correlation analysis** (Pearson, Spearman)
- **Text mining & sentiment analysis**
- **Time series forecasting (ARIMA)**
- **Optimization in inventory control**
- **Exploratory Data Analysis (EDA)**
- **Probability distributions & simulation** (Poisson, Normal, etc.)
- **Estimation theory** (sample vs. population, bias, variance)
- **Central Limit Theorem (CLT)**
- **Law of Large Numbers (LLN)**
- **Goodness-of-fit tests**

## Technologies Used

- **R** (primary) | Future expansion to **Python**
- **tidyverse (dplyr, ggplot2, tidyr)** for wrangling & visualization
- **tidytext** for text mining
- **forecast** for time series
- **ggplot2** for data visualization
- Planned: `pandas`, `scikit-learn`, etc. for Python

## How to Use

1. Clone the repository:
   ```bash
   git clone https://github.com/Rithesh077/statistics_projects
   cd statistics_projects
   ```
2. Navigate To the desired project folder.
3. Open the desired scripts.
4. Run the script.
5. The output files are saved in outputs/figures or outputs/tables.

## Future Enhancements

- **Python Projects**: Extend repository with Python implementations.
- **Machine Learning Integration**: Random Forest, XGBoost, etc.
- **Advanced Forecasting**: VAR, Prophet with seasonality & regressors.
- **Enhanced Text Analytics**: Use multiple sentiment lexicons (AFINN, NRC, Bing).
- **Interactive Dashboards**: Build with Shiny / Plotly.
- **Automated Data Pipelines**: Stream data from APIs/databases.
- **Reusable Templates**: Create modular templates for applied statistics.
- **Dynamic User Inputs**: Allow interactive selection of distribution type, sample size, parameters, etc.
