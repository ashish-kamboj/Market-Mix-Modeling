# Market Mix Modeling (ElecKart)

**Market mix modeling for an e‑commerce firm (electronic products)** — this project shows an end‑to‑end approach to estimate the impact of marketing levers (TV, Digital, SEM, Affiliates, etc.), pricing and promotions on weekly GMV (revenue) and to recommend budget allocation.

---

## Quick summary
- Problem: quantify the contribution of different marketing channels and pricing levers on sales (GMV) for an online electronics retailer and recommend budget allocation.
- Data period: July 2015 to June 2016 (weekly aggregation in the analysis).
- Main deliverables (in code): data preparation and EDA, feature engineering (adstock, lags, moving averages), multiple model families (linear, multiplicative, Koyck, distributed lag, multiplicative+distributed lag), elasticity estimates and plots, 5‑fold cross validation.

---

## Repository structure (top-level)
```
README.md
Data_Preparation_and_EDA.R        # data cleaning, monthly->weekly conversion, initial EDA
Data_Preparation_and_EDA.md      # notebook-style rendered output for the above
Data_Preparation_and_EDA_files/  # supporting images for the .md
Feature_Engineering_and_EDA.R    # feature engineering: KPIs, adstock, lags, moving averages, EDA
Feature_Engineering_and_EDA.md   # notebook-style rendered output for the above
Feature_Engineering_and_EDA_files/
ModelBuilding_CameraAccessory.R  # model building & evaluation for CameraAccessory subcategory
ModelBuilding_GamingAccessory.R  # model building & evaluation for GamingAccessory subcategory
ModelBuilding_HomeAudio.R        # model building & evaluation for HomeAudio subcategory
```

How it fits together:
- Data_Preparation_and_EDA.R
  - Ingests raw product sales (`ConsumerElectronics.csv`) and media data (`Media data and other information.xlsx`).
  - Cleans transaction-level data, prorates monthly media spends to weekly and merges media/NPS/holiday information with orders.
  - Produces initial exploratory plots (monthly & weekly GMV/units, payment-type breakdowns, weekly media spends).
- Feature_Engineering_and_EDA.R
  - Builds engineered KPIs: `list_price`, `promotional_offer`, `payment_ind`, prepaid order % and `price_tag` clustering.
  - Computes adstocked media variables (adstock rate = 0.5), moving averages, and creates 1–3 week lag / incremental lag features for price and GMV.
  - Aggregates everything to weekly level and produces response-curve plots for GMV vs independent variables.
- ModelBuilding_*.R (CameraAccessory / GamingAccessory / HomeAudio)
  - Take the weekly engineered datasets and run multiple model families (Linear, Multiplicative, Koyck, Distributed Lag, Multiplicative+Distributed Lag).
  - Use stepwise selection and manual pruning guided by p-values and VIF (car::vif) to reach final models.
  - Run 5‑fold cross-validation (DAAG::cv.lm), compute elasticity estimates from final models, and generate diagnostic & elasticity plots.
- Outputs and artifacts
  - Console outputs: model summaries, VIF tables, cross-validation prints.
  - Visuals: ggplot diagnostics and elasticity bar charts (displayed interactively; .md files contain earlier rendered outputs).

---

## Data required
Put these files in the repository root (or update file paths in the scripts):
- ConsumerElectronics.csv
- Media data and other information.xlsx

The R scripts expect those filenames exactly (see Data_Preparation_and_EDA.R lines that read them).

---

## Reproducible steps (shortest path)
1. Open R (or RStudio) in the project root.
2. Install required CRAN packages (examples below):

```r
install.packages(c(
  "readxl", "lubridate", "ggplot2", "scales", "ggthemes", "cowplot",
  "dplyr", "zoo", "DataCombine", "imputeTS", "MASS", "car", "DAAG", "knitr"
))
```

3. Source the scripts in order (they print summaries and draw plots):

```r
source("Data_Preparation_and_EDA.R")
source("Feature_Engineering_and_EDA.R")
# then run model building for subcategories you need
source("ModelBuilding_CameraAccessory.R")
source("ModelBuilding_GamingAccessory.R")
source("ModelBuilding_HomeAudio.R")
```

Notes:
- The scripts create many ggplot graphs interactively. To save plots, redirect the plotting device or modify the scripts to call ggsave() where required.
- If you want a single notebook-style output, open the supplied .md files (Data_Preparation_and_EDA.md and Feature_Engineering_and_EDA.md) which contain rendered outputs of earlier runs.

---

## Important analysis & modeling choices (documented in code)
- Weekly aggregation: monthly media spends are prorated into weekly amounts and merged with transaction-level data which is aggregated by week.
- Adstock: the adstock rate used in feature engineering is 0.50 (see Feature_Engineering_and_EDA.R). Adstock is computed via a recursive filter over weekly media spends.
- Transformations:
  - Multiplicative models use log transformations; zeroes are replaced with a small value (0.00001) before log.
  - For multiplicative + distributed lag models, negative incremental lag values are shifted to be positive before log.
- Lag features: list_price, promotional_offer and gmv are lagged (1–3 weeks) and incremental change features are created.
- Model selection: stepwise regression (both directions) is used to shortlist predictors, then iterative manual variable removal guided by p-values and VIF (car::vif) is applied.
- Cross‑validation: cv.lm from DAAG is used with m=5 and seed=29 for model stability checks.
- Elasticities: estimated from model coefficients using the standard midpoint formula implemented in each ModelBuilding_*.R script; elasticities are plotted as bar charts (positive = green, negative = red).

---

## What the model scripts output
- Model summaries (R-squared, coefficients, p-values) printed to console via summary().
- Variance Inflation Factors (vif) printed using car::vif().
- 5‑fold cross validation results printed and optionally plotted (cv.lm).
- Elasticity tables and elasticity bar charts for each final model.
- Many diagnostic ggplots (response curves, residual checks, elasticity plots). By default these are displayed in the R plotting window — edit the scripts to save to files.

---

## Suggested next steps / improvements
- Add an RMarkdown (.Rmd) that sources the scripts and produces a single reproducible report with figures (knitr / rmarkdown). The repo already contains .md files from previous runs that can be used as a base.
- Add automated saving of model artifacts (coefficients, elasticity tables, plots) as CSV/PNG so results are reproducible without an interactive session.
- Parameterize adstock rate(s) and implement grid search or optimization to estimate adstock decay parameters instead of fixing 0.5.
- Add model comparison metrics (RMSE, MAE) saved per fold to better compare candidate models.
- Add a license to clarify reuse (e.g. MIT) if you want to make this openly reusable.

---

## Dependencies (packages used across scripts)
- readxl, lubridate, ggplot2, scales, ggthemes, cowplot, dplyr, zoo, DataCombine, imputeTS, MASS, car, DAAG, knitr

---

## Attribution / Contact
Author: ashish-kamboj (original repository)
If you have improvements (automation, tests, RMarkdown report), feel free to open a PR.

---

## License
No license is specified in this repository. If you want to allow reuse, add an explicit license file (e.g., LICENSE with MIT or BSD).
