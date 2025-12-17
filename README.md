# Developing machine learning models of self-reported and register-based data to predict eating disorders in adolescence

## Overview

This repository contains the code and resources for predicting eating disorders among adolescents using both register-based and self-reported data. The study encompasses data from over 40,000 adolescents, aiming to develop robust diagnostic and prognostic models.

## Publication

**Katsiferis A**, Joensen A, Petersen LV, Ekstrøm CT, Olsen EM, Bhatt S, Nguyen TL, Strandberg Larsen K. *Developing machine learning models of self-reported and register-based data to predict eating disorders in adolescence.* **npj Mental Health Research**. 2025;4:65.

[![DOI](https://img.shields.io/badge/DOI-10.1038%2Fs44184--025--00179--x-blue)](https://doi.org/10.1038/s44184-025-00179-x)
[![Open Access](https://img.shields.io/badge/Open%20Access-green)](https://rdcu.be/eUU8Z)

### Key Findings

- **Diagnostic model** (identifying EDs by DNBC-11): AUC = 81.3 [95% CI: 78.0, 84.6]
- **Prognostic model** (predicting EDs by DNBC-18): AUC = 76.9 [95% CI: 74.3, 79.5]
- A simplified 10-predictor logistic regression achieved comparable performance to the full ML model
- Top predictors: sex, emotional symptoms, body dissatisfaction, peer problems, stress, conduct problems, parental BMI, and childhood BMI

### Data Source

The Danish National Birth Cohort (DNBC) following 96,822 children from before birth through young adulthood.

| Metric | Value |
|--------|-------|
| Diagnostic sample | 44,357 participants |
| Prognostic sample | 26,127 participants |
| Predictors evaluated | ~100 |
| Follow-up period | 18 years |

## Interactive Risk Calculator

**Try the online risk calculator:** [https://alkat19.github.io/ED_Pred/](https://alkat19.github.io/ED_Pred/)

This interactive tool implements a simplified logistic regression model using the top 10 predictors from our study, allowing users to estimate eating disorder risk based on factors measured around age 11.

> **Disclaimer:** This calculator is for educational and research purposes only. It is NOT a diagnostic tool and should NOT replace professional clinical assessment. The model was developed using Danish data and may not generalize to other populations.

## Repository Contents

| File | Description |
|------|-------------|
| `Pre_Processing.R` | Data cleaning, transformation, and preparation |
| `Modelling_Diagnostic_Main.R` | Primary diagnostic model development |
| `Modelling_Diagnostic_Secondary.R` | Secondary (extended) diagnostic models |
| `Modelling_Prognostic_Main.R` | Primary prognostic model development |
| `Modelling_Prognostic_Secondary.R` | Secondary (extended) prognostic models |
| `Figures.R` | Generation of visualizations and plots |
| `Tables.R` | Summary tables and descriptive statistics |
| `Risk_Calculator_App.R` | Source code for the interactive Shiny calculator |
| `docs/` | Shinylive deployment files for the web-based calculator |

## Citation

If you use this code or the risk calculator, please cite:

```bibtex
@article{katsiferis2025eating,
  title={Developing machine learning models of self-reported and register-based data to predict eating disorders in adolescence},
  author={Katsiferis, Alexandros and Joensen, Andrea and Petersen, Liselotte Vogdrup and Ekstr{\o}m, Claus Thorn and Olsen, Else Marie and Bhatt, Samir and Nguyen, Tri-Long and Strandberg Larsen, Katrine},
  journal={npj Mental Health Research},
  volume={4},
  pages={65},
  year={2025},
  publisher={Nature Publishing Group}
}
```

## Contact

For questions about the research or code: [alexandros.katsiferis@sund.ku.dk](mailto:alexandros.katsiferis@sund.ku.dk)

## License

Please refer to the publication for data availability information.
