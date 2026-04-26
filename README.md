# Computerized Adaptive Test for Kenya CBC Junior Secondary Pathway Selection

## Authors
[Apiwan Duangphummet](https://www.linkedin.com/in/apiwand/) | [Josahn Oginga](https://www.linkedin.com/in/josahn-oginga/) | [Marzia Azizi](https://www.linkedin.com/in/marzia-a-30217b90/)

## Dashboard
🌐 [Launch the CBC CAT Pathway Advisor](https://josahn-oginga.shinyapps.io/cbc-cat-pathway-advisor/)

## Overview
Kenya's Competency Based Curriculum (CBC) requires Junior Secondary students to select a pathway — **STEM**, **Social Sciences**, or **Arts & Sports Science** — that will shape their academic and career trajectory. Traditional fixed-form assessments expose all students to the same items regardless of ability, leading to imprecise measurement and inefficient testing.

This project implements a **Computerized Adaptive Test (CAT)** that dynamically selects items based on each student's estimated ability, achieving precise measurement in fewer questions. Pathway recommendation combines aptitude performance with an OCEAN personality profile to produce a holistic, defensible placement decision.

## Research Question
Can a psychometrically grounded CAT engine, combining IRT-calibrated aptitude items and OCEAN personality scoring, produce reliable and efficient pathway recommendations for Kenya CBC Junior Secondary students?

## Methodology

### Item Pool
- **30 aptitude items** (MCQ, dichotomous) spanning three domains: STEM (12), Social Sciences (10), Arts & Sports (8)
- **26 personality items** (Likert 1–5) measuring five OCEAN traits: Openness, Conscientiousness, Extraversion, Agreeableness, Neuroticism
- 500 simulated student responses generated from known latent parameters

### IRT Calibration
- **2PL model** fitted to aptitude items using `mirt` — estimates discrimination (*a*) and difficulty (*b*) per item
- **Graded Response Model (GRM)** fitted separately per OCEAN trait — handles polytomous Likert responses
- Model fit evaluated via M2 statistic (aptitude) and S-X² item fit (personality)

### CAT Engine
- **Item selection**: Maximum Fisher information at current θ̂; first item anchored at *b* closest to 0
- **Ability updating**: Expected A Posteriori (EAP) estimation after each response
- **Stopping rule**: SE < 0.30 or maximum 10 items administered
- Demonstrated on three students representing low, mid, and high ability profiles

### Pathway Recommendation
- Aptitude score per pathway computed as proportion correct on administered items
- Personality fit score per pathway derived from theoretically motivated OCEAN trait weights
- Final recommendation from a weighted composite: **70% aptitude / 30% personality** (adjustable in dashboard)

## How to Use
1. Clone the repository
2. Open `script/analysis.Rmd` in RStudio and run all chunks to reproduce the full analysis
3. After running, save objects via `save()` to `script/cat_objects.RData`
4. Launch the dashboard locally with `shiny::runApp("script/app.R")` or visit the live link above

## Dependencies
```r
install.packages(c("mirt", "psych", "dplyr", "readr", "readxl",
                   "ggplot2", "shiny", "shinydashboard", "fmsb"))
```

## License
This project is licensed under the [Creative Commons Attribution 4.0 International License (CC BY 4.0)](https://creativecommons.org/licenses/by/4.0/).

You are free to use, modify, and distribute this work for any purpose, including commercial use, provided you give appropriate credit to the original authors. This project is shared in the spirit of open science and public good — we encourage its use for education, research, and community development. Please ensure your use aligns with ethical standards and contributes to meaningful outcomes.
