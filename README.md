# CASIMiR: Comparing Automated Subject Indexing Methods in R

[![status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/deutsche-nationalbibliothek/casimir/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/deutsche-nationalbibliothek/casimir/actions/workflows/R-CMD-check.yaml)

CASIMiR is a toolbox to facilitate comparative analysis of automated subject
indexing methods in R. 

## Why should you use CASIMiR?

Certainly you are able to compute your F-score, precision and recall metrics
with your favourite metric function in scikit-learn or other ML libraries. 
But does that really help in understanding the quality of your favourite
subject indexing method?
If method $A$ scores 0.4 in F-score and method $B$ scores 0.41, does it mean $B$
is better than $A$?
Maybe yes. But likely there are many nuances in the results you miss by 
looking at overall score functions. Did you know: the quality of subject
suggestions may vary considerably among subject groups! It may also strongly
depend on the amount of training material per subject term. 
Here comes CASIMiR: it will help you in a detailed drill-down analysis of your
results. In addition, CASIMiR offers advanced metric functions, such as 
area under the precision-recall curve, NDCG, graded relevance metrics and
propensity scored metrics. Last but not least: CASIMiR allows to compute 
metrics with confidence intervals, based on bootstrap methods. Thus, it 
will also help you estimate the uncertainty in your results due to the
possibly limited size of your test sample. 

## Why R?

Mainly due to the authors' love for R, but here are some reasons that might
convince other people:

  * R's user-friendly capabilities for data analysis with the tidyverse packages 
  * professional visualization with ggplot2
  * seamless handling of grouped data structures
  * efficient data wrangling libraries, such as collapse and dplyr, 
    which are the backbone of CASIMiR
  * **the wonderful and inclusive R community**

## Installation instructions

### Install a stable development version from GitHub (requires compilation)

```
remotes::install_github("deutsche-nationalbibliothek/casimir")
```

<!-- ### Installation with Conda/Mamba

```
conda create --name my-env -c conda-forge r-casimir
``` -->

## Getting Started

Most functions expect at least two inputs: `gold_standard` and `predicted`.
Both are expected to be data.frames with subject suggestions in a long format.

Example table for gold standard or predictions:

| doc_id | label_id |
|--------|----------|
|   1    |   A      |
|   1    |   B      |
|   2    |   A      |
|   3    |   C      |

For ranked retrieval metrics, i.e. metrics taking into account an ranking of
the subject suggestions based on some score, the input format also expects an
additional score column:

| doc_id | label_id | score |
|--------|----------|-------|
|   1    |   A      | 0.73  |
|   1    |   B      | 0.15  |
|   2    |   A      | 0.92  |
|   3    |   C      | 0.34  |

```
res <- compute_set_retrieval_scores(
  gold_standard = dnb_gold_standard,
  predictions = dnb_test_predictions
)

head(res)
```

For more advanced examples visit our vignettes.

  * creating stratified result tables
  * producing precision-recall curves
  * computing graded relevance metrics 
  * computing propensity scored metrics

## Acknowledgments

This work was created within the [DNB AI Project](https://www.dnb.de/ki-projekt
). The project was funded by
[Federal Government Commissioner for Culture and the Media](https://kulturstaatsminister.de//)
as part of the national AI strategy. 
