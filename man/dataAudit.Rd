\name{dataAudit}
\alias{dataAudit}
\alias{dataAuditBase}
\alias{dataAuditClass}
\alias{dataAuditOptions}
\alias{dataAuditResults}
\title{Data Audit and Codebook}
\usage{
dataAudit(
  data,
  vars = list(),
  outcome = NULL,
  predictors = list(),
  group = NULL,
  caseID = NULL,
  includeOverview = TRUE,
  includeDictionary = TRUE,
  includeMissing = TRUE,
  includeQuality = TRUE,
  includeDescriptives = FALSE,
  includeFrequencies = FALSE,
  includeGraphs = FALSE,
  includeAssumptions = FALSE,
  includeSummary = TRUE,
  normalityChecks = TRUE,
  outlierChecks = TRUE,
  homogeneityChecks = FALSE,
  linearityChecks = FALSE,
  multicollinearityChecks = FALSE,
  highMissing = 20,
  moderateMissing = 5,
  smallCategoryN = 5,
  zThreshold = 3.29,
  iqrMultiplier = 1.5,
  vifWarning = 5,
  vifSerious = 10,
  skewThreshold = 1,
  kurtosisThreshold = 1,
  maxCaseRows = 50,
  graphMaxVars = 9,
  graphTopCategories = 12
)
}
\arguments{
\item{data}{A data frame supplied by R or jamovi.}
\item{vars}{Optional variables to include. If empty, all variables are audited.}
\item{outcome}{Optional outcome variable for selected assumption checks.}
\item{predictors}{Optional predictor variables for selected assumption checks.}
\item{group}{Optional grouping variable for Levene's test.}
\item{caseID}{Optional case identifier for case-level missingness and outlier output.}
\item{includeOverview, includeDictionary, includeMissing, includeQuality, includeDescriptives, includeFrequencies, includeGraphs, includeAssumptions, includeSummary}{Logical switches controlling report sections.}
\item{normalityChecks, outlierChecks, homogeneityChecks, linearityChecks, multicollinearityChecks}{Logical switches controlling assumption-screening sections.}
\item{highMissing, moderateMissing, smallCategoryN, zThreshold, iqrMultiplier, vifWarning, vifSerious, skewThreshold, kurtosisThreshold, maxCaseRows, graphMaxVars, graphTopCategories}{Editable screening thresholds and display limits.}
}
\value{
A jamovi results object containing overview, dictionary, missingness,
data-quality, descriptives, frequency, assumption-screening, and summary
outputs depending on the selected options.
}
\description{
Produces a teaching-friendly data audit and codebook for jamovi datasets.
The analysis infers variable roles from data values, reports missingness,
flags common data-quality issues, and optionally screens normality, outliers,
homogeneity of variance, linearity, and multicollinearity. Optional graphs
include missingness bars, numeric histograms, numeric boxplots, categorical
bar charts, and a numeric correlation heatmap.
}
\keyword{datasets}
