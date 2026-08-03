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
  includeAuditScore = TRUE,
  includeChecklist = TRUE,
  includeExportCodebook = TRUE,
  includeRangeRules = FALSE,
  includeDuplicateReview = TRUE,
  includeSurveyChecks = FALSE,
  includeAttentionChecks = FALSE,
  includeViolinPlots = FALSE,
  includeNormalCurveHistograms = FALSE,
  includeSurveyPlots = FALSE,
  rangeRules = "",
  screeningDesign = "ungrouped",
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
  kurtosisThreshold = 2,
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
\item{includeOverview, includeDictionary, includeMissing, includeQuality, includeDescriptives, includeFrequencies, includeGraphs, includeAssumptions, includeSummary, includeAuditScore, includeChecklist, includeExportCodebook, includeRangeRules, includeDuplicateReview, includeSurveyChecks, includeAttentionChecks}{Logical switches controlling report sections.}
\item{includeViolinPlots, includeNormalCurveHistograms, includeSurveyPlots}{Logical switches controlling optional graph displays.}
\item{rangeRules}{Optional range rules such as \code{age=18:100} or \code{likert_1=1:5}. Separate multiple rules with new lines or semicolons.}
\item{screeningDesign}{Screening design label, typically \code{"ungrouped"} or \code{"grouped"}.}
\item{normalityChecks, outlierChecks, homogeneityChecks, linearityChecks, multicollinearityChecks}{Logical switches controlling assumption-screening sections.}
\item{highMissing, moderateMissing, smallCategoryN, zThreshold, iqrMultiplier, vifWarning, vifSerious, skewThreshold, kurtosisThreshold, maxCaseRows, graphMaxVars, graphTopCategories}{Editable screening thresholds and display limits.}
}
\value{
A jamovi results object containing overview, traffic-light score, screening
checklist, notes, references, compact dictionary, exportable codebook, missingness,
data-quality, range-rule, duplicate, survey-response, attention-check, metadata, descriptive,
frequency, graph, assumption-screening, and summary outputs depending on the
selected options.
}
\description{
Produces a teaching-friendly data audit and codebook for jamovi datasets.
The analysis infers variable roles from data values, reports missingness,
flags common data-quality issues, and optionally screens missing-data
patterns, applies Little's MCAR test to usable numeric variables, visualises
missingness, and screens duplicate cases, out-of-range rules, survey response patterns,
attention-check flags such as B-lining/straight-lining,
normality, outliers, homogeneity of variance, linearity, and
multicollinearity. Optional graphs include missingness bars, numeric
histograms with optional normal curves, violin plots, numeric boxplots,
categorical bar charts, survey raincloud plots, and a numeric correlation
heatmap. The on-screen variable dictionary is compact; the exportable
codebook keeps the wider range/category, metadata, warning, and action fields.
}
\keyword{datasets}
