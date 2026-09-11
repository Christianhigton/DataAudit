\name{multilevelReliability}
\alias{multilevelReliability}
\alias{multilevelReliabilityBase}
\alias{multilevelReliabilityClass}
\alias{multilevelReliabilityOptions}
\alias{multilevelReliabilityResults}
\title{Multilevel Reliability}
\usage{
multilevelReliability(
  data,
  items = list(),
  cluster = NULL,
  time = NULL,
  includeOmega = TRUE,
  confidenceIntervals = TRUE,
  includeICCs = TRUE,
  includeStructure = TRUE,
  includeInterpretation = TRUE,
  includeModelDetails = FALSE
)
}
\arguments{
\item{data}{A data frame supplied by R or jamovi.}
\item{items}{Two or more numeric scale items measured on each occasion. Three or more are generally needed for an identified one-factor model.}
\item{cluster}{A participant or cluster identifier.}
\item{time}{An optional occasion, session, or time variable used to describe the data structure.}
\item{includeOmega}{Show within-person and between-person McDonald's omega.}
\item{confidenceIntervals}{Show 95\% confidence intervals supplied by the fitted model.}
\item{includeICCs}{Show method-of-moments variance components and variance-partition ICCs.}
\item{includeStructure}{Show observation and participant counts.}
\item{includeInterpretation}{Show plain-English interpretation and audit guidance.}
\item{includeModelDetails}{Show optional model and fit details.}
}
\value{A jamovi results object containing analysis notes, data-structure information,
multilevel omega, variance-partition ICCs, interpretation, and optional model details.}
\description{
Fits a two-level one-factor confirmatory factor model with \code{lavaan} and
estimates reliability separately for variation within participants and stable
differences between participants. Standard errors and confidence intervals for
omega are delta-method quantities from the fitted model. The ICC output
describes within-person and between-person variance and is not an inter-rater
agreement analysis.
}
\references{
Geldhof, G. J., Preacher, K. J., & Zyphur, M. J. (2014). Reliability
estimation in a multilevel confirmatory factor analysis framework.
\emph{Psychological Methods, 19}(1), 72--91.
}
\keyword{reliability}
\keyword{multilevel}
