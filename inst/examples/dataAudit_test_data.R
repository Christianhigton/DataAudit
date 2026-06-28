# Example datasets for manually testing DataAudit in R or jamovi.
# These cover clean data, missingness, binary variables, Likert items, IDs,
# constants, empty variables, miscoded missing values, skew, and duplicates.

dataAudit_example_data <- function() {
    set.seed(20260624)

    clean <- data.frame(
        id = sprintf("P%03d", 1:50),
        age = round(rnorm(50, 21, 4), 1),
        condition = factor(rep(c("control", "treatment"), 25)),
        anxiety = rnorm(50, 10, 3)
    )

    problematic <- clean
    problematic$binary <- rep(c(0, 1), 25)
    problematic$likert_1 <- sample(1:5, 50, replace = TRUE)
    problematic$likert_2 <- sample(1:7, 50, replace = TRUE)
    problematic$likert_3 <- sample(1:5, 50, replace = TRUE)
    problematic[6, c("likert_1", "likert_2", "likert_3")] <- 3
    problematic$constant <- 1
    problematic$empty <- NA_real_
    problematic$miscoded <- c(rep(999, 3), sample(1:10, 47, replace = TRUE))
    problematic$skewed <- rexp(50)
    problematic$condition_messy <- c("Male", "male", "M", "m", rep("Female", 46))
    problematic$age[c(2, 5, 9, 12, 20, 31)] <- NA
    problematic <- rbind(problematic, problematic[1:2, ])

    list(clean = clean, problematic = problematic)
}
