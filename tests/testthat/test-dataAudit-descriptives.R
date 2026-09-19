test_that("descriptives identify observed floor and ceiling effects", {
    data <- data.frame(
        floor_score = c(rep(0, 4), 1:6),
        ceiling_score = c(1:6, rep(7, 4)),
        balanced_score = 1:10
    )
    dictionary <- data.frame(
        variable = names(data),
        inferred = rep("Continuous numeric", ncol(data)),
        stringsAsFactors = FALSE
    )

    result <- DataAudit:::.da_descriptives(data, names(data), dictionary, 30)
    flags <- stats::setNames(result$floorCeilingFlag, result$variable)

    expect_equal(result$floorPct[result$variable == "floor_score"], 40)
    expect_equal(result$ceilingPct[result$variable == "ceiling_score"], 40)
    expect_equal(flags[["floor_score"]], "Floor effect")
    expect_equal(flags[["ceiling_score"]], "Ceiling effect")
    expect_equal(flags[["balanced_score"]], "No effect at or above threshold")
})

test_that("floor and ceiling checks handle constants and missing values", {
    constant <- DataAudit:::.da_floor_ceiling(c(5, 5, NA_real_), 15)
    empty <- DataAudit:::.da_floor_ceiling(c(NA_real_, Inf), 15)

    expect_equal(constant$floorPct, 100)
    expect_equal(constant$ceilingPct, 100)
    expect_match(constant$flag, "No variation")
    expect_equal(empty$floorN, 0L)
    expect_true(is.na(empty$floorPct))
    expect_equal(empty$flag, "Insufficient data")
})
