test_that("continuous task scores remain eligible for numeric graphs", {
    data <- data.frame(
        interference_nonverbal_task = c(seq(-4.42, 1.63, length.out = 40), NA),
        response_nonverbal_task = rep(c(-3.91, -2.80, -1.68, -0.57, 0.54), length.out = 41),
        ef_performance = seq(-1.94, 1.23, length.out = 41),
        interference_suppression_score = seq(-4.42, 2.14, length.out = 41),
        response_inhibition_score = rep(seq(-2.30, 0.96, length.out = 30), length.out = 41)
    )
    data[] <- lapply(data, function(x) {
        attr(x, "jmv-measure-type") <- "continuous"
        x
    })
    dictionary <- DataAudit:::.da_build_audit(data, names(data), list())$dictionary
    expect_true(all(dictionary$inferred == "Continuous numeric"))
    state <- DataAudit:::.da_plot_state(data, names(data), dictionary, list())
    expect_equal(DataAudit:::.da_plot_numeric_vars(state), names(data))
    expect_length(DataAudit:::.da_plot_categorical_vars(state), 0L)
})

test_that("ID and ordinal metadata retain their roles", {
    id <- seq(0.1, 4.1, length.out = 41)
    attr(id, "jmv-measure-type") <- "id"
    expect_equal(DataAudit:::.da_detect_type(id)$type, "ID variable")
    ordinal <- rep(c(-1.5, -0.5, 0.5, 1.5), 10)
    attr(ordinal, "jmv-measure-type") <- "ordinal"
    expect_equal(DataAudit:::.da_detect_type(ordinal)$type, "Ordinal")
    expect_equal(DataAudit:::.da_detect_type(rep(1:5, 8))$type, "Possible scale item")
})
