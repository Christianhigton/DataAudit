make_multilevel_data <- function(clusters = 50L, occasions = 6L,
                                 within_loading = .45, between_loading = 1,
                                 seed = 4242) {
    set.seed(seed)
    sizes <- if (length(occasions) == 1L) rep(occasions, clusters) else occasions
    id <- rep(seq_len(clusters), sizes)
    n <- length(id)
    between_common <- rnorm(clusters)
    within_common <- rnorm(n)
    out <- data.frame(id = id, occasion = ave(id, id, FUN = seq_along))
    for (item in seq_len(4L)) {
        between_unique <- rnorm(clusters, sd = .20)
        out[[paste0("item", item)]] <-
            between_loading * between_common[id] + between_unique[id] +
            within_loading * within_common + rnorm(n, sd = 1)
    }
    out
}

analyse <- function(data, ci = TRUE) {
    DataAudit:::.mlr_analyse(data, paste0("item", 1:4), "id", "occasion", ci)
}

test_that("ordinary repeated-measures data produce both levels", {
    result <- analyse(make_multilevel_data())
    expect_equal(result$reliability$level, c("Within-person", "Between-person"))
    expect_true(all(is.finite(result$reliability$omega)))
    expect_true(all(is.finite(result$reliability$se)))
    expect_true(all(is.finite(result$reliability$lower)))
    expect_true(all(is.finite(result$reliability$upper)))
})

test_that("within-person and between-person reliability remain distinct", {
    result <- analyse(make_multilevel_data(clusters = 80L, occasions = 8L,
        within_loading = .30, between_loading = 1.20, seed = 14))
    omega <- stats::setNames(result$reliability$omega, result$reliability$level)
    expect_gt(unname(omega["Between-person"]), unname(omega["Within-person"]) + .15)
})

test_that("missing rows are counted and confidence intervals can be hidden", {
    data <- make_multilevel_data()
    data$item1[c(2, 9, 17)] <- NA_real_
    result <- analyse(data, ci = FALSE)
    used <- result$structure$value[result$structure$measure == "Observations used in reliability model"]
    excluded <- result$structure$value[result$structure$measure == "Observations excluded for missing item/cluster data"]
    expect_equal(as.integer(used), nrow(data) - 3L)
    expect_equal(as.integer(excluded), 3L)
    expect_true(all(is.na(result$reliability$lower)))
    expect_match(paste(result$notes, collapse = " "), "excluded")
})

test_that("unequal cluster sizes and singleton participants are described", {
    sizes <- c(1L, 3L, 4L, 6L, 7L, 8L, rep(5L, 24L))
    result <- analyse(make_multilevel_data(clusters = length(sizes), occasions = sizes))
    values <- stats::setNames(result$structure$value, result$structure$measure)
    expect_equal(as.integer(values["Minimum observations per participant"]), 1L)
    expect_equal(as.integer(values["Maximum observations per participant"]), 8L)
    expect_match(paste(result$notes, collapse = " "), "only one usable observation")
})

test_that("zero-variance items fail gracefully", {
    data <- make_multilevel_data()
    data$item4 <- 3
    result <- analyse(data)
    expect_equal(nrow(result$reliability), 0L)
    expect_match(paste(result$notes, collapse = " "), "zero variance")
})

test_that("too few clusters fail gracefully", {
    result <- analyse(make_multilevel_data(clusters = 2L, occasions = 12L))
    expect_equal(nrow(result$reliability), 0L)
    expect_match(paste(result$notes, collapse = " "), "Too few participants")
})

test_that("one cluster and incomplete item selection give useful messages", {
    data <- make_multilevel_data(clusters = 1L, occasions = 20L)
    one <- analyse(data)
    expect_match(paste(one$notes, collapse = " "), "only one participant")
    few <- DataAudit:::.mlr_analyse(data, "item1", "id")
    expect_match(paste(few$notes, collapse = " "), "at least two numeric items")
})

test_that("an unestimable between-person component is diagnosed", {
    set.seed(91)
    clusters <- 30L
    occasions <- 6L
    id <- rep(seq_len(clusters), each = occasions)
    data <- data.frame(id = id, occasion = rep(seq_len(occasions), clusters))
    common <- rep(scale(rnorm(occasions), scale = FALSE), clusters)
    for (item in 1:4) {
        noise <- unlist(lapply(seq_len(clusters), function(i)
            scale(rnorm(occasions), scale = FALSE)))
        data[[paste0("item", item)]] <- common + noise
    }
    result <- analyse(data)
    expect_match(paste(result$notes, collapse = " "),
        "no estimable between-person variance|could not be estimated|did not converge")
})
