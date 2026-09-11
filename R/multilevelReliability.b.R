multilevelReliabilityClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "multilevelReliabilityClass",
    inherit = multilevelReliabilityBase,
    private = list(
        .run = function() {
            analysis <- .mlr_analyse(
                data = as.data.frame(self$data, stringsAsFactors = FALSE),
                items = self$options$items,
                cluster = self$options$cluster,
                time = self$options$time,
                ci = self$options$confidenceIntervals
            )

            self$results$notes$setContent(.mlr_notes_html(analysis$notes))

            if (self$options$includeStructure)
                .mlr_add_rows(self$results$structure, analysis$structure)
            if (self$options$includeOmega)
                .mlr_add_rows(self$results$reliability, analysis$reliability)
            if (self$options$includeICCs)
                .mlr_add_rows(self$results$iccs, analysis$iccs)
            if (self$options$includeInterpretation)
                self$results$interpretation$setContent(analysis$interpretation)
            if (self$options$includeModelDetails)
                .mlr_add_rows(self$results$modelDetails, analysis$modelDetails)
        }
    )
)

.mlr_add_rows <- function(table, rows) {
    if (is.null(rows) || nrow(rows) == 0L)
        return(invisible(NULL))
    for (i in seq_len(nrow(rows)))
        table$addRow(rowKey = i, values = as.list(rows[i, , drop = FALSE]))
    invisible(NULL)
}

.mlr_escape <- function(x) {
    x <- paste(as.character(x), collapse = "")
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x
}

.mlr_notes_html <- function(notes) {
    notes <- unique(notes[nzchar(notes)])
    if (length(notes) == 0L)
        notes <- "No data or model warnings were detected."
    items <- paste0("<li>", vapply(notes, .mlr_escape, character(1)), "</li>", collapse = "")
    paste0("<div style=\"font-family:sans-serif;line-height:1.45\"><ul>", items, "</ul></div>")
}

.mlr_empty_reliability <- function() {
    data.frame(level = character(), omega = numeric(), se = numeric(),
        lower = numeric(), upper = numeric(), stringsAsFactors = FALSE)
}

.mlr_empty_iccs <- function() {
    data.frame(measure = character(), withinVariance = numeric(),
        betweenVariance = numeric(), icc = numeric(), stringsAsFactors = FALSE)
}

.mlr_empty_details <- function() {
    data.frame(measure = character(), value = character(), stringsAsFactors = FALSE)
}

.mlr_structure_rows <- function(data, model_data, cluster, time = NULL) {
    rows <- list(
        c("Total observations", nrow(data)),
        c("Observations used in reliability model", nrow(model_data)),
        c("Observations excluded for missing item/cluster data", nrow(data) - nrow(model_data))
    )

    if (!is.null(cluster) && cluster %in% names(data)) {
        total_clusters <- length(unique(data[[cluster]][!is.na(data[[cluster]])]))
        retained <- if (nrow(model_data)) length(unique(model_data[[cluster]])) else 0L
        rows <- c(rows, list(c("Participants with non-missing ID", total_clusters),
            c("Participants retained", retained)))
        if (nrow(model_data)) {
            sizes <- as.numeric(table(model_data[[cluster]]))
            rows <- c(rows, list(
                c("Mean observations per participant", format(round(mean(sizes), 2), nsmall = 2)),
                c("Median observations per participant", format(stats::median(sizes), trim = TRUE)),
                c("Minimum observations per participant", min(sizes)),
                c("Maximum observations per participant", max(sizes)),
                c("Participants with one observation", sum(sizes == 1L))
            ))
        }
    }

    if (!is.null(time) && time %in% names(data)) {
        tx <- if (nrow(model_data)) model_data[[time]] else data[[time]][0]
        tx <- tx[!is.na(tx)]
        rows <- c(rows, list(c("Non-missing time values in model rows", length(tx)),
            c("Distinct measurement occasions", length(unique(tx)))))
        if (length(tx)) {
            if (is.numeric(tx) || inherits(tx, c("Date", "POSIXt"))) {
                range_text <- paste(format(min(tx)), format(max(tx)), sep = " to ")
                rows <- c(rows, list(c("Time range", range_text)))
            } else if (length(unique(tx)) <= 12L) {
                rows <- c(rows, list(c("Observed occasions", paste(unique(as.character(tx)), collapse = ", "))))
            }
        }
    }

    out <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
    names(out) <- c("measure", "value")
    out
}

# One-way random-effects method-of-moments decomposition for unequal cluster sizes.
# This ICC describes variance partitioning, not inter-rater agreement.
.mlr_variance_components <- function(x, cluster) {
    ok <- is.finite(x) & !is.na(cluster)
    x <- x[ok]
    cluster <- droplevels(factor(cluster[ok]))
    n <- length(x)
    j <- nlevels(cluster)
    if (n <= j || j < 2L)
        return(c(within = NA_real_, between = NA_real_, icc = NA_real_))
    nj <- as.numeric(table(cluster))
    means <- as.numeric(tapply(x, cluster, mean))
    grand <- mean(x)
    ss_between <- sum(nj * (means - grand)^2)
    ss_within <- sum((x - means[as.integer(cluster)])^2)
    ms_between <- ss_between / (j - 1)
    ms_within <- ss_within / (n - j)
    n0 <- (n - sum(nj^2) / n) / (j - 1)
    between <- (ms_between - ms_within) / n0
    between <- max(0, between)
    total <- between + ms_within
    c(within = ms_within, between = between,
        icc = if (is.finite(total) && total > 0) between / total else NA_real_)
}

.mlr_icc_table <- function(model_data, items, cluster) {
    scores <- c(list(`Scale mean` = rowMeans(model_data[items])),
        stats::setNames(lapply(items, function(v) model_data[[v]]), items))
    rows <- lapply(names(scores), function(label) {
        vc <- .mlr_variance_components(scores[[label]], model_data[[cluster]])
        data.frame(measure = label, withinVariance = unname(vc["within"]),
            betweenVariance = unname(vc["between"]), icc = unname(vc["icc"]),
            stringsAsFactors = FALSE)
    })
    do.call(rbind, rows)
}

.mlr_model_syntax <- function(items) {
    p <- length(items)
    safe <- paste0("item", seq_len(p))
    lw <- paste0("lw", seq_len(p))
    lb <- paste0("lb", seq_len(p))
    ew <- paste0("ew", seq_len(p))
    eb <- paste0("eb", seq_len(p))
    sum_lw <- paste(lw, collapse = " + ")
    sum_lb <- paste(lb, collapse = " + ")
    paste(
        "level: 1",
        paste0("withinFactor =~ ", paste0(lw, "*", safe, collapse = " + ")),
        paste0(safe, " ~~ ", ew, "*", safe, collapse = "\n"),
        paste0("omega_within := (", sum_lw, ")^2 / ((", sum_lw, ")^2 + ",
            paste(ew, collapse = " + "), ")"),
        "level: 2",
        paste0("betweenFactor =~ ", paste0(lb, "*", safe, collapse = " + ")),
        paste0(safe, " ~~ ", eb, "*", safe, collapse = "\n"),
        paste0("omega_between := (", sum_lb, ")^2 / ((", sum_lb, ")^2 + ",
            paste(eb, collapse = " + "), ")"),
        sep = "\n"
    )
}

.mlr_clean_lavaan_warnings <- function(warnings) {
    if (!length(warnings))
        return(character())
    text <- paste(warnings, collapse = " ")
    out <- character()
    if (grepl("negative", text, ignore.case = TRUE))
        out <- c(out, "The model estimated a negative variance (an improper solution). Omega at that level may be unstable or outside its usual 0-to-1 range.")
    if (grepl("not positive definite|singular|could not invert", text, ignore.case = TRUE))
        out <- c(out, "The model information or covariance matrix was singular. Standard errors or omega may not be estimable.")
    if (grepl("did not converge|failed to converge", text, ignore.case = TRUE))
        out <- c(out, "The multilevel model did not converge; omega estimates are not reported.")
    if (!length(out))
        out <- "The model produced a numerical warning. Treat the estimates as potentially unstable and inspect the model details."
    unique(out)
}

.mlr_fit_model <- function(model_data, items, cluster, ci = TRUE) {
    fit_data <- model_data[c(items, cluster)]
    names(fit_data) <- c(paste0("item", seq_along(items)), "clusterID")
    fit_data$clusterID <- as.numeric(factor(fit_data$clusterID))
    warnings <- character()
    fit <- tryCatch(
        withCallingHandlers(
            lavaan::sem(.mlr_model_syntax(items), data = fit_data,
                cluster = "clusterID", std.lv = TRUE, meanstructure = TRUE,
                parallel = "no", ncpus = 1L),
            warning = function(w) {
                warnings <<- c(warnings, conditionMessage(w))
                invokeRestart("muffleWarning")
            }
        ),
        error = function(e) e
    )
    if (inherits(fit, "error"))
        return(list(fit = NULL, reliability = .mlr_empty_reliability(),
            warnings = c(.mlr_clean_lavaan_warnings(warnings),
                "The multilevel model could not be estimated. This commonly reflects too little variation, too few independent clusters, or an unidentified measurement model.")))
    if (!isTRUE(lavaan::lavInspect(fit, "converged")))
        return(list(fit = fit, reliability = .mlr_empty_reliability(),
            warnings = unique(c(.mlr_clean_lavaan_warnings(warnings),
                "The multilevel model did not converge; omega estimates are not reported."))))

    warnings <- .mlr_clean_lavaan_warnings(warnings)
    pe <- lavaan::parameterEstimates(fit, ci = ci, level = 0.95)
    pe <- pe[pe$op == ":=" & pe$lhs %in% c("omega_within", "omega_between"), , drop = FALSE]
    wanted <- c("omega_within", "omega_between")
    pe <- pe[match(wanted, pe$lhs), , drop = FALSE]
    reliability <- data.frame(
        level = c("Within-person", "Between-person"),
        omega = pe$est,
        se = pe$se,
        lower = if (ci) pe$ci.lower else NA_real_,
        upper = if (ci) pe$ci.upper else NA_real_,
        stringsAsFactors = FALSE
    )
    invalid <- !is.finite(reliability$omega)
    if (any(invalid)) {
        lev <- paste(reliability$level[invalid], collapse = " and ")
        warnings <- c(warnings, paste(lev, "omega was not estimable."))
    }
    outside <- is.finite(reliability$omega) & (reliability$omega < 0 | reliability$omega > 1)
    if (any(outside))
        warnings <- c(warnings, "An omega estimate fell outside 0 to 1, indicating an improper or unstable solution; interpret it cautiously.")
    list(fit = fit, reliability = reliability, warnings = unique(warnings))
}

.mlr_model_details <- function(fit, n, clusters) {
    base <- list(
        c("Model", "Two-level one-factor confirmatory factor analysis"),
        c("Estimator", if (is.null(fit)) "Not estimated" else "Maximum likelihood"),
        c("Missing-data handling", "Complete rows for selected items and cluster ID"),
        c("Observations used", n),
        c("Participants retained", clusters),
        c("Converged", if (is.null(fit)) "No" else as.character(isTRUE(lavaan::lavInspect(fit, "converged"))))
    )
    if (!is.null(fit) && isTRUE(lavaan::lavInspect(fit, "converged"))) {
        fm <- tryCatch(lavaan::fitMeasures(fit), error = function(e) numeric())
        labels <- c(chisq = "Chi-square", df = "Degrees of freedom", pvalue = "Model p-value",
            cfi = "CFI", tli = "TLI", rmsea = "RMSEA", srmr_within = "SRMR (within)",
            srmr_between = "SRMR (between)")
        for (key in names(labels)) {
            if (key %in% names(fm) && is.finite(fm[[key]]))
                base <- c(base, list(c(labels[[key]], format(round(fm[[key]], 4), trim = TRUE))))
        }
    }
    out <- as.data.frame(do.call(rbind, base), stringsAsFactors = FALSE)
    names(out) <- c("measure", "value")
    out
}

.mlr_interpretation_html <- function(reliability, notes) {
    style <- "<div style=\"font-family:sans-serif;line-height:1.5\">"
    if (nrow(reliability) < 2L || any(!is.finite(reliability$omega))) {
        return(paste0(style,
            "<p><strong>Reliability could not be estimated at both levels.</strong></p>",
            "<p>Review the Analysis Notes. Multilevel omega needs usable variation and a stable measurement model at both the within- and between-person levels.</p></div>"))
    }
    w <- reliability$omega[reliability$level == "Within-person"]
    b <- reliability$omega[reliability$level == "Between-person"]
    adequate <- function(x) is.finite(x) && x >= .70
    if (adequate(w) && adequate(b)) {
        heading <- "Reliability appears adequate at both levels."
        body <- "The selected items show conventionally acceptable consistency for measuring both occasion-to-occasion variation within participants and stable differences between participants."
    } else if (!adequate(w) && adequate(b)) {
        heading <- "Caution: reliability differs across levels."
        body <- "Between-person reliability is conventionally acceptable, but within-person reliability is lower. The scale may distinguish participants more reliably than it detects changes within the same participant over time."
    } else if (adequate(w) && !adequate(b)) {
        heading <- "Caution: reliability differs across levels."
        body <- "Within-person reliability is conventionally acceptable, but between-person reliability is lower. The items may track change within participants more consistently than they distinguish participants' typical levels."
    } else {
        heading <- "Reliability may be limited at both levels."
        body <- "Both estimates are below a commonly used descriptive benchmark. Consider item quality, scale length, construct breadth, and the amount of variation at each level before deciding whether the scale is suitable."
    }
    paste0(style, "<p><strong>", heading, "</strong></p><p>", body,
        "</p><p><strong>Within-person reliability</strong> describes how consistently the items measure changes within the same participant across occasions. ",
        "<strong>Between-person reliability</strong> describes how consistently they distinguish participants who generally differ on the construct.</p>",
        "<p>The .70 comparison is only a rough convention, not a pass/fail rule. Interpret reliability in light of the intended decisions, sample, number of items, and study design.</p></div>")
}

.mlr_analyse <- function(data, items, cluster, time = NULL, ci = TRUE) {
    items <- intersect(as.character(items %||% character()), names(data))
    cluster <- as.character(cluster %||% character())
    cluster <- if (length(cluster) && cluster[1] %in% names(data)) cluster[1] else NULL
    time <- as.character(time %||% character())
    time <- if (length(time) && time[1] %in% names(data)) time[1] else NULL
    empty_data <- data[FALSE, , drop = FALSE]
    structure <- .mlr_structure_rows(data, empty_data, cluster, time)
    result <- list(structure = structure, reliability = .mlr_empty_reliability(),
        iccs = .mlr_empty_iccs(), modelDetails = .mlr_empty_details(), notes = character())

    if (length(items) < 2L) {
        result$notes <- "Select at least two numeric items. Three or more items are generally needed for an identified one-factor omega model."
        result$interpretation <- .mlr_interpretation_html(result$reliability, result$notes)
        return(result)
    }
    if (is.null(cluster)) {
        result$notes <- "Select one cluster or participant ID variable."
        result$interpretation <- .mlr_interpretation_html(result$reliability, result$notes)
        return(result)
    }
    if (cluster %in% items) {
        result$notes <- "The cluster / participant ID must be different from the selected scale items."
        result$interpretation <- .mlr_interpretation_html(result$reliability, result$notes)
        return(result)
    }
    non_numeric <- items[!vapply(data[items], is.numeric, logical(1))]
    if (length(non_numeric)) {
        result$notes <- paste("All items must be numeric. Check:", paste(non_numeric, collapse = ", "))
        result$interpretation <- .mlr_interpretation_html(result$reliability, result$notes)
        return(result)
    }

    needed <- c(items, cluster)
    used <- stats::complete.cases(data[needed])
    model_data <- data[used, unique(c(needed, time)), drop = FALSE]
    model_data[[cluster]] <- droplevels(factor(model_data[[cluster]]))
    result$structure <- .mlr_structure_rows(data, model_data, cluster, time)
    notes <- character()
    excluded <- sum(!used)
    if (excluded > 0L)
        notes <- c(notes, sprintf("%d of %d observations (%.1f%%) were excluded because an item or participant ID was missing.", excluded, nrow(data), 100 * excluded / max(1, nrow(data))))
    if (excluded / max(1, nrow(data)) >= .20)
        notes <- c(notes, "A substantial proportion of observations was excluded; assess whether missingness could affect the estimates.")
    if (!nrow(model_data)) {
        result$notes <- c(notes, "No complete observations remain for analysis.")
        result$interpretation <- .mlr_interpretation_html(result$reliability, result$notes)
        return(result)
    }
    clusters <- nlevels(model_data[[cluster]])
    if (clusters < 2L) {
        result$notes <- c(notes, "The cluster variable contains only one participant after missing data are removed. Multilevel reliability requires repeated observations from multiple participants.")
        result$modelDetails <- .mlr_model_details(NULL, nrow(model_data), clusters)
        result$interpretation <- .mlr_interpretation_html(result$reliability, result$notes)
        return(result)
    }
    if (clusters < 3L) {
        result$notes <- c(notes, "Too few participants are available to estimate a two-level factor model reliably (at least three are required).")
        result$modelDetails <- .mlr_model_details(NULL, nrow(model_data), clusters)
        result$interpretation <- .mlr_interpretation_html(result$reliability, result$notes)
        return(result)
    }
    if (clusters < 20L)
        notes <- c(notes, sprintf("Only %d participants are available. Multilevel estimates and confidence intervals may be unstable in a small cluster sample.", clusters))
    sizes <- as.numeric(table(model_data[[cluster]]))
    if (any(sizes == 1L))
        notes <- c(notes, sprintf("%d participant(s) have only one usable observation. They provide no direct within-person replication.", sum(sizes == 1L)))
    constant <- items[vapply(model_data[items], function(x) stats::var(x) == 0, logical(1))]
    if (length(constant)) {
        result$notes <- c(notes, paste("Omega cannot be estimated because these items have zero variance:", paste(constant, collapse = ", ")))
        result$iccs <- .mlr_icc_table(model_data, items, cluster)
        result$modelDetails <- .mlr_model_details(NULL, nrow(model_data), clusters)
        result$interpretation <- .mlr_interpretation_html(result$reliability, result$notes)
        return(result)
    }
    if (length(items) < 3L)
        notes <- c(notes, "A two-item one-factor model is often underidentified; omega will be reported only if the model is estimable.")

    result$iccs <- .mlr_icc_table(model_data, items, cluster)
    between_items <- result$iccs$betweenVariance[-1]
    if (all(!is.finite(between_items)) ||
            (any(is.finite(between_items)) && all(between_items <= sqrt(.Machine$double.eps), na.rm = TRUE)))
        notes <- c(notes, "The selected items have no estimable between-person variance; between-person omega is unlikely to be identifiable.")

    fitted <- .mlr_fit_model(model_data, items, cluster, ci)
    result$reliability <- fitted$reliability
    result$notes <- unique(c(notes, fitted$warnings))
    result$modelDetails <- .mlr_model_details(fitted$fit, nrow(model_data), clusters)
    result$interpretation <- .mlr_interpretation_html(result$reliability, result$notes)
    result
}
