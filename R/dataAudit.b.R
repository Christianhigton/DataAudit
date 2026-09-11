dataAuditClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "dataAuditClass",
    inherit = dataAuditBase,
    private = list(
        .run = function() {
            self$results$moduleRefs$setContent(paste0(
                "<div style=\"font-family:sans-serif;font-size:0.9em;color:#555;line-height:1.5\">",
                "<p><strong>Higton, C. (2025).</strong> <em>DataAudit: Data audit and multilevel reliability tools for jamovi</em> (Version 0.2.0) [jamovi module].</p>",
                "<p><strong>Tabachnick, B. G., &amp; Fidell, L. S. (2019).</strong> <em>Using Multivariate Statistics</em> (7th ed.). Pearson.</p>",
                "<p style=\"font-size:0.85em;color:#777\">See also: jamovi project (2025). <em>jamovi</em> (Version 2.x) [Computer Software]. Retrieved from https://www.jamovi.org</p>",
                "</div>"
            ))

            data <- as.data.frame(self$data, stringsAsFactors = FALSE)
            opts <- self$options
            vars <- .da_selected_vars(data, opts$vars)

            if (length(vars) == 0L || ncol(data) == 0L) {
                self$results$summary$setContent(.da_html("No variables are available to audit."))
                self$results$missingText$setContent(.da_html("No missing-data report can be produced because the dataset is empty."))
                return(invisible(NULL))
            }

            if (opts$moderateMissing >= opts$highMissing) {
                self$results$summary$setContent(.da_html("Configuration error: the moderate missingness threshold must be lower than the high missingness threshold. Please correct the Thresholds settings."))
                return(invisible(NULL))
            }

            audit <- .da_build_audit(data, vars, opts)
            overview <- .da_dataset_overview(data, vars, audit$dictionary, opts)
            missing <- .da_missing_by_variable(data, vars, opts)
            missing_case <- .da_missing_by_case(data, vars, opts$caseID, opts$maxCaseRows)
            quality <- .da_quality_flags(data, vars, audit$dictionary, opts)
            outliers <- .da_outliers(data, vars, audit$dictionary, opts)
            range_flags <- .da_range_rule_flags(data, vars, opts)
            duplicates <- .da_duplicate_review(data, vars, opts$caseID)
            survey_flags <- .da_survey_response_flags(data, vars, audit$dictionary, opts$caseID)
            attention_flags <- .da_attention_check_flags(data, vars, audit$dictionary, opts$caseID)
            metadata <- .da_metadata_suggestions(audit$dictionary)
            desc <- .da_descriptives(data, vars, audit$dictionary)

            if (opts$includeAuditScore)
                .da_add_rows(self$results$auditScore, .da_audit_score(data, vars, audit$dictionary, missing, quality, outliers, range_flags, duplicates, metadata, opts))

            if (opts$includeChecklist)
                .da_add_rows(self$results$screeningChecklist, .da_screening_checklist(data, vars, audit$dictionary, missing, quality, outliers, range_flags, duplicates, metadata, desc, opts))

            if (opts$includeSummary)
                self$results$summary$setContent(.da_summary_html(data, vars, overview, audit$dictionary, quality, opts))

            if (opts$includeOverview)
                .da_add_rows(self$results$overview, overview)

            if (opts$includeDictionary)
                .da_add_rows(self$results$dictionary, .da_compact_dictionary(audit$dictionary))

            if (opts$includeExportCodebook)
                .da_add_rows(self$results$exportCodebook, .da_export_codebook(audit$dictionary))

            if (opts$includeMissing) {
                .da_add_rows(self$results$missingByVariable, missing)
                .da_add_rows(self$results$missingByCase, missing_case)
                .da_add_rows(self$results$missingPatterns, .da_missing_patterns(data, vars, opts$maxCaseRows))
                .da_add_rows(self$results$littleMCAR, .da_little_mcar(data, vars, audit$dictionary))
                self$results$missingText$setContent(.da_missing_text_html(data, vars, missing))
                missing_plot_state <- .da_plot_state(data, vars, audit$dictionary, opts)
                self$results$missingPlot$setState(missing_plot_state)
                self$results$missingPatternPlot$setState(missing_plot_state)
            }

            if (opts$includeQuality) {
                .da_add_rows(self$results$quality, quality)
                .da_add_rows(self$results$metadataSuggestions, metadata)
            }

            if (opts$includeRangeRules)
                .da_add_rows(self$results$rangeRuleFlags, range_flags)

            if (opts$includeDuplicateReview)
                .da_add_rows(self$results$duplicateReview, duplicates)

            if (opts$includeSurveyChecks)
                .da_add_rows(self$results$surveyResponseFlags, survey_flags)

            if (opts$includeAttentionChecks)
                .da_add_rows(self$results$attentionCheckFlags, attention_flags)

            if (opts$includeDescriptives)
                .da_add_rows(self$results$descriptives, desc)

            if (opts$includeFrequencies)
                .da_add_rows(self$results$frequencies, .da_frequencies(data, vars, audit$dictionary))

            if (opts$includeGraphs) {
                plot_state <- .da_plot_state(data, vars, audit$dictionary, opts)
                self$results$numericPlot$setState(plot_state)
                self$results$boxPlot$setState(plot_state)
                self$results$categoricalPlot$setState(plot_state)
                self$results$correlationPlot$setState(plot_state)
                self$results$violinPlot$setState(plot_state)
                self$results$surveyPlot$setState(plot_state)
            }

            if (opts$includeAssumptions) {
                if (opts$normalityChecks) {
                    .da_add_rows(self$results$normality, .da_normality(data, vars, audit$dictionary, opts))
                    self$results$normalityNote$setContent(.da_html(
                        "For regression and ANOVA, normality is usually assessed on model residuals rather than raw variables. These results should be treated as screening information."
                    ))
                }
                if (opts$outlierChecks)
                    .da_add_rows(self$results$outliers, outliers)
                if (opts$homogeneityChecks)
                    .da_add_rows(self$results$homogeneity, .da_levene(data, vars, audit$dictionary, opts))
                if (opts$linearityChecks)
                    .da_add_rows(self$results$linearity, .da_linearity(data, opts))
                if (opts$multicollinearityChecks)
                    .da_add_rows(self$results$vif, .da_vif(data, opts))
            }
        },
        .missingPlot = function(image, ...) {
            .da_render_missing_plot(image$state)
        },
        .missingPatternPlot = function(image, ...) {
            .da_render_missing_pattern_plot(image$state)
        },
        .numericPlot = function(image, ...) {
            .da_render_numeric_plot(image$state)
        },
        .boxPlot = function(image, ...) {
            .da_render_box_plot(image$state)
        },
        .categoricalPlot = function(image, ...) {
            .da_render_categorical_plot(image$state)
        },
        .correlationPlot = function(image, ...) {
            .da_render_correlation_plot(image$state)
        },
        .violinPlot = function(image, ...) {
            .da_render_violin_plot(image$state)
        },
        .surveyPlot = function(image, ...) {
            .da_render_survey_plot(image$state)
        }
    )
)

`%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0L)
        y
    else
        x
}

.da_html <- function(text) {
    paste0("<div style=\"font-family:sans-serif;line-height:1.45\">", .da_escape(text), "</div>")
}

.da_escape <- function(x) {
    x <- as.character(x %||% "")
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x
}

.da_selected_vars <- function(data, selected) {
    selected <- selected %||% character()
    selected <- as.character(selected)
    selected <- selected[nzchar(selected)]
    if (length(selected) == 0L)
        return(names(data))
    intersect(selected, names(data))
}

.da_add_rows <- function(table, rows) {
    if (is.null(rows) || nrow(rows) == 0L)
        return(invisible(NULL))
    for (i in seq_len(nrow(rows)))
        table$addRow(rowKey = i, values = as.list(rows[i, , drop = FALSE]))
    invisible(NULL)
}

.da_build_audit <- function(data, vars, opts) {
    rows <- lapply(vars, function(v) .da_dictionary_row(data[[v]], v, opts))
    list(dictionary = do.call(rbind.data.frame, c(rows, stringsAsFactors = FALSE)))
}

.da_dictionary_row <- function(x, name, opts) {
    info <- .da_detect_type(x, name)
    valid <- sum(!is.na(x))
    miss <- sum(is.na(x))
    miss_pct <- .da_pct(miss, length(x))
    num <- .da_numeric_values(x)
    examples <- .da_examples(x)
    data.frame(
        variable = name,
        label = .da_label(x),
        jtype = .da_jamovi_type(x),
        measure = .da_measure(x),
        inferred = info$type,
        valid = valid,
        missing = miss,
        missingPct = miss_pct,
        unique = length(unique(x[!is.na(x)])),
        min = if (length(num) > 0L) min(num, na.rm = TRUE) else NA_real_,
        max = if (length(num) > 0L) max(num, na.rm = TRUE) else NA_real_,
        examples = examples,
        flag = info$flag,
        recommendedAction = .da_recommended_action(info$type, info$flag, miss_pct),
        stringsAsFactors = FALSE
    )
}

.da_recommended_action <- function(inferred, flag, missing_pct = NA_real_) {
    if (!is.na(missing_pct) && missing_pct >= 20)
        return("Review missingness before analysis")
    if (grepl("miscoded missing|out-of-range", flag, ignore.case = TRUE))
        return("Set coded missing values to missing")
    if (inferred == "ID variable")
        return("Use as ID only")
    if (inferred %in% c("Nominal categorical", "Dichotomous/binary", "Ordinal"))
        return("Use as categorical/ordinal")
    if (inferred == "Possible scale item")
        return("Check scale coding and direction")
    if (inferred %in% c("Continuous numeric", "Integer/count"))
        return("Suitable for descriptives/regression")
    if (inferred %in% c("Empty variable", "Constant variable"))
        return("Exclude or correct before analysis")
    if (grepl("categorical|scale-like|metadata", flag, ignore.case = TRUE))
        return("Recode as nominal/ordinal if appropriate")
    "Review before analysis"
}

# Infer the practical analytic role of a variable from values as well as metadata.
.da_detect_type <- function(x, name = "") {
    valid <- x[!is.na(x)]
    n <- length(valid)
    if (n == 0L)
        return(list(type = "Empty variable", flag = "No valid data"))

    u <- unique(valid)
    nu <- length(u)
    flags <- character()

    if (nu == 1L)
        return(list(type = "Constant variable", flag = "Only one valid value"))

    if (inherits(x, c("Date", "POSIXct", "POSIXt")))
        return(list(type = "Date/time", flag = ""))

    char <- as.character(valid)
    parsed_dates <- tryCatch(
        suppressWarnings(as.Date(char)),
        error = function(e) rep(as.Date(NA), length(char))
    )
    looks_date <- !is.numeric(valid) && mean(!is.na(parsed_dates)) > 0.8
    if (looks_date)
        return(list(type = "Date/time", flag = "Appears to contain dates; check storage type"))

    unique_ratio <- nu / max(n, 1L)
    avg_chars <- mean(nchar(char), na.rm = TRUE)
    if (unique_ratio > 0.95 && n >= 10L) {
        flags <- c(flags, "Possible ID variable")
        if (!is.numeric(x) || nu > 20L)
            return(list(type = "ID variable", flag = paste(flags, collapse = "; ")))
    }

    if (nu == 2L)
        return(list(type = "Dichotomous/binary", flag = .da_metadata_flag(x, "binary")))

    if (is.ordered(x))
        return(list(type = "Ordinal", flag = .da_metadata_flag(x, "ordinal")))

    num <- .da_numeric_values(x)
    numeric_like <- length(num) == n && n > 0L
    integer_like <- numeric_like && all(abs(num - round(num)) < .Machine$double.eps^0.5)

    if (numeric_like) {
        rng <- range(num, na.rm = TRUE)
        bounded_integer <- integer_like && nu <= 9L && rng[1] >= 0 && rng[2] <= 10
        if (bounded_integer)
            return(list(type = "Possible scale item", flag = paste0("Possible response range ", rng[1], "-", rng[2], .da_metadata_flag(x, "scale"))))
        if (integer_like && nu >= 8L)
            return(list(type = "Integer/count", flag = .da_metadata_flag(x, "integer")))
        if (nu <= 7L)
            return(list(type = "Ordinal", flag = .da_metadata_flag(x, "ordinal")))
        return(list(type = "Continuous numeric", flag = .da_metadata_flag(x, "continuous")))
    }

    if (is.factor(x) && nu <= 30L)
        return(list(type = "Nominal categorical", flag = .da_metadata_flag(x, "nominal")))

    if (nu <= 30L && avg_chars <= 30)
        return(list(type = "Nominal categorical", flag = .da_inconsistent_coding_flag(char)))

    if (unique_ratio > 0.5 || avg_chars > 40)
        return(list(type = "Free-text", flag = "Many unique or long text values"))

    list(type = "Unknown/mixed", flag = "Values do not clearly fit one type")
}

.da_metadata_flag <- function(x, inferred) {
    measure <- tolower(.da_measure(x))
    if (!nzchar(measure))
        return("")
    if (inferred %in% c("binary", "ordinal", "scale") && measure == "continuous")
        return("jamovi says continuous, but values appear categorical or scale-like")
    if (inferred == "continuous" && measure %in% c("nominal", "ordinal"))
        return("jamovi metadata may not match numeric continuous values")
    ""
}

.da_inconsistent_coding_flag <- function(char) {
    low <- tolower(trimws(char))
    raw <- trimws(char)
    tab <- split(raw, low)
    inconsistent <- any(vapply(tab, function(z) length(unique(z)) > 1L, logical(1)))
    if (inconsistent)
        "Possible inconsistent coding, such as differing case or abbreviations"
    else
        ""
}

.da_numeric_values <- function(x) {
    if (inherits(x, c("Date", "POSIXct", "POSIXt")))
        return(numeric())
    if (is.numeric(x) || is.integer(x))
        return(as.numeric(x[!is.na(x)]))
    if (is.factor(x))
        raw <- as.character(x)
    else
        raw <- x
    raw <- raw[!is.na(raw)]
    out <- suppressWarnings(as.numeric(raw))
    if (length(out) == 0L || any(is.na(out)))
        numeric()
    else
        out
}

.da_label <- function(x) {
    attrs <- attributes(x)
    for (nm in c("label", "jmv-desc", "description")) {
        value <- attrs[[nm]]
        if (!is.null(value) && length(value) > 0L && nzchar(as.character(value[1])))
            return(as.character(value[1]))
    }
    ""
}

.da_jamovi_type <- function(x) {
    value <- attr(x, "jmv-type") %||% attr(x, "type")
    if (!is.null(value))
        return(as.character(value[1]))
    paste(class(x), collapse = "/")
}

.da_measure <- function(x) {
    value <- attr(x, "jmv-measure-type") %||% attr(x, "measure") %||% attr(x, "measureType")
    if (!is.null(value))
        return(as.character(value[1]))
    if (is.ordered(x)) "ordinal"
    else if (is.factor(x) || is.character(x)) "nominal"
    else if (is.numeric(x) || is.integer(x)) "continuous"
    else ""
}

.da_examples <- function(x, max_n = 5L) {
    valid <- unique(x[!is.na(x)])
    if (length(valid) == 0L)
        return("")
    values <- as.character(utils::head(valid, max_n))
    paste(values, collapse = ", ")
}

.da_pct <- function(n, d) {
    if (is.null(d) || d == 0L)
        return(NA_real_)
    100 * n / d
}

.da_dataset_overview <- function(data, vars, dictionary, opts) {
    n_cases <- nrow(data)
    n_vars <- length(vars)
    scoped <- data[, vars, drop = FALSE]
    complete <- if (n_vars == 0L) 0L else sum(stats::complete.cases(scoped))
    any_missing <- if (n_vars == 0L) 0L else sum(!stats::complete.cases(scoped))
    total_cells <- n_cases * n_vars
    missing_cells <- sum(is.na(scoped))
    miss_pct <- .da_pct(missing_cells, total_cells)
    dup_rows <- if (n_cases > 0L && n_vars > 0L) sum(duplicated(scoped)) else 0L
    no_missing <- sum(dictionary$missing == 0L)
    moderate <- sum(dictionary$missingPct >= opts$moderateMissing & dictionary$missingPct < opts$highMissing, na.rm = TRUE)
    high <- sum(dictionary$missingPct >= opts$highMissing, na.rm = TRUE)

    data.frame(
        metric = c(
            "Number of rows/cases", "Number of variables", "Number of complete cases",
            "Number of cases with any missing data", "Total number of data cells",
            "Total missing cells", "Percentage of missing cells", "Number of duplicate rows",
            "Number of variables with no missing data", "Number of variables with moderate missingness",
            "Number of variables with high missingness"
        ),
        value = c(
            n_cases, n_vars, complete, any_missing, total_cells, missing_cells,
            sprintf("%.1f%%", miss_pct %||% NA_real_), dup_rows, no_missing, moderate, high
        ),
        stringsAsFactors = FALSE
    )
}

# Summarise missingness by variable and attach simple teaching recommendations.
.da_missing_by_variable <- function(data, vars, opts) {
    rows <- lapply(vars, function(v) {
        miss <- sum(is.na(data[[v]]))
        valid <- sum(!is.na(data[[v]]))
        pct <- .da_pct(miss, length(data[[v]]))
        flag <- if (miss == 0L) "none" else if (pct >= opts$highMissing) "high" else if (pct >= opts$moderateMissing) "moderate" else "low"
        rec <- switch(flag,
            none = "No missing-data action needed.",
            low = "Usually acceptable; document how missing values are handled.",
            moderate = "Review missing-data pattern before analysis.",
            high = "Consider whether the variable is usable or needs special handling."
        )
        data.frame(variable = v, valid = valid, missing = miss, missingPct = pct, flag = flag, recommendation = rec, stringsAsFactors = FALSE)
    })
    do.call(rbind.data.frame, rows)
}

.da_missing_by_case <- function(data, vars, case_id, max_rows = 50L) {
    scoped <- data[, vars, drop = FALSE]
    miss <- rowSums(is.na(scoped))
    keep <- which(miss > 0L)
    if (length(keep) == 0L)
        return(data.frame(case = character(), missing = integer(), missingPct = numeric(), flag = character(), stringsAsFactors = FALSE))
    keep <- utils::head(keep, max_rows)
    ids <- if (!is.null(case_id) && length(case_id) == 1L && case_id %in% names(data)) as.character(data[[case_id]][keep]) else as.character(keep)
    pct <- .da_pct(miss[keep], length(vars))
    flag <- ifelse(pct >= 50, "serious", ifelse(pct >= 20, "warning", "review"))
    data.frame(case = ids, missing = miss[keep], missingPct = pct, flag = flag, stringsAsFactors = FALSE)
}

# Detect common data-quality problems that students should review before analysis.
.da_quality_flags <- function(data, vars, dictionary, opts) {
    rows <- list()
    add <- function(variable, issue, details, severity, action) {
        rows[[length(rows) + 1L]] <<- data.frame(variable = variable, issue = issue, details = details, severity = severity, action = action, stringsAsFactors = FALSE)
    }

    scoped <- data[, vars, drop = FALSE]
    dup <- if (nrow(scoped) > 0L) sum(duplicated(scoped)) else 0L
    if (dup > 0L)
        add("(dataset)", "Possible duplicate rows", paste(dup, "duplicate row(s) detected."), "warning", "Check whether duplicated rows are expected repeated observations.")

    for (v in vars) {
        x <- data[[v]]
        d <- dictionary[dictionary$variable == v, , drop = FALSE]
        inferred <- d$inferred[1]
        if (inferred == "Empty variable")
            add(v, "Empty variable", "No valid values were found.", "serious", "Remove the variable or correct data import/coding.")
        if (inferred == "Constant variable")
            add(v, "Constant variable", "Only one valid value was found.", "warning", "This variable cannot explain variation in most analyses.")
        if (isTRUE(d$missingPct[1] >= opts$highMissing))
            add(v, "High missingness", sprintf("%.1f%% missing.", d$missingPct[1]), "serious", "Review the source of missingness and analysis strategy.")
        if (inferred == "ID variable")
            add(v, "Possible ID variable", "Values are all or almost all unique.", "information", "Use as an identifier, not as a continuous analysis variable.")

        valid <- x[!is.na(x)]
        if (length(valid) > 0L) {
            tab <- table(valid, useNA = "no")
            small <- names(tab)[tab < opts$smallCategoryN]
            if (length(small) > 0L && length(tab) <= 50L)
                add(v, "Very small category size", paste(utils::head(small, 6L), collapse = ", "), "warning", "Consider combining sparse categories or reporting exact counts.")

            miscoded <- c("99", "999", "-99", "-999", "888", "-1")
            present <- intersect(miscoded, unique(as.character(valid)))
            if (length(present) > 0L)
                add(v, "Possible miscoded missing value", paste(present, collapse = ", "), "warning", "Confirm whether these codes should be set to missing.")

            numeric_attempt <- suppressWarnings(as.numeric(as.character(valid)))
            text_in_numeric <- any(is.na(numeric_attempt)) && mean(!is.na(numeric_attempt)) > 0.5
            if (text_in_numeric)
                add(v, "Text values in numeric-looking variable", "Some values are numeric and others are text.", "warning", "Check import and recode text entries.")

            if (inferred %in% c("Continuous numeric", "Integer/count") && d$unique[1] <= 4L)
                add(v, "Very restricted numeric range", paste(d$unique[1], "unique values."), "information", "Check whether this should be ordinal or categorical.")

            if (inferred == "Possible scale item") {
                num <- .da_numeric_values(x)
                rng <- range(num, na.rm = TRUE)
                common_min <- rng[1] %in% c(0, 1)
                common_width <- diff(rng) %in% c(3, 4, 6)
                if (!common_min || !common_width)
                    add(v, "Possible out-of-range scale values", paste("Observed range:", paste(rng, collapse = " to ")), "warning", "Compare observed values with the questionnaire response scale.")
            }

            if (d$unique[1] > 50L && !inferred %in% c("Continuous numeric", "Integer/count", "ID variable", "Free-text"))
                add(v, "Very high cardinality", paste(d$unique[1], "unique values."), "information", "Check whether this variable is an ID or free-text field.")

            inc <- .da_inconsistent_coding_flag(as.character(valid))
            if (nzchar(inc))
                add(v, "Inconsistent coding", inc, "warning", "Standardise category labels before analysis.")
        }
    }

    if (length(rows) == 0L)
        add("(dataset)", "No major data-quality flags", "No automated checks raised a problem.", "information", "Still review coding and metadata before analysis.")
    out <- do.call(rbind.data.frame, rows)
    out$details <- .da_truncate(out$details)
    out$action  <- .da_truncate(out$action)
    out
}


.da_status <- function(severity) {
    ifelse(severity >= 2, "Serious issues", ifelse(severity == 1, "Review", "Good"))
}

.da_audit_score <- function(data, vars, dictionary, missing, quality, outliers, range_flags, duplicates, metadata, opts) {
    total_cells <- nrow(data) * length(vars)
    missing_pct <- .da_pct(sum(dictionary$missing), total_cells)
    missing_status <- if (!is.na(missing_pct) && missing_pct >= opts$highMissing) 2L else if (!is.na(missing_pct) && missing_pct >= opts$moderateMissing) 1L else 0L
    coding_serious <- sum(quality$severity == "serious", na.rm = TRUE) + nrow(range_flags)
    coding_warning <- sum(quality$severity == "warning", na.rm = TRUE)
    coding_status <- if (coding_serious > 0L) 2L else if (coding_warning > 0L) 1L else 0L
    outlier_n <- sum(pmax(outliers$zOutliers, outliers$iqrOutliers, na.rm = TRUE), na.rm = TRUE)
    outlier_status <- if (outlier_n >= 10L) 2L else if (outlier_n > 0L) 1L else 0L
    metadata_status <- if (nrow(metadata) >= 3L) 2L else if (nrow(metadata) > 0L) 1L else 0L
    assumption_status <- if (!isTRUE(opts$includeAssumptions)) 1L else if (outlier_status > 0L || metadata_status > 0L) 1L else 0L
    duplicate_status <- if (nrow(duplicates) > 0L) 1L else 0L
    overall <- max(missing_status, coding_status, outlier_status, metadata_status, assumption_status, duplicate_status, na.rm = TRUE)

    data.frame(
        domain = c("Overall", "Missingness", "Coding and accuracy", "Outliers", "jamovi metadata", "Assumptions"),
        status = .da_status(c(overall, missing_status, coding_status, outlier_status, metadata_status, assumption_status)),
        evidence = c(
            "Highest status across all screening domains.",
            sprintf("%.1f%% of selected data cells are missing.", missing_pct %||% NA_real_),
            paste(coding_serious, "serious and", coding_warning, "warning coding/data-quality flags."),
            paste(outlier_n, "potential univariate outlier flag(s)."),
            paste(nrow(metadata), "metadata repair suggestion(s)."),
            if (isTRUE(opts$includeAssumptions)) "Assumption screening was requested." else "Assumption screening was not requested."
        ),
        recommendedAction = c(
            if (overall >= 2L) "Resolve serious flags before inferential analysis." else if (overall == 1L) "Review flagged domains before analysis." else "Proceed after routine review.",
            if (missing_status >= 1L) "Inspect missing-data patterns and consider sensitivity checks." else "Document low missingness.",
            if (coding_status >= 1L) "Review impossible values, miscoded missing values, sparse categories, and labels." else "No major coding action flagged.",
            if (outlier_status >= 1L) "Inspect flagged rows and justify retention, transformation, winsorising, or removal." else "No univariate outlier action flagged.",
            if (metadata_status >= 1L) "Adjust jamovi measurement levels where suggested." else "No metadata repair action flagged.",
            if (assumption_status >= 1L) "Run assumption checks for the planned model." else "Assumption screen is acceptable for initial review."
        ),
        stringsAsFactors = FALSE
    )
}

.da_screening_checklist <- function(data, vars, dictionary, missing, quality, outliers, range_flags, duplicates, metadata, desc, opts) {
    design <- tolower(trimws(as.character(opts$screeningDesign %||% "ungrouped")))
    if (!design %in% c("grouped", "ungrouped"))
        design <- "ungrouped"
    missing_pct <- .da_pct(sum(dictionary$missing), nrow(data) * length(vars))
    outlier_n <- sum(pmax(outliers$zOutliers, outliers$iqrOutliers, na.rm = TRUE), na.rm = TRUE)
    has_group <- !is.null(opts$group) && length(opts$group) == 1L && opts$group %in% names(data)
    rows <- data.frame(
        step = c("1. Check data accuracy", "2. Check missing data", "3. Check outliers", "4. Check assumptions", "5. Consider transformations", "6. Check multicollinearity/singularity", "7. Screen for design"),
        status = c(
            if (nrow(range_flags) > 0L || any(quality$severity %in% c("warning", "serious"))) "Review" else "Good",
            if (!is.na(missing_pct) && missing_pct >= opts$highMissing) "Serious issues" else if (!is.na(missing_pct) && missing_pct >= opts$moderateMissing) "Review" else "Good",
            if (outlier_n > 0L) "Review" else "Good",
            if (isTRUE(opts$includeAssumptions)) "In progress" else "Review",
            if (any(abs(desc$skewness) > opts$skewThreshold, na.rm = TRUE)) "Review" else "Good",
            if (length(as.character(opts$predictors %||% character())) >= 2L) "In progress" else "Review",
            if (design == "grouped" && !has_group) "Review" else "Good"
        ),
        check = c(
            "Impossible values, miscoded missing values, labels, duplicate rows, and metadata mismatches.",
            "Missing by variable, missing by case, and common missing-data patterns.",
            "Univariate z-score and IQR screens with row/case IDs.",
            "Normality, linearity, homogeneity, and multicollinearity where selected.",
            "Skewness/kurtosis screens suggest whether transformation may be worth considering.",
            "VIF is available when at least two predictors are selected.",
            if (design == "grouped") "Grouped screening: also review grouping variable and homogeneity." else "Ungrouped screening: focus on distributions, outliers, linearity, and multicollinearity."
        ),
        nextAction = c(
            "Correct entry/coding errors before modelling.",
            "Decide whether to delete, impute, model missingness, or run sensitivity checks.",
            "Understand why flagged rows exist before reducing influence or removing cases.",
            "Run assumption checks that match the planned analysis.",
            "Only transform when it improves the planned analysis and remains interpretable.",
            "Remove duplicates/composites or combine variables only with a defensible reason.",
            if (design == "grouped") "Select a grouping variable and review group balance/variance." else "Continue with ungrouped screening steps."
        ),
        stringsAsFactors = FALSE
    )
    rows$check      <- .da_truncate(rows$check)
    rows$nextAction <- .da_truncate(rows$nextAction)
    rows
}

.da_export_codebook <- function(dictionary) {
    data.frame(
        variable = dictionary$variable,
        label = .da_truncate(dictionary$label, 60L),
        type = dictionary$inferred,
        measure = dictionary$measure,
        missing = dictionary$missing,
        missingPct = dictionary$missingPct,
        validRangeCategories = .da_truncate(ifelse(is.na(dictionary$min) & is.na(dictionary$max), dictionary$examples, paste(dictionary$min, dictionary$max, sep = " to ")), 60L),
        warnings = .da_truncate(dictionary$flag),
        recommendedAction = .da_truncate(dictionary$recommendedAction),
        stringsAsFactors = FALSE
    )
}

.da_compact_dictionary <- function(dictionary) {
    data.frame(
        variable = dictionary$variable,
        label = dictionary$label,
        inferred = dictionary$inferred,
        missing = dictionary$missing,
        missingPct = dictionary$missingPct,
        unique = dictionary$unique,
        recommendedAction = dictionary$recommendedAction,
        stringsAsFactors = FALSE
    )
}

.da_missing_patterns <- function(data, vars, max_rows = 50L) {
    scoped <- data[, vars, drop = FALSE]
    miss <- is.na(scoped)
    if (nrow(miss) == 0L || !any(miss))
        return(data.frame(pattern = character(), n = integer(), percent = numeric(), rows = character(), stringsAsFactors = FALSE))
    patterns_all <- apply(miss, 1L, function(row) {
        missing <- vars[row]
        if (length(missing) == 0L) "(complete)" else paste(missing, collapse = " + ")
    })
    patterns <- patterns_all[patterns_all != "(complete)"]
    tab <- sort(table(patterns), decreasing = TRUE)
    rows <- lapply(names(tab), function(pattern) {
        idx <- which(patterns_all == pattern)
        data.frame(pattern = pattern, n = as.integer(tab[[pattern]]), percent = .da_pct(tab[[pattern]], nrow(data)), rows = paste(utils::head(idx, max_rows), collapse = ", "), stringsAsFactors = FALSE)
    })
    do.call(rbind.data.frame, rows)
}

# Little's chi-square test of the MCAR assumption for numeric variables. The
# multivariate-normal mean and covariance are estimated by EM so incomplete
# rows contribute to the test rather than being discarded.
.da_little_mcar <- function(data, vars, dictionary) {
    empty_result <- function(message, n = nrow(data), variables = 0L) {
        data.frame(
            chiSquare = NA_real_, df = NA_integer_, p = NA_real_, n = as.integer(n),
            variables = as.integer(variables), interpretation = message,
            stringsAsFactors = FALSE
        )
    }

    numeric_vars <- vars[vapply(vars, function(v) {
        z <- data[[v]]
        if (inherits(z, c("Date", "POSIXct", "POSIXt")))
            return(FALSE)
        valid <- sum(!is.na(z))
        valid > 0L && length(.da_numeric_values(z)) == valid
    }, logical(1))]
    # Unique-valued numeric measurements can be conservatively inferred as IDs
    # elsewhere in the audit. Exclude them here only when their name also looks
    # like an identifier, so ordinary continuous outcomes remain testable.
    id_like <- grepl("(^|[_. -])(id|identifier|case[_. -]*(id|number|no))($|[_. -])", numeric_vars, ignore.case = TRUE)
    numeric_vars <- numeric_vars[!id_like]
    if (length(numeric_vars) < 2L)
        return(empty_result("Little's test requires at least two numeric variables.", variables = length(numeric_vars)))

    x <- as.data.frame(lapply(data[, numeric_vars, drop = FALSE], function(z) {
        suppressWarnings(as.numeric(as.character(z)))
    }))
    keep_rows <- rowSums(!is.na(x)) > 0L
    x <- as.matrix(x[keep_rows, , drop = FALSE])
    n <- nrow(x)
    p <- ncol(x)
    if (n < 3L || !anyNA(x))
        return(empty_result(if (anyNA(x)) "Too few usable cases for Little's test." else "No missing numeric values; Little's test is not needed.", n, p))

    keep_cols <- colSums(!is.na(x)) >= 2L
    x <- x[, keep_cols, drop = FALSE]
    p <- ncol(x)
    if (p < 2L)
        return(empty_result("Too few numeric variables with usable observations for Little's test.", n, p))

    center <- colMeans(x, na.rm = TRUE)
    scale <- vapply(seq_len(p), function(j) stats::sd(x[, j], na.rm = TRUE), numeric(1))
    keep_cols <- is.finite(scale) & scale > 0
    x <- x[, keep_cols, drop = FALSE]
    center <- center[keep_cols]
    scale <- scale[keep_cols]
    p <- ncol(x)
    if (p < 2L)
        return(empty_result("Little's test needs at least two non-constant numeric variables.", n, p))
    x <- sweep(sweep(x, 2L, center, "-"), 2L, scale, "/")

    em <- .da_mvn_em(x)
    if (!isTRUE(em$converged))
        return(empty_result("Little's test could not be estimated reliably for these data.", n, p))

    observed <- !is.na(x)
    pattern_key <- apply(observed, 1L, paste0, collapse = "")
    patterns <- split(seq_len(n), pattern_key)
    statistic <- 0
    df_sum <- 0L
    usable_patterns <- 0L
    for (idx in patterns) {
        obs <- which(observed[idx[1L], ])
        if (length(obs) == 0L)
            next
        pattern_mean <- colMeans(x[idx, obs, drop = FALSE])
        delta <- pattern_mean - em$mean[obs]
        inv <- .da_safe_inverse(em$sigma[obs, obs, drop = FALSE])
        if (is.null(inv))
            next
        statistic <- statistic + length(idx) * drop(t(delta) %*% inv %*% delta)
        df_sum <- df_sum + length(obs)
        usable_patterns <- usable_patterns + 1L
    }
    df <- as.integer(df_sum - p)
    if (!is.finite(statistic) || df <= 0L || usable_patterns < 2L)
        return(empty_result("There are not enough distinct estimable missing-data patterns for Little's test.", n, p))

    p_value <- stats::pchisq(statistic, df = df, lower.tail = FALSE)
    interpretation <- if (p_value < .05) {
        "Evidence against MCAR (p < .05); investigate the missingness mechanism and consider sensitivity analyses."
    } else {
        "No evidence against MCAR (p >= .05); this does not prove that data are MCAR."
    }
    data.frame(
        chiSquare = statistic, df = df, p = p_value, n = n,
        variables = p, interpretation = interpretation, stringsAsFactors = FALSE
    )
}

.da_safe_inverse <- function(x, tolerance = 1e-10) {
    x <- as.matrix(x)
    if (nrow(x) == 1L) {
        if (!is.finite(x[1L, 1L]) || x[1L, 1L] <= tolerance)
            return(NULL)
        return(matrix(1 / x[1L, 1L], 1L, 1L))
    }
    out <- tryCatch(solve(x), error = function(e) NULL)
    if (!is.null(out) && all(is.finite(out)))
        return(out)
    eig <- tryCatch(eigen((x + t(x)) / 2, symmetric = TRUE), error = function(e) NULL)
    if (is.null(eig) || max(eig$values) <= 0)
        return(NULL)
    keep <- eig$values > max(eig$values) * tolerance
    if (!any(keep))
        return(NULL)
    tcrossprod(sweep(eig$vectors[, keep, drop = FALSE], 2L, sqrt(eig$values[keep]), "/"))
}

.da_mvn_em <- function(x, max_iter = 200L, tolerance = 1e-7) {
    n <- nrow(x)
    p <- ncol(x)
    mu <- colMeans(x, na.rm = TRUE)
    filled <- x
    for (j in seq_len(p))
        filled[is.na(filled[, j]), j] <- mu[j]
    sigma <- stats::cov(filled)
    if (p == 1L)
        sigma <- matrix(sigma, 1L, 1L)
    sigma <- sigma + diag(1e-6, p)
    patterns <- split(seq_len(n), apply(!is.na(x), 1L, paste0, collapse = ""))

    for (iteration in seq_len(max_iter)) {
        sum_x <- numeric(p)
        sum_xx <- matrix(0, p, p)
        ok <- TRUE
        for (idx in patterns) {
            obs <- which(!is.na(x[idx[1L], ]))
            mis <- setdiff(seq_len(p), obs)
            for (i in idx) {
                expected <- numeric(p)
                expected[obs] <- x[i, obs]
                conditional_cov <- matrix(0, p, p)
                if (length(mis) > 0L) {
                    inv <- .da_safe_inverse(sigma[obs, obs, drop = FALSE])
                    if (is.null(inv)) {
                        ok <- FALSE
                        break
                    }
                    beta <- sigma[mis, obs, drop = FALSE] %*% inv
                    expected[mis] <- mu[mis] + drop(beta %*% (x[i, obs] - mu[obs]))
                    conditional_cov[mis, mis] <- sigma[mis, mis, drop = FALSE] - beta %*% sigma[obs, mis, drop = FALSE]
                }
                sum_x <- sum_x + expected
                sum_xx <- sum_xx + tcrossprod(expected) + conditional_cov
            }
            if (!ok)
                break
        }
        if (!ok)
            return(list(converged = FALSE))
        new_mu <- sum_x / n
        new_sigma <- sum_xx / n - tcrossprod(new_mu)
        new_sigma <- (new_sigma + t(new_sigma)) / 2 + diag(1e-8, p)
        change <- max(abs(new_mu - mu), abs(new_sigma - sigma))
        mu <- new_mu
        sigma <- new_sigma
        if (is.finite(change) && change < tolerance)
            return(list(mean = mu, sigma = sigma, converged = TRUE))
    }
    list(mean = mu, sigma = sigma, converged = FALSE)
}

.da_parse_range_rules <- function(text, names) {
    text <- as.character(text %||% "")
    parts <- unlist(strsplit(text, "[\n;]+"))
    parts <- trimws(parts)
    parts <- parts[nzchar(parts)]
    rules <- list()
    for (part in parts) {
        bits <- strsplit(part, "=", fixed = TRUE)[[1]]
        if (length(bits) != 2L)
            next
        variable <- trimws(bits[1])
        rule <- trimws(bits[2])
        if (!variable %in% names)
            next
        rules[[length(rules) + 1L]] <- list(variable = variable, rule = rule)
    }
    rules
}

.da_format_rule <- function(rule_text) {
    if (grepl(":", rule_text, fixed = TRUE)) {
        lim <- suppressWarnings(as.numeric(strsplit(rule_text, ":", fixed = TRUE)[[1]]))
        if (length(lim) == 2L && all(!is.na(lim)))
            return(paste(min(lim), "to", max(lim)))
    }
    allowed <- trimws(unlist(strsplit(rule_text, "[|,]")))
    paste(allowed, collapse = ", ")
}

.da_observed_range <- function(x) {
    num <- suppressWarnings(as.numeric(as.character(x[!is.na(x)])))
    if (length(num) > 0L && !any(is.na(num))) {
        paste(min(num), "to", max(num))
    } else {
        vals <- sort(unique(as.character(x[!is.na(x)])))
        paste(utils::head(vals, 8L), collapse = ", ")
    }
}

.da_range_rule_flags <- function(data, vars, opts) {
    rules <- .da_parse_range_rules(opts$rangeRules, names(data))
    empty <- data.frame(variable = character(), validRange = character(), observedRange = character(), rows = character(), outOfRangeValues = character(), n = integer(), action = character(), stringsAsFactors = FALSE)
    if (length(rules) == 0L)
        return(empty)
    rows <- list()
    for (rule in rules) {
        v <- rule$variable
        if (!v %in% vars)
            next
        x <- data[[v]]
        rule_text <- rule$rule
        bad <- rep(FALSE, length(x))
        if (grepl(":", rule_text, fixed = TRUE)) {
            lim <- suppressWarnings(as.numeric(strsplit(rule_text, ":", fixed = TRUE)[[1]]))
            if (length(lim) == 2L && all(!is.na(lim))) {
                num <- suppressWarnings(as.numeric(as.character(x)))
                bad <- !is.na(num) & (num < min(lim) | num > max(lim))
            }
        } else {
            allowed <- trimws(unlist(strsplit(rule_text, "[|,]")))
            bad <- !is.na(x) & !as.character(x) %in% allowed
        }
        idx <- which(bad)
        if (length(idx) > 0L) {
            rows[[length(rows) + 1L]] <- data.frame(
                variable = v,
                validRange = .da_format_rule(rule_text),
                observedRange = .da_observed_range(x),
                rows = .da_case_values(data, idx, opts$caseID),
                outOfRangeValues = paste(utils::head(unique(as.character(x[idx])), 8L), collapse = ", "),
                n = length(idx),
                action = "Check source data and recode impossible values or set justified missing codes to missing.",
                stringsAsFactors = FALSE
            )
        }
    }
    if (length(rows) == 0L)
        return(empty)
    do.call(rbind.data.frame, rows)
}

.da_duplicate_review <- function(data, vars, case_id) {
    rows <- list()
    scoped <- data[, vars, drop = FALSE]
    dup_idx <- which(duplicated(scoped) | duplicated(scoped, fromLast = TRUE))
    if (length(dup_idx) > 0L) {
        rows[[length(rows) + 1L]] <- data.frame(
            issue = "Fully duplicated selected rows",
            rows = paste(utils::head(dup_idx, 30L), collapse = ", "),
            n = length(dup_idx),
            details = "Rows match across the selected variables.",
            action = "Check whether these are true repeated observations or accidental duplicate cases.",
            stringsAsFactors = FALSE
        )
    }
    if (!is.null(case_id) && length(case_id) == 1L && case_id %in% names(data)) {
        ids <- as.character(data[[case_id]])
        repeated <- which(!is.na(ids) & nzchar(ids) & (duplicated(ids) | duplicated(ids, fromLast = TRUE)))
        if (length(repeated) > 0L) {
            rows[[length(rows) + 1L]] <- data.frame(
                issue = "Repeated case ID",
                rows = paste(utils::head(paste0(repeated, "=", ids[repeated]), 30L), collapse = ", "),
                n = length(repeated),
                details = paste(length(unique(ids[repeated])), "case ID value(s) appear more than once."),
                action = "Resolve repeated IDs before case-level analyses.",
                stringsAsFactors = FALSE
            )
        }
    }
    if (length(rows) == 0L)
        return(data.frame(issue = character(), rows = character(), n = integer(), details = character(), action = character(), stringsAsFactors = FALSE))
    do.call(rbind.data.frame, rows)
}

.da_survey_vars <- function(vars, dictionary) {
    keep <- dictionary$variable[dictionary$inferred %in% c("Ordinal", "Possible scale item")]
    intersect(vars, keep)
}

.da_response_flags_impl <- function(data, vars, dictionary, case_id, issue_straight, action_straight, issue_variation, action_variation) {
    survey_vars <- .da_survey_vars(vars, dictionary)
    if (length(survey_vars) < 3L)
        return(data.frame(row = character(), issue = character(), variables = character(), details = character(), action = character(), stringsAsFactors = FALSE))
    scoped <- data[, survey_vars, drop = FALSE]
    rows <- list()
    for (i in seq_len(nrow(scoped))) {
        values <- as.character(unlist(scoped[i, , drop = TRUE], use.names = FALSE))
        values <- values[!is.na(values) & nzchar(values)]
        if (length(values) < 3L)
            next
        prop_common <- max(table(values)) / length(values)
        unique_n <- length(unique(values))
        if (prop_common >= .90 || unique_n == 1L) {
            rows[[length(rows) + 1L]] <- data.frame(
                row = .da_case_values(data, i, case_id),
                issue = issue_straight,
                variables = paste(survey_vars, collapse = ", "),
                details = sprintf("%.0f%% of available survey-like responses use the same value.", 100 * prop_common),
                action = action_straight,
                stringsAsFactors = FALSE
            )
        } else if (unique_n <= 2L && length(values) >= 6L) {
            rows[[length(rows) + 1L]] <- data.frame(
                row = .da_case_values(data, i, case_id),
                issue = issue_variation,
                variables = paste(survey_vars, collapse = ", "),
                details = paste(unique_n, "unique response values across", length(values), "survey-like items."),
                action = action_variation,
                stringsAsFactors = FALSE
            )
        }
    }
    if (length(rows) == 0L)
        return(data.frame(row = character(), issue = character(), variables = character(), details = character(), action = character(), stringsAsFactors = FALSE))
    do.call(rbind.data.frame, rows)
}

.da_survey_response_flags <- function(data, vars, dictionary, case_id) {
    .da_response_flags_impl(
        data, vars, dictionary, case_id,
        issue_straight  = "Possible straight-lining",
        action_straight = "Review whether this response pattern is plausible before scoring survey scales.",
        issue_variation  = "Low response variation",
        action_variation = "Check whether low variation is expected for this respondent."
    )
}

.da_attention_check_flags <- function(data, vars, dictionary, case_id) {
    .da_response_flags_impl(
        data, vars, dictionary, case_id,
        issue_straight  = "Possible B-lining / straight-lining",
        action_straight = "Review this respondent before scoring scales; decide whether to retain, exclude, or run sensitivity checks.",
        issue_variation  = "Low response variation",
        action_variation = "Check whether this response pattern is plausible or indicates inattentive responding."
    )
}

.da_metadata_suggestions <- function(dictionary) {
    rows <- list()
    for (i in seq_len(nrow(dictionary))) {
        measure <- tolower(dictionary$measure[i])
        inferred <- dictionary$inferred[i]
        suggestion <- ""
        reason <- ""
        if (measure == "continuous" && inferred %in% c("Dichotomous/binary", "Ordinal", "Nominal categorical", "Possible scale item")) {
            suggestion <- if (inferred == "Nominal categorical") "Change measurement level to nominal." else "Change measurement level to ordinal."
            reason <- "Values look categorical or scale-like rather than continuous."
        } else if (measure %in% c("nominal", "ordinal") && inferred %in% c("Continuous numeric", "Integer/count")) {
            suggestion <- "Consider changing measurement level to continuous."
            reason <- "Values look numeric with enough unique values for continuous analysis."
        }
        if (nzchar(suggestion)) {
            rows[[length(rows) + 1L]] <- data.frame(
                variable = dictionary$variable[i],
                currentMeasure = dictionary$measure[i],
                inferredType = inferred,
                suggestion = suggestion,
                reason = reason,
                stringsAsFactors = FALSE
            )
        }
    }
    if (length(rows) == 0L)
        return(data.frame(variable = character(), currentMeasure = character(), inferredType = character(), suggestion = character(), reason = character(), stringsAsFactors = FALSE))
    do.call(rbind.data.frame, rows)
}

.da_descriptives <- function(data, vars, dictionary) {
    keep <- dictionary$variable[dictionary$inferred %in% c("Continuous numeric", "Integer/count")]
    keep <- intersect(vars, keep)
    rows <- lapply(keep, function(v) {
        num <- .da_numeric_values(data[[v]])
        data.frame(
            variable = v,
            n = length(num),
            missing = sum(is.na(data[[v]])),
            mean = .da_mean(num),
            sd = .da_sd(num),
            median = if (length(num) > 0L) stats::median(num) else NA_real_,
            min = if (length(num) > 0L) min(num) else NA_real_,
            max = if (length(num) > 0L) max(num) else NA_real_,
            skewness = .da_skew(num),
            kurtosis = .da_kurtosis(num),
            stringsAsFactors = FALSE
        )
    })
    if (length(rows) == 0L)
        return(data.frame(variable = character(), n = integer(), missing = integer(), mean = numeric(), sd = numeric(), median = numeric(), min = numeric(), max = numeric(), skewness = numeric(), kurtosis = numeric(), stringsAsFactors = FALSE))
    do.call(rbind.data.frame, rows)
}

.da_frequencies <- function(data, vars, dictionary) {
    keep <- dictionary$variable[dictionary$inferred %in% c("Dichotomous/binary", "Ordinal", "Nominal categorical", "Possible scale item")]
    keep <- intersect(vars, keep)
    rows <- list()
    for (v in keep) {
        x <- data[[v]]
        tab <- sort(table(x, useNA = "no"), decreasing = TRUE)
        valid <- sum(tab)
        total <- length(x)
        for (cat in names(tab)) {
            rows[[length(rows) + 1L]] <- data.frame(
                variable = v,
                category = cat,
                n = as.integer(tab[[cat]]),
                validPct = .da_pct(tab[[cat]], valid),
                totalPct = .da_pct(tab[[cat]], total),
                stringsAsFactors = FALSE
            )
        }
    }
    if (length(rows) == 0L)
        return(data.frame(variable = character(), category = character(), n = integer(), validPct = numeric(), totalPct = numeric(), stringsAsFactors = FALSE))
    do.call(rbind.data.frame, rows)
}

# Raw-variable normality screening; this is intentionally framed as screening text.
.da_normality <- function(data, vars, dictionary, opts) {
    desc <- .da_descriptives(data, vars, dictionary)
    if (nrow(desc) == 0L)
        return(data.frame(variable = character(), n = integer(), mean = numeric(), sd = numeric(), median = numeric(), skewness = numeric(), kurtosis = numeric(), w = numeric(), p = numeric(), flag = character(), stringsAsFactors = FALSE))
    rows <- lapply(seq_len(nrow(desc)), function(i) {
        v <- desc$variable[i]
        num <- .da_numeric_values(data[[v]])
        sw <- .da_shapiro(num)
        flag <- .da_normality_flag(desc$skewness[i], desc$kurtosis[i], sw$p, opts)
        data.frame(
            variable = v, n = desc$n[i], mean = desc$mean[i], sd = desc$sd[i],
            median = desc$median[i], skewness = desc$skewness[i], kurtosis = desc$kurtosis[i],
            w = sw$w, p = sw$p, flag = flag, stringsAsFactors = FALSE
        )
    })
    do.call(rbind.data.frame, rows)
}

.da_shapiro <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) < 3L || length(x) > 5000L || length(unique(x)) < 3L)
        return(list(w = NA_real_, p = NA_real_))
    out <- tryCatch(stats::shapiro.test(x), error = function(e) NULL)
    if (is.null(out))
        list(w = NA_real_, p = NA_real_)
    else
        list(w = unname(out$statistic), p = out$p.value)
}

.da_normality_flag <- function(skew, kurtosis, p, opts) {
    reasons <- character()
    if (!is.na(p) && p < .05)
        reasons <- c(reasons, "Shapiro-Wilk p < .05")
    if (!is.na(skew) && abs(skew) > opts$skewThreshold)
        reasons <- c(reasons, "skewness above threshold")
    if (!is.na(kurtosis) && abs(kurtosis) > opts$kurtosisThreshold)
        reasons <- c(reasons, "kurtosis above threshold")
    if (length(reasons) == 0L)
        "No strong raw-variable normality warning"
    else
        paste("Evidence of deviation from normality:", paste(reasons, collapse = "; "))
}

.da_outliers <- function(data, vars, dictionary, opts) {
    keep <- dictionary$variable[dictionary$inferred %in% c("Continuous numeric", "Integer/count")]
    keep <- intersect(vars, keep)
    rows <- lapply(keep, function(v) {
        raw <- data[[v]]
        num_all <- suppressWarnings(as.numeric(as.character(raw)))
        ok <- !is.na(num_all)
        num <- num_all[ok]
        if (length(num) < 2L || .da_sd(num) == 0)
            return(data.frame(variable = v, zOutliers = 0L, iqrOutliers = 0L, low = NA_real_, high = NA_real_, cases = "", flag = "Not enough variation", stringsAsFactors = FALSE))
        z <- abs((num - mean(num)) / stats::sd(num))
        qs <- stats::quantile(num, c(.25, .75), names = FALSE, na.rm = TRUE)
        iqr <- diff(qs)
        iqr_flag <- num < qs[1] - opts$iqrMultiplier * iqr | num > qs[2] + opts$iqrMultiplier * iqr
        z_flag <- z > opts$zThreshold
        idx <- which(ok)[z_flag | iqr_flag]
        cases <- .da_case_values(data, idx, opts$caseID)
        flag <- if (length(idx) > 0L) "Potential outliers detected" else "No outliers by selected rules"
        data.frame(variable = v, zOutliers = sum(z_flag), iqrOutliers = sum(iqr_flag), low = min(num), high = max(num), cases = cases, flag = flag, stringsAsFactors = FALSE)
    })
    if (length(rows) == 0L)
        return(data.frame(variable = character(), zOutliers = integer(), iqrOutliers = integer(), low = numeric(), high = numeric(), cases = character(), flag = character(), stringsAsFactors = FALSE))
    do.call(rbind.data.frame, rows)
}

.da_case_values <- function(data, idx, case_id) {
    if (length(idx) == 0L)
        return("")
    idx <- utils::head(idx, 10L)
    if (!is.null(case_id) && length(case_id) == 1L && case_id %in% names(data))
        paste(as.character(data[[case_id]][idx]), collapse = ", ")
    else
        paste(idx, collapse = ", ")
}

.da_levene <- function(data, vars, dictionary, opts) {
    group <- opts$group
    if (is.null(group) || length(group) != 1L || !group %in% names(data))
        return(.da_message_row("Select a grouping variable to run Levene's test.", "homogeneity"))
    outcomes <- intersect(vars, dictionary$variable[dictionary$inferred %in% c("Continuous numeric", "Integer/count")])
    if (length(outcomes) == 0L)
        return(.da_message_row("No suitable continuous outcome variables were found.", "homogeneity"))
    rows <- lapply(outcomes, function(v) .da_levene_one(data[[v]], data[[group]], v, group))
    do.call(rbind.data.frame, rows)
}

.da_levene_one <- function(x, g, outcome, group_name) {
    x <- suppressWarnings(as.numeric(as.character(x)))
    ok <- !is.na(x) & !is.na(g)
    x <- x[ok]
    g <- factor(g[ok])
    if (length(x) < 3L || nlevels(g) < 2L)
        return(data.frame(outcome = outcome, group = group_name, statistic = NA_real_, df = "", p = NA_real_, groupSDs = "", varRatio = NA_real_, flag = "Not enough data or groups", stringsAsFactors = FALSE))
    med <- stats::ave(x, g, FUN = stats::median)
    fit <- tryCatch(stats::aov(abs(x - med) ~ g), error = function(e) NULL)
    if (is.null(fit))
        return(data.frame(outcome = outcome, group = group_name, statistic = NA_real_, df = "", p = NA_real_, groupSDs = "", varRatio = NA_real_, flag = "Could not compute Levene's test", stringsAsFactors = FALSE))
    sm <- summary(fit)[[1]]
    stat <- sm[1, "F value"]
    p <- sm[1, "Pr(>F)"]
    df <- paste(sm[1, "Df"], sm[2, "Df"], sep = ", ")
    sds <- tapply(x, g, stats::sd, na.rm = TRUE)
    group_variances <- sds^2
    ratio <- if (any(group_variances == 0, na.rm = TRUE)) Inf else max(group_variances, na.rm = TRUE) / min(group_variances, na.rm = TRUE)
    flag <- if (!is.na(p) && p < .05) "Variances may differ; consider Welch's correction or robust alternatives." else "No strong variance warning"
    data.frame(outcome = outcome, group = group_name, statistic = stat, df = df, p = p, groupSDs = paste(names(sds), round(sds, 3), sep = "=", collapse = "; "), varRatio = ratio, flag = flag, stringsAsFactors = FALSE)
}

.da_linearity <- function(data, opts) {
    outcome <- opts$outcome
    preds <- as.character(opts$predictors %||% character())
    preds <- preds[preds %in% names(data)]
    if (is.null(outcome) || length(outcome) != 1L || !outcome %in% names(data) || length(preds) == 0L)
        return(data.frame(outcome = character(), predictor = character(), pearson = numeric(), spearman = numeric(), n = integer(), flag = character(), stringsAsFactors = FALSE))
    y <- suppressWarnings(as.numeric(as.character(data[[outcome]])))
    rows <- lapply(preds, function(p) {
        x <- suppressWarnings(as.numeric(as.character(data[[p]])))
        ok <- !is.na(x) & !is.na(y)
        n <- sum(ok)
        if (n < 3L)
            return(data.frame(outcome = outcome, predictor = p, pearson = NA_real_, spearman = NA_real_, n = n, flag = "Not enough pairwise data", stringsAsFactors = FALSE))
        pear <- suppressWarnings(stats::cor(x[ok], y[ok], method = "pearson"))
        spear <- suppressWarnings(stats::cor(x[ok], y[ok], method = "spearman"))
        diff <- abs(pear - spear)
        flag <- if (!is.na(diff) && diff > .20) "Pearson and Spearman differ; inspect scatterplot for non-linearity or outliers." else "No strong linearity warning"
        data.frame(outcome = outcome, predictor = p, pearson = pear, spearman = spear, n = n, flag = flag, stringsAsFactors = FALSE)
    })
    do.call(rbind.data.frame, rows)
}

# VIF is computed from the predictor model matrix, safely expanding factors.
.da_vif <- function(data, opts) {
    preds <- as.character(opts$predictors %||% character())
    preds <- preds[preds %in% names(data)]
    if (length(preds) < 2L)
        return(data.frame(predictor = character(), vif = numeric(), tolerance = numeric(), flag = character(), stringsAsFactors = FALSE))
    df <- data[, preds, drop = FALSE]
    ok <- stats::complete.cases(df)
    df <- df[ok, , drop = FALSE]
    if (nrow(df) < 3L)
        return(data.frame(predictor = "(predictors)", vif = NA_real_, tolerance = NA_real_, flag = "Not enough complete cases for VIF", stringsAsFactors = FALSE))
    mm <- tryCatch(stats::model.matrix(~ ., data = df), error = function(e) NULL)
    if (is.null(mm) || ncol(mm) <= 2L)
        return(data.frame(predictor = "(predictors)", vif = NA_real_, tolerance = NA_real_, flag = "VIF requires at least two usable predictor columns", stringsAsFactors = FALSE))
    mm <- mm[, colnames(mm) != "(Intercept)", drop = FALSE]
    rows <- lapply(seq_len(ncol(mm)), function(i) {
        target <- mm[, i]
        others <- mm[, -i, drop = FALSE]
        if (stats::sd(target) == 0 || ncol(others) == 0L)
            r2 <- NA_real_
        else
            r2 <- tryCatch(summary(stats::lm(target ~ others))$r.squared, error = function(e) NA_real_)
        vif <- if (is.na(r2) || r2 >= 1) Inf else 1 / (1 - r2)
        tol <- if (is.finite(vif) && vif != 0) 1 / vif else 0
        flag <- if (vif >= opts$vifSerious) "serious concern" else if (vif >= opts$vifWarning) "review" else "acceptable"
        data.frame(predictor = colnames(mm)[i], vif = vif, tolerance = tol, flag = flag, stringsAsFactors = FALSE)
    })
    do.call(rbind.data.frame, rows)
}

.da_message_row <- function(message, type) {
    if (type == "homogeneity")
        return(data.frame(outcome = "(not run)", group = "", statistic = NA_real_, df = "", p = NA_real_, groupSDs = "", varRatio = NA_real_, flag = message, stringsAsFactors = FALSE))
    stop("Unknown message row type")
}

.da_missing_text_html <- function(data, vars, missing) {
    total_cells <- nrow(data) * length(vars)
    miss <- sum(missing$missing)
    pct <- .da_pct(miss, total_cells)
    top <- utils::head(missing[order(-missing$missingPct), "variable"], 3L)
    text <- sprintf(
        "The dataset contains %s cases and %s variables. Overall, %.1f%% of cells are missing. The variables with the highest missingness are %s.",
        nrow(data), length(vars), pct, paste(top, collapse = ", ")
    )
    .da_html(text)
}

.da_summary_html <- function(data, vars, overview, dictionary, quality, opts) {
    total_cells <- nrow(data) * length(vars)
    miss <- sum(dictionary$missing)
    pct <- .da_pct(miss, total_cells)
    attention <- unique(quality$variable[quality$severity %in% c("warning", "serious")])
    attention <- attention[attention != "(dataset)"]
    type_count <- function(type) sum(dictionary$inferred == type)
    issues <- if (length(attention) == 0L) "No major variable-level warnings were detected." else paste("Variables needing attention include", paste(utils::head(attention, 6L), collapse = ", "), ".")
    assumptions <- if (isTRUE(opts$includeAssumptions)) "Assumption screening was requested; review the assumption tables as screening information rather than definitive evidence that an analysis is inappropriate." else "Assumption screening was not requested."
    text <- sprintf(
        paste(
            "This dataset contains %s cases and %s variables. Overall, %.1f%% of data cells are missing.",
            "%s",
            "%s variables appear to be IDs, %s appear categorical or binary, %s appear to be possible scale items, %s appear continuous or count-like, and %s appear to be free-text.",
            "%s",
            "Before running inferential statistics, check missing-data handling, coding consistency, sparse categories, outliers, and whether the selected assumptions apply to the intended model."
        ),
        nrow(data), length(vars), pct, issues,
        type_count("ID variable"),
        sum(dictionary$inferred %in% c("Nominal categorical", "Dichotomous/binary", "Ordinal")),
        type_count("Possible scale item"),
        sum(dictionary$inferred %in% c("Continuous numeric", "Integer/count")),
        type_count("Free-text"),
        assumptions
    )
    .da_html(text)
}

.da_truncate <- function(x, n = 90L) {
    x <- as.character(x)
    ifelse(nchar(x) > n, paste0(substr(x, 1L, n - 1L), "\u2026"), x)
}

.da_mean <- function(x) if (length(x) > 0L) mean(x) else NA_real_
.da_sd <- function(x) if (length(x) > 1L) stats::sd(x) else NA_real_

.da_skew <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) < 3L)
        return(NA_real_)
    s <- stats::sd(x)
    if (is.na(s) || s == 0)
        return(NA_real_)
    mean((x - mean(x))^3) / s^3
}

.da_kurtosis <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) < 4L)
        return(NA_real_)
    s <- stats::sd(x)
    if (is.na(s) || s == 0)
        return(NA_real_)
    mean((x - mean(x))^4) / s^4 - 3
}

.da_plot_state <- function(data, vars, dictionary, opts) {
    list(
        data = data,
        vars = vars,
        dictionary = dictionary,
        graphMaxVars = opts$graphMaxVars %||% 9L,
        graphTopCategories = opts$graphTopCategories %||% 12L,
        moderateMissing = opts$moderateMissing %||% 5,
        highMissing = opts$highMissing %||% 20,
        includeNormalCurveHistograms = isTRUE(opts$includeNormalCurveHistograms)
    )
}

.da_plot_message <- function(message) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, message, cex = 1.05)
    invisible(TRUE)
}

.da_plot_numeric_vars <- function(state) {
    dictionary <- state$dictionary
    vars <- dictionary$variable[dictionary$inferred %in% c("Continuous numeric", "Integer/count", "Possible scale item")]
    vars <- intersect(state$vars, vars)
    vars[vapply(vars, function(v) length(.da_plot_numeric_values(state$data[[v]])) > 0L, logical(1))]
}

.da_plot_categorical_vars <- function(state) {
    dictionary <- state$dictionary
    vars <- dictionary$variable[dictionary$inferred %in% c("Dichotomous/binary", "Ordinal", "Nominal categorical", "Possible scale item")]
    vars <- intersect(state$vars, vars)
    vars[vapply(vars, function(v) length(stats::na.omit(state$data[[v]])) > 0L, logical(1))]
}

.da_plot_numeric_values <- function(x) {
    if (inherits(x, c("Date", "POSIXct", "POSIXt")))
        return(numeric())
    out <- suppressWarnings(as.numeric(as.character(x)))
    out <- out[is.finite(out)]
    out
}

.da_panel_layout <- function(n) {
    if (n <= 1L)
        return(c(1L, 1L))
    cols <- ceiling(sqrt(n))
    rows <- ceiling(n / cols)
    c(rows, cols)
}

.da_render_missing_plot <- function(state) {
    if (is.null(state))
        return(.da_plot_message("No plot state is available."))
    vars <- state$vars
    if (length(vars) == 0L)
        return(.da_plot_message("No variables are available for the missing-data plot."))

    missing_pct <- vapply(vars, function(v) .da_pct(sum(is.na(state$data[[v]])), length(state$data[[v]])), numeric(1))
    ord <- order(missing_pct, decreasing = TRUE)
    max_vars <- min(length(ord), state$graphMaxVars)
    ord <- ord[seq_len(max_vars)]
    values <- missing_pct[ord]
    labels <- vars[ord]

    old <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old), add = TRUE)
    graphics::par(mar = c(4, 9, 3, 2))
    colours <- ifelse(values >= state$highMissing, "#C0392B", ifelse(values >= state$moderateMissing, "#E6A23C", "#6BAED6"))
    positions <- graphics::barplot(
        rev(values),
        horiz = TRUE,
        names.arg = rev(labels),
        las = 1,
        xlab = "Missing data (%)",
        col = rev(colours),
        border = "#2B6C9E",
        main = "Variables with the most missing data",
        xlim = c(0, max(100, values, na.rm = TRUE))
    )
    graphics::text(rev(values), positions, labels = sprintf(" %.1f%%", rev(values)), pos = 4, cex = 0.8, xpd = NA)
    graphics::abline(v = c(state$moderateMissing, state$highMissing), lty = 2, col = c("#E6A23C", "#C0392B"))
    graphics::legend("bottomright", legend = c(sprintf("%.1f%% moderate", state$moderateMissing), sprintf("%.1f%% high", state$highMissing)), lty = 2, col = c("#E6A23C", "#C0392B"), bty = "n", cex = 0.8)
    invisible(TRUE)
}

.da_render_missing_pattern_plot <- function(state) {
    if (is.null(state))
        return(.da_plot_message("No plot state is available."))
    vars <- state$vars
    if (length(vars) == 0L)
        return(.da_plot_message("No variables are available for the missing-data pattern chart."))

    missing <- is.na(state$data[, vars, drop = FALSE])
    n_cases <- nrow(missing)
    if (n_cases == 0L)
        return(.da_plot_message("No cases are available for the missing-data pattern chart."))

    keys <- apply(missing, 1L, function(row) paste0(as.integer(row), collapse = ""))
    counts <- sort(table(keys), decreasing = TRUE)
    pattern_keys <- names(counts)
    pattern_matrix <- do.call(rbind, lapply(pattern_keys, function(key) {
        as.integer(strsplit(key, "", fixed = TRUE)[[1L]])
    }))
    if (is.null(dim(pattern_matrix)))
        pattern_matrix <- matrix(pattern_matrix, nrow = 1L)
    colnames(pattern_matrix) <- vars
    n_patterns <- nrow(pattern_matrix)
    percentages <- 100 * as.numeric(counts) / n_cases
    row_labels <- sprintf(
        "Pattern %d \u2013 n=%d (%.1f%%)",
        seq_len(n_patterns), as.integer(counts), percentages
    )

    old <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old), add = TRUE)
    left_margin <- min(20, max(8, max(nchar(row_labels)) * 0.55))
    bottom_margin <- min(14, max(7, max(nchar(vars)) * 0.45))
    graphics::par(mar = c(bottom_margin, left_margin, 4, 2), xpd = NA)

    # image() draws bottom-up, so reverse the rows to keep Pattern 1 at top.
    z <- t(pattern_matrix[n_patterns:1L, , drop = FALSE])
    graphics::image(
        x = seq_len(length(vars)), y = seq_len(n_patterns), z = z,
        col = c("#F1F3F5", "#D95F59"), breaks = c(-0.5, 0.5, 1.5),
        axes = FALSE, xlab = "", ylab = "", main = "Missing Data Pattern Chart",
        useRaster = TRUE
    )
    graphics::axis(1, at = seq_along(vars), labels = vars, las = 2, tick = FALSE, cex.axis = 0.8)
    graphics::axis(2, at = seq_len(n_patterns), labels = rev(row_labels), las = 1, tick = FALSE, cex.axis = 0.8)
    if (length(vars) > 1L)
        graphics::abline(v = seq(1.5, length(vars) - 0.5, by = 1), col = "white", lwd = 1)
    if (n_patterns > 1L)
        graphics::abline(h = seq(1.5, n_patterns - 0.5, by = 1), col = "white", lwd = 1)
    graphics::box(col = "#D9DDE2")
    graphics::legend(
        "bottom", inset = c(0, -0.28), horiz = TRUE,
        legend = c("Observed", "Missing"), fill = c("#F1F3F5", "#D95F59"),
        border = c("#D9DDE2", "#D95F59"), bty = "n", xpd = NA, cex = 0.85
    )
    invisible(TRUE)
}

.da_render_numeric_plot <- function(state) {
    if (is.null(state))
        return(.da_plot_message("No plot state is available."))
    vars <- utils::head(.da_plot_numeric_vars(state), state$graphMaxVars)
    if (length(vars) == 0L)
        return(.da_plot_message("No numeric or scale-like variables are available for histograms."))

    layout <- .da_panel_layout(length(vars))
    old <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old), add = TRUE)
    graphics::par(mfrow = layout, mar = c(4, 4, 3, 1))

    for (v in vars) {
        x <- .da_plot_numeric_values(state$data[[v]])
        if (length(unique(x)) < 2L) {
            graphics::plot.new()
            graphics::title(main = v)
            graphics::text(0.5, 0.5, "Not enough variation")
        } else {
            h <- graphics::hist(x, main = v, xlab = "", col = "#9ECAE1", border = "white", freq = !isTRUE(state$includeNormalCurveHistograms))
            if (isTRUE(state$includeNormalCurveHistograms) && length(unique(x)) >= 3L && .da_sd(x) > 0) {
                xx <- seq(min(x), max(x), length.out = 100L)
                graphics::lines(xx, stats::dnorm(xx, mean = mean(x), sd = stats::sd(x)), col = "#B2182B", lwd = 2)
            }
            graphics::rug(x, col = "#2B6C9E")
        }
    }
    invisible(TRUE)
}

.da_render_box_plot <- function(state) {
    if (is.null(state))
        return(.da_plot_message("No plot state is available."))
    vars <- utils::head(.da_plot_numeric_vars(state), state$graphMaxVars)
    if (length(vars) == 0L)
        return(.da_plot_message("No numeric or scale-like variables are available for boxplots."))

    layout <- .da_panel_layout(length(vars))
    old <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old), add = TRUE)
    graphics::par(mfrow = layout, mar = c(4, 4, 3, 1))

    for (v in vars) {
        x <- .da_plot_numeric_values(state$data[[v]])
        if (length(unique(x)) < 2L) {
            graphics::plot.new()
            graphics::title(main = v)
            graphics::text(0.5, 0.5, "Not enough variation")
        } else {
            graphics::boxplot(x, horizontal = TRUE, main = v, xlab = "", col = "#A1D99B", border = "#2E7D32")
            graphics::stripchart(x, method = "jitter", add = TRUE, pch = 16, col = grDevices::adjustcolor("#1B5E20", alpha.f = 0.35))
        }
    }
    invisible(TRUE)
}

.da_render_categorical_plot <- function(state) {
    if (is.null(state))
        return(.da_plot_message("No plot state is available."))
    vars <- utils::head(.da_plot_categorical_vars(state), state$graphMaxVars)
    if (length(vars) == 0L)
        return(.da_plot_message("No categorical, binary, ordinal, or scale-like variables are available for bar charts."))

    layout <- .da_panel_layout(length(vars))
    old <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old), add = TRUE)
    graphics::par(mfrow = layout, mar = c(5, 4, 3, 1))

    for (v in vars) {
        tab <- sort(table(state$data[[v]], useNA = "no"), decreasing = TRUE)
        tab <- utils::head(tab, state$graphTopCategories)
        if (length(tab) == 0L) {
            graphics::plot.new()
            graphics::title(main = v)
            graphics::text(0.5, 0.5, "No valid categories")
        } else {
            graphics::barplot(tab, main = v, ylab = "n", las = 2, col = "#FDD0A2", border = "#D94801", cex.names = 0.75)
        }
    }
    invisible(TRUE)
}

.da_render_correlation_plot <- function(state) {
    if (is.null(state))
        return(.da_plot_message("No plot state is available."))
    vars <- utils::head(.da_plot_numeric_vars(state), state$graphMaxVars)
    if (length(vars) < 2L)
        return(.da_plot_message("At least two numeric or scale-like variables are needed for a correlation heatmap."))

    df <- as.data.frame(lapply(vars, function(v) .da_plot_numeric_values_with_na(state$data[[v]])))
    names(df) <- vars
    keep <- vapply(df, function(x) sum(is.finite(x)) >= 3L && stats::sd(x, na.rm = TRUE) > 0, logical(1))
    df <- df[, keep, drop = FALSE]
    if (ncol(df) < 2L)
        return(.da_plot_message("At least two numeric variables with variation are needed for a correlation heatmap."))

    mat <- suppressWarnings(stats::cor(df, use = "pairwise.complete.obs"))
    n <- ncol(mat)

    old <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old), add = TRUE)
    graphics::par(mar = c(8, 8, 4, 5))
    cols <- grDevices::colorRampPalette(c("#2166AC", "#F7F7F7", "#B2182B"))(101)
    graphics::image(seq_len(n), seq_len(n), mat[n:1, , drop = FALSE], col = cols, zlim = c(-1, 1), axes = FALSE, xlab = "", ylab = "", main = "Pairwise correlation heatmap")
    graphics::axis(1, at = seq_len(n), labels = colnames(mat), las = 2, cex.axis = 0.75)
    graphics::axis(2, at = seq_len(n), labels = rev(rownames(mat)), las = 2, cex.axis = 0.75)
    graphics::box()
    graphics::legend("right", inset = -0.16, legend = c("1", "0", "-1"), fill = c("#B2182B", "#F7F7F7", "#2166AC"), title = "r", xpd = TRUE, bty = "n")
    invisible(TRUE)
}

.da_render_violin_plot <- function(state) {
    if (is.null(state))
        return(.da_plot_message("No plot state is available."))
    vars <- utils::head(.da_plot_numeric_vars(state), state$graphMaxVars)
    if (length(vars) == 0L)
        return(.da_plot_message("No numeric or scale-like variables are available for violin plots."))

    values <- lapply(vars, function(v) .da_plot_numeric_values(state$data[[v]]))
    names(values) <- vars
    values <- values[vapply(values, function(x) length(unique(x)) >= 2L, logical(1))]
    if (length(values) == 0L)
        return(.da_plot_message("No numeric variables have enough variation for violin plots."))

    old <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old), add = TRUE)
    graphics::par(mar = c(7, 4, 3, 1))
    ylim <- range(unlist(values), na.rm = TRUE)
    graphics::plot(seq_along(values), seq_along(values), type = "n", xaxt = "n", xlab = "", ylab = "Value", ylim = ylim, main = "Violin plots")
    graphics::axis(1, at = seq_along(values), labels = names(values), las = 2, cex.axis = 0.75)
    for (i in seq_along(values)) {
        x <- values[[i]]
        d <- stats::density(x, na.rm = TRUE)
        width <- d$y / max(d$y) * 0.35
        graphics::polygon(c(i - width, rev(i + width)), c(d$x, rev(d$x)), col = "#C6DBEF", border = "#2171B5")
        graphics::points(rep(i, length(x)), x, pch = 16, col = grDevices::adjustcolor("#08306B", alpha.f = 0.25), cex = 0.6)
        graphics::segments(i - 0.25, stats::median(x), i + 0.25, stats::median(x), lwd = 2, col = "#B2182B")
    }
    invisible(TRUE)
}

.da_render_survey_plot <- function(state) {
    if (is.null(state))
        return(.da_plot_message("No plot state is available."))
    vars <- utils::head(.da_survey_vars(state$vars, state$dictionary), state$graphMaxVars)
    if (length(vars) == 0L)
        return(.da_plot_message("No survey-like variables are available for survey plots."))

    values <- lapply(vars, function(v) .da_plot_numeric_values(state$data[[v]]))
    names(values) <- vars
    values <- values[vapply(values, function(x) length(x) >= 3L && length(unique(x)) >= 2L, logical(1))]
    if (length(values) == 0L)
        return(.da_plot_message("No survey-like variables have enough numeric variation for raincloud plots."))

    old <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old), add = TRUE)
    layout <- .da_panel_layout(length(values))
    graphics::par(mfrow = layout, mar = c(4, 4, 3, 1))
    for (v in names(values))
        .da_render_survey_raincloud(values[[v]], v)
    invisible(TRUE)
}

.da_render_survey_raincloud <- function(x, title) {
    x <- x[is.finite(x)]
    rng <- range(x, na.rm = TRUE)
    pad <- max(0.25, diff(rng) * 0.08)
    xlim <- c(rng[1] - pad, rng[2] + pad)
    graphics::plot(
        xlim,
        c(0, 1),
        type = "n",
        yaxt = "n",
        ylab = "",
        xlab = title,
        main = title,
        bty = "l"
    )
    graphics::axis(1)

    d <- tryCatch(stats::density(x, from = xlim[1], to = xlim[2], adjust = 1.15), error = function(e) NULL)
    if (!is.null(d) && max(d$y, na.rm = TRUE) > 0) {
        height <- d$y / max(d$y, na.rm = TRUE) * 0.34
        base <- 0.68
        graphics::polygon(
            c(d$x, rev(d$x)),
            c(rep(base, length(d$x)), rev(base + height)),
            col = "#C6DBEF",
            border = "#303030"
        )
    }

    set.seed(271828)
    jitter_y <- stats::runif(length(x), min = 0.37, max = 0.58)
    jitter_x <- jitter(x, amount = max(0.03, diff(xlim) * 0.004))
    graphics::points(jitter_x, jitter_y, pch = 16, cex = 0.55, col = grDevices::adjustcolor("#2C7BE5", alpha.f = 0.55))

    stats <- grDevices::boxplot.stats(x)$stats
    box_y0 <- 0.39
    box_y1 <- 0.52
    mid_y <- mean(c(box_y0, box_y1))
    graphics::segments(stats[1], mid_y, stats[2], mid_y, col = "#555555")
    graphics::segments(stats[4], mid_y, stats[5], mid_y, col = "#555555")
    graphics::rect(stats[2], box_y0, stats[4], box_y1, border = "#303030", col = grDevices::adjustcolor("#FFFFFF", alpha.f = 0.25))
    graphics::segments(stats[3], box_y0, stats[3], box_y1, lwd = 2, col = "#303030")
    graphics::segments(stats[1], box_y0 + 0.03, stats[1], box_y1 - 0.03, col = "#555555")
    graphics::segments(stats[5], box_y0 + 0.03, stats[5], box_y1 - 0.03, col = "#555555")
}

.da_plot_numeric_values_with_na <- function(x) {
    if (inherits(x, c("Date", "POSIXct", "POSIXt")))
        return(rep(NA_real_, length(x)))
    suppressWarnings(as.numeric(as.character(x)))
}
