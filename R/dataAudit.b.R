dataAuditClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "dataAuditClass",
    inherit = dataAuditBase,
    private = list(
        .run = function() {
            data <- as.data.frame(self$data, stringsAsFactors = FALSE)
            opts <- self$options
            vars <- .da_selected_vars(data, opts$vars)

            if (length(vars) == 0L || ncol(data) == 0L) {
                self$results$summary$setContent(.da_html("No variables are available to audit."))
                self$results$missingText$setContent(.da_html("No missing-data report can be produced because the dataset is empty."))
                return(invisible(NULL))
            }

            audit <- .da_build_audit(data, vars, opts)
            overview <- .da_dataset_overview(data, vars, audit$dictionary, opts)
            missing <- .da_missing_by_variable(data, vars, opts)
            missing_case <- .da_missing_by_case(data, vars, opts$caseID, opts$maxCaseRows)
            quality <- .da_quality_flags(data, vars, audit$dictionary, opts)

            if (opts$includeSummary)
                self$results$summary$setContent(.da_summary_html(data, vars, overview, audit$dictionary, quality, opts))

            if (opts$includeOverview)
                .da_add_rows(self$results$overview, overview)

            if (opts$includeDictionary)
                .da_add_rows(self$results$dictionary, audit$dictionary)

            if (opts$includeMissing) {
                .da_add_rows(self$results$missingByVariable, missing)
                .da_add_rows(self$results$missingByCase, missing_case)
                self$results$missingText$setContent(.da_missing_text_html(data, vars, missing))
            }

            if (opts$includeQuality)
                .da_add_rows(self$results$quality, quality)

            if (opts$includeDescriptives)
                .da_add_rows(self$results$descriptives, .da_descriptives(data, vars, audit$dictionary))

            if (opts$includeFrequencies)
                .da_add_rows(self$results$frequencies, .da_frequencies(data, vars, audit$dictionary))

            if (opts$includeAssumptions) {
                if (opts$normalityChecks) {
                    .da_add_rows(self$results$normality, .da_normality(data, vars, audit$dictionary, opts))
                    self$results$normalityNote$setContent(.da_html(
                        "For regression and ANOVA, normality is usually assessed on model residuals rather than raw variables. These results should be treated as screening information."
                    ))
                }
                if (opts$outlierChecks)
                    .da_add_rows(self$results$outliers, .da_outliers(data, vars, audit$dictionary, opts))
                if (opts$homogeneityChecks)
                    .da_add_rows(self$results$homogeneity, .da_levene(data, vars, audit$dictionary, opts))
                if (opts$linearityChecks)
                    .da_add_rows(self$results$linearity, .da_linearity(data, opts))
                if (opts$multicollinearityChecks)
                    .da_add_rows(self$results$vif, .da_vif(data, opts))
            }
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
        stringsAsFactors = FALSE
    )
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
    vars <- sds^2
    ratio <- if (any(vars == 0, na.rm = TRUE)) Inf else max(vars, na.rm = TRUE) / min(vars, na.rm = TRUE)
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
