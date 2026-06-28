utils::globalVariables("super")

.dataAuditBaseInitialize <- function(options, data = NULL, datasetId = "", analysisId = "", revision = 0) {
    if (!is.null(data) &&
            is.data.frame(data) &&
            length(options$varsRequired) == 0L &&
            ncol(data) > 0L &&
            options$has("vars")) {
        vars <- options$option("vars")
        vars$value <- names(data)
    }

    super$initialize(
        package = "DataAudit",
        name = "dataAudit",
        version = c(1, 0, 0),
        options = options,
        results = dataAuditResults$new(options = options),
        data = data,
        datasetId = datasetId,
        analysisId = analysisId,
        revision = revision,
        pause = NULL,
        completeWhenFilled = FALSE,
        requiresMissings = FALSE,
        weightsSupport = "auto")
}

if (requireNamespace("jmvcore", quietly = TRUE))
    dataAuditBase$set("public", "initialize", .dataAuditBaseInitialize, overwrite = TRUE)
