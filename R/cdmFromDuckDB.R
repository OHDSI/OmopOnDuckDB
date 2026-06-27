
#' Title
#'
#' @inheritParams conDoc
#' @param cdmSchema
#' @param cdmPrefix
#' @param writeSchema
#' @param writePrefix
#' @param achillesSchema
#' @param achillesPrefix
#' @param cohortTables
#' @param otherTables
#' @param cdmVersion
#' @param cdmName
#' @param .softValidation
#'
#' @returns
#' @export
#'
#' @examples
cdmFromDuckDB <- function(con,
                          cdmSchema = "main",
                          cdmPrefix = "",
                          writeSchema = "main",
                          writePrefix = "",
                          achillesSchema = NULL,
                          achillesPrefix = "",
                          cohortTables = character(),
                          otherTables = character(),
                          cdmVersion = NULL,
                          cdmName = NULL,
                          .softValidation = FALSE) {
  # input validation
  con <- validateCon(con = con)
  cdmSchema <- validateSchema(schema = cdmSchema, con = con)
  cdmPrefix <- validatePrefix(prefix = cdmPrefix)
  writeSchema <- validateSchema(schema = writeSchema, con = con)
  writePrefix <- validatePrefix(prefix = writePrefix)
  if (!is.null(achillesSchema)) {
    achillesSchema <- validateSchema(schema = achillesSchema, con = con)
    achillesPrefix <- validatePrefix(prefix = achillesPrefix)
  }
  omopgenerics::assertCharacter(cohortTables)
  omopgenerics::assertCharacter(otherTables)
  omopgenerics::assertChoice(cdmVersion, supportedCdmVersions, null = TRUE)
  omopgenerics::assertCharacter(cdmName, length = 1, null = TRUE)
  omopgenerics::assertLogical(.softValidation, length = 1)

  # create source
  src <- duckdbSource(
    con = con,
    writeSchema = writeSchema,
    writePrefix = writePrefix
  )

  # read cdm tables
  cdmSrc <- list(con = con, writeSchema = cdmSchema, writePrefix = cdmPrefix)
  opts <- omopgenerics::omopTables(version = cdmVersion %||% "5.4")
  cdmTables <- listTablesSrc(src = cdmSrc)
  cdmTables <- cdmTables[cdmTables %in% opts] |>
    rlang::set_names() |>
    purrr::map(\(x) {
      readTable(con = con, name = fullNameId(src = cdmSrc, name = x)) |>
        omopgenerics::newCdmTable(src = src, name = x)
    })

  # find cdm name
  if (is.null(cdmName)) {
    if ("cdm_source" %in% names(cdmTables)) {
      if (!omopgenerics::isTableEmpty(cdm$cdm_source)) {
        candidate <- cdmTables$cdm_source |>
          dplyr::pull("cdm_source_name")
        if (length(candidate) == 1 & is.character(candidate)) {
          cdmName <- candidate
        }
      }
    }
  }
  cdmName <- cdmName %||% "Unknown"

  # create initial cdm
  cdm <- omopgenerics::newCdmReference(
    tables = cdmTables,
    cdmName = cdmName,
    cdmVersion = cdmVersion,
    .softValidation = .softValidation
  )

  # tables in writeSchema
  writeTables <- listTablesSrc(src = src)

  # read cohort tables
  notPresent <- cohortTables[!cohortTables %in% writeTables]
  if (length(notPresent) > 0) {
    sch <- reportSchema(schema = writeSchema, prefix = writePrefix)
    cli::cli_abort(c(x = "{.pkg {notPresent}} not present in {.var {sch}}."))
  }
  for (nm in cohortTables) {
    cdm[[nm]] <- readSourceTable(cdm = src, name = nm)
    set <- tryRead(src, nm, "set", writeTables)
    atr <- tryRead(src, nm, "attrition", writeTables)
    cod <- tryRead(src, nm, "codelist", writeTables)
    cdm[[nm]] <- omopgenerics::newCohortTable(
      table = cdm[[nm]],
      cohortSetRef = set,
      cohortAttritionRef = atr,
      cohortCodelistRef = cod,
      .softValidation = .softValidation
    )
  }

  # read other tables
  notPresent <- otherTables[!otherTables %in% writeTables]
  if (length(notPresent) > 0) {
    sch <- reportSchema(schema = writeSchema, prefix = writePrefix)
    cli::cli_abort(c(x = "{.pkg {notPresent}} not present in {.var {sch}}."))
  }
  for (nm in otherTables) {
    cdm[[nm]] <- readSourceTable(cdm = src, name = nm)
  }

  # read achilles tables
  if (!is.null(achillesSchema)) {
    achSrc <- list(con = con, writeSchema = achillesSchema, writePrefix = achillesPrefix)
    achOpts <- listTablesSrc(src = achSrc)
    achs <- omopgenerics::achillesTables(version = omopgenerics::cdmVersion(cdm))
    notPresent <- achs[!achs %in% achOpts]
    if (length(notPresent) > 0) {
      sch <- reportSchema(schema = achillesSchema, prefix = achillesPrefix)
      cli::cli_abort(c(x = "{.pkg {notPresent}} not present in {.var {sch}}."))
    }
    for (ach in achs) {
      cdm[[ach]] <- readTable(con = con, name = fullNameId(src = achSrc, name = ach)) |>
        omopgenerics::newCdmTable(src = src, name = ach)
    }
  }

  return(cdm)
}
tryRead <- function(src, nm, suffix, tables) {
  nm <- paste0(nm, "_", suffix)
  if (nm %in% tables) {
    x <- readSourceTable(cdm = src, name = nm)
  } else {
    x <- NULL
  }
  return(x)
}
