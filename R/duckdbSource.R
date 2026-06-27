
#' Create a `DuckDB` source
#'
#' @inheritParams conDoc
#'
#' @return a `DuckDB` source.
#' @export
#'
#' @examples
#' library(OmopOnDuckDB)
#'
#' src <- duckdbSource()
#'
duckdbSource <- function(con = NULL,
                         writeSchema = "main",
                         writePrefix = "") {
  # input validation
  con <- validateCon(con = con)
  writeSchema <- validateSchema(schema = writeSchema, con = con)
  writePrefix <- validatePrefix(prefix = writePrefix)

  # create source
  src <- createDuckDBSource(
    con = con,
    writeSchema = writeSchema,
    writePrefix = writePrefix
  )

  # validate source
  src <- validateDuckDBSource(src = src)

  return(src)
}

validateCon <- function(con, call = parent.frame()) {
  if (is.null(con)) {
    con <- tempfile(fileext = ".duckdb")
  }
  if (is.character(con)) {
    omopgenerics::assertCharacter(con, length = 1)
    if (!endsWith(x = con, suffix = ".duckdb")) {
      con <- paste0(con, ".duckdb")
    }
    if (file.exists(con)) {
      cli::cli_inform(c(i = "Attempting to connect to {.path {con}}."))
    } else {
      cli::cli_inform(c(i = "Creating {.pkg duckdb} database {.path {con}}."))
    }
    con <- duckdb::dbConnect(drv = duckdb::duckdb(dbdir = con))
  }
  if (isConnectionWorking(con = con)) {
    return(con)
  } else {
    cli::cli_abort(c(x = "`con` ({.cls {class(con)}}) is not a valid {.pkg duckdb} connection."))
  }
}
validateSchema <- function(schema, con, call = parent.frame()) {
  nm <- deparse(substitute(schema))
  omopgenerics::assertCharacter(schema, length = 1, nm = nm, call = call)
  if (!schemaExists(con = con, schema = schema)) {
    cli::cli_inform(c("!" = "`{nm}` ({.pkg {schema}}) does not exist. Trying to create it..."))
    createSchema(con = con, schema = schema)
    cli::cli_inform(c("v" = "`{nm}` ({.pkg {schema}}) created."))
  }
  return(schema)
}
validatePrefix <- function(prefix, call = parent.frame()) {
  nm <- deparse(substitute(prefix))
  prefix <- prefix %||% ""
  omopgenerics::assertCharacter(prefix, length = 1, nm = nm, call = call)
  return(prefix)
}
createDuckDBSource <- function(con,
                               writeSchema,
                               writePrefix) {
  structure(
    .Data = list(
      con = con,
      writeSchema = writeSchema,
      writePrefix = writePrefix
    ),
    class = "duckdb_cdm"
  )
}
validateDuckDBSource <- function(src) {
  omopgenerics::newCdmSource(src = src, sourceType = "DuckDB")
}
schemaExists <- function(con, schema) {
  dbfile <- getDbName(con = con) |>
    basename() |>
    stringr::str_replace(pattern = ".duckdb$", replacement = "")
  dplyr::tbl(con, I("information_schema.schemata")) |>
    dplyr::filter(
      .data$catalog_name == .env$dbfile,
      .data$schema_name == .env$schema
    ) |>
    dplyr::tally() |>
    dplyr::pull() |>
    as.integer() == 1L
}
getDbName <- function(con) {
  x <- DBI::dbGetInfo(con)$dbname
  if (x == ":memory:") {
    x <- "memory"
  }
  return(x)
}
createSchema <- function(con, schema) {
  DBI::dbExecute(conn = con, statement = paste0("CREATE SCHEMA ", schema, ";"))
}
isConnectionWorking <- function(con) {
  if (inherits(con, "duckdb_connection")) {
    res <- tryCatch({
      DBI::dbGetQuery(con, "SELECT 1")
      TRUE
    }, error = function(e) {
      FALSE
    })
  } else {
    res <- FALSE
  }
  return(res)
}

#' @importFrom dplyr tbl
#' @export
tbl.duckdb_cdm <- function(src, schema = NULL, name, ...) {
  if (is.null(schema)) {
    schema <- src$writeSchema
    nm <- paste0(src$writePrefix, name)
  } else {
    nm <- name
  }
  dplyr::tbl(src = src$con, I(paste0(schema, ".", nm))) |>
    dplyr::rename_all(tolower) |>
    omopgenerics::newCdmTable(src = src, name = name)
}

#' @importFrom dplyr compute
#' @export
compute.duckdb_cdm <- function(x, name, temporary, overwrite, ...) {
  src <- attr(x, "tbl_source")

  # add intermediate if needed
  query <- as.character(dbplyr::sql_render(x))
  intermediate <- !temporary && overwrite &&
    grepl(paste0("\\Q", fullNameChar(src = src, name = name), "\\E(\\W|$)"), query, perl = TRUE)
  if (intermediate) {
    nm <- omopgenerics::uniqueTableName()
    x <- dplyr::compute(x, name = nm, temporary = FALSE, overwrite = TRUE, ...)
    on.exit(DBI::dbRemoveTable(src$con, nm))
  }

  if (!temporary) {
    dropSourceTable(cdm = src, name = name)
    name <- fullNameId(src = src, name = name)
  }

  class(x) <- setdiff(class(x), "duckdb_cdm")
  dplyr::compute(x, name = name, temporary = temporary, overwrite = overwrite, ...)
}

#' @export
summary.duckdb_cdm <- function(object, ...) {
  list(
    package = "OmopOnDuckDB",
    databasePath = getDbName(object$con),
    writeSchema = object$writeSchema,
    writePrefix = object$writePrefix
  )
}

#' @importFrom omopgenerics insertTable
#' @export
insertTable.duckdb_cdm <- function(cdm, name, table, ...) {
  if (name %in% listTablesSrc(src = cdm)) {
    dropSourceTable(cdm = cdm, name = name)
  }
  writeTableSrc(src = cdm, name = name, value = table)
}

#' @importFrom omopgenerics insertCdmTo
#' @export
insertCdmTo.duckdb_cdm <- function(cdm , to) {
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)

  achillesSchema <- NULL
  cohorts <- character()
  other <- character()
  for (nm in names(cdm)) {
    x <- dplyr::collect(cdm[[nm]])
    cl <- class(x)
    if ("achilles_table" %in% cl) {
      achilles <- to$writeSchema
    }
    if (!any(c("achilles_table", "omop_table", "cohort_table") %in% cl)) {
      other <- c(other, nm)
    }
    insertTable(cdm = to, name = nm, table = x)
    if ("cohort_table" %in% cl) {
      cohorts <- c(cohorts, nm)
      insertTable(cdm = to, name = paste0(nm, "_set"), table = attr(x, "cohort_set"))
      insertTable(cdm = to, name = paste0(nm, "_attrition"), table = attr(x, "cohort_attrition"))
      insertTable(cdm = to, name = paste0(nm, "_codelist"), table = attr(x, "cohort_codelist"))
    }
  }

  cdmFromDuckDB(
    con = to$con,
    cdmSchema = to$writeSchema,
    writeSchema = to$writeSchema,
    cohortTables = cohorts,
    achillesSchema = achillesSchema,
    cdmName = omopgenerics::cdmName(cdm),
    cdmVersion = omopgenerics::cdmVersion(cdm),
    .softValidation = TRUE
  ) |>
    omopgenerics::readSourceTable(name = other)
}

#' @importFrom omopgenerics dropSourceTable
#' @export
dropSourceTable.duckdb_cdm <- function(cdm, name) {
  for (nm in name) {
    statement <- paste0("DROP TABLE IF EXISTS ", fullNameChar(src = cdm, name = nm))
    DBI::dbExecute(conn = cdm$con, statement = statement)
  }
}

#' @importFrom omopgenerics readSourceTable
#' @export
readSourceTable.duckdb_cdm <- function(cdm, name) {
  readTableSrc(src = cdm, name = name)
}

#' @importFrom omopgenerics listSourceTables
#' @export
listSourceTables.duckdb_cdm <- function(cdm) {
  listTablesSrc(src = cdm)
}

#' @importFrom omopgenerics cdmDisconnect
#' @export
cdmDisconnect.duckdb_cdm <- function(cdm, ...) {
  duckdb::dbDisconnect(conn = cdm$con)
}

#' @importFrom omopgenerics cdmTableFromSource
#' @export
cdmTableFromSource.duckdb_cdm <- function(src, value) {

}

fullNameId <- function(src, name) {
  DBI::Id(schema = src$writeSchema, table = paste0(src$writePrefix, name))
}
fullNameChar <- function(src, name) {
  paste0(src$writeSchema, ".", src$writePrefix, name)
}
listTablesSrc <- function(src) {
  listTables(
    con = src$con,
    schema = src$writeSchema,
    prefix = src$writePrefix
  )
}
listTables <- function(con, schema, prefix = "") {
  tables <- dplyr::tbl(con, I("information_schema.tables")) |>
    dplyr::filter(.data$table_schema %in% .env$schema) |>
    dplyr::pull("table_name")
  if (!identical(prefix, "")) {
    tables <- tables[startsWith(x = tables, prefix = prefix)] |>
      stringr::str_replace(pattern = paste0("^", prefix), replacement = "")
  }
  return(tables)
}
readTableSrc <- function(src, name) {
  readTable(
    con = src$con,
    name = fullNameId(src = src, name = name)
  ) |>
    omopgenerics::newCdmTable(src = src, name = name)
}
readTable <- function(con, name) {
  dplyr::tbl(con, name) |>
    dplyr::rename_all(tolower)
}
writeTableSrc <- function(src, name, value) {
  nm <- fullNameId(src = src, name = name)
  writeTable(con = src$con, name = nm, value = value) |>
    omopgenerics::newCdmTable(src = src, name = name)
}
writeTable <- function(con, name, value) {
  DBI::dbWriteTable(conn = con, name = name, value = value)
  readTable(con = con, name = name)
}
reportSchema <- function(schema, prefix) {
  if (identical(prefix, "")) {
    schema
  } else {
    paste0(schema, ".", prefix)
  }
}
