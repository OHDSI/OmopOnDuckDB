
#' Create a `DuckDB` source
#'
#' @param con A DuckDB connection or path to a duckdb file. If NULL a temp connection will be created.
#' @param writeSchema ws
#' @param writePrefix wp
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
                         writeSchema = NULL,
                         writePrefix = NULL) {
  # input validation
  con <- validateCon(con = con)
  writeSchema <- validateWriteSchema(writeSchema = writeSchema, con = con)
  writePrefix <- validateWritePrefix(writePrefix = writePrefix)

  # create source
  src <- createDuckDBSource(
    con = con,
    writeSchema = writeSchema,
    writePrefix = writePrefix
  )

  # validate source
  #src <- validateDuckDBSource(src = src)

  return(src)
}

validateCon <- function(con, call = parent.frame()) {
  if (is.null(con)) {
    con <- tempfile(fileext = ".duckdb")
  }
  if (is.character(con)) {
    omopgenerics::assertCharacter(con, length = 1, msg = msg)
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
  if (!isConnectionWorking(con = con)) {
    return(con)
  } else {
    cli::cli_abort(c(x = "`con` ({.cls {class(con)}}) is not a valid {.pkg duckdb} connection."))
  }
}
validateWriteSchema <- function(writeSchema, con, call = parent.frame()) {
  omopgenerics::assertCharacter(writeSchema, length = 1, null = T, call = call)
  if (is.null(writeSchema)) {
    writeSchema <- "main"
    cli::cli_inform(c(i = "Using default `writeSchema` as {.pkg main}."))
  }
  if (!schemaExists(con = con, schema = writeSchema)) {
    cli::cli_inform(c("!" = "`writeSchema` ({.pkg {writeSchema}}) does not exist. Trying to create it..."))
    createSchema(con = con, schema = writeSchema)
    cli::cli_inform(c("v" = "`writeSchema` ({.pkg {writeSchema}}) created."))
  }
  return(writeSchema)
}
validateWritePrefix <- function(writePrefix, call = parent.frame()) {
  writePrefix <- writePrefix %||% ""
  omopgenerics::assertCharacter(writePrefix, length = 1, call = call)
  return(writePrefix)
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
  dplyr::tbl(con, I("information_schema.schemata")) |>
    dplyr::filter(.data$schema_name == .env$schema) |>
    dplyr::tally() |>
    dplyr::pull() |>
    as.integer() == 1L
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
compute.duckdb_cdm <- function(x, name, temporary = FALSE, overwrite = TRUE, ...) {
  src <- attr(x, "tbl_source")

  # check if need intermediate
  if (intermediate) {
    nm <- omopgenerics::uniqueTableName()
    x <- x |>
      dplyr::compute(name = nm)
    on.exit(omopgenerics::dropSourceTable(cdm = x, name = nm))
  }

  if (!temporary) {
    # check if we need to drop old table
  }


}

#' @export
summary.duckdb_cdm <- function(object, ...) {
  list(
    package = "OmopOnDuckDB",
    databasePath = duckdb::dbGetInfo(object$con)$dbname,
    writeSchema = object$writeSchema,
    writePrefix = object$writePrefix
  )
}

#' @importFrom omopgenerics insertTable
#' @export
insertTable.duckdb_cdm <- function(cdm, table, name, ...) {
  if (name %in% listTablesSrc(src = cdm)) {
    dropSourceTable(cdm = cdm, name = name)
  }
  writeTableSrc(src = cdm, name = name, value = table)
}

#' @importFrom omopgenerics insertCdmTo
#' @export
insertCdmTo.duckdb_cdm <- function(cdm , to) {

}

#' @importFrom omopgenerics dropSourceTable
#' @export
dropSourceTable.duckdb_cdm <- function(cdm, name) {
  for (nm in name) {
    statement <- paste0("DROP TABLE IF EXISTS ", fullName(src = cdm, name = name))
    DBI::dbExecute(conn = con, statement = statement)
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
cdmDisconnect.duckdb_cdm <- function(cdm) {
  duckdb::dbDisconnect(conn = cdm$con)
}

#' @importFrom omopgenerics cdmTableFromSource
#' @export
cdmTableFromSource.duckdb_cdm <- function(src, value) {

}

fullName <- function(src, name) {
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
    name = fullName(src = src, name = name)
  ) |>
    omopgenerics::newCdmTable(src = src, name = name)
}
readTable <- function(con, name) {
  dplyr::tbl(con, I(name)) |>
    dplyr::rename_all(tolower)
}
writeTableSrc <- function(src, name, value) {
  name <- fullName(src = src, name = name)
  writeTable(con = src$con, name = name, value = value)
}
writeTable <- function(con, name, value) {
  DBI::dbWriteTable(conn = con, name = name, value = value)
  readTable(con = con, name = name)
}
