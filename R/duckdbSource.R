
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
compute
summary
insertTable
insertCdmTo
dropSourceTable
readSourceTable
listSourceTables
cdmDisconnect
cdmTableFromSource

