
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
  msg <- "`con`"
  con <- con %||% tempfile(fileext = ".duckdb")
  if (is.character(con)) {
    omopgenerics::assertCharacter(con, length = 1, msg = msg)
    if (!endsWith(x = con, suffix = ".duckdb")) {
      con <- paste0(con, ".duckdb")
    }
    cli::cli_inform(c(i = "Creating {.pkg duckdb} connection in {.path {con}}."))
    con <- duckdb::dbConnect(drv = duckdb::duckdb(dbdir = con))
  }
}
validateWriteSchema <- function(writeSchema, con, call = parent.frame()) {
  writeSchema <- writeSchema %||% "main"
}
validateWritePrefix <- function(writePrefix, call = parent.frame()) {
  writePrefix <- writePrefix %||% ""
  omopgenerics::assertCharacter(writePrefix, length = 1, call = call)
  return(writePrefix)
}
createDuckDBSource <- function(con,
                               writeSchema,
                               writePrefix) {
  source <- list(con = con, writeSchema = writeSchema, writePrefix = writePrefix)
  class(source) <- "duckdb_cdm"
  return(source)
}
validateDuckDBSource <- function(src) {
  omopgenerics::newCdmSource(src = src, sourceType = "DuckDB")
}
createSchema <- function() {

}
