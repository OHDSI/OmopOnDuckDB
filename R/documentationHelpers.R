
#' Helper for consistent documentation of function arguments
#' @param con A DuckDB connection or path to a DuckDB database file.
#' @param cdmSchema Name of the schema that contains the OMOP CDM tables.
#' @param cdmPrefix Prefix applied to OMOP CDM table names in `cdmSchema`.
#' @param writeSchema Name of the schema where writable tables are created.
#' @param writePrefix Prefix applied to writable table names in `writeSchema`.
#' @param achillesSchema Name of the schema that contains Achilles tables. If
#'   `NULL`, Achilles tables are not loaded.
#' @param achillesPrefix Prefix applied to Achilles table names in
#'   `achillesSchema`.
#' @param cohortTables Character vector of cohort table names to include from
#'   `writeSchema`.
#' @param otherTables Character vector of additional non-CDM table names to
#'   include from `writeSchema`.
#' @param cdmVersion OMOP CDM version.
#' @param cdmName Name assigned to the returned CDM reference. If `NULL`, the
#'   name is read from the `cdm_source` table when available.
#' @param .softValidation Logical. If `TRUE`, use soft validation when creating
#'   CDM and cohort table objects.
#'
#' @name conDoc
#' @keywords internal
NULL
