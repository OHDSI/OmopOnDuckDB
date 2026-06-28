test_that("duckdbSource validates connections, schemas, and prefixes", {
  path <- tempfile()
  src <- duckdbSource(con = path, writeSchema = "scratch", writePrefix = NULL)
  expect_s3_class(src, "cdm_source")
  expect_true(file.exists(paste0(path, ".duckdb")))
  expect_equal(summary(src)$writeSchema, "scratch")
  expect_equal(summary(src)$writePrefix, "")
  expect_true(src == src)
  expect_false(src != src)
  expect_error(src + src, "not implemented")
  omopgenerics::cdmDisconnect(src)

  src <- duckdbSource(con = paste0(path, ".duckdb"))
  expect_s3_class(src, "duckdb_cdm")
  omopgenerics::cdmDisconnect(src)

  con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  src <- duckdbSource(con = con)
  expect_equal(summary(src)$databasePath, "memory")
  omopgenerics::cdmDisconnect(src)

  con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  duckdb::dbDisconnect(con, shutdown = TRUE)
  expect_false(OmopOnDuckDB:::isConnectionWorking(con))
  expect_false(OmopOnDuckDB:::isConnectionWorking(list()))
  expect_error(duckdbSource(con = list()), "not a valid")
  expect_error(duckdbSource(writeSchema = 1))
  expect_error(duckdbSource(writePrefix = 1))
})

test_that("duckdb source table methods read, write, compute, and drop tables", {
  src <- duckdbSource(writeSchema = "scratch", writePrefix = "tmp_")
  on.exit(omopgenerics::cdmDisconnect(src), add = TRUE)

  tab <- omopgenerics::insertTable(src, "mytable", data.frame(ID = 1:2, A = 3:4))
  expect_s3_class(tab, "cdm_table")
  expect_equal(omopgenerics::listSourceTables(src), "mytable")
  expect_equal(
    as.data.frame(dplyr::collect(omopgenerics::readSourceTable(src, "mytable"))),
    data.frame(id = 1:2, a = 3:4)
  )
  expect_equal(dplyr::collect(dplyr::tbl(src, name = "mytable"))$id, 1:2)
  expect_equal(dplyr::collect(dplyr::tbl(src, schema = "scratch", name = "tmp_mytable"))$id, 1:2)
  expect_s3_class(omopgenerics::cdmTableFromSource(src, tab), "cdm_table")

  expect_error(
    omopgenerics::insertTable(src, "mytable", data.frame(ID = 3L, A = 5L), overwrite = FALSE),
    "already exists"
  )
  omopgenerics::insertTable(src, "mytable", data.frame(ID = 3L, A = 5L))
  expect_equal(dplyr::collect(omopgenerics::readSourceTable(src, "mytable"))$id, 3L)

  tmp <- omopgenerics::insertTable(src, "temp_table", data.frame(ID = 9L), temporary = TRUE)
  expect_equal(dplyr::collect(tmp)$id, 9L)

  omopgenerics::insertTable(src, "second_table", data.frame(ID = 3L, B = 7L))
  joined <- dplyr::left_join(
    omopgenerics::readSourceTable(src, "mytable"),
    omopgenerics::readSourceTable(src, "second_table"),
    by = "id"
  )
  overwritten_first <- dplyr::compute(joined, name = "mytable")
  expect_equal(dplyr::collect(overwritten_first)$b, 7L)

  omopgenerics::insertTable(src, "mytable", data.frame(ID = 3L, A = 5L))
  omopgenerics::insertTable(src, "second_table", data.frame(ID = 3L, B = 7L))
  joined <- dplyr::left_join(
    omopgenerics::readSourceTable(src, "mytable"),
    omopgenerics::readSourceTable(src, "second_table"),
    by = "id"
  )
  persistent <- dplyr::compute(joined, name = "joined_table")
  expect_true("joined_table" %in% omopgenerics::listSourceTables(src))
  overwritten_second <- dplyr::compute(joined, name = "second_table")
  expect_equal(dplyr::collect(overwritten_second)$b, 7L)
  temporary <- dplyr::compute(persistent)
  expect_true(is.na(attr(temporary, "tbl_name")))

  omopgenerics::dropSourceTable(src, c("mytable", "second_table", "joined_table", "missing_table"))
  expect_false(any(c("mytable", "second_table", "joined_table") %in% omopgenerics::listSourceTables(src)))
})

test_that("GiBleed can be inserted into DuckDB and read as a CDM reference", {
  skip_if_not_installed("omock")

  cdm0 <- omock::mockCdmFromDataset("GiBleed", source = "local")
  cdm0 <- omock::mockCohort(cdm0, name = "bleed_cohort", numberCohorts = 2, recordPerson = c(1, 2))
  for (nm in omopgenerics::achillesTables("5.3")) {
    cdm0 <- omopgenerics::emptyAchillesTable(cdm0, nm)
  }
  cdm0$extra_table <- dplyr::tibble(x = 1:2)

  src <- duckdbSource()
  on.exit(omopgenerics::cdmDisconnect(src), add = TRUE)
  cdm <- omopgenerics::insertCdmTo(cdm = cdm0, to = src)
  expect_s3_class(cdm, "cdm_reference")
  expect_equal(omopgenerics::cdmName(cdm), "GiBleed")
  expect_equal(omopgenerics::cdmVersion(cdm), "5.3")
  expect_equal(nrow(dplyr::collect(cdm$person)), 2694)
  expect_s3_class(cdm$bleed_cohort, "cohort_table")
  expect_s3_class(cdm$achilles_analysis, "achilles_table")
  expect_equal(dplyr::collect(cdm$extra_table)$x, 1:2)

  simple_cohort <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = 1L,
    cohort_start_date = as.Date("2020-01-01"),
    cohort_end_date = as.Date("2020-01-02")
  )
  omopgenerics::insertTable(src, "simple_cohort", simple_cohort)
  inferred <- cdmFromDuckDB(
    con = src$con,
    cdmVersion = "5.3",
    cohortTables = c("bleed_cohort", "simple_cohort"),
    otherTables = "extra_table",
    achillesSchema = "main",
    .softValidation = TRUE
  )
  expect_s3_class(inferred$bleed_cohort, "cohort_table")
  expect_s3_class(inferred$simple_cohort, "cohort_table")
  expect_s3_class(inferred$achilles_results, "achilles_table")
  expect_equal(dplyr::collect(inferred$extra_table)$x, 1:2)
  expect_match(omopgenerics::cdmName(inferred), "Synthea|GiBleed")

  expect_error(cdmFromDuckDB(src$con, cdmVersion = "5.3", cohortTables = "absent", .softValidation = TRUE), "not present")
  expect_error(
    cdmFromDuckDB(src$con, cdmVersion = "5.3", otherTables = "absent", writePrefix = "pref_", .softValidation = TRUE),
    "main.pref_"
  )
  omopgenerics::dropSourceTable(src, "achilles_results")
  expect_error(cdmFromDuckDB(src$con, cdmVersion = "5.3", achillesSchema = "main", .softValidation = TRUE), "not present")
})
