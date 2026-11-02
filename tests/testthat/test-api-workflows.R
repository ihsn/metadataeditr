test_that("API paths normalize trailing slashes and path segments", {
  expect_equal(
    metadataeditr:::me_api_url_for("/data_structures/7", "https://editor.test/api///"),
    "https://editor.test/api/data_structures/7"
  )
  expect_equal(metadataeditr:::me_api_path_segment("A/B", "segment"), "A%2FB")
  expect_error(metadataeditr:::me_api_path_segment(NA_character_, "segment"), "single non-empty")
})

test_that("codelist identity lookup uses query parameters", {
  calls <- new.env(parent = emptyenv())
  local_mocked_bindings(
    me_api_get = function(endpoint, query = list(), ...) {
      calls$endpoint <- endpoint
      calls$query <- query
      list(status_code = 200L, response = list())
    },
    .package = "metadataeditr"
  )

  me_codelist_by_identity("AGENCY", "REF_AREA", "1.0")

  expect_equal(calls$endpoint, "codelists/lookup_by_identity")
  expect_equal(calls$query, list(agency = "AGENCY", name = "REF_AREA", version = "1.0"))
})

test_that("indicator data row filters remain JSON arrays", {
  calls <- new.env(parent = emptyenv())
  local_mocked_bindings(
    me_api_get = function(endpoint, query = list(), ...) {
      calls$endpoint <- endpoint
      calls$query <- query
      list(status_code = 200L, response = list())
    },
    .package = "metadataeditr"
  )

  me_indicator_data_rows("IND-1", filters = list(REF_AREA = "ALB"))

  expect_equal(calls$endpoint, "indicator_dsd/data_rows/IND-1")
  expect_equal(calls$query$filters, '{"REF_AREA":["ALB"]}')
})

test_that("data structure component writes use the registry endpoints", {
  calls <- new.env(parent = emptyenv())
  local_mocked_bindings(
    me_api_post_json = function(endpoint, body = list(), ...) {
      calls$endpoint <- endpoint
      calls$body <- body
      list(status_code = 200L, response = list())
    },
    .package = "metadataeditr"
  )

  me_data_structure_component_add(7, list(name = "REF_AREA"))

  expect_equal(calls$endpoint, "data_structures/components/7")
  expect_equal(calls$body$name, "REF_AREA")
})

test_that("component catalog uses the backend structure_id filter", {
  calls <- new.env(parent = emptyenv())
  local_mocked_bindings(
    me_api_get = function(endpoint, query = list(), ...) {
      calls$endpoint <- endpoint
      calls$query <- query
      list(status_code = 200L, response = list())
    },
    .package = "metadataeditr"
  )

  me_data_structure_components(structure_id = 7)

  expect_equal(calls$endpoint, "data_structures/components")
  expect_equal(calls$query$structure_id, 7)
  expect_null(calls$query$data_structure_id)
})

test_that("codelist JSON export returns a JSON response", {
  calls <- new.env(parent = emptyenv())
  local_mocked_bindings(
    me_api_get = function(endpoint, query = list(), ...) {
      calls$endpoint <- endpoint
      calls$query <- query
      list(status_code = 200L, response = list())
    },
    .package = "metadataeditr"
  )

  me_codelist_export_json(5)

  expect_equal(calls$endpoint, "codelists/export_json/5")
  expect_identical(calls$query$download, FALSE)
})

test_that("project list omits empty default filters", {
  calls <- new.env(parent = emptyenv())
  local_mocked_bindings(
    me_get_api_url = function(endpoint = NULL) {
      paste0("https://editor.test/api/", endpoint)
    },
    me_metadataedit_http_response_json = function(response) list(),
    .package = "metadataeditr"
  )
  local_mocked_bindings(
    GET = function(url, ...) {
      calls$url <- url
      list(status_code = 200L)
    },
    .package = "httr"
  )

  me_list_projects()

  expect_equal(calls$url, "https://editor.test/api/editor/?offset=0&limit=100")
})