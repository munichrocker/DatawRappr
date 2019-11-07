library(httptest)

with_mock_api({
  test_that("Chart metadata can be retrieved", {
    test_chart <- dw_retrieve_chart_metadata("VYdbD")
    expect_s3_class(test_chart, "dw_chart")
    expect_identical(test_chart$content$status, "ok")
    expect_match(test_chart[["content"]][["data"]][[1]][["id"]], "[a-zA-Z0-9]{5}")
  })
})
