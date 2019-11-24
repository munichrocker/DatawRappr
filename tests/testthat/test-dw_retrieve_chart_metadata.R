library(httptest)

with_mock_api({
  test_that("Chart metadata can be retrieved", {
    test_chart <- dw_retrieve_chart_metadata("Kqysh")
    expect_s3_class(test_chart, "dw_chart")
    expect_match(test_chart[["content"]][["id"]], "[a-zA-Z0-9]{5}")
  })
})
