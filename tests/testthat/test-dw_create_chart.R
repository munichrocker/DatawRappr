library(httptest)

with_mock_api({
  test_that("New chart can be created", {
    new_chart <- dw_create_chart()
    expect_s3_class(new_chart, "dw_chart")
    expect_identical(new_chart$content$status, "ok")
    expect_match(new_chart[["content"]][["data"]][[1]][["id"]], "[a-zA-Z0-9]{5}")
  })
})
