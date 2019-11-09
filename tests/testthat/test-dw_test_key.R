library(httptest)

with_mock_api({
  test_that("We can get a user object", {
    user <- dw_test_key()
    expect_s3_class(user, "dw_user")
    expect_identical(user$content$status, "ok")
  })
})
