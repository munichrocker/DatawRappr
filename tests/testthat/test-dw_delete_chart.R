library(httptest)

with_mock_api({

  test_that("Chart can be deleted", {
    expect_output(dw_delete_chart("Kqysh"), "sucessfully deleted", fixed = TRUE)
  })

})
