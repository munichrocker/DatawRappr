test_that("Multiple charts can be edited and sent data", {
  # two charts that you have access to, but were created by a different user
  chart_1 <- Sys.getenv('DW_TEST_CHART1', '3aW91')
  chart_2 <- Sys.getenv('DW_TEST_CHART2', 'TInvx')

  data_1 <- data.frame(
    names=c('never', 'gonna'),
    values=c(1,2)
  )

  data_2 <- data.frame(
    names=c('give', 'you', 'up'),
    values=c(100, 99, 88)
  )

  dw_data_to_chart(data_1, chart_1)
  dw_edit_chart(chart_1, title='never gonna',
                intro='let',
                source_name = 'you',
                annotate = 'down')
  dw_publish_chart(chart_1)

  dw_data_to_chart(data_2, chart_2)
  dw_edit_chart(chart_2, # without fix, test fails here
                title='never gonna',
                intro='run around',
                source_name='and',
                annotate='desert you')
  dw_publish_chart(chart_2)

  expect_true(T) # expect to reach here without quitting
})
