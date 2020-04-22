test_that("Test Cases work",{
  expect_equal({
    1+1
  }, 2
  )

  expect_error({
    stop("This is an error")
  })

  expect_error({
    message("This is not an error")
  }, NA)

})

