# Tests depends on files in test folder
ls()

# Test for make_filename function
expect_identical(make_filename(2013),
                 'accident_2013.csv.bz2')

# Test for fars_read function
test_that('test fars_read', {
  # Throws an error when file does not exist
  expect_error(fars_read('accident_1900.csv.bz2'),
               "file 'accident_1900.csv.bz2' does not exist")

  # Will run and check dimensions and class
  dat <- fars_read('accident_2013.csv.bz2')
  expect_identical(class(dat), c("tbl_df", "tbl", "data.frame"))
  expect_equal(dim(dat), c(30202, 50))
})

system.file('extdata', 'accident_2013.csv.bz2', package = 'fars')

# test fars_read_years
test_that('test fars_read_years', {
  # Throws warning when a year does not exist in data
  yrs <- 2013:2016
  expect_warning(fars_read_years(yrs),
                 "invalid year: 2016")
  yrs <- 2013:2015
  dat <- fars_read_years(yrs)
  expect_identical(class(dat), 'list')
  expect_identical(length(dat), length(yrs))
})
# Test for fars_summarize_years function
test_that('test fars_summarize_years', {
  yrs <- 2013:2015
  dat <- fars_summarize_years(yrs)
  expect_identical(class(dat), c("tbl_df", "tbl", "data.frame"))
  expect_equal(dim(dat), c(12, 4))
})

# Test for fars_map_state function
test_that('test fars_map_state', {
  # Throws error when invalid state number is given.
  expect_error(fars_map_state(1000, 2013),
               'invalid STATE number: 1000')
})
