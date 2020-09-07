# test_that("pull_JHUCSSE_github_data works", {
#   case_data_dir = "test_case_data"
#   unlink(case_data_dir, recursive=TRUE)
# 
#   # Pull everything
#   start_time <- Sys.time()
#   pull_JHUCSSE_github_data(case_data=case_data_dir, repull_all=FALSE)
# 
#   all_file_list <- list.files(case_data_dir, full.names=TRUE)
#   expect_gt(length(all_file_list), 0)
#   for(f in all_file_list) {
#     expect_gte(file.info(f)$ctime, start_time)
#   }
# 
#   # Remove one file and replace just the one file
#   last_file <- tail(all_file_list, n=1)
#   unlink(last_file)
# 
#   # Get the most recent file
#   file_prefix = "JHUCSSE Total Cases "
#   files_in_dir <- list.files(case_data_dir, file_prefix)
#   files_in_dir_dates <- gsub(file_prefix, "", files_in_dir)
#   files_in_dir_dates <- gsub(".csv", "", files_in_dir_dates)
#   most_recent_file <- all_file_list[which.max(lubridate::mdy(files_in_dir_dates))]
# 
#   start_time <- Sys.time()
#   pull_JHUCSSE_github_data(case_data=case_data_dir, repull_all=FALSE)
# 
#   for(f in all_file_list) {
#     if (!(f %in% c(last_file, most_recent_file))) {
#       expect_lt(file.info(f)$mtime, start_time)
#     } else {
#         expect_gte(file.info(f)$mtime, start_time)
#       }
#   }
# 
#   # Re-run
#   start_time <- Sys.time()
#   pull_JHUCSSE_github_data(case_data=case_data_dir, repull_all=FALSE)
#   for(f in all_file_list) {
#     if (f != most_recent_file) {
#       expect_lt(file.info(f)$mtime, start_time)
#     } else {
#       expect_gte(file.info(f)$mtime, start_time)
#     }
#   }
# 
#   # Remove one file, but repull all of them
#   last_file <-tail(list.files(case_data_dir), fullnames=TRUE)
#   unlink(last_file)
# 
#   start_time = Sys.time()
#   pull_JHUCSSE_github_data(case_data=case_data_dir, repull_all=TRUE)
#   for(f in all_file_list) {
#     expect_gte(file.info(f)$mtime, start_time)
#   }
# 
#   unlink(case_data_dir, recursive=TRUE)
# })


test_that("get_clean_JHUCSSE_data errors on bad aggr_level", {
  expect_error(get_clean_JHUCSSE_data(aggr_level="Bad aggr level"), "Invalid aggr_level")
})

test_that("get_clean_JHUCSSE_data works with defaults", {
  expect_output(get_clean_JHUCSSE_data(), "Combined data is saved in", all=TRUE)
})

test_that("get_clean_JHUCSSE_data works with expected arguments", {
  expect_output(get_clean_JHUCSSE_data(last_date = as.POSIXct(lubridate::ymd("2020-03-20"))), 
                 "Combined data is saved in", all=TRUE)
})
