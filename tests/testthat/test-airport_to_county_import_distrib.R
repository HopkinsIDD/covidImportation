test_that("do_airport_attribution() handles more than one airport", {
  local_dir = "airport_test_data"
  states_of_interest = c("MD", "VA", "DC")
  cores = 1
  yr = 2018
  regioncode = "more-than-one-airport"
  mean_travel_file = file.path(local_dir, "travel_mean.csv")

  # Create mean_travel_file
  setup_importations(dest=states_of_interest, last_date = ISOdate(2020, 3, 15, tz=Sys.timezone()), output_dir = local_dir)

  airports_to_consider <- get_airports_to_consider(mean_travel_file, states_of_interest)

  # Write shapefile
  county_pops_df <- get_county_pops(
    states_of_interest,
    regioncode,
    yr=yr,
    local_dir=local_dir,
    write_county_shapefiles=TRUE
  )

  do_airport_attribution(airports_to_consider, regioncode=regioncode, yr=yr, cores=cores, local_dir=local_dir)
})

test_that("do_airport_attribution() handles just one airport", {
  local_dir = "airport_test_data"
  states_of_interest = c("MD")
  cores = 1
  yr = 2018
  regioncode = "just-one-airport"
  mean_travel_file = file.path(local_dir, "travel_mean.csv")
  
  # Create mean_travel_file
  setup_importations(dest=states_of_interest, last_date = ISOdate(2020, 3, 15, tz=Sys.timezone()), output_dir = local_dir)
  
  airports_to_consider <- get_airports_to_consider(mean_travel_file, states_of_interest)
  
  expect_equal(nrow(airports_to_consider), 1)
  
  # Write shapefile
  county_pops_df <- get_county_pops(
    states_of_interest,
    regioncode,
    yr=yr,
    local_dir=local_dir,
    write_county_shapefiles=TRUE
  )
  
  do_airport_attribution(airports_to_consider, regioncode=regioncode, yr=yr, cores=cores, local_dir=local_dir)
})
