#read in file to test against function output
fast_df <- readr::read_csv(system.file("extdata/DY15-05_forEcoDAAT.csv",
                                       package = "FastrCAT"))[1:17]

make_dataframe_fc(system.file("extdata", package = "FastrCAT"),
                  GE = TRUE, Cruise_report = FALSE, DF = FALSE)

test_that(
    "Dataframe and function object have same dimensions.",{
  expect_equal(dim(fastcat_data), dim(fast_df))
      }
  )

test_that(
  "Dataframe and function object are both dataframes",{
    expect_is(fastcat_data, "data.frame")
  }
)
