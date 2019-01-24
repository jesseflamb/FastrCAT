
make_dataframe_fc(system.file("extdata", package = "FastrCAT"),
                  GE = TRUE, Cruise_report = FALSE, DF = FALSE)

test_that(
    "Dataframe and function object have same dimensions.",{
  expect_equal(dim(fastcat_data), c(911, 17))
      }
  )

test_that(
  "Dataframe and function object are both dataframes",{
    expect_is(fastcat_data, "data.frame")
  }
)

test_that(
  "CRUISE column is atomic class character",{
    expect_true(is.character(fastcat_data[,"CRUISE"]))
  }
)

test_that(
  "STATION_NAME is atomic class character",{
    expect_true(is.character(fastcat_data[,"STATION_NAME"]))
  }
)

test_that(
  "HAUL_NAME is atomic class integer",{
    expect_true(is.integer(fastcat_data[,"HAUL_NAME"]))
  }
)

test_that(
  "FOCI_GRID is atomic class character",{
    expect_true(is.integer(fastcat_data[,"FOCI_GRID"]))
  }
)

test_that(
  "DATE is class POSIXct",{
    expect_true(is.POSIXct(fastcat_data[,"DATE"]))
  }
)

test_that(
  "TIME is class atomic class character",{
    expect_true(is.character(fastcat_data[,"TIME"]))
  }
)

test_that(
  "LAT is class atomic class numeric",{
    expect_true(is.numeric(fastcat_data[,"LAT"]))
  }
)

test_that(
  "LON is class atomic class numeric",{
    expect_true(is.numeric(fastcat_data[,"LON"]))
  }
)

test_that(
  "DEPTH_BOTTOM is class atomic class integer",{
    expect_true(is.integer(fastcat_data[,"DEPTH_BOTTOM"]))
  }
)

test_that(
  "DEPTH is class atomic class integer",{
    expect_true(is.integer(fastcat_data[,"DEPTH"]))
  }
)

test_that(
  "PRESSURE is class atomic class numeric",{
    expect_true(is.numeric(fastcat_data[,"PRESSURE"]))
  }
)

test_that(
  "TEMPERATURE1 is class atomic class numeric",{
    expect_true(is.numeric(fastcat_data[,"TEMPERATURE1"]))
  }
)

test_that(
  "SALINITY1 is class atomic class numeric",{
    expect_true(is.numeric(fastcat_data[,"SALINITY1"]))
  }
)

test_that(
  "CONDUCTIVITY1 is class atomic class numeric",{
    expect_true(is.numeric(fastcat_data[,"CONDUCTIVITY1"]))
  }
)

test_that(
  "SIGMA_T is class atomic class numeric",{
    expect_true(is.numeric(fastcat_data[,"SIGMA_T"]))
  }
)

test_that(
  "INSTRUMENT is class atomic class character",{
    expect_true(is.character(fastcat_data[,"INSTRUMENT"]))
  }
)

test_that(
  "DIRECTORY is class atomic class character",{
    expect_true(is.character(fastcat_data[,"DIRECTORY"]))
  }
)

test_that(
  "DEPTH calculations from PRESSURE are ", {
    expect_equal(fastcat_data$DEPTH[1:10], c(103, 102, 101, 100, 99, 98, 97, 96, 95, 94))
  }
)
