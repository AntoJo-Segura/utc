library(utc)

test_that("Difference_between_UTC_and_local_is_correct",
           expect_equal(
             format( - hoursUTC() %h+% Sys.time(), format = "%Y-%m-%d %H:%M:%S" ),
             format( toUTC(Sys.time()), format = "%Y-%m-%d %H:%M:%S" )
           )
)
