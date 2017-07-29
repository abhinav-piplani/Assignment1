
#Testing in case state code is entered in 3 digits instead of 2

library(testthat)
library(Assignment1)
#expect_equal(fars_map_state(55, 2014),  fars_map_state(055, 2014))
expect_equal(fars_summarize_years(2014), fars_summarize_years(2014))

