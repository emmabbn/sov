# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Additional test configuration should go in:
# tests/testthat/setup-*.R or tests/testthat/helper-*.R

library(testthat)
library(sov)

# Keep tests deterministic if any randomness exists
set.seed(1L)

test_check("sov")


