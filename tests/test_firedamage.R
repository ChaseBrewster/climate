test_that("firedamage_works",
          {
          expect_true(firedamage(acres = 15000, value = 1000, structures = 0, avgstructureval = 150000, helicopters = 0, days = 5, firecrews = 1, injuries = 0, fatalities = 0) > firedamage(acres = 10000, value = 1000, structures = 0, avgstructureval = 150000, helicopters = 0, days = 5, firecrews = 1, injuries = 0, fatalities = 0))
          expect_equal(firedamage(acres = 10000, value = 1000, structures = 100, avgstructureval = 100000, helicopters = 5, days = 10, firecrews = 10, injuries = 100, fatalities = 10), firedamage(acres = 100000, value = 100, structures = 1000, avgstructureval = 10000, helicopters = 5, days = 10, firecrews = 10, injuries = 10, fatalities = 100))
          expect_true(firedamage(acres = 1, value = 1, structures = 0, avgstructureval = 1, helicopters = 0, days = 1, firecrews = 1, injuries = 0, fatalities = 0) == 9601 )
        }
)
