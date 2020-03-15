test_that("calfire_stats_works" ,
          {test_data = as.data.frame(cbind(ArchiveYear = sample(2013:2019, 100, replace = TRUE), AcresBurned = sample(500:50000, 100, replace = TRUE), Injuries = sample(0:200, 100, replace = TRUE), Fatalities = sample(0:30, 100, replace = TRUE), StructuresDamaged = sample(0:1000, 100, replace =), StructuresDestroyed = sample(0:1000, 100, replace = TRUE)))

          test_data_2018 <- test_data %>%
            filter(ArchiveYear == 2018)

          expect_true(calfire_stats(fire_data = test_data, year = 2013)$Year == 2013)
          expect_equal(calfire_stats(fire_data = test_data, year = 2018)$Fires, length(test_data_2018$ArchiveYear))
          expect_length(calfire_stats(fire_data = test_data, year = 2019), 7)

          }
)



