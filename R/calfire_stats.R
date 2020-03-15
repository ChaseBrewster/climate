#'California Wildfire Stats
#'
#'This function produces summary statistics, by year, for California Wildfires from 2013 - 2019
#'
#'@param fire_data The input dataset for calculating summary statistics (Data is provided in the 'climate' package and is titled 'calfires')
#'@param year The year for which the function will return statistics (must be between 2013 - 2019)
#'@return List of the number of fires, total acres burned, injuries, fatalities, structures damaged, and structures destroyed, for a given year
#'@examples calfire_stats(fire_data = calfires, year = 2018)
#'@author Chase Brewster


calfire_stats <- function(fire_data, year){ #input data and year to produce statistics

  year = ifelse((year < 2013), return("Year must be between 2013 and 2019"), year)
  year = ifelse((year > 2019), return("Year must be between 2013 and 2019"), year)
  #data is only for years 2013 - 2019, otherwise, function will not run

  cal_fires <- fire_data %>%
    mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% #replace NA numeric values with 0 to do summary stats
    group_by(ArchiveYear) %>% #group the fires by the year they occured
    summarise(
      fires = length(ArchiveYear), #number of fires in that year
      acres_burned = sum(AcresBurned), #sum of acres burned for every fire in that year
      injuries = sum(Injuries), #sum of number of injuries that occured due to fires that year
      fatalities = sum(Fatalities), #sum of number of fatalities that occured due to fires that year
      damaged = sum(StructuresDamaged), #sum of number of structures damaged due to fires that year
      destroyed = sum(StructuresDestroyed) #sum of number of structres destroyed due to fires that year
    )

  col_names <- c("Year", "Fires", "Acres Burned", "Injuries", "Fatalities", "Structures Damaged", "Structures Destroyed")
  #make a list of column names to replace current column names

  colnames(cal_fires) <- col_names
  #apply them

  cal_fires_list <- cal_fires %>%  #filter down results to the specific year selected by the user
    filter(Year == year) #%>%
    #list() #turn it into a list

  return(cal_fires_list) #return that list
}

#check results
#data(calfires)
#calfire_stats(fire_data = calfires, year = 2017)
