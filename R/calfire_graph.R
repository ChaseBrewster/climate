#'California Wildfire Graph
#'
#'This function produces a graph of summary statistics, by year, for California Wildfires from 2013 - 2019
#'
#'@param fire_data The input dataset for calculating summary statistics (Data is provided in the 'climate' package and is titled 'calfires')
#'@param metric The metric statistic that the graph will show (must be an integer between 1 - 6)
#'\itemize{
#' \item 1: Total fires per year
#' \item 2: Total acres burned per year
#' \item 3: Total injuries sustained due to fires per year
#' \item 4: Total fatalies due to fires per year
#' \item 5: Total structures damaged due to fires per year
#' \item 6: Total structures destroyed due to fires per year
#'}
#'@return Bar graph of chosen fire statistic per year from 2013 - 2018
#'@examples calfire_graph(fire_data = calfires, metric = 1)
#'@author Chase Brewster


calfire_graph <- function(fire_data, metric){ #input data and metric to define what the graph will display

  metric = ifelse((metric != 1 & metric != 2 & metric != 3 & metric != 4 & metric != 5 & metric != 6), return("Metric must be an integer 1 through 6. See documentation for metric description."), metric)
  #metric must be an integer between 1 and 6 otherwise the function will not run

  calfiresummary <- fire_data %>%
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

  col_names <- c("Year", "Fires", "Acres", "Injuries", "Fatalities", "Damaged", "Destroyed")
  #make a list of column names to replace current column names

  colnames(calfiresummary) <- col_names
  #apply them

  #a series of if statements based on the input metric given by the user
  #depending on the metric, the variable yaxis is given a value from the summary dataset
  if(metric == 1){yaxis <- calfiresummary$Fires}
  if(metric == 2){yaxis <- calfiresummary$Acres}
  if(metric == 3){yaxis <- calfiresummary$Injuries}
  if(metric == 4){yaxis <- calfiresummary$Fatalities}
  if(metric == 5){yaxis <- calfiresummary$Damaged}
  if(metric == 6){yaxis <- calfiresummary$Destroyed}

  calfiresummary$Year <- as.factor(calfiresummary$Year)
  #make the year variable a factor

  #graph the output, using the yaxis variable, defined by the metric, chosen by the user
  graph=ggplot(calfiresummary, aes(x = Year, y = yaxis)) +
    geom_col(aes(fill= Year)) +
    theme_light() +
    scale_fill_brewer(palette = "Dark2") +
    labs(title = "California Wildfires (2013-2019)", x = "Year", y = "Metric") +
    theme(legend.position = "none") +
    theme(plot.title = element_text(hjust = 0.5))

  return(graph) #return the graph
}

#check results
#calfire_graph(fire_data = calfires, metric = 1)
