#'Wildfire Damage Calculator
#'
#'This function roughly calculates the monetary damage of a wildfire
#'
#'@param acres The number of acres burned in the fire
#'@param value The average land value of the area burned (in USD per acre)
#'@param structures The number of structures severly damaged and destroyed in the fire
#'@param avgstructureval The average value of structures in the burn area (in USD per structure)
#'@param helicopters The number of helicopters used to fight the fire
#'@param days The number of days the fire burned
#'@param firecrews The number of firecrews that fought the fire
#'@param injuries The number of injuries sustained due to the fire
#'@param fatalities The number of fatalities due to the fire
#'@return Total monetary damage of a wildfire, in USD
#'@examples firedamage(acres = 100, value = 5000, structures = 20, avgstructureval = 100000, helicopters = 5, days = 5, firecrews = 8, injuries = 8, fatalities= 0)
#'@author Chase Brewster

#begin the function
firedamage <- function(acres, value, structures, avgstructureval, helicopters, days, firecrews, injuries, fatalities){

  #The following parameters must be greater than 0 for any given fire
  acres = ifelse((acres <= 0), return("Acres must be greater than 0"), acres)
  value = ifelse((value <= 0), return("Value must be greater than 0"), value)
  avgstructureval = ifelse((avgstructureval <= 0), return("Average structure value must be greater than 0"), avgstructureval)
  days = ifelse((days <= 0), return("Days must be greater than 0"), days)
  firecrews = ifelse((firecrews <= 0), return("Firecrews must be greater than 0"), firecrews)

  #The others could be 0, depending on the fire, but cant be negative
  structures = ifelse((structures < 0), return("Structures cannot be negative"), structures)
  helicopters = ifelse((helicopters < 0), return("Helicopters cannnot be negative"), helicopters)
  injuries = ifelse((injuries < 0), return("Injuries cannot be negative"), injuries)
  fatalities = ifelse((fatalities < 0), return("Fatalities cannot be negative"), fatalities)

  property = (acres*value) + (structures*avgstructureval) #property damage value is the number of acres burned*the value of that burned land, plus the number of structures damaged/destroyed*the average value of those structures

  aircraft = helicopters*days*100000 #aircraft expenses are the number of helicopters used*the number of days*the cost of one day of helicopter use($100,000)

  labor = firecrews*20*days*480 #labor expenses are the number of firecrews*the avg number of people per firecrew(20)*the number of days spent fighting the fire*the number of hours per day spent fighting (12)*the average hourly wage of a firefighter(40)

  injury = (injuries + fatalities)*50000 #injury expenses are the number of injuries + number of fatalities * the average costs of injuries(50000)

  damage = property + aircraft + labor + injury #total damage is all of the calculations summed

  return(damage) #return the total number in $
}

#test
firedamage(acres = 100, value = 5000, structures = 20, avgstructureval = 100000, helicopters = 5, days = 5, firecrews = 8, injuries = 8, fatalities= 0)
