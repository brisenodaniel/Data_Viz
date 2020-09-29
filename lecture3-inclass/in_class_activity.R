#Daniel Briseno Servin
#SID:2282050
#Partner: Riley Kendall
#Q1
dir.create("./lecture3-inclass")
dir.create("./lecture3-inclass/data")
setwd("./lecture3-inclass")

#Q2
ccA = read.csv("./data/countrycharsA.csv")
ccB = read.csv("./data/countrycharsB.csv")
gdp = read.csv("./data/gdp.csv")

#Q3
str(ccA)
str(ccB)
str(gdp)


#Q4
ccAB = rbind(ccA,ccB)
str(ccAB)

#Q5
ccAB_gdp = cbind(ccAB, gdp)
str(ccAB_gdp)
#6
typeof(ccAB_gdp$country[1])
typeof(ccA$country[1])
typeof(ccB$country[1])

#7 
nrow(ccAB_gdp[ccAB_gdp$year > 1980 & ccAB_gdp$gdp<20000, ])

#8
results = data.frame()
for(ii in ccAB_gdp$year){
  #subset gdp data for relevant year, obtain median gdp
  year_data = ccAB_gdp[ccAB_gdp$year==ii,"gdp"]
  med = median(year_data)
  
  #compute squared difference of each gdp to median
  median_vec = rep(med,length(year_data))#vector containing the median gdp at all indices and of equal length as year data
  diff_squared = (year_data - median_vec)**2#compute difference squared
  
  
  #find index of country nearest the median
  gdp_index = which.min(diff_squared) #index of gdp in year_data vector closest to the median 
  country_index = which(ccAB_gdp$year==ii & ccAB_gdp$gdp == year_data[gdp_index])#index of country with gdp closest to median in the given year

  #construct results vector
  r_vect = data.frame(year = ii,
                      med,
                      ccAB_gdp[country_index,]$country,
                      ccAB_gdp[country_index,]$continent,
                      ccAB_gdp[country_index,]$gdp)
  results = rbind(results,r_vect)
}


#9 
results_by_contintent = c()
for(cont in unique(ccAB_gdp$continent)){
  continent_df = ccAB_gdp[ccAB_gdp$continent == cont,]
  results = data.frame()
  for(ii in continent_df$year){
  
  #subset gdp data for relevant year, obtain median gdp
  year_data = continent_df[continent_df$year==ii,"gdp"]
  med = median(year_data)
  
  #compute squared difference of each gdp to median
  median_vec = rep(med,length(year_data))#vector containing the median gdp at all indices and of equal length as year data
  diff_squared = (year_data - median_vec)**2#compute difference squared
  
  
  #find index of country nearest the median
  gdp_index = which.min(diff_squared) #index of gdp in year_data vector closest to the median 
  country_index = which(ccAB_gdp$year==ii & 
                          ccAB_gdp$gdp == year_data[gdp_index] &
                          ccAB_gdp$continent == cont)#index of country with gdp closest to median in the given year
  
  #construct results vector
  r_vect = data.frame(ccAB_gdp[country_index,]$continent,
                      year = ii,
                      med,
                      ccAB_gdp[country_index,]$country,
                      ccAB_gdp[country_index,]$gdp)
  results = rbind(results,r_vect)
  }
  results_by_contintent = append(results_by_contintent,results)
}
