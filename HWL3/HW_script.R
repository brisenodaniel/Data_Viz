setwd("..")

#create dataframe from task 5
ccA = read.csv("./Data/countrycharsA.csv")
ccB = read.csv("./Data/countrycharsB.csv")
gdp = read.csv("./Data/gdp.csv")
ccAB = rbind(ccA,ccB)
ccAB_gdp = cbind(ccAB, gdp)

#load in coords dataframe
load("./Data/map.coords.RData")

countryData_inner = merge(ccAB_gdp, coords, by = "country")
countryData_outer = merge(ccAB_gdp, coords, by = "country", all = T)
countryData_left = merge(ccAB_gdp, coords, by = "country", all.x=T)
countryData_right = merge(ccAB_gdp, coords, by = "country", all.y=T)

q = quantile(countryData_inner$gdp)
interval = findInterval(countryData_inner$gdp, q, all.inside = T)
gdp.q = factor(interval)
gdp.q_levels = table(levels(gdp.q))
View(gdp.q_levels)


#reverse order of factor levels
gdp.q = factor(gdp.q, levels = rev(levels(gdp.q)))
gdp.q_levels = table(levels(gdp.q))
View(gdp.q_levels)

#find mean latitude for each quartile
countryData_q = cbind(countryData_inner,"gdpQuartile" = gdp.q)

q1_latitude = mean(abs(countryData_q[countryData_q$gdpQuartile == 1, "lat"]))
q2_latitude = mean(abs(countryData_q[countryData_q$gdpQuartile == 2, "lat"]))
q3_latitude = mean(abs(countryData_q[countryData_q$gdpQuartile == 3, "lat"]))
q4_latitude = mean(abs(countryData_q[countryData_q$gdpQuartile == 4, "lat"]))

mean_latitudes = c(q1_latitude,q2_latitude,q3_latitude,q4_latitude)
mean_latitudes_tbl = table(mean_latitudes)
View(mean_latitudes_tbl)