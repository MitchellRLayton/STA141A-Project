# Mitchell Layton: 912307956
# STA 141A Homework Assignment #2: Due Tuesday, October 31 by 5:00 pm

# references http://ggplot2.tidyverse.org/reference/index.html  
# http://www.sthda.com/english/wiki/ggplot2-axis-scales-and-transformations
# for ggplot, and plot trasformations and edits

# http://erdavenport.github.io/R-ecology-lesson/04-dplyr.html for R tutorials and basics

# Other references are scattered throughout code right before each piece of codes that needs referencing.

library(tidyverse)
library(ggmosaic)
library(plyr)
library(ggthemes)
library(grid)
library(sqldf)
library(reshape)
library(plotly)
library(scales)
library(car)
library(RColorBrewer)




# Note I commented out my variables p1, p2, p3, p4 which are the residual and Q-Q... etc
# plots of the linear models in 7 and 8 b/c it messed up R script when running whole thing.

set.seed(1234)


airfare <- read_csv("E:/Davis/STA 141A/DATA/airfare.zip")
cpi_1996_2017 <- read_csv("E:/Davis/STA 141A/DATA/cpi_1996_2017.csv")

# 1)

    table_1a = filter(airfare, table %in% c("1a"))
  
    table_6 = filter(airfare, table %in% c("6"))
      
#2) 
    # To find the timespan that the data covers we use the "year" column vector
    
    min_year_1a = min(table_1a$year)
    max_year_1a = max(table_1a$year)
    min_quarter_1a = min(table_1a$quarter)
    max_quarter_1a = max(table_1a$quarter)
    
    min_year_6 = min(table_6$year)
    max_year_6 = max(table_6$year)
    min_quarter_6 = min(table_6$quarter)
    max_quarter_6 = max(table_6$quarter)
    
    min_year = min(cbind(min_year_1a,min_year_6))
    max_year = max(cbind(max_year_1a,max_year_6))
    min_quarter = min(cbind(min_quarter_1a,min_quarter_6))
    max_quarter = max(cbind(max_quarter_1a,max_quarter_6))
    
    cat("The timespan that this data covers is from quarter",min_quarter, min_year,"to quarter", max_quarter, max_year)
    # (1996-2017)
    
    
    # To find any gaps in the "years" or "quarters" column vectors where there are NA's.
    
    NAs_Quarter_1a = table_1a$quarter[is.na(table_1a$quarter)]

    NAs_Quarter_6 = table_6$quarter[is.na(table_6$quarter)]
    
    NAs_Year_1a = table_1a$year[is.na(table_1a$year)]
    
    NAs_Year_6 = table_6$year[is.na(table_6$year)]
    
    # To look for any missing NA's or patterns
    
    table(is.na(table_1a$lg_carrier))
    table(is.na(table_6$lg_carrier))

    table(is.na(table_1a$low_carrier))
    table(is.na(table_6$low_carrier))
    # Notice that table 1a have the most missing values in both of the carrier variables.

    
    
    table(is.na(table_1a$low_fare))
    table(is.na(table_6$low_fare))
    
    table(is.na(table_1a$lg_fare))
    table(is.na(table_6$lg_fare))
    
    
    
    
    table(is.na(table_1a$low_marketshare))
    table(is.na(table_6$low_marketshare))

    table(is.na(table_1a$lg_marketshare))
    table(is.na(table_6$lg_marketshare))
    
    
    
#3)
    # Create a table from table 6 of both city 1 and city 2 and find the max value of each to determine the most frequent cities
    # This tells us that those cities have the most connections with other cities 
    by_year1 = split(table_6$city1, table_6$year, drop = T)
    by_year1 = table(by_year1$`2017`)
    max1 = max(by_year1)
    ATL = names(by_year1[which.max(by_year1)])
    by_year2 = split(table_6$city2, table_6$year, drop = T)
    by_year2 = table(by_year2$`2017`)
    max2 = max(by_year2)
    DC = names(by_year2[which.max(by_year2)])
    cat("\nIn 2017, the cities with the most connections were,",ATL,"and",DC,"with", max1,"and",max2,"connections respectively.")
    
    
    new_table3 = table(table_6$city1)
    new_table4 = table(table_6$city2)
    MN = names(new_table3[which.min(new_table3)])
    MS = names(new_table4[which.min(new_table4)])
    max3 = min(new_table3)
    max4 = min(new_table4)
    cat("\nIn 2017, the cities with the least connections were,",MN,"and",MS,"with", max3,"and",max4,"connections respectively.")
    
    
    # Now we split the data by the city and year using the split() functions based on year 2007.
    # Then we will store name and the count of each variable 
    by_year1 = split(table_6$city1, table_6$year, drop = T)
    by_year1 = table(by_year1$`2007`)
    max1 = max(by_year1)
    ATL = names(by_year1[which.max(by_year1)])
    by_year2 = split(table_6$city2, table_6$year, drop = T)
    by_year2 = table(by_year2$`2007`)
    max2 = max(by_year2)
    DC = names(by_year2[which.max(by_year2)])
    cat("\nIn 2007, the cities with the most connections were,",ATL,"and",DC,"with", max1,"and",max2,"connections respectively.")
    
    
    by_year3 = split(table_6$city1, table_6$year, drop = T)
    by_year3 = table(by_year3$`2007`)
    min1 = min(by_year3)
    GA = names(by_year3[which.min(by_year3)])
    by_year4 = split(table_6$city2, table_6$year, drop = T)
    by_year4 = table(by_year4$`2007`)
    min2 = min(by_year4)
    TX = names(by_year4[which.min(by_year4)])
    cat("\nIn 2007, the cities with the least connections were,",GA,"and",TX,"with", min1,"and",min2,"connections respectively.")
    
  

        
    # Lastly we need to split the data according to the city based on year 1997
    by_year1 = split(table_6$city1, table_6$year, drop = T)
    by_year1 = table(by_year1$`1997`)
    max1 = max(by_year1)
    ATL = names(by_year1[which.max(by_year1)])
    by_year2 = split(table_6$city2, table_6$year, drop = T)
    by_year2 = table(by_year2$`1997`)
    max2 = max(by_year2)
    DC = names(by_year2[which.max(by_year2)])
    cat("\nIn 1997, the cities with the most connections were,",ATL,"and",DC,"with", max1,"and",max2,"connections respectively.")
    
    
    by_year3 = split(table_6$city1, table_6$year, drop = T)
    by_year3 = table(by_year3$`1997`)
    min1 = min(by_year3)
    GA = names(by_year3[which.min(by_year3)])
    by_year4 = split(table_6$city2, table_6$year, drop = T)
    by_year4 = table(by_year4$`1997`)
    min2 = min(by_year4)
    TX = names(by_year4[which.min(by_year4)])
    cat("\nIn 1997, the cities with the least connections were,",GA,"and",TX,"with", min1,"and",min2,"connections respectively.")
    
    
    # Lastly, we need to find the max value the largest increased connectivity
    
    by_city1 = split(table_6$city1, table_6$year, drop = T)
    by_1997_1 = table(by_city1$`1997`)
    by_2017_1 = table(by_city1$`2017`)
    

    by_city2 = split(table_6$city2, table_6$year, drop = T)
    by_1997_2 = table(by_city2$`1997`)
    by_2017_2 = table(by_city2$`2017`)
    
   
    x_1997 = data.frame(count(by_1997_1))
    x_2017 = data.frame(count(by_2017_1))
  
    xx_1997 = data.frame(count(by_1997_2))
    xx_2017 = data.frame(count(by_2017_2))
    
   
# For city 1
city1 = merge(x_1997,x_2017, by = "x.Var1")    
# To delete the unneccesary columns
drops = c("freq.x", "freq.y")
city1 = city1[ , !(names(city1) %in% drops)]
colnames(city1) = c("City/State", "    (YEAR) 1997", "    (YEAR) 2017")

city1$increased_connections = (city1$`    (YEAR) 2017` - city1$`    (YEAR) 1997`)
city1$increased_connections = abs(city1$increased_connections)
city1 = city1 %>%
            arrange(desc(increased_connections)) 

biggest_city1 = head(city1, 5)
biggest_city1


# For city2
city2 = merge(xx_1997,xx_2017, by = "x.Var1")    
# To delete the unneccesary columns
drops = c("freq.x", "freq.y")
city2 = city2[ , !(names(city2) %in% drops)]
colnames(city2) = c("City/State", "    (YEAR) 1997", "    (YEAR) 2017")

city2$increased_connections = (city2$`    (YEAR) 2017` - city2$`    (YEAR) 1997`)
city2$increased_connections = abs(city2$increased_connections)
city2 = city2 %>%
  arrange(desc(increased_connections)) 

biggest_city2 = head(city2, 5)
biggest_city2

#4) 
      
    # Aggregate function is best here because it is like tapply but the output is a data frame where we specify our numerical variable first followed by the categorical variables and the FUN function to apply    

    agg_funct_cities = aggregate(passengers ~ quarter + year, table_6, sum)
    
    agg_funct_airports = aggregate(passengers ~ quarter + year, table_1a, sum)
    
    agg_funct = aggregate(passengers ~ quarter + year, airfare, sum)
    
    

    
    # plot function using ggplot and the 2d density geom function. We set the data equal to our agg_function for citites and thenn pass the year on the x axis, and passengers on the y to the aes().
    # Then I faceted the data based on the quarters so we can see the yearly trends of each of the quarters of passnegers over time. 
      plot1 = ggplot(data = agg_funct, aes(year, passengers)) +
          geom_density_2d(stat="identity") +
          facet_wrap(~ quarter, nrow = 2) +
              labs(x = "Year", y = "Approximate Total # of Passengers", title = "Breakdown of Distinct Quarterly Total Passengers Over the Years (Cities & Airports)") + 
                theme(plot.title = element_text(size = rel(1.65))) +
                theme(axis.title.y = element_text(size = rel(1.77))) +
                theme(axis.text.x = element_text(size = rel(1.75))) + 
                theme(axis.text.x=element_text(angle=25, hjust= .5)) +
                theme(axis.title.x = element_text(size = rel(1.85))) +
                theme(axis.text.y = element_text(size = rel(1.35))) +
                theme(axis.ticks.length=unit(0.25,"cm")) +
                theme(axis.ticks = element_line(size = rel(2))) +
                theme(plot.margin=unit(c(1,1,1.5,1.2),"cm")) + 
                theme(strip.text.x = element_text(size = 17, colour = "red"))

      plot1
      
      plot2 = ggplot(data = agg_funct, aes(factor(year), passengers)) +
               geom_violin(scale = "count", adjust = 1.25, aes(fill = factor(year)), draw_quantiles = c(0.25, 0.5, 0.75)) + 
                       labs(x = "Year", y = "Approximate Total # of Passengers", title = "Violin Plot of Total Passengers over time (with quantiles)") + 
                         theme_few() + theme(plot.title = element_text(size = rel(1.65))) +
                         theme(axis.title.y = element_text(size = rel(1.77))) +
                         theme(axis.text.x = element_text(size = rel(1.75))) + 
                         theme(axis.text.x=element_text(angle=25, hjust= .5)) +
                         theme(axis.title.x = element_text(size = rel(1.85))) +
                         theme(axis.text.y = element_text(size = rel(1.35))) +
                         theme(axis.ticks.length=unit(0.25,"cm")) +
                         theme(axis.ticks = element_line(size = rel(2))) +
                         theme(plot.margin=unit(c(1,1,1.5,1.2),"cm")) + 
                         theme(strip.text.x = element_text(size = 22, colour = "red", angle = 22.5)) +
                         theme(legend.title = element_text(size = rel(1.65)))
      
      plot2
      
#5) 
      # Learned of the reshape() methodology of this problem from Piazza about question 5 from anonymous
      # Prefer SQL statements sometimes because of my experience from last summer internship
      temp = sqldf("SELECT year, quarter, fare FROM airfare")
      # Remap factor levels for easier merge and join
      temp$quarter = mapvalues(temp$quarter, from = c("1", "2", "3", "4"), to = c("Q1", "Q2", "Q3", "Q4"))
      
      
      # Method: Found CPI for Q1 - Q4 by taking average of 3 month groupings
      cpi_1996_2017$Q1 = ((cpi_1996_2017$Jan) + (cpi_1996_2017$Feb) + (cpi_1996_2017$Mar)) / 3
      cpi_1996_2017$Q2 = ((cpi_1996_2017$Apr) + (cpi_1996_2017$May) + (cpi_1996_2017$Jun)) / 3
      cpi_1996_2017$Q3 = ((cpi_1996_2017$Jul) + (cpi_1996_2017$Aug) + (cpi_1996_2017$Sep)) / 3
      cpi_1996_2017$Q4 = ((cpi_1996_2017$Oct) + (cpi_1996_2017$Nov) + (cpi_1996_2017$Dec)) / 3
      
      # To delete certain columns
      drops = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Oct", "Nov", "Dec", "HALF1", "HALF2")
      cpi_1996_2017 = cpi_1996_2017[ , !(names(cpi_1996_2017) %in% drops)]
      
      cpi_1996_2017 = reshape(cpi_1996_2017, varying = c("Q1","Q2","Q3","Q4"), v.names = "CPI", direction = "long")
      
      colnames(cpi_1996_2017) = c("year","Sep","quarter","CPI","id")
      drops = c("id")
      cpi_1996_2017 = cpi_1996_2017[ , !(names(cpi_1996_2017) %in% drops)]
      
      
      
      CPI_b = sqldf("SELECT Sep FROM cpi_1996_2017 WHERE Year = 2017")
      CPI_b = as.double(head(CPI_b, 1))
      
      drops = c("Sep")
      cpi_1996_2017 = cpi_1996_2017[ , !(names(cpi_1996_2017) %in% drops)]
      
      # Creating new altered name for airfare so to not affect and get rid of the table 1a observations since I am merging the cpi_1996_2017 data with table_6 data only.
      airfare_merged = merge(cpi_1996_2017, table_6, by = c("year", "quarter"), all.y = T)
      
      airfare_merged$real17_fare = (airfare_merged$fare)*(CPI_b/(airfare_merged$CPI))

#6)
      # We want real Q1 2017 airfare fares over time
      
      fares_over_time = sqldf("SELECT year, real17_fare FROM airfare_merged ORDER BY year")
      
      box_plot = ggplot(data = airfare_merged, aes(x = factor(airfare_merged$year), y = airfare_merged$real17_fare)) +
        geom_boxplot() + 
        labs(x = "Year", y = "Real Q1 2017 Airfares in dollars ($)", title = "Airfares over time Boxplot") + 
        theme_few() + theme(plot.title = element_text(size = rel(1.65))) +
        theme(axis.title.y = element_text(size = rel(1.77))) +
        theme(axis.text.x = element_text(size = rel(1.75))) + 
        theme(axis.title.x = element_text(size = rel(1.85))) +
        theme(axis.text.y = element_text(size = rel(1.35))) +
        theme(axis.ticks.length=unit(0.25,"cm")) +
        theme(axis.ticks = element_line(size = rel(2))) +
        theme(plot.margin=unit(c(1,1,1.5,1.2),"cm")) + 
        theme(strip.text.x = element_text(size = 22, colour = "red", angle = 22.5)) +
        theme(legend.title = element_text(size = rel(1.65)))
      
      box_plot
          
   
    
#7)
      # references: http://t-redactyl.io/blog/2016/05/creating-plots-in-r-using-ggplot2-part-11-linear-regression-plots.html
      #             http://www.sthda.com/english/wiki/ggplot2-axis-scales-and-transformations
    # Part 1: Find a relationship between fare and distance for year 2015 using an appropriate statistical model or test
        # reload airfare to ensure all table values are there
        
        relationship_15 = sqldf("SELECT year, miles, fare, `table` FROM airfare WHERE year = 2015 AND`table` LIKE '%1a%'")
        
        model_q7 = lm(relationship_15$fare ~ relationship_15$miles) 
      
        summary(model_q7)
        # p1 = plot(model_q7)
          
        reg_plot1 = ggplot(data = relationship_15, aes(x = relationship_15$miles, y = relationship_15$fare)) + 
                      geom_point(shape = 1) + geom_smooth(method = lm) +
                      labs(title = "(Table 1a) Regression Plot: 2015 Airfares ($) vs. Distance (Miles)") +
                      scale_x_continuous(name = "Distance (miles)", breaks = round(seq(min(relationship_15$miles) + 2, max(relationship_15$miles) + 2, by = 350),5)) +
                      # changing axis scales and tick amount referece: https://stackoverflow.com/questions/11335836/increase-number-of-axis-ticks
                      scale_y_continuous(name = "2015 Airfares ($)", breaks = round(seq(min(relationship_15$fare), max(relationship_15$fare), by = 150),0), labels = dollar) +
                        theme_few() + theme(plot.title = element_text(size = rel(1.49))) +
                        theme(axis.title.y = element_text(size = rel(1.77))) +
                        theme(axis.text.x = element_text(size = rel(1.75))) + 
                        theme(axis.title.x = element_text(size = rel(1.85))) +
                        theme(axis.text.y = element_text(size = rel(1.35))) +
                        theme(axis.ticks.length=unit(0.25,"cm")) +
                        theme(axis.ticks = element_line(size = rel(2))) +
                        theme(plot.margin=unit(c(1,1,1.5,1.2),"cm")) + 
                        theme(strip.text.x = element_text(size = 22, colour = "red", angle = 22.5)) 
                    
        reg_plot1 = reg_plot1 + annotate("rect", xmin = 825, xmax = 1165, ymin = 985, ymax = 1065, fill="white", colour="red") +
                             annotate("text", x = 1000, y = 1050, label = "R^2 == 0.3115", colour = "blue", parse = T) +
                             annotate("text", x = 1000, y = 1025, label = "alpha == 177.5", colour = "blue", parse=T) +
                             annotate("text", x = 1000, y = 1000, label = "beta == 0.0587", colour = "blue", parse=T)
        reg_plot1
      
        
        
        
      
        relationship_6_15 = sqldf("SELECT year, miles, fare, `table` FROM airfare WHERE year = 2015 AND `table` LIKE '%6%'")
        
        model_q7_6 = lm(relationship_6_15$fare ~ relationship_6_15$miles) 
        
        summary(model_q7_6)
        # p2 = plot(model_q7_6)
        
        
        reg_plot2 = ggplot(data = relationship_6_15, aes(x = relationship_6_15$miles, y = relationship_6_15$fare)) + 
                      geom_point(shape = 1) + geom_smooth(method = lm) +
                      labs(title = "(Table 6) Regression Plot: 2015 Airfares ($) vs. Distance (Miles)") +
                      scale_x_continuous(name = "Distance (miles)", breaks = round(seq(min(relationship_6_15$miles) + 3, max(relationship_6_15$miles) + 3, by = 350),5)) +
                      # changing axis scales and tick amount referece: https://stackoverflow.com/questions/11335836/increase-number-of-axis-ticks
                      scale_y_continuous(name = "2015 Airfares ($)", breaks = round(seq(min(relationship_6_15$fare) - 3, max(relationship_6_15$fare) - 3, by = 150),0), labels = dollar) +
                        theme_few() + theme(plot.title = element_text(size = rel(1.49))) +
                        theme(axis.title.y = element_text(size = rel(1.77))) +
                        theme(axis.text.x = element_text(size = rel(1.75))) + 
                        theme(axis.title.x = element_text(size = rel(1.85))) +
                        theme(axis.text.y = element_text(size = rel(1.35))) +
                        theme(axis.ticks.length=unit(0.25,"cm")) +
                        theme(axis.ticks = element_line(size = rel(2))) +
                        theme(plot.margin=unit(c(1,1,1.5,1.2),"cm")) + 
                        theme(strip.text.x = element_text(size = 22, colour = "red", angle = 22.5)) 
                    
        
        reg_plot2 = reg_plot2 + stat_ellipse(colour = "red") + annotate("rect", xmin = 805, xmax = 1185, ymin = 885, ymax = 965, fill="white", colour="red") +
                              annotate("text", x = 1000, y = 950, label = "R^2 == 0.2347", colour = "blue", parse = T) +
                              annotate("text", x = 1000, y = 925, label = "alpha == 210.8", colour = "blue", parse=T) +
                              annotate("text", x = 1000, y = 900, label = "beta == 0.0568", colour = "blue", parse=T)
        
        
        reg_plot2
      
  
      
  
#8)
        
        # Scatterplot matrices reference: https://www.statmethods.net/graphs/scatterplot.html
        scatter_data1 = sqldf("SELECT fare, miles, passengers FROM airfare WHERE year = 2015 AND `table` LIKE '%1a%'")
        # From library car: gives us a matrix of plots which can compare variables and their histrograms
        
        y = scatter_data1$fare # Outcome variable
        x = scatter_data1$miles # Predictor 1
        z = scatter_data1$passengers # Predictor 2
        
        # Pearson coefficient will tell us how strong the collinearity is between our two variables to determine if we should intrept the slope of y.
        cor(x, z, method = "pearson")
        
        # Model with 2 predictor variables
        multi_variate1 = lm(y ~ x + z)
        summary(multi_variate1)
        # p3 = plot(multi_variate1)
        
        # Model with 2 predictor variables and the interaction term.
        multi_variate2 = lm(y ~ x + z + x:z)
        summary(multi_variate2)
        # p4 = plot(multi_variate2)
        
        scatterplotMatrix(scatter_data1)

        
# Repeat above operations on table 6
        scatter_data2 = sqldf("SELECT fare, miles, passengers FROM airfare WHERE year = 2015 AND `table` LIKE '%6%'")
        y1 = scatter_data2$fare # Outcome variable
        x1 = scatter_data2$miles # Predictor 1
        z1 = scatter_data2$passengers # Predictor 2
        
        cor(x1, z1, method = "pearson")
        
        # Model with 2 predictor variables
        multi_variate3 = lm(y1 ~ x1 + z1)
        summary(multi_variate3)
        # p3 = plot(multi_variate1)
        
        # Model with 2 predictor variables and the interaction term.
        multi_variate4 = lm(y1 ~ x1 + z1 + x1:z1)
        summary(multi_variate4)
        # p4 = plot(multi_variate2)
        
        scatterplotMatrix(scatter_data2)
        
#9)
      # We want 2015 pairs of cities where the carrier with largest market share has fares below the average for that city pair
        
      # Thought Process: Select piars of cities, 
      # find the carrier(s) with largest mkt share that has fares below calculated avg of 2 citites mean fares
        
        
        select_pairs = sqldf("SELECT city1, city2, fare, lg_carrier, lg_fare FROM table_6 WHERE year = 2015 ORDER BY lg_carrier")
        answer = sqldf("SELECT * FROM select_pairs WHERE lg_fare < fare")
        
        hist(answer$lg_fare)
        hist(answer$fare)
        
        # Closely equal variances
        v1 = var(answer$lg_fare)
        v2 = var(answer$fare)
        
        
        # 2 sample T-test refernces: https://statistics.berkeley.edu/computing/r-t-tests
      
        t.test(answer$fare, answer$lg_fare, alternative = "two.sided", paired = T)

 
  
        ggplot(data = answer, aes(lg_fare))+ 
            stat_density(aes(group = answer$lg_carrier, color = answer$lg_carrier), position =  "identity", geom = "line")


        ggplot(data = answer, aes(fare))+ 
          stat_density(aes(group = answer$lg_carrier, color = answer$lg_carrier), position =  "identity", geom = "line")
        

        
#10) 

     q_10_1 =  sqldf("SELECT city1 as q10_CA_cities, city2 as Others, fare, miles, year FROM table_1a WHERE city1 LIKE '%Sacramento%' OR city1 LIKE '%San Francisco%' OR city1 LIKE '%Oakland%' OR city1 LIKE '%San Jose%' GROUP BY fare, city1 ORDER BY city1")
     q_10_2 =  sqldf("SELECT city2 as q10_CA_cities, city1 as Others, fare, miles, year FROM table_1a WHERE city2 LIKE '%Sacramento%' OR city2 LIKE '%San Francisco%' OR city2 LIKE '%Oakland%' OR city2 LIKE '%San Jose%' GROUP BY fare, city2 ORDER BY city2")
     q_10_main = rbind(q_10_1, q_10_2)
     
     # NOTE: Both San Jose and Oakland were not found in either city1 or city2 for table_1a
     
     sqldf("SELECT city1, city2 FROM table_1a WHERE city1 LIKE '%Oakland%' OR city2 LIKE '%Oakland%'")
     sqldf("SELECT city1, city2 FROM table_1a WHERE city1 LIKE '%San Jose%' OR city2 LIKE '%San Jose%'")
     
     # Fares differeing between airports.
     
     fare_differences = aggregate(fare ~ q10_CA_cities, q_10_main, mean)
        # mean of fares shows that Sac is cheaper on average by about $10
    
     distance_differences = aggregate(miles ~ q10_CA_cities, q_10_main, mean)
        # mean distances in airports: even though Sac has slightly higher mean, below will show SF has most long distance connections.
     long_dist_connect = sqldf("SELECT q10_CA_cities, Others, miles, count(miles) as Num_Connections FROM q_10_main WHERE miles = 2704 ORDER BY miles DESC")
        # shows that SF has the most longest distance connections from SF to Boston MA at 2704 miles occuring 755 times.
     long_dist_connect_1 = sqldf("SELECT q10_CA_cities, Others, miles, count(miles) as Num_Connections FROM q_10_main WHERE q10_CA_cities LIKE '%Sacramento%' ORDER BY miles DESC")
        # shows Sac's distance connections from Sac to NY at 2553 miles with 2341 connections
     
     # Previous (first qustions) analysis but depending upon years now:
     
     fare_differences_1 = aggregate(fare ~ q10_CA_cities + year, q_10_main, mean)
     sac = sqldf("SELECT * FROM fare_differences_1 WHERE q10_CA_cities LIKE '%Sacramento%'") 
     sf = sqldf("SELECT * FROM fare_differences_1 WHERE q10_CA_cities LIKE '%San Fran%'") 
      
     # plot of sac data: fare vs year
     sac_plot = ggplot(data = sac, aes(x = year, y = fare)) + 
       geom_point(shape = 1) + geom_smooth() +
       labs(title = "Sacramento Plot (table 1a): Fares ($) vs. Time (years)", x = "Year", y = "Airfares ($)") +
           theme_few() + theme(plot.title = element_text(size = rel(1.49))) +
           theme(axis.title.y = element_text(size = rel(1.77))) +
           theme(axis.text.x = element_text(size = rel(1.75))) + 
           theme(axis.title.x = element_text(size = rel(1.85))) +
           theme(axis.text.y = element_text(size = rel(1.35))) +
           theme(axis.ticks.length=unit(0.25,"cm")) +
           theme(axis.ticks = element_line(size = rel(2))) +
           theme(plot.margin=unit(c(1,1,1.5,1.2),"cm")) + 
           theme(strip.text.x = element_text(size = 22, colour = "red", angle = 22.5)) 
         sac_plot
     
     # plot of sf data: fare vs year
     sf_plot = ggplot(data = sf, aes(x = year, y = fare)) + 
       geom_point(shape = 1) + geom_smooth() +
       labs(title = "San Francisco Plot (table 1a): Fares ($) vs. Time (years)", x = "Year", y = "Airfares ($)") +
           theme_few() + theme(plot.title = element_text(size = rel(1.49))) +
           theme(axis.title.y = element_text(size = rel(1.77))) +
           theme(axis.text.x = element_text(size = rel(1.75))) + 
           theme(axis.title.x = element_text(size = rel(1.85))) +
           theme(axis.text.y = element_text(size = rel(1.35))) +
           theme(axis.ticks.length=unit(0.25,"cm")) +
           theme(axis.ticks = element_line(size = rel(2))) +
           theme(plot.margin=unit(c(1,1,1.5,1.2),"cm")) + 
           theme(strip.text.x = element_text(size = 22, colour = "red", angle = 22.5)) 
         sf_plot
         
     
     
     
     
     
     
     
     
     
     
     