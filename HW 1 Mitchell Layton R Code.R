#make sure to install packages in order to run
library(tidyverse)
library(ggmosaic)
library(plyr)
library(ggthemes)
library(grid)

#STA 141A -- Homework Assignment 1

# R Scripts:

# 1) Using dplyr package we can use either the count() OR n_distinct() function.


    #Read in the file into a variable I names college13
        college13 <- readRDS("E:/R Files/college_scorecard_13.rds")
        
        total_observations <- sum(is.na(college13) == TRUE) + sum(is.na(college13) == FALSE)
        cat("Total Observations: ", total_observations)
        
    #total_colleges <- count(college13, college13$ope_id)
    # OR USE
        total_colleges <- sum(is.na(college13$ope_id) == FALSE)
        cat("Total Colleges Recorded: ", total_colleges)
        
# 2) 
        total_features = ncol(college13)
        cat("Total Features: ", total_features)
        
    #For more information regarding the data.frame we use str() functions
        data_info = str(college13)
        
    # For finding out how many of the vectors in our data.frame() are 
    # categorical, discrete, or any other features we use the following
        
        class_table = table(sapply(college13, class))
        class_table
    # This shows a contigency table using our data frame which selects each 
    # count of the combination of factor levels
        
    #Set each index of our atomic vectors of the contigency table to display our results
        
        chr_count = class_table[1]
        factor_count = class_table[2]
        int_count = class_table[3]
        logi_count = class_table[4]
        num_count = class_table[5]
      
cat("There are",(chr_count + logi_count + factor_count), "categorical vectors in the data frame.")
cat("Given that a discrete variable is a numberic variable where observations can take on
a value based on counts from a set of distinct whole values,
there is really only 1 discrete vector in the data frame which is 'Branches'.")
    
# 3)
    # Total NA in data set
        
        total_NA = sum(is.na(college13) == TRUE)
        cat("There are",total_NA,"missing values in the data set")
        
    # To find the column vector in the data frame with the largest number of NA's we use:
        
        missing_values = max(sapply(college13, function(y) sum(is.na(y))))
        most_NA = data.frame(missing_values, row.names = 'avg_sat')
        cat("Here is the feature with most missing values:")
        most_NA
        
    # Explanation of code: Starting with the "sapply" which applies a particular function
    # to every column vector of our college13. We then specify the function(y) and create it.
    # This function finds the missing values (NA) and sums up the amount for each object.
    # Then, using the max() function we find the largest number of NA.
    # Last, I created a 1x1 data.frame() to display the avg_sat and its corresponding missing values.
        
        table_NA_plot = sort(table(sapply(college13, function(y) sum(is.na(y)))))
        table_NA_plot
        mosaicplot(table_NA_plot, main = "Mosaic Plot: Missing Values (NA)", sub = "Occurances of NA's (Ascending)", off = 35, color = TRUE, cex.axis = 1.05)
        
# 4) 
    # More public or private colleges recorded: 
        
        type_of_college = table(college13$ownership)
        type_of_college
        
        
    # Proportion of highes degree awarded code:
        
         ggplot(data = college13) + 
          geom_mosaic(aes(x = product(college13$highest_degree, college13$ownership), fill = factor(college13$highest_degree)), na.rm = TRUE, offset = 0.012) +
          labs(x = "Ownership", y = "Proportion of Degree Awarded (# of specific degree ÷ total degrees)", title = "Proportions of Highest Degree Awarded") + 
          guides(fill = guide_legend(title = "Degrees Offered", reverse = TRUE)) +
            theme(plot.title = element_text(size = rel(2))) +
            theme(axis.title.y = element_text(size = rel(1.25))) +
            theme(axis.text.x = element_text(size = rel(1.75))) + 
            theme(axis.text.x=element_text(angle=25, hjust= .5)) +
            theme(axis.title.x = element_text(size = rel(1.85))) +
            theme(axis.text.y = element_text(size = rel(1.35))) +
            theme(axis.ticks.length=unit(0.25,"cm")) +
            theme(axis.ticks = element_line(size = rel(2))) +
            theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))

          
  
    # REFERENCES: https://cran.r-project.org/web/packages/ggmosaic/vignettes/ggmosaic.htm AND R for Data Science.

    # Explanation of code: We start by using the ggplot() with its large amount of graphical packages.
    # From ggplot() we use the recently added ggmosaic package. From here the syntax is to call the ggplot function
    # specifying the data we want to use, in this case college13. Then we set the particular <GEOM_FUNCTION> we desire in 
    # this case mosaic(). Then we specify the mapping arguement aes() to specify what variables to map.
    # In our mapping funciton we specify the x object to be a product (a wrapper for a list) 
    # of our highest degree data and the ownership. The fill() fills in missing observations from the last non-missing value.
    # Then we handle the spacing of the x factors with offset(). Lastly, we handle all adjustments to our plot using theme()
    # The theme package offers so many adjustable factors and you just need to specify what and with which parameters.
        
        
# 5)
    # Average undergraduate population: 
         
         avg_undergraduate_pop = round(mean(college13$undergrad_pop, na.rm = TRUE))
         cat("The mean undergraduate population is:",avg_undergraduate_pop)
         med_undergraduate_pop = round(median(college13$undergrad_pop, na.rm = TRUE))
         cat("The median undergraduate population is:",med_undergraduate_pop)
        
    # Decile:
        
         decile_undergrad_pop = quantile(college13$undergrad_pop, prob = seq(.1:1, by = .1), na.rm = TRUE)
         (decile_undergrad_pop)
        
        
# 6) 
      # According to, https://www.census.gov/newsroom/press-releases/2014/cb14-232.html the 5 most populous states are:
      # (In order) California, Texas, Florida, New York, Illinois
        
         ca = filter(college13, state %in% c("CA"))  
         CA = cbind(quantile(ca$tuition, na.rm = TRUE))
        
         tx = filter(college13, state %in% c("TX"))  
         TX = quantile(tx$tuition, na.rm = TRUE)
         
         fl = filter(college13, state %in% c("FL"))  
         FL = quantile(fl$tuition, na.rm = TRUE)
         
         ny = filter(college13, state %in% c("NY"))  
         NY = quantile(ny$tuition, na.rm = TRUE)
         
         il = filter(college13, state %in% c("IL"))  
         IL = quantile(il$tuition, na.rm = TRUE)
         
         quantile_5_states = data.frame(cbind(CA,TX,FL,NY,IL))
                                        
         quantile_5_states
         
         
         # Index our data frame to just filter based on top 5 states
         populous_states_data = filter(college13, state %in% c("CA","TX","FL","NY","IL"))
        
         # Then we explicity set the factors (the 5 states) and their levels to be in the order we want. 
         # In this case I order them from left to right from the most populous to the least populous.
         populous_states_data$state <- factor(populous_states_data$state, levels = c("CA","TX","FL","NY","IL"))
         
         
        # Since our first feature is tuition (numerical) and second feature is state (categorical)
        # lets use either a box plot:
        # references: http://t-redactyl.io/blog/2016/04/creating-plots-in-r-using-ggplot2-part-10-boxplots.html
         
        tuition_boxplot = ggplot(data = populous_states_data) +
                              geom_boxplot(aes(x = populous_states_data$state, y = populous_states_data$tuition), na.rm = TRUE, outlier.color = "red", ) +
                              labs(x = "5 Most Populous States", y = "Tuition ($)") + theme_bw() + ggtitle("Tuition Boxplot for 5 Most Populous States") +
                                  theme(plot.title = element_text(hjust = 0.5)) +
                                  theme(plot.title = element_text(size = rel(2))) +
                                  theme(axis.title.y = element_text(size = rel(1.25))) +
                                  theme(axis.text.x = element_text(size = rel(1.75))) + 
                                  theme(axis.text.x=element_text(angle=25, hjust= .625)) +
                                  theme(axis.title.x = element_text(size = rel(1.45))) +
                                  theme(axis.text.y = element_text(size = rel(1.45))) +
                                  theme(axis.ticks.length=unit(0.25,"cm")) +
                                  theme(axis.ticks = element_line(size = rel(2))) +
                                  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))

        # The themes I used from top to bottomt were to, center adjust the ggtitle() using plot.title and shifting the element text.
        # Then I increased all 3 titles text size, as well as tilting the x axis title for nice visuals
        # Furthermore, I increased the ticks sick and length.
          
         tuition_boxplot
         
         
#7)

         # Going to create a graph with x = spending per student (by colleges), y = student 10-year earnings for each ownership factor level.
         
          #step 1: filter 3 instances with respect to ownership for entire data set.
         
         public_data = filter(college13, ownership %in% "Public")
         nonprofit_data = filter(college13, ownership %in% "Nonprofit")
         private_data = filter(college13, ownership %in% "For Profit")
         
         #PUBLIC
         spend_per_stud_public = public_data$spend_per_student
         avg_10yr_public = public_data$avg_10yr_salary
         scatter.smooth(x = spend_per_stud_public, y = avg_10yr_public, xlab = "Spending per Student (Public Colleges)", ylab = "Average 10 Year Salary (Public)", main = "Public Avg 10 Yr Salary vs. Public Colleges Spending per Student")
         
         #NONPROFIT
         spend_per_stud_nonprofit = nonprofit_data$spend_per_student
         avg_10yr_nonprofit = nonprofit_data$avg_10yr_salary
         scatter.smooth(x = spend_per_stud_nonprofit, y = avg_10yr_nonprofit, xlab = "Spending per Student (Nonprofit Colleges)", ylab = "Average 10 Year Salary (Nonprofit)", main = "Nonprofit Avg 10 Yr Salary vs. Nonprofit Colleges Spending per Student")
         
         #FOR PROFIT
         spend_per_stud_private = private_data$spend_per_student
         avg_10yr_private = private_data$avg_10yr_salary
         scatter.smooth(x = spend_per_stud_private, y = avg_10yr_private, xlab = "Spending per Student (Private Colleges)", ylab = "Average 10 Year Salary (Private)", main = "Private Avg 10 Yr Salary vs. Private Colleges Spending per Student")
         
         
         college13_spend_per_stud = college13$spend_per_student
         scatter.smooth(x = college13_spend_per_stud, y = college13$avg_10yr_salary, xlab = "Spending per Student", ylab = "Average 10 Year Salary", main = "Avg 10 Yr Salary vs. Colleges Spending per Student")
         
#8)
         
  
    
    college13 = college13 %>% mutate(
      ratio = (college13$med_10yr_salary/college13$net_cost) 
    )
    
        ratio_max = max(college13$ratio, na.rm = TRUE)
        ratio_max
        ratio_max = filter(college13, ratio_max == ratio)
        ratio_max = ratio_max$name
        
    cat("The largest ratio of median 10-year salary to cost is:   ",ratio_max)

    
    
#9)
    
    # Want to to detemine colleges that are most racially diverse. We will do so by checking the % of each race within each college and identifying
    # the lowest standard deviations which means the data will be closest together meaning more diversity.
  white = college13$race_white
  black = college13$race_black
  asian = college13$race_asian
  hispanic = college13$race_hispanic
  native = college13$race_native
  pacific = college13$race_pacific
  other = college13$race_other
  

  race_data = data.frame(white,black,asian,hispanic,native,pacific,other)

    #to add the variance as an additional column in the data frame
    college13$variance = apply(race_data,1,function(row) var(as.vector(row[1:7])))

    #Now we add the standard deviation variable into college13, then sort it and get if of the zeros
    college13$sd = sqrt(college13$variance)
    sorted = sort(college13$sd)
    sorted = sorted[sorted > 0]
    # Then we select top 5 uniersities (which I thought was a good idea to grab the top 5 from one of the classmates)
    top_5_diverse = sorted[1:5]
    # Filter our data based on the top_5_diverse standard deviation data points in our college13 data to give us all accompanying data for those 5.
    diversity_1= filter(college13, sd %in% top_5_diverse)
    diversity_2 = diversity_1 %>% select(name,race_white,race_black,race_hispanic,race_asian,race_native,race_other,sd) %>% arrange(sd)
    diversity_2
    
    
    
    
    
    
#10)
    require(data.table)
    UC = college13 %>% filter(name %like% 'University of California')
    UC %>% select(name, undergrad_pop, cost, spend_per_student, retention, completion, med_10yr_salary, sd)
    
    require(data.table)
    state_col = college13 %>% filter(name %like% 'State')
    state_col %>% select(name, undergrad_pop, cost, spend_per_student, retention, completion, med_10yr_salary, sd)
    
    
    retention_uc = mean(UC$retention, na.rm = TRUE)
    retention_uc
    retention_state = mean(state_col$retention, na.rm = TRUE)
    retention_state
    
    med10yr_uc = mean(UC$med_10yr_salary, na.rm = TRUE)
    med10yr_uc
    med10yr_state = mean(state_col$med_10yr_salary, na.rm = TRUE)
    med10yr_state
    
    sd_uc = mean(UC$sd, na.rm = TRUE)
    sd_uc
    sd_state = mean(state_col$sd, na.rm = TRUE)
    sd_state
    
    comp_uc = mean(UC$completion, na.rm = TRUE)
    comp_uc
    comp_state = mean(state_col$completion, na.rm = TRUE)
    comp_state
    
    
    
    
    
    