# Homework Assignment #4 due Tuesday, November 21th by 5:00 PM -
# Mitchell Layton                                              -
# 912307956                                                    -
# --------------------------------------------------------------

# Preload libraries incase I use any of the following I've learned thus far
library(tidyverse)
library(ggmosaic)
library(plotly)
library(scales)
library(car)
library(RColorBrewer)
library(rio)
library(readr)
library(ggmap)
library(ggrepel)
library(data.table)
library(lubridate)
library(gridExtra)
library(plyr)
library(dplyr)
library(ggthemes)
library(grid)
library(sqldf)
library(reshape)
library(zoo)
library(bootstrap)    
library(boot)




#1) ----------------------------------------------------------------

# n = turns by a player in a game
# d = two d-sided die (from 1-6)

simulate_monopoly = function(n,d) {
    
    CC = seq(1,16,1)
    CC[15] = 100  # Set at index 3 for randomness and equal to 1 to rep "Advance to GO"
    CC[16] = 200
    cc = sample(sample(sample(CC))) # simulate shuffles

    CH = seq(1,16,1)
    CH[7:16] = seq(100,1000,100) # placeholders for 10/16 chances
    ch = sample(sample(sample(CH))) # simulate shuffles

    temp_funct = function() {
        nums = seq.int(1,d,1)
        sum_of_dice = sum(sample(nums, size = 2, replace = T))
        return(sum_of_dice)
    }
    
    
    card_deck_CC = function(cc) {
        pick = cc[1]
        cc = cc[2:16]
        cc[16] = pick
        x = list(pick,cc)
        return(x)
    }
        
    card_deck_CH = function(ch) {
        pic = ch[1]
        ch = ch[2:16]
        ch[16] = pic
        y = list(pic,ch)
        return(y)
    }        
        
        
    
    dice_rolls = as.vector(replicate(n, temp_funct()))
    output = vector("double", length(dice_rolls))
    MAX_BOARD_POSITION = 39 
    current_position = 0
    
    dice_seq = seq(1,(length(dice_rolls)+1),by=1)
    
    for (i in dice_seq) {
        
            if (i == 1) {
                output[1] = 0
                output[2] = dice_rolls[1]
                output[3] = dice_rolls[1] + dice_rolls[2]
                
            }
            if (i >= 4) {
                output[i] = output[i-1] + dice_rolls[i-1]
                if ((output[i]) < 39) {
                    output[i] = (dice_rolls[i-1] + output[i-1])
                    dice_rolls[i-1] = output[i]
                }
                if (output[i] == 39) {
                    output[i+1] = output[i]
                }
                
                if (output[i] > 39) { 
                    output[i] = ((output[i] %% MAX_BOARD_POSITION) - 1)
                    dice_rolls[i-1] = output[i]
                }
                if (output[i] == 40) {
                    output[i] = 0
                } 
                if (output[i] == 30) {
                    output[i] = 10
                }
                if (output[i] %in% c(2,17,33)) {
                    pick = card_deck_CC(cc)
                    pi = pick[[1]]
                    cc = pick[[2]] 
                    if (pi < 20) {
                        output[i] = output[i]
                    }
                    else if (pi == 100) {
                        output[i] = 0 # Advance to GO
                    }
                    else 
                        output[i] = 10 # GO to JAIL
                }
                if (output[i] %in% c(7,22,36)) {
                    pic = card_deck_CH(ch)
                    p = pic[[1]]
                    ch = pic[[2]]
                    # For Go to next RR if else and UT
                    RR1 = 5
                    RR2 = 15
                    RR3 = 25
                    RR4 = 35
                    UT1 = 12
                    UT2 = 28
                    if (p < 10) {
                        output[i] = output[i]
                    }
                    else if (p == 100) { # Advance to GO
                        output[i] = 0 
                    }
                    else if (p == 200) { # Go to JAIL
                        output[i] = 10 
                    }
                    else if (p == 300) { # Go to C1
                        output[i] = 11 
                    }
                    else if (p == 400) { # Go to E3
                        output[i] = 24 
                    }
                    else if (p == 500) { # Go to H2
                        output[i] = 39 
                    }
                    else if (p == 600) {# Go to RR1
                        output[i] = 5 
                    }
                    else if (p == 700) { # Go to next RR (railroad)
                        output[i] = output[i]
                        if ((output[i] >= 0) & (output[i]<= 4)) {
                            output[i] = RR1
                        }
                        if ((output[i] >= 6) & (output[i]<= 14)) {
                            output[i] = RR2
                        }
                        if ((output[i] >= 16) & (output[i]<= 24)) {
                            output[i] = RR3
                        }
                        if ((output[i] >= 26) & (output[i]<= 34)) {
                            output[i] = RR4
                        }
                        if ((output[i] >= 36) & (output[i]<= 39)) {
                            output[i] = RR1
                        }
                        
                    }
                    else if (p == 800) { # Go to next RR (railroad)
                        output[i] = output[i]
                        if ((output[i] >= 0) & (output[i]<= 4)) {
                            output[i] = RR1
                        }
                        if ((output[i] >= 6) & (output[i]<= 14)) {
                            output[i] = RR2
                        }
                        if ((output[i] >= 16) & (output[i]<= 24)) {
                            output[i] = RR3
                        }
                        if ((output[i] >= 26) & (output[i]<= 34)) {
                            output[i] = RR4
                        }
                        if ((output[i] >= 36) & (output[i]<= 39)) {
                            output[i] = RR1
                        }
                    }
                    else if (p == 900) { # Go to next UT (utility)
                        output[i] = output[i]
                        if ((output[i] >= 0) & (output[i]<= 11)) {
                            output[i] = UT1
                        }
                        if ((output[i] >= 13) & (output[i]<= 27)) {
                            output[i] = UT2
                        }
                        if ((output[i] >= 29) & (output[i]<= 39)) {
                            output[i] = UT1
                        }
                    }
                    else
                        output[i] = output[i] - 3 
                    
                    
                }
                
                
            }
            
    } # for i
    
    if (length(output) > (n+1)) {
        output[1:(length(output)-1)]
    }
    else
        return(output)
}

output = simulate_monopoly(10000,6)
output # put in arguments (n,d) for function here
# a length n + 1 vector of positions, encoded as numbers from 0 to 39.


#2) ----------------------------------------------------------------


estimate_monopoly = function(x) {
    
    values = as.data.table(x)
    names(values) = "Values"

    positions = as.data.table(c(
        "GO", "A1", "CC1", "A2", "T1", "RR1", "B1", "CH1", "B2", "B3", "JAIL",
        "C1", "UT1", "C2", "C3", "RR2", "D1", "CC2", "D2", "D3", "FP",
        "E1", "CH2", "E2", "E3", "RR3", "F1", "F2", "UT2", "F3", "G2J",
        "G1", "G2", "CC3", "G3", "RR4", "CH3", "H1", "T2", "H2"
    ))
    
    spots = as.data.table(c(seq(0,39,1)))
    df = (cbind(positions,spots))
    names(df) = c("Square_Names","Values")
    
    merged_data = merge(values, df, by = "Values")
    table_merged = as.data.table(table(merged_data$Square_Names))
    names(table_merged) = c("squares","nums")
    
    # Get probabilities
    table_merged = table_merged %>%
        mutate(probs = (table_merged$nums)/sum(table_merged$nums))
    
    
       monopoly_freq = ggplot(data = table_merged, aes(x=squares,y=probs, fill = probs)) +
           labs(x = "Monopoly Tile Sqaures", title = "Long-term Probabilities for each Board Position (6-sided dice)") +
            geom_bar(stat="identity") + 
            scale_y_continuous("Probabilities")     
        print(monopoly_freq) 
        return(table_merged)
    
}
    # Probabilities all based on d parameter in above function. Chance (d) sided die for below input to funciton
    tab = estimate_monopoly(output)
    G = sqldf("SELECT * FROM tab ORDER BY probs DESC")
    TOP_3 = head(G,3)
    TOP_3


#3) ----------------------------------------------------------------
    # partially referenced method from Anonymous Piazza user
    replicate_function = function(k,n) {
        
        table = as.data.table(numeric(k))
            for (i in 1:k){
                nums = as.data.table(table(simulate_monopoly(n,6)))
                t1 = nums$N[nums$V1 == "10"]
                table[i,] = t1
            }
        table 
    }
    
    # Takes about 01:24.00 s to compute
    jail_values = replicate_function(1000,10000)
    names(jail_values) = "Jail"
    
    #computation of the standard error of the mean of JAIL occurances and amounts
    SE = sd(jail_values$Jail)/sqrt(length(jail_values$Jail))
    SE
    t.test(jail_values$Jail)
    
    
    
#4) ----------------------------------------------------------------

    se_boot = function(x = jail_values$Jail) {
        x.boot = sample(x, size = length(x), replace=T)
        y = sd(x.boot)/sqrt(length(x.boot)) # SE
        y
    }
    se_boot()
    se_boot.replicate = as.data.table(replicate(1000, se_boot()))
    se_boot.replicate
    t.test(se_boot.replicate)

    

    
    
#5) ----------------------------------------------------------------
    
    
    SE_long_term = function(x) {
        
        values = as.data.table(x)
        names(values) = "Values"
        
        positions = as.data.table(c(
            "GO", "A1", "CC1", "A2", "T1", "RR1", "B1", "CH1", "B2", "B3", "JAIL",
            "C1", "UT1", "C2", "C3", "RR2", "D1", "CC2", "D2", "D3", "FP",
            "E1", "CH2", "E2", "E3", "RR3", "F1", "F2", "UT2", "F3", "G2J",
            "G1", "G2", "CC3", "G3", "RR4", "CH3", "H1", "T2", "H2"
        ))
        
        spots = as.data.table(c(seq(0,39,1)))
        df = (cbind(positions,spots))
        names(df) = c("Square_Names","Values")
        
        
        
        merged_data = merge(values, df, by = "Values")
        table_merged = as.data.table(table(merged_data$Square_Names))
        names(table_merged) = c("squares","nums")

        
        # Get probs
        table_merged = table_merged %>%
            mutate(probs = (table_merged$nums)/sum(table_merged$nums))
        # Get SE
        table_merged = table_merged %>%
            mutate(SE = sd(table_merged$probs)/sqrt(table_merged$nums))
        
        SE_freq = ggplot(data = table_merged, aes(x=squares,y = SE, fill = SE)) +
            labs(x = "Monopoly Tile Sqaures", title = "SE of Long-term Probabilities for each Board Position (3-sided dice)") +
            geom_bar(stat="identity") + 
            scale_y_continuous("SE")     
        print(SE_freq) 
        return(table_merged)
        
    }
    # Probabilities all based on d parameter in above function. Chance (d) sided die for below input to funciton
    tab = SE_long_term(output)
    tab
    
#6)------------------------------------------------
   
    #Check report for explanation
    
    