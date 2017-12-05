# STA 141A - Final Project 
# Due December 5, 2017 @ 5:00 PM
# GROUP 52: Mitchell Layton, Caroline Mai, William Powell
#---------------------------------------------------------
library(ggthemes)
library(reshape)
library(ggplot2)
library(parallelDist)
#1)---------------------------------------------------------------------


read_digits = function(x) {
    txt = read.table(x)
}

train = read_digits("E:/Davis/STA 141A/DATA/train.txt")
test = read_digits("E:/Davis/STA 141A/DATA/test.txt")

#2)---------------------------------------------------------------------

view_digit = function(read_data, observation) {
    temp = melt(read_data[observation,])
    temp = temp$value[2:length(temp$value)]
    temp = matrix(temp, nrow = 16, ncol = 16, byrow = T)
    rotate <- function(x) {
        t(apply(x, 2, rev))
    }
    temp = rotate(temp)
    image(temp, axes = FALSE, col = grey(seq(0, 1, length = 256)))
    
}

view_digit(train,130)


#3)---------------------------------------------------------------------


# Display  graphically what each digit (0-9) looks like on average

avg_digit = function(read_data) {
    
    rotate <- function(x) {
        t(apply(x, 2, rev))
    }
    
    nums = seq(0,9,1)
    for (i in nums) {
        # Used sqldf() package for SQL query like statements. Also to get sqldf to use my i in my for loop,
        # had to use sprintf and strwrap for wrapping my SQL statement string senctence to be able to
        # use the C-style string formatting command within the sqldf() function.
        temp = sqldf(strwrap(sprintf("SELECT * FROM read_data WHERE V1 = '%s'",i), simplify = T))
        temp = as.data.table(sapply(temp,mean))[2:257]
        temp = melt(temp, id=1)
        temp = temp$V1
        temp = matrix(temp, nrow = 16, ncol = 16, byrow = T)
        temp = rotate(temp)
        print(image(temp, axes = FALSE, col = grey(seq(0, 1, length = 256))))
    }
    
}
avg_digit(data)


#4)---------------------------------------------------------------------
start_time <- Sys.time()
predict_knn = function(points, train, k, metric) {
    
    #Set up a vector for the predicted classes
    predictions = c(rep(NA,nrow(points)))
    
    #A matrix of the prediction points, excluding the class values, to use for the distance function
    dist_mat = as.matrix(rbind(points,train[1:nrow(train),])[,2:257])
    
    #A matrix of all the distances
    one_dist = as.matrix(parDist(dist_mat,method = metric))
    
    for (i in 1:nrow(points)){
        #frequencies of the nearest classes stored in "classes"
        classes = c(rep(0,10))
        #Extracting distances from the matrix
        distances = one_dist[(1+nrow(points)):nrow(one_dist),i]
        #Finding the closest k
        k_nearest = sort(distances)[1:k]
        for(j in 1:k){
            #incrementing frequencies of classes for the k-nearest neighbors
            classes[train[which(distances==k_nearest[j]),1] + 1] = classes[train[which(distances==k_nearest[j]),1] + 1] + 1
        }
        #print(classes)
        
        #Solving ties
        if (length(which(classes==max(classes))) > 1){
            #choose one of the max frequency classes at random (-1 to adjust from 1:10 to 0:9)
            predictions[i] = sample(which(classes==max(classes)),1) - 1
        }
        else{
            #choose the max frequency class
            predictions[i] = which(classes==max(classes)) - 1
        }
    }
    #output the vector of predictions
    predictions
}
#Or try it on the test data
predict_knn(test,train,95,"euclidean")
end_time <- Sys.time()
end_time - start_time

#5)---------------------------------------------------------------------


cv_error_knn = function(train_data,k,metric) {
    
    #Create 10 equally size folds
    folds <- cut(seq(1,nrow(train_data[2:257])),breaks = 10,labels = FALSE)
    
    #Perform 10 fold cross validation
    for(i in 1:10) {
        #Segement your data by fold using the which() function 
        test_indexes <- which(folds == i,arr.ind = TRUE)
        tested = train_data[test_indexes, ]
        compare = tested$V1
        trained = train_data[-test_indexes, ]
        
        #Use the test and train data partitions:
        P = as.data.table(predict_knn(tested,trained,k,metric))
        x = length(which(P!=compare))
        y = x/length(compare)
        y
    }
    m = mean(y)
    return(m)
}
cv_error = cv_error_knn(train,1,"euclidean")
cv_error

dist

#6)---------------------------------------------------------------------


rates1 = c(rep(0,15))
rates2 = c(rep(0,15))

start_time <- Sys.time()
for (i in 1:15) { 
    
    error_rates_EUC = cv_error_knn(train,i,"euclidean")
    error_rates_MAN = cv_error_knn(train,i,"manhattan")
    
    rates1[i] = error_rates_EUC
    rates2[i] = error_rates_MAN
    
}
end_time <- Sys.time()
end_time - start_time

x = seq(1,15,1)
data = as.data.frame(cbind(x,rates1,rates2))
names(data) = c("k","Euclidean","Manhattan")
data = melt(data, id = "k")
names(data) = c("k","Distance Metric","CV_Error")



p = ggplot(data) + 
    geom_point(aes(x = k, y = CV_Error, colour = `Distance Metric`), size = 4, shape = 19) +
    geom_line(aes(x=k,y=CV_Error, colour = `Distance Metric`)) +
    geom_text_repel(aes(x=k,y=CV_Error,label=round(CV_Error,5)),size=3.15) +
    theme_calc() +
    scale_x_continuous("k", breaks = c(seq(1,15,1))) +
    scale_y_continuous("CV Error Rates") +
    labs(title = "K-Dependent CV Error Rates") + 
    theme(axis.title.x = element_text(size = rel(1.25))) +
    theme(axis.title.y = element_text(size = rel(1.15))) +
    theme(axis.text.x = element_text(size = rel(1.25))) +
    theme(axis.text.y = element_text(size = rel(1.25))) + 
    theme(title = element_text(size = rel(1.5))) +
    theme(legend.title = element_text(size = rel(.85))) +
    theme(legend.text = element_text(size = rel(.9))) 

    
p


#7)---------------------------------------------------------------------

create_errormatrix = function(train_data,k,metric) {
    
    #Create 10 equally size folds
    folds <- cut(seq(1,nrow(train_data[2:257])),breaks = 10,labels = FALSE)
    
    #Empty vectors to append the predictions and real values
    all_P = c()
    all_real = c()
    #Perform 10 fold cross validation
    for(i in 1:10) {
        #Segement your data by fold using the which() function 
        test_indexes <- which(folds == i,arr.ind = TRUE)
        tested = train_data[test_indexes, ]
        all_real = c(all_real,tested$V1)
        trained = train_data[-test_indexes, ]
        
        #Use the test and train data partitions:
        P = predict_knn(tested,trained,k,metric)
        all_P = c(all_P,P)
    }
    #Use all the appended values in the matrix
    return(data.frame(real = all_real, prediction = all_P))
}
#The data frame now has all 7291 obs.


start_time <- Sys.time()
for (i in c(1,3,4)) { #3 best K-values and dist metric
    
    testing = create_errormatrix(train,i,"euclidean")
    print(table(testing))
    
}
end_time <- Sys.time()
end_time - start_time

#8)---------------------------------------------------------------------
# Analysis In Report.

#9)---------------------------------------------------------------------

start_time <- Sys.time()

actual_test = test[,1]
test_frame = as.data.frame(actual_test)
test_eu = c(rep(0,15))
test_man = c(rep(0,15))

for (i in 1:15) { 
    
    pred_eu = predict_knn(test,train,i,"euclidean")
    pred_man = predict_knn(test,train,i,"manhattan")
    
    eu_frame = cbind(pred_eu, test_frame)
    man_frame = cbind(pred_man, test_frame)
    
    error_eu = nrow(subset(eu_frame, pred_eu!= actual_test))/nrow(test)
    error_man = nrow(subset(man_frame, pred_man!= actual_test))/nrow(test)
    
    test_eu[i] = error_eu
    test_man[i] = error_man
    
}
end_time <- Sys.time()
end_time - start_time


x = seq(1,15,1)
data2 = as.data.frame(cbind(x,test_eu,test_man))
names(data2) = c("k","Euclidean","Manhattan")
data2 = melt(data2, id = "k")
names(data2) = c("k","Distance Metric","test_set_Error")
p2 = ggplot(data2) + 
    geom_point(aes(x = k, y = test_set_Error, colour = `Distance Metric`), size = 4, shape = 19) +
    geom_line(aes(x = k, y = test_set_Error, colour = `Distance Metric`), size = 1) +
    geom_text_repel(aes(x=k,y=test_set_Error,label=round(test_set_Error,5)),size=3.15) +
    theme_calc() +
    scale_x_continuous("k", breaks = c(seq(1,15,1))) +
    scale_y_continuous("Test Set Error Rates") +
    labs(title = "K-Dependent Test Set Error Rates") + 
    theme(axis.title.x = element_text(size = rel(1.25))) +
    theme(axis.title.y = element_text(size = rel(1.15))) +
    theme(axis.text.x = element_text(size = rel(1.25))) +
    theme(axis.text.y = element_text(size = rel(1.25))) + 
    theme(title = element_text(size = rel(1.5))) +
    theme(legend.title = element_text(size = rel(.85))) +
    theme(legend.text = element_text(size = rel(.9)))


p2









