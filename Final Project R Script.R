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

#For testing the function, you can use: 
predict_knn(test[1:5,],train,100,"euclidean")
#Or try it on the test data
predict_knn(test,train,90,"minkowski")


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



#6)---------------------------------------------------------------------


rates1 = c(rep(0,15))
rates2 = c(rep(0,15))

for (i in 1:15) { 
    
    error_rates_EUC = cv_error_knn(train,i,"euclidean")
    error_rates_MAN = cv_error_knn(train,i,"manhattan")
    
    rates1[i] = error_rates_EUC
    rates2[i] = error_rates_MAN
    
}


x = seq(1,15,1)
data = as.data.frame(cbind(x,rates1,rates2))
names(data) = c("k","Euclidean","Manhattan")
data = melt(data, id = "k")
names(data) = c("k","Distance Metric","CV_Error")

p = ggplot(data) + 
        geom_point(aes(x = k, y = CV_Error, colour = `Distance Metric`), size = 4, shape = 19) +
        geom_text_repel(aes(x=k,y=CV_Error,label=round(CV_Error,5)),size=3.15) +
        theme_dark() +
        scale_x_continuous("k", breaks = c(seq(1,15,1))) +
        scale_y_continuous("CV Error Rates") +
        labs(title = "K-Dependent CV Error Rates") + 
        theme(axis.title.x = element_text(size = rel(1.25))) +
        theme(axis.title.y = element_text(size = rel(1.15))) +
        theme(axis.text.x = element_text(size = rel(1.25))) +
        theme(axis.text.y = element_text(size = rel(1.25))) + 
        theme(title = element_text(size = rel(1.5))) 
p










