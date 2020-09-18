#Lab 1 continued...

#We made a new script in the same project 

#--------------HEADER----------------------
#' @title Matricies, lists, and more!
#' @author Nina McDonnell
#' @date 9-11-2020
#' ----------------------------------------
MyVector <- 1:50
#' Try "?matrix" for help
MyMatrix <- matrix(MyVector,ncol=10)
MyMatrix

MyMatrix[2,2]

#byrow
matrix(MyVector, ncol=10, byrow = FALSE) #Byrow=FALSE is default. This means that valyes are filled down columns, and then across rows, rather than vice-versa
matrix(MyVector, ncol=10, byrow = TRUE)

#lets look at more indices

#a_matrix[row,column]
MyMatrix[2,2]
MyMatrix[2, ] #rows
MyMatrix[ ,2] #colums

#expolore matrix
str(MyMatrix) #shows it is an matrix of integers, with 5 rows and 10 columns
summary(MyMatrix) #gives independent summary for each column
length(MyMatrix) #only gives number of values

#functions for dimensions
dim(MyMatrix) #tells you dimensions of matrix (ex. 5 by 10)
nrow(MyMatrix) #number of rows
ncol(MyMatrix) #number of rows

#Exercise: Try creating a 10 x 10 matrix of random uniform numbers between 5 and 50, and get the row and column means (rowMeans() and colMeans()). 
#What’s the output of str()? 

MyVector2 <- runif(n=100, min=5, max=50)
MyMatrix2 <- matrix(MyVector2, nrow = 10, ncol = 10)
str(MyMatrix2)
summary(MyMatrix2)
rowMeans(MyMatrix2)
colMeans(MyMatrix2)

#Lists
MyList <- list(first=1:10,
               seconf=letters[10:25])

MyList

#Whats in it?!
str(MyList)
str(MyList[first]) #not a valid subset. does not work
str(MyList["first"])

class(MyList["first"])

#to get a vector back
MyList[["first"]] #gives the list
class(MyList[["first"]]) #gives the class

#If you don't know subset names
MyList[1]
MyList[[1]]

#EXERCISE: Try this out. Create a list consisting of a vector of numbers, an NA, and a list which contains two vectors. 
#What is there? Also, check out our old friends str and summary

MyList2 <- list(first=1:10,
               second=NA,
               third=Vector1,Vector2)
str(MyList2)
summary(MyList2)

#Nested and mixed lists
BigList <- list(first=1:10,
                second=NA,
                third=list(a=letters[1:5],
                           b=list(one="1",
                                  two="2")))
                
BigList #when you run it, you can see that "first", "second", "a", "b", etc. are vectors, indicated by the $ in front.  Double dollar signs indicate that its a nested list!
BigList$first #Use dollar signs to see the nested lists
BigList$third$a
BigList$third$b$one

BigList[[3]][[2]][[2]] #also brings back "2". 

#Sometimes R gives back error messages that look like these ^ Its helpful to understand that its referening levels of lists. 

#side note: using "enter" after commas to separate parts of code and make it more legible!

#------DATA FRAMES----------

View(MyMatrix) #pulls up table of the matrix
View (BigList) #pulls up the data structure

data(mtcars) #load built in dataset
str(mtcars)
View(mtcars)

mtcars$mpg #shows the vector "mpg"

#data frame is tabular
mtcars[1,5] #gives values for row, column
mtcars[1,"drat"] #same as above, just using the column name
names(mtcars) #shows you all the column names for the data set
mtcars[3:10, c("mpg","wt")] #shows dataframe with rows 3-10 for mpg and wt

#you can also use functions designed for matricies

colMeans(mtcars)
colMeans(mtcars$mpg) #<- does not work because this is a list, not a matrix.
summary(mtcars)
#--------Testing lists after lab--------------


TestList2 <- list(1:10) 
TestList2

TestList <- list(first=1:10,
                 second=20:30)
TestList$first #gives you vector object "first" from the list

print(TestList)
str(TestList)
summary(TestList)
