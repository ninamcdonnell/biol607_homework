#Lab 1- Orienting to R workspace


#Keep change log to help remember what you've done

#---------CHANGE LOG----------------
# 9/10/20- made script!
# 9/11/20- continued laerning during lab block
#-------------------------

#Try entering an equation into the console or script. Hit command+return to run!

(1+6)*7

#or use a function...

sqrt((1+6)*7)

#' Use "#'" to carry over "#" when you hit "enter" so you don't 
#' have to write them on every comment line
#' 
#' 
#' 
#' 

#Try "?" before a function to get the help file!!! Wow, I wish I knew this sooner
#scroll down to examples! Generally, put help commands into the console not script.

?log

#Trying this for a function from a package I use...

library(survival)
library(survminer)

# "??" Will search through all packages and look for any functions that match. Use if "?" does not pull anything up. Generally, put help commands into the console not script.

??surv 

#Excercise: find a function, look at help page, and use it
x<-c(1,2,3,4,5,8,15)
?sd
sd(x)

#------- DAY 2------------

#Opening project file will also reopen the working directory it was set to before

#We have variables #### <- 4 # makes a section header so you can navigate more easily!

pi
exp(1)
foo <-sqrt(2) #can also be:
sqrt(2) ->foo #but this is not usually done
foo=sqrt(2) #This works, but it is ambiguous. We cannot tell that is being assigned to what. = is reserved for specifying function arguments. In R predecessor, S+, there used to an arrow key on computer. 
#Key stroke "option"+"-" makes an arrow!

foo #Now it can be recalled and seen in the environment panel.
foo^2
log(foo)

#Variable naming rules:

#'1. Be meaningful!
#'2. Easily readable
#'3. Keep it short
#'4. Never start with a number
#'5. If you use multiple words, use either snake or camel case
#'    ex. metamorph_mass or metamorph.mass (snake), MetamorphMass (camel)
#'    But, sometimes dots are used in functions, so dashes are better
#'    Stick with one or the other.

#Classes of objects:

class(foo)
TextString <- "this is text"

#Booleans. TRUE=logical, FALSE=not logical (1 and 0). Must be typed in all caps
TRUE
FALSE

#Check to see if true/false with equality statements
foo == 1 #equality is a ==
foo < 1
foo <= 1
foo >= 1

#Missing values
NA
class(NA)
NA+0 # if you add 0 to NA value, its still NA

#related special variables
NaN #Not missing, just not a number. Ex:
3/0

#EXERCISE: 
#1. Make a variable. 
#2. Now make a variable out of some math equation. 
#3. Try adding variables of different classes together - what happens? Write your answer below.

#You cannot add objects of different classes 
bips<-(6*8)/7
bips
class(bips)
baps<-"baps"
baps
class(baps)

#there are other classes!
1L
class(1L)

bips+baps
foo+TRUE
foo+NaN

#vectors!

MyVector <- c(1,1,3,4,5,6,7)
MyVector
class(MyVector) #class in numeric

class(c("a", "b", "c")) #class is chacter

#a useful character vector
letters
LETTERS

#what is the 12th letter in alphabet?
letters[12]

#for a range of values, use :

letters[1:10]

letters[MyVector]

#EXERCISE: Make two vectors and add them together. First try it with numbers. 
#Then try vectors of different object types. What happens?

Vector1 <-c(4,5,6,8,9,15,37)
Vector2 <-c(2,3,4,5)
Vector3 <-c("a","b","c")
Vector4 <-c("we", "ba", "fu")
Vector5 <-c(1,2,3,4,5,13,26)
Vector6 <-c(2,3,4,5,"a") #numbers and letters make everything converted to sting of characters

Vector1+Vector2
Vector1+Vector3
Vector3+Vector4
Vector1+Vector5
#Must be same class type and length to be added

#Functions to make vectors
1:10
seq(from=1, to=10, by= 0.1)

#Random numbers
MyUnif <- runif(n=100, min=13.5, max=200)

#many functions act on vectors
sum(MyUnif)
mean(MyUnif)

#str and summarize- what is in that object###
str(MyUnif)
#str gives you class, length, and head (first few values). Great for detecting if something is obviously wrong

#Sometimes problems are less obvious
NaVector <- c(1:100, NA, 10:100)
srt(NaVector) #You cannot see the NA because its too far down

summary(NaVector) #Will tell you if there is an NA messing everything up!
print(NaVector) #Lists values

#EXERCISE: Create a vector of any class. str() and summarize() it. Now, create two vectors of different object types. 
#Combine them. What do these two useful functions tell you what happened? Write your answer below.

Vector7<-seq(from=1, to=100, by= 0.5)
str(Vector7)
summary(Vector7)

Vector8<-(letters)
str(Vector8)
summary(Vector8)

Vector9<-c(Vector8,Vector7)
str(Vector9)
summary(Vector9)
#class was converted to character (the least presumptive) for all values

#() reserves for functions. [] are for things we can subset!!!

#There are also [[]] and {}, but those are for the future!

#Use commans +A to select everyting, then run 
