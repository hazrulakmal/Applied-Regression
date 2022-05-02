getwd()#find directory

dir()

#variable
new_mean <- 1
new_mean

#check what variables are in the environment 
ls()

#vector
first_vector <- c(1,2,3,4)
ls()

#is it vector?
is.vector(first_vector)


#TRUE carries value of 1 and FALSE carries a value of 0

true_haz = as.numeric(TRUE)
is.numeric(true_haz)

# element wise power
first_vector^2


#another vector addition
second_vector = c(4,5,3,2)

second_vector+first_vector
second_vector > first_vector


identical(first_vector, c(1,2,3,4))
identical(first_vector, c(2-1,2,3,4))



#take elements in a vector
first_vector[c(1,4)]
first_vector[1:3]




#which funtion gives the location of the elements in a vector


first_array<-array(first_vector,dim=c(2,2))
first_array


#works more like pandas
# logical functions inside vectors------------------------------------------------------------------------
first_vector[which(first_vector>3)]






