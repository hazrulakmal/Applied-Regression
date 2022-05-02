#first question
array.1<-array(c(1,2,3,4,5,6,7,8,9,10,11,12),dim=c(4,3))
array.1

#second question
rep(3,12)

array.2<-array(rep(3,12),dim=c(4,3))
array.2

array.3 = array.1+array.2
array.3[2,3]

#third question
x=2
y=2
x+y

sqrt(2)*sqrt(8)

2/2/2 #1/2

ONE=c(1,3,5,7); TWO=c(2,4,6,8)

sum(1:10) # sum funtion. SUM do not work 

2^5
hello = 2^(1:10) #a vector list of 2 to the power of an element in the array
is.vector(hello)


pi
Pi #pi variable

2pi

pundulum = 2*pi*sqrt(0.5/9.81)

#question four 

array.tf = array.1>array.2
length(which(array.tf==TRUE))


#question five
vec.1 = as.vector(array.1)
vec.1[10]

#question six
array.4 = subset(array.1, select= -c(2))
array.4[2,2]
array.4

#question seven
my.name<-c("Sara","Geneletti")
is.numeric(my.name)


#question eight
silly.func = function(vec, a, b){
v.new = (vec+a)/b
              v.new
}

silly.func(vec.1, 4,5)[5]

