## for ordering manuscript

## 59 boxes
## 33 cardboard boxes
## 15 brown cardboard boxes
##  3 small brown cardboard boxes

## error cardboard = 0.1
## error brown = 0.2
## error small = 0.3

# p(error) = 1-(1-epsilon)^N
# p(no error) = (1-epsilon)^N1 * (1-epsilon)^N2 * ...

# subjectivity ordering
((1-0.01)^59)*((1-0.02)^33)*((1-0.03)^15)

# reverse subjectivity ordering
((1-0.01)^15)*((1-0.02)^33)*((1-0.03)^59)


# subjectivity ordering
1-((1-0.1)^8)*((1-0.2)^4)*((1-0.3)^2)

# reverse subjectivity ordering
1-((1-0.3)^8)*((1-0.2)^4)*((1-0.1)^2)



## 59 boxes
## 33 small boxes
## 15 brown small boxes
##  3 carboard brown small boxes