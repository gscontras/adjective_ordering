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
1-((1-0.01)^59)*((1-0.02)^33)*((1-0.03)^15)

# reverse subjectivity ordering
1-((1-0.01)^15)*((1-0.02)^33)*((1-0.03)^59)


# subjectivity ordering
1-((1-0.1)^8)*((1-0.2)^4)*((1-0.3)^2)

# reverse subjectivity ordering
1-((1-0.3)^8)*((1-0.2)^4)*((1-0.1)^2)

# small brown
1-((1-0.2)^4 * (1-0.64)^2)

# brown small
1-((1-0.2)^2 * (1-0.64)^4)


  .8^3 *.2 * .48 +       ## 1 box
  .8^4 * .44 +           ## 2 box1
  .8^2 * .2^2  * 0.44 +  ## 2 box2
  .8^2 * .2^2  * 0.44 +  ## 2 box3
  .8^3 * .2 * 0.4 +      ## 3 box1
  .8^3 * .2 * 0.4 +      ## 3 box2
  .8 * .2^3 * 0.4 +      ## 3 box3
  .8^2 * .2^2 * .36      ## 4 box
  
  
  .8^3 *.2 * .48  +     ## 1 box
  (.8^4  +           ## 2 box1
  .8^2 * .2^2   +  ## 2 box2
  .8^2 * .2^2) * .44   +  ## 2 box3
  (.8^3 * .2  +      ## 3 box1
  .8^3 * .2  +      ## 3 box2
  .8 * .2^3) * .4     +  ## 3 box3
  .8^2 * .2^2 * 0.36     ## 4 box
  