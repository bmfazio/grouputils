# A sensible modification of the base sample() function
sensamp <- \(x, n)if(length(x)<=n)x else sample(x, n)
