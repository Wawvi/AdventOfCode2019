#Day 8

input = readLines("Input/ex8.txt")

treat_input = function(input){
  as.numeric(unlist(strsplit(input, "")))
}

input = treat_input(input)
## Part 1
to_layers = function(input, nrow, ncol){
  n = length(input)
  ndim = n/(ncol*nrow)
  
  x = list()
  for(dim in 1:ndim){
    start = (dim-1)*(ncol*nrow) + 1 
    end = start + (ncol*nrow) - 1
    x[[dim]] = matrix(input[start:end], nrow, ncol, byrow = TRUE)
  }
  return(x)
}

layers = to_layers(input, 6, 25)

count_condition = function(layer, to_find){
  length(layer[layer == to_find])
}

min_0 = layers[[which.min(sapply(1:length(layers), function(i) count_condition(layers[[i]], 0)))]]
count_condition(min_0, 1) * count_condition(min_0, 2)


## Part 2
layers[[1]][which(layers[[1]] == 2)]
stack_layers = function(layers){
  picture = layers[[1]]
  
  for(layer in 2:length(layers)){
    
    if(any(picture == 2)){
      
      pixel_to_replace = which(picture == 2)
      picture[pixel_to_replace] = layers[[layer]][pixel_to_replace]
      
      } else {
        
      return(picture) 
        
      }
  }
  return(picture)
}

image(stack_layers(layers))
