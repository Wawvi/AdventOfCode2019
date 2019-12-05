#Day 4

##Part 1
range = 123257:647015
X = strsplit(range, "") 
X = lapply(X, as.numeric)

adjacent_digit = function(input){
  any(
    sapply(2:6,
           function(i){
             input[i] == input[i-1]
             })
    )
}

increasing_digit = function(input){
  all(
    sapply(2:6,
           function(i){
             input[i] >= input[i-1]
           })
    
  )
}


is_increasing = sapply(1:length(X), function(i) increasing_digit(X[[i]]))
X_increasing = X[is_increasing]
is_adjacent = sapply(1:length(X_increasing), function(i) adjacent_digit(X_increasing[[i]])) 
X_final = X_increasing[is_adjacent]


##Part 2

#Adjacent identical digits must be in a group of exactly two

one_adjacent_digit = function(input){
  
  i = 1
  while(i < 6){
    state = TRUE
    n = 0
    while(state & i < 6){ #state stays true if adjacent digit is identical
      state = input[i] == input[i + 1]
      if(state){n = n + 1} #n increases as long a digit is the same
      i = i + 1 #test next digit 
    } #loops end if state is false, then tests next not identical adjacent digit, ends when all digits were tested
    if(n == 1){return(TRUE)} #if only one adjacent identical digit, return true and end function, else continue loop until i == 5
    
  }
  return(FALSE)
}

is_one_adjacent = sapply(1:length(X_increasing), function(i) one_adjacent_digit(X_increasing[[i]])) 
X_final2 = X_increasing[is_one_adjacent]

