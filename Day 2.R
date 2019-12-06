#Day 2

##Part 1
input = c(1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,9,19,1,13,19,23,2,23,9,27,1,6,27,31,2,10,31,35,1,6,35,39,2,9,39,43,1,5,43,47,2,47,13,51,2,51,10,55,1,55,5,59,1,59,9,63,1,63,9,67,2,6,67,71,1,5,71,75,1,75,6,79,1,6,79,83,1,83,9,87,2,87,10,91,2,91,10,95,1,95,5,99,1,99,13,103,2,103,9,107,1,6,107,111,1,111,5,115,1,115,2,119,1,5,119,0,99,2,0,14,0)

program_alarm_1202 = function(input){
  position = 1
  while(TRUE){
    pos_a = input[position + 1]
    pos_b = input[position + 2]
    pos_c = input[position + 3]
  
    if(input[position] == 1){
      input[pos_c + 1] = input[pos_a + 1] + input[pos_b + 1]
      position = position + 4
      
    } else if(input[position] == 2) {
      input[pos_c + 1] = input[pos_a + 1] * input[pos_b + 1]
      position = position + 4
    
    
    } else if(input[position] == 99) {
      return(input)
      
    } else {
      stop("Something went wrong.")
      
    }
  }
}

initial_state = function(input){
  input[2] = 12
  input[3] = 2
  return(input)
}

program_alarm_1202(initial_state(input))[1]


##Part 2

initial_state = function(input, noun, verb){
  input[2] = noun
  input[3] = verb
  return(input)
}

output = function(input, noun, verb){program_alarm_1202(initial_state(input, noun, verb))[1]}

for(noun in 0:99){
  for(verb in 0:99){
    if(output(input, noun, verb) == 19690720){
      return()
    }
  }
}
100 * noun + verb
