#Day 5 https://adventofcode.com/2019/day/5

##Part 1
treat_instruction = function(instruction){
  as.numeric(
    strsplit(
      paste(
        c(rep(0, 5 - nchar(instruction)),
          instruction),
        collapse = ""),
      "")[[1]]
    )
}


interprete_instruction = function(instruction){
  list(
    op_code = as.numeric(paste0(instruction[4], instruction[5])),
    mode_param_a = instruction[3],
    mode_param_b = instruction[2],
    mode_param_c = instruction[1]
  )
}

intcode_computer = function(program, input){
  position = 1
  output = list()
  n_output = 1
  while(TRUE){
    instruction = interprete_instruction(
      treat_instruction(
        program[position]
        )
      )
    value_a = program[position + 1]
    value_b = program[position + 2]
    value_c = program[position + 3]
    
    param_a = ifelse(instruction[["mode_param_a"]] == 0, program[value_a + 1], value_a)
    param_b = ifelse(instruction[["mode_param_b"]] == 0, program[value_b + 1], value_b)
    param_c = ifelse(instruction[["mode_param_c"]] == 0, program[value_c + 1], value_c)
    
    if(instruction[["op_code"]] == 1){
      program[value_c + 1] = param_a + param_b
      position = position + 4
      
    } else if(instruction[["op_code"]] == 2) {
      program[value_c + 1] = param_a * param_b
      position = position + 4
      
      
    } else if(instruction[["op_code"]] == 3) {
      program[value_a + 1] = input
      position = position + 2
      
      
    } else if(instruction[["op_code"]] == 4) {
      output[[n_output]] = param_a
      position = position + 2
      n_output = n_output + 1
      
      
    } else if(instruction[["op_code"]] == 99) {
      return(output)
      
    } else {
      stop("Error ! Error !")
      
    }
  }
}

program_input = c(3,225,1,225,6,6,1100,1,238,225,104,0,1102,88,66,225,101,8,125,224,101,-88,224,224,4,224,1002,223,8,223,101,2,224,224,1,224,223,223,1101,87,23,225,1102,17,10,224,101,-170,224,224,4,224,102,8,223,223,101,3,224,224,1,223,224,223,1101,9,65,225,1101,57,74,225,1101,66,73,225,1101,22,37,224,101,-59,224,224,4,224,102,8,223,223,1001,224,1,224,1,223,224,223,1102,79,64,225,1001,130,82,224,101,-113,224,224,4,224,102,8,223,223,1001,224,7,224,1,223,224,223,1102,80,17,225,1101,32,31,225,1,65,40,224,1001,224,-32,224,4,224,102,8,223,223,1001,224,4,224,1,224,223,223,2,99,69,224,1001,224,-4503,224,4,224,102,8,223,223,101,6,224,224,1,223,224,223,1002,14,92,224,1001,224,-6072,224,4,224,102,8,223,223,101,5,224,224,1,223,224,223,102,33,74,224,1001,224,-2409,224,4,224,1002,223,8,223,101,7,224,224,1,223,224,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,107,677,677,224,1002,223,2,223,1006,224,329,101,1,223,223,108,677,677,224,1002,223,2,223,1005,224,344,101,1,223,223,1007,677,677,224,1002,223,2,223,1006,224,359,101,1,223,223,1107,226,677,224,1002,223,2,223,1006,224,374,1001,223,1,223,8,677,226,224,1002,223,2,223,1006,224,389,101,1,223,223,1108,677,677,224,1002,223,2,223,1005,224,404,1001,223,1,223,7,226,226,224,1002,223,2,223,1006,224,419,101,1,223,223,1107,677,677,224,1002,223,2,223,1005,224,434,101,1,223,223,107,226,226,224,102,2,223,223,1005,224,449,101,1,223,223,107,677,226,224,1002,223,2,223,1006,224,464,1001,223,1,223,8,226,677,224,102,2,223,223,1006,224,479,1001,223,1,223,108,677,226,224,102,2,223,223,1005,224,494,1001,223,1,223,1108,677,226,224,1002,223,2,223,1005,224,509,1001,223,1,223,1107,677,226,224,1002,223,2,223,1005,224,524,101,1,223,223,1008,226,226,224,1002,223,2,223,1006,224,539,101,1,223,223,1008,226,677,224,1002,223,2,223,1005,224,554,1001,223,1,223,7,226,677,224,1002,223,2,223,1005,224,569,101,1,223,223,1007,677,226,224,1002,223,2,223,1006,224,584,1001,223,1,223,7,677,226,224,102,2,223,223,1006,224,599,101,1,223,223,1007,226,226,224,102,2,223,223,1006,224,614,101,1,223,223,1008,677,677,224,1002,223,2,223,1006,224,629,101,1,223,223,108,226,226,224,102,2,223,223,1006,224,644,101,1,223,223,1108,226,677,224,1002,223,2,223,1005,224,659,101,1,223,223,8,226,226,224,1002,223,2,223,1005,224,674,101,1,223,223,4,223,99,226)
intcode_computer(program_input, 1)

## Part 2

intcode_computer = function(program, input){
  position = 1
  output = list()
  n_output = 1
  while(TRUE){
    instruction = interprete_instruction(
      treat_instruction(
        program[position]
      )
    )
    value_a = program[position + 1]
    value_b = program[position + 2]
    value_c = program[position + 3]
    
    param_a = ifelse(instruction[["mode_param_a"]] == 0, program[value_a + 1], value_a)
    param_b = ifelse(instruction[["mode_param_b"]] == 0, program[value_b + 1], value_b)
    param_c = ifelse(instruction[["mode_param_c"]] == 0, program[value_c + 1], value_c)
    
    if(instruction[["op_code"]] == 1){
      program[value_c + 1] = param_a + param_b
      position = position + 4
      
    } else if(instruction[["op_code"]] == 2) {
      program[value_c + 1] = param_a * param_b
      position = position + 4
      
      
    } else if(instruction[["op_code"]] == 3) {
      program[value_a + 1] = input
      position = position + 2
      
      
    } else if(instruction[["op_code"]] == 4) {
      output[[n_output]] = param_a
      position = position + 2
      n_output = n_output + 1
      
      
    } else if(instruction[["op_code"]] == 5) {
        
        if(param_a != 0){
          position = param_b + 1
        } else {
          position = position + 3
        }
      
    } else if(instruction[["op_code"]] == 6) {
      
        if(param_a == 0){
          position = param_b + 1
        } else {
          position = position + 3
        }
      
    } else if(instruction[["op_code"]] == 7) {
        program[value_c + 1] = ifelse(param_a < param_b, 1, 0)
        position = position + 4
      
    } else if(instruction[["op_code"]] == 8) {
        program[value_c + 1] = ifelse(param_a == param_b, 1, 0)
        position = position + 4
      
    } else if(instruction[["op_code"]] == 99) {
      return(output)
      
    } else {
      stop("Error ! Error !")
      
    }
  }
}

intcode_computer(program_input, 5)



