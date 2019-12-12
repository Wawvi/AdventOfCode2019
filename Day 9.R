#Day 9

## Part 1
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


intcode_computer = function(program, input = NA, phase_setting = input, position = 1, output = NA, relative_base = 0){
  input = c(phase_setting, input)
  
  #parameter mode for values
  parameter_mode = function(program, param_mode, param_value, relative_base){
    if(param_mode == 0){#position mode
      param = program[param_value + 1]
    } else if(param_mode == 1){#immediate mode
      param = param_value
    } else if(param_mode == 2){#relative mode
      param = program[param_value + relative_base + 1]
    }
    return(ifelse(is.na(param), 0, param))
  }
  
  #set input, do we need to use phase ?
  phase_used = TRUE
  if(position == 1){
    phase_used = FALSE
  }
  
  n_output = 1
  #state information and output
  output = list(
    output = list(),
    position = position,
    program = program,
    stop_99 = FALSE,
    phase = phase_setting
  )
  
  #intcode program
  while(TRUE){
    instruction = interprete_instruction(
      treat_instruction(
        program[position]
      )
    )
    #absolute value
    value_a = ifelse(is.na(program[position + 1]), 0, program[position + 1])
    value_b = ifelse(is.na(program[position + 2]), 0, program[position + 2])
    value_c = ifelse(is.na(program[position + 3]), 0, program[position + 3])
    
    #Value modified depending on parameter mode
    param_a = parameter_mode(program, instruction[["mode_param_a"]], value_a, relative_base)
    param_b = parameter_mode(program, instruction[["mode_param_b"]], value_b, relative_base)
    param_c = parameter_mode(program, instruction[["mode_param_c"]], value_c, relative_base)
    
    #Pointer position for op_code
    position_a = ifelse(instruction[["mode_param_a"]] == 2, value_a + relative_base + 1, value_a + 1)
    position_b = ifelse(instruction[["mode_param_b"]] == 2, value_b + relative_base + 1, value_b + 1)
    position_c = ifelse(instruction[["mode_param_c"]] == 2, value_c + relative_base + 1, value_c + 1)
    
    if(instruction[["op_code"]] == 1){
      program[position_c] = param_a + param_b
      position = position + 4
      
    } else if(instruction[["op_code"]] == 2) {
      program[position_c] = param_a * param_b
      position = position + 4
      
      
    } else if(instruction[["op_code"]] == 3) {
      program[position_a] = ifelse(phase_used, input[2], input[1])
      phase_used = TRUE
      position = position + 2
      
      
    } else if(instruction[["op_code"]] == 4) {
      position = position + 2
      output[["output"]][[n_output]] = param_a
      n_output = n_output + 1
      output[["position"]] = position
      output[["program"]] = program
      #return(output) #no amp
      
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
      program[position_c] = ifelse(param_a < param_b, 1, 0)
      position = position + 4
      
    } else if(instruction[["op_code"]] == 8) {
      program[position_c] = ifelse(param_a == param_b, 1, 0)
      position = position + 4
      
    } else if(instruction[["op_code"]] == 9) {
      relative_base = relative_base + param_a
      position = position + 2
      
    } else if(instruction[["op_code"]] == 99) {
      output[["position"]] = position
      output[["program"]] = program
      output[["stop_99"]] = TRUE
      output[["output"]] = unlist(output[["output"]])
      return(output)
      
    } else {
      stop("Error ! Error !")
      
    }
  }
}


program = as.numeric(unlist(strsplit(readLines("Input/ex9.txt"), ",")))
intcode_computer(c(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99))
intcode_computer(c(1102,34915192,34915192,7,4,7,99,0))
intcode_computer(c(104,1125899906842624,99))

intcode_computer(program, 1)

##Part 2
intcode_computer(program, 2)
