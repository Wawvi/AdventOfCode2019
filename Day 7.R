#Day 7
library(magrittr)
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

intcode_computer = function(program, input, phase_setting){
  input = c(phase_setting, input)
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
      program[value_a + 1] = input[1]
      input = input[-1]
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
      return(output[[n_output - 1]])
      
    } else {
      stop("Error ! Error !")
      
    }
  }
}

amplifier = function(program, start_input, phase_setting_sequence){
  input = start_input
  for(phase_setting in phase_setting_sequence){
    input = intcode_computer(program, input, phase_setting)
  }
  return(input)
}

#All combinaisons of phase settings with unique value
any_duplicate = function(vector){
  length(unique(vector)) != length(vector)
}

unique_combi = function(vector, times){
  grid = expand.grid(replicate(times, vector, simplify = FALSE))
  test = apply(grid, 1, any_duplicate)
  grid[!test, ]
  
}


program = c(3,8,1001,8,10,8,105,1,0,0,21,38,59,84,93,110,191,272,353,434,99999,3,9,101,5,9,9,1002,9,5,9,101,5,9,9,4,9,99,3,9,1001,9,3,9,1002,9,2,9,101,4,9,9,1002,9,4,9,4,9,99,3,9,102,5,9,9,1001,9,4,9,1002,9,2,9,1001,9,5,9,102,4,9,9,4,9,99,3,9,1002,9,2,9,4,9,99,3,9,1002,9,5,9,101,4,9,9,102,2,9,9,4,9,99,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,99)

max_thruster_signal = function(program, start_input, phase_setting_options){
  
  combi_phase_setting = unique_combi(phase_setting_options, 5)
  combi_phase_setting = lapply(1:nrow(combi_phase_setting), function(i) unlist(combi_phase_setting[i,]))
  max = which.max(sapply(1:length(combi_phase_setting), function(i) amplifier(program, start_input, combi_phase_setting[[i]])))
  amplifier(program, start_input, combi_phase_setting[[max]])
  
}

max_thruster_signal(program, 0, 0:5)

## Part 2

intcode_computer = function(program, input, phase_setting, position = 1, output = NA){
  input = c(phase_setting, input)
  
  #set input, do we need to use phase ?
  phase_used = TRUE
  if(position == 1){
    phase_used = FALSE
  }
  
  
  #state information and output
  output = list(
    output = output,
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
      program[value_a + 1] = ifelse(phase_used, input[2], input[1])
      phase_used = TRUE
      position = position + 2
      
      
    } else if(instruction[["op_code"]] == 4) {
      position = position + 2
      output[["output"]] = param_a
      output[["position"]] = position
      output[["program"]] = program
      return(output) #stop program at each output to continue to next amp
      
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
      output[["position"]] = position
      output[["program"]] = program
      output[["stop_99"]] = TRUE
      return(output)
      
    } else {
      stop("Error ! Error !")
      
    }
  }
}

amplifier = function(program, start_input, phase_setting_sequence){
  initialise_amp = function(program, start_input, phase_setting){
    list(
      output = start_input,
      position = 1,
      program = program,
      phase = phase_setting,
      stop_99 = FALSE
    )
  }
  amp_a = initialise_amp(program, 0, phase_setting_sequence[[1]])
  amp_b = initialise_amp(program, 0, phase_setting_sequence[[2]])
  amp_c = initialise_amp(program, 0, phase_setting_sequence[[3]])
  amp_d = initialise_amp(program, 0, phase_setting_sequence[[4]])
  amp_e = initialise_amp(program, 0, phase_setting_sequence[[5]])
    
  while(!amp_e[["stop_99"]]){
    amp_a = intcode_computer(amp_a[["program"]], amp_e[["output"]], amp_a[["phase"]], amp_a[["position"]], amp_a[["output"]])
    amp_b = intcode_computer(amp_b[["program"]], amp_a[["output"]], amp_b[["phase"]], amp_b[["position"]], amp_b[["output"]])
    amp_c = intcode_computer(amp_c[["program"]], amp_b[["output"]], amp_c[["phase"]], amp_c[["position"]], amp_c[["output"]])
    amp_d = intcode_computer(amp_d[["program"]], amp_c[["output"]], amp_d[["phase"]], amp_d[["position"]], amp_d[["output"]])
    amp_e = intcode_computer(amp_e[["program"]], amp_d[["output"]], amp_e[["phase"]], amp_e[["position"]], amp_e[["output"]])
  }
  return(amp_e[["output"]])
}

max_thruster_signal(program, 0, 5:9)

