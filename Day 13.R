#Day 13

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
    phase = phase_setting,
    relative_base = relative_base
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
      output[["position"]] = position
      output[["program"]] = program
      output[["relative_base"]] = relative_base
      n_output = n_output + 1
      
      #return(output) #return output each times
      
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
      output[["relative_base"]] = relative_base
      output[["output"]] = unlist(output[["output"]])
      return(output)
      
    } else {
      stop("Error ! Error !")
      
    }
  }
}


input = read.table("Input/ex13.txt", stringsAsFactors = FALSE)
input = unlist(strsplit(input$V1, ","))
input = as.numeric(input)

game = function(program, input = NA){
  x = intcode_computer(program, input)
  frame = x[["output"]]
  
  frame = data.frame(
    x = frame[1:length(frame) %% 3 == 1],
    y = frame[1:length(frame) %% 3 == 2],
    tile = frame[1:length(frame) %% 3 == 0]
  )
  output = list(frame = frame, output = x)
  return(output)
  
}

data = game(input)

length(data[["frame"]]$tile[data[["frame"]]$tile == 2])
data[["frame"]][data[["frame"]]$tile == 4,]
data[["frame"]][data[["frame"]]$tile == 3,]
## Part 2

arcade_cabinet = function(program, input = NA, phase_setting = input, position = 1, output = NA, relative_base = 0){
  
  
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
  if(position == 0){#no phase setting here
    phase_used = FALSE
  }
  
  n_output = 1
  #state information and output
  output = list(
    output = list(),
    position = position,
    program = program,
    stop_99 = FALSE,
    phase = phase_setting,
    relative_base = relative_base
  )
  
  game_state = list()
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
    
    
    #update game_state
    if((n_output - 1) %% 3 == 0 & n_output != 1){
      x = output[["output"]][[n_output-3]]
      y = output[["output"]][[n_output-2]]
      tile = output[["output"]][[n_output-1]]
      xy = paste0(x, ", ", y)
      game_state[[xy]] = list(x = x, y = y, tile = tile, xy = xy)
    }
    
    
    if(instruction[["op_code"]] == 1){
      program[position_c] = param_a + param_b
      position = position + 4
      
    } else if(instruction[["op_code"]] == 2) {
      program[position_c] = param_a * param_b
      position = position + 4
      
      
    } else if(instruction[["op_code"]] == 3) {
      
      ball_position = game_state[sapply(game_state, function(x)  x[["tile"]] == 4 & x[["xy"]] != "0, -1")][[1]] #last position of the ball
      paddle_position = game_state[sapply(game_state, function(x)  x[["tile"]] == 3 & x[["xy"]] != "0, -1")][[1]]  #last position of the paddle
      
      if(ball_position[["y"]] > paddle_position[["y"]]){stop("YOU LOST")}
      
      
      input = if(ball_position[["x"]] == paddle_position[["x"]]){0} else if(ball_position[["x"]] > paddle_position[["x"]]){1} else{-1}
      
      program[position_a] = input #ifelse(phase_used, input, phase_setting) no phase_setting
      phase_used = TRUE
      position = position + 2
      
      
    } else if(instruction[["op_code"]] == 4) {
      position = position + 2
      output[["output"]][[n_output]] = param_a
      output[["position"]] = position
      output[["program"]] = program
      output[["relative_base"]] = relative_base
      n_output = n_output + 1
      
      #return(output) #return output each times
      
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
      output[["relative_base"]] = relative_base
      output[["output"]] = unlist(output[["output"]])
      output[["game_state"]] = game_state
      return(output)
      
    } else {
      stop("Error ! Error !")
      
    }
  }
}
input[1] = 2
x = arcade_cabinet(input)
x[["game_state"]][x[["game_state"]]$x == -1 & x[["game_state"]]$y == 0,]
x[["game_state"]][["-1, 0"]]

