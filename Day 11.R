#Day 11

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
      
      return(output) #return output each times
      
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
      return(output)
      
    } else {
      stop("Error ! Error !")
      
    }
  }
}

program = c(3,8,1005,8,339,1106,0,11,0,0,0,104,1,104,0,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,0,10,4,10,1002,8,1,29,2,1108,11,10,1,1,20,10,2,107,6,10,3,8,102,-1,8,10,101,1,10,10,4,10,108,0,8,10,4,10,101,0,8,62,1006,0,29,1006,0,12,1,1101,5,10,1,2,20,10,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,0,10,4,10,1001,8,0,99,1006,0,30,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,0,10,4,10,1001,8,0,124,1006,0,60,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,1,10,4,10,101,0,8,149,2,1007,2,10,1,1105,10,10,3,8,1002,8,-1,10,101,1,10,10,4,10,108,0,8,10,4,10,101,0,8,178,1,1108,15,10,1,1101,5,10,1,109,8,10,1006,0,20,3,8,102,-1,8,10,1001,10,1,10,4,10,108,1,8,10,4,10,101,0,8,215,1006,0,61,1006,0,16,2,1105,15,10,1006,0,50,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,1,8,10,4,10,101,0,8,250,1,1003,10,10,1,9,19,10,2,1004,6,10,2,1106,2,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,1,10,4,10,101,0,8,289,1,1103,13,10,2,105,17,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,1,8,10,4,10,1002,8,1,318,101,1,9,9,1007,9,1086,10,1005,10,15,99,109,661,104,0,104,1,21101,0,825599304340,1,21101,356,0,0,1106,0,460,21101,0,937108545948,1,21102,1,367,0,1106,0,460,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21102,1,21628980315,1,21101,0,414,0,1105,1,460,21101,0,3316673539,1,21101,425,0,0,1106,0,460,3,10,104,0,104,0,3,10,104,0,104,0,21102,988753428840,1,1,21102,1,448,0,1106,0,460,21102,825544569700,1,1,21102,459,1,0,1106,0,460,99,109,2,21202,-1,1,1,21102,1,40,2,21102,491,1,3,21102,481,1,0,1105,1,524,109,-2,2106,0,0,0,1,0,0,1,109,2,3,10,204,-1,1001,486,487,502,4,0,1001,486,1,486,108,4,486,10,1006,10,518,1101,0,0,486,109,-2,2105,1,0,0,109,4,2102,1,-1,523,1207,-3,0,10,1006,10,541,21102,0,1,-3,21201,-3,0,1,22102,1,-2,2,21102,1,1,3,21102,560,1,0,1106,0,565,109,-4,2105,1,0,109,5,1207,-3,1,10,1006,10,588,2207,-4,-2,10,1006,10,588,22101,0,-4,-4,1105,1,656,21202,-4,1,1,21201,-3,-1,2,21202,-2,2,3,21102,1,607,0,1106,0,565,22102,1,1,-4,21101,0,1,-1,2207,-4,-2,10,1006,10,626,21101,0,0,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,648,21202,-1,1,1,21101,0,648,0,105,1,523,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2105,1,0)



painting_robot = function(program, panel_color = 0){
  step = 1
  stop_99 = FALSE
  current_coords = "X = 0, Y = 0"
  panel_state = list()
  panel_state[[current_coords]] = c(X = 0, Y = 0, color = panel_color, step = step, orientation = 0)
  
  next_orientation = function(orientation, direction){
    angle = ifelse(direction == 0, -90, 90)
    new_orientation = orientation + angle
    
    new_orientation = if(new_orientation < 0){
      360 + new_orientation
    } else if(new_orientation >= 360) {
      new_orientation - 360
    } else {
      new_orientation
    }
    new_orientation
  }
  
  next_position = function(X, Y, orientation){
    if(orientation == 0){
      return(c(X = X, Y = Y + 1))
    } else if(orientation == 270){
      return(c(X = X - 1, Y = Y))
    } else if(orientation == 180){
      return(c(X = X, Y = Y - 1))
    } else {
      return(c(X = X + 1, Y = Y))
    }
  }
  
  next_color = function(next_position_string){
    if(any(next_position_string %in% names(panel_state))){
      return(panel_state[[next_position_string]][["color"]])
    } else {return(0)}
  }
  
  
  program_position = 1
  relative_base = 0
  while(TRUE){
    step = step + 1
    intcode_output = intcode_computer(program, input = panel_state[[current_coords]][["color"]], position = program_position, relative_base = relative_base)
    
    #stop condition
    stop_99 = intcode_output[["stop_99"]]
    if(stop_99){return(panel_state)}
    
    #update next incode_computer input
    program = intcode_output[["program"]]
    program_position = intcode_output[["position"]]
    relative_base = intcode_output[["relative_base"]]
    
    #output is color or movement ?
    if(step %% 2 == 0){
      
      panel_state[[current_coords]][["color"]] = intcode_output[["output"]][[1]] #change color of current panel
      
    } else {
      
      #move to next panel
      new_orientation = next_orientation(panel_state[[current_coords]][["orientation"]], intcode_output[["output"]][[1]])
      new_position = next_position(panel_state[[current_coords]][["X"]], panel_state[[current_coords]][["Y"]], new_orientation)
      current_coords = paste0(paste0("X = ", new_position[["X"]], ", Y = ",  new_position[["Y"]]))
      new_color = next_color(current_coords)
      
      #state of new panel
      panel_state[[current_coords]] = c(X = new_position[["X"]], Y = new_position[["Y"]], 
                                        color = new_color, 
                                        step = step, 
                                        orientation = new_orientation)
    }
    
  }
  
}

result = painting_robot(program, 0)

## Part 2

result = painting_robot(program, panel_color = 1)

df = data.frame(
  X = sapply(1:250, function(x) result[[x]][["X"]]),
  Y = sapply(1:250, function(x) result[[x]][["Y"]]),
  color = sapply(1:250, function(x) result[[x]][["color"]])
)

df$color = ifelse(df$color == 1, "#", ".")
library(ggplot2)
ggplot(df) +
  geom_text(aes(x = X, y = Y, label = color))


