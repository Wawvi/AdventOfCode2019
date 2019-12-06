#Day 1

## Part 1
fuel_req = function(mass){
  
  floor(mass/3)-2
  
}
input = read.table("Input/ex1.txt")
sum(fuel_req(input$V1))

## Part 2

fuel_req = function(mass){
  #new fuel required formula
  fuel = floor(mass/3) - 2
  return(ifelse(fuel < 0, 0, fuel))
}

module_fuel_req_list = function(mass){
  #recursive fuel requirement list of a module
  return_list = list()
  i = 0
  while(fuel_req(mass) > 0){
    i = i + 1
    mass = fuel_req(mass)
    return_list[[i]] = mass
  }
  return(return_list)
}

module_fuel_req = function(mass) {sum(unlist(module_fuel_req_list(mass)))} #sum fuel requirement of a module

total_fuel = function(mass_vector){sum(sapply(mass_vector, function(x) module_fuel_req(x)))} #sum fuel requirement of all modules in mass vector

total_fuel(input$V1)

