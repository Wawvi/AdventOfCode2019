#Day 6

##Part 1


input = read.table("Input/ex6.txt", stringsAsFactors = FALSE)

#compute all paths from each planet to COM
orbit_from_each_point = function(input){
  input = list(
    center = sapply(input, function(x) strsplit(x, ")")[[1]][1]),
    planet = sapply(input, function(x) strsplit(x, ")")[[1]][2])
  )
  
  output = list()
  max_path = length(input[["planet"]])
  
  for(path in 1:max_path){
    output[[path]] = list(center = input[["center"]][path],
                          planet = input[["planet"]][path])
    
    n_in_chain = 1
    next_in_chain = output[[path]][["center"]][n_in_chain]
    
    while(next_in_chain != "COM"){
      
      index = which(input[["planet"]] == next_in_chain)
      
      output[[path]][["center"]] = c(output[[path]][["center"]],  input[["center"]][index])
      output[[path]][["planet"]] = c(output[[path]][["planet"]], input[["planet"]][index])
              
      n_in_chain = n_in_chain + 1
      next_in_chain = output[[path]][["center"]][n_in_chain]
      
    }
    
  }
  return(output)
  
}

total_orbits = function(x) {sum(sapply(x, function(x) length(x[["planet"]])))}

x = orbit_from_each_point(input$V1)
total_orbits(x)

##Part 2

names(x)  = sapply(x, function(x) names(x) <- x[["planet"]][1])

#At some point, the path from YOU and SAN to COM will cross, where does it first cross, how many steps are needed
path_from_YOU = x[["YOU"]]$center
path_from_SAN = x[["SAN"]]$center

min(which(path_from_YOU %in% path_from_SAN)) + min(which(path_from_SAN %in% path_from_YOU)) - 2 




