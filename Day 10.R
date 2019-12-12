#Day 10

## Part 1
treat_input = function(input){
  x = t(sapply((sapply(strsplit(input, ""), as.data.frame, stringsAsFactors = FALSE)), rbind))
  x = data.frame(x,stringsAsFactors = FALSE)
  colnames(x) = paste0("X", seq(1:ncol(x)))
  rownames(x) = seq(1:nrow(x))
  return(x)
}


find_asteroid = function(input){which(input == "#", arr.ind = TRUE, useNames = FALSE)}

angle_to_asteroid = function(coords_asteroid_start, coords_asteroid_end){
  #compute angle between a perpendicular vector starting from asteroid_start and the vector asteroid_start => asteroid_end
  row_start = coords_asteroid_start[1]
  col_start = coords_asteroid_start[2]
  
  row_end = coords_asteroid_end[1]
  col_end = coords_asteroid_end[2]
  
  vector_xline = c(1, 0)
  vector2 = c(col_end - col_start, row_end - row_start)
  vector2_len = sqrt((col_end - col_start)^2 + (row_end - row_start)^2)
  
  above_line = ifelse(row_end - row_start >= 0, 1, -1)
  cos_of_angle = (vector_xline %*% vector2)/(1%*%vector2_len)
  angles = above_line*acos(cos_of_angle)*180/pi
  return(round(angles, 8))

}

angle_with_asteroids = function(coords_asteroid_start, all_asteroids){
  sapply(1:nrow(all_asteroids), function(i) angle_to_asteroid(coords_asteroid_start, all_asteroids[i,]))
}


count_asteroid_in_view = function(coords_asteroid_start, all_asteroids){
  angles = angle_with_asteroids(coords_asteroid_start, all_asteroids)
  length(unique(angles[!is.nan(angles)]))
}

best_asteroid = function(input){
  coord_asteroids = find_asteroid(input)
  coord_asteroids[which.max(sapply(1:nrow(coord_asteroids), function(i) count_asteroid_in_view(coord_asteroids[i,], coord_asteroids))),]
}#gives Y, X + 1






input = read.csv("Input/ex10.txt", sep="", dec=",", header = FALSE, stringsAsFactors = FALSE)
input = treat_input(input$V1)
count_asteroid_in_view(best_asteroid(input), find_asteroid(input))


##Part 2

distance_to_asteroid = function(coords_asteroid_start, coords_asteroid_end){
  #compute angle between a perpendicular vector starting from asteroid_start and the vector asteroid_start => asteroid_end
  row_start = coords_asteroid_start[1]
  col_start = coords_asteroid_start[2]
  
  row_end = coords_asteroid_end[1]
  col_end = coords_asteroid_end[2]
  
  return(sqrt((col_end - col_start)^2 + (row_end - row_start)^2))
}

distance_to_asteroids = function(coords_asteroid_start, all_asteroids){
  sapply(1:nrow(all_asteroids), function(i) distance_to_asteroid(coords_asteroid_start, all_asteroids[i,]))
}

destroy_order = function(input){
  
  asteroids = find_asteroid(input)
  angles_to_station = angle_with_asteroids(best_asteroid(input), find_asteroid(input))
  rectified_angles = ifelse(angles_to_station < 0, angles_to_station + 450, angles_to_station + 90)
  rectified_angles = ifelse(rectified_angles >= 360, rectified_angles - 360, rectified_angles) 
  distance_to_station = distance_to_asteroids(best_asteroid(input), find_asteroid(input))
  
  x = data.frame(
    x = asteroids[,2],
    y = asteroids[,1],
    angle = rectified_angles,
    distance = distance_to_station
  )
  x = x[order(x$angle,x$distance),]
  x = x[!is.na(x$angle),]
  rownames(x) = 1:nrow(x)
  x$id = 1:nrow(x)
  
  library(dplyr)
  x = x %>%
    group_by(angle) %>%
    mutate(turn = row_number())
  
  destroy_order = list()
  n = 1
  for(n_turn in unique(x$turn)){
    drop = x[x$turn == n_turn,]
    ndrop = 1
    while(ndrop <= nrow(drop)){
      destroy_order[[n]] = drop[ndrop,]
      n = n + 1
      ndrop = ndrop + 1
    }
    
  }
  
  return(destroy_order)

}

destroy_order_from_station = destroy_order(input)
(destroy_order_from_station[[200]]$x - 1)*100 + (destroy_order_from_station[[200]]$y - 1)
