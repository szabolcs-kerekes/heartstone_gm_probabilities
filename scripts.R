library(data.table)

### Get data in csv format
data <- fread('test_data.csv')

archetypes <- data[,.(Archetype, Class),]
probs <- melt(data, id.vars = c("Archetype", "Class"), variable.name = "Versus", value.name = "Winrate")

### Simulate deck choosing
selector <- function(archetypes){
  build <- c()
  classes <- c()
  archs <- nrow(archetypes)

  # First build
  first_arch_no <- sample(archs, 1)
  first_arch <- archetypes[first_arch_no,Archetype,]
  first_class <- archetypes[first_arch_no,Class,]  
  
  build <- c(build, first_arch)  
  classes <- c(classes, first_class)
    
  # Second build
  second_class <- classes
  while(second_class %in% classes){
    second_arch_no <- sample(archs, 1)
    second_arch <- archetypes[second_arch_no,Archetype,]
    second_class <- archetypes[second_arch_no,Class,]  
  }
  
  build <- c(build, second_arch)
  classes <- c(classes, second_class)
  
  # Third build
  third_class <- classes[1]
  while(third_class %in% classes){
    third_arch_no <- sample(archs, 1)
    third_arch <- archetypes[third_arch_no,Archetype,]
    third_class <- archetypes[third_arch_no,Class,]  
  }

  build <- c(build, third_arch)
  classes <- c(classes, third_class)  
  
  # Fourth build
  fourth_class <- classes[1]
  while(fourth_class %in% classes){
    fourth_arch_no <- sample(archs, 1)
    fourth_arch <- archetypes[fourth_arch_no,Archetype,]
    fourth_class <- archetypes[fourth_arch_no,Class,]  
  }
  
  build <- c(build, fourth_arch)
  classes <- c(classes, fourth_class) 
  
  return(build)
}

### Simulate battle via probabilities
# sample from the probability

all_results <- list()

start <- Sys.time()

for (i in 1:100000){
  deck1 <- selector(archetypes)
  deck2 <- selector(archetypes)
  d1_id <- paste(sort(deck1),collapse = " / ")
  d2_id <- paste(sort(deck2),collapse = " / ")
  
  win_list <- c()
  # 1st match
  d1_m1 <- deck1[sample(4,1)]
  d2_m1 <- deck2[sample(4,1)]
  
  d1_d2_prob <- probs[Archetype == d1_m1 & Versus == d2_m1, Winrate]
  
  winner <- 
    ifelse(sample(c(0, 1), size = 1, prob = c(d1_d2_prob, 1-d1_d2_prob)) == 0, 'd2', 'd1')
  
  win_list <- c(win_list, winner)
  
  if(winner == 'd1'){
    deck1 <- deck1[!deck1 %in% d1_m1]
    d1_m2 <- deck1[sample(3,1)]
    d2_m2 <- deck2[sample(4,1)]
    
  } else {
    deck2 <- deck2[!deck2 %in% d2_m1]
    d2_m2 <- deck2[sample(3,1)]
    d1_m2 <- deck1[sample(4,1)]
  }
  
  d1_d2_prob <- probs[Archetype == d1_m2 & Versus == d2_m2, Winrate]
  
  winner <- 
    ifelse(sample(c(0, 1), size = 1, prob = c(d1_d2_prob, 1-d1_d2_prob)) == 0, 'd2', 'd1')
  
  win_list <- c(win_list, winner)
  
  if(sum(win_list == 'd1') > 1 | sum(win_list == 'd2') > 1){
    outcome <- ifelse(sum(win_list == 'd1') > 1, 1, 0)
    
    match_result <- list('deck1' = d1_id, 'deck2' = d2_id, 'result' = outcome)    
  } else {
    
    if(winner == 'd1'){
      deck1 <- deck1[!deck1 %in% d1_m2]
      d1_m3 <- deck1[sample(length(deck1),1)]
      d2_m3 <- deck2[sample(length(deck2),1)]
      
    } else {
      deck2 <- deck2[!deck2 %in% d2_m2]
      d2_m3 <- deck2[sample(length(deck2),1)]
      d1_m3 <- deck1[sample(length(deck1),1)]
    }
    
    d1_d2_prob <- probs[Archetype == d1_m2 & Versus == d2_m2, Winrate]
    
    winner <- 
      ifelse(sample(c(0, 1), size = 1, prob = c(d1_d2_prob, 1-d1_d2_prob)) == 0, 'd2', 'd1')
    
    win_list <- c(win_list, winner)
    
    outcome <- ifelse(sum(win_list == 'd1') > 1, 1, 0)
    
    match_result <- list('deck1' = d1_id, 'deck2' = d2_id, 'result' = outcome)
  }

  all_results[[i]] <- match_result
  

}
end <- Sys.time()
print(end-start)

all_results <- rbindlist(all_results)

all_results[,.(winrate = mean(result), count = .N),by = .(deck1)][order(-winrate, -count)]

# 2nd match is winner not participating


# 3rd match to decide

### Aggregate results

### Visualise

