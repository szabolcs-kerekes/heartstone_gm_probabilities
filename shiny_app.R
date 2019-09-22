library(shiny)

data <- fread('test_data.csv')

archetypes <- data[,.(Archetype, Class),]
probs <- melt(data, id.vars = c("Archetype", "Class"), variable.name = "Versus", value.name = "Winrate")

ui <- fluidPage(
  
  titlePanel("Odds calculator"),
  
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      checkboxGroupInput('player1_decks', 'Player 1',
                         unique(archetypes$Archetype)),
      checkboxGroupInput('player2_decks', 'Player 2',
                         unique(archetypes$Archetype)),
      
      numericInput('sim_no', 'Number of simulations', 1, 100000, 1),
      
      numericInput('switch', 'Switch', 0, 1, 1)
    ),
    
    mainPanel(verbatimTextOutput("winrate"),
              verbatimTextOutput("deck1"))
    
  )
  
)

server <- function(input, output){
  
  data <- fread('test_data.csv')
  
  archetypes <- data[,.(Archetype, Class),]
  probs <- melt(data, id.vars = c("Archetype", "Class"), variable.name = "Versus", value.name = "Winrate")
  
  
  reactive_deck1 <- reactive({
    deck1 <- input$player1_decks
    
    return(deck1)
  })
  
  output$deck1 <- renderText(reactive_deck1())
  
  
  reactive_winrate_calc <- reactive({
    
  temp_results <- list()
  
  cycles <- input$sim_no * input$switch
    
  for (i in 1:cycles){
    deck1 <- input$player1_decks
    deck2 <- input$player2_decks
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
    
    temp_results[[i]] <- match_result
  }
  
  temp_results <- rbindlist(temp_results)
  
  if (input$switch == 1){
    winrate <- mean(temp_results$result)
  } else {
    winrate <- 0
  }
  
  return(winrate)
  
  })
  

output$winrate <- renderText(reactive_winrate_calc())
  
}

shinyApp(ui, server)