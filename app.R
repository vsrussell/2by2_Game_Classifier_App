library(shiny)
library(dplyr)

#FUNCTIONS ---------------------------------------------------------------------

check_super_dom <- function(values_p1, values_p2, row_strat_ls, col_strat_ls){
  super_dom_row <- NA
  super_dom_col <- NA
  
  #check row
  if(min(values_p1[1], values_p1[2]) > max(values_p1[3], values_p1[4])){
    super_dom_row <- row_strat_ls[1]
  }
  if(min(values_p1[3], values_p1[4]) > max(values_p1[1], values_p1[2])){
    super_dom_row <- row_strat_ls[2]
  }
  
  #check col
  if(min(values_p2[1], values_p2[3]) > max(values_p2[2], values_p2[4])){
    super_dom_col <- col_strat_ls[1]
  }
  if(min(values_p2[2], values_p2[4]) > max(values_p2[1], values_p2[3])){
    super_dom_col <- col_strat_ls[2]
  }
  return(list(super_dom_row, super_dom_col))
}

check_dom <- function(values_p1, values_p2, row_strat_ls, col_strat_ls){
  dom_row <- NA
  dom_col <- NA
  
  if((values_p1[1] > values_p1[3]) & (values_p1[2] > values_p1[4])){
    dom_row <- row_strat_ls[1]
  }
  if((values_p1[1] < values_p1[3]) & (values_p1[2] < values_p1[4])){
    dom_row <- row_strat_ls[2]
  }
  if((values_p2[1] > values_p2[2]) & (values_p2[3] > values_p2[4])){
    dom_col <- col_strat_ls[1]
  }
  if((values_p2[1] < values_p2[2]) & (values_p2[3] < values_p2[4])){
    dom_col <- col_strat_ls[2]
  }
  return(list(dom_row, dom_col))
}

find_all_PSNE <- function(values_p1, values_p2){
  row_C1 <- max(values_p1[1], values_p1[3])
  row_C1 <- grep(paste0("^",row_C1,"$"), values_p1) #forces exact matches
  row_C2 <- max(values_p1[2], values_p1[4])
  row_C2 <- grep(paste0("^",row_C2,"$"), values_p1) #forces exact matches
  
  col_R1 <- max(values_p2[1], values_p2[2])
  col_R1 <- grep(paste0("^",col_R1,"$"), values_p2) #forces exact matches
  col_R2 <- max(values_p2[3], values_p2[4])
  col_R2 <- grep(paste0("^",col_R2,"$"), values_p2) #forces exact matches
  
  PSNE <- list()
  for(i in c(row_C1, row_C2)){ #if col and row point to same box count a PSNE
    if(i %in% c(col_R1, col_R2)){
      PSNE <- c(PSNE, i)
    }
  }
  return(PSNE)
}

master_servant_class <- function(values_p1, values_p2, psne, master){
  game <- ""
  subgame <- ""
  strategy <- ""
  if(master==2){ #if row master
    if(values_p1[psne]==max(values_p1)){ #if best value for master (row) in psne
      game <- "Master and Beloved Servant"
      strategy <- "Timing Irrelevant"
    }
    else{ #if best value for master (row) not in psne
      game <- "Master and Annoying Servant"
      if(values_p2[psne]==max(values_p2)){ #if "servant" (col) max is in PSNE
        subgame <- "Market Entry"
        strategy <- "Both prefer themselves first"
      }
      else{ #if servant" (col) max is not in PSNE
        subgame <- "Apprentice Dilemma"
        strategy <- "Both prefer Apprentice (Master/Row) first"
      }
    }
  }
  if(master==1){ #if master is column
    if(values_p2[psne]==max(values_p2)){ #if best value for master (col) in psne
      game <- "Master and Beloved Servant"
      strategy <- "Timing Irrelevant"
    }
    else{ #if best value for master (col) not in psne
      game <- "Master and Annoying Servant"
      if(values_p1[psne]==max(values_p1)){ #if "servant" (row) max is in PSNE
        subgame <- "Market Entry"
        strategy <- "Both prefer themselves first"
      }
      else{ #if servant" (col) max is not in PSNE
        subgame <- "Apprentice Dilemma"
        strategy <- "Both prefer Apprentice (Master/Col) first"
      }
    }
  }
  return(c(game, subgame, strategy))
}

hide_and_seek <- function(values_p1, values_p2){
    subgame <- ""
    strategy <- ""
  
    max_p1 <- sort(values_p1)[4]
    max2_p1 <- sort(values_p1)[3]
    min2_p1 <- sort(values_p1)[2]
    min_p1 <- sort(values_p1)[1]
    
    max_p2 <- sort(values_p2)[4]
    max2_p2 <- sort(values_p2)[3]
    min2_p2 <- sort(values_p2)[2]
    min_p2 <- sort(values_p2)[1]
    
    pos_max_p1 <- match(max_p1, values_p1) #position/box
    pos_max2_p1 <- match(max2_p1, values_p1)
    pos_min2_p1 <- match(min2_p1, values_p1) #position/box
    pos_min_p1 <- match(min_p1, values_p1)
    
    pos_max_p2 <- match(max_p2, values_p2) #position/box
    pos_max2_p2 <- match(max2_p2, values_p2)
    pos_min2_p2 <- match(min2_p2, values_p2) #position/box
    pos_min_p2 <- match(min_p2, values_p2)
    
    #determine if two max for row are in same col
    row_cares <- FALSE
    col_cares <- FALSE
    
    #check row
    if(min(values_p1[1], values_p1[3]) > max(values_p1[2], values_p1[4]) | #if top two row values in first col
       min(values_p1[2], values_p1[4]) > max(values_p1[1], values_p1[3])){ #or second col
      row_cares <- TRUE
    }
    
    #check col
    if(min(values_p2[1], values_p2[2]) > max(values_p2[3], values_p2[4]) | #if top two col values in first row
       min(values_p2[3], values_p2[4]) > max(values_p2[1], values_p2[2])){ #or second row
       col_cares <- TRUE
    }
    
    if(!(row_cares) & !(col_cares)){ #if p2 maxes not equal to any p1 maxes
      subgame <- "Cat and Mouse"
      strategy <- "Both prefer themselves last"
    }
    if(row_cares & col_cares){
      supplier <- which(c(pos_max_p1==pos_min_p2, pos_max_p2==pos_min_p1))
      if(length(supplier)==1){
        if(supplier==1){
          subgame <- "Hold Up"
          strategy <- "Both want Row/Supplier to move first"
        }
        else{
          subgame <- "Hold Up"
          strategy <- "Both want Col/Supplier to move first"
        }
      }
    }
    if(subgame==""){ #if still not classified
      if(pos_min2_p1==pos_min2_p2){#if third best in same position
        if(row_cares){
          subgame <- "Intimidation"
          strategy <- "Both want Target/Row to move first"
        }
        else{
          subgame <- "Intimidation"
          strategy <- "Both want Target/Col to move first"
        }
      } 
      else{ #if nothing else this is last option left
        subgame <- "Staffing Dilemma"
        strategy <- "Both prefer themselves last"
      }
    }
    return(c(subgame, strategy))
}

classify_game <- function(values_p1, values_p2, row_name_ls, col_name_ls){
  super_dom_strat <- unlist(check_super_dom(values_p1, values_p2, row_name_ls, col_name_ls))
  dom_strat <- unlist(check_dom(values_p1, values_p2, row_name_ls, col_name_ls))
  max_row <- max(values_p1)
  max_col <- max(values_p2)
  game <- ""
  strategy <- ""
  subgame <- ""

  if(!all(is.na(super_dom_strat))){ #if any superdominant strategy find
    if(sum(is.na(super_dom_strat))==1){
      game <- "Immovable Object" #two superdom
      strategy <- "Timing Irrelevant"
    }
    else{
      game <- "Slam Dunk" #one super dom
      strategy <- "Timing Irrelevant"
    }
  }
  else{ #move on to dominant if no superdominant strategy
    # Create an empty 2x2 data frame with specified row and column names
    if(!all(is.na(dom_strat))){ #if any superdominant strategy find
      if(sum(!is.na(dom_strat))>1){
        pos_row <- grep(unlist(check_dom(values_p1, values_p2, row_name_ls, col_name_ls))[1], row_name_ls)
        pos_col <- grep(unlist(check_dom(values_p1, values_p2, row_name_ls, col_name_ls))[2], col_name_ls)
        
        df <- data.frame(Column1 = NA, Column2 = NA)
        df[1,1] <- toString(list(values_p1[[1]], values_p2[[1]]))
        df[1,2] <- toString(list(values_p1[[2]], values_p2[[2]]))
        df[2,1] <- toString(list(values_p1[[3]], values_p2[[3]]))
        df[2,2] <- toString(list(values_p1[[4]], values_p2[[4]]))
        
        values <- as.numeric(unlist(strsplit(df[pos_row, pos_col],", ")))
        if(max_row==values[1] & max_col==values[2]){
          game <- "Happy Marriage"
          strategy <- "Timing Irrelevant"
        }
        if(max_row!=values[1] & max_col!=values[2]){
          game <- "Prisoner's Dilemma"
          strategy <- "Timing Irrelevant"
        }
        if(game==""){ #if not HM or PD
          game <- "Food Fight"
          strategy <- "Timing Irrelevant"
        }
      }
      if(sum(is.na(dom_strat))==1){ #if only one dominant strategy
        psne <- unlist(find_all_PSNE(values_p1, values_p2)) #find PSNE location
        which_dom <- which(is.na(dom_strat)) #find which is master by dominant strategy
        master_serv <- master_servant_class(values_p1, values_p2, psne, which_dom) #get master/servant class
        game <- master_serv[1]
        subgame <- master_serv[2]
        strategy <- master_serv[3]
      }
    }
    else{
      #Find PSNE
      PSNE <- unlist(find_all_PSNE(values_p1, values_p2))
      if(length(PSNE)==2){
        #check if both like outcome
        fav_row <- which.max(c(values_p1[PSNE[1]], values_p1[PSNE[2]]))
        fav_col <- which.max(c(values_p2[PSNE[1]], values_p2[PSNE[2]]))
        if(fav_row==fav_col){
          game <- "Assurance"
          strategy <- "Both prefer sequential"
        }
        else{
          game <- "Chicken"
          strategy <- "Both prefer themselves first"
        }
      }
      else{
        game <- "Hide and Seek"
        hide_seek <- hide_and_seek(values_p1, values_p2)
        subgame <- hide_seek[1]
        strategy <- hide_seek[2]
        }
      }
    }
  return(c(game, subgame, strategy))
}

#UI ----------------------------------------------------------------------------

# Define UI
ui <- fluidPage(
  titlePanel("Interactive 2x2 Game Solver"),
  
  # Description panel just below the title panel
  div(
    p("Instructions: Please enter unique numeric values for row and column payoff combinations in the style of row_val, col_val (white space allowed).
      Feel free to relabel row and column choices to fit the game being modeled.
      Then check what game type and strategy by selecting 'Solve Game Type.'"),
    style = "padding: 20px;"  # Add some padding around the text for better spacing
  ),
  
  # Custom CSS for matrix styling
  tags$style(type = 'text/css', "
    .matrix-input .form-group {
      border: 2px solid #000;
      padding: 10px;
      border-radius: 5px;
      margin-bottom: 0px; /* Remove the default bottom margin */
    }
    .matrix-label {
      text-align: center;
      font-weight: bold;
      margin-top: 25px; /* Align with inputs */
    }
  "),
  
  # Matrix with editable row and column labels
  fluidRow(
    column(width = 2, offset = 1),
    column(width = 3, div(textInput("col1_name", label = NULL, value = "C"), class = "matrix-label")),
    column(width = 3, div(textInput("col2_name", label = NULL, value = "D"), class = "matrix-label"))
  ),
  fluidRow(
    column(width = 2, offset = 1, div(textInput("row1_name", label = NULL, value = "A"), class = "matrix-label")),
    column(width = 3, div(textInput("cell1", label = NULL, placeholder = "row_val1, col_val1"), class = "matrix-input")),
    column(width = 3, div(textInput("cell2", label = NULL, placeholder = "row_val1, col_val2"), class = "matrix-input"))
  ),
  fluidRow(
    column(width = 2, offset = 1, div(textInput("row2_name", label = NULL, value = "B"), class = "matrix-label")),
    column(width = 3, div(textInput("cell3", label = NULL, placeholder = "row_val2, col_val1"), class = "matrix-input")),
    column(width = 3, div(textInput("cell4", label = NULL, placeholder = "row_val2, col_val2"), class = "matrix-input"))
  ),
  
  # Action button to process the matrix
  fluidRow(
    column(width = 2, offset = 5, actionButton("solve", "Solve Game Type"))
  ),
  
  # Placeholder for output
  mainPanel(
    uiOutput("result")
  )
)

#SERVER ------------------------------------------------------------------------

# Define server logic
server <- function(input, output) {
  observeEvent(input$solve, {
    #get labels
    row_label1 <- input$row1_name
    row_label2 <- input$row2_name
    
    col_label1 <- input$col1_name
    col_label2 <- input$col2_name
    
    values_clean <- c(sapply(list(input$cell1, input$cell2, input$cell3, input$cell4), function(cell) {
      gsub("\\s+", "", cell)})) #remove whitespace
    
    values_check <- c(sapply(values_clean, function(cell) {
      grepl("^-?[0-9]+,-?[0-9]+$", cell)}))
    
    if(all(values_check)){
      # Extract the first value before the comma for each cell
      value1_list <- c(sapply(values_clean, function(cell) {
        as.numeric(strsplit(cell, ",")[[1]][1] %>% trimws())
      }))
      
      value2_list <- c(sapply(values_clean, function(cell) {
        as.numeric(strsplit(cell, ",")[[1]][2] %>% trimws())
      }))
      
      #check no tied values (could complicate things)
      if(length(unique(value1_list))<4 | length(unique(value2_list))<4){
        rep_message <- ""
        if(length(unique(value1_list))<4){
          rep_message <- paste(rep_message," There is repetition in row values.")
        }
        if(length(unique(value2_list))<4){
          rep_message <- paste(rep_message," There is repetition in column values.")
        }
        output$result <- renderPrint({
          HTML(paste(rep_message,"Make sure all 4 row_val and col_val values are unique"))
        })
      }
      else{
        classify_game <- classify_game(value1_list, value2_list, c(row_label1, row_label2), c(col_label1, col_label2))
        
        game <- paste0("Game: ",classify_game[1])
        strategy <- paste0("Strategy: ",classify_game[3])
        if(classify_game[2]==""){
          subgame <- ""
        }
        else{
          subgame <- paste0("Game subcategory: ",classify_game[2])
        }
        
        # Render the list of first values
        output$result <- renderPrint({
          HTML(paste(game, subgame, strategy, sep = "<br>"))
        })
      }
      if(!all(values_check)){
        output$result <- renderPrint({
          HTML(paste("User input wrong format, please input in format: #,#"))
        })
      }
    }
  })
}


#RUN ---------------------------------------------------------------------------

# Run the app
shinyApp(ui = ui, server = server)

