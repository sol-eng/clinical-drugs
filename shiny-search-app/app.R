
library(shiny)
library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(htmltools)
library(bigrquery)
library(visNetwork)
library(openfda)

definitions <- read_csv("definitions.csv")


result_item <- function(id = 0, 
                        name = "",
                        tty_name = "",
                        tty_id = ""
                        ){
  paste0("
   <table style=\"width:100%\">
     <tr>
       <td style='FONT-SIZE:24px; COLOR:#000000; LINE-HEIGHT:24px; '>
           <a href=.?_inputs_&search=", id,">", name ,"</a>
       </td>
     </tr>
     <tr>
       <td style='FONT-SIZE:16px; COLOR:#000000; LINE-HEIGHT:30px;padding: 0px 0px 20px 0px;'>
        <font color='green'>Term Type: <b>", tty_name,"</b></font>
      </td>
     </tr>
  </table>")
} 


result_stats <- function(query_time  = "",
                         total_results = ""){
  paste0("
         <table style=\"width:50%\">
           <tr>
             <td style='FONT-SIZE:12px; COLOR:#000000; ;padding: 0px 0px 20px 0px; '>
                Query time: ", round(query_time, 2) ,"
             </td>
             <td style='FONT-SIZE:12px; COLOR:#000000; ;padding: 0px 0px 20px 0px; '>
                Total results: ", total_results ,"
             </td>
           </tr>
         </table>")
} 

ui <- function(request) {
  fluidPage(
    titlePanel("Clinical Drug Information"),
    fluidRow(htmltools::br()),
    fluidRow(column(11,textInput("search", label = "Search Rx", value = ""))),
    fluidRow(column(11,htmlOutput("results"))),
    fluidRow(
      column(7, visNetworkOutput("network")),
      column(5, dataTableOutput("administered"))
    ),
    fluidRow(
      column(4, dataTableOutput("medications")),
      column(8, dataTableOutput("ingredients"))
    )
  )
}


  
server <- function(input, output, session) {
  
  con <- DBI::dbConnect(
    bigquery(),
    project = "bigquery-public-data",
    dataset = "nlm_rxnorm",
    billing = "bigrquery-demo",
    use_legacy_sql = FALSE
  )
  
  rx_pathways <- tbl(con, "rxn_all_pathways_current") 
  
  
  output$results <- renderText({
    req(input$search)
    if(nchar(input$search) > 3 && is.na(as.integer(req(input$search)))){
      query_time <-  system.time({
        df <- rx_pathways %>%
          mutate(search_name = str_to_lower(SOURCE_NAME)) %>%
          filter(
            search_name %like% !!paste0("%", input$search,"%"),
            SOURCE_NAME == TARGET_NAME,
            TARGET_TTY %in% c("BN", "DF", "IN")
          ) %>%
          select(SOURCE_RXCUI, SOURCE_NAME, SOURCE_TTY) %>%
          collect() %>%
          inner_join(definitions, by = c("SOURCE_TTY" = "TTY"))
      })
      
      results <- df %>%
        transpose() %>% 
        map_chr(~result_item(.x$SOURCE_RXCUI, .x$SOURCE_NAME, .x$Name, .x$SOURCE_TTY))
      
      results <- c(
        "<br/>",
        result_stats(query_time[3], 
        nrow(df)), results)
      
    } else {
      updateTabsetPanel(session, "results")
      NULL
    }
  
  })
  
  item_details <- reactive({
    if(req(as.numeric(input$search))){
      rx_pathways %>%
        filter(TARGET_RXCUI == input$search | SOURCE_RXCUI == input$search) %>%
        filter(TARGET_TTY == "BN" | SOURCE_TTY == "BN") %>%
        mutate(
          bn_id  = ifelse(SOURCE_TTY == "BN", SOURCE_RXCUI, TARGET_RXCUI),
          bn_name = ifelse(SOURCE_TTY == "BN", SOURCE_NAME, TARGET_NAME)
        ) %>%
        select(bn_id, bn_name) %>%
        inner_join(rx_pathways, by = c("bn_id" = "SOURCE_RXCUI")) %>%
        collect()
    }
  })

  
  output$medications <- renderDataTable({
    if(req(as.numeric(input$search))){
      item_details()  %>%
        filter(TARGET_TTY == "BN") %>%
        group_by(TARGET_TTY, TARGET_NAME) %>%
        summarise() %>%
        ungroup() %>%
        select(Medications = TARGET_NAME) %>%
        datatable(
          options = list(
            searching = FALSE,
            pagelength = 10,
            target = "cell"
          ),
          rownames = FALSE
        )
    } else {
      NULL
    }
  })
  
  
  
  output$administered <- renderDataTable({
    if(req(as.numeric(input$search))){
      item_details()  %>%
        filter(TARGET_TTY == "DF") %>%
        group_by(TARGET_RXCUI, TARGET_NAME, SOURCE_NAME ) %>%
        tally() %>% 
        ungroup() %>%
        select(-n) %>%
        group_by(TARGET_RXCUI, TARGET_NAME) %>%
        tally() %>%
        ungroup() %>%
        select(
          Code = TARGET_RXCUI,
          Administered = TARGET_NAME,
          Medications = n
        ) %>%
        datatable(
          options = list(
            searching = FALSE,
            pageLength = 10,
            target = "cell"
          ),
          rownames = FALSE
        )
    } else {
      NULL
    }
  })
  
  observeEvent(input$administered_cell_clicked, {
    cell <- input$administered_cell_clicked
    if(as.numeric(req(cell$value)) > 0) {
      updateTextInput(session, "search", value = cell$value)
    }
  })
  
  output$ingredients <- renderDataTable({
    if(req(as.numeric(input$search))){
      item_details()  %>%
        filter(TARGET_TTY == "IN") %>%
        group_by(TARGET_RXCUI, TARGET_NAME, SOURCE_NAME ) %>%
        tally() %>% 
        ungroup() %>%
        select(-n) %>%
        group_by(TARGET_RXCUI, TARGET_NAME) %>%
        tally() %>%
        ungroup() %>%
        select(
          Code = TARGET_RXCUI,
          Ingredients = TARGET_NAME,
          Medications = n
        ) %>%
        datatable(
          options = list(
            searching = FALSE,
            pageLength = 10
          ),
          rownames = FALSE
        )
    } else {
      NULL
    }
  })
  
  observeEvent(input$ingredients_cell_clicked, {
    cell <- input$ingredients_cell_clicked
    if(as.numeric(req(cell$value)) > 0) {
      updateTextInput(session, "search", value = cell$value)
    }
  })
  
  output$network <- renderVisNetwork({
    
    if(req(as.numeric(input$search))){
      result_item1 <- item_details() %>%
        filter(TARGET_TTY %in% c("BN", "DF", "IN"))
      
      nodes <- bind_rows(
        select(result_item1, id = TARGET_RXCUI, title = TARGET_NAME, group = TARGET_TTY),
        select(result_item1, id = bn_id, title = SOURCE_NAME, group = SOURCE_TTY)) %>%
        group_by(id, title, group) %>%
        summarise() %>%
        ungroup() %>%
        mutate(label = "")
      
      edges <- result_item1 %>%
        select(from = bn_id, to = TARGET_RXCUI) %>%
        filter(from != to) %>%
        group_by(from, to) %>%
        summarise()
      
      visNetwork(nodes, edges, width = "100%") %>%
        visGroups(groupname = "BN",  color = "#999999", shape = "triangle") %>%
        visGroups(groupname = "IN",  color = "#E69F00", shape = "dot" ) %>%
        visGroups(groupname = "DF",  color = "#F0E442", shape = "diamond" ) %>%
        visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE)) %>%
        visLayout(randomSeed = 123) %>% 
        visPhysics(stabilization = FALSE) %>%
        visEdges(smooth = FALSE)
    } else {
      NULL
    }
  })
  
}

shinyApp(ui, server, enableBookmarking = "url")