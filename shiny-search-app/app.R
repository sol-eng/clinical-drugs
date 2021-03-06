pacman::p_load(
  shiny, dplyr, purrr, readr, stringr, htmltools, bigrquery, visNetwork, 
  DT, shinymaterial
  )

ui <- function(request) {
  material_page(
    nav_bar_color = "orange",
    background_color = "white", 
    title = "Clinical Drug Information",
    nav_bar_fixed = TRUE,
    fluidPage(
      fluidRow(htmltools::br()),
      fluidRow(
        column(
          11,
          material_card(
            title = "Search Rx",
            textInput("search", label = "", value = ""),
            depth = 4,
            shiny::tags$p("Terms to try: 'aspirin', 'albuterol', 'inhaler'")
          )
        )
      ),
      fluidRow(column(11, htmlOutput("results"))),
      fluidRow(column(11, htmlOutput("title"))),
      fluidRow(
        column(7, visNetworkOutput("network")),
        column(5, dataTableOutput("administered"))
      ),
      fluidRow(
        column(6, dataTableOutput("medications")),
        column(6, dataTableOutput("ingredients"))
      )
    )
  )
}

definitions <- read_csv("definitions.csv")

result_item <- function(id = 0,
                        name = "",
                        tty_name = "",
                        tty_id = "") {
  paste0("
   <table style=\"width:100%\">
     <tr>
       <td style='FONT-SIZE:24px; COLOR:#000000; LINE-HEIGHT:24px; '>
           <a href=.?_inputs_&search=", id, ">", name, "</a>
       </td>
     </tr>
     <tr>
       <td style='FONT-SIZE:16px; COLOR:#000000; LINE-HEIGHT:30px;padding: 0px 0px 20px 0px;'>
        <font color='green'>Term Type: <b>", tty_name, "</b></font>
      </td>
     </tr>
  </table>")
}


result_stats <- function(query_time = "",
                         total_results = "") {
  paste0("
         <table style=\"width:50%\">
           <tr>
             <td style='FONT-SIZE:12px; COLOR:#000000; ;padding: 0px 0px 20px 0px; '>
                Query time: ", round(query_time, 2), "
             </td>
             <td style='FONT-SIZE:12px; COLOR:#000000; ;padding: 0px 0px 20px 0px; '>
                Total results: ", total_results, "
             </td>
           </tr>
         </table>")
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
    if (nchar(input$search) > 3 && is.na(as.integer(req(input$search)))) {
      query_time <- system.time({
        df <- rx_pathways %>%
          mutate(search_name = str_to_lower(SOURCE_NAME)) %>%
          filter(
            search_name %like% !!paste0("%", input$search, "%"),
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
        result_stats(
          query_time[3],
          nrow(df)
        ), results
      )
    } else {
      updateTabsetPanel(session, "results")
      NULL
    }
  })

  item_details <- reactive({
    if (req(as.numeric(input$search))) {
      df <- rx_pathways %>%
        filter(TARGET_RXCUI == input$search | SOURCE_RXCUI == input$search) %>%
        filter(TARGET_TTY == "BN" | SOURCE_TTY == "BN") %>%
        mutate(
          bn_id = ifelse(SOURCE_TTY == "BN", SOURCE_RXCUI, TARGET_RXCUI),
          bn_name = ifelse(SOURCE_TTY == "BN", SOURCE_NAME, TARGET_NAME)
        ) %>%
        select(bn_id, bn_name) %>%
        inner_join(rx_pathways, by = c("bn_id" = "SOURCE_RXCUI")) %>%
        group_by_all() %>%
        summarise() %>%
        collect() 
      
      df %>%
        ungroup()%>%
        left_join(definitions, by = c("SOURCE_TTY" = "TTY" )) %>%
        rename(
          source_name = SOURCE_NAME,
          source_tty  = SOURCE_TTY,
          target_id = TARGET_RXCUI,
          target_name = TARGET_NAME,
          target_tty = TARGET_TTY,
          target_category = Name
        ) %>%
        select(
          - Example,
          - Description
        )
      
    }
  })
  
  output$title <- renderText({
    if (req(as.numeric(input$search))) {
    
      item <- item_details() %>% 
        filter(target_id == input$search) %>% 
        select(target_name, target_category) %>% 
        head(1) %>% 
        as.character()
      
      paste0("<h2>" , item[1], " (", item[2], ")", "</h2>")
    }
    
  })

  output$medications <- renderDataTable({
    if (req(as.numeric(input$search))) {
      item_details() %>%
        filter(target_tty == "BN") %>%
        group_by(target_tty, target_name) %>%
        summarise() %>%
        ungroup() %>%
        select(Medications = target_name) %>%
        datatable(
          options = list(
            searching = FALSE,
            pagelength = 10,
            bLengthChange = FALSE,
            bPaginate = FALSE,
            target = "row"
          ),
          rownames = FALSE
        )
    } else {
      NULL
    }
  })

  output$administered <- renderDataTable({
    if (req(as.numeric(input$search))) {
      item_details() %>%
        filter(target_tty == "DF") %>%
        group_by(target_id, target_name, source_name) %>%
        tally() %>%
        ungroup() %>%
        select(-n) %>%
        group_by(target_id, target_name) %>%
        tally() %>%
        ungroup() %>%
        select(
          Code = target_id,
          Administered = target_name,
          Medications = n
        ) %>%
        datatable(
          options = list(
            searching = FALSE,
            pageLength = 10,
            bLengthChange = FALSE,
            bPaginate = FALSE,
            target = "row"
          ),
          rownames = FALSE
        )
    } else {
      NULL
    }})

  observeEvent(input$administered_cell_clicked, {
    cell <- input$administered_cell_clicked
    if (req(cell$col==0)) {
      updateTextInput(session, "search", value = cell$value)
    }
  })

  output$ingredients <- renderDataTable({
    if (req(as.numeric(input$search))) {
      item_details() %>%
        filter(target_tty == "IN") %>% 
        group_by(target_id, target_name) %>%
        tally() %>%
        select(
          Code = target_id,
          Ingredients = target_name,
          Medications = n
        ) %>%
        datatable(
          options = list(
            searching = FALSE,
            pageLength = 10,
            bLengthChange = FALSE,
            bPaginate = FALSE,
            target = "row"
          ),
          rownames = FALSE
        )
    } else {
      NULL
    }
  })

  observeEvent(input$ingredients_cell_clicked, {
    cell <- input$ingredients_cell_clicked
    if (req(cell$col==0)) {
      updateTextInput(session, "search", value = cell$value)
    }
  })

  output$network <- renderVisNetwork({
    if (req(as.numeric(input$search))) {
      result_item1 <- item_details() %>%
        filter(target_tty %in% c("BN", "DF", "IN"))

      nodes <- bind_rows(
        select(result_item1, id = target_id, title = target_name, group = target_tty),
        select(result_item1, id = bn_id, title = source_name, group = source_tty)
        ) %>%
        group_by(id, title, group) %>%
        summarise() %>%
        ungroup() %>%
        mutate(label = "")

      edges <- result_item1 %>%
        select(
          from = bn_id, 
          to = target_id
          ) %>%
        filter(from != to) %>%
        group_by(from, to) %>%
        summarise()

      visNetwork(nodes, edges, width = "100%") %>%
        visGroups(groupname = "BN", color = "#999999", shape = "triangle") %>%
        visGroups(groupname = "IN", color = "#E69F00", shape = "dot") %>%
        visGroups(groupname = "DF", color = "#F0E442", shape = "diamond") %>%
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
