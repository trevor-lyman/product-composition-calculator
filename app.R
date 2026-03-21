# Trevor Pettit
# Assembly-Based Product Composition Calculator 
# 20 March 2026

# Load Libraries ####
library(shiny)
library(ggplot2)
library(dplyr)
library(forcats)
library(ggrepel)
library(assertthat)
library(readxl)
library(tidyr)
library(shinyjs)

set.seed(67)

AssysProductA <- c("Mod Assembly", "Assembly 1A", "Assembly 1B", 
                   "Assembly 1C", "Assembly 2A", "Assembly 2B", 
                   "Assembly 2C", "Assembly 3A", "Assembly 3B", 
                   "Assembly 3C", "Assembly 4", "Assembly 5")

AssysProductB <- c("Mod Assembly", "Assembly 1A", "Assembly 1B", 
                   "Assembly 1C", "Assembly 2A", "Assembly 2B", 
                   "Assembly 2C", "Assembly 3A", "Assembly 3B", 
                   "Assembly 3C", "Assembly 4", "Assembly 5")

AssysProductC <- c("Mod Assembly", "Assembly 1A", "Assembly 1B", 
                   "Assembly 1C", "Assembly 2A", "Assembly 2B", 
                   "Assembly 2C", "Assembly 3A", "Assembly 3B", 
                   "Assembly 3C", "Assembly 4", "Assembly 5")

AssysProductD <- c("Mod Assembly", "Assembly 1A", "Assembly 1B", 
                   "Assembly 1C", "Assembly 2A", "Assembly 2B", 
                   "Assembly 2C", "Assembly 4", "Assembly 6",
                   "Assembly 8")

AssysProductE <- c("Mod Assembly", "Assembly 2A", "Assembly 2B", 
                   "Assembly 2C", "Assembly 3A", "Assembly 3B", 
                   "Assembly 3C", "Assembly 5", "Assembly 6", 
                   "Assembly 7")

AssysProductF <- c("Mod Assembly", "Assembly 2A", "Assembly 2B", 
                   "Assembly 2C", "Assembly 3A", "Assembly 3B", 
                   "Assembly 3C", "Assembly 5", "Assembly 6", 
                   "Assembly 7")

AssysProductG <- c("Mod Assembly", "Assembly 1A", "Assembly 1B", 
                   "Assembly 1C", "Assembly 2A", "Assembly 2B", 
                   "Assembly 2C", "Assembly 3A", "Assembly 3B", 
                   "Assembly 3C", "Assembly 4", "Assembly 5")

assy_data <- 
  data.frame(Sort = c(seq_along(AssysProductA), seq_along(AssysProductB),
                      seq_along(AssysProductC), seq_along(AssysProductD),
                      seq_along(AssysProductE), seq_along(AssysProductF),
                      seq_along(AssysProductG))) %>%
  mutate(Product = c(rep("Product A", length(AssysProductA)),
                     rep("Product B", length(AssysProductB)),
                     rep("Product C", length(AssysProductC)),
                     rep("Product D", length(AssysProductD)),
                     rep("Product E", length(AssysProductE)),
                     rep("Product F", length(AssysProductF)),
                     rep("Product G", length(AssysProductG))
  )) %>%
  mutate(Part = c(AssysProductA, AssysProductB, AssysProductC,
                  AssysProductD, AssysProductE, AssysProductF,
                  AssysProductG
  )) %>%
  mutate(Part.Wt = abs(rnorm(n = length(Product), mean = 20, sd = 20))) %>%
  rowwise() %>%
  mutate(
    total = sample(60:95, 1),
    cuts = list(sort(sample(1:(total - 1), 2))),
    PctMaterial1 = cuts[1],
    PctMaterial2 = cuts[2] - cuts[1],
    PctMaterial3 = total - cuts[2],
    PctOther = 100 - total
  ) %>%
  select(-cuts, -total) %>%
  ungroup() 

last_updated <- file.info("app.R")$mtime

# Build UI ####
# Title banner UI
title_banner <- function(title_text = "Assembly-Based Product Composition Calculator",
                         subtitle_text = "Gibby's Sustainability Solutions",
                         banner_color = "#2C3E50",
                         logo_path = "logo.png") {
  
  tags$div(
    style = paste0("display: flex; align-items: center; 
                   justify-content: space-between;",
                   "background-color:", banner_color, "; 
                   color: white; padding: 15px 20px;"),
    
    # Logo
    tags$img(src = logo_path, height = "200px"),
    
    # Title + subtitle centered
    tags$div(
      style = "text-align: left; flex-grow: 1;",
      tags$h1(title_text, style = "margin: 20px; margin-bottom: 0px; 
              font-size: 50px;"),
      tags$h4(subtitle_text, style = "margin: 20px; margin-top: 5px; 
      margin-bottom: 20px; font-size: 44px; 
              font-weight: normal; font-style: italic;")
    ),
    
    # Right spacer to keep title centered
    tags$div(style = "width: 50px;")
  )
}

ui <- fluidPage(
  # Banner at top
  title_banner(
    banner_color = "#FF46A2",      
    logo_path = "logo.png"          
  ),
  br(),
  # Disclaimer panel
  wellPanel(
    style = "background-color: #f8f9fa; border-left: 5px solid #FF46A2; 
           padding: 10px; padding-bottom: 10px; margin-top: 20px;",
    tags$p(
      tags$strong("Note: "), 
      "This is a sanitized demo version built using simulated data and generalized product structures.",
      style = "word-wrap: break-word; word-break: break-word; margin: 0;"
    )
  ),
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      #sidebarPanel {
        background-color: #FF46A2; 
        color: white;
        padding: 1rem;
        border-radius: 0.3rem;
      }
      .form-group {
        color: white;
      }
      .control-label {
        color: white;
      }
      /* Style the dropdown ribbon */
    .selectize-input {
      background-color: white !important;
      color: black !important;
      border: 1px solid #FF46A2;
    }

    /* Style dropdown options */
    .selectize-dropdown-content {
      background-color: white !important;
      color: black !important;
    }

    /* Highlight selected item */
    .selectize-dropdown .active {
      background-color: #FF9FCF !important;
      color: white !important;
    }
    
    /* Style for the selected item in the dropdown */
    .selectize-dropdown .selected {
      background-color: #FF46A2 !important;
      color: white !important;
    }

    "))
  ),
  
  ## Universal Inputs ####
  sidebarLayout(
    sidebarPanel(
      tags$h2("Assembly Configuration",
              style = "color: white; font-weight: bold; text-align: center; 
                  margin-top: 5px; margin-bottom: 18px; font-size: 24px"),
      tags$script(HTML("Shiny.addCustomMessageHandler('weightEdited', 
      function(message) {
      Shiny.setInputValue('weightWasEdited', true, {priority: 'event'});
      });
      $(document).on('focus', '#GlobalWeight', function() {
      Shiny.setInputValue('weightWasEdited', true, {priority: 'event'});
      });
                       ")),
      tags$meta(name = "viewport", 
                content = "width=device-width, initial-scale=1"),
      id = "sidebarPanel",
      selectInput("Product", "Product", choices = c(
        "Product A", "Product B", "Product C", "Product D", "Product E", 
        "Product F", "Product G") %>% sort()
      ), # End of Product dropdown selectInput block
      div(style = "margin-bottom: 1px;", 
          numericInput("GlobalWeight", "Total Product Weight (Kg)",
                       value = 0, min = 0)),
      
      ### Inputs for Product A, B, C, G ####
      conditionalPanel(
        condition = "input.Product == 'Product A' || 
        input.Product == 'Product B' || 
        input.Product == 'Product C' || input.Product == 'Product G'",
        numericInput("ModsHigh", "Modular Units High", value = 1, min = 1,
                     max = 100),
        numericInput("ModsWide", "Modular Units Wide", value = 1, min = 1,
                     max = 100),
        selectInput("Assy1", "Assembly 1 Selection", 
                    choices = c("--", "Assembly 1A", "Assembly 1B", 
                                "Assembly 1C")),
        selectInput("Assy2", "Assembly 2 Selection", 
                    choices = c("--", "Assembly 2A", "Assembly 2B", 
                                "Assembly 2C")),
        selectInput("Assy3", "Assembly 3 Selection", 
                    choices = c("--", "Assembly 3A", "Assembly 3B", 
                                "Assembly 3C")),
        selectInput("Market", "Market Selection", 
                    choices = c("--", "Market 1", "Market 2"))
      ), 
      
      ### Inputs for Product D ####
      conditionalPanel(
        condition = "input.Product == 'Product D'",
        numericInput("ModsHigh", "Modular Units High", value = 1, min = 1),
        numericInput("ModsWide", "Modular Units Wide", value = 1, min = 1),
        selectInput("Assy1", "Assembly 1 Selection", 
                    choices = c("--", "Assembly 1A", "Assembly 1B", 
                                "Assembly 1C")),
        selectInput("Assy2", "Assembly 2 Selection", 
                    choices = c("--", "Assembly 2A", "Assembly 2B", 
                                "Assembly 2C")),
        selectInput("Market", "Market Selection", 
                    choices = c("--", "Market 1", "Market 2"))
      ), 
      
      ### Inputs for Product E, F ####
      conditionalPanel(
        condition = "input.Product == 'Product E' || 
        input.Product == 'Product F'",
        numericInput("ModsHigh", "Modular Units High", value = 1, min = 1),
        numericInput("ModsWide", "Modular Units Wide", value = 1, min = 1),
        selectInput("Assy2", "Assembly 2 Selection", 
                    choices = c("--", "Assembly 2A", "Assembly 2B", 
                                "Assembly 2C")),
        selectInput("Assy3", "Assembly 3 Selection", 
                    choices = c("--", "Assembly 3A", "Assembly 3B", 
                                "Assembly 3C")),
        selectInput("Market", "Market Selection5", 
                    choices = c("--", "Market 1", "Market 2"))
      ), 
      
      ### Download Buttons ####
      div(style = "display: flex; flex-wrap: wrap; gap: 10px; 
          margin-top: 25px",
          downloadButton("downloadData", "Download Final Data",
                         style = "width: 100%;"),
          downloadButton("downloadPlot", "Download Pie Chart",
                         style = "width: 100%;")
      ),
      ### Reset Button ####
      div(style = "margin-top: 10px;",
          actionButton("resetBtn", "Reset Calculator",
                       style = "width: 100%; background-color:#FFCCE6; 
                          color:grey; font-weight:bold;")
      ),
      tags$p(
        paste("Last updated:", format(last_updated, "%B %d, %Y")),
        style = "color: white; font-size: 0.9em; text-align: center;
            margin-top: 25px; margin-bottom: 5px;"
      )
    ),
    
    
    ## Main Panel for Outputs ####
    mainPanel(
      div(style = "overflow-x: auto; width: 100%; text-align: center;",
          div(style = "min-width: 800px; display: inline-block;",
              plotOutput("pieChart", width = "100%", height = "40vh")
          )
      ),
      div(style = "overflow-x: auto; width: 100%;",
          tableOutput("finalTable")
      )
    )
  ), 
  br(), br(),
  
  # Footer panel outside sidebarLayout
  div(
    style = paste0(
      "background-color: #f8f9fa;",     # light grey
      "border-top: 3px solid #FF46A2;", # thin line using banner color
      "text-align: right;",
      "padding: 30px 50px 50px 50px;",
      "font-size: 14px;",
      "color: #555;"
    ),
    "Trevor Pettit, 2026",
    tags$br(),tags$br(),
    tags$a("About This Project", 
           href = "https://github.com/trevor-lyman/product-composition-calculator", 
           target = "_blank", style = "margin-right: 8px;"),
    tags$span("|", style = "margin: 0 8px; color: #555;"),
    tags$a("GitHub", 
           href = "https://github.com/trevor-lyman", target = "_blank", 
           style = "margin-right: 8px;"),
    tags$span("|", style = "margin: 0 8px; color: #555;"),
    tags$a("LinkedIn", 
           href = "https://www.linkedin.com/in/trevor-pettit-2b115a161/", 
           target = "_blank")
  )
) 

# Build Server Logic ####
server <- function(input, output, session) {
  
  ## Universal Logic ####
  ## Define flags for default vs. user edited weight ####
  defaultWeight <- reactiveVal(0)
  userEditedWeight <- reactiveVal(FALSE)
  
  ## Build Custom Ceiling Function ####
  ceiling_custom <- function(x, significance) {
    ceiling(x / significance) * significance
  }
  # same logic as ceiling() in Excel
  
  ## Branch A Logic ####
  # Product A, Product B, Product C, Product G
  runBranchA <- function(req_df, assy_data) {
    req(input$GlobalWeight)
    # Extract values from req_df
    req <- req_df
    
    ### Stop reactivity loop: freeze RNG per input state ####
    seed_key <- paste(req$Product, req$ModsHigh, req$ModsWide,
                      req$Assy1, req$Assy2, req$Assy3, req$Market, sep = "|")
    seed_val <- sum(utf8ToInt(seed_key)) %% .Machine$integer.max
    set.seed(seed_val)
    
    ### Quantity Logic ####
    QtyMods <- req$ModsHigh * req$ModsWide
    
    QtyAssembly1A <- 
      if (req$Assy1 == "Assembly 1A" && req$Product == "Product A") {
        ceiling_custom(QtyMods * (runif(n = 1, min = 1, max = 100)/100), 1)
      } else if (req$Assy1 == "Assembly 1A" && req$Product == "Product B") {
        ceiling_custom(QtyMods * (runif(n = 1, min = 1, max = 100)/100), 1)
      } else if (req$Assy1 == "Assembly 1A") {
        ceiling_custom(QtyMods * (runif(n = 1, min = 1, max = 100)/100), 1)
      } else {
        0
      }
    
    QtyAssembly1B <- 
      if (req$Assy1 == "Assembly 1B" && req$Product == "Product A") {
        ceiling_custom(QtyMods * (runif(n = 1, min = 1, max = 100)/100), 1)
      } else if (req$Assy1 == "Assembly 1B" && req$Product == "Product B") {
        ceiling_custom(QtyMods * (runif(n = 1, min = 1, max = 100)/100), 1)
      } else if (req$Assy1 == "Assembly 1B") {
        ceiling_custom(QtyMods * (runif(n = 1, min = 1, max = 100)/100), 1)
      } else {
        0
      }
    
    QtyAssembly1C <- 
      if (req$Assy1 == "Assembly 1C" && req$Product == "Product A") {
        ceiling_custom(QtyMods * (runif(n = 1, min = 1, max = 100)/100), 1)
      } else if (req$Assy1 == "Assembly 1C" && req$Product == "Product B") {
        ceiling_custom(QtyMods * (runif(n = 1, min = 1, max = 100)/100), 1)
      } else if (req$Assy1 == "Assembly 1C") {
        ceiling_custom(QtyMods * (runif(n = 1, min = 1, max = 100)/100), 1)
      } else {
        0
      }
    
    QtyAssembly2A <- 
      if (req$Assy2 == "Assembly 2A" && req$Market == "Market 1") {
        ceiling_custom(QtyMods * (runif(n = 1, min = 1, max = 100)/100), 1)
      } else if (req$Assy2 == "Assembly 2A") {
        ceiling_custom(QtyMods * (runif(n = 1, min = 1, max = 100)/100), 1)
      } else {
        0
      }
    
    QtyAssembly2B <- 
      if (req$Assy2 == "Assembly 2B" && req$Market == "Market 1") {
        ceiling_custom(QtyMods * (runif(n = 1, min = 1, max = 100)/100), 1)
      } else if (req$Assy2 == "Assembly 2B") {
        ceiling_custom(QtyMods * (runif(n = 1, min = 1, max = 100)/100), 1)
      } else {
        0
      }
    
    QtyAssembly2C <- 
      if (req$Assy2 == "Assembly 2C" && req$Market == "Market 1") {
        ceiling_custom(QtyMods * (runif(n = 1, min = 1, max = 100)/100), 1)
      } else if (req$Assy2 == "Assembly 2C") {
        ceiling_custom(QtyMods * (runif(n = 1, min = 1, max = 100)/100), 1)
      } else {
        0
      }
    
    QtyAssembly3A <- 
      if (req$Assy3 == "Assembly 3A") {
        ceiling_custom(QtyMods * (runif(n = 1, min = 1, max = 100)/100), 1)
      } else {
        0
      }
    
    QtyAssembly3B <- 
      if (req$Assy3 == "Assembly 3B") {
        ceiling_custom(QtyMods * (runif(n = 1, min = 1, max = 100)/100), 1)
      } else {
        0
      }
    
    QtyAssembly3C <- 
      if (req$Assy3 == "Assembly 3C") {
        ceiling_custom(QtyMods * (runif(n = 1, min = 1, max = 100)/100), 1)
      } else {
        0
      }
    
    QtyAssembly4 <- ceiling_custom(
      req$ModsHigh*(runif(n = 1, min = 1, max = 100)/100), 50)
    
    QtyAssembly5 <- if (req$Assy3 == "Assembly 3A") {
      ceiling_custom(QtyMods * (runif(n = 1, min = 1, max = 100)/100), 1)
    } else if (req$Assy3 == "Assembly 3B") {
      ceiling_custom(QtyMods * (runif(n = 1, min = 1, max = 100)/100), 1)
    } else {
      ceiling_custom(QtyMods * (runif(n = 1, min = 1, max = 100)/100), 1)
    }

    ### Aggregate Qty Data #### 
    calc <- as.data.frame(matrix(c(
      "Mod Assembly", QtyMods,
      "Assembly 1A", QtyAssembly1A, 
      "Assembly 1B", QtyAssembly1B,
      "Assembly 1C", QtyAssembly1C, 
      "Assembly 2A", QtyAssembly2A, 
      "Assembly 2B", QtyAssembly2B,
      "Assembly 2C", QtyAssembly2C, 
      "Assembly 3A", QtyAssembly3A,
      "Assembly 3B", QtyAssembly3B,
      "Assembly 3C", QtyAssembly3C,
      "Assembly 4", QtyAssembly4, 
      "Assembly 5", QtyAssembly5 
    ), ncol = 2, byrow = TRUE))
    
    colnames(calc) <- c("Part", "Qty")
    
    calc$Qty <- as.numeric(calc$Qty)
    
    ### Merge Calculated Qtys with Metadata #### 
    temp0 <- assy_data %>% filter(Product == req$Product) 
    
    data <- temp0 %>%
      left_join(calc, by = "Part") %>%
      arrange(Sort)
    
    data$Qty[is.na(data$Qty)] <- 0
    
    data <- data %>%
      mutate(Sort = ifelse(is.na(Sort), row_number(), Sort))
    
    data$Product[is.na(data$Product)] <- req$Product
    
    ### Calculate Total Wt From Qty & Part Wt ####
    data$Qty <- as.numeric(data$Qty)
    
    data <- data %>%
      mutate(Total.Wt = Part.Wt * Qty)
    
    ### Composition Calculations####
    comp <- data %>%
      mutate(across(starts_with("Pct"), ~ (.x/100) * Total.Wt, 
                    .names = "Kg.{substr(.col, 4, nchar(.col))}")) %>%
      select(Sort, Product, Part, Part.Wt, Qty, Total.Wt, 
             starts_with("Kg.")) %>%
      mutate(Kg.Unevaluated = 0)
    
    total_known_weight <- sum(comp$Total.Wt, na.rm = TRUE)
    
    defaultWeight(total_known_weight)
    
    # Only auto-update if user hasn't manually edited the weight
    # Only auto-update if we have a meaningful known total (prevents forcing 0 kg)
    if (!userEditedWeight()) {
      if (is.finite(total_known_weight) && total_known_weight > 0) {
        proposed <- ceiling_custom(total_known_weight, 10)
        if (!identical(proposed, input$GlobalWeight)) {
          updateNumericInput(session, "GlobalWeight", value = proposed)
        }
      }
    }
    other_weight <- max(
      as.numeric(input$GlobalWeight) - as.numeric(total_known_weight),
      0, na.rm = TRUE
    )
    
    comp2 <- comp %>%
      bind_rows(data.frame(
        Product = req$Product,
        Part = "Unevaluated Material",
        Total.Wt = other_weight,
        Kg.Material1 = 0,
        Kg.Material2 = 0,
        Kg.Material3 = 0,
        Kg.Other = 0,
        Kg.Unevaluated = other_weight
      ))
    
    tot <- comp2 %>%
      add_row(Part = "Total", 
              Product = req$Product,
              Total.Wt = sum(comp2$Total.Wt),
              Kg.Material1 = sum(comp2$Kg.Material1), 
              Kg.Material2 = sum(comp2$Kg.Material2), 
              Kg.Material3 = sum(comp2$Kg.Material3),
              Kg.Other = sum(comp2$Kg.Other),
              Kg.Unevaluated = sum(comp2$Kg.Unevaluated))
    
    final <- tot %>%
      add_row(Part = "Percent", 
              Product = req$Product,
              Kg.Material1 = tot$Kg.Material1[tot$Part == "Total"]/
                tot$Total.Wt[tot$Part == "Total"] * 100,
              Kg.Material2 = tot$Kg.Material2[tot$Part == "Total"]/
                tot$Total.Wt[tot$Part == "Total"] * 100,
              Kg.Material3 = tot$Kg.Material3[tot$Part == "Total"]/
                tot$Total.Wt[tot$Part == "Total"] * 100,
              Kg.Other = 
                tot$Kg.Other[tot$Part == "Total"]/
                tot$Total.Wt[tot$Part == "Total"] * 100,
              Kg.Unevaluated = 
                tot$Kg.Unevaluated[tot$Part == "Total"]/
                tot$Total.Wt[tot$Part == "Total"] * 100) %>%
      select(-Sort) %>%
      filter(Part != "Percent")
    
    totals <- comp2 %>%
      summarise(
        Total = sum(Total.Wt),
        Pct.Material1 = sum(Kg.Material1) / Total * 100,
        Pct.Material2 = sum(Kg.Material2) / Total * 100,
        Pct.Material3 = sum(Kg.Material3) / Total * 100,
        Pct.Other = sum(Kg.Other) / Total * 100,
        Pct.Unevaluated = sum(Kg.Unevaluated) / Total * 100
      )
    
    pie.data <- totals %>%
      pivot_longer(cols = starts_with("Pct."), names_to = "Material", 
                   values_to = "PctComposition") %>%
      mutate(Material = gsub("Pct\\.", "", Material)) %>%
      select(Material, PctComposition)
    
    return(list(final = final, pie.data = pie.data))
  }
  
  ## Branch B Logic ####
  # Note: Branch B Logic Applies to Product D
  runBranchB <- function(req_df2, assy_data) {
    req(input$GlobalWeight)
    
    ### Extract values from req_df
    req <- req_df2
    
    ### Stop reactivity loop: freeze RNG per input state ####
    seed_key <- paste(req$Product, req$ModsHigh, req$ModsWide,
                      req$Assy1, req$Assy2, req$Market, sep = "|")
    seed_val <- sum(utf8ToInt(seed_key)) %% .Machine$integer.max
    set.seed(seed_val)
    
    ### Quantity Logic ####
    QtyMods <- req$ModsHigh * req$ModsWide
    
    QtyAssembly1A <- 
      if (req$Assy1 == "Assembly 1A") {
        ceiling_custom(QtyMods * (runif(n = 1, min = 1, max = 100)/100), 1)
      } else {
        0
      }
    
    QtyAssembly1B <- 
      if (req$Assy1 == "Assembly 1B") {
        ceiling_custom(QtyMods * (runif(n = 1, min = 1, max = 100)/100), 1)
      } else {
        0
      }
    
    QtyAssembly1C <- 
      if (req$Assy1 == "Assembly 1C") {
        ceiling_custom(QtyMods * (runif(n = 1, min = 1, max = 100)/100), 1)
      } else {
        0
      }
    
    QtyAssembly2A <- 
      if (req$Assy2 == "Assembly 2A" && req$Market == "Market 1") {
        ceiling_custom(QtyMods * (runif(n = 1, min = 1, max = 100)/100), 1)
      } else if (req$Assy2 == "Assembly 2A") {
        ceiling_custom(QtyMods * (runif(n = 1, min = 1, max = 100)/100), 1)
      } else {
        0
      }
    
    QtyAssembly2B <- 
      if (req$Assy2 == "Assembly 2B" && req$Market == "Market 1") {
        ceiling_custom(QtyMods * (runif(n = 1, min = 1, max = 100)/100), 1)
      } else if (req$Assy2 == "Assembly 2B") {
        ceiling_custom(QtyMods * (runif(n = 1, min = 1, max = 100)/100), 1)
      } else {
        0
      }
    
    QtyAssembly2C <- 
      if (req$Assy2 == "Assembly 2C" && req$Market == "Market 1") {
        ceiling_custom(QtyMods * (runif(n = 1, min = 1, max = 100)/100), 1)
      } else if (req$Assy2 == "Assembly 2C") {
        ceiling_custom(QtyMods * (runif(n = 1, min = 1, max = 100)/100), 1)
      } else {
        0
      }
    
    QtyAssembly4 <- ceiling_custom(
      req$ModsHigh*(runif(n = 1, min = 1, max = 100)/100), 50)

    QtyAssembly6 <- ceiling(runif(n = 1, min = 1, max = 100)) # fixed number
    
    QtyAssembly8 <- if (req$Assy1 == "Assembly 1A") {
      runif(n = 1, min = 1, max = 100)
    } else if (req$Assy1 == "Assembly 1B") {
      runif(n = 1, min = 1, max = 100)
    } else if (req$Assy1 == "Assembly 1C") {
      runif(n = 1, min = 1, max = 100)
    } else {
      0
    }
    
    ### Aggregate Data ####
    calc <- as.data.frame(matrix(c(
      "Mod Assembly", QtyMods,
      "Assembly 1A", QtyAssembly1A, 
      "Assembly 1B", QtyAssembly1B,
      "Assembly 1C", QtyAssembly1C, 
      "Assembly 2A", QtyAssembly2A, 
      "Assembly 2B", QtyAssembly2B,
      "Assembly 2C", QtyAssembly2C, 
      "Assembly 4", QtyAssembly4, 
      "Assembly 6", QtyAssembly6,
      "Assembly 8", QtyAssembly8
    ), ncol = 2, byrow = TRUE))
    
    colnames(calc) <- c("Part", "Qty")
    
    calc$Qty <- as.numeric(calc$Qty)
    
    ### Merge Calculated Quantities with Metadata ####
    temp0 <- assy_data %>% filter(Product == req$Product) 
    
    data <- temp0 %>%
      left_join(calc, by = "Part") %>%
      arrange(Sort)
    
    data$Qty[is.na(data$Qty)] <- 0
    
    data <- data %>%
      mutate(Sort = ifelse(is.na(Sort), row_number(), Sort))
    
    data$Product[is.na(data$Product)] <- req$Product
    
    ### Calculate Total Wt From Qty & Part Wt ####
    data$Qty <- as.numeric(data$Qty)
    
    data <- data %>%
      mutate(Total.Wt = Part.Wt * Qty)
    
    ### Composition Calculations####
    comp <- data %>%
      mutate(across(starts_with("Pct"), ~ (.x/100) * Total.Wt, 
                    .names = "Kg.{substr(.col, 4, nchar(.col))}")) %>%
      select(Sort, Product, Part, Part.Wt, Qty, Total.Wt, 
             starts_with("Kg.")) %>%
      mutate(Kg.Unevaluated = 0)
    
    total_known_weight <- sum(comp$Total.Wt, na.rm = TRUE)
    
    defaultWeight(total_known_weight)
    
    # Only auto-update if user hasn't manually edited the weight
    # Only auto-update if we have a meaningful known total (prevents forcing 0 kg)
    if (!userEditedWeight()) {
      if (is.finite(total_known_weight) && total_known_weight > 0) {
        proposed <- ceiling_custom(total_known_weight, 10)
        if (!identical(proposed, input$GlobalWeight)) {
          updateNumericInput(session, "GlobalWeight", value = proposed)
        }
      }
    }
    other_weight <- max(
      as.numeric(input$GlobalWeight) - as.numeric(total_known_weight),
      0, na.rm = TRUE
    )
    
    comp2 <- comp %>%
      bind_rows(data.frame(
        Product = req$Product,
        Part = "Unevaluated Material",
        Total.Wt = other_weight,
        Kg.Material1 = 0,
        Kg.Material2 = 0,
        Kg.Material3 = 0,
        Kg.Other = 0,
        Kg.Unevaluated = other_weight
      ))
    
    tot <- comp2 %>%
      add_row(Part = "Total", 
              Product = req$Product,
              Total.Wt = sum(comp2$Total.Wt),
              Kg.Material1 = sum(comp2$Kg.Material1), 
              Kg.Material2 = sum(comp2$Kg.Material2), 
              Kg.Material3 = sum(comp2$Kg.Material3),
              Kg.Other = sum(comp2$Kg.Other),
              Kg.Unevaluated = sum(comp2$Kg.Unevaluated))
    
    final <- tot %>%
      add_row(Part = "Percent", 
              Product = req$Product,
              Kg.Material1 = tot$Kg.Material1[tot$Part == "Total"]/
                tot$Total.Wt[tot$Part == "Total"] * 100,
              Kg.Material2 = tot$Kg.Material2[tot$Part == "Total"]/
                tot$Total.Wt[tot$Part == "Total"] * 100,
              Kg.Material3 = tot$Kg.Material3[tot$Part == "Total"]/
                tot$Total.Wt[tot$Part == "Total"] * 100,
              Kg.Other = 
                tot$Kg.Other[tot$Part == "Total"]/
                tot$Total.Wt[tot$Part == "Total"] * 100,
              Kg.Unevaluated = 
                tot$Kg.Unevaluated[tot$Part == "Total"]/
                tot$Total.Wt[tot$Part == "Total"] * 100) %>%
      select(-Sort) %>%
      filter(Part != "Percent")
    
    totals <- comp2 %>%
      summarise(
        Total = sum(Total.Wt),
        Pct.Material1 = sum(Kg.Material1) / Total * 100,
        Pct.Material2 = sum(Kg.Material2) / Total * 100,
        Pct.Material3 = sum(Kg.Material3) / Total * 100,
        Pct.Other = sum(Kg.Other) / Total * 100,
        Pct.Unevaluated = sum(Kg.Unevaluated) / Total * 100
      )
    
    pie.data <- totals %>%
      pivot_longer(cols = starts_with("Pct."), names_to = "Material", 
                   values_to = "PctComposition") %>%
      mutate(Material = gsub("Pct\\.", "", Material)) %>%
      select(Material, PctComposition)
    
    return(list(final = final, pie.data = pie.data))
  }
  
  ## Branch C Logic ####
  # Note: Branch C Logic Applies to Product E, F
  runBranchC <- function(req_df3, assy_data) {
    req(input$GlobalWeight)
    
    ### Extract values from req_df ####
    req <- req_df3
    
    ### Stop reactivity loop: freeze RNG per input state ####
    seed_key <- paste(req$Product, req$ModsHigh, req$ModsWide,
                      req$Assy2, req$Assy3, req$Market, sep = "|")
    seed_val <- sum(utf8ToInt(seed_key)) %% .Machine$integer.max
    set.seed(seed_val)
    
    ### Quantity Logic ####
    QtyMods <- req$ModsHigh * req$ModsWide
    
    QtyAssembly2A <- 
      if (req$Assy2 == "Assembly 2A" && req$Market == "Market 1") {
        ceiling_custom(QtyMods * (runif(n = 1, min = 1, max = 100)/100), 1)
      } else if (req$Assy2 == "Assembly 2A") {
        ceiling_custom(QtyMods * (runif(n = 1, min = 1, max = 100)/100), 1)
      } else {
        0
      }
    
    QtyAssembly2B <- 
      if (req$Assy2 == "Assembly 2B" && req$Market == "Market 1") {
        ceiling_custom(QtyMods * (runif(n = 1, min = 1, max = 100)/100), 1)
      } else if (req$Assy2 == "Assembly 2B") {
        ceiling_custom(QtyMods * (runif(n = 1, min = 1, max = 100)/100), 1)
      } else {
        0
      }
    
    QtyAssembly2C <- 
      if (req$Assy2 == "Assembly 2C" && req$Market == "Market 1") {
        ceiling_custom(QtyMods * (runif(n = 1, min = 1, max = 100)/100), 1)
      } else if (req$Assy2 == "Assembly 2C") {
        ceiling_custom(QtyMods * (runif(n = 1, min = 1, max = 100)/100), 1)
      } else {
        0
      }
    
    QtyAssembly3A <- 
      if (req$Assy3 == "Assembly 3A") {
        ceiling_custom(QtyMods * (runif(n = 1, min = 1, max = 100)/100), 1)
      } else {
        0
      }
    
    QtyAssembly3B <- 
      if (req$Assy3 == "Assembly 3B") {
        ceiling_custom(QtyMods * (runif(n = 1, min = 1, max = 100)/100), 1)
      } else {
        0
      }
    
    QtyAssembly3C <- 
      if (req$Assy3 == "Assembly 3C") {
        ceiling_custom(QtyMods * (runif(n = 1, min = 1, max = 100)/100), 1)
      } else {
        0
      }
    
    QtyAssembly5 <- if (req$Assy3 == "Assembly 3A") {
      ceiling_custom(QtyMods * (runif(n = 1, min = 1, max = 100)/100), 1)
    } else if (req$Assy3 == "Assembly 3B") {
      ceiling_custom(QtyMods * (runif(n = 1, min = 1, max = 100)/100), 1)
    } else {
      ceiling_custom(QtyMods * (runif(n = 1, min = 1, max = 100)/100), 1)
    }
    
    QtyAssembly6 <- ceiling(runif(n = 1, min = 1, max = 100)) # fixed number
    
    QtyAssembly7 <- 
      ceiling_custom(req$ModsWide/runif(n = 1, min = 1, max = 100), 10) 
    
    ### Aggregate Data ####
    calc <- as.data.frame(matrix(c(
      "Assembly 2A", QtyAssembly2A,
      "Assembly 2B", QtyAssembly2B,
      "Assembly 2C", QtyAssembly2C,
      "Assembly 3A", QtyAssembly3A,
      "Assembly 3B", QtyAssembly3B,
      "Assembly 3C", QtyAssembly3C,
      "Assembly 5", QtyAssembly5,
      "Assembly 6", QtyAssembly6,
      "Assembly 7", QtyAssembly7
    ), ncol = 2, byrow = T))
    colnames(calc) <- c("Part", "Qty")
    
    calc$Qty <- as.numeric(calc$Qty)
    
    ### Merge Calculated Qtys with Metadata #### 
    temp0 <- assy_data %>% filter(Product == req$Product) 
    
    data <- temp0 %>%
      left_join(calc, by = "Part") %>%
      arrange(Sort)
    
    data$Qty[is.na(data$Qty)] <- 0
    
    data <- data %>%
      mutate(Sort = ifelse(is.na(Sort), row_number(), Sort))
    
    data$Product[is.na(data$Product)] <- req$Product
    
    ### Calculate Total Wt From Qty & Part Wt ####
    data$Qty <- as.numeric(data$Qty)
    
    data <- data %>%
      mutate(Total.Wt = Part.Wt * Qty)
    
    ### Composition Calculations####
    comp <- data %>%
      mutate(across(starts_with("Pct"), ~ (.x/100) * Total.Wt, 
                    .names = "Kg.{substr(.col, 4, nchar(.col))}")) %>%
      select(Sort, Product, Part, Part.Wt, Qty, Total.Wt, 
             starts_with("Kg.")) %>%
      mutate(Kg.Unevaluated = 0)
    
    total_known_weight <- sum(comp$Total.Wt, na.rm = TRUE)
    
    defaultWeight(total_known_weight)
    
    # Only auto-update if user hasn't manually edited the weight
    # Only auto-update if we have a meaningful known total (prevents forcing 0 kg)
    if (!userEditedWeight()) {
      if (is.finite(total_known_weight) && total_known_weight > 0) {
        proposed <- ceiling_custom(total_known_weight, 10)
        if (!identical(proposed, input$GlobalWeight)) {
          updateNumericInput(session, "GlobalWeight", value = proposed)
        }
      }
    }
    other_weight <- max(
      as.numeric(input$GlobalWeight) - as.numeric(total_known_weight),
      0, na.rm = TRUE
    )
    
    comp2 <- comp %>%
      bind_rows(data.frame(
        Product = req$Product,
        Part = "Unevaluated Material",
        Total.Wt = other_weight,
        Kg.Material1 = 0,
        Kg.Material2 = 0,
        Kg.Material3 = 0,
        Kg.Other = 0,
        Kg.Unevaluated = other_weight
      ))
    
    tot <- comp2 %>%
      add_row(Part = "Total", 
              Product = req$Product,
              Total.Wt = sum(comp2$Total.Wt),
              Kg.Material1 = sum(comp2$Kg.Material1), 
              Kg.Material2 = sum(comp2$Kg.Material2), 
              Kg.Material3 = sum(comp2$Kg.Material3),
              Kg.Other = sum(comp2$Kg.Other),
              Kg.Unevaluated = sum(comp2$Kg.Unevaluated))
    
    final <- tot %>%
      add_row(Part = "Percent", 
              Product = req$Product,
              Kg.Material1 = tot$Kg.Material1[tot$Part == "Total"]/
                tot$Total.Wt[tot$Part == "Total"] * 100,
              Kg.Material2 = tot$Kg.Material2[tot$Part == "Total"]/
                tot$Total.Wt[tot$Part == "Total"] * 100,
              Kg.Material3 = tot$Kg.Material3[tot$Part == "Total"]/
                tot$Total.Wt[tot$Part == "Total"] * 100,
              Kg.Other = 
                tot$Kg.Other[tot$Part == "Total"]/
                tot$Total.Wt[tot$Part == "Total"] * 100,
              Kg.Unevaluated = 
                tot$Kg.Unevaluated[tot$Part == "Total"]/
                tot$Total.Wt[tot$Part == "Total"] * 100) %>%
      select(-Sort) %>%
      filter(Part != "Percent")
    
    totals <- comp2 %>%
      summarise(
        Total = sum(Total.Wt),
        Pct.Material1 = sum(Kg.Material1) / Total * 100,
        Pct.Material2 = sum(Kg.Material2) / Total * 100,
        Pct.Material3 = sum(Kg.Material3) / Total * 100,
        Pct.Other = sum(Kg.Other) / Total * 100,
        Pct.Unevaluated = sum(Kg.Unevaluated) / Total * 100
      )
    
    pie.data <- totals %>%
      pivot_longer(cols = starts_with("Pct."), names_to = "Material", 
                   values_to = "PctComposition") %>%
      mutate(Material = gsub("Pct\\.", "", Material)) %>%
      select(Material, PctComposition) %>%
      mutate(Material = str_replace(Material, "(Material)(\\d+)", "\\1 \\2"))
    
    pie.data$Material[pie.data$Material == "Material1"] <- "Material 1"
    
    return(list(final = final, pie.data = pie.data))
  }
  
  ## Reactive Data Pipeline ####
  data <- reactive({
    req(input$Product)
    
    if (input$Product %in% c("Product A", "Product B", "Product C", 
                             "Product G")) {
      req(input$ModsHigh, input$ModsWide, input$Assy1, input$Assy2, 
          input$Assy3, input$Market, input$GlobalWeight)
      
      req_df <- data.frame(
        Product = input$Product,
        ModsHigh = input$ModsHigh,
        ModsWide = input$ModsWide,
        Assy1 = input$Assy1,
        Assy2 = input$Assy2,
        Assy3 = input$Assy3,
        Market = input$Market,
        stringsAsFactors = FALSE
      )
      
      return(runBranchA(req_df, assy_data))
    }
    
    if (input$Product == "Product D") {
      req(input$ModsHigh, input$ModsWide, input$Assy1, input$Assy2, 
          input$Market, input$GlobalWeight)
      
      req_df2 <- data.frame(
        Product = input$Product,
        ModsHigh = input$ModsHigh,
        ModsWide = input$ModsWide,
        Assy1 = input$Assy1,
        Assy2 = input$Assy2,
        Market = input$Market,
        stringsAsFactors = FALSE
      )
      
      return(runBranchB(req_df2, assy_data))
    }
    
    if (input$Product %in% c("Product E", "Product F")) {
      req(input$ModsHigh, input$ModsWide, input$Assy2, input$Assy3, 
          input$Market, input$GlobalWeight)
      
      req_df3 <- data.frame(
        Product = input$Product,
        ModsHigh = input$ModsHigh,
        ModsWide = input$ModsWide,
        Assy2 = input$Assy2,
        Assy3 = input$Assy3,
        Market = input$Market,
        stringsAsFactors = FALSE
      )
      
      return(runBranchC(req_df3, assy_data))
    }
  })
  
  ## Default Values ####
  observeEvent(input$Product, {
    defaultWeight <- switch(input$Product,
                            "Product A" = 500,
                            "Product B" = 600,
                            "Product C" = 550,
                            "Product D" = 400,
                            "Product E" = 650,
                            "Product F" = 700,
                            "Product G" = 700,
                            0 # fallback
    )
    
    ## Flags for default vs. user edited weight behavior #### 
    observeEvent(input$weightWasEdited, {
      userEditedWeight(TRUE)
    })
    
    observeEvent(input$Product, {
      userEditedWeight(FALSE)
    })
    
    observeEvent(input$ModsHigh, {
      userEditedWeight(FALSE)
    })
    
    observeEvent(input$ModsWide, {
      userEditedWeight(FALSE)
    })
    
    updateNumericInput(session, "GlobalWeight", value = defaultWeight)
    
    output$calculatedWeight <- renderText({
      paste("Calculated Weight:", round(defaultWeight(), 2), "Kg")
    })
    
  })
  
  ## Reset Calculator Fields ####
  observeEvent(input$resetBtn, {
    
    # Reset product selection first (this triggers conditionalPanels)
    updateSelectInput(session, "Product", selected = "Product A")
    
    # Reset numeric inputs
    updateNumericInput(session, "GlobalWeight", value = 0)
    updateNumericInput(session, "ModsHigh", value = 1)
    updateNumericInput(session, "ModsWide", value = 1)
    
    # Reset assembly dropdowns
    updateSelectInput(session, "Assy1", selected = "--")
    updateSelectInput(session, "Assy2", selected = "--")
    updateSelectInput(session, "Assy3", selected = "--")
    
    # Reset market dropdown
    updateSelectInput(session, "Market", selected = "--")
    
    # Reset flags for auto‑weight behavior
    userEditedWeight(FALSE)
  })
  
  ## Force mods high and mods wide inputs to be < 100 ####
  observeEvent(input$ModsHigh, {
    if (!is.null(input$ModsHigh) && input$ModsHigh > 100) {
      updateNumericInput(session, "ModsHigh", value = 100)
    }
  })
  
  observeEvent(input$ModsWide, {
    if (!is.null(input$ModsWide) && input$ModsWide > 100) {
      updateNumericInput(session, "ModsWide", value = 100)
    }
  })
  
  ## Generate Pie Chart ####
  # Build the plot as a reactive so both renderPlot and download use the same object
  make_pie <- reactive({
    req(data())
    pie.data <- data()$pie.data
    
    pie.data <- pie.data %>%
      mutate(Material = case_when(
        Material %in% c("Material1", "Material2", "Material3") ~ 
          gsub("Material(\\d+)", "Material \\1", Material),
        Material == "Other" ~ "Other Material",
        Material == "Unevaluated" ~ "Unevaluated Material"
      ),
      Material = factor(Material, levels = c("Material 1", "Material 2", 
                                             "Material 3", "Other Material", 
                                             "Unevaluated Material"))
      )
    
    # Define fixed color mapping (same as before)
    material_colors <- c(
      "Material 1" = "#E60073",  
      "Material 2" = "#FF46A2",  
      "Material 3" = "#FF80BF",  
      "Other Material" = "#FFB3D9",  
      "Unevaluated Material" = "#FFCCE6"   
    )
    
    # Remove materials with 0% composition
    pie.data <- pie.data %>% dplyr::filter(PctComposition > 0)
    
    # If nothing to plot yet, show a friendly message
    if (nrow(pie.data) == 0) {
      # Return a blank ggplot with a message instead of erroring or warning
      return(
        ggplot() +
          theme_void(base_size = 28) +
          annotate("text", x = 0, y = 0, 
                   label = "No composition data to plot yet.",
                   size = 8)
      )
    }
    
    # Keep only colors for materials that actually appear in data
    present_mats <- intersect(names(material_colors), unique(pie.data$Material))
    # If there is any unexpected material name, you can optionally map it to "Other"
    # (Uncomment this block if needed)
    # if (length(setdiff(unique(pie.data$Material), names(material_colors))) > 0) {
    #   pie.data$Material <- ifelse(pie.data$Material %in% names(material_colors),
    #                               pie.data$Material, "Other")
    #   present_mats <- intersect(names(material_colors), unique(pie.data$Material))
    # }
    
    # Create data labels
    labels <- pie.data %>%
      dplyr::mutate(
        csum = rev(cumsum(rev(PctComposition))),
        pos  = PctComposition / 2 + dplyr::lead(csum, 1),
        pos  = dplyr::if_else(is.na(pos), PctComposition / 2, pos)
      )
    
    # Build the plot object
    # Inside your plot code
    p <- ggplot(pie.data, aes(x = "", y = PctComposition, fill = Material)) +
      geom_col(width = 1) +
      coord_polar(theta = "y") +
      scale_fill_manual(
        values = material_colors[present_mats]
      ) +
      ggrepel::geom_label_repel(
        data = labels,
        aes(y = pos, label = paste0(format(round(PctComposition, 2), nsmall = 1), "%")),
        size = 8, nudge_x = 1, show.legend = FALSE, color = "black"
      ) +
      guides(fill = guide_legend(title = "Material")) +
      theme_void(base_size = 28) +
      theme(legend.position = "top")
    
    p + theme(legend.position = "top") +
      guides(fill = guide_legend(title = "Material", nrow = 2, byrow = TRUE))
    
  })
  
  # Render to screen
  output$pieChart <- renderPlot({
    make_pie()
  })
  
  ## Generate Table Output ####
  output$finalTable <- renderTable({
    req(data())
    df <- data()$final
    
    # Reorder columns
    preferred_order <- c(
      "Product", "Part", "Qty", "Part.Wt", "Total.Wt",
      "Kg.Material1", "Kg.Material2", "Kg.Material3", "Kg.Other", 
      "Kg.Unevaluated"
    )
    keep <- intersect(preferred_order, names(df))
    df <- dplyr::select(df, dplyr::all_of(keep), dplyr::everything())
    
    # Rename columns for display
    df <- dplyr::rename(df,
                        "Product" = "Product",
                        "Part" = "Part",
                        "Quantity" = "Qty",
                        "Part Weight (kg)" = "Part.Wt",
                        "Total Weight (kg)" = "Total.Wt",
                        "Material 1 Weight (kg)" = "Kg.Material1",
                        "Material 2 Weight (kg)" = "Kg.Material2",
                        "Material 3 Weight (kg)" = "Kg.Material3",
                        "Other Weight (kg)" = "Kg.Other",
                        "Unevaluated Weight (kg)" = "Kg.Unevaluated"
    )
    
    df
  },
  striped = TRUE, bordered = TRUE, hover = TRUE,
  spacing = "xs", width = "100%", align = "lrrrrrrrrr")
  
  ## Build Plot Download Handler ####
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste0("pie_chart-", Sys.Date(), ".png")
    },
    content = function(file) {
      ggplot2::ggsave(file, plot = make_pie(), device = "png", width = 15, 
                      height = 8, dpi = 600)
    }
  )
  
  ## Build Table Download Handler ####
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("final_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data()$final, file, row.names = FALSE)
    })
  
} # End of server logic

# Launch App ####
shinyApp(ui = ui, server = server) 
