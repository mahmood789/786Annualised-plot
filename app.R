# app.R

# ---------------------------------------------------------------
# Required packages
# ---------------------------------------------------------------
library(shiny)
library(bs4Dash)
library(ggplot2)
library(ggbeeswarm)   # for geom_quasirandom
library(dplyr)
library(DT)

# ---------------------------------------------------------------
# 1. Define a sample CSV as a string for download.
#    The CSV contains two types of rows:
#      - "summary": for mean value and SE (for each outcome and group)
#      - "point": for individual study values (se left blank)
# ---------------------------------------------------------------
sample_csv <- 
  'row_type,outcome,group,value,se
summary,All-Cause Mortality,LGE+,5.08,0.8
summary,All-Cause Mortality,LGE-,1.07,0.3
summary,Ventricular Arrhythmia,LGE+,6.72,1.0
summary,Ventricular Arrhythmia,LGE-,0.04,0.01
summary,Composite,LGE+,12.4,2.0
summary,Composite,LGE-,1.18,0.5
point,All-Cause Mortality,LGE+,3.2,
point,All-Cause Mortality,LGE+,4.7,
point,All-Cause Mortality,LGE+,6.1,
point,All-Cause Mortality,LGE-,1.0,
point,All-Cause Mortality,LGE-,1.3,
point,Ventricular Arrhythmia,LGE+,8.2,
point,Ventricular Arrhythmia,LGE+,6.9,
point,Ventricular Arrhythmia,LGE-,0.08,
point,Ventricular Arrhythmia,LGE-,0.03,
point,Composite,LGE+,13.0,
point,Composite,LGE+,11.5,
point,Composite,LGE+,14.2,
point,Composite,LGE-,1.6,
point,Composite,LGE-,0.9,
'

# ---------------------------------------------------------------
# 2. Define the UI with bs4Dash and many customization options.
# ---------------------------------------------------------------
ui <- bs4DashPage(
  title = "Customizable Plot App",
  header = bs4DashNavbar(title = "Customizable Plot for LGE+ vs LGE-"),
  sidebar = bs4DashSidebar(
    bs4SidebarMenu(
      bs4SidebarMenuItem("Data", tabName = "data", icon = icon("table")),
      bs4SidebarMenuItem("Plot", tabName = "plot", icon = icon("chart-bar")),
      bs4SidebarMenuItem("Summary", tabName = "summary", icon = icon("info-circle"))
    )
  ),
  body = bs4DashBody(
    tabItems(
      # ----- Data Tab -----
      tabItem(
        tabName = "data",
        fluidRow(
          box(
            width = 6, status = "primary", solidHeader = TRUE,
            title = "Download Sample CSV",
            downloadButton("downloadSample", "Download Sample CSV"),
            br(), br(),
            helpText("The sample CSV contains two row types:",
                     tags$ul(
                       tags$li("'summary': Contains mean (value) and standard error (se) for each outcome and group"),
                       tags$li("'point': Contains individual study data (se left blank)")
                     ),
                     "You can modify or create your own CSV with columns: row_type, outcome, group, value, se."
            )
          ),
          box(
            width = 6, status = "primary", solidHeader = TRUE,
            title = "Upload Your CSV",
            fileInput("fileInput", "Upload CSV", 
                      accept = c(".csv", "text/csv", "text/plain")),
            helpText("CSV must contain columns: row_type, outcome, group, value, se")
          )
        )
      ),
      # ----- Plot Tab -----
      tabItem(
        tabName = "plot",
        fluidRow(
          box(
            width = 12, status = "primary", solidHeader = TRUE,
            title = "Plot Options",
            fluidRow(
              column(3,
                     textInput("plotTitle", "Plot Title", "Custom Plot: LGE+ vs LGE-"),
                     textInput("plotSubtitle", "Plot Subtitle", "Mean ± (Error Multiplier * SE) + Individual Study Points"),
                     textInput("xLabel", "X-axis Label", "Outcome"),
                     textInput("yLabel", "Y-axis Label", "Annualized Event Rate (%)")
              ),
              column(3,
                     numericInput("errorMult", "Error Bar Multiplier", value = 1.5, min = 0.1, step = 0.1),
                     numericInput("pointSize", "Point Size", value = 2.5, min = 0.5, step = 0.1),
                     checkboxInput("includeError", "Include Error Bars", value = TRUE),
                     numericInput("xTextAngle", "X-axis Text Angle", value = 0, min = 0, max = 90, step = 5),
                     textInput("legendTitle", "Legend Title", "Group")
              ),
              column(3,
                     selectInput("plotTheme", "Plot Theme",
                                 choices = c("Minimal" = "minimal", 
                                             "Classic" = "classic", 
                                             "BW" = "bw", 
                                             "Void" = "void"),
                                 selected = "minimal"),
                     selectInput("fillColorLGEpos", "Fill Color for LGE+", 
                                 choices = c("salmon", "tomato", "red", "orange"), 
                                 selected = "salmon"),
                     selectInput("fillColorLGEneg", "Fill Color for LGE-", 
                                 choices = c("palegreen3", "lightgreen", "green", "seagreen"), 
                                 selected = "palegreen3"),
                     selectInput("pointColorLGEpos", "Point Color for LGE+", 
                                 choices = c("red3", "firebrick", "darkred"), 
                                 selected = "red3"),
                     selectInput("pointColorLGEneg", "Point Color for LGE-", 
                                 choices = c("darkgreen", "forestgreen", "green4"), 
                                 selected = "darkgreen")
              ),
              column(3,
                     textInput("bgColor", "Plot Background Color", "white"),
                     textInput("gridColor", "Grid Color", "gray90"),
                     numericInput("titleSize", "Title Font Size", value = 16, min = 10, max = 30),
                     numericInput("axisTitleSize", "Axis Title Size", value = 14, min = 10, max = 30)
              )
            )
          )
        ),
        fluidRow(
          # Additional options for Histogram and Density plots
          box(
            width = 12, status = "warning", solidHeader = TRUE,
            title = "Additional Options for Histogram, Density, & ECDF Plots",
            column(6,
                   numericInput("histBins", "Histogram Bins", value = 10, min = 5, max = 30)
            ),
            column(6,
                   sliderInput("densityAdjust", "Density Adjust", value = 1, min = 0.5, max = 3, step = 0.1)
            )
          )
        ),
        fluidRow(
          box(
            width = 12, status = "primary", solidHeader = TRUE,
            title = "Plots",
            tabsetPanel(
              tabPanel("Bar+Scatter",
                       plotOutput("barScatterPlot", height = "600px"),
                       br(),
                       downloadButton("downloadPlot1", "Download Bar+Scatter Plot (PNG)")
              ),
              tabPanel("Boxplot+Scatter",
                       plotOutput("boxScatterPlot", height = "600px"),
                       br(),
                       downloadButton("downloadPlot2", "Download Boxplot+Scatter Plot (PNG)")
              ),
              tabPanel("Violin+Scatter",
                       plotOutput("violinScatterPlot", height = "600px"),
                       br(),
                       downloadButton("downloadPlot3", "Download Violin+Scatter Plot (PNG)")
              ),
              tabPanel("Scatter Only",
                       plotOutput("scatterPlot", height = "600px"),
                       br(),
                       downloadButton("downloadPlot4", "Download Scatter Only Plot (PNG)")
              ),
              tabPanel("Histogram",
                       plotOutput("histPlot", height = "600px"),
                       br(),
                       downloadButton("downloadPlot5", "Download Histogram Plot (PNG)")
              ),
              tabPanel("Density Plot",
                       plotOutput("densityPlot", height = "600px"),
                       br(),
                       downloadButton("downloadPlot6", "Download Density Plot (PNG)")
              ),
              tabPanel("ECDF Plot",
                       plotOutput("ecdfPlot", height = "600px"),
                       br(),
                       downloadButton("downloadPlot7", "Download ECDF Plot (PNG)")
              )
            )
          )
        )
      ),
      # ----- Summary Tab -----
      tabItem(
        tabName = "summary",
        fluidRow(
          box(
            width = 12, status = "primary", solidHeader = TRUE,
            title = "Data Summary (Verbatim & Head)",
            verbatimTextOutput("dataSummary"),
            br(),
            h4("First 10 Rows of Data:"),
            tableOutput("dataHead")
          )
        ),
        fluidRow(
          box(
            width = 12, status = "primary", solidHeader = TRUE,
            title = "Aggregate Statistics by Outcome and Group",
            DTOutput("aggTable"),
            br(),
            downloadButton("downloadAgg", "Download Aggregated Data (CSV)")
          )
        ),
        fluidRow(
          box(
            width = 12, status = "info", solidHeader = TRUE,
            title = "Selected Options Summary",
            verbatimTextOutput("optionsSummary")
          )
        )
      )
    )
  )
)

# ---------------------------------------------------------------
# 3. Define the server logic
# ---------------------------------------------------------------
server <- function(input, output, session) {
  
  # --------------------------
  # A. Download Sample CSV
  # --------------------------
  output$downloadSample <- downloadHandler(
    filename = function() {
      "sample_data.csv"
    },
    content = function(file) {
      writeLines(sample_csv, con = file)
    }
  )
  
  # --------------------------
  # B. Read Data: Use uploaded CSV or fallback to sample CSV
  # --------------------------
  dataInput <- reactive({
    if (is.null(input$fileInput)) {
      df <- read.csv(text = sample_csv, stringsAsFactors = FALSE)
    } else {
      df <- read.csv(input$fileInput$datapath, stringsAsFactors = FALSE)
    }
    validate(
      need(all(c("row_type", "outcome", "group", "value", "se") %in% names(df)),
           "CSV must contain columns: row_type, outcome, group, value, se")
    )
    df
  })
  
  # --------------------------
  # C. Define a reactive ggplot theme based on selection plus appearance options
  # --------------------------
  getPlotTheme <- reactive({
    baseTheme <- switch(input$plotTheme,
                        "classic" = theme_classic(base_size = 14),
                        "bw"      = theme_bw(base_size = 14),
                        "void"    = theme_void(base_size = 14),
                        theme_minimal(base_size = 14)
    )
    baseTheme +
      theme(
        plot.background = element_rect(fill = input$bgColor, color = NA),
        panel.background = element_rect(fill = input$bgColor, color = NA),
        panel.grid.major = element_line(color = input$gridColor),
        plot.title = element_text(size = input$titleSize),
        axis.title = element_text(size = input$axisTitleSize)
      )
  })
  
  # Common scales for fill and color based on inputs:
  getFillScale <- reactive({
    scale_fill_manual(values = c("LGE+" = input$fillColorLGEpos, "LGE-" = input$fillColorLGEneg))
  })
  
  getColorScale <- reactive({
    scale_color_manual(values = c("LGE+" = input$pointColorLGEpos, "LGE-" = input$pointColorLGEneg))
  })
  
  # --------------------------
  # D. Create Bar+Scatter Plot (Plot 1)
  # --------------------------
  plotReactive1 <- reactive({
    df <- dataInput()
    df_summary <- df %>% filter(row_type == "summary")
    df_points  <- df %>% filter(row_type == "point")
    
    outcomes_order <- unique(df_summary$outcome)
    df_summary$outcome <- factor(df_summary$outcome, levels = outcomes_order)
    df_points$outcome  <- factor(df_points$outcome, levels = outcomes_order)
    
    p <- ggplot() +
      geom_col(
        data = df_summary,
        aes(x = outcome, y = value, fill = group),
        position = position_dodge(width = 0.7),
        width = 0.6
      ) +
      {if (input$includeError)
        geom_errorbar(
          data = df_summary,
          aes(
            x = outcome,
            ymin = value - input$errorMult * se,
            ymax = value + input$errorMult * se,
            group = group
          ),
          position = position_dodge(width = 0.7),
          width = 0.2
        )
      } +
      geom_quasirandom(
        data = df_points,
        aes(x = outcome, y = value, color = group),
        dodge.width = 0.7,
        size = input$pointSize,
        alpha = 0.7
      ) +
      getFillScale() +
      getColorScale() +
      labs(
        title = input$plotTitle,
        subtitle = input$plotSubtitle,
        x = input$xLabel,
        y = input$yLabel,
        color = input$legendTitle,
        fill = input$legendTitle
      ) +
      getPlotTheme() +
      theme(
        legend.position = "right",
        axis.text.x = element_text(angle = input$xTextAngle, hjust = ifelse(input$xTextAngle > 0, 1, 0.5))
      )
    
    p
  })
  
  # --------------------------
  # E. Create Boxplot+Scatter Plot (Plot 2)
  # --------------------------
  plotReactive2 <- reactive({
    df <- dataInput()
    df_points <- df %>% filter(row_type == "point")
    df_summary <- df %>% filter(row_type == "summary")
    
    outcomes_order <- unique(df_summary$outcome)
    df_points$outcome <- factor(df_points$outcome, levels = outcomes_order)
    df_summary$outcome <- factor(df_summary$outcome, levels = outcomes_order)
    
    p <- ggplot(data = df_points, aes(x = outcome, y = value, fill = group)) +
      geom_boxplot(position = position_dodge(width = 0.7), alpha = 0.5, outlier.shape = NA) +
      geom_jitter(aes(color = group),
                  position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.7),
                  size = input$pointSize, alpha = 0.8) +
      geom_point(data = df_summary, aes(x = outcome, y = value, group = group),
                 position = position_dodge(width = 0.7), shape = 21, size = input$pointSize + 1, color = "black") +
      getFillScale() +
      getColorScale() +
      labs(
        title = input$plotTitle,
        subtitle = input$plotSubtitle,
        x = input$xLabel,
        y = input$yLabel,
        color = input$legendTitle,
        fill = input$legendTitle
      ) +
      getPlotTheme() +
      theme(
        legend.position = "right",
        axis.text.x = element_text(angle = input$xTextAngle, hjust = ifelse(input$xTextAngle > 0, 1, 0.5))
      )
    
    p
  })
  
  # --------------------------
  # F. Create Violin+Scatter Plot (Plot 3)
  # --------------------------
  plotReactive3 <- reactive({
    df <- dataInput()
    df_points <- df %>% filter(row_type == "point")
    df_summary <- df %>% filter(row_type == "summary")
    
    outcomes_order <- unique(df_summary$outcome)
    df_points$outcome <- factor(df_points$outcome, levels = outcomes_order)
    df_summary$outcome <- factor(df_summary$outcome, levels = outcomes_order)
    
    p <- ggplot(data = df_points, aes(x = outcome, y = value, fill = group)) +
      geom_violin(position = position_dodge(width = 0.7), alpha = 0.5) +
      geom_jitter(aes(color = group),
                  position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.7),
                  size = input$pointSize, alpha = 0.8) +
      geom_point(data = df_summary, aes(x = outcome, y = value, group = group),
                 position = position_dodge(width = 0.7), shape = 21, size = input$pointSize + 1, color = "black") +
      getFillScale() +
      getColorScale() +
      labs(
        title = input$plotTitle,
        subtitle = input$plotSubtitle,
        x = input$xLabel,
        y = input$yLabel,
        color = input$legendTitle,
        fill = input$legendTitle
      ) +
      getPlotTheme() +
      theme(
        legend.position = "right",
        axis.text.x = element_text(angle = input$xTextAngle, hjust = ifelse(input$xTextAngle > 0, 1, 0.5))
      )
    
    p
  })
  
  # --------------------------
  # G. Create Scatter Only Plot (Plot 4)
  # --------------------------
  plotReactive4 <- reactive({
    df <- dataInput()
    df_points <- df %>% filter(row_type == "point")
    df_summary <- df %>% filter(row_type == "summary")
    outcomes_order <- unique(df_summary$outcome)
    df_points$outcome <- factor(df_points$outcome, levels = outcomes_order)
    
    p <- ggplot(data = df_points, aes(x = outcome, y = value, color = group)) +
      geom_quasirandom(dodge.width = 0.7,
                       size = input$pointSize,
                       alpha = 0.8) +
      getColorScale() +
      labs(
        title = input$plotTitle,
        subtitle = input$plotSubtitle,
        x = input$xLabel,
        y = input$yLabel,
        color = input$legendTitle
      ) +
      getPlotTheme() +
      theme(
        legend.position = "right",
        axis.text.x = element_text(angle = input$xTextAngle, hjust = ifelse(input$xTextAngle > 0, 1, 0.5))
      )
    
    p
  })
  
  # --------------------------
  # H. Create Histogram Plot (Plot 5)
  # --------------------------
  plotReactive5 <- reactive({
    df <- dataInput() %>% filter(row_type == "point")
    df_summary <- dataInput() %>% filter(row_type == "summary")
    outcomes_order <- unique(df_summary$outcome)
    df$outcome <- factor(df$outcome, levels = outcomes_order)
    
    p <- ggplot(df, aes(x = value, fill = group)) +
      geom_histogram(alpha = 0.7, position = "dodge", bins = input$histBins) +
      facet_wrap(~ outcome) +
      getFillScale() +
      labs(
        title = paste(input$plotTitle, "- Histogram"),
        x = "Value", 
        y = "Count"
      ) +
      getPlotTheme() +
      theme(legend.position = "right")
    
    p
  })
  
  # --------------------------
  # I. Create Density Plot (Plot 6)
  # --------------------------
  plotReactive6 <- reactive({
    df <- dataInput() %>% filter(row_type == "point")
    df_summary <- dataInput() %>% filter(row_type == "summary")
    outcomes_order <- unique(df_summary$outcome)
    df$outcome <- factor(df$outcome, levels = outcomes_order)
    
    p <- ggplot(df, aes(x = value, fill = group)) +
      geom_density(alpha = 0.7, adjust = input$densityAdjust) +
      facet_wrap(~ outcome) +
      getFillScale() +
      labs(
        title = paste(input$plotTitle, "- Density Plot"),
        x = "Value", 
        y = "Density"
      ) +
      getPlotTheme() +
      theme(legend.position = "right")
    
    p
  })
  
  # --------------------------
  # J. Create ECDF Plot (Plot 7)
  # --------------------------
  plotReactive7 <- reactive({
    df <- dataInput() %>% filter(row_type == "point")
    df_summary <- dataInput() %>% filter(row_type == "summary")
    outcomes_order <- unique(df_summary$outcome)
    df$outcome <- factor(df$outcome, levels = outcomes_order)
    
    p <- ggplot(df, aes(x = value, color = group)) +
      stat_ecdf(geom = "step", size = 1) +
      facet_wrap(~ outcome) +
      getColorScale() +
      labs(
        title = paste(input$plotTitle, "- ECDF Plot"),
        x = "Value", 
        y = "ECDF",
        color = input$legendTitle
      ) +
      getPlotTheme() +
      theme(legend.position = "right")
    
    p
  })
  
  # --------------------------
  # K. Render the plots in their tabs
  # --------------------------
  output$barScatterPlot <- renderPlot({ plotReactive1() })
  output$boxScatterPlot <- renderPlot({ plotReactive2() })
  output$violinScatterPlot <- renderPlot({ plotReactive3() })
  output$scatterPlot <- renderPlot({ plotReactive4() })
  output$histPlot <- renderPlot({ plotReactive5() })
  output$densityPlot <- renderPlot({ plotReactive6() })
  output$ecdfPlot <- renderPlot({ plotReactive7() })
  
  # --------------------------
  # L. Download handlers for each plot
  # --------------------------
  output$downloadPlot1 <- downloadHandler(
    filename = function() {
      "bar_scatter_plot.png"
    },
    content = function(file) {
      ggsave(file, plot = plotReactive1(), width = 10, height = 6, dpi = 300)
    }
  )
  
  output$downloadPlot2 <- downloadHandler(
    filename = function() {
      "box_scatter_plot.png"
    },
    content = function(file) {
      ggsave(file, plot = plotReactive2(), width = 10, height = 6, dpi = 300)
    }
  )
  
  output$downloadPlot3 <- downloadHandler(
    filename = function() {
      "violin_scatter_plot.png"
    },
    content = function(file) {
      ggsave(file, plot = plotReactive3(), width = 10, height = 6, dpi = 300)
    }
  )
  
  output$downloadPlot4 <- downloadHandler(
    filename = function() {
      "scatter_only_plot.png"
    },
    content = function(file) {
      ggsave(file, plot = plotReactive4(), width = 10, height = 6, dpi = 300)
    }
  )
  
  output$downloadPlot5 <- downloadHandler(
    filename = function() {
      "histogram_plot.png"
    },
    content = function(file) {
      ggsave(file, plot = plotReactive5(), width = 10, height = 6, dpi = 300)
    }
  )
  
  output$downloadPlot6 <- downloadHandler(
    filename = function() {
      "density_plot.png"
    },
    content = function(file) {
      ggsave(file, plot = plotReactive6(), width = 10, height = 6, dpi = 300)
    }
  )
  
  output$downloadPlot7 <- downloadHandler(
    filename = function() {
      "ecdf_plot.png"
    },
    content = function(file) {
      ggsave(file, plot = plotReactive7(), width = 10, height = 6, dpi = 300)
    }
  )
  
  # --------------------------
  # M. Summary Tab: Show text outputs and aggregated statistics
  # --------------------------
  output$dataSummary <- renderPrint({
    df <- dataInput()
    cat("Dataset Summary:\n")
    print(summary(df))
  })
  
  output$dataHead <- renderTable({
    head(dataInput(), 10)
  })
  
  output$aggTable <- renderDT({
    # Aggregate individual study ("point") rows with detailed stats
    df <- dataInput() %>% filter(row_type == "point")
    agg <- df %>% 
      group_by(outcome, group) %>% 
      summarize(
        Count = n(),
        Mean = round(mean(value, na.rm = TRUE), 2),
        Median = round(median(value, na.rm = TRUE), 2),
        SD = round(sd(value, na.rm = TRUE), 2),
        Min = round(min(value, na.rm = TRUE), 2),
        Max = round(max(value, na.rm = TRUE), 2)
      ) %>% ungroup()
    datatable(agg)
  })
  
  output$downloadAgg <- downloadHandler(
    filename = function() {
      "aggregated_data.csv"
    },
    content = function(file) {
      df <- dataInput() %>% filter(row_type == "point")
      agg <- df %>% 
        group_by(outcome, group) %>% 
        summarize(
          Count = n(),
          Mean = round(mean(value, na.rm = TRUE), 2),
          Median = round(median(value, na.rm = TRUE), 2),
          SD = round(sd(value, na.rm = TRUE), 2),
          Min = round(min(value, na.rm = TRUE), 2),
          Max = round(max(value, na.rm = TRUE), 2)
        ) %>% ungroup()
      write.csv(agg, file, row.names = FALSE)
    }
  )
  
  output$optionsSummary <- renderPrint({
    cat("Current Plot Options:\n")
    cat("Plot Title: ", input$plotTitle, "\n")
    cat("Plot Subtitle: ", input$plotSubtitle, "\n")
    cat("X-axis Label: ", input$xLabel, "\n")
    cat("Y-axis Label: ", input$yLabel, "\n")
    cat("Error Bar Multiplier: ", input$errorMult, "\n")
    cat("Point Size: ", input$pointSize, "\n")
    cat("X-axis Text Angle: ", input$xTextAngle, "\n")
    cat("Legend Title: ", input$legendTitle, "\n")
    cat("Plot Theme: ", input$plotTheme, "\n")
    cat("Fill Colors: LGE+ =", input$fillColorLGEpos, ", LGE- =", input$fillColorLGEneg, "\n")
    cat("Point Colors: LGE+ =", input$pointColorLGEpos, ", LGE- =", input$pointColorLGEneg, "\n")
    cat("Background Color: ", input$bgColor, "\n")
    cat("Grid Color: ", input$gridColor, "\n")
    cat("Title Font Size: ", input$titleSize, "\n")
    cat("Axis Title Size: ", input$axisTitleSize, "\n")
    cat("Histogram Bins: ", input$histBins, "\n")
    cat("Density Adjust: ", input$densityAdjust, "\n")
  })
  
}

# ---------------------------------------------------------------
# 4. Run the App
# ---------------------------------------------------------------
shinyApp(ui, server)
