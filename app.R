# Libraries
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)

# Data
bmi <- read.csv("bmi-age-2022.csv")
unique(bmi$sex)

bmi$Sex <- factor(
  bmi$sex,
  levels = c(1, 2),
  labels = c("Boy", "Girl")
)

bmi$AgeYears <- bmi$agemos / 12

# Helper Functions
convert_to_months <- function(age, units) {
  if (units == "Years") {
    age * 12
  } else {
    age
  }
}

nearest_reference <- function(age_months, user_sex) {
  
  sex_code <- if (user_sex == "Boy") 1 else 2
  
  ref <- bmi %>%
    filter(sex == sex_code)
  
  if (nrow(ref) == 0) {
    return(NULL)
  }
  
  ref %>%
    slice(which.min(abs(agemos - age_months)))
  
}

calculate_percentile <- function(user_bmi, ref){
  if (is.null(ref) || nrow(ref) == 0) {
    return(NA)
  }
  if(user_bmi <= ref$P95){
    
    z <- (((user_bmi/ref$M)^ref$L)-1)/
      (ref$L*ref$S)
    
    return(
      pnorm(z)*100
    )
    
  }
  
  z95 <- qnorm(.95)
  
  z <- z95 +
    ((user_bmi-ref$P95)/
       ref$sigma)
  
  return(
    pnorm(z)*100
  )
  
}

weight_category <- function(percentile) {
  
  if (percentile < 5) {
    "Underweight"
  } else if (percentile < 85) {
    "Healthy Weight"
  } else if (percentile < 95) {
    "Overweight"
  } else {
    "Obesity"
  }
  
}

curve_data <- function(df, curves) {
  
  cols <- c(
    "P5" = "5th",
    "P50" = "50th",
    "P85" = "85th",
    "P95" = "95th"
  )
  
  df %>%
    select(
      AgeYears,
      Sex,
      all_of(names(cols))
    ) %>%
    pivot_longer(
      cols = all_of(names(cols)),
      names_to = "Curve",
      values_to = "BMI"
    ) %>%
    mutate(
      Curve = recode(Curve, !!!cols)
    ) %>%
    filter(Curve %in% curves)
  
}

# User Interface
ui <- fluidPage(
  
  titlePanel("CDC 2022 BMI-for-Age Percentile Explorer"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Growth Chart Filters"),
      
      checkboxGroupInput(
        "sexFilter",
        "Sex",
        choices = c("Boy", "Girl"),
        selected = c("Boy", "Girl")
      ),
      
      sliderInput(
        "ageRange",
        "Age Range (Years)",
        min = 2,
        max = 20,
        value = c(2, 20),
        step = 0.5
      ),
      
      checkboxGroupInput(
        "curveFilter",
        "Percentile Curves",
        choices = c(
          "5th",
          "50th",
          "85th",
          "95th"
        ),
        selected = c(
          "5th",
          "50th",
          "85th",
          "95th"
        )
      ),
      
      hr(),
      
      h4("BMI Percentile Calculator"),
      
      radioButtons(
        "userSex",
        "Sex",
        choices = c("Boy", "Girl"),
        inline = TRUE
      ),
      
      fluidRow(
        
        column(
          width = 8,
          
          numericInput(
            "userAge",
            "Age",
            value = 10,
            min = 2
          )
          
        ),
        
        column(
          width = 4,
          
          selectInput(
            "ageUnits",
            "Units",
            choices = c(
              "Years",
              "Months"
            )
          )
          
        )
        
      ),
      
      numericInput(
        "userBMI",
        "BMI (kg/m²)",
        value = 18,
        min = 5,
        max = 60
      ),
      
      hr(),
      
      h4("Results"),
      
      uiOutput("percentileText"),
      
      uiOutput("categoryText")
      
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        tabPanel(
          "Growth Curves",
          plotlyOutput(
            "curvePlot",
            height = "700px"
          )
        ),
        
        tabPanel(
          "BMI Categories",
          plotlyOutput(
            "categoryPlot",
            height = "700px"
          )
        )
        
      )
      
    )
    
  )
  
)
# Server
server <- function(input, output, session) {
  
  filteredData <- reactive({
    
    curve_data(
      bmi %>%
        filter(
          Sex %in% input$sexFilter,
          AgeYears >= input$ageRange[1],
          AgeYears <= input$ageRange[2]
        ),
      input$curveFilter
    )
    
  })
  
  userResult <- reactive({
    
    ageMonths <- convert_to_months(
      input$userAge,
      input$ageUnits
    )
    print(input$userSex)
    print(unique(bmi$sex))
    ref <- nearest_reference(
      ageMonths,
      input$userSex
    )
    if (is.null(ref)) {
      return(NULL)
    }
    percentile <- calculate_percentile(
      input$userBMI,
      ref
    )
    
    percentile <- max(
      0,
      min(100, percentile)
    )
    
    list(
      ageMonths = ageMonths,
      ageYears = ageMonths / 12,
      bmi = input$userBMI,
      sex = input$userSex,
      percentile = percentile,
      category = weight_category(percentile)
    )
    
  })
  
  output$percentileText <- renderUI({
    
    req(userResult())
    
    tagList(
      
      h3(
        round(userResult()$percentile,1)
      ),
      
      strong("BMI Percentile")
      
    )
    
  })
  
  output$categoryText <- renderUI({
    
    req(userResult())
    
    tagList(
      
      h4(
        userResult()$category
      ),
      
      strong("CDC Classification")
      
    )
    
  })
  
  output$curvePlot <- renderPlotly({
    
    df <- filteredData()
    
    color_map <- c(
      "5th" = "dodgerblue3",
      "50th" = "forestgreen",
      "85th" = "darkorange2",
      "95th" = "red3",
      "Your BMI" = "black"
    )
    
    user <- userResult()
    
    if (!is.null(user)) {
      
      userPoint <- data.frame(
        AgeYears = user$ageYears,
        BMI = user$bmi,
        Curve = "Your BMI",
        Sex = factor(user$sex, levels = levels(df$Sex)),
        text = paste(
          "<b>Your BMI</b>",
          "<br>Age:", round(user$ageYears, 1), " years",
          "<br>BMI:", round(user$bmi, 1), " kg/m²",
          "<br>Percentile:", round(user$percentile, 1)
        )
      )
      
      df <- bind_rows(df, userPoint)
      
    }
    
    p <- ggplot(
      df,
      aes(
        x = AgeYears,
        y = BMI,
        color = Curve,
        group = interaction(Sex, Curve),
        text = paste(
          "Sex:", Sex,
          "<br>Age:", round(AgeYears, 1), "years",
          "<br>BMI:", round(BMI, 2), "kg/m²",
          "<br>Percentile:", Curve
        )
      )
    ) +
      
      geom_line(
        data = subset(df, Curve != "Your BMI"),
        linewidth = 1.1
      ) +
      
      facet_wrap(~Sex) +
      
      scale_color_manual(
        values = color_map
      ) +
      
      labs(
        title = "BMI Growth Curves by Percentile",
        subtitle = "CDC 2022 BMI-for-Age Reference Curves",
        x = "Age (Years)",
        y = "BMI (kg/m²)",
        color = "Percentile"
      ) +
      
      theme_minimal(base_size = 14) +
      
      theme(
        plot.title = element_text(
          hjust = .5,
          face = "bold"
        ),
        plot.subtitle = element_text(
          hjust = .5
        ),
        legend.position = "bottom",
        strip.text = element_text(
          face = "bold",
          size = 12
        )
      )
    
    if (!is.null(user)) {
      
      p <- p +
        geom_point(
          data = subset(df, Curve == "Your BMI"),
          aes(
            x = AgeYears,
            y = BMI,
            text = text
          ),
          inherit.aes = FALSE,
          shape = 8,
          size = 6,
          color = "black"
        )
    }
    
    ggplotly(
      p,
      tooltip = "text"
    )
  })
  
  output$categoryPlot <- renderPlotly({
    
    df <-
      curve_data(
        bmi %>%
          filter(
            Sex %in% input$sexFilter,
            AgeYears >= input$ageRange[1],
            AgeYears <= input$ageRange[2]
          ),
        c("5th", "85th", "95th")
      )
    
    p5 <- df %>%
      filter(Curve == "5th") %>%
      arrange(Sex, AgeYears)
    
    p85 <- df %>%
      filter(Curve == "85th") %>%
      arrange(Sex, AgeYears)
    
    p95 <- df %>%
      filter(Curve == "95th") %>%
      arrange(Sex, AgeYears)
    
    ribbonData <-
      p5 %>%
      select(Sex, AgeYears, BMI5 = BMI) %>%
      left_join(
        p85 %>%
          select(Sex, AgeYears, BMI85 = BMI),
        by = c("Sex","AgeYears")
      ) %>%
      left_join(
        p95 %>%
          select(Sex, AgeYears, BMI95 = BMI),
        by = c("Sex","AgeYears")
      )
    
    p <-
      
      ggplot(ribbonData,
             aes(x = AgeYears)) +
      
      geom_ribbon(
        aes(
          ymin = 0,
          ymax = BMI5,
          fill = "Underweight"
        ),
        alpha = .45
      ) +
      
      geom_ribbon(
        aes(
          ymin = BMI5,
          ymax = BMI85,
          fill = "Healthy"
        ),
        alpha = .45
      ) +
      
      geom_ribbon(
        aes(
          ymin = BMI85,
          ymax = BMI95,
          fill = "Overweight"
        ),
        alpha = .45
      ) +
      
      geom_ribbon(
        aes(
          ymin = BMI95,
          ymax = max(ribbonData$BMI95) + 5,
          fill = "Obesity"
        ),
        alpha = .45
      ) +
      
      facet_wrap(~Sex) +
      
      scale_fill_manual(
        
        values = c(
          
          "Underweight" = "dodgerblue3",
          
          "Healthy" = "forestgreen",
          
          "Overweight" = "darkorange2",
          
          "Obesity" = "red3"
          
        )
        
      ) +
      
      labs(
        
        title = "BMI Categories by Age",
        
        subtitle =
          "CDC BMI-for-Age Health Categories",
        
        x = "Age (Years)",
        
        y = "BMI (kg/m²)",
        
        fill = "Weight Category"
        
      ) +
      
      theme_minimal(base_size = 14) +
      
      theme(
        
        plot.title =
          element_text(
            hjust = .5,
            face = "bold"
          ),
        
        plot.subtitle =
          element_text(hjust = .5),
        
        legend.position = "right",
        
        plot.margin = margin(
          t = 10,
          r = 10,
          b = 30,
          l = 10
        ),
        
        strip.text =
          element_text(
            face = "bold"
          )
        
      )
    
    user <- userResult()
    
    boundaryPoint <- NULL
    
    if (!is.null(user)) {
      
      ref <- nearest_reference(
        user$ageMonths,
        user$sex
      )
      
      bmi5  <- ref$P5
      bmi85 <- ref$P85
      
      if (user$bmi < bmi5) {
        
        boundaryBMI <- bmi5
        
      } else if (user$bmi > bmi85) {
        
        boundaryBMI <- bmi85
        
      } else {
        
        if ((user$bmi - bmi5) < (bmi85 - user$bmi)) {
          boundaryBMI <- bmi5
        } else {
          boundaryBMI <- bmi85
        }
        
      }
      
      boundaryPoint <- data.frame(
        
        Sex = factor(
          user$sex,
          levels = levels(ribbonData$Sex)
        ),
        
        AgeYears = user$ageYears,
        
        y = user$bmi,
        
        boundary = boundaryBMI,
        
        distance = abs(user$bmi - boundaryBMI)
        
      )
      
    }
    
    if (!is.null(user)) {
      
      p <-
        p +
        geom_point(
          
          data = data.frame(
            Sex = factor(
              user$sex,
              levels = levels(ribbonData$Sex)
            ),
            AgeYears = user$ageYears,
            
            BMI = user$bmi
            
          ),
          aes(
            x = AgeYears,
            y = BMI
          ),
          inherit.aes = FALSE,
          shape = 8,
          size = 5,
          color = "black"
        )
    }
    if (!is.null(boundaryPoint)) {
      
      p <- p +
        
        geom_segment(
          
          data = boundaryPoint,
          
          aes(
            x = AgeYears,
            xend = AgeYears,
            y = y,
            yend = boundary
          ),
          
          inherit.aes = FALSE,
          
          linewidth = .5,
          
          linetype = "dashed",
          
          color = "black"
          
        )
    p <- p +
      
      geom_point(
        data = data.frame(
          Sex = boundaryPoint$Sex,
          AgeYears = boundaryPoint$AgeYears,
          BMI = (boundaryPoint$y + boundaryPoint$boundary) / 2,
          text = paste0(
            "Distance to nearest healthy BMI boundary: ",
            round(boundaryPoint$distance, 2),
            " kg/m²"
          )
        ),
        aes(
          AgeYears,
          BMI,
          text = text
        ),
        inherit.aes = FALSE,
        alpha = 0.01,
        size = 10
      )
  }
    ggplotly(
      p,
      tooltip = "text"
    )
    
  })
}

shinyApp(ui, server)