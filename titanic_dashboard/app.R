# Import libraries
library(dplyr)
library(ggplot2)
library(shiny)
library(shinydashboard)

ui = dashboardPage(
  dashboardHeader(title = "Titanic Dashboard"),
  dashboardSidebar(
    fileInput("tdFile", "Dane wejściowe",
      accept = c(
        "text/csv",
        "text/comma-separated-values,text/plain",
        ".csv")),
    sidebarMenu(
      menuItem("Podział wg cech", tabName = "featBreakdown", icon = icon("chart-pie")),
      menuItem("Rozkład wieku", tabName = "ageDistrib", icon = icon("birthday-cake")),
      menuItem("Oszacuj szanse przetrwania", tabName = "survChances", icon = icon("life-ring")))),
  dashboardBody(
    tabItems(
      tabItem(tabName = "featBreakdown",
        fluidRow(
          column(12,
            selectInput(
              "featSelect",
              "Kryterium podziału",
              choices = list(
                "Płeć" = 1,
                "Klasa biletu" = 2,
                "Miasto startowe" = 3),
              selected = 1,
              multiple = FALSE,
              width = '100%')),
          column(6, plotOutput("pieChart1", width = '100%')),
          column(6, plotOutput("pieChart2", width = '100%')))),
      tabItem(tabName = "ageDistrib"),
      tabItem(tabName = "survChances")
    )
  )
);

server = function(input, output) {
  inputData = reactive({
    inputFile = input$tdFile;
    if (is.null(inputFile))
      return(NULL);
    mutate(
      read.csv(inputFile$datapath, sep = ","),
      Sex = case_when(
        Sex == 1 ~ "Mężczyzna",
        Sex == 2 ~ "Kobieta",
        TRUE ~ "Nieznana"
      ),
      Embarked = case_when(
        Embarked == 1 ~ "Cherbourg",
        Embarked == 2 ~ "Queenstown",
        Embarked == 3 ~ "Southampton",
        TRUE ~ "Nieznane"
      ),
      Pclass = case_when(
        Pclass == 1 ~ "Wyższa",
        Pclass == 2 ~ "Średnia",
        Pclass == 3 ~ "Niższa"
      ));
  });
  
  pieChartLayers = reactive({
    list(
      geom_bar(width = 1, stat = "identity"),
      coord_polar("y", start = 0),
      theme_minimal(),
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        plot.title=element_text(family = "Helvetica Neue", size=16, face="bold", hjust = 0.5)),
      labs(
        fill = case_when(
          input$featSelect == 1 ~ "Płeć",
          input$featSelect == 2 ~ "Klasa biletu",
          input$featSelect == 3 ~ "Miasto startowe")));
  });
  
  output$pieChart1 = renderPlot({
    td = inputData();
    if (is.null(td)) {
      return(NULL);s
    } else {
      ggplot(td,
        aes(
          x = "",
          y = "",
          fill = case_when(
            input$featSelect == 1 ~ Sex,
            input$featSelect == 2 ~ Pclass,
            input$featSelect == 3 ~ Embarked))) +
      pieChartLayers() + ggtitle("Wszyscy pasażerowie");
    }
  }, bg = "transparent");
  
  output$pieChart2 = renderPlot({
    td = inputData();
    if (is.null(td)) {
      return(NULL);
    } else {
      ggplot(td,
        aes(
          x = "",
          y = Survived,
          fill = case_when(
            input$featSelect == 1 ~ Sex,
            input$featSelect == 2 ~ Pclass,
            input$featSelect == 3 ~ Embarked))) +
        pieChartLayers() + ggtitle("Ocaleni pasażerowie");
    }
  }, bg = "transparent");
};

shinyApp(ui, server)