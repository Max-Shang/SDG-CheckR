#https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/

library(shiny)
library(shinythemes)
library(shinyWidgets)


ui <- fluidPage(
  theme = shinytheme("cerulean"),
  
  
  headerPanel(
    fluidRow(
      column(11, img(height = 50, width = 500, src = "logo11.png"))
    )
  ),
  
  titlePanel(
    fluidRow(
      column(6, img(height = 250, width = 450, src = "logo10.png")),
      column(4,  offset = 4, h1(strong(id="title", ("SDG File Checker"))),
             tags$style(HTML("#title{color: cerulean;}"))
      )
    )
  ),
  
  sidebarLayout(
    
    sidebarPanel(width = 5,
                 
                 #      img(src='logo2.png', height = 200, width = 300),
                 fileInput('file1', 'Choose xlsx file containing SDG data',
                           accept = c(".xlsx")),
                 h3(em("Results displayed to the right.")),
                 h5(em("To see source code for SDGcheckR click link below.")),
                 h5(a(href = 'https://github.com/Max-Shang/SDG-CheckR',"https://github.com/Max-Shang/SDG-CheckR"))
    ),
    
    
    mainPanel(width = 7,
              
              tabsetPanel(type = "tabs",
              tabPanel("File Checks", 
              h3(strong("1. Variable Summary")),
              p("Each row is a variable, and the table contains frequencies of valid observations, NAs, and Ns. The total is included as a check."),
              tableOutput('blankcolumns'),
              
              h3(strong("2. Column Names Check")),
              p("The table below lists the columns which do not match names in the template. Please check."),
              tableOutput('columncheck'),
              
              h3(strong("3. Required area (i.e. country, region, etc.) Check")),
              p("The table lists required areas which are not in the your file."),
              tableOutput('reqareas'),
              
              h3(strong("4. Correct area code check")),
              p("The table below shows geographic areas for which the code in the input file does not match the template."),
              tableOutput('geocodes'),
              
              h3(strong("5. Correct unit check")),
              p("The table below shows the units of measure which do not match the template."),
              tableOutput('units'),
              
              h3(strong("6. Correct nature check")),
              p("The table below shows the nature values which do not match the template."),
              tableOutput('natureT'),
              
              h3(strong("7. Check nature codes applied to Ns and NAs")),
              #p("The table below shows the nature values which do not match the template."),
              textOutput('natureNsNAs'),
              
              h3(strong("8. Source Check")),
              p("This sections checks whether non-missing values have a corresponding source"),
              tableOutput('SourceC'),
              
              h3(strong("9. Duplicate record check")),
              p("The table below shows a list of duplicated records"),
              tableOutput('duplicatesT'),
              
              
              h3(strong("10. Regional Names Check")),
              p("The table below shows which Country names do not match the tempable "),
              tableOutput('RegionName'),
              
              
              h3(strong("11. SEX check")),
              p("The table below shows whether the codes for the SEX column match the template"),
              tableOutput('cl_sex_check'),
              
              h3(strong("12. Series I.D and Code Checks ")),
              p("The tables below show whether Series ID or Code are missing, and whether more than one ID/Code is included"),
              tableOutput('series_id_code'),
              
              # 
              h3(strong("13. Regional Averages Check")),
              p("The table below shows which regional averages are included in the file but at least 50% of the countries in the region are not included"),
              tableOutput('Regionalavgs')
              # 
    )
              )
    
    
  )
    )
  
)

