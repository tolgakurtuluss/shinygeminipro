source("library.R")
source("functions.R")

ui <- fluidPage(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
        tags$link(rel = "stylesheet", href = "https://use.fontawesome.com/releases/v5.15.4/css/all.css")
    ),
    
    div(class = "custom-header",
        h1("Google Gemini Pro - Text & Vision Chat", style = "margin: 0;"),
        p("An elegant interface for exploring Google's Gemini multimodal model", style = "margin-top: 10px;")
    ),
    
    tabsetPanel(
        tabPanel("README",
                fluidRow(
                    column(10, offset = 1,
                           div(class = "feature-card",
                               h2(icon("info-circle"), "About"),
                               p("ShinyGeminiPro is an elegant interface for exploring Google's Gemini multimodal conversational model in an open-source R-based environment."),
                               div(class = "github-button",
                                   a(href = "https://github.com/tolgakurtuluss/shinygeminipro",
                                     target = "_blank",
                                     icon("github"), "View on GitHub")
                               )
                           ),
                           
                           div(class = "feature-card",
                               h2(icon("star"), "Key Features"),
                               tags$ul(
                                   tags$li(icon("keyboard"), "Advanced text input capabilities"),
                                   tags$li(icon("image"), "Seamless image upload and processing"),
                                   tags$li(icon("robot"), "State-of-the-art AI response generation"),
                                   tags$li(icon("history"), "Interactive chat history")
                               )
                           )
                    )
                )
        ),
        
        tabPanel("Chat Interface",
                sidebarLayout(
                    sidebarPanel(
                        div(class = "sidebar-panel",
                            textInput("apikey", "API Key", ""),
                            tags$small(
                                "Get your API key at ",
                                a(href = "https://ai.google.dev/", target = "_blank", "Google AI Studio")
                            ),
                            hr(),
                            textAreaInput("userInput", "Your Message", rows = 4,
                                        placeholder = "Type your message here..."),
                            fileInput("imgInput", "Upload Image",
                                    accept = c('image/png', 'image/jpeg')),
                            actionButton("generateBtn", "Generate Response",
                                       class = "btn-primary btn-lg btn-block",
                                       icon = icon("magic"))
                        )
                    ),
                    mainPanel(
                        div(class = "feature-card",
                            withSpinner(DT::dataTableOutput("chatOutput"))
                        )
                    )
                )
        ),
        
        tabPanel("Contact",
                div(class = "contact-section",
                    div(class = "feature-card",
                        h2(icon("envelope"), "Get in Touch"),
                        tags$ul(class = "list-unstyled",
                               tags$li(icon("envelope"), 
                                      a(href = "mailto:tolgakurtulus95@gmail.com", "tolgakurtulus95@gmail.com")),
                               tags$li(icon("twitter"),
                                      a(href = "https://twitter.com/tolgaakurtuluss", "@tolgaakurtuluss")),
                               tags$li(icon("github"),
                                      a(href = "https://github.com/tolgakurtuluss", "GitHub Profile"))
                        )
                    ),
                    
                    div(class = "feature-card",
                        h3(icon("star"), "Special Offer!"),
                        p("Get a FREE API key until January 1st, 2024!",
                          "Star the repository and contact us to claim your key.")
                    )
                )
        )
    )
)

server <- function(input, output, session) {
  chat_history <- reactiveVal(data.frame(Role = character(0), Message = character(0), stringsAsFactors = FALSE))
  
  observeEvent(input$generateBtn, {
    headers <- c(`Content-Type` = "application/json")
    params <- list(`key` = input$apikey)
    api_key = input$apikey
    
    user_text <- input$userInput
    
    if (is.null(input$imgInput)) {
      response <- generateContent(user_text,api_key = api_key)
      response = ifelse(is.null(response),"An error occured please refresh page.",response)
      chat_history(rbind(chat_history(), data.frame(Role = "User", Message = user_text)))
    }else{
      imgpath = input$imgInput$datapath
      response <- generate_content_vision(user_text,imgpath,api_key = api_key)
      response = ifelse(is.null(response),"An error occured please refresh page.",response)
      imglink=gsub("\\\\","/",imgpath)
      base64 <- dataURI(file = imglink, mime = imglink)
      chat_history(rbind(chat_history(), data.frame(Role = "User", Message = paste(user_text,"- ???? attached! - ",tags$img(src = base64, alt= "error", width = 45, height = 45)))))
    }
    
    chat_history(rbind(chat_history(), data.frame(Role = "Model", Message = response)))
    
    output$chatOutput <- renderDataTable({
      DT::datatable(
        chat_history(),
        options = list(
          paging = FALSE,
          searching = FALSE
        ),
        rownames = FALSE,
        class = 'cell-border stripe',
        escape = FALSE
      )
    })
    
    updateTextInput(session, "userInput", value = "")
    
  })
}

shinyApp(ui, server)