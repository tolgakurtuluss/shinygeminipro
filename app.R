source("library.R")
source("functions.R")

ui <- fluidPage(
  titlePanel("Google Gemini Pro - Text & Vision Chat 🔮 - ShinyGeminiPro App"),
  tabsetPanel(
    tabPanel("README",
             fluidPage(
               titlePanel("👋 Hello there!"),
               tags$h3("ℹ️ About"),
               tags$ul(
                 tags$li("🌐 This is ShinyGeminiPro app for exploring the capabilities of Google's Gemini multimodal conversational model in an open-source R-based environment."),
                 tags$li("🌟 This app is entirely free and open-sourced. Don't forget to give it a star and support open-source!"),
                 tags$div(
                   style="text-align:center; margin-top: 15px; color: white; background-color: #FFFFFF",
                   a(href="https://github.com/tolgakurtuluss/shinygeminipro", target="_blank",
                     img(src="https://github.githubassets.com/images/modules/logos_page/GitHub-Mark.png", height="30px"),
                     "View repository on Github"
                   )
                 )

               ),
               tags$h3("🚀 Features"),
               tags$ol(
                 tags$li("💬 Text Input: Enter user prompts and queries in the \"User Input\" text area."),
                 tags$li("📷 Image Upload: Optionally, upload images along with text for a multimodal conversational experience."),
                 tags$li("🤖 Response Generation: Utilize the Gemini API to generate responses based on the provided user input and images."),
                 tags$li("📜 Chat History: View the conversation history between the user and the Gemini model in a tabular format.")
               )
             )
    ),
    tabPanel("Try Gemini Pro (Text & Vision Model)",
             sidebarLayout(
               sidebarPanel(
                 textInput("apikey", "API Key", "placeyourapikey"),
                 tags$p("Find your own Google GeminiPro API:",
                        tags$a(href = "https://ai.google.dev/", target="_blank", "https://ai.google.dev/?hl=tr")
                 ),tags$hr(),
                 textAreaInput("userInput", "User Input", placeholder ="You can use your own API or visit CONTACT Tab!", rows = 3),
                 fileInput("imgInput", "Upload Image", accept = c('image/png', 'image/jpeg')),
                 tags$hr(),
                 actionButton("generateBtn", "Generate Response🔮"),
                 tags$p("*Responses with GeminiPro Vision model may take up to 10-15 seconds.")
               ),
               mainPanel(
                 DT::dataTableOutput("chatOutput")
               )
             )
    ),
    tabPanel("CONTACT",
             tags$h3("For inquiries and support, feel free to contact me."),
             tags$p("📬 Email: ", tags$a(href = "mailto:tolgakurtulus95@gmail.com?subject=About ShinyGeminiPro App", "tolgakurtulus95@gmail.com")),
             tags$p("🐦 Twitter: ", tags$a(href = "https://twitter.com/tolgaakurtuluss", "@tolgaakurtuluss")),
             tags$p("🌐 GitHub Profile: ", tags$a(href = "https://github.com/tolgakurtuluss", "https://github.com/tolgakurtuluss")),
             tags$hr(),
             style = "text-align: center; margin: 15px 0;",
             tags$h4(
               "🌟 UPDATE! - Gemini Pro API is still FREE to claim! 🚀",
               " More details about API pricing can be found on ",
               tags$a(href = "https://ai.google.dev/pricing?hl=tr", "Google AI Pricing"),
               ".",
               tags$h5(" I know some of you are eager to try this app, and guess what?",
                       " I can provide a FREE API for you to use until 01-01-2024!",
                       " Just give a star to this repo on GitHub and contact me to receive the API!")
             ),
             tags$hr(),
             tags$div(
               style="text-align:center; margin-top: 15px; color: white; background-color: #FFFFFF",
               a(href="https://github.com/tolgakurtuluss/shinygeminipro", target="_blank",
                 img(src="https://github.githubassets.com/images/modules/logos_page/GitHub-Mark.png", height="30px"),
                 "View on Github"
               )
             ),
             tags$p(tags$hr(),tags$img(height = 80, width = 120,src = "https://th.bing.com/th?id=OIF.4bJQrWkht3%2flvBJh4kOk4w&rs=1&pid=ImgDetMain"),tags$hr(),
                    "❓ Want to discover the Gemini model?",
                    tags$a(href = "https://deepmind.google/technologies/gemini/?hl=tr#introduction", target="_blank", "Gemini official page🔮")
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
      chat_history(rbind(chat_history(), data.frame(Role = "User", Message = paste(user_text,"- 🎨 attached! - ",tags$img(src = base64, alt= "error", width = 45, height = 45)))))
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
