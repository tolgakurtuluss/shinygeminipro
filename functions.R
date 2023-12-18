# Function to generate content using Google Gemini Pro API
generateContent <- function(prompt, api_key) {
  # Set headers
  headers <- c(
    `Content-Type` = "application/json"
  )

  # Set parameters
  params <- list(
    `key` = api_key
  )

  # Prepare data with the provided story prompt
  data <- sprintf('{"contents": [{"parts":[{"text": "%s"}]}]}', prompt)

  # Make the API request
  res <- httr::POST(
    url = "https://generativelanguage.googleapis.com/v1beta/models/gemini-pro:generateContent",
    httr::add_headers(.headers = headers),
    query = params,
    body = data
  )

  # Extract and return the generated story text
  return(content(res)$candidates[[1]]$content$parts[[1]]$text)
}

# Function to generate content with vision using Google Gemini Pro API
generate_content_vision <- function(prompt, image_source, api_key) {
  # Read image data and encode it in base64
  if (startsWith(image_source, "http")) {
    # If image_source is a URL, download the image and encode it in base64
    temp_path <- tempfile(fileext = ".jpg")
    download.file(image_source, temp_path, mode = "wb")
    image_data <- base64enc::base64encode(readBin(temp_path, "raw", file.info(temp_path)$size))
    unlink(temp_path)  # Remove the temporary file
  } else {
    # If image_source is a local file path, read the image data and encode it in base64
    image_data <- base64enc::base64encode(readBin(image_source, "raw", file.info(image_source)$size))
  }

  # Create a list structure representing the JSON content
  json_content <- list(
    contents = list(
      list(
        parts = list(
          list(text = prompt),
          list(
            inline_data = list(
              mime_type = "image/jpeg",
              data = image_data
            )
          )
        )
      )
    )
  )

  # Convert the list to JSON and write it to a file
  json_string <- toJSON(json_content, auto_unbox = TRUE, pretty = TRUE)
  writeLines(json_string, "request.json")

  # Set headers
  headers <- c(
    `Content-Type` = "application/json"
  )

  # Set parameters
  params <- list(
    `key` = api_key
  )

  # Upload the JSON file
  data <- upload_file("request.json")

  # Make the API request for vision
  res <- httr::POST(url = "https://generativelanguage.googleapis.com/v1beta/models/gemini-pro-vision:generateContent", httr::add_headers(.headers = headers), query = params, body = data)

  # Extract and return the generated text with vision
  return(content(res)$candidates[[1]]$content$parts[[1]]$text)
}
