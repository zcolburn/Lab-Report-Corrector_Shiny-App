# Load packages
library(shiny)
library(dplyr)

# Load error identification data
load("manualIdentifiers.r")

# Generate error identification regular expressions
splitDelimitersOne <- lapply(poorPhrasesDocument[,"phrase"], function(item){
  paste0("(?<=",item,")")
}) %>% paste(., collapse = "|")
splitDelimitersTwo <- lapply(poorPhrasesDocument[,"phrase"], function(item){
  paste0("(?=",item,")")
}) %>% paste0(., collapse = "|")

# Generate HTML output formatting objects
normalSpanFront <- "<span>"
spanEnd <- "</span>"
badSpanFrontPartOne <- "<span onclick='showProblem("
badSpanFrontPartThree <- ")' class='badPhrase'>"
badSpanFrontPartFour <- ")' class='badPhrase currentlySelectedBadPhrase'>"
betweenIssueMarker <- "<br><br>"

issueTitleLead <- "<span class='issueTitle'>"
issueTitleEnd <- "</span>"
issueNumberSpan <- "<span class='issueNumber'>"
issueNumberSpanEnd <- ". </span>"
solutionStart <- "<br><span class='solution'>"
solutionEnd <- "</span>"


previousIssueFieldOutput <- "<span></span>"

# Server logic
shinyServer(function(session, input, output) {
  # Set UI
  renderUI({getPage(includeHTML("www/index.html"))})
  
  
  
  
  # Input tab 
  #-------------------------------------
  # Set empty objects that will be modified pending input
  finalOutput <- ""
  textCharList <- list()
  outputTextList <- list()
  colorDeterminer <- list()
  processedInputText <- list()
  currentInput <- ""
  
  # Function to set input data when user enters text in input field
  getInput <- reactive({
    input$inputField
  })
  
  # When user inputs text, update the output field
  observeEvent(input$inputField, {
    # Retrieve input
    inputText <- getInput()

    output$outputField <- renderText({
      # Handle the no text condition
      if (nchar(inputText) == 0) return("")
      currentInput <<- inputText
      
      # Find bad phrases
      listText <- rep(list(c()), nchar(inputText))
      listChars <- strsplit(inputText, "") %>% unlist()
      # For each bad phrase...
      lapply(1:nrow(poorPhrasesDocument), function(delimiterNumber){
        # Find the locations of each bad phrase
        delimiterPhrase <- poorPhrasesDocument[delimiterNumber, "phrase"]
        temp <- gregexpr(delimiterPhrase, inputText, ignore.case = !poorPhrasesDocument[delimiterNumber, "mindCase"])[[1]]
        tempDataFrame <- data.frame(
          "matchPosition" = as.vector(temp),
          "matchLength" = attr(temp, "match.length")
        )
        # Identify the solution(s) for this issue
        currentIssueId <- poorPhrasesDocument[delimiterNumber, "issueIds"]
        for (matchNumber in 1:nrow(tempDataFrame)) {
          matchPosition <- tempDataFrame[matchNumber,"matchPosition"]
          matchLength <- tempDataFrame[matchNumber,"matchLength"]
          if (matchLength == -1) next
          positionMatches <- matchPosition:(matchPosition + matchLength - 1)
          lapply(positionMatches, function(index){
            listText[[index]] <<- c(listText[[index]], currentIssueId)
          })
        }
      })
      textCharList <<- listText
      
      # Determine the number of problems
      problemText <- input$problemNumber
      if (!exists("problemText")) {
        problemText <- 0
      } else if (length(problemText) == 0) {
        problemText <- 0
      }
      probNum <- as.numeric(problemText)
      
      # Determine text colors
      if (probNum == 0) {
        colorDeterminer <<- rep(FALSE, length(textCharList))
      } else {
        currentIssues <- textCharList[[probNum]]
        colorDeterminer <<- lapply(1:length(textCharList), function(currentItem){
          if (is.null(textCharList[[currentItem]])[1]) return(FALSE)
          currentValue <- textCharList[[currentItem]]
          if (any(currentValue %in% currentIssues)) return(TRUE)
          return(FALSE)
        }) %>% unlist()
      }
      
      # Set text colors
      outputTextList <<- lapply(1:length(listChars), function(index){
        if (length(listText[[index]]) == 0) {
          return(paste0(normalSpanFront, listChars[[index]], spanEnd))
        }
        if (!colorDeterminer[index]) return(paste0(badSpanFrontPartOne, index, badSpanFrontPartThree, listChars[[index]], spanEnd))
        return(paste0(badSpanFrontPartOne, index, badSpanFrontPartFour, listChars[[index]], spanEnd))
      })
      
      outputText <- outputTextList %>% unlist() %>% paste0(., collapse = "")
      
      return(outputText)
    })
  })

  
  # Set feedback HTML
  output$issueField <- renderText({
    # Identify problem number
    problemText <- input$problemNumber
    probNum <- as.numeric(problemText)
    
    # Identify the number of problems
    probNumLength <- length(probNum)
    if (probNumLength != 1) return(finalOutput)
    
    # Determine current issue
    issueNumber <- textCharList[[probNum]] %>% unique()
    allIssues <- issueIds[issueNumber] %>% unlist() %>% unique()
    
    # Determine solutions
    solutionIds <- issueIdToSolutionMap[issueIdToSolutionMap[,"issueId"] %in% allIssues,"solutionId"]
    allSolutions <- solutions[solutionIds]
    
    # Retrieve and format solution HTML
    unlistedOutput <- lapply(1:length(allSolutions), function(currentSolutionNumber){
      solutionData <- allSolutions[[currentSolutionNumber]]
      title <- solutionData$title
      solution <- solutionData$solution
      return(
        paste0(issueNumberSpan, currentSolutionNumber,issueNumberSpanEnd, issueTitleLead, title, issueTitleEnd, solutionStart, solution, solutionEnd)
      )
    }) %>% unlist() %>% as.vector()
    
    # Merge all solution feedback into a single HTML block
    finalOutput <- paste0(unlistedOutput, collapse = betweenIssueMarker)
    finalOutput <<- finalOutput
    
    return(finalOutput)
  })
  
})
