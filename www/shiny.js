// Synchronize scroll position of input and output fields
var isSyncingInputScroll = false;
var isSyncingOutputScroll = false;
var inputField = document.getElementById('inputField');
var outputField = document.getElementById('outputField');








// Set problem number in order to retrieve and show appropriate feedback
var problemNumber = 0;
function showProblem(problemNumberInput) {
  problemNumber = problemNumberInput;
  var problemNumberOutput = problemNumber;
  Shiny.onInputChange("problemNumber", problemNumberOutput);
}





$(document).ready(function() {
  // Synchronize field scrolling in the input tab
  $("#inputField").on("scroll", function() {
    if (!isSyncingInputScroll) {
      isSyncingOutputScroll = true;
      $("#outputField").scrollTop($("#inputField").scrollTop());
    }
    isSyncingInputScroll = false;
  });

  $("#outputField").on("scroll", function() {
    if (!isSyncingOutputScroll) {
      isSyncingInputScroll = true;
      $("#inputField").scrollTop($("#outputField").scrollTop());
    }
    isSyncingOutputScroll = false;
  });

  //About tab






  //Tab switching
  $(".mainTabs").hide();
  $("#inputTab").show();
  $("#navTabInputButton").click(function(){
    $(".mainTabs").hide();
    $("#inputTab").show();
  });
  $("#navTabAboutButton").click(function(){
    $(".mainTabs").hide();
    $("#aboutTab").show();
  });




});
