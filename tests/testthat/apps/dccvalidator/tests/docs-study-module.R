app <- ShinyDriver$new("../")
app$snapshotInit("docs-study-module")

app$setInputs(`documentation-doc_study-study_choice` = "DiCAD")
app$snapshot()
app$setInputs(`documentation-doc_study-study_exists` = "No")
app$setInputs(`documentation-doc_study-study_text` = "test")
app$snapshot()
