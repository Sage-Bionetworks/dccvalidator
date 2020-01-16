app <- ShinyDriver$new("../../")
app$snapshotInit("open_tabs")

app$waitFor("false", timeout = 5000)
app$snapshot()
app$snapshot()
