app <- ShinyDriver$new("../../")
app$snapshotInit("initial-load")

app$waitFor("false", timeout = 5000)
app$snapshot()
