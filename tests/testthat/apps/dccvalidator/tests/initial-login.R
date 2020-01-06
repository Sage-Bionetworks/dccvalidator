app <- ShinyDriver$new("../")
app$snapshotInit("initial-login")

# Set javascript expression to false so waits until timeout
app$waitFor("false", timeout = 10000)
app$snapshot()
