#######################################
####  Create project to log files  ####
#######################################

## This is not run by the app, but is an example of how to create the Synapse
## project that stores logged data.

library("reticulate")
synapse <- reticulate::import("synapseclient")
syn <- synapse$Synapse()
syn$login()

new_proj <- synapse$Project("mytest20191119")
new_proj_id <- syn$store(new_proj)

## Give dcctravistest CREATE, UPDATE, and READ (for testing purposes)
syn$setPermissions(
  entity = new_proj_id$properties$id,
  principalId = "3384770",
  accessType = list("CREATE", "READ", "UPDATE")
)

## Give AMP-AD_SageCuration team edit and delete access
syn$setPermissions(
  entity = new_proj_id$properties$id,
  principalId = "3346847",
  accessType = list("CREATE", "READ", "DOWNLOAD", "UPDATE", "DELETE")
)

## Give Mette additional permissions
syn$setPermissions(
  entity = new_proj_id$properties$id,
  principalId = "273995",
  accessType = list(
    "CREATE",
    "READ",
    "DOWNLOAD",
    "UPDATE",
    "DELETE",
    "CHANGE_PERMISSIONS"
  )
)

## Give AMP-AD consortium team CREATE, UPDATE, and READ
syn$setPermissions(
  entity = new_proj_id$properties$id,
  principalId = "3320424",
  accessType = list("CREATE", "READ", "UPDATE")
)
