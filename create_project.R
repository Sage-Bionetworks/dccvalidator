#######################################
####  Create project to log files  ####
#######################################

library("synapser")
synLogin()
new_proj <- Project("AMP-AD validation")
new_proj_id <- synStore(new_proj)

## Give dcctravistest CREATE, UPDATE, and READ (for testing purposes)
synSetPermissions(
  entity = new_proj_id$properties$id,
  principalId = "3384770",
  accessType = list("CREATE", "READ", "UPDATE")
)

## Give AMP-AD_SageCuration team edit and delete access
synSetPermissions(
  entity = new_proj_id$properties$id,
  principalId = "3346847",
  accessType = list("CREATE", "READ", "DOWNLOAD", "UPDATE", "DELETE")
)

## Give Mette additional permissions
synSetPermissions(
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
synSetPermissions(
  entity = new_proj_id$properties$id,
  principalId = "3320424",
  accessType = list("CREATE", "READ", "UPDATE")
)
