library(AzureStor)
library(jsonlite)
library(janitor)
library(nhsbsaR)
library(dplyr)

config <- jsonlite::fromJSON("./data-raw/execution.json")

account_name <- Sys.getenv('AZURE_BLOB_STORAGE_PDS_ACCOUNT_NAME')
account_key <- Sys.getenv('AZURE_BLOB_STORAGE_PDS_ACCOUNT_KEY')
endpoint_suffix <- "core.windows.net"

container_name <- config$import$container
folder_path <- config$import$folder


# Create a blob service client
blob_endpoint <- sprintf('https://%s.blob.%s', account_name, endpoint_suffix)
blob_client <- storage_endpoint(blob_endpoint, key=account_key)

# Get the blob client instance for the given container
blob_container <- storage_container(blob_client, container_name)

# List all blobs in the specified folder
blobs_in_folder <- list_blobs(blob_container, prefix=folder_path)

# Extract the 'name' values from the result
blob_names <- blobs_in_folder[["name"]]

# Loop through each blob in the folder and download
for (blob_name in blob_names) {
  
  local_file_path <- paste0(config$import$local_path, '/', basename(blob_name))
  
  # Check if the file exists locally, and if so, delete it
  if (file.exists(local_file_path)) {
    file.remove(local_file_path)
  }
  
  # Download the blob data to the local file
  storage_download(blob_container, blob_name, local_file_path, overwrite = TRUE)
}
