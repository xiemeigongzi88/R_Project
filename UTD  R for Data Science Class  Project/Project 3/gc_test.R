getwd()
setwd("C:\\Users\\sxw17\\Desktop\\2019 Spring\\R\\HW\\3")

options(googleAuthR.scopes.selected = "https://www.googleapis.com/auth/cloud-platform")

# installed lib from GitHub for this (successfully loaded)
library(googleComputeEngineR)


project<- "TextClassificationsubmitRcode"
zone<- "us-west2-a"

account_key<-"My First Project-e10bd8528860.json"

Sys.setenv(GCE_AUTH_FILE=account_key,
           GCE_DEFAULT_PROJECT_ID=project,
           GCE_DEFAULT_ZONE=zone)


gce_auth()
library(googleComputeEngineR)


gce_get_project()