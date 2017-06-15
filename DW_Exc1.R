library(readxl)
library(dplyr)

#Load the Data into R
refine_Original <- read_excel("C:/Users/keert/Desktop/DS-Spring Board/Data Wrangling/DW Exc1/refine_Original.xlsx")

#Clean up brand names
refine_Original$company <- gsub(".*s$","philips",refine_Original$company,ignore.case = T)
refine_Original$company <- gsub("^ak.*","akzo",refine_Original$company,ignore.case = T)
refine_Original$company <- gsub(".*en$","van houten",refine_Original$company,ignore.case = T)
refine_Original$company <- gsub(".*er$","unilever",refine_Original$company,ignore.case = T)

#Separate product code and number
refine_Original <- separate(refine_Original,`Product code / number`,c("Product_Code","Product_Number"), sep ="-")

#Add product categories
Category <- function(x){
  if(x=="p"){
    return("Smartphone")
  }else if(x=="x"){
    return("Laptop")
  }else if(x=="v"){
    return("TV")
  }else if(x=="q"){
    return("Tablet")
  }
}

refine_Original$Product_Category <- sapply(refine_Original$Product_Code,Category,USE.NAMES = FALSE)

#Add full address for geocoding
refine_Original$Full_Address <- paste(refine_Original$address,refine_Original$city,refine_Original$country,sep=",")


#Create dummy variables for company and product category
company_philips <- function(x2){
  
    if(x2=="philips"){
    return(1)
  }else {
    return(0)
  }
}
company_akzo <- function(x2){
   if(x2=="akzo"){
    return(1)
  }else {
    return(0)
  }
}
company_vanhouten <- function(x2){
  if(x2=="van houten"){
    return(1)
  }else {
    return(0)
  }
} 
company_unilever <- function(x2){
  if(x2=="unilever"){
    return(1)
  }else {
    return(0)
  }
}  

refine_Original$company_philips <- sapply(refine_Original$company,company_philips)
refine_Original$company_akzo <- sapply(refine_Original$company,company_akzo)
refine_Original$company_vanhouten <- sapply(refine_Original$company,company_vanhouten)
refine_Original$company_unilever <- sapply(refine_Original$company,company_unilever)


