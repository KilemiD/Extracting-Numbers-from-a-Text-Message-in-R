#the cleaning process
#libraries
setwd("C:/Users/John/OneDrive/whatsapp study")
library(rwhatsapp)
library(dplyr)
library(tidyr)
chat=rwa_read("WhatsApp Chat with kplc (4).txt")%>%
  filter(!is.na(author))
View(chat)
#getting only the text column
token=chat%>%
  select(text)
View(token)
#removing other parts of the chat without kplc
#and remaining with ones that have actual tokens
token_clean=token%>%
  filter(grepl('KPLC',text))
#separating the texts using some rule, in this case (:)
#first specify the column names
token3=token_clean%>%
  separate(text,c("info","mtr","token","date","hour","units","amt","amt2","token amt"),
           ":",extra="merge")
View(token3)
#remove the text accompanying the separation you have just made
token3$token=gsub("Date","",token3$token)
token3$mtr=gsub("Token","",token3$mtr)
token3$hour=gsub("Units","",token3$hour)
token3$units=gsub("Amt Ksh","", token3$units)
token3$amt=gsub("Token Amt","", token3$amt)
token3$amt2=gsub("VAT","", token3$amt2)
token3$info=gsub("MtrNo","",token3$info)
View(token3)

#some tokens do not have the hyphen, remove that
token4=token3%>%
  filter(grepl('-',token))
#some tokens have spaces before the start of token
#remove that
token4$token=gsub(" ","",token4$token)
#token4$token=gsub(" UnitskWh","",token4$token)

token4=token4%>%
  filter(!grepl('Units',token))
#your interest is the 20 digit code for tokens
#so get only that from your data
#we will use the other variables as well but later
token_digits=strsplit(token4$token,"")  
View(token_digits)
#you want a data frame
digits2=data.frame(t(sapply(token_digits,`[`)))
View(digits2)
#time to remove the hyphens from the data frame
digits3=digits2%>%
  select(-c(X5,X10,X15,X20,X25))

#now rename the columns
colnames(digits3)=c("one","two","three","four","five","six","seven",
                    "eight","nine","ten","eleven","twelve","thirteen",
                    "fourteen","fifteen","sixteen","seventeen",
                    "eighteen","nineteen",
                    "twenty")
#now that you have your data set, check if it is numeric
View(digits3)

str(digits3)
#now convert to numeric
digits3_num=data.frame(lapply(digits3,as.numeric))