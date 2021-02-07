
library(rvest)
library(tidyverse)


# 1: пишем  функцию, которая собирает данные по одной странице
Coursera_Page <- function(url){
  
  page <- read_html(url)
  course <- page %>% html_nodes(".tab-contents") %>% html_nodes("ul") %>%html_nodes("h2")%>% html_text()
  university <- page %>% html_nodes(".tab-contents") %>% html_nodes("ul") %>% html_nodes(".partner-name")%>% html_text()
  
  type  <- page %>% html_nodes(".tab-contents") %>%html_nodes("ul") %>% html_nodes(".horizontal-box")%>%html_nodes("div")%>%html_text()
  type <- type[seq(1, length(type), 3)]
  type <- type %>% as.factor()
  
  
  students <- page %>% html_nodes(".tab-contents") %>% html_nodes("ul") %>% html_nodes(".enrollment-number")%>% html_text() %>% 
    str_replace_all("m", "000000") %>% str_replace_all("\\.", "") %>% str_replace_all("k", "000") %>% as.numeric()

  
  rate <- page %>% html_nodes(".tab-contents") %>% html_nodes("ul") %>% html_nodes(".ratings-text") %>% html_text() %>% as.numeric()
  level <- page%>% html_nodes(".tab-contents") %>% html_nodes("ul") %>%html_nodes(".difficulty")%>% html_text()%>%as.factor()
  
  
#использую аналог функции cbind с github
  source('Cbind.R')
  qdat <- Cbind(course, university, type, students, rate, level)
  return(qdat)
  
}

# пишем функцию, которая будет соединять пять листов с информацией в единый дата-фрейм

Coursera_Func <- function(x){
  res <- strsplit(x, split = " ")
  input_preparation <-  paste(unlist(res), collapse='+')
  link <- paste0("https://www.coursera.org/search?query=", input_preparation, collapse = NULL)
  #page <- read_html(link)
  
  p1 <- Coursera_Page(paste0(link, '&page=1&index=prod_all_products_term_optimization', collapse = NULL))
  p2 <- Coursera_Page(paste0(link, '&page=2&index=prod_all_products_term_optimization', collapse = NULL))
  p3 <- Coursera_Page(paste0(link, '&page=3&index=prod_all_products_term_optimization', collapse = NULL))
  p4 <- Coursera_Page(paste0(link, '&page=4&index=prod_all_products_term_optimization', collapse = NULL))
  p5 <- Coursera_Page(paste0(link, '&page=5&index=prod_all_products_term_optimization', collapse = NULL))
  
  
  all_coursera <- rbind(p1,p2,p3,p4,p5)
  
  all_coursera <- all_coursera %>% filter(type == "COURSE" | type == "DEGREE") 
  return(all_coursera)
}


# интерфейс для пользователя: вводим интересуемый курс


user_input <- readline(prompt = "Enter course: ")
a<- Coursera_Func(user_input)

#2: считаем корреляцию 

t2 <- na.omit(Coursera_Func('web design'))
cor(t2$students, t2$rate)

t2 <- Coursera_Func('web design')




