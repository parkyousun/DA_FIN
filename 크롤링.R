```{r}
library(KoNLP)
library(rJava)
library(multilinguer)
library(rvest)

base <- "https://search.naver.com/search.naver?sm=tab_hty.top&where=news&query=%EC%95%84%ED%8C%8C%ED%8A%B8%2B%EC%A0%95%EC%B1%85&oquery=%EC%95%84%ED%8C%8C%ED%8A%B8%EC%A0%95%EC%B1%85&tqi=hK9mDsprvxZssPQu4flssssstO4-016&start="

urls <- NULL
for(x in 0:9){
  urls <- c(urls, paste(base,x*10+1,sep=''))
}
urls
html <- read_html(urls[1])
html

urls[1] %>%
  read_html() %>%
  html_nodes('#sp_nws1 > div.news_wrap.api_ani_send > div > div.news_info > div.info_group > a') %>%
  html_attr('href')

urls[1] %>%
  read_html() %>%
  html_nodes('#sp_nws1 > div > div > div > div > a') %>%
  html_attr('href')

news_link <- NULL

for(url in urls){
  html <- read_html(url)
  news_link <- c(news_link, html %>%
                   html_nodes('#sp_nws1 > div.news_wrap.api_ani_send > div > div.news_info > div.info_group > a') %>%
                   html_attr('href')
  )
  encoding = "EUC-KR"}
news_link

contents <- NULL
title <- NULL

for(link in news_link){
  html <- read_html(link)
  contents <- c(contents, html %>%
                  html_nodes('._article_body_contents')%>%
                  html_text())
  title <- c(title, html %>%
               html_nodes('#articleTitle') %>%
               html_text())
}


contents_df <- as.data.frame(contents)
title_df <- as.data.frame(title)
news_final <- cbind(title_df, contents_df)
head(news_final)

content <- gsub("[\r\n\t]", ' ', contents)
content <- gsub("[[:punct:]]",' ', content)
content <- gsub("[[:cntrl:]]", ' ', content)
content <- gsub("\\d+", ' ', content)
content <- gsub("[a-z]",' ', content)
content <- gsub("[A-Z]", ' ', content)
content <- gsub("\\s+",' ', content)
news_final
```