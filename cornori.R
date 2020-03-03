install.packages("rvest")
library("rvest")
library("dplyr")
library("tidyverse")
library("plotly")
url<-"https://www.worldometers.info/coronavirus/"
cd <- url %>%
  xml2::read_html()%>%
  html_nodes(xpath ='//*[@id="main_table_countries"]')%>%
  html_table
cd <- cd[[1]]
head(cd)
View(cd)

colnames(cd)
names(cd)[1]<-"Country"
colnames()


install.packages("devtools")
devtools::install_github("RamiKrispin/coronavirus")
library(coronavirus)
data(coronavirus)
View(coronavirus)
library("dplyr")
library("tidyverse")
library("gglplot")
library("ggplot2")
library("plotly")

cvd <- coronavirus %>% 
  dplyr::group_by(date, type) %>%
  dplyr::summarise(total_case = sum(cases)) %>%
  tidyr::pivot_wider(names_from = type,
                     values_from = total_case) %>%
  dplyr::arrange(confirmed) %>%
  
  
  ggplot()+geom_bar(data=coronavirus,aes(x=date,fill=type))




Corona_scatter<-coronavirus %>%
  dplyr::mutate(country = dplyr::if_else(Country.Region == "Mainland China", 
                                         "China", 
                                         "Rest")) %>%
  dplyr::group_by(date, country) %>%
  dplyr::summarise(total = sum(cases)) %>% 
  dplyr::ungroup() %>%
  tidyr::pivot_wider(names_from = country, values_from = total) 

a <-plot_ly(Corona_scatter,x=~date,y=~China,name ='China', type='scatter', mode='lines',stackgroup = 'one', 
          groupnorm = 'percent', line = list(color = 'red'))
a<- a%>%add_trace(y=~Rest, name="other countries",mode='lines',line = list(color = 'green'))
a<- a%>%layout(title = "Impact of Coronavirus - China vs other countries",
              yaxis = list(title = "Proportion of New Cases", ticksuffix = '%'),
              xaxis = list(title = "Date"))
print(a)

coronavirus %>% 
  filter(date == max(date))%>%
  select(Country.Region, type, cases) %>%
  group_by(Country.Region, type)%>%
  summarise(total = sum(cases, na.rm = TRUE))%>%
  pivot_wider(names_from = type,
              values_from = total)%>%
  arrange(total)

ggplot(data=coronavirus,aes(date,cases,color=type))+
  geom_line(size=2)+
  geom_point(size=2)





 







