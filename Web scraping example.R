# An example of a web scraping code
library(rvest)
library(RCurl)
countryNames <- c("Argentina","Bolivia","Brasil","Chile","Colombia","Costa Rica",
                  "Rep??blica Dominicana","Ecuadro","Guatemala","Honduras","Mexico",
                  "Nicaragua","Panama","Paraguay","Peru","Salvador","Uruguay",
                  "Venezuela")

countryEnd <- c("ar","bo","br","cl","co","cr","do","ec","gt","hn","mx","ni","pa",
                "py","pe","sv","uy","ve")

productList <- c("Iphone 7","PS4","Converse","Biblia")

urlProducts <- c("/iphone/iphone-7_Desde_","/ps4-playstation-consolas_Desde_","/converse-tenis_Desde_",
                 "/biblia-libros_Desde_")


vectorproducts<-c()
vectorprice<-c()
vectorsold<-c()
vectorproduct<-c()
country<-c()

for(a in 1:length(countryEnd) ){
  
  for (b in 1:length(urlProducts)) {
    pagina <- 1
    
    repeat { 
      
      if(countryEnd[a]=="br"){
        completeurl <- paste0("https://lista.mercadolivre.com.",countryEnd[a],urlProducts[b],pagina)
        
      }else if(countryEnd[a]=="cl"){
        completeurl <- paste0("https://listado.mercadolibre.",countryEnd[a],urlProducts[b],pagina)
        
      }else if(countryEnd[a]=="cr"){
        completeurl <- paste0("https://listado.mercadolibre.co.",countryEnd[a],urlProducts[b],pagina)
        
      }else{
        completeurl <- paste0("https://listado.mercadolibre.com.",countryEnd[a],urlProducts[b],pagina)
      }
    
      
      if (url.exists(completeurl)){ 
        web <-read_html(completeurl)
        products_html <- html_nodes(web,'.list-view-item-title')
        products <- html_text(products_html)
        price_html <- html_nodes(web,'.price__fraction')
        pricetext_html <- html_nodes(web,'.price__text')
        sold_html <- html_nodes(web,'.item__condition')
        price <- c(html_text(price_html),html_text(pricetext_html))
        sold <- html_text(sold_html)
        
       
        vectorproducts <-c(vectorproducts,products)
        vectorprice<-c(vectorprice,price)
        vectorsold<-c(vectorsold,sold)
        for (c in products) {
          country<-c(country,countryNames[a])
          vectorproduct<-c(vectorproduct,productList[b])
        }
        
        df <- data.frame(vectorproducts,vectorprice,vectorsold,vectorproduct,country)
        
        pagina<-pagina+50
      }
      else{break}
    }
  }
}
fileName<-paste("Scrap mercadolibre",as.character.Date(Sys.time()),".csv")
write.csv(df, file = fileName)
