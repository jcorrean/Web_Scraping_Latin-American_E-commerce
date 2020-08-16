library(rvest)

countryNames <- c("Argentina","Bolivia")

countryEnd <- c("ar","bo")

productList <- c("Iphone 7","PS4","Converse","Biblia")

urlProducts <- c("/iphone/iphone-7_Desde_","/ps4-playstation-consolas_Desde_",
                 "/converse-tenis_Desde_","/biblia-libros_Desde_")


vectorproducts<-c()
vectorprice<-c()
vectorsold<-c()
vectorproduct<-c()
country<-c()
vectorquestions<-c()
goodfeedback<-c()
neutralfeedback<-c()
badfeedback<-c()
experience<-c()

for(a in 1:length(countryEnd) ){
  
  for (b in 1:length(urlProducts)) {
    pagina <- 1
    
    repeat { 
      
      if(countryEnd[a]=="br"){
        completeurl <- paste0("https://lista.mercadolivre.com.",countryEnd[a],urlProducts[b],pagina,"_DisplayType_LF")
        
      }else if(countryEnd[a]=="cl"){
        completeurl <- paste0("https://listado.mercadolibre.",countryEnd[a],urlProducts[b],pagina,"_DisplayType_LF")
        
      }else if(countryEnd[a]=="cr"){
        completeurl <- paste0("https://listado.mercadolibre.co.",countryEnd[a],urlProducts[b],pagina,"_DisplayType_LF")
        
      }else{
        completeurl <- paste0("https://listado.mercadolibre.com.",countryEnd[a],urlProducts[b],pagina,"_DisplayType_LF")
      }
      
      flag0<-""
      flag0<-tryCatch(
        {
          read_html(completeurl)
        },
        error=function(cond) {
          return("a")
        }
        
      )
    
      
      if (flag0!="a"){ 
        web <-read_html(completeurl)
        products_html <- html_nodes(web,'.list-view-item-title')
        products <- html_text(products_html)
        price_html <- html_nodes(web,'.price__fraction')
        pricetext_html <- html_nodes(web,'.price__text')
        sold_html <- html_nodes(web,'.item__condition')
        price <- c(html_text(price_html),html_text(pricetext_html))
        sold <- html_text(sold_html)
        urlList<-web %>% html_nodes(".item__info-title") %>% html_attr('href')
        print(completeurl)
       
        vectorproducts <-c(vectorproducts,products)
        vectorprice<-c(vectorprice,price)
        vectorsold<-c(vectorsold,sold)
        
        for (c in 1:length(products)) {
          country<-c(country,countryNames[a])
          vectorproduct<-c(vectorproduct,productList[b])
          product<-read_html(urlList[c])
          questions<-html_nodes(product,".questions__item--question p")
          vectorquestions<-c(vectorquestions,length(questions))
          print(urlList[c])
          
          urlprofile<-product %>% html_nodes(".card-block-link")%>% html_attr('href')
          print(urlprofile)
          
          getfeedback<-function(urlprofile0){
            reputation<-read_html(urlprofile0)
            
            good<-html_nodes(reputation,".buyers-feedback-bar--positive .buyers-feedback-qualification")
            neutral<-html_nodes(reputation,".buyers-feedback-bar--neutral .buyers-feedback-qualification")
            bad<-html_nodes(reputation,".buyers-feedback-bar--negative .buyers-feedback-qualification")
            exp<-html_nodes(reputation,".experience")
            
            if (length(good)==0) {
              points<-html_nodes(reputation, ".reputation-filters b")
              exp1<-html_nodes(reputation,"strong")
              
              goodfeedback<-html_text(points[1])
              neutralfeedback<-html_text(points[2])
              badfeedback<-html_text(points[3])
              if (length(exp1)==0|length(exp1)<6){
                experience<-"N/A"
              }else{
                experience<-html_text(exp1[6])
              }
              
              if(length(goodfeedback)==0){
                print("Perfil no Existe")
                goodfeedback<-"N/A"
                neutralfeedback<-"N/A"
                badfeedback<-"N/A"
                experience<-"N/A"
              }
              
            }else{
              goodfeedback<-html_text(good)
              neutralfeedback<-html_text(neutral)
              badfeedback<-html_text(bad)
              experience<-html_text(exp)
              
              
            }
            feedback<-c(goodfeedback,neutralfeedback,badfeedback,experience)
            return(feedback)
          }
          
          flag1<-""
          flag1<-tryCatch(
            {
              read_html(urlprofile)
            },
            error=function(cond) {
              return("w")
            })
          
          if( length(urlprofile)==0){
            print("no hay perfil")
            goodfeedback<-c(goodfeedback,"ventas insuficientes para calcular reputacion")
            neutralfeedback<-c(neutralfeedback,"ventas insuficientes para calcular reputacion")
            badfeedback<-c(badfeedback,"ventas insuficientes para calcular reputacion")
            experience<-c(experience,"N/A")
          }else if( flag1 != "w" ){
            x=getfeedback(urlprofile)
            goodfeedback<-c(goodfeedback,x[1])
            neutralfeedback<-c(neutralfeedback,x[2])
            badfeedback<-c(badfeedback,x[3])
            experience<-c(experience,x[4])
          }else if(flag1=="w"){
            w<- product %>% html_node(".vip-section-seller-info .ui-view-more__link") %>% html_attr('href')
            
            switch (countryEnd[a],
                    ar = {newurl<-paste0("https://articulo.mercadolibre.com.ar",w)},
                    bo = {newurl<-paste0("https://articulo.mercadolibre.com.bo",w)},
            )
            
            print(newurl)
            flag2<-""
            flag2<-tryCatch(
              {
                read_html(newurl)
              },
              error=function(cond) {
                return("y")
              })
            
            if(flag2!="y"){
              urlprofile<- read_html(newurl) %>% html_node(".feedback-profile-link a") %>% html_attr('href')
              print(urlprofile)
              x=getfeedback(urlprofile)
              goodfeedback<-c(goodfeedback,x[1])
              neutralfeedback<-c(neutralfeedback,x[2])
              badfeedback<-c(badfeedback,x[3])
              experience<-c(experience,x[4])
            }else{
              print("Perfil no Existe")
              goodfeedback<-c(goodfeedback,"N/A")
              neutralfeedback<-c(neutralfeedback,"N/A")
              badfeedback<-c(badfeedback,"N/A")
              experience<-c(experience,"N/A")
            }
          }
          
        }
        
        df <- data.frame(vectorproducts,vectorprice,vectorsold,
                         vectorproduct,country,vectorquestions,
                         goodfeedback,neutralfeedback,badfeedback,
                         experience)
        
        pagina<-pagina+50
      }
      else{break}
    }
  }
}
fileName<-paste0("Scrap mercadolibre",as.character.Date(Sys.time()),".csv")
write.csv(df, file = fileName)
