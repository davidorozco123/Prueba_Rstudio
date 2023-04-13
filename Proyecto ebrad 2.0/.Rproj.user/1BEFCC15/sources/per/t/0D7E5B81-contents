#Librerias----
library(tidyverse) #Paquete de librerias tidy
library(tidytext) #Mineria de texto
library(dplyr) #Manipulacion de Data Frames
library(ggplot2) #Creacion de graficos
library(rtweet) #Conexion y creacion de solicitudes a la API de Twitter
library(RColorBrewer) #Paleta de colores para graficos
library(wordcloud) #Grafica de nube de palabras
library(openxlsx) #Crear y leer archivos Excel
library(lubridate) #Manipulacion de fechas y horario
library(stopwords) #Lista de palabras vacias para filtrar
library(tweetbotornot) #Analisis de deteccion de bot en rtweet
library(formattable) #Para transformar numeros a porcentajes
#Parametros de busqueda----

busqueda <- "\"Marcelo Ebrard\" OR \"Canciller Ebrard\" OR m_ebrard" #Busqueda a realizar en Twitter
fecha_inicio <- "2023-01-16 00:00:00" #Fecha y hora donde se iniciara la busqueda. Maximo 7 dias antes
fecha_final <- "2023-01-20 23:59:59" #Fecha y hora donde se terminara la busqueda
numero_tweets <- 198000 #Numero de tweets solicitados

#Creacion data frame "tweets"----

tweets <- rtweet::search_tweets(q = busqueda,
                                n = numero_tweets,
                                include_rts = TRUE,
                                type = "recent", #Tipo de tweet: "recent", "popular" o "mixed"
                                retryonratelimit = TRUE) %>%  
busqueda <- "\"Marcelo Ebrard\" OR \"Canciller Ebrard\" OR m_ebrard" #Busqueda a realizar en Twitter
fecha_inicio <- "2023-04-10 00:00:00" #Fecha y hora donde se iniciara la busqueda. Maximo 7 dias antes
fecha_final <- "2023-04-11 23:59:59" #Fecha y hora donde se terminara la busqueda
numero_tweets <- 2000 #Numero de tweets solicitados

#Respaldo del data frame "tweets"----

rtweet::write_as_csv(tweets, "tweets_data.csv")

#Deteccion de bots----

bot <- tweetbotornot::botornot(tweets, fast = TRUE)

bot$prob_bot <- formattable::percent(bot$prob_bot)

bot <- bot %>% 
  dplyr::select(screen_name,
                prob_bot)

tweets <- dplyr::full_join(tweets, bot, by = "screen_name")

#Data frame alcance de emisores por usuario----

alcance_usuario <- tweets %>% 
  dplyr::select(usuario = screen_name,
                seguidores = followers_count) %>%
  dplyr::group_by(usuario) %>%
  dplyr::summarise(publicaciones = dplyr::n(), alcance = base::sum(seguidores)) %>% 
  dplyr::arrange(desc(alcance))

openxlsx::write.xlsx(alcance_usuario, "alcance_usuario.xlsx")
  
#Data frame alcance de emisores por nombre----

alcance_nombre <- tweets %>% 
  dplyr::select(usuario = name,
                seguidores = followers_count) %>%
  dplyr::group_by(usuario) %>%
  dplyr::summarise(publicaciones = dplyr::n(), alcance = base::sum(seguidores)) %>% 
  dplyr::arrange(desc(alcance))

openxlsx::write.xlsx(alcance_nombre, "alcance_nombre.xlsx")

#Depuración de data frame tweets basado en la lista de lideres de opinion----

tweets_lideres <- tweets %>%  
  dplyr::filter(screen_name %in% lideres) %>%
  dplyr::select(USUARIO = screen_name,
                NOMBRE = name,
                PUBLICADO = created_at,
                TWEET = text,
                SEGUIDORES = followers_count,
                ME_GUSTA = favorite_count,
                RETWEET = retweet_count)

openxlsx::write.xlsx(tweets_lideres, "tweets_lideres.xlsx")

#Data frame alcance de lideres----

alcance_lideres <- tweets_lideres %>% 
  dplyr::select(NOMBRE,
                SEGUIDORES) %>% 
  dplyr::group_by(NOMBRE) %>% 
  dplyr::summarise(TWEETS = n(), 
                   ALCANCE = sum(SEGUIDORES)) %>% 
  dplyr::arrange(desc(ALCANCE))

openxlsx::write.xlsx(alcance_lideres, "alcance_lideres.xlsx")

#Grafica lideres de opinion----

tweets_lideres %>%
  count(USUARIO, sort = TRUE) %>%
  mutate(USUARIO = reorder(USUARIO, n)) %>%
  slice_max(USUARIO, n = 10) %>%
  ggplot(aes(x = USUARIO, y = n, fill = n)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  theme_classic() +
  scale_fill_gradient(low = "#BC955C",
                      high = "#691C32") +
  geom_text(aes(label = n),
            hjust = -0.2) +
  labs(title = "Lideres de opinion",
       subtitle = "N° de tweets emitidos",
       x = NULL,
       y = NULL,
       caption = "Los resultados reflejados corresponden al momento en que se realizó
       la consulta, por lo que éstas pueden variar con el transcurso del tiempo.")

#Depuración de data frame tweets basado en la lista de medios de comunicacion----

tweets_medios <- tweets %>% 
  dplyr::filter(screen_name %in% medios) %>%
  dplyr::select(USUARIO = screen_name,
                NOMBRE = name,
                PUBLICADO = created_at,
                TWEET = text,
                SEGUIDORES = followers_count,
                ME_GUSTA = favorite_count,
                RETWEET = retweet_count)

openxlsx::write.xlsx(tweets_medios, "tweets_medios.xlsx")

#Data frame alcance de medios----

alcance_medios <- tweets_medios %>% 
  dplyr::select(NOMBRE,
                SEGUIDORES) %>% 
  dplyr::group_by(NOMBRE) %>% 
  dplyr::summarise(TWEETS = n(), 
                   ALCANCE = sum(SEGUIDORES)) %>% 
  dplyr::arrange(desc(ALCANCE))

openxlsx::write.xlsx(alcance_medios, "alcance_medios.xlsx")

#Grafica Medios de comunicacion----

tweets_medios %>%
  count(USUARIO, sort = TRUE) %>%
  mutate(USUARIO = reorder(USUARIO, n)) %>%
  slice_max(USUARIO, n = 10) %>%
  ggplot(aes(x = USUARIO, y = n, fill = n)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  theme_classic() +
  scale_fill_gradient(low = "#BC955C",
                      high = "#691C32") +
  geom_text(aes(label = n),
            hjust = -0.2) +
  labs(title = "Medios de comunicación",
       subtitle = "N° de tweets emitidos",
       x = NULL,
       y = NULL,
       caption = "Los resultados reflejados corresponden al momento en que se realizó
       la consulta, por lo que éstas pueden variar con el transcurso del tiempo.")

#Lista de palabras vacias----

palabras_vacias <- stopwords("es") %>%
  base::as.data.frame()

palabras_custom <- c("marcelo", "ebrard", "@m_ebrard", "d", "bla", "cómo", "q",
                     "https://t.co/ympwdipy9c", "https://t.co/epbqlptafa",
                     "https://t.co/1qkbikgvgw", "https://t.co/l3qvyptiiy",
                     "https://t.co/ilfkojxx1i", "https://t.co/h4wvpb10k3", "@mebrard",
                     "▪","https://t.co/tceh7tpf90", "#cih2022alc", "https://t.co/ylh5afj3gx",
                     "https://t.co/1ewn4o6rbl") %>%
  base::as.data.frame()
  
#Separacion de tweets en palabras----

tweets_token <- tweets %>% 
  dplyr::select(text) %>%
  tidytext::unnest_tokens(output = "palabra",
                          input = "text",
                          token = "tweets") %>%       
  dplyr::anti_join(palabras_vacias, 
                   by = c("palabra" = ".")) %>% 
  dplyr::anti_join(palabras_custom, 
                   by = c("palabra" = "."))

#Conteo de palabras----

tweets_palabras <- tweets_token %>% 
  dplyr::count(palabra, 
               sort = TRUE) %>%
  dplyr::mutate(palabra = reorder(palabra, n))

openxlsx::write.xlsx(tweets_palabras, "tweets_palabras.xlsx")

#Gráfica conceptos----

tweets_token %>%
  dplyr::count(palabra, sort = TRUE) %>%
  dplyr::mutate(palabra = reorder(palabra, n)) %>%
  dplyr::slice_max(palabra, n = 10) %>%
  ggplot2::ggplot(aes(x = palabra, y = n, fill = n)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  theme_classic() +
  scale_fill_gradient(low = "#BC955C",
                      high = "#691C32") +
  geom_text(aes(label = n),
            hjust = -0.2) +
  labs(title = "Conceptos",
       subtitle = "N° de conceptos relacionados a la búsqueda",
       x = NULL,
       y = NULL,
       caption = "Los resultados reflejados corresponden al momento en que se realizó
       la consulta, por lo que éstas pueden variar con el transcurso del tiempo.")

#Clasificacion de palabras Positivas/negativas----

valores_affin <- read.csv("lexico_afinn.en.es.csv",
                          stringsAsFactors = F,
                          fileEncoding = "latin1")

tweets_afinn <- tweets %>%
  tidytext::unnest_tokens(input = "text",
                          output = "Palabra",
                          token = "tweets") %>%
  dplyr::inner_join(valores_affin, ., by = "Palabra") %>%
  dplyr::mutate(Tipo = ifelse(Puntuacion > 0, "Positivo", "Negativo"))

#Gráfica palabras negativas y positivas----

tweets_afinn %>%
  dplyr::count(Palabra, Tipo, sort = TRUE) %>%
  dplyr::group_by(Tipo) %>%
  dplyr::mutate(Palabra = reorder(Palabra, n)) %>%
  dplyr::slice_max(Palabra, n = 10) %>%
  ggplot2::ggplot(aes(x = Palabra, 
             y = n, 
             fill = Tipo)) +    
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = n), 
            hjust = 1, 
            vjust = 0.5) +
  facet_wrap( ~ Tipo, scales = "free_y") +
  coord_flip() +
  theme_bw() +
  labs(title = "Sentimiento por palabra",
       subtitle = "N° de palabras por sentimiento de genera",
       x = NULL,
       y = NULL)

#Nube de palabras positivas más usadas----

tweets_afinn %>%
  count(Palabra, Tipo) %>%
  filter(Tipo == "Positivo") %>%
  with(wordcloud(words = Palabra,
                 freq = n,
                 max.words = 100,
scale = c(3, .7),
                 rot.per = 0.3,
                 random.order = FALSE,
                 colors = brewer.pal(4, "BrBG")))

#Nube de palabras negativas más usadas----

tweets_afinn %>%
  count(Palabra, Tipo) %>%
  filter(Tipo == "Negativo") %>%
  with(wordcloud(words = Palabra,
                 freq = n,
                 max.words = 100,
scale = c(3, .8),
                 rot.per = 0.3,
                 random.order = FALSE,
                 colors = brewer.pal(4, "RdGy")))

#Clasificacion de tweets por sentimiento y creacion de engagement----

tweets_engagement <- tweets_afinn %>%
  dplyr::group_by(status_id) %>%
  dplyr::summarise(Puntuacion_tweet = base::mean(Puntuacion)) %>%
  dplyr::left_join(tweets, ., by = "status_id") %>% 
  dplyr::mutate(Puntuacion_tweet = base::ifelse(is.na(Puntuacion_tweet), 0, Puntuacion_tweet)) %>%
  dplyr::mutate(Sentimiento = base::ifelse(Puntuacion_tweet == 0, "Neutro", base::ifelse(Puntuacion_tweet > 0, "Positivo", "Negativo"))) %>% 
  dplyr::select(Usuario = screen_name,
                Nombre = name,
                Fecha = created_at,
                Tweet = text,
                Seguidores = followers_count,
                Me_Gusta = favorite_count,
                Retweets = retweet_count,
                Probabilidad_Bot = prob_bot,
                Sentimiento)

openxlsx::write.xlsx(tweets_engagement, "tweets_engagement.xlsx")

#Grafica de linea de tiempo----

rtweet::ts_plot(tweets, by = "days") +
        ggplot2::geom_line(colour = "#00acee") +
				ggplot2::geom_point(colour = "#4682b4") +
				ggplot2::geom_text(ggplot2::aes(label = n),
                     hjust = 0, 
                     vjust = 0) +
				ggplot2::labs(title = "Línea de tiempo",
				subtitle = "Representación del volumen de la conversación por día",
				x = NULL,
				y = "N° de tweets acumulados",
				caption = "Los resultados reflejados corresponden al momento en que se realizó
				la consulta, por lo que éstas pueden variar con el transcurso del tiempo.")     

r