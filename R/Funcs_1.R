#Useful functions for Brazil paper
vec.state  <- 1:27
lab.state  <- c("Acre","Alagoas","Amapá","Amazonas","Bahia","Ceará","Distrito Federal","Espírito Santo","Goiás",              
                "Maranhão","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Pará","Paraíba","Paraná","Pernambuco",
                "Piauí","Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondônia","Roraima",
                "Santa Catarina","São Paulo","Sergipe","Tocantins")
names(vec.state) <- lab.state

vec.region <- 1:5
lab.region <- c("North","Northeast","Center West","Southeast","South")
names(vec.region) <- lab.region

vec.sex    <- 1:3
lab.sex    <- c("female","male","no information")
names(vec.sex) <- lab.sex

vec.race   <- 1:6
lab.race   <- c("White","Black","Brown","Indigenous","no race information","Asian")
names(vec.race) <- lab.race

vec.age    <- c(0,1,seq(5,100,5),999)
lab.age    <- c("0 to 1","1 to 4","5 to 9","10 to 14","15 to 19","20 to 24","25 to 29","30 to 34",
                "35 to 39","40 to 44","45 to 49","50 to 54","55 to 59","60 to 64","65 to 69","70 to 74",
                "75 to 79","80 to 84","85 to 90","90 to 94","95 to 99","more than 100","no age information")
names(vec.age) <- lab.age

vec.cause  <- 1:11
lab.cause  <- c("avoidable","diabetes","ischemic heart diseases","alcoholic liver disease","lung cancer",
                "hiv","homicide","road traffic accidents","self inflicted injuries","rest","total")
names(vec.cause) <- lab.cause