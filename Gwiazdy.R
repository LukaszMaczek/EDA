library(mice)
library(dplyr)
library(ggplot2)
library(corrplot)



head(star_class, 3)
help("colnames")

colnames(star_class) <- c("Temperatura", "Jasność", 
                          "Promień", "Abs_Wielk_Gwiazd",
                          "Typ", "Kolor", "Typ_Spektralny")

str(star_class)
dim(star_class)

summary(star_class)
stars <- star_class
stars$Kolor <- as.factor(stars$Kolor)

str(stars)
stars$Kolor[stars$Kolor == "Blue White"] <- "Blue-White"
stars$Kolor[stars$Kolor == "Blue white "] <- "Blue-White"
stars$Kolor[stars$Kolor == "Blue white"] <- "Blue-White"
stars$Kolor[stars$Kolor == "Blue-white"] <- "Blue-White"
stars$Kolor[stars$Kolor == "Blue "] <- "Blue"
stars$Kolor[stars$Kolor == "white"] <- "White"
stars$Kolor[stars$Kolor == "yellow-white"] <- "White-Yellow"
stars$Kolor[stars$Kolor == "yellowish"] <- "Yellowish"
summary(stars$Kolor)

stars$Kolor <- as.factor(stars$Kolor)
#usuwanie jednego braku danych
is.na(stars$Kolor)
stars$Kolor
md.pattern(stars)
sum(is.na(stars))
stars <- cc(stars)
#usuwamy puste poziomy factor w zmiennej Kolory
poziom_usuwamy <- c("Blue ", "Blue-white", "Blue white", "Blue White", "Blue white ",
                    "white", "yellow-white", "yellowish")

stars <- droplevels(stars, exclude = poziom_usuwamy)
summary(stars$Kolor)
summary(stars)

summary(stars$Typ_Spektralny)
stars$Typ_Spektralny
stars$Typ_Spektralny <- as.factor(stars$Typ_Spektralny)

stars$Typ <- as.factor(stars$Typ)
summary(stars$Typ)

#===========================================
stars %>%
  #filter(Jasność < 0.01)%>%
  #filter(Typ %in% c(0, 1, 2)) %>%
  ggplot() +
  geom_point(mapping = aes(x = Jasność, y = Temperatura, col = Typ))

stars %>%
  filter(Typ == 1) %>%
  ggplot() +
  geom_point(mapping = aes(x = Jasność, y = Temperatura, col = Typ, size = 2))

stars %>%
  ggplot()+
  geom_boxplot(aes(x = Jasność, y = Temperatura))

stars %>%
  ggplot() + 
  geom_density(aes(x = Temperatura),
                 bins = 15,
               fill = "green")

stars %>%
  ggplot() +
  geom_histogram(aes(x = Temperatura), fill = "lightblue", color = "darkblue")

stars %>%
  group_by(Typ_Spektralny)%>%
  ggplot() +
  geom_boxplot(aes(x = Typ_Spektralny, y = Promień))

summary(stars)

stars %>%
  filter(Typ_Spektralny == "B", Promień < 100) %>%
  ggplot() +
  geom_point(mapping = aes(x = Promień, y = Temperatura))

stars %>%
  filter(Typ_Spektralny == "K") %>%
  ggplot() +
  geom_point(mapping = aes(x = Promień, y = Temperatura))
  
stars %>%
  ggplot() +
  geom_point(mapping = aes(y = Typ, x = Typ_Spektralny))
'
Typ 0-5
0 - Brązowy? Karzeł
1 - Czerwony Karzeł
2 - Biały Karzeł
3 - Ciąg główny (main sequence)
4 - Giganty
5 - Supergiganty
'
'
Typ Spektralny O, B, A, F, G, K ,M
Klasa O: Gwiazdy bardzo gorące, jasne, o błękitnym kolorze.
Klasa B: Gorące, ale nieco chłodniejsze niż gwiazdy typu O, o błękitnym lub białym kolorze.
Klasa A: Gwiazdy białe lub białoniebieskie, o niższej temperaturze niż gwiazdy typu B.
Klasa F: Jasno-żółte gwiazdy o niższej temperaturze niż gwiazdy typu A.
Klasa G: Gwiazdy żółte, do których należy nasze Słońce.
Klasa K: Pomarańczowe gwiazdy o niższej temperaturze niż gwiazdy typu G.
Klasa M: Czerwone gwiazdy, najzimniejsze w tej klasyfikacji.
'
stars %>%
  ggplot() +
  geom_point(mapping = aes(x = Temperatura, y = Abs_Wielk_Gwiazd,
                           col = Typ_Spektralny))
#od najmniejszej temperatury do największej
levels(stars$Typ_Spektralny) <- c("M", "K", "G", "F", "A", "B", "O") 

stars %>%
  ggplot() +
  geom_point(mapping = aes(x = Temperatura, y = Abs_Wielk_Gwiazd,
                           col = Typ))
'
Gwiazdy typów gorących jak, np. O lub B mogą być chłodne na skutek pewnych 
zdarzeń lub ewolucji, może schłodzić się i zmniejszać swoją temperaturę. 
Na przykład, gwiazda typu O może ewoluować w kierunku bardziej chłodnej 
gwiazdy typu B, a potem dalej.
'
'
Absolute Magnitude oznacza jasność gwiazdy, gdyby był położona w odległości
10 parseków od obserwatora (1 parsek to około 3,26 lat świetlnych), im mniejsze,
tym jaśniejsze
'
summary(stars$Abs_Wielk_Gwiazd)
w <- which.max(stars$Abs_Wielk_Gwiazd)
(stars[w,])
w1<-which.max(stars$Promień)
(stars[w1,])


stars %>%
  ggplot() +
  geom_point(mapping = aes(x = Jasność, y = Abs_Wielk_Gwiazd,
                           col = Typ))

stars %>%
  ggplot() +
  geom_line(aes(x = Jasność, y = Abs_Wielk_Gwiazd,
                           col = Typ))

cor = cor(stars[,c("Jasność", "Temperatura", "Abs_Wielk_Gwiazd", "Promień")],
           method = "spearman")
round(cor, 2)

corrplot(cor, method = "number", type = "upper") #duża korelacja miedzy promieniem a jasnoscia

cor(stars$Jasność, stars$Abs_Wielk_Gwiazd, method = "spearman") #duża ujemna korelacja -> 
# -> odwrotnie proporcjonalnie, co się zgadza z założeniami

'====================================
Pytania:
1. Czy w danych zmienne Jasność i Absolutna Wielkość Gwiazdowa, są odwrotnie
proporcjonalne, jak sugerowałaby znajomość tych zależności? (Tak)
2.Czy istnieje związek pomiędzy temperaturą gwiazdy a jej jasnością?
3.Czy istnieje związek między temperaturą gwiazdy a jej Promieniem(wielkością)?
4. Czy wszystkie gwiazdy typu spektralnego charakteryzującego się niską temperaturą(np. M, K)
są małe lub duże (Typ)?
5. Gwiazdy którego typu charakteryzują się najwyższą temperaturą/wielkością/jasnością?
6.Czy istnieje związek pomiędzy Typem gwiazdy a jej Typem Spektralnym?
7. Czy istnieje zależność pomiedzy jasnością gwiazdy a jej promieniem?
8. Jakie kolory mają konkretne typy gwiazd? Czy zgadza się to z wiedzą teoretyczna?
======================================'

stars%>%
  ggplot() +
  geom_bar(aes(x = Typ))
stars %>%
  ggplot() +
  geom_bar(aes(x = Typ_Spektralny))
stars%>%
  ggplot() +
  geom_bar(aes(y = Kolor))

stars%>%
  filter(Typ %in% c(0))%>%
  ggplot() +
  geom_point(mapping = aes(x=Promień, y=Abs_Wielk_Gwiazd, col = Typ))

cor(stars$Jasność, stars$Promień, method = "spearman")

hist(stars$Temperatura)
hist(stars$Jasność)
hist(stars$Promień)
hist(stars$Abs_Wielk_Gwiazd)

stars %>%
  ggplot()+
  geom_bar(aes(x = Typ_Spektralny))

#Zmienne Temperatura, Jasność i Promień mają rozkład zbliżony do rozkładu chi^2?

