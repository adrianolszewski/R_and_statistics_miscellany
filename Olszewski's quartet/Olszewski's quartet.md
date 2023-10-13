# Olszewski's vs. Anscombe's quartet
You probably heard about the [Anscombe's quartet](https://en.wikipedia.org/wiki/Anscombe%27s_quartet).
It's almost a textbook justification for **looking at the data first and not trusting solely descriptive statistics**!

I decided to make my own, Olszewski's quartet! It shows 4 faces in different moods. The **mean** and **variance** of the Y coordinate is exactly (NOT approximately!) the same for all 4 faces.
Also, the Pearson's correlation is almost 0.

![obraz](https://github.com/adrianolszewski/R_and_statistics_miscellany/assets/95669100/93b1d1f6-8710-485e-a751-0d7cb45b1cae)

# How did I make it?
First I needed to make somehow datasets storing sketches of faces in four different moods: happy, sad, angry and surprised.
The moods are expressed just by the shape of mouth and eyebrows. Simple, but does the job.

I decided to use the awesome **[drawdata.xyz/](https://drawdata.xyz/)** service.
It allows one to draw 4 datasets (A, B, C, D), but I needed 5: 1 for the overall face (without mouth and eyebrows) and 4 different moods.
So I decided to split it into two datasets: a common dataset with face, and another dataset storing the elements of moods.

Yep, it was a bit challenging to determine the right position of mouth and eyebrows not seeing the original face - I had to reset it to start another dataset.
Fortunately, I found a way to match them: I put an adhesive card on the screen, took a pencil and sketched the face on it.
Then, after resetting the board, I used the sketched face as a template to decide where to put mouth and eyebrows :)

![obraz](https://github.com/adrianolszewski/R_and_statistics_miscellany/assets/95669100/3a1e1358-85a8-4ad4-b51b-6e456ede35bb)

Finally I ended up with the following datasets:
![obraz](https://github.com/adrianolszewski/R_and_statistics_miscellany/assets/95669100/c9194889-b46e-4c7e-9cc3-923edd4a90c3)

I downloaded them as CSV files. They look like this:
```r
> head(mood)
    x     y z
1 257 111.6 a
2 257 110.6 a
3 257 110.6 a
4 257 109.6 a
5 258 109.6 a
6 258 108.6 a
 
> tail(mood)
       x     y z
1460 498 411.6 d
1461 499 409.6 d
1462 499 408.6 d
1463 499 408.6 d
1464 500 408.6 d
1465 499 408.6 d
```

The "z" column represents the group. They are named: a, b, c, d

Let's print them in R:
``` r
face <- read.csv("faces.csv")
mood <- read.csv("moods.csv")

layout(t(1:2))
plot(face$x, face$y)
plot(mood$x, mood$y)
```
![obraz](https://github.com/adrianolszewski/R_and_statistics_miscellany/assets/95669100/d6e0817a-76ae-4f2a-afa4-fc4a79c74166)

That's it! But means and variances of their Y coordinates, of course, vary:
```r
> tapply(mood$y, mood$z, function(x) c(mean=mean(x), var=var(x)))
$a
      mean        var 
  262.8756 29554.2238 
$b
      mean        var 
  220.5737 24246.4995 
$c
      mean        var 
  164.9827 19558.0978 
$d
     mean       var 
  234.329 30944.038 
```

So I needed some hocus-pocus to make them perfectly equal.

The simples way is to just standardize the Y coordinate: $Y_{std} = \frac{Y - mean(Y)}{variance(Y)}$
This gives Y cordinate with exactly 0 mean and unit variance.

That's all! But I wanted to make guessing a little bit more difficult, and decided to shift the mean and "explode" variance a bit, to not suggest the obvious solution 😉

```r
inflate_variance <- function(x, factor) {
  mean_x <- mean(x)
  scaled_var <- (x - mean_x) * sqrt(factor)
  x <- (mean_x + scaled_var)
}

> var( inflate_variance( rnorm(n = 10000, mean = 0, sd = 1), factor = 5))
[1] 5.045869

> var( inflate_variance( rnorm(n = 10000, mean = 0, sd = 1), factor = 2.5))
[1] 2.497489

> var( inflate_variance( rnorm(n = 10000, mean = 0, sd = 1), factor = 11.6))
[1] 11.45029
```
It works!
Now I can combine all data, re-scale the Y coordinate and finally plot the faces and their descriptive statistics.

BTW, the ~0 Pearson's correlation comes out of the box.
Just look how the facecs are drawn: all elements except nose are symmetric (more or less - I'm not a good sketcher...).
Consequently, all increases in Y compensate (cancel) corresponding decreases, so the overall change is 0. So is the Pearson's R. 

And that's all. Enjoy!

``` r
library(dplyr)
library(ggplot2)

do.call(bind_rows,
        lapply(letters[1:4], function(mood_id) {
          
          bind_cols("Face" = paste("Face", toupper(mood_id)),
                bind_rows(face, mood[mood$z == mood_id,])) %>% 
          mutate(y = scale(y, center = TRUE, scale = TRUE), 
                 y = y + 5,
                 y = inflate_variance(y, factor=2.2));
          })
        ) %>% 
ggplot() + 
  geom_point(aes(x=x, y=y, col=Face), show.legend = FALSE) +
  facet_wrap(~Face, ncol = 2) +
  theme_bw() +
  geom_label(data = . %>% 
                      group_by(Face) %>% 
                      summarize(Mean = mean(y),
                                Var  = var(y),
                                R    = cor(x, y)),
             aes(x=0, y=1.8, fill = Face, 
                 label = sprintf("Mean: %.3f | Variance: %.3f | Pearson's R: %.2f", Mean, Var, R)),
             hjust = 0, vjust = 0.5, show.legend = FALSE,
             fontface = "bold", col = "white") +
  labs(title = "Olszewski's quartet :-)",
       subtitle = "Always look at the data first! Don't rely solely on  descriptive statistics!",
       caption = "Adrian Olszewski, 2023\nhttps://www.linkedin.com/in/adrianolszewski/") +
  theme(plot.title = element_text(size = 20),
        plot.caption = element_text(face = "italic"))
```
