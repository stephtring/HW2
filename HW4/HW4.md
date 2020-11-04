HW4
================

## Part 1: HPC

# Problem 1: Make sure your code is nice

Rewrite the following R functions to make them faster. It is OK (and
recommended) to take a look at Stackoverflow and Google The following
functions can be written to be more efficient without using parallel:

``` r
# Total row sums
fun1 <- function(mat) {
  n <- nrow(mat)
  ans <- double(n) 
  for (i in 1:n) {
    ans[i] <- sum(mat[i, ])
  }
  ans
}

fun1alt <- function(mat) {
 
 #cbind(ans, total=rowSums(ans))
  
  ans <- rowSums(mat)
  ans

}

 x1 <- matrix(rnorm(1000000),nrow=100)
# Benchmarking
microbenchmark::microbenchmark(
  fun1(x1),
  fun1alt(x1), unit='relative'
)
```

    ## Unit: relative
    ##         expr     min       lq     mean   median       uq      max neval
    ##     fun1(x1) 7.38236 7.520315 8.565496 7.640604 10.07639 12.50482   100
    ##  fun1alt(x1) 1.00000 1.000000 1.000000 1.000000  1.00000  1.00000   100

``` r
# Cumulative sum by row
fun2 <- function(mat) {
  n <- nrow(mat)
  k <- ncol(mat)
  ans <- mat
  for (i in 1:n) {
    for (j in 2:k) {
      ans[i,j] <- mat[i, j] + ans[i, j - 1]
    }
  }
  ans
}

fun2alt <- function(mat) {
  n <- nrow(mat)
  ans <- mat
  for (i in 1:n) {
    ans[i,] <- cumsum(mat[i,])
  }
  ans
}

 x2 <- matrix(rnorm(10000),nrow=100)

# Benchmarking
microbenchmark::microbenchmark(
  fun2(x2),
  fun2alt(x2), unit='relative'
)
```

    ## Unit: relative
    ##         expr      min       lq     mean   median       uq      max neval
    ##     fun2(x2) 4.070768 3.609969 3.296877 3.538218 3.465159 1.764532   100
    ##  fun2alt(x2) 1.000000 1.000000 1.000000 1.000000 1.000000 1.000000   100

``` r
# Use the data with this code
set.seed(2315)
dat <- matrix(rnorm(200 * 100), nrow = 200)



# Test for the first
microbenchmark::microbenchmark(
  fun1(dat),
  fun1alt(dat), unit = "relative", check = "equivalent"
)
```

    ## Unit: relative
    ##          expr      min       lq     mean   median       uq max neval
    ##     fun1(dat) 7.635281 9.432645 9.111712 9.503618 9.139415 6.2   100
    ##  fun1alt(dat) 1.000000 1.000000 1.000000 1.000000 1.000000 1.0   100

``` r
# Test for the second
microbenchmark::microbenchmark(
  fun2(dat),
  fun2alt(dat), unit = "relative", check = "equivalent"
)
```

    ## Unit: relative
    ##          expr      min       lq     mean   median      uq       max neval
    ##     fun2(dat) 6.392573 3.658919 3.007721 3.617581 3.55786 0.1510201   100
    ##  fun2alt(dat) 1.000000 1.000000 1.000000 1.000000 1.00000 1.0000000   100

# Problem 2: Parallelize everyhing

``` r
sim_pi <- function(n = 1000, i = NULL) {
  p <- matrix(runif(n*2), ncol = 2)
  mean(rowSums(p^2) < 1) * 4
}

# Here is an example of the run
set.seed(156)
sim_pi(1000) # 3.132
```

    ## [1] 3.132

``` r
#In order to get accurate estimates, we can run this function multiple times, with the following code:

# This runs the simulation a 4,000 times, each with 10,000 points
set.seed(1231)
system.time({
  ans <- unlist(lapply(1:4000, sim_pi, n = 10000))
  print(mean(ans))
})
```

    ## [1] 3.14124

    ##    user  system elapsed 
    ##   2.965   0.776   3.750

``` r
library(parallel)

 
  # Making the cluster using `ncpus`
  # STEP 1: Make cluster
 system.time ({  
 cl <- parallel::makePSOCKcluster(4)

  # STEP 2: Set it up (espoty if needed, idx and dat)
  
 clusterSetRNGStream(cl, 1231)

  
    # STEP 3: THIS FUNCTION NEEDS TO BE REPLACES WITH parLapply
ans <- unlist(parLapply(cl, 1:4000, sim_pi, n=10000))
   print(mean(ans))
  
  # STEP 4: Gsopt the cluster
  parallel::stopCluster(cl)
  ans
  
})
```

    ## [1] 3.141578

    ##    user  system elapsed 
    ##   0.013   0.010   1.916

## SQL

``` r
# install.packages(c("RSQLite", "DBI"))

library(RSQLite)
library(DBI)

# Initialize a temporary in memory database
con <- dbConnect(SQLite(), ":memory:")

# Download tables
film <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/film.csv")
film_category <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/film_category.csv")
category <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/category.csv")

# Copy data.frames to database
dbWriteTable(con, "film", film)
dbWriteTable(con, "film_category", film_category)
dbWriteTable(con, "category", category)

dbListTables(con)
```

    ## [1] "category"      "film"          "film_category"

``` r
knitr::opts_chunk$set(eval = FALSE)
library(RSQLite)
library(DBI)
# Initialize a temporary in memory database
con <- dbConnect(SQLite(), ":memory:")
```

``` sql
PRAGMA table_inf(film_category)
```

# Question 1

How many many movies is there avaliable in each rating catagory.

``` sql

SELECT rating, COUNT(film_id) AS movies
FROM film
GROUP by rating
```

# Question 2

What is the average replacement cost and rental rate for each rating
category.

``` sql
SELECT rating, AVG(replacement_cost) AS avg_replace_cost, AVG(rental_rate) AS avg_rent_rate
FROM film
GROUP by rating
```

# Question 3

Use table film\_category together with film to find the how many films
there are witth each category ID

``` sql

SELECT category_id, COUNT(x.film_id) AS film
FROM film_category x JOIN film y
ON x.film_id=y.film_id
GROUP by category_id
```

# Question 4

Incorporate table category into the answer to the previous question to
find the name of the most popular category.

``` sql
SELECT name, x.category_id, COUNT(x.film_id) AS films
FROM film_category x JOIN film y
ON x.film_id=y.film_id
INNER JOIN category z ON x.category_id=z.category_id
GROUP by x.category_id
ORDER by films DESC
```

The name of the most popular cateogry is Sports
