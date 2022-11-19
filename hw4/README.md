566-hw4
================
Yumeng Gao
2022-11-18

# HPC

## Problem 1: Make sure your code is nice

Rewrite the following R functions to make them faster. It is OK (and
recommended) to take a look at Stackoverflow and Google

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
  rowSums(mat)
}


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
  t(apply(mat,1,cumsum))
}


# Use the data with this code
set.seed(2315)
dat <- matrix(rnorm(200 * 100), nrow = 200)

# Test for the first
t1= microbenchmark::microbenchmark(fun1(dat), fun1alt(dat))
print(t1, unit = "relative", check = "equivalent")
```

    ## Unit: relative
    ##          expr      min       lq     mean   median       uq      max neval
    ##     fun1(dat) 10.86751 11.19555 12.15576 10.80572 11.04453 19.62195   100
    ##  fun1alt(dat)  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000   100

``` r
# Test for the second
t2= microbenchmark::microbenchmark(fun2(dat), fun2alt(dat))
print(t2, unit = "relative", check = "equivalent")
```

    ## Unit: relative
    ##          expr      min       lq     mean   median       uq      max neval
    ##     fun2(dat) 4.462494 2.360217 2.159223 2.322512 2.285218 1.117262   100
    ##  fun2alt(dat) 1.000000 1.000000 1.000000 1.000000 1.000000 1.000000   100

The last argument, check = “equivalent”, is included to make sure that
the functions return the same result.

## Problem 2: Make things run faster with parallel computing

The following function allows simulating PI

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

In order to get accurate estimates, we can run this function multiple
times, with the following code:

``` r
set.seed(1231)
system.time({
  ans <- unlist(lapply(1:4000, sim_pi, n = 10000))
  print(mean(ans))
})
```

    ## [1] 3.14124

    ##    user  system elapsed 
    ##   3.384   1.054   4.736

Rewrite the previous code using parLapply() to make it run faster. Make
sure you set the seed using clusterSetRNGStream():

``` r
library(parallel)
cl <- makePSOCKcluster(4L) 
clusterSetRNGStream(cl, 1234)
n=10000
clusterExport(cl, c("n", "sim_pi"))
system.time({
  ans <- unlist(parLapply(cl, 1:4000, sim_pi, n))
  print(mean(ans))
})
```

    ## [1] 3.141138

    ##    user  system elapsed 
    ##   0.005   0.001   0.671

``` r
stopCluster(cl)
```

# SQL

Setup a temporary database by running the following chunk

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
```

When you write a new chunk, remember to replace the r with sql,
connection=con.

## Question 1

How many many movies is there avaliable in each rating catagory.

``` sql
SELECT rating,
  COUNT(*) AS "Count"
FROM film
GROUP BY rating
```

| rating | Count |
|:-------|------:|
| G      |   180 |
| NC-17  |   210 |
| PG     |   194 |
| PG-13  |   223 |
| R      |   195 |

5 records

## Question 2

What is the average replacement cost and rental rate for each rating
category.

``` sql
SELECT rating, 
  AVG(replacement_cost) AS "Average Replacement Cost",
  AVG(rental_rate) AS "Average Rental Rate"
FROM film
GROUP BY rating
```

| rating | Average Replacement Cost | Average Rental Rate |
|:-------|-------------------------:|--------------------:|
| G      |                 20.12333 |            2.912222 |
| NC-17  |                 20.13762 |            2.970952 |
| PG     |                 18.95907 |            3.051856 |
| PG-13  |                 20.40256 |            3.034843 |
| R      |                 20.23103 |            2.938718 |

5 records

## Question 3

Use table film_category together with film to find the how many films
there are with each category ID

``` sql
SELECT category_id,
  COUNT(*) as "Count"
FROM film AS a INNER JOIN film_category AS b
on a.film_id = b.film_id
GROUP BY category_id 
```

| category_id | Count |
|:------------|------:|
| 1           |    64 |
| 2           |    66 |
| 3           |    60 |
| 4           |    57 |
| 5           |    58 |
| 6           |    68 |
| 7           |    62 |
| 8           |    69 |
| 9           |    73 |
| 10          |    61 |

Displaying records 1 - 10

## Question 4

Incorporate table category into the answer to the previous question to
find the name of the most popular category.

``` sql
SELECT c.name, b.category_id,
  COUNT(*) AS "Count"
FROM ((film_category AS b
INNER JOIN film AS a ON a.film_id = b.film_id)
INNER JOIN category AS c ON b.category_id = c.category_id)
GROUP BY c.name
ORDER BY "Count" DESC
```

| name        | category_id | Count |
|:------------|------------:|------:|
| Sports      |          15 |    74 |
| Foreign     |           9 |    73 |
| Family      |           8 |    69 |
| Documentary |           6 |    68 |
| Animation   |           2 |    66 |
| Action      |           1 |    64 |
| New         |          13 |    63 |
| Drama       |           7 |    62 |
| Sci-Fi      |          14 |    61 |
| Games       |          10 |    61 |

Displaying records 1 - 10

From this table, we can find that Sports is the most famous category.

### Clean up

``` r
dbDisconnect(con)
```
