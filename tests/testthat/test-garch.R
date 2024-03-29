library(cvar)
context("garch")

test_that("GarchModel works ok", {
    mo1a <- GarchModel(omega = 1, alpha = 0.3, beta = 0.5)
    expect_equal(class(mo1a), "garch1c1")

    mo1b <- GarchModel(omega = 1, alpha = 0.3, beta = 0.5, cond.dist = "norm")

    identical(GarchModel(mo1a, omega = 0.4),
              GarchModel(      omega = 0.4, alpha = 0.3, beta = 0.5) )

    ##  GARCH(1,1) with standardised-t_5
    mo_t <- GarchModel(omega = 1, alpha = 0.3, beta = 0.5, cond.dist = list("std", nu = 5))
    expect_identical(mo_t, GarchModel(mo1a, cond.dist = list("std", nu = 5)))
})

test_that("garch1c1 related functions work ok", {
    ## try to deal with the misterious error on TravisCI and devtools::test()
    ## set.seed(123)
    
    a_mo <- GarchModel(omega = 0.4, alpha = 0.3, beta = 0.5)
    a <- sim_garch1c1(a_mo, n = 100, n.start = 100, seed = 1234)
    a_pred <- predict(a_mo, n.ahead = 5, Nsim = 100, eps = a$eps, sigmasq = a$h, seed = 1235)

    ## 2019-03-13 deal with change in RNG in R-devel (for 3.6.0), see email from Kurt Hornik in Org/
    ##        I couldn't make his suggestion work.
    ## TODO: maybe it would be better to just load 'a' from a saved version.
    if(getRversion() < "3.6.0"){
        a_saved <- "a_before_6.0.RDS"
        a_pred_saved <- "a_pred_before_6.0.RDS"
    }else{
        a_saved <- "a.RDS"
        a_pred_saved <- "a_pred.RDS"
    }

    expect_equal_to_reference(a, a_saved)
    expect_equal_to_reference(a_pred, a_pred_saved)

    ## as above but without 'seed'
    sim_garch1c1(a_mo, n = 100, n.start = 100)
    predict(a_mo, n.ahead = 5, Nsim = 100, eps = a$eps, sigmasq = a$h)

    expect_equal(.get_dist_elem(NULL, "r"), .dist$norm$r)
    ## for now
    expect_error(.get_dist_elem("unknowndist", "r"))
    expect_error(.get_dist_elem(list("unknowndist"), "r"))
    expect_error(.get_dist_elem(5, "r"))

    .get_dist_elem(list("norm"), "r")
    .get_dist_elem(list("norm", mean = 5, sd = 3), "r")
    .get_dist_elem(list("norm", mean = 5, sd = 3, n = 10), "r")

    .get_dist_elem("std", "r")
    .get_dist_elem(list("std", df = 5), "r")
    .get_dist_elem(list("std", df = 5, n = 10), "r")

    .get_dist_elem(list("ged"), "r")

    expect_equal(.get_dist_elem(NULL, "p"), .dist$norm[["p"]])

    .get_dist_elem("norm", "d")
    .get_dist_elem("norm", "p")
    .get_dist_elem("norm", "q")
    .get_dist_elem("norm", "r")

    .get_dist_elem(list("norm"), "p")
    .get_dist_elem(list("norm", mean = 4, sd = 2), "p")
    
    ## for now
    expect_error(.get_dist_elem("unknowndist"))
    expect_error(.get_dist_elem(list("unknowndist")))
    expect_error(.get_dist_elem(5))

})
