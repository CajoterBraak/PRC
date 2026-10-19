library(tinytest)


data(pyrifos, package = "vegan")


#the chlorpyrifos experiment from van den Brink & ter Braak 1999
data(pyrifos, package = "vegan") #log-transformed species data from package vegan

Design <- data.frame(Time=gl(11, 12, labels=c(-4, -1, 0.1, 1, 2, 4, 8, 12, 15, 19, 24)),
                     Treatment=factor(rep(c(0.1, 0, 0, 0.9, 0, 44, 6, 0.1, 44, 0.9, 0, 6), 11)),
                     cosm = gl(12, 1, length=132))

dose_levels <- sort(unique(c(0.1, 0, 0, 0.9, 0, 44, 6, 0.1, 44, 0.9, 0, 6)))
ldose <- log(10*dose_levels+1)

levels(Design$Treatment) <- ldose # if log dose
resp <- pyrifos

# set up model
modelS <- set_smoothPRC_model(data = Design)
expect_equal(modelS$spline,~spl2D(time, dose, nseg = c(6, 6), degree = 3, pord = 2))
expect_equal(modelS$splineH0,~spl1D(time, nseg = 6, degree = 3, pord = 2))
#qual
modelS <- set_smoothPRC_model(treatment.coding = "quali", data = Design)

expect_equal(modelS$spline,
             ~spl1D(time, nseg = 6, degree = 3, pord = 2) + spl1D(D1, nseg = 6,
             degree = 3, pord = 2) + spl1D(D2, nseg = 6, degree = 3, pord = 2) +
              spl1D(D3, nseg = 6, degree = 3, pord = 2) + spl1D(D4, nseg = 6,
              degree = 3, pord = 2)
             )
expect_equal(modelS$splineH0,~spl1D(time, nseg = 6, degree = 3, pord = 2))

expect_error(set_smoothPRC_model(treatment.coding = "qua", data = Design),
"'arg' should be one of “quantitative”, “qualitative”")


modelS <- set_smoothPRC_model(data = Design, treatment.coding = "qual",
               nseg=c(time=7,dose = 3), pord =1)



expect_equal(modelS$spline,
             ~spl1D(time, nseg = 7, degree = 3, pord = 1) + spl1D(D1, nseg = 7,
             degree = 3, pord = 1) + spl1D(D2, nseg = 7, degree = 3, pord = 1) +
             spl1D(D3, nseg = 7, degree = 3, pord = 1) + spl1D(D4, nseg = 7,
                                                        degree = 3, pord = 1)

    )

expect_equal(modelS$splineH0,~spl1D(time, nseg = 7, degree = 3, pord = 1))






expect_equal(modelS$splineH0,~spl1D(time, nseg = 7, degree = 3, pord = 1))

# test to confirm that we can set spline and splineH0 so what we want
modelS <- set_smoothPRC_model(spline = ~x1, splineH0 = ~x2, data = Design)

expect_equal(modelS$spline,~x1)

expect_equal(modelS$splineH0,~x2)
expect_error(smoothPRC(pyrifos, model = modelS),
             "spline should be a formula of form ")

modelS <- set_smoothPRC_model(nseg =c(time = 5, dose = 7), treatment.coding = "qual", data = Design)

expect_equal(modelS$spline,
             ~spl1D(time, nseg = 5, degree = 3, pord = 2) + spl1D(D1, nseg = 5,
              degree = 3, pord = 2) + spl1D(D2, nseg = 5, degree = 3, pord = 2) +
              spl1D(D3, nseg = 5, degree = 3, pord = 2) + spl1D(D4, nseg = 5,
              degree = 3, pord = 2)
             )

expect_equal(modelS$splineH0,~spl1D(time, nseg = 5, degree = 3, pord = 2))







