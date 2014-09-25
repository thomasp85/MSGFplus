library(MSGFplus)

context('msgfParTolerance: Construction and standard properties')

test_that('constructor works', {
    expect_error(msgfParTolerance(20, unit='test'))
    expect_error(msgfParTolerance(low=2:3, high=2, unit='Da'))
    expect_is(msgfParTolerance(20, unit='ppm'), 'msgfParTolerance')
    expect_is(msgfParTolerance(low=1, high=1.5, unit='Da'), 'msgfParTolerance')
    expect_is(msgfParTolerance(c(1, 1.5), unit='Da'), 'msgfParTolerance')
    expect_equal(msgfParTolerance(c(20,30), unit='ppm'), msgfParTolerance(low=20, high=30, unit='ppm'))
})

test_that('length works', {
    expect_equal(length(msgfParTolerance()), 0)
    expect_equal(length(msgfParTolerance(20, unit='ppm')), 1)
})