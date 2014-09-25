library(MSGFplus)

context('msgfParNtt: Construction and standard properties')

test_that('constructor works', {
    expect_error(msgfParNtt(3))
    expect_error(msgfParNtt(-1))
    expect_error(msgfParNtt(0:2))
    expect_error(msgfParNtt('test'))
    expect_is(msgfParNtt(2), 'msgfParNtt')
})

test_that('length works', {
    expect_equal(length(msgfParNtt()), 0)
    expect_equal(length(msgfParNtt(2)), 1)
})