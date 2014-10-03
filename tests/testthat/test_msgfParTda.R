context('msgfParTda: Construction and standard properties')

test_that('constructor works', {
    expect_error(msgfParTda(3))
    expect_error(msgfParTda(c(T,F)))
    expect_error(msgfParTda('test'))
    expect_is(msgfParTda(TRUE), 'msgfParTda')
})

test_that('length works', {
    expect_equal(length(msgfParTda()), 0)
    expect_equal(length(msgfParTda(TRUE)), 1)
})