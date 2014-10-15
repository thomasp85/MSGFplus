context('msgfParIsotopeError: Construction and standard properties')

test_that('constructor works', {
    expect_error(msgfParIsotopeError(1.5))
    expect_error(msgfParIsotopeError('test'))
    expect_error(msgfParIsotopeError(0:4))
    expect_is(msgfParIsotopeError(c(2,3)), 'msgfParIsotopeError')
})

test_that('length works', {
    expect_equal(length(msgfParIsotopeError()), 0)
    expect_equal(length(msgfParIsotopeError(c(2,3))), 1)
})