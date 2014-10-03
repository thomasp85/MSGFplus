context('msgfParMatches: Construction and standard properties')

test_that('constructor works', {
    expect_error(msgfParMatches(1.5))
    expect_error(msgfParMatches(2:3))
    expect_error(msgfParMatches('test'))
    expect_is(msgfParMatches(2), 'msgfParMatches')
})

test_that('length works', {
    expect_equal(length(msgfParMatches()), 0)
    expect_equal(length(msgfParMatches(2)), 1)
})