library(MSGFplus)

context('msgfParFragmentation: Construction and standard properties')

test_that('constructor works', {
    expect_error(msgfParFragmentation(0:3))
    expect_error(msgfParFragmentation(10))
    expect_error(msgfParFragmentation('test'))
    expect_is(msgfParFragmentation(0), 'msgfParFragmentation')
    expect_is(msgfParFragmentation('CID'), 'msgfParFragmentation')
})

test_that('length works', {
    expect_equal(length(msgfParFragmentation()), 0)
    expect_equal(length(msgfParFragmentation(0)), 1)
})