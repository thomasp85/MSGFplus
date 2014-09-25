library(MSGFplus)

context('msgfParLengthRange: Construction and standard properties')

test_that('constructor works', {
    expect_error(msgfParLengthRange(3))
    expect_error(msgfParLengthRange(3:2))
    expect_error(msgfParLengthRange('test'))
    expect_is(msgfParLengthRange(c(6, 40)), 'msgfParLengthRange')
})

test_that('length works', {
    expect_equal(length(msgfParLengthRange()), 0)
    expect_equal(length(msgfParLengthRange(c(6, 40))), 1)
})