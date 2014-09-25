library(MSGFplus)

context('msgfParChargeRange: Construction and standard properties')

test_that('constructor works', {
    expect_error(msgfParChargeRange(3))
    expect_error(msgfParChargeRange(3:2))
    expect_error(msgfParChargeRange('test'))
    expect_is(msgfParChargeRange(2:3), 'msgfParChargeRange')
})

test_that('length works', {
    expect_equal(length(msgfParChargeRange()), 0)
    expect_equal(length(msgfParChargeRange(2:3)), 1)
})