library(MSGFplus)

context('msgfParInstrument: Construction and standard properties')

test_that('constructor works', {
    expect_error(msgfParInstrument(0:3))
    expect_error(msgfParInstrument(10))
    expect_error(msgfParInstrument('test'))
    expect_is(msgfParInstrument(0), 'msgfParInstrument')
    expect_is(msgfParInstrument('TOF'), 'msgfParInstrument')
})

test_that('length works', {
    expect_equal(length(msgfParInstrument()), 0)
    expect_equal(length(msgfParInstrument(0)), 1)
})