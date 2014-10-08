context('msgfParEnzyme: Construction and standard properties')

test_that('constructor works', {
    expect_error(msgfParEnzyme(0:3))
    expect_error(msgfParEnzyme(10))
    expect_error(msgfParEnzyme('test'))
    expect_is(msgfParEnzyme(0), 'msgfParEnzyme')
    expect_is(msgfParEnzyme('Trypsin'), 'msgfParEnzyme')
})

test_that('length works', {
    expect_equal(length(msgfParEnzyme()), 0)
    expect_equal(length(msgfParEnzyme(0)), 1)
})