context('Checking the installed version of java')

test_that('Zero length', {
    expect_warning(.verifyJavaVersion(character(0)))
})

test_that('Old style Java version numbers', {
    expect_silent(.verifyJavaVersion( 'openjdk version \"1.8.0_171\"' ))
    expect_warning(.verifyJavaVersion( 'openjdk version \"1.6.0_171\"' ))
})

test_that('New style Java version numbers', {
    expect_silent(.verifyJavaVersion( 'openjdk version \"10.0.1\"' ))
})