context('getMSGFpar: Extract parameter strings')

testPar <- msgfPar(
    database=system.file(package='MSGFplus', 'extdata', 'milk-proteins.fasta'),
    tolerance='20 ppm',
    isotopeError=c(0,2),
    tda=TRUE,
    fragmentation='CID',
    instrument='TOF',
    enzyme='Lys-C',
    protocol='No protocol',
    ntt=2,
    modification=list(
       nMod=2,
       modifications=list(
           list(name='Carbamidomethyl',
                composition='C2H3N1O1',
                residues='C',
                type='fix',
                position='any'
           ),
           list(name='Oxidation',
                mass=15.994915,
                residues='M',
                type='opt',
                position='any'
           )
       )
    ),
    lengthRange=c(6,40),
    chargeRange=c(2,7),
    matches=1
)

test_that('all relevant classes implement the method', {
    expect_is(getMSGFpar(testPar), 'character')
    expect_is(getMSGFpar(testPar@tolerance), 'character')
    expect_is(getMSGFpar(testPar@isotopeError), 'character')
    expect_is(getMSGFpar(testPar@tda), 'character')
    expect_is(getMSGFpar(testPar@fragmentation), 'character')
    expect_is(getMSGFpar(testPar@instrument), 'character')
    expect_is(getMSGFpar(testPar@enzyme), 'character')
    expect_is(getMSGFpar(testPar@protocol), 'character')
    expect_is(getMSGFpar(testPar@ntt), 'character')
    expect_is(getMSGFpar(testPar@modification), 'character')
    expect_is(getMSGFpar(testPar@lengthRange), 'character')
    expect_is(getMSGFpar(testPar@chargeRange), 'character')
    expect_is(getMSGFpar(testPar@matches), 'character')
})

test_that('empty objects return empty strings', {
    expect_equal(getMSGFpar(msgfParTolerance()), '')
    expect_equal(getMSGFpar(msgfParIsotopeError()), '')
    expect_equal(getMSGFpar(msgfParTda()), '')
    expect_equal(getMSGFpar(msgfParFragmentation()), '')
    expect_equal(getMSGFpar(msgfParInstrument()), '')
    expect_equal(getMSGFpar(msgfParEnzyme()), '')
    expect_equal(getMSGFpar(msgfParProtocol()), '')
    expect_equal(getMSGFpar(msgfParNtt()), '')
    expect_equal(getMSGFpar(msgfParModificationList()), '')
    expect_equal(getMSGFpar(msgfParLengthRange()), '')
    expect_equal(getMSGFpar(msgfParChargeRange()), '')
    expect_equal(getMSGFpar(msgfParMatches()), '')
})

test_that('empty msgfPar object throws error', {
    expect_error(getMSGFpar(msgfPar()), 'Cannot get parameters for an empty msgfPar object')
})

test_that('msgfParTolerance behaves', {
    expect_equal(getMSGFpar(msgfParTolerance(20, unit='ppm')), '-t 20ppm')
    expect_equal(getMSGFpar(msgfParTolerance(low=0.5, high=1.5, unit='Da')), '-t 0.5Da,1.5Da')
})

test_that('msgfParIsotopeError behaves', {
    expect_equal(getMSGFpar(msgfParIsotopeError(c(-2,2))), '-ti -2,2')
})

test_that('msgfParTda behaves', {
    expect_equal(getMSGFpar(msgfParTda(TRUE)), '-tda 1')
    expect_equal(getMSGFpar(msgfParTda(FALSE)), '-tda 0')
})

test_that('msgfParFragmentation behaves', {
    expect_equal(getMSGFpar(msgfParFragmentation(0)), '-m 0')
})

test_that('msgfParInstrument behaves', {
    expect_equal(getMSGFpar(msgfParInstrument(0)), '-inst 0')
})

test_that('msgfParEnzyme behaves', {
    expect_equal(getMSGFpar(msgfParEnzyme(0)), '-e 0')
})

test_that('msgfParProtocol behaves', {
    expect_equal(getMSGFpar(msgfParProtocol(0)), '-protocol 0')
})

test_that('msgfParNtt behaves', {
    expect_equal(getMSGFpar(msgfParNtt(2)), '-ntt 2')
})

#
#### Change in path during testing so cannot test output
#
# unlink(system.file('modification_temp.txt', package='MSGFplus'), force=TRUE)
# test_that('msgfParModificationList behaves', {
#     expect_equal(getMSGFpar(testPar@modification), paste('-mod', system.file('modification_temp.txt', package='MSGFplus')))
#     expect_true(file.exists(system.file('modification_temp.txt', package='MSGFplus')))
# })

test_that('msgfParLengthRange behaves', {
    expect_equal(getMSGFpar(msgfParLengthRange(c(6, 40))), '-minLength 6 -maxLength 40')
})

test_that('msgfParChargeRange behaves', {
    expect_equal(getMSGFpar(msgfParChargeRange(c(2, 4))), '-minCharge 2 -maxCharge 4')
})

test_that('msgfParMatches behaves', {
    expect_equal(getMSGFpar(msgfParMatches(2)), '-n 2')
})