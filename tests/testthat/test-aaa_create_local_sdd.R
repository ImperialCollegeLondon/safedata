test_that("Test that the fixture generating the locally mocked SDD works", {

    local_sdd <- local_create_sdd()

    expect_type(local_sdd, "character")
    expect_true(dir.exists(local_sdd))
    expect_true(file.exists(file.path(local_sdd, "index.json")))

})