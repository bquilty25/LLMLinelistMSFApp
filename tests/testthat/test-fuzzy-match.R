test_that("fuzzy_match handles list-style case ids", {
    expect_true(fuzzy_match("ABC001, DEF002", "DEF002, ABC001", "contacts"))
    expect_false(fuzzy_match("ABC001", "XYZ999", "contacts"))
    expect_true(fuzzy_match("[]", "", "contacts"))
})

test_that("fuzzy_match handles order-insensitive names", {
    expect_true(fuzzy_match("Jane Doe", "Doe Jane", "name"))
    expect_false(fuzzy_match("Jane Doe", "Alice Smith", "name"))
})

test_that("fuzzy_match handles exact cleaned text comparisons", {
    expect_true(fuzzy_match("Village-A", "village a", "residence_village"))
    expect_false(fuzzy_match("Village-A", "Village-B", "residence_village"))
})
