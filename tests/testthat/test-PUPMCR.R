test_that("Image analysis works", {
  expect_no_error(raynercolor.LAB(system.file("fungi.png", package = "PUPMCR")))
  expect_no_error(raynercolor.RGB(system.file("fungi.png", package = "PUPMCR")))
  expect_no_error(hue.LAB(system.file("fungi.png", package = "PUPMCR")))
  expect_no_error(hue.RGB(system.file("fungi.png", package = "PUPMCR")))
  expect_no_error(fungalpigments.LAB(system.file("fungi.png", package = "PUPMCR")))
  expect_no_error(fungalpigments.RGB(system.file("fungi.png", package = "PUPMCR")))
})
