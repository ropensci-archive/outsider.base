context('Testing \'hello world\'')
test_that('hello world module works', {
  skip_if(!docker_available)
  pkg_install(flpth = mdl_flpth)
  image_install(pkgnm = pkgnm)
  expect_true(om..hello.world::hello_world())
  uninstall(pkgnm = pkgnm)
})
