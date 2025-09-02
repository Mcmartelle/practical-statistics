def main [] {
  let js = "elm.js"
  let min = "elm.min.js"
  elm make --optimize --output=elm.js src/Main.elm
  uglifyjs elm.js --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" | uglifyjs --mangle --output elm.min.js
  if $env.LAST_EXIT_CODE == 0 {
    success
    ls elm*.js #Showing the minified size difference
  } else {
    failure
  }
}
