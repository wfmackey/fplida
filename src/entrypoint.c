// Forward routine registration from C to Rust, so the linker does not strip
// the registration symbol.
//
// R.h and Rinternals.h are deliberately not included. Nothing here uses an R
// type, and including them required a #pragma to silence a clang diagnostic,
// which R CMD check reports as "pragmas suppressing diagnostics".

void R_init_fplida_extendr(void *dll);

void R_init_fplida(void *dll) {
    R_init_fplida_extendr(dll);
}
