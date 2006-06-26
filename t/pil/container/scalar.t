#!/usr/bin/pugs

use v6;
use Test::PIL::Bootstrap;

check_pil();

pil_is_eq(
    '^Scalar.identifier()', 
    '"Scalar-0.0.1-url:pugscode.org"', 
    '... Scalar has the correct identifier');

pil_is_eq('^Scalar.has_method("FETCH")', 'true', '... ^Scalar.has_method(FETCH)');
pil_is_eq('^Scalar.has_method("STORE")', 'true', '... ^Scalar.has_method(STORE)');

pil_is_eq('^Scalar.does("Scalar")', 'true', '... ^Scalar.does(Scalar)');