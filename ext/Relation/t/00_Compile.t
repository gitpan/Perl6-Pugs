#!/usr/bin/pugs
use v6;

use Test;

plan( 2 );

#use Relation; pass "(dummy instead of broken use_ok)";
flunk( q{use Relation ... prerequisites still missing}, :todo<feature> );
skip( 1, q{is( Relation.meta.identifier.version, 0.0.1,
    'Relation is the correct version' );} );