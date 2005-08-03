# The lines like
#   die unless $self->[...]->isa(...)
# make sure that PIL::Parser gave us correct nodes.
package PIL::PStmts;

use warnings;
use strict;

sub fixup {
  die unless @{ $_[0] } == 2;

  return bless [
    $_[0]->[0]->fixup,
    $_[0]->[1]->fixup,
  ] => "PIL::PStmts";
}

sub as_js {
  my $self = shift;

  # Update $?POSITION.
  my $pos =
    sprintf "_24main_3a_3a_3fPOSITION.STORE(new PIL2JS.Box.Constant(%s))",
    PIL::doublequote $PIL::CUR_POS;

  # Add a &return() to the last statement of a sub.
  if($PIL::IN_SUBLIKE and $self->[1]->isa("PIL::PNil")) {
    my $js = $self->[0]->as_js;
    # Note: Purely cosmetical hacking on the generated JS! (else it would be
    # eevil).
    $js =~ s/\n$//;
    if($PIL::IN_SUBLIKE >= PIL::SUBROUTINE) {
      return "$pos;\n_26main_3a_3areturn.FETCH()([PIL2JS.Context.ItemAny, $js]);";
    } elsif($PIL::IN_SUBLIKE >= PIL::SUBBLOCK) {
      return "$pos;\n_26main_3a_3aleave.FETCH()([PIL2JS.Context.ItemAny, $js]);";
    } else {
      # !!! *Never* do a native JS return(), as it defeats var restore.
      return "$pos;\n_26PIL2JS_3a_3aInternals_3a_3asmallreturn.FETCH()([PIL2JS.Context.ItemAny, $js]);";
    }
  } else {
    my @js = ($self->[0]->as_js, $self->[1]->as_js);
    $js[0] =~ s/\n$//;
    return "$pos;\n$js[0];\n$js[1]";
  }
}

1;