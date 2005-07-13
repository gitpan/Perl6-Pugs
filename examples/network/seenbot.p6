#!/usr/bin/pugs

use v6;
use Net::IRC;

# Parse @*ARGS
my $nick   = @*ARGS[0] // "blechbot";
my $server = @*ARGS[1] // "localhost";
my ($host, $port) = split ":", $server;
$port //= 6667;

my %seen;
my @chans;

# Create new bot "object"
my $bot = new_bot(nick => $nick, host => $host, port => $port, debug_raw => 1);
$bot<connect>();
$bot<login>();
$bot<add_handler>("INVITE",   &on_invite);
$bot<add_handler>("PRIVMSG",  &on_privmsg);
$bot<add_handler>("loggedin", &on_loggedin);
$bot<run>();

sub on_loggedin($event) {
  $bot<join>($_) for @chans;
}

sub on_invite($event) {
  my ($from, $chan) = $event<from rest>;
  debug "Got an invitation from \"$from\" to join channel \"$chan\".";
  $bot<join>($chan);
}

sub on_privmsg($event) {
  %seen{$event<from_nick>} = {
    date => time,
    text => $event<rest>,
  };

  given $event<rest> {
    debug "Received a ?-request from $event<from>: $event<rest>"
      if substr($event<rest>, 0, 1) eq "?";

    my $reply_to = substr($event<object>, 0, 1) eq "#" ?? $event<object> :: $event<from_nick>;

    when rx:P5/^\?seen\s+(.+)$/ {
      my $reply_msg = %seen{$0}
	?? "$0 was last seen {int(time() - %seen{$0}<date>)} seconds ago, saying: %seen{$0}<text>"
	:: "Never seen $0.";
      $bot<notice>(to => $reply_to, text => $reply_msg);
    }

    when rx:P5/^\?quit\s*(.*)$/ {
      $bot<quit>($0);
    }

    when rx:P5/^\?raw\s+(.+)$/ {
      $bot<raw>($0);
    }

    when rx:P5/^\?uptime$/ {
      my $start_time = INIT { time };
      $bot<notice>(to => $reply_to, text => "Running for {int(time() - $start_time)} seconds.");
    }

    when rx:P5/^\?sleep\s+(\d+)$/ {
      sleep $0;
    }

    when rx:P5/^\?reconnect/ {
      @chans = $bot<channels>();
      $bot<reconnect>();
      $bot<login>();
    }

    # This is *not* correct CTCP parsing (the CTCP specification is much more
    # complex than this simple regex), but IRC clients only send this when
    # their users enter /PING bot.
    when rx:P5/^\001PING (.*)\001$/ {
      $bot<notice>(to => $event<from_nick>, text => "\001PING $0\001");
    }
  }
}
