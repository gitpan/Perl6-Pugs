#!/usr/bin/pugs
use v6;

use HTTP::Date <str2time time2str>;
use HTTP::Headers::Util <split_header_words join_header_words>;

class HTTP::Cookies-0.0.1 {
    ## Class variables
    our $EPOCH_OFFSET;
    
    ## Attributes
    has %:cookies           is rw;
    
    has $.file              is rw;
    has $.autosave          is rw;
    has $.ignore_discard    is rw;
    has $.hide_cookie2      is rw;
    
    $EPOCH_OFFSET = 0; # difference from Unix epoch
    
    if ($*OS eq "MacOS") {
        require Time::Local;
        $EPOCH_OFFSET = Time::Local::timelocal(0,0,0,1,0,70);
    }
    
    ## Creation and destruction
    submethod BUILD (Str $.file, Bool ?$.autosave = 0, Bool ?$.ignore_discard = 0, Bool ?$.hide_cookie2 = 0) {
        ./load();
    }
    
    submethod DESTROY () {
        ./save() if $.autosave;
    }
    
    ## Instance methods
    method add_cookie_header ($request) {
        ...
    }
    
    method extract_cookies ($response) {
        ...
    }
    
    # XXX lots of potential `where /.../` clauses here :-)
    method set_cookie (Num $version, Str $key, Str $val, Str $path, Str $domain, Str ?$port, Bool ?$path_spec = bool::false, Bool ?$secure = bool::false, Num ?$maxage, Bool ?$discard = bool::false, *%rest) {
        return $?SELF if $path !~ m,^/, || $key ~~ m,^\$,;
        
        if $port.defined {
            return $?SELF unless $port ~~ m:P5/^_?\d+(?:,\d+)*/;
        }
        
        my $expires;
        
        if $maxage.defined {
            if $maxage <= 0 {
                %:cookies{$domain}{$path}.delete($key);
                return $?SELF;
            }
            
            $expires = time() + $maxage;
        }
        
        my @array = ($version, $val, $port, $path_spec, $secure, $expires, $discard);
        @array.push(*%rest) if %rest.keys;
        
        @array.pop while !defined @array[-1];
        
        %:cookies{$domain}{$path}{$key} = \@array;
        return $?SELF;
    }
    
    method set_cookie_ok (*@_) { 1; }
    
    method save (Str ?$file = $.file) {
        my $fh = open($file, :w);
        
        $fh.say("#LWP-Cookies-1.0");
        $fh.print(./as_string(!$.ignore_discard));
        $fh.close;
        
        1;
    }
    
    method load (Str ?$file = $.file) {
        my $fh = open($file, :r) or return;
        
        # XXX ensure record seperator == "\n" -- how?
        my $magic = =$fh;
        
        unless ($magic ~~ m:P5/^\#LWP-Cookies-(\d+\.\d+)/) {
            warn "$file does not seem to contain cookies";
            return;
        }
    
        for (=$fh) {
            next unless s/^Set-Cookie3\:\s*//;
            chomp;
            
            for split_header_words($_) -> @cookie {
                my ($key, $val) = @cookie.splice(0, 2);
                
                my %hash = @cookie;
                
                my $version = %hash.delete('version');
                my $path    = %hash.delete('path');
                my $domain  = %hash.delete('domain');
                my $port    = %hash.delete('port');
                my $expires = str2time(%hash.delete('expires'));
                
                my $path_spec = %hash.exists('path_spec'); %hash.delete('path_spec');
                my $secure    = %hash.exists('secure');    %hash.delete('secure');
                my $discard   = %hash.exists('discard');   %hash.delete('discard');
                
                my @array = ($version, $val, $port, $path_spec, $secure, $expires, $discard);
                push @array, %hash if %hash;
                %:cookies{$domain}{$path}{$key} = @array;
            }
        }

        $fh.close;
        1;
    }
    
    method revert () {
        ./clear.load;
    }
    
    multi method clear () {
        %:cookies = ();
        
        $?SELF;
    }
    
    multi method clear (*@_) {
        if (@_ == 1) {
            %:cookies.delete(@_[0]);
        } elsif (@_ == 2) {
            %:cookies{@_[0]}.delete(@_[1]);
        } elsif (@_ == 3) {
            %:cookies{@_[0]}{@_[1]}.delete(@_[2]);
        }
        
        $?SELF;
    }
    
    method clear_temporary_cookies () {
        ./scan(sub (*@_) { if (@_[9]) || (!@_[8].defined) { @_[8] = -1; ./set_cookie(*@_); } });
    }
    
    method scan (Code $callback) {
        for %:cookies.keys.sort -> $domain {
            for %:cookies{$domain}.keys.sort -> $path {
                for %:cookies{$domain}{$path}.keys.sort -> $key is rw {
                    my ($version, $val, $port, $path_spec, $secure, $expires, $discard, *%rest) := @{$key};
                    %rest //= {};
                    
                    $cb.($version, $key, $val, $path, $domain, $port, $path_spec, $secure, $expires, $discard, *%rest);
                }
            }
        }
    }
    
    method as_string (Bool ?$skip_discardables) {
        # XXX use nested gather/take
        my @ret = (gather {
            ./scan(sub ($version, $key, $val, $path, $domain, ?$port, ?$path_spec, ?$secure, ?$maxage, ?$discard, *%rest) {
                return if $discard && $skip_discardables;
                
                my @h = ($key, $val);
                
                @h.push('path', $path);
                @h.push('domain', $domain);
                @h.push('port', $port) if $port.defined;
                @h.push('path_spec', undef) if $path_spec;
                @h.push('secure', undef) if $secure;
                @h.push('expires', HTTP::Date::time2isoz($expires)) if $expires;
                @h.push('discard' => undef) if $discard;
                
                for %rest.keys.sort -> $k {
                    @h.push($k, %rest{$k});
                }
                
                @h.push('version', $version);
                
                take "Set-Cookie3: " ~ join_header_words(@h);
            });
            take "";
        }).join("\n");
    }
    
    ## Class methods
    # these may also be called on an instance, but they are not tied to a
    # particular instance
    method :host (HTTP::Request $r, URI $uri) {
        if (my $h = $r.header('Host')) {
            $h ~~ s:P5/:\d+$//;
            return $h.lc;
        }
        
        return $uri.host.lc;
    }
    
    method :uri_path (URI $uri) {
        my $path;
        
        if ($uri.can('epath')) {
            $path = $uri.epath; # URI::URL method
        } else {
            $path = $uri.path;  # URI::_generic method
        }
        
        $path.chars || $path = "/";
        return $path;
    }
    
    # XXX how should this binding be done?
    #our &:url_path ::= &:uri_path; # for backwards compatibility
    
    method :normalize_path (Str $str) {
        given ($str) {
            s:P5:g/%([0-9a-fA-F][0-9a-fA-F])/{
                my $x = $0.uc;
                $x eq "2F"|"25" ?? "%$x" :: pack("C", hex($x));
            }/;
            s:P5:g/([\0-\x20\x7f-\xff])/{ ord($0).as('%%%02X') }/;
        }
    }
}