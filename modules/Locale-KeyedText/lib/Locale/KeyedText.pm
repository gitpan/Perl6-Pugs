#!pugs
use v6;

######################################################################

=head1 NAME

Locale::KeyedText - Refer to user messages in programs by keys

=head1 DEPENDENCIES

Perl Version: 6

Core Modules: I<none>

Non-Core Modules: I<none>

=head1 COPYRIGHT AND LICENSE

Locale::KeyedText is Copyright (c) 1999-2005, Darren R. Duncan.  All rights
reserved.  Address comments, suggestions, and bug reports to
B<perl@DarrenDuncan.net>, or visit "http://www.DarrenDuncan.net" for more
information.

Locale::KeyedText is free software; you can redistribute it and/or modify it
under the terms of the GNU Lesser General Public License (LGPL) version 2.1 as
published by the Free Software Foundation (http://www.fsf.org/).  You should
have received a copy of the LGPL as part of the Locale::KeyedText distribution,
in the file named "LICENSE"; if not, write to the Free Software Foundation,
Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.

Any versions of Locale::KeyedText that you modify and distribute must carry
prominent notices stating that you changed the files and the date of any
changes, in addition to preserving this original copyright notice and other
credits. Locale::KeyedText is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the LGPL for more details.

While it is by no means required, the copyright holders of Locale::KeyedText
would appreciate being informed any time you create a modified version of
Locale::KeyedText that you are willing to distribute, because that is a
practical way of suggesting improvements to the standard version.

=cut

######################################################################
######################################################################

subtype KeyName of Str where { .defined and m/^\w+$/ }
subtype PkgName of Str where { .defined and m/^<[a-zA-Z0-9_:]>+$/ }

######################################################################
######################################################################

module Locale::KeyedText-0.0.2 {

######################################################################

sub new_message( KeyName $msg_key, Str ?%msg_vars is shape(KeyName) ) returns Locale::KeyedText::Message {
	return Locale::KeyedText::Message.new( $msg_key, %msg_vars );
}

sub new_translator( PkgName @set_names, PkgName @member_names ) returns Locale::KeyedText::Translator {
	return Locale::KeyedText::Translator.new( @set_names, @member_names );
}

######################################################################

} # module Locale::KeyedText

######################################################################
######################################################################

class Locale::KeyedText::Message {
	trusts Locale::KeyedText::Translator;
	has KeyName $:msg_key; # str - the machine-readable key that uniquely identifies this message
	has Str %:msg_vars is shape(KeyName); # hash (str,str) - named variables for messages, if any, go here

######################################################################

method new( $class: KeyName $msg_key, Str ?%msg_vars is shape(KeyName) ) returns Locale::KeyedText::Message {
	return $class.bless( { msg_key => $msg_key, msg_vars => %msg_vars } );
}

######################################################################

method get_message_key( $message: ) returns KeyName {
	return $message.:msg_key;
}

method get_message_variable( $message: KeyName $var_name ) returns KeyName {
	return $message.:msg_vars{$var_name};
}

method get_message_variables( $message: ) returns Hash is shape(KeyName) of Str {
	return {$message.:msg_vars}; # copy list values; new hash ref puts attr in list context, so it flattens
}

######################################################################

method as_string( $message: ) returns Str {
	# This method is intended for debugging use only.
	return $message.:msg_key~': '~$message.:msg_vars.pairs.sort.
		map:{ .key~'='~(.value // '') }.join( ', ' ); # S02 says sorting Pairs sorts keys by default.
	# I might use %hash.as() later, but don't know if it is customizable to sort or make undefs the empty str.
}

######################################################################

} # class Locale::KeyedText::Message

######################################################################
######################################################################

class Locale::KeyedText::Translator {
	has PkgName @:tmpl_set_nms; # array of str - list of Template module Set Names to search
	has PkgName @:tmpl_mem_nms; # array of str - list of Template module Member Names to search

######################################################################

method new( $class: PkgName @set_names, PkgName @member_names ) returns Locale::KeyedText::Translator {
	return $class.bless( { tmpl_set_nms => @set_names, tmpl_mem_nms => @member_names };
}

######################################################################

method get_template_set_names( $translator: ) returns Array of PkgName {
	return [$translator.:tmpl_set_nms]; # copy list values; new array ref puts attr in list context, so it flattens
}

method get_template_member_names( $translator: ) returns Array of PkgName {
	return [$translator.:tmpl_mem_nms]; # copy list values; new array ref puts attr in list context, so it flattens
}

######################################################################

method translate_message( $translator: Locale::KeyedText::Message $message ) returns Str {
	$message.:defined or return;
	my Str $text = undef;
	MEMBER: for $translator.:tmpl_mem_nms -> $member_name {
		SET: for $translator.:tmpl_set_nms -> $set_name {
			my PkgName $template_module_name = $set_name~$member_name;
			unless( $template_module_name.meta.can("get_text_by_key") ) {
				require $template_module_name;
				unless( $template_module_name.meta.can("get_text_by_key") ) {
					next SET;
				}
			}
			try {
				$text = $template_module_name.get_text_by_key( $message.:msg_key );
				CATCH {
					next SET;
				}
			}
			$text or next SET;
			for $message.:msg_vars.kv -> $var_name, $var_value {
				$var_value //= '';
				$text ~~ s:g/\{$var_name\}/$var_value/; # assumes msg props cleaned on input
			}
			last MEMBER;
		}
	}
	return $text;
}

######################################################################

method as_string( $translator: ) returns Str {
	# This method is intended for debugging use only.
	return 'SETS: '~$translator.:tmpl_set_nms.as( '%s', ', ' )~'; MEMBERS: '~
		$translator.:tmpl_mem_nms.as( '%s', ', ' );
}

######################################################################

} # class Locale::KeyedText::Translator

######################################################################
######################################################################

1;

=pod

=head1 SYNOPSIS

The following demonstrates a simple library and a simple program that uses it;
both are N-multi-lingual and ship with English and French support files.  While 
there is no support file specific to the library for a certain third language, 
the one with the program also adds support to the library.

=head2 Content of shared library file 'MyLib.pm':

	module MyLib;

	require Locale::KeyedText;

	sub my_invert( Str $number ) returns Num {
		$number.defined or throw Locale::KeyedText.new_message( 'MYLIB_MYINV_NO_ARG' );
		$number ~~ m/\d/ or throw Locale::KeyedText.new_message( 
			'MYLIB_MYINV_BAD_ARG', { 'GIVEN_VALUE' => $number } );
		$number == 0 and throw Locale::KeyedText.new_message( 'MYLIB_MYINV_RES_INF' );
		return 1 / $number;
	}

=head2 Content of English language Template file 'MyLib/L/Eng.pm':

	module MyLib::L::Eng;

	my Str %text_strings ::= (
		'MYLIB_MYINV_NO_ARG' => 'my_invert(): argument NUMBER is missing',
		'MYLIB_MYINV_BAD_ARG' => 'my_invert(): argument NUMBER is not a number, it is "{GIVEN_VALUE}"',
		'MYLIB_MYINV_RES_INF' => 'my_invert(): result is infinite because argument NUMBER is zero',
	);

	sub get_text_by_key( Str $msg_key ) returns Str { return $text_strings{$msg_key}; }

=head2 Content of French language (rough manual translation) Template file 'MyLib/L/Fre.pm':

	module MyLib::L::Fre;

	my Str %text_strings ::= (
		'MYLIB_MYINV_NO_ARG' => 'my_invert(): paramètre NUMBER est manquant',
		'MYLIB_MYINV_BAD_ARG' => 'my_invert(): paramètre NUMBER est ne nombre, il est "{GIVEN_VALUE}"',
		'MYLIB_MYINV_RES_INF' => 'my_invert(): aboutir a est infini parce que paramètre NUMBER est zero',
	);

	sub get_text_by_key( Str $msg_key ) returns Str { return $text_strings{$msg_key}; }

=head2 Content of main program 'MyApp.pl':

	require MyLib;
	require Locale::KeyedText;

	main( grep { $_ ~~ m/^<[a-zA-Z]>+$/ } @*ARGS ); # user indicates language as command line argument

	sub main( Str ?@user_lang_prefs = 'Eng' ) {
		my Locale::KeyedText::Translator $translator = Locale::KeyedText.new_translator( 
			['MyApp::L::', 'MyLib::L::'], @user_lang_prefs );

		show_message( $translator, Locale::KeyedText.new_message( 'MYAPP_HELLO' ) );

		LOOP: {
			show_message( $translator, Locale::KeyedText.new_message( 'MYAPP_PROMPT' ) );
			my Str $user_input = $*IN; $user_input.chomp;

			$user_input or last LOOP; # user chose to exit program

			try {
				my Num $result = MyLib.my_invert( $user_input );
				show_message( $translator, Locale::KeyedText.new_message( 'MYAPP_RESULT', 
					{ 'ORIGINAL' => $user_input, 'INVERTED' => $result } ) );
				CATCH {
					show_message( $translator, $! ); # input error, detected by library
				}
			};

			redo LOOP;
		}

		show_message( $translator, Locale::KeyedText.new_message( 'MYAPP_GOODBYE' ) );
	}

	sub show_message( Locale::KeyedText::Translator $translator, Locale::KeyedText::Message $message ) returns Str {
		my Str $user_text = $translator.translate_message( $message );
		unless( $user_text ) {
			print $*ERR "internal error: can't find user text for a message: \n"~
				"   "~$message.as_string()~"\n"~
				"   "~$translator.as_string()~"\n";
			exit;
		}
		print $*OUT $user_text~"\n";
	}

=head2 Content of English language Template file 'MyApp/L/Eng.pm':

	module MyApp::L::Eng;

	my Str %text_strings ::= (
		'MYAPP_HELLO' => 'Welcome to MyApp.',
		'MYAPP_GOODBYE' => 'Goodbye!',
		'MYAPP_PROMPT' => 'Enter a number to be inverted, or press ENTER to quit.',
		'MYAPP_RESULT' => 'The inverse of "{ORIGINAL}" is "{INVERTED}".',
	);

	sub get_text_by_key( Str $msg_key ) returns Str { return $text_strings{$msg_key}; }

=head2 Content of French language (rough manual translation) Template file 'MyApp/L/Fre.pm':

	module MyApp::L::Fre;

	my Str %text_strings ::= (
		'MYAPP_HELLO' => 'Bienvenue allé MyApp.',
		'MYAPP_GOODBYE' => 'Salut!',
		'MYAPP_PROMPT' => 'Fournir nombre être inverser, ou appuyer sur ENTER être arrêter.',
		'MYAPP_RESULT' => 'Renversement "{ORIGINAL}" est "{INVERTED}".',
	);

	sub get_text_by_key( Str $msg_key ) returns Str { return $text_strings{$msg_key}; }

=head2 Content of alternate text Template file 'MyApp/L/Homer.pm':

	module MyApp::L::Homer;

	my Str %text_strings ::= (
		'MYAPP_HELLO' => 'Light goes on!',
		'MYAPP_GOODBYE' => 'Light goes off!',
		'MYAPP_PROMPT' => 'Give me a county thingy, or push that big button instead.',
		'MYAPP_RESULT' => 'Turn "{ORIGINAL}" upside down and get "{INVERTED}", not "{ORIGINAL}".',
		'MYLIB_MYINV_NO_ARG' => 'Why you little ...!',
		'MYLIB_MYINV_BAD_ARG' => '"{GIVEN_VALUE}" isn\'t a county thingy!',
		'MYLIB_MYINV_RES_INF' => 'Don\'t you give me a big donut!',
	);

	sub get_text_by_key( Str $msg_key ) returns Str { return $text_strings{$msg_key}; }

=head1 DESCRIPTION

Many times during a program's operation, the program (or a module it uses) will
need to display a message to the user, or generate a message to be shown to the
user.  Often this is an error message of some kind, but it could also be a
prompt message for interactive systems.

Modern programs or database systems often refer to an error condition by an
internal code which is guaranteed to be unique for a situation, and this is
mapped to a user-readable message at some point.  For example, Oracle databases
often have error codes in a format like 'ORA-03542'.  These codes are "machine
readable"; any application receiving such a code can identify it easily in its
conditional logic, using a simple 'equals', and then the application can "do
the right thing".  No parsing or ambiguity involved.  By contrast, if a program
simply returned words for the user, such as 'error opening file', programs
would have a harder time figuring out the best way to deal with it.  But for
displaying to users, easy messages are better.  Also, different users have
their own languages, and users with either different skill levels or different
security privileges, should be shown different messages for the same error
conditions.  Developers get more details, joe public gets very little.

I have found that when it comes to getting the most accurate program text for
users, we still get the best results by having a human being write out that
text themselves.

What Locale::KeyedText does is associate each member in a set of key-codes,
which are hard-coded into your application or module, with one or more text
strings to show human users.  This association would normally be stored in a
Perl file that defines and returns an anonymous hash definition.  While it is
obvious that people who would be writing the text would have to know how to
edit Perl files, this shouldn't be a problem because Locale::KeyedText is only
meant to be used with user text that is associated with hard-coded program
conditions.  In other words, this user text is *part of the program*, and not
the program's users' own data; only someone already involved in making the
program would be editing them.  At the same time, this information is in
separate resource files used by the program, so that if you wanted to upgrade
or localize what text the user sees, you only have to update said separate
resource files, and not change your main program.

I was inspired to have this organization partly by how Mac OS X manages its
resources.  It is the standard practice for Mac OS X programs, including the
operating system itself, to have the user language data in separate files
(usually XML files I think) from the main program binary.  Each user language
is in a separate file, and adding a localization to a Mac OS X program is as
simple as adding a language file to the program package.  No recompilation
necessary.  This is something that end users could do, although program package
installers usually do it.  An os-level preference / control-panel displays a
list of all the languages your programs do or might have, and lets you arrange
the list in order of preference.  When you open a program, it will search for
language files specific to the program in the order you chose so to pick a
supported language closest to your preference.  Presumably the messages in
these files are looked up by the program using keys.  Mac OS X (and the
previous non-Unix Mac OS) handles lots of other program resources as data files
as well, making them easy to upgrade.

Locale::KeyedText aims to bring this sort of functionality to Perl modules or
programs. Your module or program can be distributed with one or more resource
files containing text for users, and your program would use associated keys
internally.

It is strongly suggested (but not required) that each Perl module which uses
this would come up with keys which are unique across all Perl modules (perhaps
the key name can start with the module name?).  An advantage of this is that,
for example, your module could come with a set of user messages, but another
module or program which uses yours may wish to override some of your messages,
showing other messages instead which are more appropriate to the context in
which they are using your module.  One can override simply by using the same
key code with a new user message in one of their own resource files.  At some
appropriate place, usually in the main program, Locale::KeyedText can be given
input that says what resource files it should use and in what order they should
be consulted.  When Locale::KeyedText is told to fetch the user message for a
certain code, it returns the first one it finds.  This also works for the
multiple language or permissions issue; simply order the files appropriately in
the search list.  The analogy is similar to inheriting from multiple modules
which have the same method names as you or each other, or having multiple
search directories in your path that modules could be installed in.

Generally, when a program module would return a code-key to indicate a
condition, often it will also provide some variable values to be interpolated
into the user strings; Locale::KeyedText would also handle this.

Locale::KeyedText never outputs anything by itself to the user, but rather
returns its results for calling code to output as it sees fit.

One of the main distinctions of this approach over similar modules is that text
is always looked up by a key which is not meant to be meaningful for a user.
Whereas, with the other modules like "gettext" it looks like you are supposed
to pass in english text and they translate it, which could produce ambiguous
results or associations.  Or alternately, the other modules require your text
data to be stored in a format other than Perl files.  Or alternately they have
a compiled C component or otherwise have external dependencies;
Locale::KeyedText has no external dependencies (it is very simple).  There are
other differences.  Where other solutions take variables, they seem to be
positional (like with 'sprintf'); whereas, Locale::KeyedText has named
variables, which can be used in any order, or not used at all, or used multiple
times.  Locale::KeyedText is generally a simpler solution than alternatives,
and doesn't know about language specific details like encodings or plurality.

For some practical examples of Locale::KeyedText in use, see my dependent CPAN
modules whose problem domain is databases and/or SQL.

=head1 CLASSES IN THIS MODULE

This module is implemented by several object-oriented Perl 5 packages, each of
which is referred to as a class.  They are: B<Locale::KeyedText> (the module's
name-sake), B<Locale::KeyedText::Message> (aka B<Message>), and
B<Locale::KeyedText::Translator> (aka B<Translator>).

I<While all 3 of the above classes are implemented in one module for
convenience, you should consider all 3 names as being "in use"; do not create
any modules or packages yourself that have the same names.>

The Message and Translator classes do most of the work and are what you mainly
use.  The name-sake class mainly exists to guide CPAN in indexing the whole
module, but it also provides a few wrapper functions over the other classes for
your convenience; you never instantiate an object of Locale::KeyedText itself.

=head1 MESSAGE OBJECT PROPERTIES

One object type that this module implements is the B<Message>.  It is a simple
container which stores data to be used or displayed by your program.  Using it
has no side effects on your program's environment or its globals; it also has no 
external dependencies.  

A B<Message> object has two main properties:

=over 4

=item

B<Message Key> - A short string which identifies what base message (or type of
message) this object is an instance of.  The key is intended to be read by a
machine and mapped to a user-readable message; the key itself is not meant to
be meaningful to a user.  Alternately, if you decide to use Message objects like 
Exceptions, then the key would indicate what condition is being reported.

=item

B<Message Variables> - An associative array (hash ref) containing variable
names and values that are associated with this Message instance, and can be
interpolated into the human-readable version.  Each variable name is a
machine-readable short string; the allowed variable names you can have depend
on the Message Key it is being used with (others are ignored).  Each variable
value should be a scalar of some kind.

=back

Both a Message object's Message Key property and each of the keys in its
Message Variables property must not be an empty string and may only contain
Perl "word" (\w) characters.

=head1 TEMPLATE OBJECT PROPERTIES

Locale::KeyedText doesn't define any "Template" objects, but it expects you to 
make modules having a specific simple API that will serve their role.  See the 
SYNOPSIS POD for examples of valid Template modules.

A Template module is very simple, consisting mainly of a data-stuffed hash and
an accessor method to read values from it by key.  Each template hash key
corresponds to the Message Key property of a Message object, and each hash
value contains the user-readable message text associated with the Message; this
user string may also contain variable names that correspond to Message
Variables, which will be substituted at run-time before the text is shown to
the user.

Each Template module ideally comes as part of a set, at least one member large,
with each set member being an an exclusive alternative for the rest of the set.
There is a separate template module for each distinct "user language" (or "user
type") for each distinct Message; each file can be shared by multiple Messages
but the whole module must represent a single language.

The name of each Template module has two parts, the Set Name and the Member
Name.  The Set Name comes first and makes up most of the module name; it must
be the same for every module in the same set as the current one.  The Member
Name comes next and is what distinguishes each module from others in its set.
For maximum flexibility in their use, the full name of a module consists of the
two parts concatenated without any delimiter.  This means, for example, that
the full module names in a set could be either [Foo::L::Eng, Foo::L::Fre,
Foo::L::Ger] or [L::FooEng, L::FooFre, L::FooGer]; the latter is mainy useful 
if you want modules from multiple sets in the same disk directory.  In the 
first example, the Set Name is "Foo::L::" and in the second it is "L::Foo".

A library could be distributed with a Template module set that is specific to
it, another library likewise, and a program which uses both libraries could
have yet another set for itself.  When the program is run, it would determine
either from a user config file or a user interface that the current user is
fluent in (and prefers) language A but also understands language B.  Later on,
if for example the first library generates an error message and wants it shown
to the user, the main program would check each of the 3 Template module sets in
turn, looking at just the set member for each that corresponds to language A,
looking for a match to said error message.  If it finds one, then that is
displayed; if not, it then checks each set's member for language B and displays
that; and so on.

I<For the present, Locale::KeyedText expects its Template objects to come from
Perl modules, but in the future they may alternately be something else, such as
XML files.>

=head1 TRANSLATOR OBJECT PROPERTIES

Another object type that this module implements is the B<Translator>.  While it
stores some properties for configuration, its main purpose is to convert
Message objects on demand into user-readable message strings, using data from 
external Template objects as a template.

A B<Translator> object has 2 main properties:

=over 4

=item

B<Template Sets> - An ordered array where each element is a Template module Set
Name.  When we have to translate a message, the corresponding Template modules
will be searched in the order they appear in this array until a match for that
message is found.  Since a program or library may wish to override the user
text of another library which it uses, the Template module for the program or
first library should appear first in the array.  This property is analogous to 
Perl's @ISA package variable.

=item

B<Template Members> - An ordered array where each element is a Template module
Member Name and usually corresponds to a language like English or French.  The
order of these items corresponds to an individual user's (or user role's)
preferences such that each says what language they prefer to communicate in,
and what their backup choices are, in order, if preferred ones aren't
supported by a program or its libraries.  When translating a message, a match 
in found in the most preferred language is used.

=back

Each of a Translator object's Template Sets and Template Members property
elements must not be an empty string and may only contain characters that are
valid in a Perl package name; the code presently disallows any characters that
are not in [a-zA-Z0-9_:].

=head1 SYNTAX

This class does not export any functions or methods, so you need to call them
using object notation.  This means using B<Class-E<gt>function()> for functions
and B<$object-E<gt>method()> for methods.  If you are inheriting this class for
your own modules, then that often means something like B<$self-E<gt>method()>.  

=head1 CONSTRUCTOR WRAPPER FUNCTIONS

These functions are stateless and can be invoked only off of the module name;
they are thin wrappers over other methods and exist strictly for convenience.

=head2 new_message( MSG_KEY[, MSG_VARS] )

	my $message = Locale::KeyedText.new_message( 'INVALID_FOO_ARG', 
		{ 'ARG_NAME' => 'BAR', 'GIVEN_VAL' => $bar_value } );

This function wraps Locale::KeyedText::Message.new( MSG_KEY[, MSG_VARS] ).

=head2 new_translator( SET_NAMES, MEMBER_NAMES )

	my $translator = Locale::KeyedText.new_translator( 
		['Foo::L::','Bar::L::'], ['Eng', 'Fre', 'Ger'] );

This function wraps Locale::KeyedText::Translator.new( SET_NAMES, MEMBER_NAMES ).

=head1 MESSAGE CONSTRUCTOR FUNCTIONS AND METHODS

This function/method is stateless and can be invoked off of either the Message
class name or an existing Message object, with the same result.

=head2 new( MSG_KEY[, MSG_VARS] )

	my $message = Locale::KeyedText::Message.new( 'FOO_GOT_NO_ARGS' );
	my $message2 = Locale::KeyedText::Message.new( 'INVALID_FOO_ARG', 
		{ 'ARG_NAME' => 'BAR', 'GIVEN_VAL' => $bar_value } );
	my $message3 = $message.new( 'TABLE_NO_EXIST', { 'GIVEN_TABLE_NAME' => $table_name } );
	my $message4 = Locale::KeyedText::Message.new( 'TABLE_COL_NO_EXIST', 
		{ 'GIVEN_TABLE_NAME' => $table_name, 'GIVEN_COL_NAME' => $col_name } );

This function creates a new Locale::KeyedText::Message object and returns it,
assuming the method arguments are valid; if they are not, it returns undef. The
Message Key property of the new object is set from the MSG_KEY string argument;
the optional MSG_VARS hash ref argument sets the "Message Variables" property
if provided (it defaults to empty if the argument is undefined).

=head1 MESSAGE OBJECT METHODS

These methods are stateful and may only be invoked off of Message objects.

=head2 get_message_key()

	my $msg_key = $message.get_message_key();

This method returns the Message Key property of this object.

=head2 get_message_variable( VAR_NAME )

	my $value = $message.get_message_variable( 'GIVEN_COL_NAME' );

This method returns the Message Variable value associated with the variable
name specified in VAR_NAME.

=head2 get_message_variables()

	my %msg_vars = $message.get_message_variables();

This method returns all Message Variable names and values in this object as a
hash ref.

=head1 TRANSLATOR CONSTRUCTOR FUNCTIONS AND METHODS

This function/method is stateless and can be invoked off of either the
Translator class name or an existing Translator object, with the same result.

=head2 new_translator( SET_NAMES, MEMBER_NAMES )

	my $translator = Locale::KeyedText::Translator.new( 
		['Foo::L::','Bar::L::'], ['Eng', 'Fre', 'Ger'] );
	my $translator2 = $translator.new( 'Foo::L::', 'Eng' );

This function creates a new Locale::KeyedText::Translator object and returns
it, assuming the method arguments are valid; if they are not, it returns undef.
The Template Sets property of the new object is set from the SET_NAMES array
ref (or string) argument, and Template Members is set from MEMBER_NAMES.

=head1 TRANSLATOR OBJECT METHODS

These methods are stateful and may only be invoked off of Message objects.

=head2 get_template_set_names()

	my @set_names = $translator.get_template_set_names();

This method returns all Template Sets elements in this object as an array ref.

=head2 get_template_member_names()

	my @set_names = $translator.get_template_member_names();

This method returns all Template Members elements in this object as an array ref.

=head2 translate_message( MESSAGE )

	my $user_text_string = $translator.translate_message( $message );

This method takes a (machine-readable) Message object as its MESSAGE argument
and returns an equivalent human readable text message string; this assumes that
a Template corresponding to the Message could be found using the Translator
object's Set and Member properties; if none could be matched, this method
returns undef.  This method could be considered to implement the 'main'
functionality of Locale::KeyedText.

=head1 METHODS FOR DEBUGGING

These methods are stateful and may only be invoked off of either Message or
Translator objects.

=head2 as_string()

	my $dump_string = $message.as_string();
	my $dump_string = $translator.as_string();

This method returns a stringified version of this object which is suitable for
debugging purposes (such as to test that the object's contents look good at a
glance); no property values are escaped and you shouldn't try to extract them.

=head1 AN ALTERNATIVE TO SEPARATED TEMPLATE FILES

A feature extension to Locale::KeyedText allows you to store your Template
class packages inside the same files as your other program code, rather than
the Templates being in their own files.  This feature is in recognition to
developers that want to reduce as much as possible the number of separate files
in their program distribution, at the cost of not being able to update user
text or add support for new languages separately from updating the program code
files themselves (one of Locale::KeyedText's original design principles).

Keep in mind that both methods of storing Template class packages can be used
at the same time.  Translator.translate_message() will first check for an
embedded package by the appropriate name and use that if it exists; if one does
not then it will try to use the external file, as is standard practice.

The following is an altered version of the SYNOPSIS documentation that shows
Template class packages for MyLib and MyApp embedded in the code files rather
than being separate; this example totals 3 files instead of the old 7 files.  
Actually, it shows both methods together, with 4 embedded, 1 separate.

=head2 Content of shared library file 'MyLib.pm':

	module MyLib {
		require Locale::KeyedText;
		sub my_invert( Str $number ) returns Num {
			$number.defined or throw Locale::KeyedText.new_message( 'MYLIB_MYINV_NO_ARG' );
			$number ~~ m/\d/ or throw Locale::KeyedText.new_message( 
				'MYLIB_MYINV_BAD_ARG', { 'GIVEN_VALUE' => $number } );
			$number == 0 and throw Locale::KeyedText.new_message( 'MYLIB_MYINV_RES_INF' );
			return 1 / $number;
		}
	}

	module MyLib::L::Eng {
		my Str %text_strings ::= (
			'MYLIB_MYINV_NO_ARG' => 'my_invert(): argument NUMBER is missing',
			'MYLIB_MYINV_BAD_ARG' => 'my_invert(): argument NUMBER is not a number, it is "{GIVEN_VALUE}"',
			'MYLIB_MYINV_RES_INF' => 'my_invert(): result is infinite because argument NUMBER is zero',
		);

		sub get_text_by_key( Str $msg_key ) returns Str { return $text_strings{$msg_key}; }
	}

	module MyLib::L::Fre {
		my Str %text_strings ::= (
			'MYLIB_MYINV_NO_ARG' => 'my_invert(): paramètre NUMBER est manquant',
			'MYLIB_MYINV_BAD_ARG' => 'my_invert(): paramètre NUMBER est ne nombre, il est "{GIVEN_VALUE}"',
			'MYLIB_MYINV_RES_INF' => 'my_invert(): aboutir a est infini parce que paramètre NUMBER est zero',
		);
		sub get_text_by_key( Str $msg_key ) returns Str { return $text_strings{$msg_key}; }
	}

=head2 Content of main program 'MyApp.pl':

	require MyLib;
	require Locale::KeyedText;

	main( grep { $_ ~~ m/^<[a-zA-Z]>+$/ } @*ARGS ); # user indicates language as command line argument

	sub main( Str ?@user_lang_prefs = 'Eng' ) {
		my Locale::KeyedText::Translator $translator = Locale::KeyedText.new_translator( 
			['MyApp::L::', 'MyLib::L::'], @user_lang_prefs );

		show_message( $translator, Locale::KeyedText.new_message( 'MYAPP_HELLO' ) );

		LOOP: {
			show_message( $translator, Locale::KeyedText.new_message( 'MYAPP_PROMPT' ) );
			my Str $user_input = $*IN; $user_input.chomp;

			$user_input or last LOOP; # user chose to exit program

			try {
				my Num $result = MyLib.my_invert( $user_input );
				show_message( $translator, Locale::KeyedText.new_message( 'MYAPP_RESULT', 
					{ 'ORIGINAL' => $user_input, 'INVERTED' => $result } ) );
				CATCH {
					show_message( $translator, $! ); # input error, detected by library
				}
			};

			redo LOOP;
		}

		show_message( $translator, Locale::KeyedText.new_message( 'MYAPP_GOODBYE' ) );
	}

	sub show_message( Locale::KeyedText::Translator $translator, Locale::KeyedText::Message $message ) returns Str {
		my Str $user_text = $translator.translate_message( $message );
		unless( $user_text ) {
			print $*ERR "internal error: can't find user text for a message: \n"~
				"   "~$message.as_string()~"\n"~
				"   "~$translator.as_string()~"\n";
			exit;
		}
		print $*OUT $user_text~"\n";
	}

	module MyApp::L::Eng {
		my Str %text_strings ::= (
			'MYAPP_HELLO' => 'Welcome to MyApp.',
			'MYAPP_GOODBYE' => 'Goodbye!',
			'MYAPP_PROMPT' => 'Enter a number to be inverted, or press ENTER to quit.',
			'MYAPP_RESULT' => 'The inverse of "{ORIGINAL}" is "{INVERTED}".',
		);
		sub get_text_by_key( Str $msg_key ) returns Str { return $text_strings{$msg_key}; }
	}

	module MyApp::L::Fre {
		my Str %text_strings ::= (
			'MYAPP_HELLO' => 'Bienvenue allé MyApp.',
			'MYAPP_GOODBYE' => 'Salut!',
			'MYAPP_PROMPT' => 'Fournir nombre être inverser, ou appuyer sur ENTER être arrêter.',
			'MYAPP_RESULT' => 'Renversement "{ORIGINAL}" est "{INVERTED}".',
		);
		sub get_text_by_key( Str $msg_key ) returns Str { return $text_strings{$msg_key}; }
	}

=head2 Content of alternate text Template file 'MyApp/L/Homer.pm':

	module MyApp::L::Homer;

	my Str %text_strings ::= (
		'MYAPP_HELLO' => 'Light goes on!',
		'MYAPP_GOODBYE' => 'Light goes off!',
		'MYAPP_PROMPT' => 'Give me a county thingy, or push that big button instead.',
		'MYAPP_RESULT' => 'Turn "{ORIGINAL}" upside down and get "{INVERTED}", not "{ORIGINAL}".',
		'MYLIB_MYINV_NO_ARG' => 'Why you little ...!',
		'MYLIB_MYINV_BAD_ARG' => '"{GIVEN_VALUE}" isn\'t a county thingy!',
		'MYLIB_MYINV_RES_INF' => 'Don\'t you give me a big donut!',
	);

	sub get_text_by_key( Str $msg_key ) returns Str { return $text_strings{$msg_key}; }

=head1 BUGS

This code has never actually been executed, given that no Perl 6 engine is yet
complete enough.  Hopefully that will change soon, though.  There are doubtless
numerous bugs in it.  That said, this module's code is based on a Perl 5 version
that has executed and was thoroughly tested; the main design should be fine.

I only have superficial memory recall of non-English languages.  I made the
French language example in the SYNOPSIS by manually translating the English one
with a printed on paper English to French dictionary; it probably contains
multiple grammatical errors (not that the English was well-formed either).

=head1 CAVEATS

All Locale::KeyedText functions and methods currently will fail silently if
they are given bad input; they will not throw any exceptions.  (At the same
time, however, the objects will always be internally consistent and can
continue to be used.)  This means, for example, if translate_message() fails
because it tries to use a Template module (whose name you provide) that doesn't
exist or that doesn't have the required API, it will return the same undef
value that it returns if all named Template modules are correct but no match
for the Message argument is found; it will do likewise if the given argument
isn't a valid Message object.  Locale::KeyedText will not provide any specifics
as to why it failed.  Depending on your usage, that may be exactly what you
want, or it may not be.  A large part of the reason for this silence is that
Locale::KeyedText itself is supposed to be very simple and internally language
independent; a thrown plain Perl exception would contain some detail in a
specific user language.  Likewise, a thrown Message object exception would
require external files itself to resolve them, leading to recursive complexity.
Suggestions for an alternate "proper" solution are welcome; meanwhile, the
current solution seems best to me.

=head1 SEE ALSO

These Perl 5 modules: L<Locale::Maketext>, L<Locale::gettext>,
L<Locale::PGetText>, L<DBIx::BabelKit>.

=cut