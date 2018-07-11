unit class GCode;
use v6;

grammar Parser {
    rule TOP { <gcodes> }
    token gcodes {
        [ $<original> = ( <.ws> $<gc> = <.comment> ) ]*
        [
            [
                [ $<original> = ( <.ws> $<gc> = <.gcode> <.ws> ) ]
                [ [ $<original> = ( $<gc> = <.comment> <.ws> ) ]* ]
            ]
            || <error>
        ]+
    }
    token error { <.ws> \S+ }
    token comment { ';' <-[\n]>* [ "\n" | $ ] | '(' <-[)]>* ')' }
    token gcode { <letter> <parameter>? }
    token letter { :i <[A..Z]> }
    token parameter {
        | $<param> = <.fractional>
        | $<param> = <.integer>
        | $<param> = <.string>
        | $<param> = <.special>
    }
    token integer { <[-+]>? <[0..9]>+ }
    token fractional { <[-+]>? <[0..9]>+ '.' <[0..9]>+ }
    token special { \S+ }
    token string { '"' [ $<chars> = [ <-["]> | '""' ] ]+ '"' }
}

class Field {
    has Str $.letter;
    has $.parameter;
}

class Comment {
    has Str $.comment;
}

class Hunk {
    has $.original;
    has $.term;
}

class File {
    has Hunk @.hunks;
    method fields() { @!hunks.grep({ .term ~~ Field })».term }
    multi method Str { [~] @!hunks».original }
}

class Actions {
    has $!line = 0;
    method !next-line($n) { $!line += $n; }

    method TOP($/) {
        make File.new(hunks => $<gcodes>.made)
    }
    method ws($/) { self!next-line("$/".comb.grep("\n").elems) }
    method gcodes($/) {
        make gather for @($<original>) -> $original {
            take Hunk.new(
                original => "$original",
                term     => $original<gc>.made,
            );
        }
    }
    method gcode($/) {
        make Field.new(
            letter => $<letter>.made,
            parameter => $<parameter> ?? $<parameter>.made !! Nil,
        );
    }
    method error($/) { die "Parsing error at $!line: $/" }
    method comment($/) {
        self!next-line("$/".comb.grep("\n").elems);
        make Comment.new(comment => "$/");
    }
    method letter($/) { make "$/" }
    method parameter($/) { make $<param>.made }
    method integer($/) { make "$/".Int }
    method fractional($/) { make "$/".Rat }
    method string($/) { make [~] $<chars>.map({ $_ eq '""' ?? '"' !! $_ }) }
}

multi method parse(IO $thing) { self.parse($thing.slurp) }
multi method parse(Str() $thing) {
    Parser.parse($thing, actions => Actions.new).made
        // die "Cannot parse GCode";
}
