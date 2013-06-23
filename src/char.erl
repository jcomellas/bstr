%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@aptela.com>
%%% @copyright 2008-2011  Juan Jose Comellas, Mahesh Paolini-Subramanya.
%%% @doc Character functions.
%%% @end
%%%-------------------------------------------------------------------
-module(char).

-author('Juan Jose Comellas <juanjo@comellas.org>').
-author('Mahesh Paolini-Subramanya <mahesh@aptela.com>').

-export([ hex_to_integer/1
        , integer_to_hex/1
        , integer_to_hex/2
        , is_alnum/1
        , is_alpha/1
        , is_atom/1
        , is_blank/1
        , is_digit/1
        , is_lower/1
        , is_space/1
        , is_upper/1
        , is_xdigit/1
        , lower/1
        , must_urlencode/1
        , must_xmlencode/1
        , upper/1
        ]).


%% @doc Determines if a character is alphabetic.
-spec is_alpha(char()) -> boolean().
is_alpha(Char) ->
    ((Char >= $A) andalso (Char =< $Z)) orelse
    ((Char >= $a) andalso (Char =< $z)).


%% @doc Determines if a character is alphanumeric.
-spec is_alnum(char()) -> boolean().
is_alnum(Char) ->
    ((Char >= $A) andalso (Char =< $Z)) orelse
    ((Char >= $a) andalso (Char =< $z)) orelse
    ((Char >= $0) andalso (Char =< $9)).


%% @doc Determines if a character is lower-case.
-spec is_lower(char()) -> boolean().
is_lower(Char) ->
    ((Char >= $a) andalso (Char =< $z)).


%% @doc Determines if a character is upper-case.
-spec is_upper(char()) -> boolean().
is_upper(Char) ->
    ((Char >= $A) andalso (Char =< $Z)).


%% @doc Determines if a character is a decimal digit.
-spec is_digit(char()) -> boolean().
is_digit(Char) ->
    ((Char >= $0) andalso (Char =< $9)).


%% @doc Determines if a character is an hexadecimal digit.
-spec is_xdigit(char()) -> boolean().
is_xdigit(Char) ->
    ((Char >= $0) andalso (Char =< $9)) orelse
    ((Char >= $A) andalso (Char =< $F)) orelse
    ((Char >= $a) andalso (Char =< $f)).


%% @doc Determines if a character is blank (\n, \r, \t, \f, \v).
-spec is_blank(char()) -> boolean().
is_blank(Char) ->
    ((Char =:= $\s) orelse (Char =:= $\t)).


%% @doc Determines if a character is a space or a tab.
-spec is_space(char()) -> boolean().
is_space(Char) ->
    ((Char =:= $\s) orelse (Char =:= $\n) orelse (Char =:= $\r) orelse
     (Char =:= $\t) orelse (Char =:= $\f) orelse (Char =:= $\v)).


%% @doc Determines if a character is allowed in an unquoted atom
%%       (i.e. lower case, numeric, '_' or '@').
-spec is_atom(char()) -> boolean().
is_atom(Char) ->
    ((Char >= $a) andalso (Char =< $z)) orelse
    ((Char >= $0) andalso (Char =< $9)) orelse
    (Char =:= $_) orelse
    (Char =:= $@).


%% @doc Convert a character to lower-case.
-spec lower(char()) -> char().
lower(Char) when is_integer(Char) ->
    case is_upper(Char) of
        true ->
            Char - $A + $a;
        false ->
            Char
    end.


%% @doc Convert a character to upper-case.
-spec upper(char()) -> char().
upper(Char) when is_integer(Char) ->
    case is_lower(Char) of
        true ->
            Char - $a + $A;
        false ->
            Char
    end.


%% @doc Convert an integer between 0 and 15 to an hexadecimal character.
-spec integer_to_hex(integer()) -> char().
integer_to_hex(N) when N >= 0 ->
    if
        N =< 9 ->
            $0 + N;
        N =< 15 ->
            $A - 10 + N;
        true ->
            erlang:error(badarg)
    end.

%% @doc Convert an integer between 0 and 15 to an hexadecimal character
%%      choosing the case (lower, upper) for the hexadecimal digits.
-spec integer_to_hex(integer(), 'lower' | 'upper') -> char().
integer_to_hex(N, lower) when N >= 0 ->
    if
        N =< 9 ->
            $0 + N;
        N =< 15 ->
            $a - 10 + N;
        true ->
            erlang:error(badarg)
    end;
integer_to_hex(N, upper) ->
    integer_to_hex(N).


%% @doc Convert an hexadecimal character to an integer. If the character is
%%      not an hexadecimal character we return a 'badarg' exception.
-spec hex_to_integer(char()) -> integer().
hex_to_integer(Char) ->
    if
        (Char >= $0) and (Char =< $9) ->
            Char - $0;
        (Char >= $A) and (Char =< $F) ->
            Char - $A + 10;
        (Char >= $a) and (Char =< $f) ->
            Char - $a + 10;
        true ->
            erlang:error(badarg)
    end.


%% @spec must_urlencode(integer()) -> boolean()
-spec must_urlencode(char()) -> boolean().
must_urlencode(Char) ->
    not (((Char >= $0) andalso (Char =< $9)) orelse
         ((Char >= $A) andalso (Char =< $Z)) orelse
         ((Char >= $a) andalso (Char =< $z)) orelse
         (Char =:= $-) orelse (Char =:= $_) orelse
         (Char =:= $.) orelse (Char =:= $~)).


%% @doc Determine whether a character has to be XML-encoded.
%%      See [http://en.wikipedia.org/wiki/UTF-8#Description] and
%%      [http://www.asciitable.com/].
-spec must_xmlencode(char()) -> boolean().
must_xmlencode(Char) ->
    (Char < 32) orelse (Char =:= 192) orelse (Char =:= 193) orelse (Char > 244)
        orelse (Char =:= $&) orelse (Char =:= $<) orelse (Char =:= $>)
        orelse (Char =:= $') orelse (Char =:= $").

