%% @author Juan Jose Comellas <juanjo@comellas.org>
%% @copyright 2008 Juan Jose Comellas

%% @doc String implemented over an Erlang binary.

-module(char).
-author('Juan Jose Comellas <juanjo@comellas.org>').

-export([is_alpha/1, is_alnum/1, is_lower/1, is_upper/1, is_digit/1, is_xdigit/1,
         is_blank/1, is_space/1, is_atom/1, lower/1, upper/1,
         integer_to_hex/1, integer_to_hex/2, hex_to_integer/1,
         must_urlencode/1, must_xmlencode/1]).


%%--------------------------------------------------------------------
%% @spec is_alpha(integer()) -> boolean()
%% @doc  Determines if a character is alphabetic.
%%--------------------------------------------------------------------
is_alpha(Char) ->
    ((Char >= $A) andalso (Char =< $Z)) orelse
    ((Char >= $a) andalso (Char =< $z)).


%%--------------------------------------------------------------------
%% @spec is_alnum(integer()) -> integer()
%% @doc  Determines if a character is alphanumeric.
%%--------------------------------------------------------------------
is_alnum(Char) ->
    ((Char >= $A) andalso (Char =< $Z)) orelse
    ((Char >= $a) andalso (Char =< $z)) orelse
    ((Char >= $0) andalso (Char =< $9)).


%%--------------------------------------------------------------------
%% @spec is_lower(integer()) -> boolean()
%% @doc  Determines if a character is lower-case.
%%--------------------------------------------------------------------
is_lower(Char) ->
    ((Char >= $a) andalso (Char =< $z)).


%%--------------------------------------------------------------------
%% @spec is_upper(integer()) -> boolean()
%% @doc  Determines if a character is upper-case.
%%--------------------------------------------------------------------
is_upper(Char) ->
    ((Char >= $A) andalso (Char =< $Z)).


%%--------------------------------------------------------------------
%% @spec is_digit(integer()) -> boolean()
%% @doc  Determines if a character is a decimal digit.
%%--------------------------------------------------------------------
is_digit(Char) ->
    ((Char >= $0) andalso (Char =< $9)).


%%--------------------------------------------------------------------
%% @spec is_xdigit(integer()) -> boolean()
%% @doc  Determines if a character is an hexadecimal digit.
%%--------------------------------------------------------------------
is_xdigit(Char) ->
    ((Char >= $0) andalso (Char =< $9)) orelse
    ((Char >= $A) andalso (Char =< $F)) orelse
    ((Char >= $a) andalso (Char =< $f)).


%%--------------------------------------------------------------------
%% @spec is_blank(integer()) -> boolean()
%% @doc  Determines if a character is blank (\n, \r, \t, \f, \v).
%%--------------------------------------------------------------------
is_blank(Char) ->
    ((Char =:= $\s) orelse (Char =:= $\t)).


%%--------------------------------------------------------------------
%% @spec is_space(integer()) -> boolean()
%% @doc  Determines if a character is a space or a tab.
%%--------------------------------------------------------------------
is_space(Char) ->
    ((Char =:= $\s) orelse (Char =:= $\n) orelse (Char =:= $\r) orelse
     (Char =:= $\t) orelse (Char =:= $\f) orelse (Char =:= $\v)).


%%--------------------------------------------------------------------
%% @spec is_atom(integer()) -> boolean()
%% @doc  Determines if a character is allowed in an unquoted atom
%%       (i.e. lower case, numeric, '_' or '@').
%%--------------------------------------------------------------------
is_atom(Char) ->
    ((Char >= $a) andalso (Char =< $z)) orelse
    ((Char >= $0) andalso (Char =< $9)) orelse
    (Char =:= $_) orelse
    (Char =:= $@).


%%--------------------------------------------------------------------
%% @spec lower(char()) -> char()
%% @doc  Convert a character to lower-case.
%%--------------------------------------------------------------------
lower(Char) when is_integer(Char) ->
    case is_upper(Char) of
        true ->
            Char - $A + $a;
        false ->
            Char
    end.


%%--------------------------------------------------------------------
%% @spec upper(char()) -> char()
%% @doc  Convert a character to upper-case.
%%--------------------------------------------------------------------
upper(Char) when is_integer(Char) ->
    case is_lower(Char) of
        true ->
            Char - $a + $A;
        false ->
            Char
    end.


%%--------------------------------------------------------------------
%% @spec integer_to_hex(integer()) -> integer()
%% @doc  Convert an integer between 0 and 15 to an hexadecimal character.
%%--------------------------------------------------------------------
integer_to_hex(N) when N >= 0 ->
    if
        N =< 9 ->
            $0 + N;
        N =< 15 ->
            $A - 10 + N;
        true ->
            erlang:error(badarg)
    end.

%%--------------------------------------------------------------------
%% @spec integer_to_hex(integer(), lower | upper) -> integer()
%% @doc  Convert an integer between 0 and 15 to an hexadecimal character
%%       choosing the case (lower, upper) for the hexadecimal digits.
%%--------------------------------------------------------------------
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


%%--------------------------------------------------------------------
%% @spec hex_to_integer(integer()) -> integer()
%% @doc  Convert an hexadecimal character to an integer. If the character is
%%       not an hexadecimal character we return a 'badarg' exception.
%%--------------------------------------------------------------------
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


%%--------------------------------------------------------------------
%% @spec must_urlencode(integer()) -> boolean()
%% @doc  Determine whether a character has to be URL-encoded.
%%--------------------------------------------------------------------
must_urlencode(Char) ->
    not (((Char >= $0) andalso (Char =< $9)) orelse
         ((Char >= $A) andalso (Char =< $Z)) orelse
         ((Char >= $a) andalso (Char =< $z)) orelse
         (Char =:= $-) orelse (Char =:= $_) orelse
         (Char =:= $.) orelse (Char =:= $~)).


%%--------------------------------------------------------------------
%% @spec must_xmlencode(integer()) -> boolean()
%% @doc  Determine whether a character has to be XML-encoded.
%% @see  http://en.wikipedia.org/wiki/UTF-8#Description
%% @see  http://www.asciitable.com/
%%--------------------------------------------------------------------
must_xmlencode(Char) -> 
    (Char < 32) orelse (Char =:= 192) orelse (Char =:= 193) orelse (Char > 244) 
        orelse (Char =:= $&) orelse (Char =:= $<) orelse (Char =:= $>) 
        orelse (Char =:= $') orelse (Char =:= $").

