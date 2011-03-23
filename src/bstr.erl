%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@aptela.com>
%%% @copyright (C) 2008-2010, Juan Jose Comellas.
%%% @doc String implemented over an Erlang binary.
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(bstr).
-author('Juan Jose Comellas <juanjo@comellas.org>').
-author('Mahesh Paolini-Subramanya <mahesh@aptela.com>').

-export([len/1, equal/2, concat/2, nth/2, index/2, rindex/2,
         member/2, prefix/2, suffix/2,
         is_alpha/1, is_alnum/1, is_lower/1, is_upper/1, is_digit/1,
         is_xdigit/1, is_blank/1, is_space/1, is_atom_as_binary/1,
         is_atom_char/1, is_numeric/1,
         insert/3, duplicate/2, substr/2, substr/3, left/2, right/2,
         pad/2, pad/3, lpad/2, lpad/3, rpad/2, rpad/3,
         strip/1, strip/2, lstrip/1, lstrip/2, rstrip/1, rstrip/2, chomp/1,
         split/2, join/1, join/2, join/3, lower/1, upper/1, bstr/1,
         from_atom/1, to_atom/1, to_existing_atom/1, from_list/1, to_list/1,
         to_boolean/1, from_integer/1, from_integer/2, from_integer/3,
         to_integer/1, to_integer/2, from_float/1, to_float/1, from_number/1, to_number/1,
         integer_to_hex_char/1, integer_to_hex_char/2, hex_char_to_integer/1,
         get_line/1, urlencode/1, urldecode/1, xmlencode/1, xmldecode/1,
         hexencode/1, hexdecode/1]).


%% @doc  Return the length of a string.
-spec len(binary()) -> non_neg_integer().
len(Str) when is_binary(Str) ->
    size(Str).


%% @doc Checks if two strings are equal.
-spec equal(binary(), binary()) -> boolean().
equal(Str, Str) when is_binary(Str) ->
    true;
equal(Str, _) when is_binary(Str) ->
    false.


%% @doc Concatenate two strings.
-spec concat(binary(), binary()) -> binary().
concat(Str1, Str2) when is_binary(Str1), is_binary(Str2) ->
    <<Str1/binary, Str2/binary>>.


%% @doc Return the character in the nth position of the string.
-spec nth(binary(), pos_integer()) -> char().
nth(Str, Pos) when is_binary(Str), Pos > 0, Pos =< size(Str) ->
    Offset = Pos - 1,
    <<_Head:Offset/binary, Char, _Tail/binary>> = Str,
    Char.


%% @doc Return the index of the first appearance of a character in a string.
-spec index(binary(), char()) -> integer().
index(Str, Char) when is_binary(Str), is_integer(Char) ->
   index(Str, Char, 0).
index(<<Char, _Tail/binary>>, Char, N) ->
   N;
index(<<_Char, Tail/binary>>, Char, N) ->
   index(Tail, Char, N + 1);
index(<<>>, _Char, _N) ->
    -1.


%% @doc Return the index of the last appearance of a character in a string.
-spec rindex(binary(), char()) -> integer().
rindex(Str, Char) when is_binary(Str), is_integer(Char) ->
    rindex(Str, Char, size(Str) - 1).
rindex(Str, Char, Offset) ->
    case Str of
        <<_Head:Offset/binary, Char, _Tail/binary>> ->
            Offset;
        <<_Head:Offset/binary, _Char, _Tail/binary>> ->
            rindex(Str, Char, Offset - 1);
        _ ->
            -1
    end.


%% @doc Return whether the character is present in the string.
-spec member(binary(), char()) -> boolean().
member(<<Char, _Tail/binary>>, Char) ->
   true;
member(<<_Char, Tail/binary>>, Char) ->
   member(Tail, Char);
member(<<>>, _Char) ->
    false.


%% @doc Indicates whether a string is a prefix of another one.
-spec prefix(binary(), binary()) -> boolean().
prefix(Str, Prefix) when is_binary(Str), is_binary(Prefix) ->
    N = size(Prefix),
    case Str of
        <<Prefix:N/binary, _Tail/binary>> ->
            true;
        _ ->
            false
    end.


%% @doc Indicates whether a string is a suffix of another one.
-spec suffix(binary(), binary()) -> boolean().
suffix(Str, Suffix) when is_binary(Str), is_binary(Suffix) ->
    N = size(Str) - size(Suffix),
    case Str of
        <<_Head:N/binary, Suffix/binary>> ->
            true;
        _ ->
            false
    end.


%% @doc Determines if a string is composed of alphabetic characters.
-spec is_alpha(binary()) -> boolean().
is_alpha(<<>>) ->
    false;
is_alpha(Str) when is_binary(Str) ->
    is_x(Str, fun char:is_alpha/1);
is_alpha(Char) when is_integer(Char) ->
    char:is_alpha(Char).


%% @doc Determines if a string is composed of alphanumeric characters.
-spec is_alnum(binary()) -> boolean().
is_alnum(<<>>) ->
    false;
is_alnum(Str) when is_binary(Str) ->
    is_x(Str, fun char:is_alnum/1);
is_alnum(Char) when is_integer(Char) ->
    char:is_alnum(Char).


%% @doc Determines if a string is composed of lower-case alphabetic characters.
-spec is_lower(binary()) -> boolean().
is_lower(<<>>) ->
    false;
is_lower(Str) when is_binary(Str) ->
    is_x(Str, fun char:is_lower/1);
is_lower(Char) when is_integer(Char) ->
    char:is_lower(Char).


%% @doc Determines if a string is composed of upper-case alphabetic characters.
-spec is_upper(binary()) -> boolean().
is_upper(<<>>) ->
    false;
is_upper(Str) when is_binary(Str) ->
    is_x(Str, fun char:is_upper/1);
is_upper(Char) when is_integer(Char) ->
    char:is_upper(Char).


%% @doc Determines if a string is composed of digits.
-spec is_digit(binary()) -> boolean().
is_digit(<<>>) ->
    false;
is_digit(Str) when is_binary(Str) ->
    is_x(Str, fun char:is_digit/1);
is_digit(Char) when is_integer(Char) ->
    char:is_digit(Char).


%% @doc Determines if a string is composed of hexadecimal digits.
-spec is_xdigit(binary()) -> boolean().
is_xdigit(<<>>) ->
    false;
is_xdigit(Str) when is_binary(Str) ->
    is_x(Str, fun char:is_xdigit/1);
is_xdigit(Char) when is_integer(Char) ->
    char:is_xdigit(Char).


%% @doc Determines if a string is composed of blank characters.
-spec is_blank(binary()) -> boolean().
is_blank(<<>>) ->
    false;
is_blank(Str) when is_binary(Str) ->
    is_x(Str, fun char:is_blank/1);
is_blank(Char) when is_integer(Char) ->
    char:is_blank(Char).


%% @doc Determines if a string is composed of spaces or tabs.
-spec is_space(binary()) -> boolean().
is_space(<<>>) ->
    false;
is_space(Str) when is_binary(Str) ->
    is_x(Str, fun char:is_space/1);
is_space(Char) when is_integer(Char) ->
    char:is_space(Char).


%% @doc Determines if a string is an unquoted atom.
-spec is_atom_as_binary(binary()) -> boolean().
is_atom_as_binary(<<>>) ->
    false;
is_atom_as_binary(<<Char, Tail/binary>>) ->
    char:is_lower(Char) andalso is_x(Tail, fun is_atom_char/1);
is_atom_as_binary(Char) when is_integer(Char) ->
    is_atom_char(Char).

%% @doc Determine if a character is lower case, numeric, '_' or '@'.
-spec is_atom_char(char()) -> boolean().
is_atom_char(Char) ->
    ((Char >= $a) andalso (Char =< $z)) orelse
    ((Char >= $0) andalso (Char =< $9)) orelse
    (Char =:= $_) orelse
    (Char =:= $@).


%% @doc Determines if a string is a number.
-spec is_numeric(binary()) -> boolean().
is_numeric(Str) when is_binary(Str) ->
    is_numeric_sign(Str);
is_numeric(Char) when is_integer(Char) ->
    char:is_digit(Char).

is_numeric_sign(<<Char, Tail/binary>>)
  when (Char >= $0 andalso Char =< $9) orelse (Char =:= $-) orelse (Char =:= $+) ->
    is_numeric_digits(Tail);
is_numeric_sign(_Str) ->
    false.

is_numeric_digits(<<Char, Tail/binary>>) when (Char >= $0 andalso Char =< $9) ->
    is_numeric_digits(Tail);
is_numeric_digits(<<Char, Tail/binary>>) when Char =:= $. ->
    is_numeric_decimals(first, Tail);
is_numeric_digits(<<_Char, _Tail/binary>>) ->
    false;
is_numeric_digits(<<>>) ->
    true.

is_numeric_decimals(_Stage, <<Char, Tail/binary>>) when (Char >= $0 andalso Char =< $9) ->
    is_numeric_decimals(second, Tail);
is_numeric_decimals(second, <<>>) ->
    true;
is_numeric_decimals(_Stage, _Str) ->
    false.


%%--------------------------------------------------------------------
%% @spec is_x(Str::binary(), Fun::fun((char()) -> boolean())) -> boolean()
%% @doc  Helper function used to check whether all the characters in a string
%%       meet a specific criteria that is passed as a function to it.
%% @end
%%--------------------------------------------------------------------
%% is_x(<<>>, _Fun, _Offset) ->
%%     false;
%% is_x(Str, Fun, Offset) ->
%%     case Str of
%%         <<_Head:Offset/binary, Char, _Tail/binary>> ->
%%             case Fun(Char) of
%%                 true ->
%%                     is_x(Str, Fun, Offset + 1);
%%                 false ->
%%                     false
%%             end;
%%         %% If we reach the end we have a string composed entirely of characters
%%         %% that meet the criteria specified in the Fun().
%%         _ ->
%%             true
%%     end.
%% This version is about 5% faster than the one above. Re-test once Erlang R12B
%% is released.
-spec is_x(Str::binary(), Fun::fun((char()) -> boolean())) -> boolean().
is_x(<<Char, Tail/binary>>, Fun) ->
    case Fun(Char) of
        true ->
            is_x(Tail, Fun);
        false ->
            false
    end;
is_x(<<>>, _Fun) ->
    true.


%% @doc Insert a string into another one at the indicated position.
-spec insert(binary(), pos_integer(), binary()) -> binary().
insert(Str, Pos, Str1) when is_binary(Str), is_integer(Pos) ->
    N = Pos - 1,
    case Str of
        <<Head:N/binary, Tail/binary>> ->
            <<Head/binary, Str1/binary, Tail/binary>>;
        _ ->
            erlang:error(badarg)
    end.


%% @doc Return 'Number' copies of a string.
-spec duplicate(binary(), integer()) -> binary().
duplicate(Str, Num) ->
    duplicate(Str, Num, []).
duplicate(Str, Num, Acc) when Num > 0 ->
    duplicate(Str, Num - 1, [Str | Acc]);
duplicate(_Str, _Num, Acc) ->
    erlang:list_to_binary(Acc).


%% @doc Return a substring starting at position 'Pos'.
-spec substr(binary(), integer()) -> binary().
substr(Str, 1) when is_binary(Str) ->
    Str;
substr(Str, Pos) when is_binary(Str) ->
    N = Pos - 1,
    case Str of
        <<_Head:N/binary, Substr/binary>> ->
            Substr;
        _ ->
            <<>>
    end.

%% @doc Return a substring starting at position 'Pos' with a length of 'Len' bytes.
-spec substr(binary(), Pos::integer(), Len::integer()) -> binary().
substr(Str, 1, Len) when is_binary(Str), Len =:= size(Str) ->
    Str;
substr(Str, Pos, Len) when is_binary(Str) ->
    N = Pos - 1,
    case Str of
        <<_Head:N/binary, Substr:Len/binary, _Tail/binary>> ->
            Substr;
        <<_Head:N/binary, Substr/binary>> ->
            Substr;
        _ ->
            <<>>
    end.


%% @doc Return a substring of 'Len' bytes starting from the beginning of the
%%      string. If the string does not have enough characters, the original
%%      string is returned.
-spec left(binary(), integer()) -> binary().
left(Str, Len) when is_binary(Str), Len >= size(Str) ->
    Str;
left(Str, Len) when is_binary(Str), Len >= 0 ->
    <<Left:Len/binary, _Tail/binary>> = Str,
    Left.


%% @doc Return a substring of 'Len' bytes starting from the end of the string.
%%      If the string does not have enough characters, the original string is
%%      returned.
-spec right(binary(), integer()) -> binary().
right(Str, Len) when is_binary(Str), Len >= size(Str) ->
    Str;
right(Str, Len) when is_binary(Str), Len >= 0 ->
    Offset = size(Str) - Len,
    <<_Head:Offset/binary, Right/binary>> = Str,
    Right.


%% @doc Return a string of 'Len' bytes padded with spaces to the left and to the right.
-spec pad(binary(), non_neg_integer()) -> binary().
pad(Str, Len) when is_binary(Str), Len >= 0 ->
    pad(Str, Len, $\s).

%% @doc Return a string of 'Len' bytes padded with 'Chars' to the left and to the right.
-spec pad(binary(), integer(), char()) -> binary().
pad(Str, Len, Char) when is_binary(Str), Len >= 0 ->
    PadLen = Len - size(Str),
    if
        PadLen > 0 ->
            LeftPadLen = PadLen div 2,
            RightPadLen = PadLen - LeftPadLen,
            Padding = duplicate(Char, LeftPadLen),
            if
                RightPadLen > 0 ->
                    if
                        LeftPadLen =:= RightPadLen ->
                            <<Padding/binary, Str/binary, Padding/binary>>;
                        true ->
                            <<Padding/binary, Str/binary, Padding/binary, Char>>
                    end;
                true ->
                    <<Padding/binary, Str/binary>>
            end;
        true ->
            Str
    end.


%% @doc Return a string of 'Len' bytes left-padded with spaces.
-spec lpad(binary(), non_neg_integer()) -> binary().
lpad(Str, Len) when is_binary(Str), Len >= 0 ->
    lpad(Str, Len, $\s).

%% @doc Return a string of 'Len' bytes left-padded with 'Chars'.
-spec lpad(binary(), non_neg_integer(), char()) -> binary().
lpad(Str, Len, Char) when is_binary(Str), Len >= 0 ->
    PadLen = Len - size(Str),
    if
        PadLen > 0 ->
            Padding = duplicate(Char, PadLen),
            <<Padding/binary, Str/binary>>;
        true ->
            Str
    end.


%% @doc Return a string of 'Len' bytes right-padded with spaces.
-spec rpad(binary(), non_neg_integer()) -> binary().
rpad(Str, Len) when is_binary(Str), Len >= 0 ->
    rpad(Str, Len, $\s).

%% @doc Return a string of 'Len' bytes right-padded with 'Chars'.
-spec rpad(binary(), non_neg_integer(), char()) -> binary().
rpad(Str, Len, Char) ->
    PadLen = Len - size(Str),
    if
        PadLen > 0 ->
            Padding = duplicate(Char, PadLen),
            <<Str/binary, Padding/binary>>;
        true ->
            Str
    end.


%% @doc Remove all the spaces present both to the left and to the right of the string.
-spec strip(binary()) -> binary().
strip(Str) when is_binary(Str) ->
    strip(Str, <<"\s\t\n\r\f\v">>).

%% @doc Remove all the 'Chars' present both to the left and to the right of the string.
-spec strip(binary(), char()) -> binary().
strip(Str, Char) when is_binary(Str) andalso (is_integer(Char) orelse is_binary(Char)) ->
    rstrip(lstrip(Str, Char), Char).


%% @doc Remove all the spaces present to the left of the string.
-spec lstrip(binary()) -> binary().
lstrip(Str) when is_binary(Str) ->
    lstrip(Str, <<"\s\t\n\r\f\v">>).

%% @doc Remove all the 'Chars' present to the left of the string.
-spec lstrip(binary(), char() | binary()) -> binary().
lstrip(Str, Char) when is_binary(Str), is_integer(Char) ->
    lstrip_char(Str, Char);
lstrip(Str, Chars) when is_binary(Str), is_binary(Chars) ->
    lstrip_bin(Str, Chars).

%% @hidden
lstrip_char(<<Char, Tail/binary>>, Char) ->
    lstrip_char(Tail, Char);
lstrip_char(Str, _Char) ->
    Str.

%% @hidden
lstrip_bin(<<Char, Tail/binary>> = Str, Chars) ->
    case member(Chars, Char) of
        true ->
            lstrip_bin(Tail, Chars);
        _ ->
            Str
    end;
lstrip_bin(Str, _Chars) ->
    Str.


%% @doc Remove all the spaces present to the right of the string.
-spec rstrip(binary()) -> binary().
rstrip(Str) when is_binary(Str) ->
    rstrip(Str, <<"\s\t\n\r\f\v">>).

%% @doc Remove all the 'Chars' present to the right of the string.
-spec rstrip(binary(), char() | binary()) -> binary().
rstrip(Str, Char) when is_binary(Str), is_integer(Char) ->
    rstrip_char(Str, Char, size(Str) - 1);
rstrip(Str, Chars) when is_binary(Str), is_binary(Chars) ->
    rstrip_bin(Str, Chars, size(Str) - 1).

%% @hidden
rstrip_char(Str, Char, Pos) ->
    case Str of
        <<_Head:Pos/binary, Char, _Tail/binary>> ->
            rstrip_char(Str, Char, Pos - 1);
        <<_Head:Pos/binary, _Tail/binary>> ->
            N = Pos + 1,
            <<Stripped:N/binary, _Dummy/binary>> = Str,
            Stripped;
        _ ->
            <<>>
    end.

%% @hidden
rstrip_bin(Str, Chars, Pos) ->
    case Str of
        <<_Head:Pos/binary, Char, _Tail/binary>> ->
            case member(Chars, Char) of
                true ->
                    rstrip_bin(Str, Chars, Pos - 1);
                _ ->
                    N = Pos + 1,
                    <<Stripped:N/binary, _Dummy/binary>> = Str,
                    Stripped
            end;

        _ ->
            <<>>
    end.


%% @doc Remove all the newlines (\r and \n) present at the end of the string.
-spec chomp(binary()) -> binary().
chomp(Str) when is_binary(Str) ->
    chomp(Str, size(Str) - 1).
chomp(Str, Pos) ->
    case Str of
        <<_Head:Pos/binary, $\n, _Tail/binary>> ->
            chomp(Str, Pos - 1);
        <<_Head:Pos/binary, $\r, _Tail/binary>> ->
            chomp(Str, Pos - 1);
        <<_Head:Pos/binary, _Tail/binary>> ->
            N = Pos + 1,
            <<Stripped:N/binary, _Whitespace/binary>> = Str,
            Stripped;
        _ ->
            <<>>
    end.


%% @doc Divide a string into a list of tokens that were originally separated
%%      by the character 'Sep'.
-spec split(binary(), Sep::char() | binary()) -> list(binary()).
split(<<>>, _Sep) ->
    [];
split(Str, Sep) when is_binary(Str), is_integer(Sep) ->
    lists:reverse(split_char_sep(Str, Sep, 0, []));
split(Str, Sep) when is_binary(Str), is_binary(Sep) ->
    Tokens =
        case Sep of
            <<Char>> ->
                split_char_sep(Str, Char, 0, []);
            _ ->
                split_str_sep(Str, Sep, 0, [])
        end,
    lists:reverse(Tokens).

%% @doc Helper function used to tokenize a string when the separator is a character.
-spec split_char_sep(binary(), char(), integer(), [binary()]) -> [binary()].
split_char_sep(Str, Sep, Pos, Tokens) ->
    case Str of
        <<Token:Pos/binary, Sep, Tail/binary>> ->
            split_char_sep(Tail, Sep, 0, [Token | Tokens]);
        <<_Head:Pos/binary, _Tail/binary>> ->
            split_char_sep(Str, Sep, Pos + 1, Tokens);
        _ ->
            [Str | Tokens]
    end.

%% @doc Helper function used to tokenize a string when there are multiple separators.
-spec split_str_sep(binary(), binary(), non_neg_integer(), [binary()]) -> [binary(),...].
split_str_sep(Str, Sep, Pos, Tokens) ->
    case Str of
        <<Token:Pos/binary, Char, Tail/binary>> ->
            Index = index(Sep, Char),
            if
                Index >= 0 ->
                    split_str_sep(Tail, Sep, 0, [Token | Tokens]);
                true ->
                    split_str_sep(Str, Sep, Pos + 1, Tokens)
            end;
        _ ->
            [Str|Tokens]
    end.


%% @doc Join a a list of strings into one string.
% join(List) when is_list(List) ->
%     join_list(List, <<>>).
% join_list([Head|Tail], Acc) ->
%     Value = bstr(Head),
%     join_list(Tail, <<Acc/binary, Value/binary>>);
% join_list([], Acc) ->
%     Acc.
%% This version is about 30% faster than the one above. Re-test once Erlang R12B
%% is released. This test was performed before adding support for deep lists.
-spec join([binary()]) -> binary().
join(List) when is_list(List) ->
    list_to_binary(join_list(List, [])).
join_list([Head | Tail], Acc) when is_atom(Head) ->
    join_list(Tail, [atom_to_list(Head) | Acc]);
join_list([Head | Tail], Acc) when is_list(Head) ->
    join_list(Tail, [join_list(Head, []) | Acc]);
join_list([Head | Tail], Acc) ->
    join_list(Tail, [Head | Acc]);
join_list([], Acc) ->
    lists:reverse(Acc).


%% @doc Join a a list of strings into one string, adding a separator between
%%      each string.
-spec join([binary()], Sep::char() | binary()) -> binary().
join(List, Sep) when is_list(List) ->
    list_to_binary(join_list_sep(List, Sep)).

-spec join_list_sep([any()], char() | binary()) -> [any()].
join_list_sep([Head | Tail], Sep) when is_atom(Head) ->
    join_list_sep(Tail, Sep, [atom_to_list(Head)]);
join_list_sep([Head | Tail], Sep) when is_list(Head) ->
    join_list_sep(Tail, Sep, [join_list(Head, [])]);
join_list_sep([Head | Tail], Sep) ->
    join_list_sep(Tail, Sep, [Head]);
join_list_sep([], _Sep) ->
    [].
join_list_sep([Head | Tail], Sep, Acc) when is_atom(Head) ->
    join_list_sep(Tail, Sep, [atom_to_list(Head), Sep | Acc]);
join_list_sep([Head | Tail], Sep, Acc) when is_list(Head) ->
    join_list_sep(Tail, Sep, [join_list(Head, []), Sep | Acc]);
join_list_sep([Head | Tail], Sep, Acc) ->
    join_list_sep(Tail, Sep, [Head, Sep | Acc]);
join_list_sep([], _Sep, Acc) ->
    lists:reverse(Acc).


%%--------------------------------------------------------------------
%% @spec join(Members :: list(binary()),
%%            Sep     :: char() | binary(),
%%            Esc     :: char() ) -> binary()
%% @doc  Join a a list of strings into one string, adding a separator between
%%       each string and escaping both the separator and the escape char itself
%%       with the escape char.
%% @end
%% E.g.:
%%       `bstr:join([<<"1">>, <<",">>, <<"\1">>, <<"2,3">>], $,, $\) ->`
%%       `       <<"1,\,,\\1,2\,3">>.`
%%--------------------------------------------------------------------
-spec join([binary()], Sep :: char() | binary(), Esc :: char()) -> binary().
join(Members, Sep, Esc) ->
    EscStr =
        case Esc of
            $\\ ->
                "\\\\";
            $& ->
                "\\&";
            OtherEsc ->
                [OtherEsc]
        end,
    SepStr =
        case Sep of
            $\\ ->
                "\\\\";
            $& ->
                "\\&";
            OtherSep ->
                [OtherSep]
        end,
    {ok, Find}  = re:compile([$[ | SepStr ++ "]|[" ++ EscStr ++ "]"]), %% "[sep]|[esc]"
    Replace     = EscStr ++ "&",
    bstr:join(
      lists:map(fun(Member) when is_atom(Member) ->
                        re:replace(atom_to_list(Member),
                                   Find,
                                   Replace,
                                   [{return, binary}, global]);
                   (Member) ->
                        re:replace(Member,
                                   Find,
                                   Replace,
                                   [{return, binary}, global])
                end, Members),
      Sep).

%% @doc Convert all the characters in a bstr to lowercase.
-spec lower(binary() | char()) -> binary() | char().
lower(Str) when is_binary(Str) ->
    lower_nocopy(Str, 0);
lower(Char) when is_integer(Char) ->
    case char:is_upper(Char) of
        true ->
            Char - $A + $a;
        false ->
            Char
    end.

%% The first part scans the string to see if it finds an upper-case character.
%% If it finds one, it then switches to the version of the function that copies
%% each character.
lower_nocopy(Str, N) ->
    case Str of
        <<Head:N/binary, Char, Tail/binary>> ->
            case char:is_upper(Char) of
                true ->
                    Lower = Char - $A + $a,
                    lower_copy(Tail, [Lower, Head]);
                false ->
                    lower_nocopy(Str, N + 1)
            end;
        _ ->
            Str
    end.
%% This part accumulates each of the characters in a list.
lower_copy(<<Char, Tail/binary>>, Acc) ->
    case char:is_upper(Char) of
        true ->
            lower_copy(Tail, [(Char - $A + $a) | Acc]);
        false ->
            lower_copy(Tail, [Char | Acc])
    end;
lower_copy(<<>>, Acc) ->
    list_to_binary(lists:reverse(Acc)).


%% @doc Convert all the characters in a bstr to uppercase.
-spec upper(binary() | char()) -> binary() | char().
upper(Str) when is_binary(Str) ->
    upper_nocopy(Str, 0);
upper(Char) when is_integer(Char) ->
    case char:is_lower(Char) of
        true ->
            Char - $a + $A;
        false ->
            Char
    end.
%% The first part scans the string to see if it finds a lower-case character.
%% If it finds one, it then switches to the version of the function that copies
%% each character.
upper_nocopy(Str, N) ->
    case Str of
        <<Head:N/binary, Char, Tail/binary>> ->
            case char:is_lower(Char) of
                true ->
                    Upper = Char - $a + $A,
                    upper_copy(Tail, [Upper, Head]);
                false ->
                    upper_nocopy(Str, N + 1)
            end;
        _ ->
            Str
    end.
%% This part accumulates each of the characters in a list.
upper_copy(<<Char, Tail/binary>>, Acc) ->
    case char:is_lower(Char) of
        true ->
            upper_copy(Tail, [(Char - $a + $A) | Acc]);
        false ->
            upper_copy(Tail, [Char | Acc])
    end;
upper_copy(<<>>, Acc) ->
    list_to_binary(lists:reverse(Acc)).



%% @doc Convert an "object" to a bstr.
-spec bstr(binary() | atom() | list() | char()) -> binary().
bstr(Bin) when is_binary(Bin) ->
    Bin;
bstr(Pid) when is_pid(Pid) ->
    list_to_binary(pid_to_list(Pid));
bstr(Atom) when is_atom(Atom) ->
    list_to_binary(atom_to_list(Atom));
bstr(Integer) when is_integer(Integer) ->
    <<Integer>>;
bstr(List) when is_list(List) ->
    list_to_binary(List).


%% @doc Convert an atom to a bstr.
-spec from_atom(atom()) -> binary().
from_atom(Atom) when is_atom(Atom) ->
    list_to_binary(atom_to_list(Atom)).

%% @doc Convert a bstr containing a string to an Erlang atom.
-spec to_atom(binary()) -> atom().
to_atom(Str) when is_binary(Str) ->
    list_to_atom(binary_to_list(Str)).

%% @doc Convert a bstr containing a string to an Erlang atom only if the atom
%%      already existed (i.e. had been previously defined).
-spec to_existing_atom(binary()) -> atom().
to_existing_atom(Str) when is_binary(Str) ->
    list_to_existing_atom(binary_to_list(Str)).


%% @doc Convert a list containing a string to a binary.
-spec from_list(list()) -> binary().
from_list(List) when is_list(List) ->
    list_to_binary(List).


%% @doc Convert a bstr containing a string to an Erlang list/string.
-spec to_list(binary()) -> [byte()].
to_list(Str) when is_binary(Str) ->
    binary_to_list(Str).


%% @doc Convert a bstr containing a string to an Erlang list/string.
-spec to_boolean(binary()) -> boolean().
to_boolean(<<"true">>) ->
    true;
to_boolean(<<"false">>) ->
    false.


%% @doc Convert an integer to a bstr in base 10 format.
-spec from_integer(integer()) -> binary().
from_integer(I) ->
    from_integer(I, 10, upper).


%% @doc Convert an integer to a bstr in base 'n' format.
-spec from_integer(integer(), 1..255) -> binary().
from_integer(I, Base) ->
    from_integer(I, Base, upper).

%% @doc Convert an integer to a bstr in base 'n' format in the specified case.
-spec from_integer(integer(), 1..255, upper | lower) -> binary().
from_integer(I, Base, Case) when is_integer(I), is_integer(Base), Base >= 2, Base =< 1 + $Z - $A + 10 ->
    BaseLetter = case Case of
                     upper ->
                         $A;
                     lower ->
                         $a
                 end,

    list_to_binary(
      if
          I < 0 ->
              [$- | from_integer(-I, Base, BaseLetter, [])];
          true ->
              from_integer(I, Base, BaseLetter, [])
      end
     );
from_integer(I, Base, Case) ->
    erlang:error(badarg, [I, Base, Case]).

%% Helper function to convert an integer to a base 'n' representation.
-spec from_integer(integer(), pos_integer(), 65 | 97, [integer()]) -> [integer(),...].
from_integer(I0, Base, BaseLetter, Acc) ->
    I1 = I0 div Base,
    Digit = I0 rem Base,

    Acc1 = [
            if
                (Digit >= 10) ->
                    Digit - 10 + BaseLetter;
                true ->
                    Digit + $0
            end | Acc
           ],
    if
        I1 =:= 0 ->
            Acc1;
        true ->
            from_integer(I1, Base, BaseLetter, Acc1)
    end.


%% @doc Convert a bstr containing a string representing a decimal number
%%      to an integer.
-spec to_integer(binary()) -> integer().
to_integer(<<$-, Str/binary>>) ->
    -to_decimal_integer(Str, 0, 0);
to_integer(<<$+, Str/binary>>) ->
    to_decimal_integer(Str, 0, 0);
to_integer(<<>>) ->
    erlang:error(badarg, [<<>>]);
to_integer(Str) when is_binary(Str) ->
    to_decimal_integer(Str, 0, 0).
%% Part of the function converts the string into a base-10 number
to_decimal_integer(Str, Offset, Acc) ->
    case Str of
        <<_Head:Offset/binary, Char, _Tail/binary>> when (Char >= $0) and (Char =< $9) ->
            to_decimal_integer(Str, Offset + 1, Acc * 10 + (Char - $0));
        <<_Head:Offset/binary>> ->
            Acc;
        _Number ->
            %% We throw the same exception thrown by list_to_integer() if a
            %% non-numeric character is found
            erlang:error(badarg, [Str])
    end.


%% @doc Convert a bstr containing a string representing a positive number
%%      in the specified 'Base' to an integer. 'Base' must be an integer
%%      between 2 and 32.
% Optimized version for base 10
-spec to_integer(binary(), 1..255) -> integer().
to_integer(Str, 10) ->
    to_integer(Str);
to_integer(Str, Base) when is_integer(Base), Base >= 2, Base =< 1 + $Z - $A + 10 ->
    case Str of
        <<$-, Number/binary>> ->
            -to_base_n_integer(Number, 0, Base, 0);
        <<$+, Number/binary>> ->
            to_base_n_integer(Number, 0, Base, 0);
        <<>> ->
            erlang:error(badarg, [<<>>]);
        Number ->
            to_base_n_integer(Number, 0, Base, 0)
    end;
to_integer(Str, Base) ->
    erlang:error(badarg, [Str, Base]).
% Generic version for the rest of the bases
to_base_n_integer(Str, Offset, Base, Acc) ->
    case Str of
        <<_Head:Offset/binary, Char, _Tail/binary>> ->
            N =
                if
                    (Char >= $0) and (Char =< $9) and (Char < Base + $0) ->
                        (Char - $0);
                    (Char >= $A) and (Char =< $Z) and (Char < Base + $A) ->
                        10 + (Char - $A);
                    (Char >= $a) and (Char =< $z) and (Char < Base + $a) ->
                        10 + (Char - $a);
                    true ->
                        %% We throw the same exception thrown by list_to_integer() if an
                        %% invalid character is found
                        erlang:error(badarg, [Str, Base]),
                        %% To avoid compiler warning
                        0
                end,
            to_base_n_integer(Str, Offset + 1, Base, Acc * Base + N);

        <<_Head:Offset/binary>> ->
            Acc;

        _Number ->
            %% We throw the same exception thrown by list_to_integer() if a
            %% non-numeric character is found
            erlang:error(badarg, [Str])
    end.


%% @doc Convert a floating point number to a bstr.
-spec from_float(float()) -> binary().
from_float(Float) ->
    %% Use mochinum to avoid weird formatting by the Erlang float_to_list() BIF.
    list_to_binary(mochinum:digits(Float)).


%% @doc Convert a bstr formatted as a floating point number to a float.
-spec to_float(binary()) -> float().
to_float(Str) ->
    list_to_float(binary_to_list(Str)).


%% @doc Convert an integer or floating point number into a bstr.
-spec from_number(integer() | float()) -> binary().
from_number(Number) ->
    %% Use mochinum to avoid weird formatting by the Erlang float_to_list() BIF.
    list_to_binary(mochinum:digits(Number)).


%% @doc Convert a formatted binary into an integer or floating point number.
-spec to_number(binary()) -> integer() | float().
to_number(Str) ->
    Number = << <<Char>> || <<Char>> <= Str, (char:is_digit(Char) orelse Char =:= $. orelse Char =:= $- orelse Char =:= $+) >>,
    case member(Number, $.) of
        true ->
            list_to_float(binary_to_list(Number));
        false ->
            list_to_integer(binary_to_list(Str))
    end.


%% @doc Get the first text line from a binary string. It returns a tuple with
%%      the first text line as the first element and the rest of the string as
%%      the last element.
-spec get_line(binary()) -> {binary(), binary()}.
get_line(Str) ->
    get_line(Str, 0).
get_line(Str, Offset) ->
    case Str of
        <<Line:Offset/binary, $\n, Tail/binary>> ->
            {Line, Tail};
        <<Line:Offset/binary, $\r, $\n, Tail/binary>> ->
            {Line, Tail};
        Line when Offset >= size(Line) ->
            {Str, <<>>};
        _ ->
            get_line(Str, Offset + 1)
    end.


%% @doc Encode a bstr using the URL-encoding scheme.
-spec urlencode(binary()) -> binary().
urlencode(Str) when is_binary(Str) ->
    urlencode(Str, 0).
%% This part of the function iterates over the bstr without copying any data
%% and once it finds a character that has to be URL-encoded it switches to
%% the version of the function that accumulates the converted bstr.
urlencode(Str, N) ->
    case Str of
        <<Head:N/binary, Char, _Tail/binary>> ->
            case is_urlencoded(Char) of
                true ->
                    Hi = integer_to_hex_char((Char band 16#f0) bsr 4),
                    Lo = integer_to_hex_char((Char band 16#0f)),
                    urlencode(Str, N + 1, [Lo, Hi, $%, Head]);

                false ->
                    urlencode(Str, N + 1)
            end;
        _ ->
            Str
    end.
urlencode(Str, N, Acc) ->
    case Str of
        <<_Head:N/binary, Char, _Tail/binary>> ->
            case is_urlencoded(Char) of
                true ->
                    Hi = integer_to_hex_char((Char band 16#f0) bsr 4),
                    Lo = integer_to_hex_char((Char band 16#0f)),
                    urlencode(Str, N + 1, [Lo, Hi, $% | Acc]);

                false ->
                    urlencode(Str, N + 1, [Char | Acc])
            end;
        _ ->
            list_to_binary(lists:reverse(Acc))
    end.

%% @doc Determine whether a character has to be URL-encoded.
is_urlencoded(Char) ->
    not (((Char >= $0) andalso (Char =< $9)) orelse
         ((Char >= $A) andalso (Char =< $Z)) orelse
         ((Char >= $a) andalso (Char =< $z)) orelse
         (Char =:= $-) orelse (Char =:= $_) orelse
         (Char =:= $.) orelse (Char =:= $~)).

%% @doc Convert an integer between 0 and 15 to an hexadecimal character.
-spec integer_to_hex_char(char()) -> char().
integer_to_hex_char(N) when N >= 0 ->
    if
        N =< 9 ->
            $0 + N;
        N =< 15 ->
            $A - 10 + N;
        true ->
            erlang:error(badarg)
    end.
%% @hidden
-spec integer_to_hex_char(char(), lower | upper) -> char().
integer_to_hex_char(N, lower) when N >= 0 ->
    if
        N =< 9 ->
            $0 + N;
        N =< 15 ->
            $a - 10 + N;
        true ->
            erlang:error(badarg)
    end;
integer_to_hex_char(N, upper) ->
    integer_to_hex_char(N).


%% @doc Decode a bstr using the URL-encoding scheme.
-spec urldecode(binary()) -> binary().
urldecode(Str) ->
    urldecode(Str, 0).
%% This part of the function iterates over the bstr without copying any data
%% and once it finds a character that is URL-encoded it switches to
%% the version of the function that accumulates the converted bstr.
urldecode(Str, N) ->
    case Str of
        <<Head:N/binary, $%, Hi, Lo, _Tail/binary>> ->
            Char = ((hex_char_to_integer(Hi) bsl 4) bor hex_char_to_integer(Lo)),
            urldecode(Str, N + 3, <<Head/binary, Char>>);
        <<_Head:N/binary, Char, _Tail/binary>> ->
            if
                Char =:= $% ->
                    erlang:error(badarg);
                true ->
                    urldecode(Str, N + 1)
            end;
        _ ->
            Str
    end.
urldecode(Str, N, Acc) ->
    case Str of
        <<_Head:N/binary, $%, Hi, Lo, _Tail/binary>> ->
            Char = ((hex_char_to_integer(Hi) bsl 4) bor hex_char_to_integer(Lo)),
            urldecode(Str, N + 3, <<Acc/binary, Char>>);
        <<_Head:N/binary, Char, _Tail/binary>> ->
            if
                Char =:= $% ->
                    erlang:error(badarg);
                true ->
                    urldecode(Str, N + 1, <<Acc/binary, Char>>)
            end;
        _ ->
            Acc
    end.

%% @doc Convert an hexadecimal character to an integer. If the character is not an
%%      hexadecimal character we return a 'badarg' exception.
-spec hex_char_to_integer(char()) -> char().
hex_char_to_integer(Char) ->
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



%% @doc Encode a bstr using the XML-encoding scheme.
%%      WARNING: This function assumes that the input is a valid UTF-8 string
%%               and supports non-printable characters in the ASCII range
%%               00h-1Fh. Bytes that are not part of a valid UTF-8 character
%%               are not converted at all.
-spec xmlencode(binary()) -> binary().
xmlencode(Str) when is_binary(Str) ->
    xmlencode(Str, 0).
%% This part of the function iterates over the binary() without copying any data
%% and once it finds a character that has to be XML-encoded it switches to
%% the version of the function that accumulates the converted bstr.
xmlencode(Str, Offset) ->
    case Str of
        <<Head:Offset/binary, Char, _Tail/binary>> ->
            case is_xmlencoded(Char) of
                true ->
                    Encoded = xmlencode_char(Char),
                    xmlencode(Str, Offset + 1, [$;, Encoded, $&, Head]);

                false ->
                    xmlencode(Str, Offset + 1)
            end;
        _ ->
            Str
    end.
xmlencode(Str, Offset, Acc) ->
    case Str of
        <<_Head:Offset/binary, Char, _Tail/binary>> ->
            case is_xmlencoded(Char) of
                true ->
                    Encoded = xmlencode_char(Char),
                    xmlencode(Str, Offset + 1, [$;, Encoded, $& | Acc]);

                false ->
                    xmlencode(Str, Offset + 1, [Char | Acc])
            end;
        _ ->
            list_to_binary(lists:reverse(Acc))
    end.

%% @doc Determine whether a character has to be XML-encoded. See
%% <a href="http://en.wikipedia.org/wiki/UTF-8#Description">Wikipedia</a>,
%% <a href="http://www.asciitable.com/">ASCII Table</a>
is_xmlencoded(Char) ->
    (Char < 32) orelse (Char =:= 192) orelse (Char =:= 193) orelse (Char > 244)
        orelse (Char =:= $&) orelse (Char =:= $<) orelse (Char =:= $>)
        orelse (Char =:= $') orelse (Char =:= $").

%% Encode a single character using the XML encoding scheme to create a
%% character entity reference.
xmlencode_char($&) ->
    <<"amp">>;
xmlencode_char($<) ->
    <<"lt">>;
xmlencode_char($>) ->
    <<"gt">>;
xmlencode_char($') ->
    <<"apos">>;
xmlencode_char($") ->
    <<"quot">>;
xmlencode_char(Char) when Char < 32 ->
    Hi = integer_to_hex_char((Char band 16#f0) bsr 4),
    Lo = integer_to_hex_char((Char band 16#0f)),
    <<"#x", Hi, Lo>>;
xmlencode_char(Char) ->
    Char.


%% @doc Decode a bstr using the XML-encoding scheme to resolve any character
%%      entity reference present in the string.
-spec xmldecode(binary()) -> binary().
xmldecode(Str) ->
    xmldecode(Str, 0).
%% This part of the function iterates over the bstr without copying any data
%% and once it finds a character that is XML-encoded it switches to
%% the version of the function that accumulates the converted bstr.
xmldecode(Str, Offset) ->
    case xmldecode_char(Str, Offset) of
        %% We found a character that does not need to be decoded
        {_Char, 1} ->
            xmldecode(Str, Offset + 1);
        %% We found a character that needs to be encoded; we create the accumulator
        %% and call the function that copies each character to create a new string.
        {Char, Length} ->
            {Head, _Tail} = split_binary(Str, Offset),
            xmldecode(Str, Offset + Length, [Char, Head]);
        eof ->
            Str
    end.
xmldecode(Str, Offset, Acc) ->
    case xmldecode_char(Str, Offset) of
        {Char, Length} ->
            xmldecode(Str, Offset + Length, [Char | Acc]);
        eof ->
            list_to_binary(lists:reverse(Acc))
    end.

%% @doc Given a string and an offset, this function checks whether there is a
%%      character in that position that has to be decoded using the XML
%%      encoding scheme for character entity references and returns a tuple
%%      with the decoded character and the length of the encoded substring in
%%      the original string.
-spec xmldecode_char(binary(), integer()) -> {char(), 1 | 4 | 5 | 6} | eof.
xmldecode_char(Str, Offset) ->
    case Str of
        <<_Head:Offset/binary, "&amp;", _Tail/binary>> ->
            {$&, 5};
        <<_Head:Offset/binary, "&lt;", _Tail/binary>> ->
            {$<, 4};
        <<_Head:Offset/binary, "&gt;", _Tail/binary>> ->
            {$>, 4};
        <<_Head:Offset/binary, "&apos;", _Tail/binary>> ->
            {$', 6};
        <<_Head:Offset/binary, "&quot;", _Tail/binary>> ->
            {$", 6};
        <<_Head:Offset/binary, "&#x", Hi, Lo, $;,  _Tail/binary>> ->
            {((hex_char_to_integer(Hi) bsl 4) bor hex_char_to_integer(Lo)), 6};
        <<_Head:Offset/binary, Char, _Tail/binary>> ->
            {Char, 1};
        _ ->
           eof
    end.


%% @doc Encode a bstr converting each character to its hexadecimal
%%      representation.
-spec hexencode(binary()) -> binary().
hexencode(Str) when is_binary(Str) ->
    hexencode(Str, []).
hexencode(<<Hi:4, Lo:4, Tail/binary>>, Acc) ->
    hexencode(Tail, [integer_to_hex_char(Lo, lower), integer_to_hex_char(Hi, lower) | Acc]);
hexencode(<<>>, Acc) ->
    list_to_binary(lists:reverse(Acc)).


%% @doc Decode a bstr with an hexadecimal representation of a string.
-spec hexdecode(binary()) -> binary().
hexdecode(Str) when is_binary(Str) ->
    hexdecode(Str, []).
hexdecode(<<Hi, Lo, Tail/binary>>, Acc) ->
    Char = ((hex_char_to_integer(Hi) bsl 4) bor hex_char_to_integer(Lo)),
    hexdecode(Tail, [Char | Acc]);
% If the number of characters wasn't even we raise an exception.
hexdecode(<<_Char>>, _Acc) ->
    erlang:error(badarg);
hexdecode(<<>>, Acc) ->
    list_to_binary(lists:reverse(Acc)).

