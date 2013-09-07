%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @author Mahesh Paolini-Subramanya <mahesh@aptela.com>
%%% @copyright 2008-2011  Juan Jose Comellas, Mahesh Paolini-Subramanya.
%%% @doc String implemented over an Erlang binary.
%%% @end
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(bstr).

-author('Juan Jose Comellas <juanjo@comellas.org>').
-author('Mahesh Paolini-Subramanya <mahesh@aptela.com>').

-export([ bstr/1
        , chomp/1
        , concat/2
        , duplicate/2
        , equal/2
        , from_atom/1
        , from_float/1
        , from_integer/1
        , from_integer/2
        , from_integer/3
        , from_list/1
        , from_number/1
        , get_line/1
        , hex_char_to_integer/1
        , hexdecode/1
        , hexencode/1
        , index/2
        , insert/3
        , integer_to_hex_char/1
        , integer_to_hex_char/2
        , is_alnum/1
        , is_alpha/1
        , is_atom_as_binary/1
        , is_atom_char/1
        , is_blank/1
        , is_digit/1
        , is_lower/1
        , is_numeric/1
        , is_space/1
        , is_upper/1
        , is_xdigit/1
        , join/1
        , join/2
        , join/3
        , left/2
        , len/1
        , lower/1
        , lpad/2
        , lpad/3
        , lstrip/1
        , lstrip/2
        , member/2
        , nth/2
        , pad/2
        , pad/3
        , prefix/2
        , right/2
        , rindex/2
        , rpad/2
        , rpad/3
        , rstrip/1
        , rstrip/2
        , split/2
        , strip/1
        , strip/2
        , substr/2
        , substr/3
        , suffix/2
        , to_atom/1
        , to_boolean/1
        , to_existing_atom/1
        , to_float/1
        , to_integer/1
        , to_integer/2
        , to_list/1
        , to_number/1
        , upper/1
        , urldecode/1
        , urlencode/1
        , xmldecode/1
        , xmlencode/1
        ]).


%% @doc  Return the length of a string.
-spec len(binary()) -> non_neg_integer().
len(Str) when is_binary(Str) ->
    size(Str).


%% @doc Checks if two strings are equal.
-spec equal(binary(), binary()) -> boolean().
equal(Str, Str) ->
    true;
equal(_, _) ->
    false.


%% @doc Concatenate two strings.
-spec concat(binary(), binary()) -> binary().
concat(Str1, Str2) when is_binary(Str1), is_binary(Str2) ->
    <<Str1/binary, Str2/binary>>.


%% @doc Return the character in the nth position of the string.
-spec nth(binary(), pos_integer()) -> char().
nth(Str, Pos) when Pos > 0, Pos =< size(Str) ->
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
is_blank(Char) when is_integer(Char) ->
    char:is_blank(Char);
is_blank(Str) ->
    is_x(Str, fun char:is_blank/1).


%% @doc Determines if a string is composed of spaces or tabs.
-spec is_space(binary()) -> boolean().
is_space(<<>>) ->
    false;
is_space(Char) when is_integer(Char) ->
    char:is_space(Char);
is_space(Str) ->
    is_x(Str, fun char:is_space/1).


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
    is_numeric_decimals(Tail, first);
is_numeric_digits(<<_Char, _Tail/binary>>) ->
    false;
is_numeric_digits(<<>>) ->
    true.

is_numeric_decimals(<<Char, Tail/binary>>, _Stage) when (Char >= $0 andalso Char =< $9) ->
    is_numeric_decimals(Tail, second);
is_numeric_decimals(<<>>, second) ->
    true;
is_numeric_decimals(_Str, _Stage) ->
    false.


%% @doc  Helper function used to check whether all the characters in a string
%%       meet a specific criteria that is passed as a function to it.
%% @end
-spec is_x(Str::binary(), Fun::fun((char()) -> boolean())) -> boolean().
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


%% @doc Return 'Count' copies of a string.
-spec duplicate(char() | binary(), Count :: non_neg_integer()) -> binary().
duplicate(Char, Count) when is_integer(Char) ->
    duplicate_char(Char, Count, <<>>);
duplicate(Str, Count) ->
    duplicate_bin(Str, Count, <<>>).

duplicate_char(Char, Count, Acc) when Count > 0 ->
    duplicate_char(Char, Count - 1, <<Acc/binary, Char>>);
duplicate_char(_Char, _Len, Acc) ->
    Acc.

duplicate_bin(Str, Count, Acc) when Count > 0 ->
    duplicate_bin(Str, Count - 1, <<Acc/binary, Str/binary>>);
duplicate_bin(_Str, _Len, Acc) ->
    Acc.


%% @doc Return a substring starting at position 'Pos'.
-spec substr(binary(), integer()) -> binary().
substr(Str, 1) ->
    Str;
substr(Str, Pos) ->
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
left(Str, Len) when Len >= size(Str) ->
    Str;
left(Str, Len) when Len >= 0 ->
    <<Left:Len/binary, _Tail/binary>> = Str,
    Left.


%% @doc Return a substring of 'Len' bytes starting from the end of the string.
%%      If the string does not have enough characters, the original string is
%%      returned.
-spec right(binary(), integer()) -> binary().
right(Str, Len) when Len >= size(Str) ->
    Str;
right(Str, Len) when Len >= 0 ->
    Offset = size(Str) - Len,
    <<_Head:Offset/binary, Right/binary>> = Str,
    Right.


%% @doc Return a string of 'Len' bytes padded with spaces to the left and to the right.
-spec pad(binary(), non_neg_integer()) -> binary().
pad(Str, Len) when Len >= 0 ->
    pad(Str, Len, $\s).

%% @doc Return a string of 'Len' bytes padded with 'Chars' to the left and to the right.
-spec pad(binary(), non_neg_integer(), char()) -> binary().
pad(Str, Len, Char) when Len >= 0, is_integer(Char) ->
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
lpad(Str, Len) when Len >= 0 ->
    lpad(Str, Len, $\s).

%% @doc Return a string of 'Len' bytes left-padded with 'Chars'.
-spec lpad(binary(), non_neg_integer(), char()) -> binary().
lpad(Str, Len, Char) when Len >= 0, is_integer(Char) ->
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
rpad(Str, Len) when Len >= 0 ->
    rpad(Str, Len, $\s).

%% @doc Return a string of 'Len' bytes right-padded with 'Chars'.
-spec rpad(binary(), non_neg_integer(), char()) -> binary().
rpad(Str, Len, Char) when Len >= 0, is_integer(Char) ->
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
strip(Str) ->
    strip(Str, <<"\s\t\n\r\f\v">>).

%% @doc Remove all the 'Chars' present both to the left and to the right of the string.
-spec strip(binary(), char() | binary()) -> binary().
strip(Str, Char) ->
    rstrip(lstrip(Str, Char), Char).


%% @doc Remove all the spaces present to the left of the string.
-spec lstrip(binary()) -> binary().
lstrip(Str) ->
    lstrip(Str, <<"\s\t\n\r\f\v">>).

%% @doc Remove all the 'Chars' present to the left of the string.
-spec lstrip(binary(), char() | binary()) -> binary().
lstrip(Str, Char) when is_integer(Char) ->
    lstrip_char(Str, Char);
lstrip(Str, Chars) when is_binary(Chars) ->
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
rstrip(Str) ->
    rstrip(Str, <<"\s\t\n\r\f\v">>).

%% @doc Remove all the 'Chars' present to the right of the string.
-spec rstrip(binary(), char() | binary()) -> binary().
rstrip(Str, Char) when is_integer(Char) ->
    rstrip_char(Str, Char, size(Str) - 1);
rstrip(Str, Chars) ->
    rstrip_bin(Str, Chars, size(Str) - 1).

%% @hidden
rstrip_char(Str, Char, Pos) ->
    case Str of
        <<Head:Pos/binary, Char>> ->
            rstrip_char(Head, Char, Pos - 1);
        _ ->
            Str
    end.

%% @hidden
rstrip_bin(Str, Chars, Pos) ->
    case Str of
        <<Head:Pos/binary, Char>> ->
            case member(Chars, Char) of
                true ->
                    rstrip_bin(Head, Chars, Pos - 1);
                _ ->
                    Str
            end;

        _ ->
            Str
    end.


%% @doc Remove all the newlines (\r and \n) present at the end of the string.
-spec chomp(binary()) -> binary().
chomp(Str) ->
    Pos = size(Str) - 1,
    case Str of
        <<Head:Pos/binary, Char>> when Char =:= $\r; Char =:= $\n ->
            chomp(Head);
        _ ->
            Str
    end.


%% @doc Divide a string into a list of tokens that were originally separated
%%      by the character 'Sep'.
-spec split(binary(), Sep::char() | binary()) -> list(binary()).
split(<<>>, _Sep) ->
    [];
split(Str, Sep) when is_integer(Sep) ->
    lists:reverse(split_char_sep(Str, <<>>, Sep, []));
split(Str, Sep)  ->
    Tokens =
        case Sep of
            <<Char>> ->
                split_char_sep(Str, <<>>, Char, []);
            _ ->
                split_str_sep(Str, <<>>, Sep, [])
        end,
    lists:reverse(Tokens).

%% @doc Helper function used to tokenize a string when the separator is a character.
-spec split_char_sep(binary(), binary(), char(), [binary()]) -> [binary()].
split_char_sep(<<Sep, Tail/binary>>, TokenAcc, Sep, Tokens) ->
    split_char_sep(Tail, <<>>, Sep, [TokenAcc | Tokens]);
split_char_sep(<<Char, Tail/binary>>, TokenAcc, Sep, Tokens) ->
    split_char_sep(Tail, <<TokenAcc/binary, Char>>, Sep, Tokens);
split_char_sep(<<>>, TokenAcc, _Sep, Tokens) ->
    [TokenAcc | Tokens].

%% @doc Helper function used to tokenize a string when there are multiple separators.
-spec split_str_sep(binary(), binary(), binary(), [binary()]) -> [binary(),...].
split_str_sep(<<Char, Tail/binary>>, TokenAcc, Sep, Tokens) ->
    case member(Sep, Char) of
        true ->
            split_str_sep(Tail, <<>>, Sep, [TokenAcc | Tokens]);
        false ->
            split_str_sep(Tail, <<TokenAcc/binary, Char>>, Sep, Tokens)
    end;
split_str_sep(<<>>, TokenAcc, _Sep, Tokens) ->
    [TokenAcc | Tokens].


%% @doc Join a a list of strings into one string.
-spec join([binary()]) -> binary().
%% join(List) when is_list(List) ->
%%     join_list(List, <<>>).
%% join_list([Head|Tail], Acc) ->
%%     Value = bstr(Head),
%%     join_list(Tail, <<Acc/binary, Value/binary>>);
%% join_list([], Acc) ->
%%     Acc.
%% This version is about 5% faster than the one above with Erlang R14B01.
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


%% @doc  Join a a list of strings into one string, adding a separator between
%%       each string and escaping both the separator and the escape char itself
%%       with the escape char.
%%
%%       e.g.:
%%       ``bstr:join([<<"1">>, <<",">>, <<"\1">>, <<"2,3">>], $,, $\) -> <<"1,\,,\\1,2\,3">>.''
%% @end
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

%% @doc Convert all the characters in a binary to lowercase.
-spec lower(binary() | char()) -> binary() | char().
lower(Char) when is_integer(Char) ->
    case char:is_upper(Char) of
        true ->
            Char - $A + $a;
        false ->
            Char
    end;
lower(Str) ->
    lower(Str, <<>>).

lower(<<Char, Tail/binary>>, Acc) ->
    case char:is_upper(Char) of
        true ->
            Lower = Char - $A + $a,
            lower(Tail, <<Acc/binary, Lower>>);
        false ->
            lower(Tail, <<Acc/binary, Char>>)
    end;
lower(<<>>, Acc) ->
    Acc.


%% @doc Convert all the characters in a binary to uppercase.
-spec upper(binary() | char()) -> binary() | char().
upper(Char) when is_integer(Char) ->
    case char:is_lower(Char) of
        true ->
            Char - $a + $A;
        false ->
            Char
    end;
upper(Str) ->
    upper(Str, <<>>).

upper(<<Char, Tail/binary>>, Acc) ->
    case char:is_lower(Char) of
        true ->
            Upper = Char - $a + $A,
            upper(Tail, <<Acc/binary, Upper>>);
        false ->
            upper(Tail, <<Acc/binary, Char>>)
    end;
upper(<<>>, Acc) ->
    Acc.


%% @doc Convert an "object" to a binary.
-spec bstr(binary() | atom() | list() | char()) -> binary().
bstr(Bin) when is_binary(Bin) ->
    Bin;
bstr(Pid) when is_pid(Pid) ->
    list_to_binary(pid_to_list(Pid));
bstr(Atom) when is_atom(Atom) ->
    from_atom(Atom);
bstr(Integer) when is_integer(Integer), Integer > 0, Integer =< 255 ->
    <<Integer>>;
bstr(Integer) when is_integer(Integer) ->
    from_integer(Integer);
bstr(Float) when is_float(Float) ->
    from_float(Float);
bstr(List) when is_list(List) ->
    list_to_binary(List).


%% @doc Convert an atom to a bstr.
-spec from_atom(atom()) -> binary().
from_atom(Atom) ->
    atom_to_binary(Atom, utf8).

%% @doc Convert a bstr containing a string to an Erlang atom.
-spec to_atom(binary()) -> atom().
to_atom(Str) ->
    binary_to_atom(Str, utf8).

%% @doc Convert a bstr containing a string to an Erlang atom only if the atom
%%      already existed (i.e. had been previously defined).
-spec to_existing_atom(binary()) -> atom().
to_existing_atom(Str) ->
    binary_to_existing_atom(Str, utf8).


%% @doc Convert a list containing a string to a binary.
-spec from_list(list()) -> binary().
from_list(List) ->
    list_to_binary(List).


%% @doc Convert a bstr containing a string to an Erlang list/string.
-spec to_list(binary()) -> [byte()].
to_list(Str) ->
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
    integer_to_binary(I).


%% @doc Convert an integer to a bstr in base 'n' format.
%% -spec from_integer(integer(), 1..255) -> binary().
from_integer(I, 10) ->
    erlang:integer_to_binary(I);
from_integer(I, Base)
  when erlang:is_integer(I), erlang:is_integer(Base),
       Base >= 2, Base =< 1+$Z-$A+10 ->
    %% We can't use integer_to_binary/2 in versions <= R16B01 as the function
    %% generates wrong output values in bases other than 10 for 0 and negative
    %% integers.
    if I < 0 ->
	    <<$-,(from_integer1(-I, Base, <<>>))/binary>>;
       true ->
	    from_integer1(I, Base, <<>>)
    end;
from_integer(I, Base) ->
    erlang:error(badarg, [I, Base]).

%%     integer_to_binary(I, Base).
%% integer_to_binary(I0, Base, R0) ->
from_integer1(I0, Base, R0) ->
    D = I0 rem Base,
    I1 = I0 div Base,
    R1 = if
             D >= 10 -> <<(D-10+$A),R0/binary>>;
             true -> <<(D+$0),R0/binary>>
         end,
    if
        I1 =:= 0 -> R1;
        true -> from_integer1(I1,Base,R1)
    end.


%% @doc Convert an integer to a bstr in base 'n' format in the specified case.
-spec from_integer(integer(), 2..36, upper | lower) -> binary().
from_integer(I, Base, Case) when is_integer(I), is_integer(Base), Base >= 2, Base =< $Z - $A + 11 ->
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
to_integer(Str) ->
    binary_to_integer(Str).


%% @doc Convert a bstr containing a string representing a positive number
%%      in the specified 'Base' to an integer. 'Base' must be an integer
%%      between 2 and 32.
% Optimized version for base 10
-spec to_integer(binary(), 1..255) -> integer().
to_integer(Str, Base) ->
    binary_to_integer(Str, Base).


%% @doc Convert a floating point number to a bstr.
-spec from_float(float()) -> binary().
from_float(Float) ->
    float_to_binary(Float, [{decimals, 10}, compact]).


%% @doc Convert a bstr formatted as a floating point number to a float.
-spec to_float(binary()) -> float().
to_float(Str) ->
    binary_to_float(Str).


%% @doc Convert an integer or floating point number into a bstr.
-spec from_number(integer() | float()) -> binary().
from_number(Number) when is_float(Number) ->
    float_to_binary(Number, [{decimals, 10}, compact]);
from_number(Number) ->
    integer_to_binary(Number).


%% @doc Convert a formatted binary into an integer or floating point number.
-spec to_number(binary()) -> integer() | float().
to_number(Str) ->
    case member(Str, $.) of
        true  -> binary_to_float(Str);
        false -> binary_to_integer(Str)
    end.


%% @doc Get the first text line from a binary string. It returns a tuple with
%%      the first text line as the first element and the rest of the string as
%%      the last element.
-spec get_line(binary()) -> {binary(), binary()}.
get_line(Str) ->
    get_line(Str, <<>>).

get_line(<<$\n, Tail/binary>>, Acc) ->
    {Acc, Tail};
get_line(<<$\r, $\n, Tail/binary>>, Acc) ->
    {Acc, Tail};
get_line(<<Char, Tail/binary>>, Acc) ->
    get_line(Tail, <<Acc/binary, Char>>);
get_line(<<>>, Acc) ->
    {Acc, <<>>}.


%% @doc Encode a bstr using the URL-encoding scheme.
-spec urlencode(binary()) -> binary().
urlencode(Str) ->
    urlencode(Str, <<>>).

urlencode(<<Char, Tail/binary>>, Acc) ->
    urlencode(Tail,
              case is_urlencoded(Char) of
                  true ->
                      Hi = integer_to_hex_char((Char band 16#f0) bsr 4),
                      Lo = integer_to_hex_char((Char band 16#0f)),
                      <<Acc/binary, $%, Hi, Lo>>;
                  false ->
                      <<Acc/binary, Char>>
              end
             );
urlencode(<<>>, Acc) ->
    Acc.



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
    urldecode(Str, <<>>).

urldecode(<<$%, Hi, Lo, Tail/binary>>, Acc) ->
    Char = ((hex_char_to_integer(Hi) bsl 4) bor hex_char_to_integer(Lo)),
    urldecode(Tail, <<Acc/binary, Char>>);
urldecode(<<$%, _Tail/binary>>, _Acc) ->
    erlang:error(badarg);
urldecode(<<Char, Tail/binary>>, Acc) ->
    urldecode(Tail, <<Acc/binary, Char>>);
urldecode(<<>>, Acc) ->
    Acc.


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
xmlencode(Str) ->
    xmlencode(Str, <<>>).

xmlencode(<<Char, Tail/binary>>, Acc) ->
    xmlencode(Tail,
              case is_xmlencoded(Char) of
                  true ->
                      Encoded = xmlencode_char(Char),
                      <<Acc/binary, $&, Encoded/binary, $;>>;
                  false ->
                      <<Acc/binary, Char>>
              end
             );
xmlencode(<<>>, Acc) ->
    Acc.


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
    xmldecode(Str, <<>>).

xmldecode(<<"&amp;", Tail/binary>>, Acc) ->
    xmldecode(Tail, <<Acc/binary, $&>>);
xmldecode(<<"&lt;", Tail/binary>>, Acc) ->
    xmldecode(Tail, <<Acc/binary, $<>>);
xmldecode(<<"&gt;", Tail/binary>>, Acc) ->
    xmldecode(Tail, <<Acc/binary, $>>>);
xmldecode(<<"&apos;", Tail/binary>>, Acc) ->
    xmldecode(Tail, <<Acc/binary, $'>>);
xmldecode(<<"&quot;", Tail/binary>>, Acc) ->
    xmldecode(Tail, <<Acc/binary, $">>);
xmldecode(<<"&#x", Hi, Lo, $;, Tail/binary>>, Acc) ->
    Char = (hex_char_to_integer(Hi) bsl 4) bor hex_char_to_integer(Lo),
    xmldecode(Tail, <<Acc/binary, Char>>);
xmldecode(<<Char, Tail/binary>>, Acc) ->
    xmldecode(Tail, <<Acc/binary, Char>>);
xmldecode(<<>>, Acc) ->
    Acc.


%% @doc Encode a bstr converting each character to its hexadecimal
%%      representation.
-spec hexencode(binary()) -> binary().
hexencode(Str) ->
    hexencode(Str, <<>>).

hexencode(<<Hi:4, Lo:4, Tail/binary>>, Acc) ->
    HiChar = integer_to_hex_char(Hi, lower),
    LoChar = integer_to_hex_char(Lo, lower),
    hexencode(Tail, <<Acc/binary, HiChar, LoChar>>);
hexencode(<<>>, Acc) ->
    Acc.


%% @doc Decode a bstr with an hexadecimal representation of a string.
-spec hexdecode(binary()) -> binary().
hexdecode(Str) ->
    hexdecode(Str, <<>>).

hexdecode(<<Hi, Lo, Tail/binary>>, Acc) ->
    Char = ((hex_char_to_integer(Hi) bsl 4) bor hex_char_to_integer(Lo)),
    hexdecode(Tail, <<Acc/binary, Char>>);
% If the number of characters wasn't even we raise an exception.
hexdecode(<<_Char>>, _Acc) ->
    erlang:error(badarg);
hexdecode(<<>>, Acc) ->
    Acc.
