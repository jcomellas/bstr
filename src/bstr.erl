%% @author Juan Jose Comellas <juanjo@comellas.org
%% @copyright 2008, Juan Jose Comellas

%% @doc String implemented over an Erlang binary.

-module(bstr).
-author('Juan Jose Comellas <juanjo@comellas.org>').

-export([empty/1, len/1, equal/2, concat/2, nth/2, index/2, rindex/2,
         member/2, prefix/2, suffix/2, is_utf8/1,
         is_alpha/1, is_alnum/1, is_lower/1, is_upper/1, is_digit/1, is_xdigit/1,
         is_blank/1, is_space/1, is_atom_as_binary/1,
         insert/3, duplicate/2, substr/2, substr/3, left/2, right/2,
         pad/2, pad/3, lpad/2, lpad/3, rpad/2, rpad/3,
         strip/1, strip/2, lstrip/1, lstrip/2, rstrip/1, rstrip/2, chomp/1,
         split/2, join/1, join/2, lower/1, upper/1, bstr/1,
         from_atom/1, to_atom/1, to_existing_atom/1, from_list/1, to_list/1,
         from_integer/1, from_integer/2, from_integer/3,
         to_integer/1, to_integer/2, to_boolean/1,
         get_line/1, urlencode/1, urldecode/1, xmlencode/1, xmldecode/1,
         hexencode/1, hexdecode/1, replace/3, copy/1, copy/2]).


%%--------------------------------------------------------------------
%% @spec empty(bstr() | undefined) -> boolean()
%% @doc  Return true if a string is empty.
%%--------------------------------------------------------------------
empty(<<>>) ->
    true;
empty(undefined) ->
    true;
empty(Str) when is_binary(Str) ->
    false.


%%--------------------------------------------------------------------
%% @spec len(bstr()) -> integer()
%% @doc  Return the length of a string.
%%--------------------------------------------------------------------
len(Str) when is_binary(Str) ->
    size(Str).


%%--------------------------------------------------------------------
%% @spec equal(bstr(), bstr()) -> true | false
%% @doc  Test if 2 strings are equal.
%%--------------------------------------------------------------------
equal(Str, Str) when is_binary(Str) ->
    true;
equal(Str, _) when is_binary(Str) ->
    false.


%%--------------------------------------------------------------------
%% @spec concat(bstr(), bstr()) -> bstr()
%% @doc  Concatenate 2 strings.
%%--------------------------------------------------------------------
concat(Str1, Str2) when is_binary(Str1), is_binary(Str2) ->
    <<Str1/binary, Str2/binary>>.


%%--------------------------------------------------------------------
%% @spec nth(bstr(), integer()) -> char()
%% @doc  Return the character in the nth position of the string.
%%--------------------------------------------------------------------
nth(Str, Pos) when is_binary(Str), Pos > 0, Pos =< size(Str) ->
    Offset = Pos - 1,
    <<_Head:Offset/binary, Char, _Tail/binary>> = Str,
    Char.


%%--------------------------------------------------------------------
%% @spec index(bstr(), char()) -> integer()
%% @doc  Return the index of the first appearance of a character in a string.
%%--------------------------------------------------------------------
index(Str, Char) when is_binary(Str), is_integer(Char) ->
   index(Str, Char, 0).
index(<<Char, _Tail/binary>>, Char, N) ->
   N;
index(<<_Char, Tail/binary>>, Char, N) ->
   index(Tail, Char, N + 1);
index(<<>>, _Char, _N) ->
    -1.


%%--------------------------------------------------------------------
%% @spec rindex(bstr(), char()) -> integer()
%% @doc  Return the index of the last appearance of a character in a string.
%%--------------------------------------------------------------------
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


%%--------------------------------------------------------------------
%% @spec member(bstr(), char()) -> bool()
%% @doc  Return whether the character is present in the string.
%%--------------------------------------------------------------------
member(Str, Char) ->
    index(Str, Char) >= 0.


%%--------------------------------------------------------------------
%% @spec prefix(bstr(), bstr()) -> bool()
%% @doc  Indicates whether a string is a prefix of another one.
%%--------------------------------------------------------------------
prefix(Str, Prefix) when is_binary(Str), is_binary(Prefix) ->
    N = size(Prefix),
    case Str of
        <<Prefix:N/binary, _Tail/binary>> ->
            true;
        _ ->
            false
    end.


%%--------------------------------------------------------------------
%% @spec suffix(bstr(), bstr()) -> bool()
%% @doc  Indicates whether a string is a suffix of another one.
%%--------------------------------------------------------------------
suffix(Str, Suffix) when is_binary(Str), is_binary(Suffix) ->
    N = size(Str) - size(Suffix),
    case Str of
        <<_Head:N/binary, Suffix/binary>> ->
            true;
        _ ->
            false
    end.


%%--------------------------------------------------------------------
%% @spec is_utf8(binary()) -> boolean()
%% @doc  Determines if a string is composed of utf-8 characters.
%%--------------------------------------------------------------------
is_utf8(Bin) when is_binary(Bin) ->
	case unicode:characters_to_binary(Bin, utf8, utf8) of
		Bin ->
			true;
		_ ->
			false
    end.


%%--------------------------------------------------------------------
%% @spec is_alpha(binary()) -> boolean()
%% @doc  Determines if a string is composed of alphabetic characters.
%%--------------------------------------------------------------------
is_alpha(<<>>) ->
    false;
is_alpha(Str) when is_binary(Str) ->
    is_x(Str, fun char:is_alpha/1);
is_alpha(Char) when is_integer(Char) ->
    char:is_alpha(Char).


%%--------------------------------------------------------------------
%% @spec is_alnum(binary()) -> boolean()
%% @doc  Determines if a string is composed of alphanumeric characters.
%%--------------------------------------------------------------------
is_alnum(<<>>) ->
    false;
is_alnum(Str) when is_binary(Str) ->
    is_x(Str, fun char:is_alnum/1);
is_alnum(Char) when is_integer(Char) ->
    char:is_alnum(Char).


%%--------------------------------------------------------------------
%% @spec is_lower(binary()) -> boolean()
%% @doc  Determines if a string is composed of lower-case alphabetic characters.
%%--------------------------------------------------------------------
is_lower(<<>>) ->
    false;
is_lower(Str) when is_binary(Str) ->
    is_x(Str, fun char:is_lower/1);
is_lower(Char) when is_integer(Char) ->
    char:is_lower(Char).


%%--------------------------------------------------------------------
%% @spec is_upper(binary()) -> boolean()
%% @doc  Determines if a string is composed of upper-case alphabetic characters.
%%--------------------------------------------------------------------
is_upper(<<>>) ->
    false;
is_upper(Str) when is_binary(Str) ->
    is_x(Str, fun char:is_upper/1);
is_upper(Char) when is_integer(Char) ->
    char:is_upper(Char).


%%--------------------------------------------------------------------
%% @spec is_digit(binary()) -> boolean()
%% @doc  Determines if a string is composed of digits.
%%--------------------------------------------------------------------
is_digit(<<>>) ->
    false;
is_digit(Str) when is_binary(Str) ->
    is_x(Str, fun char:is_digit/1);
is_digit(Char) when is_integer(Char) ->
    char:is_digit(Char).


%%--------------------------------------------------------------------
%% @spec is_xdigit(binary()) -> boolean()
%% @doc  Determines if a string is composed of hexadecimal digits.
%%--------------------------------------------------------------------
is_xdigit(<<>>) ->
    false;
is_xdigit(Str) when is_binary(Str) ->
    is_x(Str, fun char:is_xdigit/1);
is_xdigit(Char) when is_integer(Char) ->
    char:is_xdigit(Char).


%%--------------------------------------------------------------------
%% @spec is_blank(binary()) -> boolean()
%% @doc  Determines if a string is composed of blank characters.
%%--------------------------------------------------------------------
is_blank(<<>>) ->
    false;
is_blank(Str) when is_binary(Str) ->
    is_x(Str, fun char:is_blank/1);
is_blank(Char) when is_integer(Char) ->
    char:is_blank(Char).


%%--------------------------------------------------------------------
%% @spec is_space(binary()) -> boolean()
%% @doc  Determines if a string is composed of spaces or tabs.
%%--------------------------------------------------------------------
is_space(<<>>) ->
    false;
is_space(Str) when is_binary(Str) ->
    is_x(Str, fun char:is_space/1);
is_space(Char) when is_integer(Char) ->
    char:is_space(Char).


%%--------------------------------------------------------------------
%% @spec is_atom_as_binary(binary()) -> boolean()
%% @doc  Determines if a string is an unquoted atom.
%%--------------------------------------------------------------------
is_atom_as_binary(<<>>) ->
    false;
is_atom_as_binary(<<Char, Tail/binary>>) ->
    char:is_lower(Char) andalso is_x(Tail, fun char:is_atom/1);
is_atom_as_binary(Char) when is_integer(Char) ->
    char:is_atom(Char).


%%--------------------------------------------------------------------
%% @spec is_x(Str::bstr(), Fun::fun(), Offset::integer()) -> boolean()
%% @doc  Helper function used to check whether all the characters in a string
%%       meet a specific criteria that is passed as a function to it.
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
is_x(<<Char, Tail/binary>>, Fun) ->
    case Fun(Char) of
        true ->
            is_x(Tail, Fun);
        false ->
            false
    end;
is_x(<<>>, _Fun) ->
    true.


%%--------------------------------------------------------------------
%% @spec insert(bstr(), integer(), bstr()) -> bstr()
%% @doc  Insert a string into another one at the indicated position.
%%--------------------------------------------------------------------
insert(Str, Pos, Str1) when is_binary(Str), is_integer(Pos) ->
    N = Pos - 1,
    case Str of
        <<Head:N/binary, Tail/binary>> ->
            <<Head/binary, Str1/binary, Tail/binary>>;
        _ ->
            erlang:error(badarg)
    end.


%%--------------------------------------------------------------------
%% @spec duplicate(bstr(), Number) -> bstr()
%% @doc  Return 'Number' copies of a string.
%%--------------------------------------------------------------------
duplicate(Str, Num) ->
    duplicate(Str, Num, []).
duplicate(Str, Num, Acc) when Num > 0 ->
    duplicate(Str, Num - 1, [Str | Acc]);
duplicate(_Str, _Num, Acc) ->
    erlang:list_to_binary(Acc).


%%--------------------------------------------------------------------
%% @spec substr(bstr(), Pos) -> bstr()
%% @doc  Return a substring starting at position 'Pos'.
%%--------------------------------------------------------------------
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
%%--------------------------------------------------------------------
%% @spec substr(bstr(), Pos::integer(), Len::integer()) -> bstr()
%% @doc  Return a substring starting at position 'Pos' with a length of 'Len' bytes.
%%--------------------------------------------------------------------
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


%%--------------------------------------------------------------------
%% @spec left(bstr(), Len) -> bstr()
%% @doc  Return a substring of 'Len' bytes starting from the beginning of the 
%%       string. If the string does not have enough characters, the original 
%%       string is returned.
%%--------------------------------------------------------------------
left(Str, Len) when is_binary(Str), Len >= size(Str) ->
    Str;
left(Str, Len) when is_binary(Str), Len >= 0 ->
    <<Left:Len/binary, _Tail/binary>> = Str,
    Left.


%%--------------------------------------------------------------------
%% @spec right(bstr(), Len) -> bstr()
%% @doc  Return a substring of 'Len' bytes starting from the end of the string.
%%       If the string does not have enough characters, the original string is
%%       returned.
%%--------------------------------------------------------------------
right(Str, Len) when is_binary(Str), Len >= size(Str) ->
    Str;
right(Str, Len) when is_binary(Str), Len >= 0 ->
    Offset = size(Str) - Len,
    <<_Head:Offset/binary, Right/binary>> = Str,
    Right.


%%--------------------------------------------------------------------
%% @spec pad(bstr(), Len) -> bstr()
%% @doc  Return a string of 'Len' bytes padded with spaces to the left and to the right.
%%--------------------------------------------------------------------
pad(Str, Len) when is_binary(Str), Len >= 0 ->
    pad(Str, Len, $\s).
%%--------------------------------------------------------------------
%% @spec pad(bstr(), Len, Char) -> bstr()
%% @doc  Return a string of 'Len' bytes padded with 'Chars' to the left and to the right.
%%--------------------------------------------------------------------
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


%%--------------------------------------------------------------------
%% @spec lpad(bstr(), Len) -> bstr()
%% @doc  Return a string of 'Len' bytes left-padded with spaces.
%%--------------------------------------------------------------------
lpad(Str, Len) when is_binary(Str), Len >= 0 ->
    lpad(Str, Len, $\s).
%%--------------------------------------------------------------------
%% @spec lpad(bstr(), Len, Char) -> bstr()
%% @doc  Return a string of 'Len' bytes left-padded with 'Chars'.
%%--------------------------------------------------------------------
lpad(Str, Len, Char) when is_binary(Str), Len >= 0 ->
    PadLen = Len - size(Str),
    if
        PadLen > 0 ->
            Padding = duplicate(Char, PadLen),
            <<Padding/binary, Str/binary>>;
        true ->
            Str
    end.


%%--------------------------------------------------------------------
%% @spec rpad(bstr(), Len) -> bstr()
%% @doc  Return a string of 'Len' bytes right-padded with spaces.
%%--------------------------------------------------------------------
rpad(Str, Len) when is_binary(Str), Len >= 0 ->
    rpad(Str, Len, $\s).
%%--------------------------------------------------------------------
%% @spec rpad(bstr(), Len, Char) -> bstr()
%% @doc  Return a string of 'Len' bytes right-padded with 'Chars'.
%%--------------------------------------------------------------------
rpad(Str, Len, Char) ->
    PadLen = Len - size(Str),
    if
        PadLen > 0 ->
            Padding = duplicate(Char, PadLen),
            <<Str/binary, Padding/binary>>;
        true ->
            Str
    end.


%%--------------------------------------------------------------------
%% @spec strip(bstr()) -> bstr()
%% @doc  Remove all the spaces present both to the left and to the right of the string.
%%--------------------------------------------------------------------
strip(Str) when is_binary(Str) ->
    strip(Str, $\s).
%%--------------------------------------------------------------------
%% @spec strip(bstr(), Char) -> bstr()
%% @doc  Remove all the 'Chars' present both to the left and to the right of the string.
%%--------------------------------------------------------------------
strip(Str, Char) when is_binary(Str), is_integer(Char) ->
    rstrip(lstrip(Str, Char), Char).


%%--------------------------------------------------------------------
%% @spec lstrip(bstr()) -> bstr()
%% @doc  Remove all the spaces present to the left of the string.
%%--------------------------------------------------------------------
lstrip(Str) when is_binary(Str) ->
    lstrip(Str, $\s).
%%--------------------------------------------------------------------
%% @spec lstrip(bstr(), Char) -> bstr()
%% @doc  Remove all the 'Chars' present to the left of the string.
%%--------------------------------------------------------------------
lstrip(Str, Char) when is_binary(Str), is_integer(Char) ->
    lstrip(Str, Char, 0).
lstrip(Str, Char, Pos) ->
    case Str of
        <<_Head:Pos/binary, Char, _Tail/binary>> ->
            lstrip(Str, Char, Pos + 1);
        <<_Head:Pos/binary, Tail/binary>> ->
            Tail;
        _ ->
            <<>>
    end.


%%--------------------------------------------------------------------
%% @spec rstrip(bstr()) -> bstr()
%% @doc  Remove all the spaces present to the right of the string.
%%--------------------------------------------------------------------
rstrip(Str) when is_binary(Str) ->
    rstrip(Str, $\s).
%%--------------------------------------------------------------------
%% @spec rstrip(bstr(), Char) -> bstr()
%% @doc  Remove all the 'Chars' present to the right of the string.
%%--------------------------------------------------------------------
rstrip(Str, Char) when is_binary(Str), is_integer(Char) ->
    rstrip(Str, Char, size(Str) - 1).
rstrip(Str, Char, Pos) ->
    case Str of
        <<_Head:Pos/binary, Char, _Tail/binary>> ->
            rstrip(Str, Char, Pos - 1);
        <<_Head:Pos/binary, _Tail/binary>> ->
            N = Pos + 1,
            <<Stripped:N/binary, _Dummy/binary>> = Str,
            Stripped;
        _ ->
            <<>>
    end.


%%--------------------------------------------------------------------
%% @spec chomp(bstr()) -> bstr()
%% @doc  Remove all the newlines (\r and \n) present at the end of the string.
%%--------------------------------------------------------------------
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


%%--------------------------------------------------------------------
%% @spec split(bstr(), Sep::char() | Sep::bstr()) -> list(bstr())
%% @doc  Divide a string into a list of tokens that were originally separated
%%       by the character 'Sep'.
%%--------------------------------------------------------------------
split(<<>>, _Sep) ->
    [];
split(Str, Sep) when is_binary(Str), is_integer(Sep) ->
    lists:reverse(split_char_sep(Str, Sep, 0, []));
split(Str, Sep) when is_binary(Str), is_binary(Sep) ->
    case Sep of
        <<Char>> ->
            Tokens = split_char_sep(Str, Char, 0, []);
        _ ->
            Tokens = split_str_sep(Str, Sep, 0, [])
    end,
    lists:reverse(Tokens).
%% Helper function used to tokenize a string when the separator is a character.
split_char_sep(Str, Sep, Pos, Tokens) ->
    case Str of
        <<Token:Pos/binary, Sep, Tail/binary>> ->
            split_char_sep(Tail, Sep, 0, [Token | Tokens]);
        <<_Head:Pos/binary, _Tail/binary>> ->
            split_char_sep(Str, Sep, Pos + 1, Tokens);
        _ ->
            [Str | Tokens]
    end.
%% Helper function used to tokenize a string when there are multiple separators.
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


%%--------------------------------------------------------------------
%% @spec join(list(bstr())) -> bstr()
%% @doc  Join a a list of strings into one string.
%%--------------------------------------------------------------------
% join(List) when is_list(List) ->
%     join_list(List, <<>>).
% join_list([Head|Tail], Acc) ->
%     Value = bstr(Head),
%     join_list(Tail, <<Acc/binary, Value/binary>>);
% join_list([], Acc) ->
%     Acc.
%% This version is about 30% faster than the one above. Re-test once Erlang R12B
%% is released. This test was performed before adding support for deep lists.
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


%%--------------------------------------------------------------------
%% @spec join(list(bstr()), Sep::char() | Sep::bstr()) -> bstr()
%% @doc  Join a a list of strings into one string, adding a separator between
%%       each string.
%%--------------------------------------------------------------------
join(List, Sep) when is_list(List) ->
    list_to_binary(join_list_sep(List, Sep)).

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
%% @spec lower(bstr() | char()) -> bstr() | char()
%% @doc  Convert all the characters in a bstr to lowercase.
%%--------------------------------------------------------------------
lower(Str) when is_binary(Str) ->
    lower_nocopy(Str, 0);
lower(Char) when is_integer(Char) ->
    char:lower(Char).

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


%%--------------------------------------------------------------------
%% @spec upper(bstr() | char()) -> bstr() | char()
%% @doc  Convert all the characters in a bstr to uppercase.
%%--------------------------------------------------------------------
upper(Str) when is_binary(Str) ->
    upper_nocopy(Str, 0);
upper(Char) when is_integer(Char) ->
    char:upper(Char).

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



%%--------------------------------------------------------------------
%% @spec bstr(binary() | atom() | list() | char()) -> bstr()
%% @doc  Convert an "object" to a bstr.
%%--------------------------------------------------------------------
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


%%--------------------------------------------------------------------
%% @spec from_atom(atom()) -> bstr()
%% @doc  Convert an atom to a bstr.
%%--------------------------------------------------------------------
from_atom(Atom) when is_atom(Atom) ->
    list_to_binary(atom_to_list(Atom)).

%%--------------------------------------------------------------------
%% @spec to_atom(bstr()) -> atom()
%% @doc  Convert a bstr containing a string to an Erlang atom.
%%--------------------------------------------------------------------
to_atom(Str) when is_binary(Str) ->
    list_to_atom(binary_to_list(Str)).

%%--------------------------------------------------------------------
%% @spec to_existing_atom(bstr()) -> atom()
%% @doc  Convert a bstr containing a string to an Erlang atom only if the atom
%%       already existed (i.e. had been previously defined.
%%--------------------------------------------------------------------
to_existing_atom(Str) when is_binary(Str) ->
    list_to_existing_atom(binary_to_list(Str)).


%%--------------------------------------------------------------------
%% @spec from_list(list()) -> bstr()
%% @doc  Convert a list containing a string to a bstr.
%%--------------------------------------------------------------------
from_list(List) when is_list(List) ->
    list_to_binary(List).


%%--------------------------------------------------------------------
%% @spec to_list(bstr()) -> string()
%% @doc  Convert a bstr containing a string to an Erlang list/string.
%%--------------------------------------------------------------------
to_list(Str) when is_binary(Str) ->
    binary_to_list(Str).


%%--------------------------------------------------------------------
%% @spec to_list(bstr()) -> string()
%% @doc  Convert a bstr containing a string to an Erlang list/string.
%%--------------------------------------------------------------------
to_boolean(<<"true">>) ->
    true;
to_boolean(<<"false">>) ->
    false.


%%--------------------------------------------------------------------
%% @spec from_integer(integer()) -> bstr()
%% @doc  Convert an integer to a bstr in base 10 format.
%%--------------------------------------------------------------------
from_integer(I) ->
    from_integer(I, 10, upper).


%%--------------------------------------------------------------------
%% @spec from_integer(integer(), Base::integer()) -> bstr()
%% @doc  Convert an integer to a bstr in base 'n' format.
%%--------------------------------------------------------------------
from_integer(I, Base) ->
    from_integer(I, Base, upper).

%%--------------------------------------------------------------------
%% @spec from_integer(integer(), Base::integer(), upper | lower) -> bstr()
%% @doc  Convert an integer to a bstr in base 'n' format in the specified case.
%%--------------------------------------------------------------------
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


%%--------------------------------------------------------------------
%% @spec to_integer(bstr()) -> integer()
%% @doc  Convert a bstr containing a string representing a decimal number
%%       to an integer.
%%--------------------------------------------------------------------
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


%%--------------------------------------------------------------------
%% @spec to_integer(bstr(), Base::integer()) -> integer()
%% @doc  Convert a bstr containing a string representing a positive number
%%       in the specified 'Base' to an integer. 'Base' must be an integer
%%       between 2 and 32.
%%--------------------------------------------------------------------
% Optimized version for base 10
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
            if
                (Char >= $0) and (Char =< $9) and (Char < Base + $0) ->
                    N = (Char - $0);
                (Char >= $A) and (Char =< $Z) and (Char < Base + $A) ->
                    N = 10 + (Char - $A);
                (Char >= $a) and (Char =< $z) and (Char < Base + $a) ->
                    N = 10 + (Char - $a);
                true ->
                    %% We throw the same exception thrown by list_to_integer() if an
                    %% invalid character is found
                    erlang:error(badarg, [Str, Base]),
                    %% To avoid compiler warning
                    N = 0
            end,
            to_base_n_integer(Str, Offset + 1, Base, Acc * Base + N);

        <<_Head:Offset/binary>> ->
            Acc;

        _Number ->
            %% We throw the same exception thrown by list_to_integer() if a
            %% non-numeric character is found
            erlang:error(badarg, [Str])
    end.


%%--------------------------------------------------------------------
%% @spec get_line(bstr()) -> {bstr(), bstr{}}
%% @doc  Get the first text line from a binary string. It returns a tuple with
%%       the first text line as the first element and the rest of the string as
%%       the last element.
%%--------------------------------------------------------------------
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


%%--------------------------------------------------------------------
%% @spec urlencode(bstr()) -> bstr()
%% @doc  Encode a bstr using the URL-encoding scheme.
%%--------------------------------------------------------------------
urlencode(Str) when is_binary(Str) ->
    urlencode(Str, 0).
%% This part of the function iterates over the bstr without copying any data
%% and once it finds a character that has to be URL-encoded it switches to
%% the version of the function that accumulates the converted bstr.
urlencode(Str, N) ->
    case Str of
        <<Head:N/binary, Char, _Tail/binary>> ->
            case char:must_urlencode(Char) of
                true ->
                    Hi = char:integer_to_hex((Char band 16#f0) bsr 4),
                    Lo = char:integer_to_hex((Char band 16#0f)),
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
            case char:must_urlencode(Char) of
                true ->
                    Hi = char:integer_to_hex((Char band 16#f0) bsr 4),
                    Lo = char:integer_to_hex((Char band 16#0f)),
                    urlencode(Str, N + 1, [Lo, Hi, $% | Acc]);

                false ->
                    urlencode(Str, N + 1, [Char | Acc])
            end;
        _ ->
            list_to_binary(lists:reverse(Acc))
    end.


%%--------------------------------------------------------------------
%% @spec urldecode(bstr()) -> bstr()
%% @doc  Decode a bstr using the URL-encoding scheme.
%%--------------------------------------------------------------------
urldecode(Str) ->
    urldecode(Str, 0).
%% This part of the function iterates over the bstr without copying any data
%% and once it finds a character that is URL-encoded it switches to
%% the version of the function that accumulates the converted bstr.
urldecode(Str, N) ->
    case Str of
        <<Head:N/binary, $%, Hi, Lo, _Tail/binary>> ->
            Char = try
                       ((char:hex_to_integer(Hi) bsl 4) bor char:hex_to_integer(Lo))
                   catch
                       _:_ ->
                           $?
                   end,
            urldecode(Str, N + 3, <<Head/binary, Char>>);
        <<_Head:N/binary, Char, _Tail/binary>> ->
            if 
                Char =:= $% ->
                    $?;
                true ->
                    urldecode(Str, N + 1)
            end;
        _ ->
            Str
    end.
urldecode(Str, N, Acc) ->
    case Str of
        <<_Head:N/binary, $%, Hi, Lo, _Tail/binary>> ->
            Char = try
                       ((char:hex_to_integer(Hi) bsl 4) bor char:hex_to_integer(Lo))
                   catch 
                       _:_ ->
                           $?
                   end,
            urldecode(Str, N + 3, <<Acc/binary, Char>>);
        <<_Head:N/binary, Char, _Tail/binary>> ->
            if
                Char =:= $% ->
                    Acc;
                true ->
                    urldecode(Str, N + 1, <<Acc/binary, Char>>)
            end;
        _ ->
            Acc
    end.


%%--------------------------------------------------------------------
%% @spec xmlencode(bstr()) -> bstr()
%% @doc  Encode a bstr using the XML-encoding scheme.
%%       WARNING: This function assumes that the input is a valid UTF-8 string
%%                and supports non-printable characters in the ASCII range 
%%                00h-1Fh. Bytes that are not part of a valid UTF-8 character 
%%                are not converted at all.
%%--------------------------------------------------------------------
xmlencode(Str) when is_binary(Str) ->
    xmlencode(Str, 0).
%% This part of the function iterates over the bstr() without copying any data
%% and once it finds a character that has to be XML-encoded it switches to
%% the version of the function that accumulates the converted bstr.
xmlencode(Str, Offset) ->
    case Str of
        <<Head:Offset/binary, Char, _Tail/binary>> ->
            case char:must_xmlencode(Char) of
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
            case char:must_xmlencode(Char) of
                true ->
                    Encoded = xmlencode_char(Char),
                    xmlencode(Str, Offset + 1, [$;, Encoded, $& | Acc]);

                false ->
                    xmlencode(Str, Offset + 1, [Char | Acc])
            end;
        _ ->
            list_to_binary(lists:reverse(Acc))
    end.

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
    Hi = char:integer_to_hex((Char band 16#f0) bsr 4),
    Lo = char:integer_to_hex((Char band 16#0f)),
    <<"#x", Hi, Lo>>;
xmlencode_char(Char) ->
    Char.


%%--------------------------------------------------------------------
%% @spec xmldecode(bstr()) -> bstr()
%% @doc  Decode a bstr using the XML-encoding scheme to resolve any character
%%       entity reference present in the string.
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @spec xmldecode_char(bstr(), Offset:integer()) -> {char(), Length::integer()}
%% @doc  Given a string and an offset, this function checks whether there is a
%%       character in that position that has to be decoded using the XML 
%%       encoding scheme for character entity references and returns a tuple 
%%       with the decoded character and the length of the encoded substring in 
%%       the original string.
%%--------------------------------------------------------------------
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
            {((char:hex_to_integer(Hi) bsl 4) bor char:hex_to_integer(Lo)), 6};
        <<_Head:Offset/binary, Char, _Tail/binary>> ->
            {Char, 1};
        _ ->
           eof
    end.


%%--------------------------------------------------------------------
%% @spec hexencode(bstr()) -> bstr()
%% @doc  Encode a bstr converting each character to its hexadecimal 
%%       representation.
%%--------------------------------------------------------------------
hexencode(Str) when is_binary(Str) ->
    hexencode(Str, []).
hexencode(<<Hi:4, Lo:4, Tail/binary>>, Acc) ->
    hexencode(Tail, [char:integer_to_hex(Lo, lower), char:integer_to_hex(Hi, lower) | Acc]);
hexencode(<<>>, Acc) ->
    list_to_binary(lists:reverse(Acc)).


%%--------------------------------------------------------------------
%% @spec hexdecode(bstr()) -> bstr()
%% @doc  Decode a bstr with an hexadecimal representation of a string.
%%--------------------------------------------------------------------
hexdecode(Str) when is_binary(Str) ->
    hexdecode(Str, []).
hexdecode(<<Hi, Lo, Tail/binary>>, Acc) ->
    Char = try
               ((char:hex_to_integer(Hi) bsl 4) bor char:hex_to_integer(Lo))
           catch
               _:_ ->
                   $_
           end,
    hexdecode(Tail, [Char | Acc]);
%% If the number of characters wasn't even we DON'T 
%% raise an exception, just set an invalid character.
hexdecode(<<_Char>>, _Acc) ->
    $_;
hexdecode(<<>>, Acc) ->
    list_to_binary(lists:reverse(Acc)).

%%--------------------------------------------------------------------
%% @spec replace(Data:bstr(), From:bstr(), To:bstr()) -> bstr()
%% @doc  Replace From w/ To in Data
%%--------------------------------------------------------------------
replace(Data, From, To) -> 
    replace(Data, erlang:byte_size(From), Data, From, To, 0). 

replace(OrigData, FromLength, Data, From, To, Count) -> 
    case Data of 
        <<From:FromLength/binary, Right/binary>> -> 
            Replace = replace(Right, From, To), 
            <<Left:Count/binary, _/binary>> = OrigData, 
            <<Left/binary, To/binary, Replace/binary>>; 
        
        <<_:8, Other/binary>> -> 
            replace(OrigData, FromLength, Other, From, To, Count+1); 
        
        <<>> -> 
            OrigData 
    end.

%%--------------------------------------------------------------------
%% @spec copy(Subject:bstr(), N:int()) -> bstr()
%% @doc  Creates a copy of Subject, freeing up Subject for garbage collection
%%			N is currently unused.  This function will get overridden in R14B
%%				by binary:copy/1 and binary:copy/2
%%--------------------------------------------------------------------
copy(Subject) when is_binary(Subject) -> 
    copy(Subject, 1);

copy(Subject) ->
	Subject.

copy(Subject, _N) when is_binary(Subject) ->
	Size = size(Subject),
	_Subject1 = <<Subject/binary, 1>>,
	Subject2 = <<Subject/binary, 2>>,
	<<Subject3:Size/binary, 2>> = Subject2,
	Subject3;

copy(Subject, _N) ->
	Subject.
