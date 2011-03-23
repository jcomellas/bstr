%%% @author Juan Jose Comellas <jcomellas@novamens.com>
%%% @copyright 2008 Aptela, Inc

%%% @doc Tests for the bstr module.

-module(bstr_test).
-author('Juan Jose Comellas <jcomellas@novamens.com>').

-include_lib("eunit/include/eunit.hrl").

-import(bstr, [len/1, equal/2, concat/2, nth/2, index/2, rindex/2,
               member/2, prefix/2, suffix/2,
               is_alpha/1, is_alnum/1, is_lower/1, is_upper/1, is_digit/1,
               is_xdigit/1, is_blank/1, is_space/1, is_atom_as_binary/1, is_numeric/1,
               insert/3, duplicate/2, substr/2, substr/3, left/2, right/2,
               pad/2, pad/3, lpad/2, lpad/3, rpad/2, rpad/3,
               strip/1, strip/2, lstrip/1, lstrip/2, rstrip/1, rstrip/2, chomp/1,
               split/2, join/1, join/2, lower/1, upper/1, bstr/1,
               from_atom/1, to_atom/1, to_existing_atom/1, from_list/1, to_list/1,
               to_boolean/1, from_integer/1, from_integer/2, from_integer/3,
               to_integer/1, to_integer/2, from_float/1, to_float/1, from_number/1, to_number/1,
               integer_to_hex_char/1, integer_to_hex_char/2, hex_char_to_integer/1, 
               get_line/1, urlencode/1, urldecode/1, xmlencode/1, xmldecode/1,
               hexencode/1, hexdecode/1]).


%%%-------------------------------------------------------------------
%%% UNIT TESTS
%%%-------------------------------------------------------------------

%%% Test for the len/1 function
len_1_test() ->
    ?assertMatch(0, len(<<>>)),
    ?assertMatch(3, len(<<"ABC">>)).

%%% Test for the equal/2 function
equal_2_test() ->
    ?assert(equal(<<>>, <<>>)),
    ?assert(equal(<<"ABC123">>, <<"ABC123">>)),
    ?assertNot(equal(<<"ABC123">>, <<"abc123">>)).

%%% Test for the concat/2 function
concat_2_test() ->
    ?assertMatch(<<"123">>, concat(<<>>, <<"123">>)),
    ?assertMatch(<<"123">>, concat(<<"123">>, <<>>)),
    ?assertMatch(<<"ABC123">>, concat(<<"ABC">>, <<"123">>)).

%%% Test for the nth/2 function
nth_2_test() ->
    ?assertMatch($A, nth(<<"ABC">>, 1)),
    ?assertMatch($B, nth(<<"ABC">>, 2)),
    ?assertMatch($C, nth(<<"ABC">>, 3)),
    ?assertError(function_clause, nth(<<"ABC">>, 0)),
    ?assertError(function_clause, nth(<<"ABC">>, 4)),
    ?assertError(function_clause, nth(<<>>, 0)),
    ?assertError(function_clause, nth(<<>>, 1)).

%%% Test for the index/2 function
index_2_test() ->
    ?assertMatch(-1, index(<<>>, $A)),
    ?assertMatch(0, index(<<"ABC">>, $A)),
    ?assertMatch(1, index(<<"ABC">>, $B)),
    ?assertMatch(2, index(<<"ABC">>, $C)),
    ?assertMatch(-1, index(<<"ABC">>, $D)),
    ?assertMatch(1, index(<<"ABCABC">>, $B)).

%%% Test for the rindex/2 function
rindex_2_test() ->
    ?assertMatch(-1, rindex(<<>>, $A)),
    ?assertMatch(0, rindex(<<"ABC">>, $A)),
    ?assertMatch(1, rindex(<<"ABC">>, $B)),
    ?assertMatch(2, rindex(<<"ABC">>, $C)),
    ?assertMatch(-1, rindex(<<"ABC">>, $D)),
    ?assertMatch(4, rindex(<<"ABCABC">>, $B)),
    ?assertMatch(5, rindex(<<"ABCDEF">>, $F)).

%%% Test for the member/2 function
member_2_test() ->
    ?assertNot(member(<<>>, $A)),
    ?assert(member(<<"ABC">>, $A)),
    ?assert(member(<<"ABC">>, $B)),
    ?assert(member(<<"ABC">>, $C)),
    ?assertNot(member(<<"ABC">>, $D)),
    ?assert(member(<<"ABCABC">>, $B)).

%%% Test for the prefix/2 function
prefix_2_test() ->
    ?assert(prefix(<<>>, <<>>)),
    ?assertNot(prefix(<<>>, <<"ABC">>)),
    ?assert(prefix(<<"ABC">>, <<>>)),
    ?assert(prefix(<<"ABC">>, <<"AB">>)),
    ?assert(prefix(<<"ABC">>, <<"ABC">>)),
    ?assertNot(prefix(<<"AB">>, <<"ABC">>)).

%%% Test for the suffix/2 function
suffix_2_test() ->
    ?assert(suffix(<<>>, <<>>)),
    ?assertNot(suffix(<<>>, <<"ABC">>)),
    ?assert(suffix(<<"ABC">>, <<>>)),
    ?assert(suffix(<<"ABC">>, <<"BC">>)),
    ?assert(suffix(<<"ABC">>, <<"ABC">>)),
    ?assertNot(suffix(<<"AB">>, <<"ABC">>)).

%%% Test for the is_alpha/1 function
is_alpha_1_test() ->
    ?assertNot(is_alpha(<<>>)),
    ?assert(is_alpha(<<"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz">>)),
    ?assertNot(is_alpha(<<"ABC123">>)),
    ?assertNot(is_alpha(<<"ABC DEF">>)),
    ?assertNot(is_alpha(<<"A-B-C">>)),
    ?assert(is_alpha($A)),
    ?assert(is_alpha($z)),
    ?assertNot(is_alpha($1)),
    ?assertNot(is_alpha($-)).

%%% Test for the is_alnum/1 function
is_alnum_1_test() ->
    ?assertNot(is_alnum(<<>>)),
    ?assert(is_alnum(<<"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz">>)),
    ?assertNot(is_alnum(<<"ABC 123">>)),
    ?assertNot(is_alnum(<<"A-B-C">>)),
    ?assert(is_alnum($A)),
    ?assert(is_alnum($z)),
    ?assert(is_alnum($1)),
    ?assertNot(is_alnum($-)).

%%% Test for the is_lower/1 function
is_lower_1_test() ->
    ?assertNot(is_lower(<<>>)),
    ?assert(is_lower(<<"abcdefghijklmnopqrstuvwxyz">>)),
    ?assertNot(is_lower(<<"ABCDEFGHIJKLMNOPQRSTUVWXYZ">>)),
    ?assertNot(is_lower(<<"0123456789">>)),
    ?assertNot(is_lower(<<"abc123">>)),
    ?assertNot(is_lower(<<"abc def">>)),
    ?assertNot(is_lower(<<"a-b-c">>)),
    ?assertNot(is_lower($A)),
    ?assert(is_lower($z)),
    ?assertNot(is_lower($1)),
    ?assertNot(is_lower($-)).

%%% Test for the is_upper/1 function
is_upper_1_test() ->
    ?assertNot(is_upper(<<>>)),
    ?assert(is_upper(<<"ABCDEFGHIJKLMNOPQRSTUVWXYZ">>)),
    ?assertNot(is_upper(<<"abcdefghijklmnopqrstuvwxyz">>)),
    ?assertNot(is_upper(<<"0123456789">>)),
    ?assertNot(is_upper(<<"ABC123">>)),
    ?assertNot(is_upper(<<"ABC DEF">>)),
    ?assertNot(is_upper(<<"A-B-C">>)),
    ?assert(is_upper($A)),
    ?assertNot(is_upper($z)),
    ?assertNot(is_upper($1)),
    ?assertNot(is_upper($-)).

%%% Test for the is_digit/1 function
is_digit_1_test() ->
    ?assertNot(is_digit(<<>>)),
    ?assert(is_digit(<<"0123456789">>)),
    ?assertNot(is_digit(<<"ABC123">>)),
    ?assertNot(is_digit(<<"abc123">>)),
    ?assertNot(is_digit(<<"123 456">>)),
    ?assertNot(is_digit(<<"1.23456">>)),
    ?assertNot(is_digit($A)),
    ?assertNot(is_digit($z)),
    ?assert(is_digit($1)),
    ?assertNot(is_digit($-)).

%%% Test for the is_digit/1 function
is_xdigit_1_test() ->
    ?assertNot(is_xdigit(<<>>)),
    ?assert(is_xdigit(<<"0123456789ABCDEFabcdef">>)),
    ?assertNot(is_xdigit(<<"ABCDEFGHIJKLMNOPQRSTUVWXYZ">>)),
    ?assertNot(is_xdigit(<<"abcdefghijklmnopqrstuvwxyz">>)),
    ?assertNot(is_digit(<<"XYZ123">>)),
    ?assertNot(is_digit(<<"xyz123">>)),
    ?assertNot(is_digit(<<"123 ABC">>)),
    ?assertNot(is_digit(<<"A.BCDEF">>)),
    ?assert(is_xdigit($A)),
    ?assertNot(is_xdigit($z)),
    ?assert(is_xdigit($1)),
    ?assertNot(is_xdigit($-)).

%%% Test for the is_blank/1 function
is_blank_1_test() ->
    ?assertNot(is_blank(<<>>)),
    ?assert(is_blank(<<"\s\t">>)),
    ?assertNot(is_blank(<<"\s\n\r\t\f\v">>)),
    ?assertNot(is_blank(<<"0123456789">>)),
    ?assertNot(is_blank(<<"ABCDEFGHIJKLMNOPQRSTUVWXYZ">>)),
    ?assertNot(is_blank(<<"abcdefghijklmnopqrstuvwxyz">>)),
    ?assertNot(is_blank(<<"  \t1\t  ">>)),
    ?assert(is_blank($\s)),
    ?assert(is_blank($\t)),
    ?assertNot(is_blank($A)),
    ?assertNot(is_blank($z)),
    ?assertNot(is_blank($1)),
    ?assertNot(is_blank($-)).

%%% Test for the is_space/1 function
is_space_1_test() ->
    ?assertNot(is_blank(<<>>)),
    ?assert(is_space(<<"\s\n\r\t\f\v">>)),
    ?assertNot(is_space(<<"0123456789">>)),
    ?assertNot(is_space(<<"ABCDEFGHIJKLMNOPQRSTUVWXYZ">>)),
    ?assertNot(is_space(<<"abcdefghijklmnopqrstuvwxyz">>)),
    ?assertNot(is_space(<<"  \t1\t  ">>)),
    ?assert(is_space($\s)),
    ?assert(is_space($\r)),
    ?assert(is_space($\n)),
    ?assert(is_space($\t)),
    ?assertNot(is_space($A)),
    ?assertNot(is_space($z)),
    ?assertNot(is_space($1)),
    ?assertNot(is_space($-)).

%%% Test for the is_atom_as_binary/1 function
is_atom_as_binary_1_test() ->
    ?assertNot(is_atom_as_binary(<<>>)),
    ?assertNot(is_atom_as_binary(<<"\s\n\r\t\f\v">>)),
    ?assertNot(is_atom_as_binary(<<"0123456789">>)),
    ?assertNot(is_atom_as_binary(<<"ABCDEFGHIJKLMNOPQRSTUVWXYZ">>)),
    ?assert(is_atom_as_binary(<<"abcdefghijklmnopqrstuvwxyz">>)),
    ?assertNot(is_atom_as_binary(<<"  \t1\t  ">>)),
    ?assertNot(is_atom_as_binary(<<"@all">>)),
    ?assertNot(is_atom_as_binary(<<"_all">>)),
    ?assertNot(is_atom_as_binary(<<"4all">>)),
    ?assert(is_atom_as_binary(<<"user@host">>)),
    ?assert(is_atom_as_binary(<<"this_is_an_atom_2">>)).

%%% Test for the is_numeric/1 function
is_numeric_1_test() ->
    ?assertNot(is_numeric(<<>>)),
    ?assert(is_numeric(<<"0123456789">>)),
    ?assert(is_numeric(<<"+123456.789">>)),
    ?assert(is_numeric(<<"-123456.789">>)),
    ?assertNot(is_numeric(<<"ABC123">>)),
    ?assertNot(is_numeric(<<"abc123">>)),
    ?assertNot(is_numeric(<<"123 456">>)),
    ?assertNot(is_numeric(<<".23456">>)),
    ?assertNot(is_numeric(<<"123-456">>)),
    ?assertNot(is_numeric(<<"123+456">>)),
    ?assertNot(is_numeric(<<"123456.">>)),
    ?assertNot(is_numeric($A)),
    ?assertNot(is_numeric($z)),
    ?assert(is_numeric($1)),
    ?assertNot(is_numeric($-)).

%%% Test for the insert/3 function
insert_3_test() ->
    ?assertMatch(<<"123">>, insert(<<>>, 1, <<"123">>)),
    ?assertMatch(<<"123ABC">>, insert(<<"ABC">>, 1, <<"123">>)),
    ?assertMatch(<<"A123BC">>, insert(<<"ABC">>, 2, <<"123">>)),
    ?assertMatch(<<"AB123C">>, insert(<<"ABC">>, 3, <<"123">>)),
    ?assertMatch(<<"ABC123">>, insert(<<"ABC">>, 4, <<"123">>)),
    ?assertError(badarg, insert(<<"ABC">>, 0, <<"123">>)),
    ?assertError(badarg, insert(<<"ABC">>, 5, <<"123">>)).

%%% Test for the duplicate/2 function
duplicate_2_test() ->
    ?assertMatch(<<>>, duplicate(<<>>, 3)),
    ?assertMatch(<<>>, duplicate(<<"*">>, 0)),
    ?assertMatch(<<"****">>, duplicate(<<"*">>, 4)),
    ?assertMatch(<<"*!*!*!">>, duplicate(<<"*!">>, 3)).

%%% Test for the substr/2 function
substr_2_test() ->
    ?assertMatch(<<>>, substr(<<>>, 2)),
    ?assertMatch(<<"ABC">>, substr(<<"ABC">>, 1)),
    ?assertMatch(<<"BC">>, substr(<<"ABC">>, 2)),
    ?assertMatch(<<"C">>, substr(<<"ABC">>, 3)),
    ?assertMatch(<<>>, substr(<<"ABC">>, 4)).

%%% Test for the substr/3 function
substr_3_test() ->
    ?assertMatch(<<>>, substr(<<>>, 1, 2)),
    ?assertMatch(<<"A">>, substr(<<"ABC">>, 1, 1)),
    ?assertMatch(<<"AB">>, substr(<<"ABC">>, 1, 2)),
    ?assertMatch(<<"ABC">>, substr(<<"ABC">>, 1, 3)),
    ?assertMatch(<<"ABC">>, substr(<<"ABC">>, 1, 4)),
    ?assertMatch(<<"B">>, substr(<<"ABC">>, 2, 1)),
    ?assertMatch(<<"BC">>, substr(<<"ABC">>, 2, 2)),
    ?assertMatch(<<"BC">>, substr(<<"ABC">>, 2, 3)),
    ?assertMatch(<<"BC">>, substr(<<"ABC">>, 2, 4)),
    ?assertMatch(<<"C">>, substr(<<"ABC">>, 3, 1)),
    ?assertMatch(<<"C">>, substr(<<"ABC">>, 3, 2)),
    ?assertMatch(<<>>, substr(<<"ABC">>, 4, 1)).

%%% Test for the left/2 function
left_2_test() ->
    ?assertMatch(<<>>, left(<<>>, 0)),
    ?assertMatch(<<"">>, left(<<>>, 1)),
    ?assertMatch(<<"AB">>, left(<<"ABC">>, 2)),
    ?assertMatch(<<"ABC">>, left(<<"ABC">>, 3)),
    ?assertMatch(<<"ABC">>, left(<<"ABC">>, 4)).

%%% Test for the left/2 function
right_2_test() ->
    ?assertMatch(<<>>, right(<<>>, 0)),
    ?assertMatch(<<"">>, right(<<>>, 1)),
    ?assertMatch(<<"BC">>, right(<<"ABC">>, 2)),
    ?assertMatch(<<"ABC">>, right(<<"ABC">>, 3)),
    ?assertMatch(<<"ABC">>, right(<<"ABC">>, 4)).

%%% Test for the pad/2 function
pad_2_test() ->
    ?assertMatch(<<>>, pad(<<>>, 0)),
    ?assertMatch(<<" ">>, pad(<<>>, 1)),
    ?assertMatch(<<"ABC">>, pad(<<"ABC">>, 2)),
    ?assertMatch(<<"ABC">>, pad(<<"ABC">>, 3)),
    ?assertMatch(<<" ABC ">>, pad(<<"ABC">>, 5)),
    ?assertMatch(<<" ABC  ">>, pad(<<"ABC">>, 6)).

%%% Test for the pad/3 function
pad_3_test() ->
    ?assertMatch(<<>>, pad(<<>>, 0, $*)),
    ?assertMatch(<<"*">>, pad(<<>>, 1, $*)),
    ?assertMatch(<<"ABC">>, pad(<<"ABC">>, 2, $*)),
    ?assertMatch(<<"ABC">>, pad(<<"ABC">>, 3, $*)),
    ?assertMatch(<<"*ABC*">>, pad(<<"ABC">>, 5, $*)),
    ?assertMatch(<<"*ABC**">>, pad(<<"ABC">>, 6, $*)).

%%% Test for the lpad/2 function
lpad_2_test() ->
    ?assertMatch(<<>>, lpad(<<>>, 0)),
    ?assertMatch(<<" ">>, lpad(<<>>, 1)),
    ?assertMatch(<<"ABC">>, lpad(<<"ABC">>, 2)),
    ?assertMatch(<<"ABC">>, lpad(<<"ABC">>, 3)),
    ?assertMatch(<<"  ABC">>, lpad(<<"ABC">>, 5)).

%%% Test for the lpad/3 function
lpad_3_test() ->
    ?assertMatch(<<>>, lpad(<<>>, 0, $*)),
    ?assertMatch(<<"*">>, lpad(<<>>, 1, $*)),
    ?assertMatch(<<"ABC">>, lpad(<<"ABC">>, 2, $*)),
    ?assertMatch(<<"ABC">>, lpad(<<"ABC">>, 3, $*)),
    ?assertMatch(<<"**ABC">>, lpad(<<"ABC">>, 5, $*)).

%%% Test for the rpad/2 function
rpad_2_test() ->
    ?assertMatch(<<>>, rpad(<<>>, 0)),
    ?assertMatch(<<" ">>, rpad(<<>>, 1)),
    ?assertMatch(<<"ABC">>, rpad(<<"ABC">>, 2)),
    ?assertMatch(<<"ABC">>, rpad(<<"ABC">>, 3)),
    ?assertMatch(<<"ABC  ">>, rpad(<<"ABC">>, 5)).

%%% Test for the rpad/3 function
rpad_3_test() ->
    ?assertMatch(<<>>, rpad(<<>>, 0, $*)),
    ?assertMatch(<<"*">>, rpad(<<>>, 1, $*)),
    ?assertMatch(<<"ABC">>, rpad(<<"ABC">>, 2, $*)),
    ?assertMatch(<<"ABC">>, rpad(<<"ABC">>, 3, $*)),
    ?assertMatch(<<"ABC**">>, rpad(<<"ABC">>, 5, $*)).


%%% Test for the strip/2 function
strip_2_test() ->
    ?assertMatch(<<>>, strip(<<>>)),
    ?assertMatch(<<>>, strip(<<" ">>)),
    ?assertMatch(<<"ABC">>, strip(<<"ABC  ">>)),
    ?assertMatch(<<"ABC">>, strip(<<"ABC  \t\n">>)),
    ?assertMatch(<<"ABC">>, strip(<<"  ABC">>)),
    ?assertMatch(<<"ABC">>, strip(<<"\r\v\f  ABC">>)),
    ?assertMatch(<<"ABC">>, strip(<<"  ABC  ">>)),
    ?assertMatch(<<"ABC">>, strip(<<"\r  ABC  \n">>)).

%%% Test for the strip/3 function
strip_3_test() ->
    ?assertMatch(<<>>, strip(<<>>, $*)),
    ?assertMatch(<<>>, strip(<<>>, <<"\r\n\s">>)),
    ?assertMatch(<<>>, strip(<<"*">>, $*)),
    ?assertMatch(<<>>, strip(<<"*">>, <<"*">>)),
    ?assertMatch(<<"ABC">>, strip(<<"ABC**">>, $*)),
    ?assertMatch(<<"ABC">>, strip(<<"ABC^^*&*">>, <<"*&^">>)),
    ?assertMatch(<<"ABC">>, strip(<<"**ABC">>, $*)),
    ?assertMatch(<<"ABC">>, strip(<<"@@@@!!%%%%ABC">>, <<"%@!">>)),
    ?assertMatch(<<"ABC">>, strip(<<"**ABC**">>, $*)),
    ?assertMatch(<<"ABC">>, strip(<<"*#*%ABC/**">>, <<"#%/*">>)).


%%% Test for the lstrip/2 function
lstrip_2_test() ->
    ?assertMatch(<<>>, lstrip(<<>>)),
    ?assertMatch(<<>>, lstrip(<<" ">>)),
    ?assertMatch(<<"ABC  ">>, lstrip(<<"ABC  ">>)),
    ?assertMatch(<<"ABC  \t\n">>, lstrip(<<"ABC  \t\n">>)),
    ?assertMatch(<<"ABC">>, lstrip(<<"  ABC">>)),
    ?assertMatch(<<"ABC">>, lstrip(<<"\r\v\f  ABC">>)),
    ?assertMatch(<<"ABC  ">>, lstrip(<<"  ABC  ">>)),
    ?assertMatch(<<"ABC  \n">>, lstrip(<<"\r  ABC  \n">>)).

%%% Test for the lstrip/3 function
lstrip_3_test() ->
    ?assertMatch(<<>>, lstrip(<<>>, $*)),
    ?assertMatch(<<>>, lstrip(<<>>, <<"\r\n\s">>)),
    ?assertMatch(<<>>, lstrip(<<"*">>, $*)),
    ?assertMatch(<<>>, lstrip(<<"*">>, <<"*">>)),
    ?assertMatch(<<"ABC**">>, lstrip(<<"ABC**">>, $*)),
    ?assertMatch(<<"ABC^^*&*">>, lstrip(<<"ABC^^*&*">>, <<"*&^">>)),
    ?assertMatch(<<"ABC">>, lstrip(<<"**ABC">>, $*)),
    ?assertMatch(<<"ABC">>, lstrip(<<"@@@@!!%%%%ABC">>, <<"%@!">>)),
    ?assertMatch(<<"ABC**">>, lstrip(<<"**ABC**">>, $*)),
    ?assertMatch(<<"ABC/**">>, lstrip(<<"*#*%ABC/**">>, <<"#%/*">>)).


%%% Test for the rstrip/2 function
rstrip_2_test() ->
    ?assertMatch(<<>>, rstrip(<<>>)),
    ?assertMatch(<<>>, rstrip(<<>>, <<"\r\n\s">>)),
    ?assertMatch(<<>>, rstrip(<<" ">>)),
    ?assertMatch(<<"ABC">>, rstrip(<<"ABC  ">>)),
    ?assertMatch(<<"ABC">>, rstrip(<<"ABC  \t\n">>)),
    ?assertMatch(<<"  ABC">>, rstrip(<<"  ABC">>)),
    ?assertMatch(<<"\r\v\f  ABC">>, rstrip(<<"\r\v\f  ABC">>)),
    ?assertMatch(<<"  ABC">>, rstrip(<<"  ABC  ">>)),
    ?assertMatch(<<"\r  ABC">>, rstrip(<<"\r  ABC  \n">>)).

%%% Test for the rstrip/3 function
rstrip_3_test() ->
    ?assertMatch(<<>>, rstrip(<<>>, $*)),
    ?assertMatch(<<>>, rstrip(<<>>, <<"\r\n\s">>)),
    ?assertMatch(<<>>, rstrip(<<"*">>, $*)),
    ?assertMatch(<<>>, rstrip(<<"*">>, <<"*">>)),
    ?assertMatch(<<"ABC">>, rstrip(<<"ABC**">>, $*)),
    ?assertMatch(<<"ABC">>, rstrip(<<"ABC^^*&*">>, <<"*&^">>)),
    ?assertMatch(<<"**ABC">>, rstrip(<<"**ABC">>, $*)),
    ?assertMatch(<<"@@@@!!%%%%ABC">>, rstrip(<<"@@@@!!%%%%ABC">>, <<"%@!">>)),
    ?assertMatch(<<"**ABC">>, rstrip(<<"**ABC**">>, $*)),
    ?assertMatch(<<"*#*%ABC">>, rstrip(<<"*#*%ABC/**">>, <<"#%/*">>)).

%%% Test for the chomp/1 function
chomp_1_test() ->
    ?assertMatch(<<>>, chomp(<<>>)),
    ?assertMatch(<<>>, chomp(<<"\n">>)),
    ?assertMatch(<<"ABC">>, chomp(<<"ABC">>)),
    ?assertMatch(<<"ABC">>, chomp(<<"ABC\r\n">>)),
    ?assertMatch(<<"\r\nABC">>, chomp(<<"\r\nABC">>)),
    ?assertMatch(<<"\n\nABC">>, chomp(<<"\n\nABC\n\n">>)).

%%% Test for the split/2 function
split_2_test() ->
    %% Tokenize using a single separator
    ?assertMatch([], split(<<>>, $.)),
    ?assertMatch([<<"ABC">>], split(<<"ABC">>, $.)),
    ?assertMatch([<<>>, <<"ABC">>], split(<<".ABC">>, $.)),
    ?assertMatch([<<"ABC">>, <<>>], split(<<"ABC-">>, $-)),
    ?assertMatch([<<"ABC">>, <<"DEF">>], split(<<"ABC_DEF">>, $_)),
    ?assertMatch([<<"ABC">>, <<"DEF">>, <<"GHI">>], split(<<"ABC&DEF&GHI">>, $&)),
    %% Tokenize using a group of separators
    ?assertMatch([<<"ABC">>], split(<<"ABC">>, <<".-_,">>)),
    ?assertMatch([<<>>, <<"ABC">>], split(<<".ABC">>, <<".-_,">>)),
    ?assertMatch([<<"ABC">>, <<>>], split(<<"ABC-">>, <<".-_,">>)),
    ?assertMatch([<<"ABC">>, <<"DEF">>], split(<<"ABC_DEF">>, <<"_">>)),
    ?assertMatch([<<"ABC">>, <<"DEF">>, <<"GHI">>], split(<<"ABC+DEF GHI">>, <<"+ ">>)).

%%% Test for the join/1 function
join_1_test() ->
    %% Empty elements
    ?assertMatch(<<>>, join([<<>>, <<>>])),
    ?assertMatch(<<"ABC">>, join([<<"ABC">>, <<>>, <<>>])),
    ?assertMatch(<<"123">>, join([<<>>, <<"123">>, <<>>])),
    ?assertMatch(<<"abc">>, join([<<>>, <<>>, <<"abc">>])),
    %% Single element
    ?assertMatch(<<"ABC">>, join([<<"ABC">>])),
    %% Mutliple elements
    ?assertMatch(<<"ABC123abc">>, join([<<"ABC">>, <<"123">>, <<"abc">>])),
    %% Multiple elements with different data types
    ?assertMatch(<<"ABC123abc">>, join([<<"ABC">>, "123", abc])),
    ?assertMatch(<<"ABC123abc">>, join(['ABC', "123", <<"abc">>])),
    ?assertMatch(<<"ABC123abc">>, join(["ABC", <<"123">>, abc])).

%%% Test for the join/2 function
join_2_test() ->
    %% Empty elements
    ?assertMatch(<<>>, join([], $,)),
    ?assertMatch(<<"||">>, join([<<>>, <<>>, <<>>], $|)),
    ?assertMatch(<<"ABC||">>, join([<<"ABC">>, <<>>, <<>>], $|)),
    ?assertMatch(<<"##123##">>, join([<<>>, <<"123">>, <<>>], <<"##">>)),
    ?assertMatch(<<"??abc">>, join([<<>>, <<>>, <<"abc">>], $?)),
    %% Single element
    ?assertMatch(<<"ABC">>, join([<<"ABC">>], $|)),
    %% Mutliple elements with character separator
    ?assertMatch(<<"ABC,123,abc">>, join([<<"ABC">>, <<"123">>, <<"abc">>], $,)),
    %% Mutliple elements with string separator
    ?assertMatch(<<"ABC**123**abc">>, join([<<"ABC">>, <<"123">>, <<"abc">>], <<"**">>)),
    %% Multiple elements with different data types
    ?assertMatch(<<"ABC 123 abc">>, join([<<"ABC">>, "123", abc], $\s)),
    ?assertMatch(<<"ABC 123 abc">>, join(['ABC', "123", <<"abc">>], $\s)),
    ?assertMatch(<<"ABC 123 abc">>, join(["ABC", <<"123">>, abc], $\s)),
    ?assertMatch(<<"ABC 123 abc">>, join([<<"ABC">>, "123", abc], <<" ">>)),
    ?assertMatch(<<"ABC 123 abc">>, join(['ABC', "123", <<"abc">>], <<" ">>)),
    ?assertMatch(<<"ABC 123 abc">>, join(["ABC", <<"123">>, abc], <<" ">>)).

%%% Test for the lower/1 function
lower_1_test() ->
    %% Empty string
    ?assertMatch(<<>>, lower(<<>>)),
    %% Numbers and symbols
    ?assertMatch(<<"-=+*12345">>, lower(<<"-=+*12345">>)),
    ?assertMatch($-, lower($-)),
    ?assertMatch($1, lower($1)),
    %% Upper case characters
    ?assertMatch(<<"abcde">>, lower(<<"ABCDE">>)),
    ?assertMatch($b, lower($B)),
    %% Lower case characters
    ?assertMatch(<<"abcde">>, lower(<<"abcde">>)),
    ?assertMatch($c, lower($c)),
    %% Combined characters
    ?assertMatch(<<"abcde">>, lower(<<"AbCdE">>)).

%%% Test for the upper/1 function
upper_1_test() ->
    %% Empty string
    ?assertMatch(<<>>, upper(<<>>)),
    %% Numbers and symbols
    ?assertMatch(<<"-=+*12345">>, upper(<<"-=+*12345">>)),
    ?assertMatch($-, upper($-)),
    ?assertMatch($1, upper($1)),
    %% Upper case characters
    ?assertMatch(<<"ABCDE">>, upper(<<"ABCDE">>)),
    ?assertMatch($B, upper($B)),
    %% Lower case characters
    ?assertMatch(<<"ABCDE">>, upper(<<"abcde">>)),
    ?assertMatch($C, upper($c)),
    %% Combined characters
    ?assertMatch(<<"ABCDE">>, upper(<<"AbCdE">>)).

%%% Test for the from_atom/1 function
from_atom_1_test() ->
    %% Empty atom
    ?assertMatch(<<>>, from_atom('')),
    %% Normal characters
    ?assertMatch(<<"abc@localhost">>, from_atom(abc@localhost)),
    %% Symbols
    ?assertMatch(<<"ABC+-=[]/123">>, from_atom('ABC+-=[]/123')).

%%% Test for the to_atom/1 function
to_atom_1_test() ->
    %% Empty string
    ?assertMatch('', to_atom(<<>>)),
    %% Normal string
    ?assertMatch(abc, to_atom(<<"abc">>)),
    %% Symbols
    ?assertMatch('ABC+-=[]/123', to_atom(<<"ABC+-=[]/123">>)).

%%% Test for the to_existing_atom/1 function
to_existing_atom_1_test() ->
    %% Empty string
    ?assertMatch('', to_existing_atom(<<>>)),
    %% We cannot test the failure of this function because once an atom is
    %% created it cannot be removed from the virtual machine's atom table.
    %% Str2 = <<"***THIS_ATOM_MUST_NOT_BE_ALREADY_DEFINED***">>,
    %% ?assertError(badarg, to_existing_atom(Str2)),
    %% Normal string
    ?assertMatch(abc, to_existing_atom(<<"abc">>)).

%%% Test for the from_list/1 function
from_list_1_test() ->
    %% Empty string
    ?assertMatch(<<>>, from_list([])),
    %% Normal string
    ?assertMatch(<<"ABC123">>, from_list("ABC123")).

%%% Test for the to_list/1 function
to_list_1_test() ->
    %% Empty string
    ?assertMatch([], to_list(<<>>)),
    %% Normal string
    ?assertMatch("ABC123", to_list(<<"ABC123">>)).

%%% Test for the from_integer/1 function
from_integer_1_test() ->
    %% Positive integer
    ?assertMatch(<<"12345">>, from_integer(12345)),
    %% Negative integer
    ?assertMatch(<<"-12345">>, from_integer(-12345)),
    %% Floating point number
    ?assertError(badarg, from_integer(12345.67)).

%%% Test for the to_integer/1 function
to_integer_1_test() ->
    %% Positive integer
    ?assertMatch(12345, to_integer(<<"12345">>)),
    %% Positive integer (explicit)
    ?assertMatch(12345, to_integer(<<"+12345">>)),
    %% Negative integer
    ?assertMatch(-12345, to_integer(<<"-12345">>)),
    %% Empty string
    ?assertError(badarg, to_integer(<<>>)),
    %% Floating point number
    ?assertError(badarg, to_integer(<<"12345.67">>)),
    %% Invalid characters
    ?assertError(badarg, to_integer(<<"ABC">>)).

%%% Test for the from_integer/2 function
from_integer_2_test() ->
    %% Base-2 integer
    ?assertMatch(<<"0">>, from_integer(0, 2)),
    ?assertMatch(<<"1">>, from_integer(1, 2)),
    ?assertMatch(<<"110110">>, from_integer(54, 2)),
    ?assertMatch(<<"-1000000">>, from_integer(-64, 2)),
    %% Base-16 integer
    ?assertMatch(<<"A">>, from_integer(10, 16)),
    ?assertMatch(<<"D4BE">>, from_integer(54462, 16)),
    ?assertMatch(<<"-D4BE">>, from_integer(-54462, 16)),
    %% Floating point number
    ?assertError(badarg, from_integer(12345.67, 16)).

%%% Test for the to_integer/2 function
to_integer_2_test() ->
    %% Base-2 integer
    ?assertMatch(1, to_integer(<<"1">>, 2)),
    ?assertMatch(54, to_integer(<<"110110">>, 2)),
    ?assertMatch(-54, to_integer(<<"-110110">>, 2)),
    %% Base-16 integer
    ?assertMatch(10, to_integer(<<"A">>, 16)),
    ?assertMatch(54462, to_integer(<<"D4BE">>, 16)),
    ?assertMatch(54462, to_integer(<<"d4be">>, 16)),
    %% Empty string
    ?assertError(badarg, to_integer(<<>>, 2)),
    %% Invalid characters
    ?assertError(badarg, to_integer(<<"-ABC">>)).

%%% Test for the from_integer/3 function
from_integer_3_test() ->
    %% Base-16 integer (upper case)
    ?assertMatch(<<"A">>, from_integer(10, 16, upper)),
    ?assertMatch(<<"D4BE">>, from_integer(54462, 16, upper)),
    ?assertMatch(<<"-D4BE">>, from_integer(-54462, 16, upper)),
    %% Base-16 integer (lower case)
    ?assertMatch(<<"a">>, from_integer(10, 16, lower)),
    ?assertMatch(<<"d4be">>, from_integer(54462, 16, lower)),
    ?assertMatch(<<"-d4be">>, from_integer(-54462, 16, lower)),
    %% Floating point number
    ?assertError(badarg, from_integer(12345.67, 16)).

%%% Test for the from_float/1 function
from_float_1_test() ->
    %% Positive integer
    ?assertMatch(<<"12345">>, from_float(12345)),
    %% Negative integer
    ?assertMatch(<<"-12345">>, from_float(-12345)),
    %% Floating point number
    ?assertMatch(<<"12345.67">>, from_float(12345.67)),
    %% Non-numeric string.
    ?assertError(badarg, from_float(<<"abcdef">>)).

%%% Test for the to_float/1 function
to_float_1_test() ->
    %% Positive floating point number
    ?assertMatch(12345.0, to_float(<<"12345.0">>)),
    %% Negative floating point number
    ?assertMatch(-12345.0, to_float(<<"-12345.0">>)),
    %% Empty string
    ?assertError(badarg, to_float(<<>>)),
    %% Integer
    ?assertError(badarg, to_float(<<"12345">>)),
    %% Invalid characters
    ?assertError(badarg, to_float(<<"ABC">>)).

%%% Test for the from_number/1 function
from_number_1_test() ->
    %% Positive integer
    ?assertMatch(<<"12345">>, from_number(12345)),
    %% Negative integer
    ?assertMatch(<<"-12345">>, from_number(-12345)),
    %% Floating point number
    ?assertMatch(<<"12345.67">>, from_number(12345.67)),
    %% Non-numeric string.
    ?assertError(badarg, from_number(<<"abcdef">>)).

%%% Test for the to_number/1 function
to_number_1_test() ->
    %% Positive integer
    ?assertMatch(12345, to_number(<<"12345">>)),
    %% Negative integer
    ?assertMatch(-12345, to_number(<<"-12345">>)),
    %% Positive floating point number
    ?assertMatch(12345.0, to_number(<<"12345.0">>)),
    %% Negative floating point number
    ?assertMatch(-12345.0, to_number(<<"-12345.0">>)),
    %% Empty string
    ?assertError(badarg, to_number(<<>>)),
    %% Invalid characters
    ?assertError(badarg, to_number(<<"ABC">>)).

%%% Test for the get_line/1 function
get_line_1_test() ->
    Str1 = <<>>,
    ?assertMatch({<<>>, <<>>}, get_line(Str1)),
    Str2 = <<"ABCDEF">>,
    ?assertMatch({<<"ABCDEF">>, <<>>}, get_line(Str2)),
    Str3 = <<"ABCDEF\n">>,
    ?assertMatch({<<"ABCDEF">>, <<>>}, get_line(Str3)),
    Str4 = <<"ABCDEF\r\n">>,
    ?assertMatch({<<"ABCDEF">>, <<>>}, get_line(Str4)),
    Str5 = <<"ABCDEF\nGHIJKL">>,
    ?assertMatch({<<"ABCDEF">>, <<"GHIJKL">>}, get_line(Str5)),
    Str6 = <<"ABCDEF\r\nGHIJKL">>,
    ?assertMatch({<<"ABCDEF">>, <<"GHIJKL">>}, get_line(Str6)),
    Str7 = <<"ABCDEF\nGHIJKL\n">>,
    ?assertMatch({<<"ABCDEF">>, <<"GHIJKL\n">>}, get_line(Str7)),
    Str8 = <<"ABCDEF\r\nGHIJKL\r\n">>,
    ?assertMatch({<<"ABCDEF">>, <<"GHIJKL\r\n">>}, get_line(Str8)),
    Str9 = <<"ABCDEF\nGHIJKL\nMNOPQR">>,
    ?assertMatch({<<"ABCDEF">>, <<"GHIJKL\nMNOPQR">>}, get_line(Str9)),
    Str10 = <<"ABCDEF\r\nGHIJKL\r\nMNOPQR">>,
    ?assertMatch({<<"ABCDEF">>, <<"GHIJKL\r\nMNOPQR">>}, get_line(Str10)),
    Str11 = <<"ABCDEF\nGHIJKL\r\nMNOPQR">>,
    ?assertMatch({<<"ABCDEF">>, <<"GHIJKL\r\nMNOPQR">>}, get_line(Str11)),
    Str12 = <<"ABCDEF\r\nGHIJKL\nMNOPQR">>,
    ?assertMatch({<<"ABCDEF">>, <<"GHIJKL\nMNOPQR">>}, get_line(Str12)).
    
%%% Test for the urlencode/1 function
urlencode_1_test() ->
    %% Unreserved characters (lower case letters)
    Str1 = <<"abcdefghijklmnopqrstuvwxyz">>,
    ?assertEqual(Str1, urlencode(Str1)),
    %% Unreserved characters (upper case letters)
    Str2 = <<"ABCDEFGHIJKLMNOPQRSTUVWXYZ">>,
    ?assertEqual(Str2, urlencode(Str2)),
    %% Unreserved characters (numbers and symbols)
    Str3 = <<"01234567890-_.~">>,
    ?assertEqual(Str3, urlencode(Str3)),
    %% Reserved characters
    Str4 = <<"!*'();:@&=+$,/?%#[]">>,
    Enc4 = <<"%21%2A%27%28%29%3B%3A%40%26%3D%2B%24%2C%2F%3F%25%23%5B%5D">>,
    ?assertEqual(Enc4, urlencode(Str4)),
    %% Control characters
    Str5 = <<16#00, 16#01, 16#02, 16#03, 16#04, 16#05, 16#06, 16#07,
             16#08, 16#09, 16#0A, 16#0B, 16#0C, 16#0D, 16#0E, 16#0F,
             16#10, 16#11, 16#12, 16#13, 16#14, 16#15, 16#16, 16#17,
             16#18, 16#19, 16#1A, 16#1B, 16#1C, 16#1D, 16#1E, 16#1F>>,
    Enc5 = <<"%00%01%02%03%04%05%06%07"
             "%08%09%0A%0B%0C%0D%0E%0F"
             "%10%11%12%13%14%15%16%17"
             "%18%19%1A%1B%1C%1D%1E%1F">>,
    ?assertEqual(Enc5, urlencode(Str5)),
    %% Text with embedded spaces
    Str6 = <<"This is a test for urlencode">>,
    Enc6 = <<"This%20is%20a%20test%20for%20urlencode">>,
    ?assertEqual(Enc6, urlencode(Str6)).

%%% Test for the urldecode/1 function
urldecode_1_test() ->
    %% Unreserved characters (lower case letters)
    Str1 = <<"abcdefghijklmnopqrstuvwxyz">>,
    ?assertEqual(Str1, urldecode(Str1)),
    %% Unreserved characters (upper case letters)
    Str2 = <<"ABCDEFGHIJKLMNOPQRSTUVWXYZ">>,
    ?assertEqual(Str2, urldecode(Str2)),
    %% Unreserved characters (numbers and symbols)
    Str3 = <<"01234567890-_.~">>,
    ?assertEqual(Str3, urldecode(Str3)),
    %% Reserved characters
    Enc4 = <<"%21%2A%27%28%29%3B%3A%40%26%3D%2B%24%2C%2F%3F%25%23%5B%5D">>,
    ?assertMatch(<<"!*'();:@&=+$,/?%#[]">>, urldecode(Enc4)),
    %% Control characters
    Str5 = <<16#00, 16#01, 16#02, 16#03, 16#04, 16#05, 16#06, 16#07,
             16#08, 16#09, 16#0A, 16#0B, 16#0C, 16#0D, 16#0E, 16#0F,
             16#10, 16#11, 16#12, 16#13, 16#14, 16#15, 16#16, 16#17,
             16#18, 16#19, 16#1A, 16#1B, 16#1C, 16#1D, 16#1E, 16#1F>>,
    Enc5 = <<"%00%01%02%03%04%05%06%07"
             "%08%09%0A%0B%0C%0D%0E%0F"
             "%10%11%12%13%14%15%16%17"
             "%18%19%1A%1B%1C%1D%1E%1F">>,
    ?assertEqual(Str5, urldecode(Enc5)),
    %% Text with embedded spaces
    Enc6 = <<"This%20is%20a%20test%20for%20urlencode">>,
    ?assertMatch(<<"This is a test for urlencode">>, urldecode(Enc6)),
    %% Invalid length of URL-encoded character
    ?assertError(badarg, urldecode(<<"ABC%Z">>)),
    %% Invalid URL-encoded character
    ?assertError(badarg, urldecode(<<"ABC%ZZ">>)).

%%% Test for the xmlencode/1 function
xmlencode_1_test() ->
    %% Empty string
    ?assert(xmlencode(<<>>) =:= <<>>),
    %% Characters that are not encoded: lower case letters
    ?assertMatch(<<"abcdefghijklmnopqrstuvwxyz">>, 
                 xmlencode(<<"abcdefghijklmnopqrstuvwxyz">>)),
    %% Characters that are not encoded: upper case letters
    ?assertMatch(<<"ABCDEFGHIJKLMNOPQRSTUVWXYZ">>, 
                 xmlencode(<<"ABCDEFGHIJKLMNOPQRSTUVWXYZ">>)),
    %% Characters that are not encoded: numbers and symbols
    ?assertMatch(<<"01234567890-_.~">>, xmlencode(<<"01234567890-_.~">>)),
    %% Characters that are encoded
    ?assertMatch(<<"&amp;&lt;&gt;&apos;&quot;">>, xmlencode(<<"&<>'\"">>)),
    %% Non-printable characters
    ?assertMatch(<<"&#x00;&#x01;&#x02;&#x03;&#x04;&#x05;&#x06;&#x07;"
                  "&#x08;&#x09;&#x0A;&#x0B;&#x0C;&#x0D;&#x0E;&#x0F;"
                  "&#x10;&#x11;&#x12;&#x13;&#x14;&#x15;&#x16;&#x17;"
                  "&#x18;&#x19;&#x1A;&#x1B;&#x1C;&#x1D;&#x1E;&#x1F;">>,
                 xmlencode(<<0,  1,  2,  3,  4,  5,  6,  7,
                             8,  9, 10, 11, 12, 13, 14, 15, 
                            16, 17, 18, 19, 20, 21, 22, 23, 
                            24, 25, 26, 27, 28, 29, 30, 31>>)),
    %% Text with embedded spaces
    ?assertMatch(<<"&lt;element attr=&quot;1234&quot;/&gt;">>, 
                 xmlencode(<<"<element attr=\"1234\"/>">>)).

%%% Test for the xmldecode/1 function
xmldecode_1_test() ->
    %% Empty string
    ?assert(xmldecode(<<>>) =:= <<>>),
    %% Characters that are not encoded: lower case letters
    ?assertMatch(<<"abcdefghijklmnopqrstuvwxyz">>, 
                 xmldecode(<<"abcdefghijklmnopqrstuvwxyz">>)),
    %% Characters that are not encoded: upper case letters
    ?assertMatch(<<"ABCDEFGHIJKLMNOPQRSTUVWXYZ">>, 
                 xmldecode(<<"ABCDEFGHIJKLMNOPQRSTUVWXYZ">>)),
    %% Characters that are not encoded: numbers and symbols
    ?assertMatch(<<"01234567890-_.~">>, xmldecode(<<"01234567890-_.~">>)),
    %% Characters that are encoded
    ?assertMatch(<<"&<>'\"">>, xmldecode(<<"&amp;&lt;&gt;&apos;&quot;">>)),
    %% Non-printable characters
    ?assertMatch(<<0,  1,  2,  3,  4,  5,  6,  7,
                   8,  9, 10, 11, 12, 13, 14, 15, 
                  16, 17, 18, 19, 20, 21, 22, 23, 
                  24, 25, 26, 27, 28, 29, 30, 31>>,
                 xmldecode(<<"&#x00;&#x01;&#x02;&#x03;&#x04;&#x05;&#x06;&#x07;"
                            "&#x08;&#x09;&#x0A;&#x0B;&#x0C;&#x0D;&#x0E;&#x0F;"
                            "&#x10;&#x11;&#x12;&#x13;&#x14;&#x15;&#x16;&#x17;"
                            "&#x18;&#x19;&#x1A;&#x1B;&#x1C;&#x1D;&#x1E;&#x1F;">>)),
    %% Text with embedded spaces
    ?assertMatch(<<"<element attr=\"1234\"/>">>, 
                 xmldecode(<<"&lt;element attr=&quot;1234&quot;/&gt;">>)),
    %% Invalid encoding
    ?assertMatch(<<"**&abc;**">>, xmldecode(<<"**&abc;**">>)).


%%% Test for the hexencode/1 function
hexencode_1_test() ->
    %% Empty string
    ?assert(hexencode(<<>>) =:= <<>>),
    %% Unprintable (control) characters
    ?assertMatch(<<"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20">>,
                 hexencode(<< 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10,
                             11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21,
                             22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32>>)),
    %% Numbers
    ?assertMatch(<<"30313233343536373839">>, hexencode(<<"0123456789">>)),
    %% Alphabetic (upper case) letters
    ?assertMatch(<<"4142434445464748494a4b4c4d4e4f505152535455565758595a">>,
                 hexencode(<<"ABCDEFGHIJKLMNOPQRSTUVWXYZ">>)),
    %% Alphabetic (lower case) letters
    ?assertMatch(<<"6162636465666768696a6b6c6d6e6f707172737475767778797a">>,
                 hexencode(<<"abcdefghijklmnopqrstuvwxyz">>)).

%%% Test for the hexdecode/1 function
hexdecode_1_test() ->
    %% Empty string
    ?assert(hexdecode(<<>>) =:= <<>>),
    %% Unprintable (control) characters
    ?assertMatch(<< 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10,
                   11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21,
                   22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32>>,
                 hexdecode(<<"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20">>)),
    %% Numbers
    ?assertMatch(<<"0123456789">>, hexdecode(<<"30313233343536373839">>)),
    %% Alphabetic (upper case) letters
    ?assertMatch(<<"ABCDEFGHIJKLMNOPQRSTUVWXYZ">>,
                 hexdecode(<<"4142434445464748494a4b4c4d4e4f505152535455565758595a">>)),
    %% Alphabetic (lower case) letters
    ?assertMatch(<<"abcdefghijklmnopqrstuvwxyz">>,
                 hexdecode(<<"6162636465666768696a6b6c6d6e6f707172737475767778797a">>)).

