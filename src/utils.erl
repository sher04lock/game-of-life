-module(utils).

%% API
-export([remove_duplicates/1]).


remove_duplicates(List) when is_list(List) -> sets:to_list(sets:from_list(List));
remove_duplicates(X) -> X.
