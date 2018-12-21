%%%-------------------------------------------------------------------
%%% @author denys.gonchar@erlang-solutions.com
%%% @copyright 2018 Erlang Solutions Ltd.
%%% @doc
%%%   helper module <br/>
%%%   contains functions required for config file pre-processing
%%% @end
%%%-------------------------------------------------------------------

-module(config).

%% API
-export([get_config/0,
         get_config/1,
         process_config/1]).

%% helper API functions for config processing.
-export([getenv/2,
         getenv/3]).
%%====================================================================
%% API
%%====================================================================

%%%-------------------------------------------------------------------
%%% @doc
%%% executes config:get_config/1 for default config file.
%%%
%%% default config file is specified in one of the next ways: <br/>
%%%  1) in &lt;APP_NAME_IN_CAPITALS&gt;_CONFIG OS env. variable <br/>
%%%  2) if OS env. variable is not set, 'config_file' erlang app
%%%     env. variable is used instead <br/>
%%%  3) if erlang app env. variable is not set, &lt;app_name&gt;.cfg
%%%     is used.
%%% @end
%%%-------------------------------------------------------------------
-spec get_config() -> list().
get_config() ->
    {ok, App} = application:get_application(),
    AppName = atom_to_list(App),
    EnvConfigVariable = string:uppercase(AppName) ++ "_CONFIG",
    DefaultConfigFile1 = AppName ++ ".cfg",
    DefaultConfigFile2 = application:get_env(App, config_file, DefaultConfigFile1),
    ConfigFile = os:getenv(EnvConfigVariable, DefaultConfigFile2),
    get_config(ConfigFile).

%%%-------------------------------------------------------------------
%%% @doc
%%% reads config file and processes it.
%%% @param ConfigFile path to configuration file. it can be absolute
%%% path or path relative to the current erlang application's priv
%%% directory
%%% @end
%%%
%%%-------------------------------------------------------------------
-spec get_config(string()) -> list().
get_config(ConfigFile) ->
    {ok, App} = application:get_application(),
    PrivDir = code:priv_dir(App),
    ConfigFilePath = filename:absname(ConfigFile, PrivDir),
    {ok, Config} = file:consult(ConfigFilePath),
    process_config(Config).

%%%-------------------------------------------------------------------
%%% @doc
%%% Recursively goes through the lists/tuples/maps and replaces
%%% all the {{M,F,A}} occurrences with the values returned from
%%% M:F(A) call. <br/>
%%% Returned values are not processed anymore and used as is. <br/>
%%% Arguments of the function processes prior to the call.
%%% @end
%%%-------------------------------------------------------------------
-spec process_config(list()) -> list().
process_config(Config) when is_list(Config) ->
    process_config_elements(Config).

%%====================================================================
%% helper API functions for config processing.
%%====================================================================

%%%-------------------------------------------------------------------
%%% @doc
%%% helper function, can be used in config file to extract value of
%%% of OS environment variable.
%%% @end
%%%-------------------------------------------------------------------
-spec getenv(string(), integer) -> integer();
            (string(), binary) -> bitstring();
            (string(), list) -> string().
getenv(Env, integer) ->
    getenv(Env, integer, 0);
getenv(Env, list) ->
    getenv(Env, list, "");
getenv(Env, binary) ->
    getenv(Env, binary, <<"">>).

%%%-------------------------------------------------------------------
%%% @doc
%%% helper function, the same as config:getenv/2 but with additional
%%% parameter - default value
%%% @end
%%%-------------------------------------------------------------------
-spec getenv(string(), integer, integer()) -> integer();
            (string(), binary, bitstring()) -> bitstring();
            (string(), list, string()) -> string().
getenv(Env, Type, DefaultValue) ->
    case os:getenv(Env) of
        false -> DefaultValue;
        Value -> convert_env(Value, Type)
    end.

%%====================================================================
%% Internal functions
%%====================================================================
process_config_elements({{M, F, A}}) when is_atom(M), is_atom(F), is_list(A) ->
    erlang:apply(M, F, process_config_elements(A));
process_config_elements(Tuple) when is_tuple(Tuple) ->
    list_to_tuple(process_config_elements(tuple_to_list(Tuple)));
process_config_elements(Map) when is_map(Map) ->
    maps:from_list(process_config_elements(maps:to_list(Map)));
process_config_elements(List) when is_list(List) ->
    [process_config_elements(Element) || Element <- List];
process_config_elements(Element) -> Element.


convert_env(Value, binary) ->
    list_to_binary(Value);
convert_env(Value, integer) ->
    list_to_integer(Value);
convert_env(Value, list) ->
    Value.