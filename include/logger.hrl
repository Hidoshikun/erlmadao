-ifndef(LOGGER_HRL).
-define(LOGGER_HRL, true).


%% Error levels
-define(LOG_LEVELS, [
    {0, none, "No log"},
    {1, critical, "Critical"},
    {2, error, "Error"},
    {3, warning, "Warning"},
    {4, info, "Info"},
    {5, debug, "Debug"}
]).

-compile([{parse_transform, lager_transform}]).

-define(DEBUG(Format),
    lager:debug(Format)).
-define(DEBUG(Format, Args),
    lager:debug(Format, Args)).

-define(INFO_MSG(Format),
    lager:info(Format)).
-define(INFO_MSG(Format, Args),
    lager:info(Format, Args)).

-define(WARNING_MSG(Format),
    lager:warning(Format)).
-define(WARNING_MSG(Format, Args),
    lager:warning(Format, Args)).

-define(ERROR_MSG(Format),
    lager:error(Format)).
-define(ERROR_MSG(Format, Args),
    lager:error(Format, Args)).

-define(CRITICAL_MSG(Format),
    lager:critical(Format)).
-define(CRITICAL_MSG(Format, Args),
    lager:critical(Format, Args)).

-define(PRINT(Format),
    io:format("[~p/~p] " ++ Format, [?MODULE, ?LINE])).
-define(PRINT(Format, Args),
    io:format("[~p/~p] " ++ Format, [?MODULE, ?LINE] ++ Args)).

-define(FORMAT_RECORD(Record),
    lager:pr(Record, ?MODULE)).
-define(FORMAT_RECORD_LIST(RecordList),
    [?FORMAT_RECORD(Record) || Record <- RecordList]).


-endif. % LOGGER_HRL