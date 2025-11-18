%% Topos Error Record Definition
%% Shared header for error handling across the compiler

-record(error, {
    severity :: error | warning | note,
    code :: atom(),                          % e.g., 'E001_syntax_error'
    message :: string(),                      % Primary error message
    file :: string() | undefined,             % Source file path
    line :: pos_integer(),                    % Line number (1-indexed)
    column :: pos_integer() | undefined,      % Column number (1-indexed)
    source_line :: string() | undefined,      % The actual source line
    context_before :: [string()],             % Lines before error
    context_after :: [string()],              % Lines after error
    suggestion :: string() | undefined,       % How to fix it
    related :: [#error{}]                     % Related errors/notes
}).
