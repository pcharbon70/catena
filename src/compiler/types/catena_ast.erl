%%%-------------------------------------------------------------------
%%% @doc Simplified AST for Type Inference
%%%
%%% This module provides simplified Abstract Syntax Tree structures
%%% for use during type inference. These structures are simpler than
%%% the full parser AST and focus only on the information needed for
%%% type checking.
%%%
%%% This is a minimal PoC implementation that will be replaced by
%%% integration with the full parser in future tasks.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_ast).

-export_type([
    expr/0,
    pattern/0,
    literal/0
]).

%%%===================================================================
%%% Type Definitions
%%%===================================================================

%% @doc Expression AST nodes
-type expr() ::
    % Literals
    {lit, literal()} |

    % Variables
    {var, atom()} |

    % Function application
    {app, expr(), expr()} |

    % Lambda abstraction
    {lam, atom(), expr()} |

    % Let binding (non-recursive)
    {'let', atom(), expr(), expr()} |

    % Let-rec binding (recursive)
    {'letrec', atom(), expr(), expr()} |

    % If-then-else
    {'if', expr(), expr(), expr()} |

    % Case expression (pattern matching)
    {case_expr, expr(), [{pattern(), expr()}]} |

    % Tuple construction
    {tuple, [expr()]} |

    % Record construction
    {record, [{atom(), expr()}]} |

    % Record field access
    {field, expr(), atom()} |

    % Variant constructor
    {variant, atom(), [expr()]} |

    % Type annotation
    {ann, expr(), catena_types:type()}.

%% @doc Pattern AST nodes
-type pattern() ::
    % Wildcard pattern
    {pwild} |

    % Literal pattern
    {plit, literal()} |

    % Variable pattern (binds variable)
    {pvar, atom()} |

    % Tuple pattern
    {ptuple, [pattern()]} |

    % Record pattern
    {precord, [{atom(), pattern()}]} |

    % Variant pattern
    {pvariant, atom(), [pattern()]} |

    % As-pattern (pattern with name)
    {pas, atom(), pattern()}.

%% @doc Literal values
-type literal() ::
    {int, integer()} |
    {float, float()} |
    {bool, boolean()} |
    {string, binary()} |
    {atom, atom()} |
    {unit}.
