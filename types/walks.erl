% example types (type safety)
% -type person() :: {person, name(), age()}.
% -type people() :: [person()].
% -type name()   :: {firstname, string()}.
% -type age()    :: integer().
% -opaque type   :: ... (used so only the module know this type structure)

% example function spec
% -spec file:open(FileName, Modes) -> {ok, Handle} | {error, Why} when 
%       FileName  :: string(),
%       Modes     :: [Mode],
%       Mode      :: read | write | ...,
%       Handle    :: file_handle(),
%       Why       :: error_term().

-module(walks).
-export([plan_route/2]).

% plan_route specs
% plan route will receive two points and will return a route type
-spec plan_route(From:: point(), To:: point()) -> route().

% type declarations
-type direction() :: north | south | east | west.
-type point()     :: {integer(), integer()}.
-type route()     :: [{go, direction(), integer()}].
