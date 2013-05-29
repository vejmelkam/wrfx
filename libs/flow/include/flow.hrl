
%% the plan record
%% @type plan() = #plan{id::atom(), tasks::[task()]}

-type task() :: { term(), term(), list() }.

-record(plan, {id :: atom(),        % id of the plan
	       tasks :: [task()]    % a list of tasks
	      }).
