
%% the plan record
%% @type plan() = #plan{id::atom(), tasks::[task()]}

-type task() :: { term(), term(), list() }.

-record(plan, {id :: atom(),        % id of the plan
	       tasks :: [task()]    % a list of tasks
	      }).


-record(instr_task, {mfa :: {atom(), atom(), [term()]},            % task to instrument
		     with_key :: term(),                           % store under this key in plan instrumentation record
		     what :: [atom()]                               % what instrumentation is to be obtained
		    }).
