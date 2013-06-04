
-type date() :: {non_neg_integer(), 1..12, 0..31}.
-type time() :: {0..23, 0..59, 0..59}.
-type datetime() :: {date(), time()}.


%% A record describing a stored job that can be executed by the scheduler
%% or directly by the module which implements the job.
-record(job_desc, {key :: atom(),                                % the key of the job (job_desc key)
		   cfg :: [{term(), term()}]                     % the configuration (a job-specific plist)
		  }).


%% A record of a finished job.
-record(job_report, {job_id :: string(),                         % a unique job name
		     job_desc_key :: atom(),                     % id of the job (job_desc key) 
		     started :: calendar:datetime(),             % timestamp when the job was run
		     completed :: calendar:datetime(),           % when job was completed
		     result :: { success | failure, string() },  % job result and explanation string
		     wksp_dir :: [{term(),term()}],              % workspace plist (multiple workspaces possible)
		     stor_dom :: string(),                       % storage domain for output files
		     log_stor_id :: string(),                    % plan log file storage id
		     inst :: [{term(), term()}]                  % a plist for instrumentation (e.g. execution times of tasks)
		    }).
		     


%% A record describing an active job
-record(job_activity, {job_desc_key :: atom(),                             % the id of the job (job_desc key)
		       status :: {running, calendar:datetime()} 
			         | {waiting, tte_sec},           % status = running | waiting
		       run_time_est = unavailable :: unavailable | integer()   % run_time = estimated run_time
		      }).
		     
