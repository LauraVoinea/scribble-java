-record(state_data, {mc_counter_1 = 0 :: integer(), a_pid :: pid() | undefined, c_pid :: pid() | undefined}).
-type state_data() :: #state_data{mc_counter_1 :: integer(), a_pid :: pid() | undefined, c_pid :: pid() | undefined}.

-export_type([state_data/0]).
