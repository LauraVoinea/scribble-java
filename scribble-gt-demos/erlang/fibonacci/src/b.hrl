-record(state_data, {mc_counter_1 = 0 :: integer(),
                     a_pid :: pid() | undefined,
                     prev_value :: integer(),
                     curr_value :: integer()
}).