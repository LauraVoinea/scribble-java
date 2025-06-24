-record(state_data, {
    mc_counter_1 = 0 :: integer(),
    carol_pid :: pid() | undefined,
    srv_pid :: pid() | undefined
}).