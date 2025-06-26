# Erlang Online Wallet Protocol Implementation

This project contains an Erlang implementation of a distributed protocol named "Online Wallet", 
specified in `scribble-gt-demos/scribble/OnlineWallet.scr`. It demonstrates the interaction between three roles: A, B, and S, using Erlang's `gen_statem` behavior.

## Implementation Details
*   **Structure per Role (e.g., Role B):**
    *   `gen_b.erl`: A generic `gen_statem` wrapper. It handles the core state machine logic defined by the protocol (states, transitions, event forwarding, message sending with counters). It takes a callback module as an argument. This module is typically *not* modified by the user implementing the protocol logic.
    *   `b.erl`: The callback module for `gen_b.erl`. It implements the application-specific logic for role B, such as making choices, managing role-specific data, and reacting to messages forwarded by `gen_b`. This is the module the user would typically implement or modify.
    *   `b.hrl`: A header file defining the `state_data` record used by role B to maintain its state (e.g., PIDs of other roles, counters).
*   **Message Handling:** Messages between roles include a counter (`mc_counter_1` in the provided code) to discard stale messages stemming from either side of the mixed-choice. 
* The generic modules (`gen_a`, `gen_b`, `gen_s`) handle checking these counters and removing stale messages from the event queue before forwarding events to the callback modules.
*   **Debugging/Logging:** The `start_link` functions are configured to enable `gen_statem` tracing, logging debug information to files like `a_debug.log`, `b_debug.log`, `s_debug.log`.

## How to Run

 1.  **Prerequisites:** Ensure you have Erlang/OTP and rebar3 installed.
 2.  **Navigate to Project Root:** Open your terminal and `cd` into the `online_wallet` directory (the one containing `rebar3.config`).
 3.  **Compile the project.** Rebar3 will fetch dependencies (if any) and compile your source code.
 ```bash
    rebar3 compile
 ```
 4. **Run the Application:**
 ```bash
  rebar3 shell
 ```
   Inside the Erlang shell, start the application:
```rebar3
 application:start(online_wallet).
```
   You should see interaction logs for roles A, B, and S, and debug files (`a_debug.log`, `b_debug.log`, `s_debug.log`) will be created in the project root.

5. **Stop the Application:**
```rebar3
   application:stop(online_wallet).
```
   To exit the shell, press `Ctrl+C` twice or type `q().` and press Enter.


## Key Features Demonstrated
* Handling of multiparty communication, branching/select, mixed choice, and message purging.
* Separation of generic protocol mechanics (the `gen_*` modules) from application-specific logic (`a.erl`, `b.erl`, `s.erl`).
* Use of counters to discard stale messages in mixed-choice scenarios.

