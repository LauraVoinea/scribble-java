# Erlang Two Buyer Protocol Implementation

This project contains an Erlang implementation of a distributed protocol named "two_buyer", 
specified in `scribble-gt-demos/scribble/TwoBuyer.scr`. It demonstrates the interaction between three roles: A, B, and C, using Erlang's `gen_statem` behavior.

## Implementation Details
*   **Structure per Role (e.g., Role Bob):**
    *   `gen_bob.erl`: A generic `gen_statem` wrapper. It handles the core state machine logic defined by the protocol (states, transitions, event forwarding, message sending with counters). It takes a callback module as an argument. This module is typically *not* modified by the user implementing the protocol logic.
    *   `bob.erl`: The callback module for `gen_bob.erl`. It implements the application-specific logic for role B, such as making choices, managing role-specific data, and reacting to messages forwarded by `gen_bob`. This is the module the user would typically implement or modify.
    *   `bob.hrl`: A header file defining the `state_data` record used by role Bob to maintain its state (e.g., PIDs of other roles, counters).
*   **Message Handling:** Messages between roles include a counter (`mc_counter_1` in the provided code) to discard stale messages stemming from either side of the mixed-choice. 
* The generic modules (`gen_alice`, `gen_bob`, `gen_seller`) handle checking these counters and removing stale messages from the event queue before forwarding events to the callback modules.
*   **Debugging/Logging:** The `start_link` functions are configured to enable `gen_statem` tracing, logging debug information to files like `alice_debug.log`, `bob_debug.log`, `seller_debug.log`.

## How to Run

1.  **Prerequisites:** Ensure you have Erlang/OTP and rebar3 installed.
2.  **Navigate to Project Root:** Open your terminal and `cd` into the `two_buyer` directory (the one containing `rebar3.config`).
3.  **Compile the project.** Rebar3 will fetch dependencies (if any) and compile your source code. 
``` bash
    rebar3 compile
```
4. **Run the Application:** 

    ```bash
    rebar3 shell
    ```
   Inside the shell, start the application:
    ```rebar3
    application:start(two_buyer). 

5. You should see the output from the functions of roles A, B, and C in the shell. The debug logs (`a_debug.log`, `b_debug.log`, `c_debug.log`) will be created in the project's root directory (or wherever the shell is started). 

6. **Stop:** To stop the application:
     ```rebar3
     application:stop(two_buyer).
     ```
To stop the shell, press `Ctrl+C`, or type `q().` and press Enter.

7. **Run Tests:** Execute the following command in the `two_buyer/` directory.
    ```bash
    rebar3 eunit
    ```



## Key Features Demonstrated

*   Handling of multiparty, branch/select, mixed choice, and message purging.
*   Generic protocol mechanics (`gen_*` modules) vs. application-specific logic (`alice.erl`, `bob.erl`, `seller.erl`).

---
