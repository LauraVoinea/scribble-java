# Erlang SMTP Implementation

This project contains an Erlang implementation of a distributed protocol named "smtp", 
specified in `scribble-gt-demos/scribble/smtp.scr`. It demonstrates the interaction between two roles: C, S using Erlang's `gen_statem` behavior.

## Implementation Details
*   **Structure per Role (e.g., Role C):**
    *   `gen_c.erl`: A generic `gen_statem` wrapper. It handles the core state machine logic defined by the protocol (states, transitions, event forwarding, message sending with counters). It takes a callback module as an argument. This module is typically *not* modified by the user implementing the protocol logic.
    *   `c.erl`: The callback module for `gen_c.erl`. It implements the application-specific logic for role C, such as making choices, managing role-specific data, and reacting to messages forwarded by `gen_c`. This is the module the user would typically implement or modify.
    *   `c.hrl`: A header file defining the `state_data` record used by role B to maintain its state (e.g., PIDs of other roles, counters).
*   **Message Handling:** Messages between roles include a counter (`mc_counter_1` in the provided code) to discard stale messages stemming from either side of the mixed-choice. 
* The generic modules (`gen_c`, `gen_s`) handle checking these counters and removing stale messages from the event queue before forwarding events to the callback modules.
*   **Debugging/Logging:** The `start_link` functions are configured to enable `gen_statem` tracing, logging debug information to files like `c_debug.log`, `s_debug.log`.

## How to Run

1.  **Prerequisites:** Ensure you have Erlang/OTP and rebar3 installed.
2.  **Navigate to Project Root:** Open your terminal and `cd` into the `smtp` directory (the one containing `rebar3.config`).
3.  **Compile the project.** Rebar3 will fetch dependencies (if any) and compile your source code. 
``` bash
    rebar3 compile
```
4. **Run the Application:** 

    ```bash
    rebar3 shell
    ```
   Inside the shell, start the application:
    ```erlang
    application:start(smtp). 

5. You should see the output from the functions of roles C and S in the shell. The debug logs (`c_debug.log`, `s_debug.log`) will be created in the project's root directory (or wherever the shell is started). 

6. **Stop:** To stop the application:
 ```erlang
 application:stop(smtp).
 ```
To stop the shell, press `Ctrl+C`, or type `q().` and press Enter.

7. **Run Tests:** Execute the following command in the `smtp/` directory.
    ```bash
    rebar3 eunit
    ```



## Key Features Demonstrated

*   Handling of multiparty, branch/select, mixed choice, and message purging.
*   Generic protocol mechanics (`gen_*` modules) vs. application-specific logic (`c.erl`, `s.erl`).

---
