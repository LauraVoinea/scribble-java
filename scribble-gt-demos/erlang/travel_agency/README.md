# Erlang Calculator Protocol Implementation

This project contains an Erlang implementation of a distributed protocol named "travel_agency", 
specified in `scribble-gt-demos/scribble/travel_agency.scr`. It demonstrates the interaction between three roles: Client, Agency, and Supplier, using Erlang's `gen_statem` behavior.

## Implementation Details
*   **Structure per Role (e.g., Role B):**
    *   `gen_client.erl`: A generic `gen_statem` wrapper. It handles the core state machine logic defined by the protocol (states, transitions, event forwarding, message sending with counters). It takes a callback module as an argument. This module is typically *not* modified by the user implementing the protocol logic.
    *   `client.erl`: The callback module for `gen_client.erl`. It implements the application-specific logic for role Client, such as making choices, managing role-specific data, and reacting to messages forwarded by `gen_client`. This is the module the user would typically implement or modify.
    *   `client.hrl`: A header file defining the `state_data` record used by role Client to maintain its state (e.g., PIDs of other roles, counters).
*   **Message Handling:** Messages between roles include a counter (`mc_counter_1` in the provided code) to discard stale messages stemming from either side of the mixed-choice. 
* The generic modules (`gen_client`, `gen_supplier`, `gen_agency`) handle checking these counters and removing stale messages from the event queue before forwarding events to the callback modules.
*   **Debugging/Logging:** The `start_link` functions are configured to enable `gen_statem` tracing, logging debug information to files like `supplier_debug.log`, `client_debug.log`, `agency_debug.log`.

## How to Run

1.  **Prerequisites:** Ensure you have Erlang/OTP and rebar3 installed.
2.  **Navigate to Project Root:** Open your terminal and `cd` into the `travel_agency` directory (the one containing `rebar3.config`).
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
    application:start(travel_agency). 

5. You should see the output from the functions of roles A, B, and C in the shell. The debug logs (`a_debug.log`, `b_debug.log`, `c_debug.log`) will be created in the project's root directory (or wherever the shell is started). 

6. **Stop:** To stop the application:
 ```erlang
 application:stop(travel_agency).
 ```
To stop the shell, press `Ctrl+C`, or type `q().` and press Enter.

7. **Run Tests:** Execute the following command in the `travel_agency/` directory.
    ```bash
    rebar3 eunit
    ```



## Key Features Demonstrated

*   Handling of multiparty, branch/select, mixed choice, and message purging.
*   Generic protocol mechanics (`gen_*` modules) vs. application-specific logic (`client.erl`, `supplier.erl`, `agency.erl`).

---
