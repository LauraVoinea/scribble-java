# Erlang Circuit Breaker Protocol Implementation

This project contains an Erlang implementation of a distributed protocol named "circuit_breaker", 
specified in `scribble-gt-demos/scribble/CircuitBreaker.scr`. It demonstrates the interaction between four roles: API, Controller, Storage, User, using Erlang's `gen_statem` behavior.

## Implementation Details
-   **Structure per Role (e.g., Role API):**
-       `gen_<role>.erl`: A generic `gen_statem` wrapper handling the core state machine (states, transitions, event forwarding, message counters). This module is typically *not* modified by the user.
-       `<role>.erl`: The callback module implementing application-specific logic for that role (choices, state data, message handling).
-       `<role>.hrl`: A header file defining the `state_data` record for that role (PIDs, counters).
-   **Message Handling:** Messages between roles include a counter (`mc_counter_1`) to discard stale messages from mixed choices.
-   The generic modules (`gen_api`, `gen_controller`, `gen_storage`, `gen_user`) check these counters and purge stale events before invoking callbacks.

## How to Run

1.  **Prerequisites:** Ensure you have Erlang/OTP and rebar3 installed.
2.  **Navigate to Project Root:** Open your terminal and `cd` into the `circuit_breaker` directory (the one containing `rebar3.config`).
3.  **Compile the project.** Rebar3 will fetch dependencies (if any) and compile your source code:
    ```bash
    rebar3 compile
    ```
4.  **Run the Application:**
    ```bash
    rebar3 shell
    ```
    Inside the shell, start the application:
    ```rebar3 
    application:start(circuit_breaker).
    ```
5.  You should see output from the functions of roles API, Controller, Storage, and User in the shell.
6.  **Stop:** To stop the application:
    ```rebar3
    application:stop(circuit_breaker).
    ```

To stop the shell, press `Ctrl+C` or type `q().` and press Enter.

7. **Run Tests:** Execute the following command in the `circuit_breaker/` directory.
    ```bash
    rebar3 eunit
    ```



## Key Features Demonstrated

*   Handling of multiparty, branch/select, mixed choice, and message purging.
*   Generic protocol mechanics (`gen_*` modules) vs. application-specific logic (`controller.erl`, `api.erl`, `controller.erl`, `user.erl`).

---
