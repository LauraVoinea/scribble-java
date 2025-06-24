# Erlang DistributedLogging Protocol Implementation

This project contains an Erlang implementation of a distributed protocol named "DistributedLogging", specified in `scribble-gt-demos/scribble/DistributedLogging.scr`. It demonstrates the interaction between two roles: Controller and Logs, using Erlang's `gen_statem` behavior.

## Implementation Details
*   **Structure per Role (e.g., Controller):**
    *   `gen_controller.erl`: A generic `gen_statem` wrapper. It handles the core state machine logic defined by the protocol (states, transitions, event forwarding, message sending with counters). It takes a callback module as an argument. This module is typically *not* modified by the user implementing the protocol logic.
    *   `controller.erl`: The callback module for `gen_controller.erl`. It implements the application-specific logic for the Controller role, such as making choices, managing role-specific data, and reacting to messages forwarded by `gen_controller`. This is the module the user would typically implement or modify.
    *   `controller.hrl`: A header file defining the `state_data` record used by Controller to maintain its state (e.g., PIDs of other roles, counters).
    *   Similarly, `gen_logs.erl`, `logs.erl`, and `logs.hrl` are provided for the Logs role.
*   **Message Handling:** Messages between roles include a counter (`mc_counter_1` in the provided code) to discard stale messages stemming from either side of the mixed-choice. The generic modules (`gen_controller`, `gen_logs`) handle checking these counters and removing stale messages from the event queue before forwarding events to the callback modules.
*   **Debugging/Logging:** The `start_link` functions are configured to enable `gen_statem` tracing, logging debug information to files like `controller_debug.log`, `logs_debug.log`.

## How to Run

1.  **Prerequisites:** Ensure you have Erlang/OTP and rebar3 installed.
2.  **Navigate to Project Root:** Open your terminal and `cd` into the `distributed_logging` directory (the one containing `rebar3.config`).
3.  **Build the Project:**
    ```sh
    rebar3 compile
    ```
4.  **Run the Application:**
    ```bash
    rebar3 shell
    ```
    Inside the shell, start the application:
    ```rebar3 
    application:start(distribute_logging).
    ```
5.  You should see output from the functions of roles API, Controller, Storage, and User in the shell.

6.  **Stop:** To stop the application:
    ```rebar3
    application:stop(distribute_logging).
    ```

7.  **Logs:** Debug output will be written to files such as `controller_debug.log` and `logs_debug.log` if tracing is enabled in the code.

## File Structure

- `src/controller.erl`, `src/controller.hrl`, `src/gen_controller.erl`: Implementation for the Controller role.
- `src/logs.erl`, `src/logs.hrl`, `src/gen_logs.erl`: Implementation for the Logs role.
- `src/distributed_logging_app.erl`, `src/distributed_logging_sup.erl`: Application and supervisor setup.
- `src/distributed_logging.app.src`: OTP application resource file.

## Notes
- The protocol logic is split between generic state machine modules (`gen_*`) and role-specific callback modules (`controller.erl`, `logs.erl`).
- Message counters are used to ensure correct message ordering and to discard stale messages.
