# Erlang Calculator Protocol Implementation

This project contains an Erlang implementation of a distributed protocol named "calculator", 
specified in `scribble-gt-demos/scribble/calculator.scr`. It demonstrates the interaction between three roles: A, B, and C, using Erlang's `gen_statem` behavior.

## Implementation Details
*   **Structure per Role (e.g., Role B):**
    *   `gen_b.erl`: A generic `gen_statem` wrapper. It handles the core state machine logic defined by the protocol (states, transitions, event forwarding, message sending with counters). It takes a callback module as an argument. This module is typically *not* modified by the user implementing the protocol logic.
    *   `b.erl`: The callback module for `gen_b.erl`. It implements the application-specific logic for role B, such as making choices, managing role-specific data, and reacting to messages forwarded by `gen_b`. This is the module the user would typically implement or modify.
    *   `b.hrl`: A header file defining the `state_data` record used by role B to maintain its state (e.g., PIDs of other roles, counters).
*   **Message Handling:** Messages between roles include a counter (`mc_counter_1` in the provided code) to discard stale messages stemming from either side of the mixed-choice. 
* The generic modules (`gen_a`, `gen_b`, `gen_c`) handle checking these counters and removing stale messages from the event queue before forwarding events to the callback modules.
*   **Debugging/Logging:** The `start_link` functions are configured to enable `gen_statem` tracing, logging debug information to files like `a_debug.log`, `b_debug.log`, `c_debug.log`.

## How to Run

1.  **Prerequisites:** Ensure you have Erlang/OTP and rebar3 installed.
2.  **Navigate to Project Root:** Open your terminal and `cd` into the `calculator` directory (the one containing `rebar3.config`).
3.  **Compile the project.** Rebar3 will fetch dependencies (if any) and compile your source code. 
``` bash
    rebar3 compile
```
4. **Run the Application:** You can run the application from the command line, providing two numbers and an operation (`sum` or `diff`).

   ```bash
   rebar3 compile
   erl -pa _build/default/lib/calculator/ebin -noshell -s calculator main -s init stop -- <num1> <num2> <sum|diff>
   ```

   For example:

   ```bash
   erl -pa _build/default/lib/calculator/ebin -noshell -s calculator main -s init stop -- 10 5 diff
   ```

5. You should see the output from the functions of roles A, B, and C in the shell. The debug logs (`a_debug.log`, `b_debug.log`, `c_debug.log`) will be created in the project's root directory (or wherever the shell is started).

6. **Stop:** The application will stop automatically after the operation is complete. To stop the shell (if you are running it interactively), press `Ctrl+C`, or type `q().` and press Enter.

7. **Run Tests:** Execute the following command in the `calculator/` directory.
    ```bash
    rebar3 eunit
    ```



## Key Features Demonstrated

*   Handling of multiparty, branch/select, mixed choice, and message purging.
*   Generic protocol mechanics (`gen_*` modules) vs. application-specific logic (`a.erl`, `b.erl`, `c.erl`).

---
