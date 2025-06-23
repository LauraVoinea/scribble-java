# Fibonacci Protocol Implementation

This project contains an Erlang implementation of the Fibonacci protocol, specified in `scribble-gt-demos/scribble/Fibonacci.scr`. 
It demonstrates recursive computation between two roles a and b.

## Implementation Details
*   **Structure per Role:**
    *   `gen_a.erl` / `gen_b.erl`: Generic `gen_statem` wrappers that implement the protocol state machine (states, transitions, message forwarding, keeping stale messages counters). These are not typically modified.
    *   `a.erl` / `b.erl`: Callback modules implementing the Fibonacci logic for each role, handling incoming messages and generating outgoing messages.
    *   `a.hrl` / `b.hrl`: Header files defining the `state_data` record (e.g., PIDs, current counter).
*   **Message Handling:** Messages include a mixed-choice counter (`mc_counter_1`) to discard stale messages. The generic modules check counters and purge stale events before invoking callbacks.
*   **Debugging/Logging:** Enable `gen_statem` tracing; debug logs are written to `a_debug.log` and `b_debug.log` in the project root.

## How to Run

1.  **Prerequisites:** Ensure you have Erlang/OTP and `rebar3` installed.
2.  **Navigate to Fibonacci Directory:**
    ```bash
    cd scribble-gt-demos/erlang/fibonacci
    ```
3.  **Compile the project.** Rebar3 will fetch dependencies and compile the source code.
     ``` bash
        rebar3 compile
     ```
4. **Run the Application:**
 
     ```bash
    rebar3 shell
     ```
    Inside the shell, start the application:
     ```rebar3
    application:start(fibonacci). 
     ```
 
5. You should see a sequence of Fibonacci values exchanged between roles a and b in the shell. Debug logs (`a_debug.log`, `b_debug.log`) will be created in the project root.
 
6. **Stop:** To stop the application:
     ```rebar3
     application:stop(fibonacci).
     ```

    To stop the shell, press `Ctrl+C` twice or type `q().` and press Enter.
 
7. **Run Tests:** Execute the following command in the `fibonacci/` directory.
    ```bash
    rebar3 eunit
    ```
## Key Features Demonstrated    
*   Recursive computation of Fibonacci numbers.
*   Handling of recursion, branch/select, and mixed choice.
*   Generic protocol mechanics (`gen_*` modules) vs. application-specific logic (`a.erl`, `b.erl`).
