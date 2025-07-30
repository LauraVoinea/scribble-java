
---

**OOPSLA 24/25 -- Paper #672 -- Artifact Overview**

# Mixed-Choice, Asynchronous Multiparty Session Types


---


# 1. <a name="INTRO"></a> Introduction

## 1.1. Archive contents

The artifact archive `paper672.zip` contains:

- An **Overview** of the artifact (i.e., this document) in three formats:
    - markdown: `paper672-overview.md` -- links clickable depending on markdown viewer app;
    - pdf: `paper672-overview.pdf`.
- The **main artifact** as a **Docker image**: `oopsla2425-paper672-artifact.tar.gz`
- The original submission version of our **paper**: `oopsla2425-paper672.pdf`

Following the Call for Artifacts, this Overview has the following sections:

- [1. Introduction](#INTRO)
- [2. Hardware dependencies](#HARDWARE)
- [3. Getting started guide](#START)
- [4. Step-by-step instructions](#STEPS)
- [5. Reusability guide](#REUSABILITY)


---

## 1.2. <a name="PURPOSE"></a> Purpose: A toolchain for specifying and implementing Erlang `gen_statem` programs using mixed-choice multiparty session types

This artifact demonstrates the prototype toolchain presented in the
submitted paper.

Acronyms:

- MST = Multiparty Session Types
- EFSM = Event-driven State Machine (a la Erlang's [`gen_statem`](https://www.erlang.org/doc/system/statem.html))

As described in the paper (mainly Secs. 2.2 and 5), a programmer follows two
main steps:

1. **Specify the message passing protocol** using our mixed-choice extension of the Scribble protocol language.  
   Our tool statically validates the syntactic conditions for well-formedness
   based on our theory in the paper.
   If valid, it generates, for each role in the protocol, two Erlang modules:
   - **A protocol-enforcing generic behaviour module -- `gen_<role>.erl`**  
     This module is a library that programmers use but do not modify.  It
     provides a protocol- and role-specific `gen_statem` wrapper that enforces
     the protocol rules for the given role.  It encapsulates the EFSM logic
     derived from the protocol for this role, including state transitions,
     message handling, and the runtime mechanisms for mixed-choice and garbage
     collection.  It exposes a public API for sending messages.  
     (The terminology "generic behaviour" is from Erlang -- to reiterate: this
     module is specifically for the given protocol and role.)
   - **A template callback module -- `<role>.erl`**  
     This module is generated to provide a template implementation of a user
     program on top of the above module.  The code generation creates
     placeholder callback functions for handling the EFSM events.  The
     programmer can modify and add to these to implement the required
     application-specific logic (e.g., local computations after receiving a
     message, internal decisions for selecting protocol choices, etc).

2. **Adapt and complete the generated template programs.**  
The programmer can complete the program as follows.
    -   Modify and add to the placeholder functions in the template
        (`<role>.erl`) the specific logic for the application.
    -   Configure how the different roles will be launched and connected at
        startup.  This may include:
        -   Launching a process for each role in the protocol.
        -   Distributing the initial contact information (process IDs) so the
            roles know how to communicate with each other (the connection function generated in `<role>.erl` can be used for that.
        -   Providing supervision of these processes for fault-tolerance.


---

## 1.3. <a name="CLAIMS"></a> Claims


The main statement in the submitted paper: (Apologies, we did not
format it as an explicit "Data-Availability Statement" section.)

> l.1067 **<em>We will submit our implementation, examples and RabbitMQ case study as an artifact.</em>**

Our Docker image supports these claims by providing these materials:

- The source code of:
  - Our Scribble-based tool for validating protocols and generating Erlang
    `gen_statem` code.
  - All the example protocols from Table 1 along with their pre-generated (and
    minimally implemented) Erlang code.
  - rabbitmq-server with a replaced amqp_selective_consumer module following our framework.
- Scripts and a preconfigured Erlang/OTP environment with `rebar3` (the
  official Erlang build tool) for building and running all of the above.



---

---

# 2. <a name="HARDWARE"></a> Hardware dependencies

Our artifact has no specific hardware requirements.


---

---

# 3. <a name="START"></a> Getting started guide


## 3.1. <a name="INITIAL"></a> Initial set up and test

**Prerequisites**.

- Docker is installed and running – e.g., see this
  [tutorial](https://docs.docker.com/get-started/).

Notes on the artifact.

- This docker image has been tested on following environment as the host machine:
  - MacOS 14.7.1, on a MacBook Pro with 1.4 GHz Quad-Core Intel Core i5 and 16GB RAM.
  - MacOS 15.3.1, on a MacBook Pro with M1 and 16GB RAM.
  - MacOS 15.5, on a MacBook Pro with M4 and 16GB RAM.
  - Windows 10, Intel Core i5 @ 2.6 GHz (4 cores) and 16GB RAM.

<!--
- The artifact Docker image has `vim` and `nano` preinstalled.
-->


**Steps** for starting from scratch.

1. Put the Docker image `oopsla2425-paper672-artifact.tar.gz` in some
   local directory.  In these steps, we will refer to this directory as
   `$MY_LOCAL_DIR`.

2. Check that Docker is running.

3. Load the image and launch a container with an interactive session.  
   In `$MY_LOCAL_DIR`, do:
   ```sh
   docker load -i oopsla2425-paper672-artifact.tar.gz
   docker run -it --rm --entrypoint /bin/bash scribble-gt
   ```

4. **Test: Protocol validation and code generation.**
    The simplest way to check that the toolchain is working is to run the main script that processes all of our example protocols.  This will validate each protocol and generate the corresponding Erlang code.
    Inside the container, run:
    ```sh
    ./mMST.sh -run-scribble-examples
    ```
    **Expected output**.
    The script will loop through all `.scr` protocol files under `scribble-gt-demos/scribble/`, validating and projecting each of them. It should complete without errors.
    The generated Erlang code will be written to `scribble-java/generated/<protocol_name>` 

    To run all implemented Erlang examples: 
    ```sh
    ./mMST.sh -run-erlang-examples
    ```

    **Expected output**.
    The script will loop through all Erlang/OTP applications under `scribble-gt-demos/erlang/`, running each of them. It should print to console the output of each application, capturing the interactions of roles for each. It should complete without errors.
    
    The script will iterate through each Erlang/OTP application located in `scribble-gt-demos/erlang/`, executing them individually. It will display the output of each application on the console, including logs of the state machines(`*DBG*`). It should complete without errors.

5. **Test: Compile and run an Erlang example.**
    Let's run the `CircuitBreaker` example. The Erlang code has been pre-generated in the image, so we just need to compile and run it.  
    Inside the container, go to the example's directory and
    launch an Erlang shell using `rebar3`:
    ```sh
    cd scribble-gt-demos/erlang/circuit_breaker
    rebar3 shell
    ```
    Inside the Erlang shell (you'll see a `===> Verifying dependencies...` message followed by an Erlang prompt `1>`), start the application:
    ```rebar3
    application:start(circuit_breaker).
    ```
    **Expected output**.
    You will see log messages from the different roles (API, Controller, Storage, User) as they interact according to the protocol. Debug logs (`*DBG*`) from the generated gen_<role>.erl modules provide detailed information about events internal to the EFSM, such as message receptions, consumptions, and state transitions, which verify correct protocol execution.
   The output should look similar to the following (though the exact interleaving and branch choices may vary due to random selection):
   ```
    ok
    *DBG* usr consume internal init_state in state s1
    2> Controller: s1 Sending start_storage to Storage 
    *DBG* controller receive internal {start_controller} in state s1
    *DBG* storage receive cast {<0.142.0>,{start_storage}} in state s1
    Storage: s1 Received start_storage from Controller <0.142.0>
    *DBG* controller consume internal {start_storage} in state s1 => s3
    *DBG* storage receive internal {hard_ping} in state s1
    Controller: s3 Sending start_controller to API 
    ...
    ```

6.  Stop the example.
    First, stop the application.  In the Erlang shell:
    ```rebar3
    application:stop(circuit_breaker).
    ```
    You can then exit the Erlang shell by pressing `ctrl+c`,
    or by entering `q().` and pressing Enter.


---

## 3.2. <a name="DIRS"></a> Main directories insider the container

<table border-collapse="collapse">
<tr>
<td width=25%><strong></strong></td>
<td width=25%><strong>Description</strong></td>
<td width=50%><strong>Path</strong> inside Docker image</td>
</tr>
<tr>
<td>Toolchain</td>
<td>Base dir</td>
<td><code>scribble-java</code></td>
</tr>
<tr>
<td></td>
<td>Main module</td>
<td><code>scribble-java/scribble-gt</code></td>
</tr>
<tr>
<td></td>
<td>Base out dir for <code>mMST.sh</code></td>
<td><code>scribble-java/generated</code></td>
</tr>
<tr>
<td>Examples</td>
<td>Base dir</td>
<td><code>scribble-java/scribble-gt-demos</code></td>
</tr>
<td></td>
<td>Protocol specifications</td>
<td><code>scribble-java/scribble-gt-demos/scribble</code></td>
</tr>
<tr>
<td></td>
<td>Erlang implementations</td>
<td><code>scribble-java/scribble-gt-demos/erlang</code></td>
</tr>
</table>

Modules found in the Base dir other than `scribble-gt` are just the
pre-existing modules inherited from base Scribble.  Our work for this artifact
is in specifically the mentioned `scribble-gt` module.

---


## 3.3. <a name="COMMANDS"></a> Main commands inside the container

Following [1.2](#PURPOSE), the two main activities supported by this artifact
container are:

- Use our tool to validate protocols and generate the protocol- and
  role-specific Erlang modules.
- Implement and run Erlang programs.


**Protocol validation and code generation**.
The main script is:  

```sh
./mMST.sh <<flag>> <<flagargs>> <protocolfile>
```

The `<protocolfile>` is a `.scr` file and is mandatory.  The other `<<..>>` elements are optional.

The following demonstrates usages and key flags.  Assume we are in the
`scribble-java` directory inside the container:

- Check a protocol file:
  ```sh
  ./mMST.sh scribble-gt-demos/scribble/<FileName.scr>
  ```
  This will run the tool on the specified `.scr` file (from the pre-written
  examples).  There is no console output unless there is an error.

- Generate the Erlang code for all roles in a protocol and write to file(s):
  ```sh
  ./mMST.sh -gt-gen-erlang <ProtocolName> scribble-gt-demos/scribble/<FileName.scr>
  ```
  The output will be written to
  `scribble-java/generated/<ProtocolName>`.

- Generate the Erlang code for a specific role in the protocol and write to
  file(s):
  ```sh
  ./mMST.sh -gt-gen-erlang-role <ProtocolName> <Role> scribble-gt-demos/scribble/<FileName.scr>
  ```
  The output will be written to
  `scribble-java/generated/<ProtocolName>`.

- Generate an EFSM representation of the protocol for a specific role:
  ```sh
  ./mMST.sh -gt-gen-efsm <ProtocolName> <Role> scribble-gt-demos/scribble/<FileName.scr>
  ```
  This generates a .dot and a .png file. The output will be written to
  `scribble-java/generated/<ProtocolName>`.

- Remove generated files and build artifacts from the Erlang examples:
  ```sh
  ./mMST.sh -clean-all
  ```

**Compiling and running the Erlang programs**.

- We provide pre-implemented Erlang applications for the examples.  
  Each is implemented as a self-contained `rebar3` project using the
  output of our code generation tool from above.

- To run all Erlang examples:
  ```sh
  ./mMST.sh -run-erlang-examples
  ```

- To run an Erlang example individually, go to the example's directory.  E.g.,
  ```
  cd scribble-gt-demos/erlang/timeout
  ```
- Start an interactive Erlang shell.  This will automatically compile the
  project.
  ```
  rebar3 shell
  ```
- Inside the Erlang shell:
  - Do `application:start(example_name)` to run the application.
  - Do `application:stop(example_name)` to stop the application.
  - Press `ctrl+c` to exit the Erlang shell.  

See [4.1.](#EXAMPLES) for a list of all the examples, their dir names, and
their application names (for running by specifying in `application:start`).

<!--
build

- Scribble?

execute

- scribc
- erlang

 Once inside the container shell you can:

Run each example:

```
./mMST.sh -gt-no-corr scribble-gt-demos/scribble/Timeout.scr
```

This will run scribble on the example, also generate code under ./generated/<ProtocolName>

Run the erlang application: 

```
cd scribble-gt-demos/erlang/timeout/
rebar3 shell
```

Inside rebar3 shell:

```
application:start(timeout). 
application:stop(timeout).
Ctrl + C to quit the rebar3 shell.
Exit to exit the container.
```
-->


---

## 3.4. <a name="NOTES"></a> Additional notes

Warnings.

- Our implementation is set to print most errors noisily, e.g., full stack traces.  Look at the top of the trace for the error message.

Minor syntax/notation differences.

- Our implementation and examples use this syntax for mixed-choices:
  ```
  mixed { ... } or A -> B { ... }  // B is the "observer" of the MC.
  ```
  In the paper, we omitted the `A -> B` part as it can be easily
  inferred from the contents of the LHS and RHS brackets.
- Our current implementation moves the `@` annotation in Fig. 7 (LHS) to here:
  ```
  mixed {
      ...                 // As in the Fig.
  } or W @failed -> FD {  // @ annotation moved here.
      ...                 // As in the Fig but without the @ annotation.
  }
  ```
- Our current implementation  moves the `*` annotation in Fig. 7 (RHS) to
  here:
  ```
  Stop*() from P to Q;
  ```

Minor corrections.

- In Fig. 7 (RHS) in the paper:
  - Lines 5 and 6 should be swapped.
  - There should be an additional line `Ack() from Q to P;` between lines 9
    and 10.

Other notes.

- In the error messages of our tool:
  - The terminology "clear-termination" in our paper is often instead called "left committing".
  - The terminology "single-decision" in our paper is often instead called
    "right commiting".




---

---

# 4. <a name="STEPS"></a> Step-by-step instructions

- <a href="#EXAMPLES">4.1.</a> **Compiling and running the examples** mentioned in the paper.
  - <a href="#TABLE1">4.1.1.</a> **Examples from Table 1**.
  - <a href="#RABBITMQ">4.1.2.</a> **RabbitMQ use case**.
- <a href="#ADDITIONAL">4.2.</a> **Additional info** about the examples.



---

## 4.1. <a name="EXAMPLES"></a> Compiling and running the examples referred to in the paper

### 4.1.1. <a name="TABLE1"></a> Examples from Table 1 in the paper

Please the Table 1 in the paper for references for the examples.

<table border-collapse="collapse">
<tr>
<td width=3% style="border-bottom: 1px solid black"></td>
<td width=20% style="border-bottom: 1px solid black"><strong>Example</strong></td>
<td width=20% style="border-bottom: 1px solid black"><strong>Protocol</strong></td>
<td width=20% style="border-bottom: 1px solid black"><strong>dir/application name</strong></td>
<td width=25% style="border-bottom: 1px solid black"><strong>Erlang src dir</strong></td>
</tr>
<tr>
<td>(1)</td>
<td>Calculator</td>
<td><code>Calculator.scr</code></td>
<td><code>calculator</code></td>
<td><code>calculator/src</code></td>
</tr>
<tr>
<td>(2)</td>
<td>CircuitBreaker</a></td>
<td><code>CircuitBreaker.scr</code></td>
<td><code>circuit_breaker</code></td>
<td><code>circuit_breaker/src</code></td>
</tr>
<tr>
<td>(3)</td>
<td>DistributedLogging</td>
<td><code>DistributedLogging.scr</code></td>
<td><code>distributed_logging</code></td>
<td><code>distributed_logging/src<code></td>
</tr>
<tr>
<td>(4)</td>
<td>Fibonacci</td>
<td><code>Fibonacci.scr</code></td>
<td><code>fibonacci</code></td>
<td><code>fibonacci/src</code></td>
</tr>
<tr>
<td>(5)</td>
<td>SMTP</td>
<td><code>SMTP.scr</code></td>
<td><code>smtp</code></td>
<td><code>SMTP/src<code></td>
</tr>
<tr>
<td>(6)</td>
<td>TwoBuyer</td>
<td><code>TwoBuyer.scr</code></td>
<td><code>twobuyer</code></td>
<td><code>two_buyer/src</code></td>
</tr>
<tr>
<td>(7)</td>
<td>TravelAgency</td>
<td><code>TravelAgency.scr</code></td>
<td><code>travel_agency</code></td>
<td><code>travel_agency/src</code></td>
</tr>
<tr>
<td>(8)</td>
<td>OnlineWallet</td>
<td><code>OnlineWallet.scr</code></td>
<td><code>online_wallet</code></td>
<td><code>online_wallet/src</code></td>
</tr>
</table>

- The protocol files are found in
  `scribble-java/scribble-gt-demos/scribble`.
- The dir/application name is both:
  - The name of the subdirectory containing the example Erlang implementation,
    under `scribble-java/scribble-gt-demos/erlang`.
  - The name to supply as an argument to `application:start` to run the
    application inside the Erlang shell (cf. [3.3](#COMMANDS)]).
- The Erlang code source dirs are specified for completeness.  They are under
  `scribble-java/scribble-gt-demos/erlang`.

As mentioned in the paper (l.961), our example Erlang programs are *"minimal
but functional skeleton programs for each role of each example to test the
runtime I/O and event handling dynamics, and embedded MC mechanisms such as
stale message purging. However, our minimal implementations do not necessarily
perform the full application logic intended of each example."*

See [3.3](#COMMANDS) for the command line instructions for running each
example.

---

### Expected outputs.

The following briefly summarizes the expected output of each example in the
table from above.  

**Note:** the implementation uses random to select one branch or another, so the exact interleaving of some send/receive events may vary. Each trace in the table shows one valid execution; your console output should follow the same pattern of role-to-role messages but may differ in ordering or timing.


- &#8203;(1) **Calculator**

  This example demonstrates a distributed multiparty calculator protocol with branching, recursion, mixed choice, and garbage collection of stale messages.

    **Expected output (abridged)**.
    ```
    srv: Initializing with callback module srv
    srv initialized 
    *DBG* srv enter in state s1
    *DBG* srv consume internal init_state in state s1
    alice: Initializing with callback module alice
    alice initialized 
    *DBG* alice enter in state s4
    *DBG* alice consume internal init_state in state s4
    carol: Initializing with callback module carol
    carol initialized 
    *DBG* carol enter in state s1
    *DBG* carol receive internal {first} in state s1
    *DBG* carol consume internal init_state in state s1
    Carol: s1 Sending first to Srv 
    *DBG* carol receive internal {second} in state s1
    *DBG* srv receive cast {<0.145.0>,{first,1}} in state s1
    ```

- &#8203;(2) **CircuitBreaker**

  **Expected output (abridged)**.

    ```
    controller: Initializing with callback module controller
    controller initialized 
    *DBG* controller enter in state s1
    *DBG* controller receive internal {start_storage} in state s1
    *DBG* controller consume internal init_state in state s1
    api: Initializing with callback module api
    api initialized 
    *DBG* api enter in state s1
    *DBG* api consume internal init_state in state s1
    storage: Initializing with callback module storage
    storage initialized 
    *DBG* storage enter in state s1
    ```

- &#8203; (3) **DistributedLogging**
  
  **Expected output (abridged)**.
    ```
    controller: Initializing with callback module controller
    controller initialized 
    API: logs is not available yet. Will retry...
    logs: Initializing with callback module logs
    logs initialized 
    Controller: s1 Sending start_logging to Logs 
    Logs: s1 Received start_logging 0 from Controller <0.142.0>
    Controller: s9 Sending timeout to Logs 
    Logs: s9 Sending log_failure to Controller 
    Logs: s13 Received timeout from Controller <0.142.0>
    Logs: s5 Sending ack to Controller 
    Controller: s6 Sending restart to Logs 
    Logs: s6 Received restart 1 from Controller <0.142.0>
    Logs: s9 Sending log_success to Controller 
    Controller: s9 Received log_success 1 from Logs 
    ```

  5.  **Stop**: `application:stop(distributed_logging).` then `q().`.

- &#8203; (4) **Fibonacci**

  **Expected output (abridged)**.

    ```
      a: Initializing with callback module a
      b is not available yet. Will retry...
      a initialized 
      B: Initializing with callback module b
      b initialized 
      A: s5 Sending fibonacci to b 1
      *DBG* b enter in state s5
      *DBG* b receive internal {error} in state s5
      *DBG* b consume internal init_state in state s5
      *DBG* b consume internal {error} in state s5
      *DBG* b receive cast {<0.142.0>,{fibonacci,1},1} in state s5
      *DBG* b receive internal {fibonacci} in state s5
      *DBG* b consume cast {<0.142.0>,{fibonacci,1},1} in state s5 => s6
      B: s6 Sending fibonacci 1 to a
      A: s6 Received 1, next is 2
      A: s5 Sending fibonacci to b 2
    ```

- &#8203; (5) **SMTP**
    **Expected output (abridged)**.

    ```
    s: Initializing with callback module s
    s initialized 
    *DBG* s enter in state s1
    *DBG* s receive internal {'220'} in state s1
    *DBG* s consume internal init_state in state s1
    s connected 
    c is not available yet. Will retry...
    c: Initializing with callback module client
    c initialized 
    *DBG* client enter in state s1
    *DBG* client consume internal init_state in state s1
    S: s1 Sending 220 to C 
    *DBG* client receive cast {<0.142.0>,{'220'}} in state s1
    *DBG* s receive internal {'Timeout'} in state s1
    c connected 
    C: s1 Connected to S <0.142.0> 
    *DBG* s consume internal {'220'} in state s1 => s5
    ```

- &#8203; (6) **TwoBuyer**
    **Expected output (abridged)**.

    ```
    alice: Initializing with callback module alice
    alice initialized 
    *DBG* alice enter in state s4
    *DBG* alice receive internal {request_title} in state s4
    *DBG* alice consume internal init_state in state s4
    alice connected 
    seller is not available yet. Will retry...
    bob: Initializing with callback module bob
    bob initialized 
    *DBG* bob enter in state s4
    *DBG* bob consume internal init_state in state s4
    seller: Initializing with callback module seller
    seller initialized 
    *DBG* seller enter in state s5
    ```

- &#8203; (7) **TravelAgency**
    **Expected output (abridged)**.

    ```
    client: Initializing with callback module client
    client initialized 
    *DBG* client enter in state s3
    *DBG* client receive internal {booking_request} in state s3
    *DBG* client consume internal init_state in state s3
    client connected 
    agency is not available yet. Will retry...
    supplier: Initializing with callback module supplier
    supplier initialized 
    *DBG* supplier enter in state s5
    *DBG* supplier consume internal init_state in state s5
    agency: Initializing with callback module agency
    agency initialized 
    *DBG* agency enter in state s3
    ```

- &#8203; (8) **OnlineWallet**

  **Expected output (abridged)**.
  ```
  a: Initializing with callback module a
  a initialized 
  *DBG* a enter in state s1
  *DBG* a consume internal init_state in state s1
  c: Initializing with callback module client
  c initialized 
  *DBG* client enter in state s1
  *DBG* client receive internal {login} in state s1
  *DBG* client consume internal init_state in state s1
  C: s1 Sending login to A 
  c connected 
  s is not available yet. Will retry...
  s: Initializing with callback module s
  s initialized 
  *DBG* s enter in state s1
  ```



---

### 4.1.2. <a name="RABBITMQ"></a> RabbitMQ Use Case

This section describes how to run the RabbitMQ case study mentioned in the paper. This example demonstrates the framework on a more complex, real-world protocol.

For the RabbitMQ case study, we model the protocol followed by `amqp_selective_consumer` in `scribble-gt-demos/scribble`. 
We generate the `gen_amqp_selective_consumer.erl` and `amqp_selective_consumer.erl` modules, fully implement them, then swap them into `rabbitmq-server/deps/amqp-client/src/`. 
To test run `make` in `scribble-gt-demos/erlang/rabbitmq-server`. This will build the server and run its tests.The build should complete without errors.
---

## 4.2. <a name="ADDITIONAL"></a> Additional info about the examples

Table 1 in the paper has a few info columns that can be checked against the
examples in the artifact if the reader wishes.
We give some quick pointers for doing so as appropriate for each example:

- **M** -- Multiparty, here informally meaning more than two roles, cf. binary (two-party) session types.  (Of course, formally a binary session type is also an MST.)
  - Scribble: more than two roles.
  - Erlang: separate `gen_statem` processes/modules for each role (e.g., `gen_<role>.erl`, `<role>.erl`).
- **B/S** -- Branch/Select: protocol can proceed one out of a set of paths.
  - Scribble: A `choice at <role> ...` statement.
  - Erlang: callback functions in the `gen_<role>.erl`, `<role>.erl` modules that implement each branch label mapping to different state transitions.
- **Rec** -- Recursion: protocol can repeat.
  - Scribble: `rec X { ... }` and `continue X` statements.
  - Erlang: state functions loop in `<role>.erl` via `gen_<role>.erl` callbacks invoking the same state upon `continue` events.
- **MC** -- Mixed-choice.
  - Scribble: `mixed { ... } or A -> B { ... }` statements.
  - Erlang: mixed-choice states encoded in the generic behaviour module with a mixed-choice counter; fresh messages (matching the counter) drive `gen_statem` transitions.
- **nMC** -- non-directed MC.
  - Scribble: Mixed-choices of, e.g., the following form:
    ```
    mixed {
        1() from A to B;
        2() from C to B;  // *
        3() from B to A;
        4() from B to C;
    } or A -> B {
        5() from B to A;
        6() from B to C;
    }
    ```
    Due to multiparty asynchrony, the marked line can correspond to a
    situation where `C` is sending to `B` on the LHS while `B` is sending
    to `A` on the RHS.
  - Erlang:  same `gen_statem` encoding, but without directed commit triggers; the first valid message commits and stale ones are purged.
- **GC** -- garbage collection (stale message purging).
  - Scribble: inherent to every mixed-choice in our asynchronous MST.  E.g., 
    ```
    mixed {
        1() from A to B;
        2() from B to A;
    } or A -> B {
        3() from B to A;
    }
    ```
    While `B` sends message `3`, `A` may be concurrently sending message `1` -- message
    `1` will be implicitly purged by `B`'s runtime.
  - Erlang: generic behaviour module compares per-message counters against state data and discards stale messages before dispatch.












---

---

# <a name="REUSABILITY"></a> 5. Resusability guide

## 5.1. <a name="TUTORIAL"></a> TUTORIAL

Overview of how to write your own example.

#### Protocol specification.

We write a simple HelloWorld protocol.

```
    module Hello;  // Must match the file name, i.e., Hello.scr
    
    data <erlang> "string()" from "erlang" as mystring;
    data <erlang> "integer()" from "erlang" as myint;
    
    global protocol Proto1(role A, role B) {
        mixed {
            Hello(mystring) from A to B;
            World(myint) from B to A;
        } or A -> B  {
            Sorry(mystring) from B to A;
        }
    }
```

- The protocol name is `Proto1`.
- It has two roles `A` and `B`.
- In the statement:
  ```
  Hello(mystring) from A to B;
  ```
  - `Hello` is the message label (or "operator").
  - `mystring` is a message payload type.
    - The `data` statements at the top define the underlying Erlang types.
  - The message is sent by `A` and received by `B`.
- The statement
  ```
  mixed {
      ...
  } or A -> B {
      ...
  }
  ```
  is our construct for mixed choice (MC).  Based on the theory in our paper,
  it specifies `B` as the "observer" of the MC.

Other protocol constructs are inherited from based Scribble and can be seen by
example.  E.g., Fibonacci is a simple example that demonstrates in addition
to mixed-choices:

- Protocol branches, e.g.,
  ```
  choice at a {
      fibonacci(Num) from a to b;
      ...
  } or {
      stop() from a to b;
      ...
  }
  ```
- Recursive protocols, e.g.,
  ```
  rec fib {
      ...
          continue fib;
      ...
  }
  ```


#### Erlang implementation.

The generated Erlang code consists of two modules for each role:
- `gen_<role>.erl`: A generic behaviour module that enforces the protocol.
- `<role>.erl`: A template callback module for the application logic.

Once you’ve generated the `gen_<role>.erl` and `<role>.erl` modules for your protocol, 
copy them into your application’s `src/` directory (e.g. `cp ~/scribble-java/generated/MyProto/*.erl application/src/`). 
Then:
1. Implement callbacks. Open each `<role>.erl` file and fill in the callback stubs (`s1`, ..., `sn`, `make_choice_`, etc.) 
with application logic. These callbacks will be invoked by the `gen_<role>` behaviour whenever messages arrive or internal 
events fire.  Use `io:format/2` or Erlang’s logger to trace role interactions.
2. Wire into OTP supervision tree. Next, wire each `<role>` module into your OTP supervision tree. 
In your `my_app_app.erl` (or equivalent), start each `<role>` as a supervised worker. 
3. Compile and run.
  ```
  rebar3 compile
  rebar3 shell
  application:start(my_app).
  ```
For a practical walkthrough of OTP application structure and supervision trees, see the “Building OTP Applications” chapter 
on Learn You Some Erlang: https://learnyousomeerlang.com/otp-applications



