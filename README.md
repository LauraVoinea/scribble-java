
---

**OOPSLA 24/25 > Paper #672 > Artifact TODO > Overview**

# Mixed-Choice, Asynchronous Multiparty Session Types

ANONYMOUS AUTHORS

<!--
- a brief Introduction,
- a Hardware Dependencies section,
- a Getting Started Guide, and
- Step-by-Step Instructions for how you propose to evaluate the functionality of your artifact (with appropriate connections to the relevant sections of your paper).
- a Reusability Guide for how you propose to evaluate the reusability of your artifact.
-->


---

# 1. <a name="INTRO"></a> Introduction

> *In the Introduction, briefly explain the purpose of the artifact and how it supports the paper. We recommend listing all claims in the paper and stating whether or not each is supported. For supported claims, say how the artifact provides support. For unsupported claims, explain why they are omitted.*

## 1.1. Contents

The artifact archive `paper672.zip` contains:

- An **Overview** of the artifact (i.e., this document) in three formats:
    - html+css: `paper672-overview.html` with `pandoc.css` -- recommended for clickable links;
    - markdown: `paper672-overview.md` -- links clickable depending on markdown viewer app;
    - pdf: `paper672-overview.pdf`.
- The **main artifact** as a **Docker image**: `oopsla2425-paper672-artifactTODO.tar.gz`
    - The Docker image contains the mixed-choice Scribble toolchain (protocol validation and Erlang `gen_statem` code generation), a preconfigured Erlang/OTP environment with `rebar3`, and all example protocols plus their generated Erlang code under `scribble-gt-demos`.
- The original submission version of our **paper**: `oopsla2425-paper672.pdf`

Following the Call for Artifacts, this Overview has the following sections:

- [1. Introduction](#INTRO)
- [2. Hardware dependencies](#HARDWARE)
- [3. Getting started guide](#START)
- [4. Step-by-step instructions](#STEPS)
- [5. Reusability guide](#REUSABILITY)
- Appendix
  - [A.1. TODO](#EXTRA) -- TODO


---

## 1.2. <a name="PURPOSE"></a> Purpose: A toolchain for specifying and implementing Erlang `gen_statem` programs using Multiparty Session Types with mixed-choice

This artifact demonstrates the prototype toolchain presented in the
submitted paper.

As described in the paper (mainly Secs. 2.2 and 5), a programmer follows two
main steps:

1. **Specify the mixed-choice protocol using our extension of Scribble.**  
   Our toolchain will statically validate the syntactic conditions
   for well-formedness and generate:
   - ...Protocol- and role-specific Erlang `gen_statem` APIs **TODO**
   - ...Template Erlang `gen_statem` programs that implement each role
     **TODO**
2. **Adapt and complete the generated template programs.**  
The developer completes the program by:
    -   Filling in the placeholder functions in the template (`<role>.erl`) with the specific logic for the application.
    -   Configuring how the different roles (now running as independent components) will be launched and connected at startup.
        -   Launching a process for each role in the protocol.
        -   Distributing the initial contact information (process IDs) so the roles know how to communicate with each other.
        -   Supervising these processes to provide fault-tolerance, automatically restarting any component that might fail.

See [A.2.](#YOURWAY) for a tutorial with concrete illustration of these steps **TODO**.  


The main safety guarantees:

1.  **Static Protocol Validation:** Before generating any code, the tool validates the input Scribble protocol against syntactic approximations of properties from our theory (e.g., awareness, balance, projectability).

2.  **Correct-by-Construction `gen_statem` Scaffolding:** For valid protocols, the tool generates protocol-specific Erlang `gen_statem` behaviour and callback modules. This scaffolding enforces the protocol for each role.

3.  **Runtime Safety for Mixed-Choice:** The generated code tracks mixed-choice instances and **purges stale messages** from other branches, preventing them from causing protocol violations.

4.  **Static Analysis:** The generated code also includes type specifications that can be used with tools such as Dialyzer, allowing some programming errors to be caught statically. This is complemented by runtime checks that leverage Erlang's standard failure-handling mechanisms for robustness.

---

## 1.3. <a name="CLAIMS"></a> Claims

<table style="border: 1px solid black;">
<tr><td>
<b>NOTE</b>: Our paper is subject to <b>Major Revisions</b>.  Following the Chair's
instructions, we are submitting this initial artifact now, and will submit a
revised artifact later along with the revised paper.  Possible changes:

- The implementation of static protocol validation may change as a result of
  changes to the theory to ensure the property of orphan message freedom (as
  requested by the reviewers).
- The implementation of runtime purging of stale messages may similarly change
  to match any changes to the theory.
- Examples will be adjusted if impacted by the above (but not expected).
</td></tr>
</table>

The main statement in the submitted paper (l.1067): (Apologies, we did not
format it as an explicit "Data-Availability Statement" section.)

> **<em>We will submit our implementation, examples and RabbitMQ case study as an artifact.</em>**

...enumerate these and other claims from the paper related to our toolchain and examples.

Claims:

- Submit
  - Implementation source -- SUPPORTED
    - Java Scribble
      - Protocol validation
      - Code gen (including EFSMs)
      - Erlang runtime
  - Examples source -- SUPPORTED
    - Erlang examples
      - Rabbit MQ -- SUPPORTED
- syntactic WF -- SUPPORTED
- running examples -- SUPPORTED
- ...TODO

Summary of differences: **TODO**

- ...code gen changed?
- ...minor Scribble notation differences




---

---

# 2. <a name="HARDWARE"></a> Hardware dependencies

<!--
In the Hardware Dependencies section, describe the hardware required to evaluate the artifact. If the artifact requires specific hardware (e.g., many cores, disk space, GPUs, specific processors), please provide instructions on how to gain access to the hardware. Keep in mind that reviewers must remain anonymous.
-->

Our artifact does not require specific hardware.


---

---

# 3. <a name="START"></a> Getting started guide

> *In the Getting Started Guide, give instructions for setup and basic testing. List any software requirements and/or passwords needed to access the artifact. The instructions should take roughly 30 minutes to complete. Reviewers will follow the guide during an initial kick-the-tires phase and report issues as they arise.*
>
> *The Getting Started Guide should be as simple as possible, and yet it should stress the key elements of your artifact. Anyone who has followed the Getting Started Guide should have no technical difficulties with the rest of your artifact.*


## 2.1. Initial set up and test


**Prerequisites**.

- Docker is installed and running – e.g., see this
  [tutorial](https://docs.docker.com/get-started/) .

**Notes** on the artifact.

- The artifact Docker image has `vim` and `nano` preinstalled.
- This docker image has been tested on following environment as the host machine:
  - MacOS 13.4.1, on a MacBook Pro with 1.4 GHz Quad-Core Intel Core i5 and 16GB RAM
  - Manjaro Linux laptop with Intel i7-1185G7 @ 3.00GHz and 32 GB RAM

...Once inside the container shell you can:

Run all examples: 

```
./mMST.sh -run_all_gt_examples
```

...This should run scribble on all examples under scribble-gt-demos/scribble. 

...

**Steps** for starting from scratch.

1. Put the Docker image `oopsla23-paper437-artifact56-docker.tar.gz` in some local directory.  
   In these steps, we will refer to this directory as `$MY_LOCAL_DIR`.

2. Check that Docker is running.

3. Load the image and launch a container with an interactive session.  
   In `$MY_LOCAL_DIR`, do:
   ```sh
   docker load -i oopsla23-paper437-artifact56-docker.tar.gz
   docker run --rm -it rbst
   ```

4. **"Kick-the-tires" Test: Validate all protocols and generate code.**
    The simplest way to check that the toolchain is working is to run the main script to process all example protocols. This will validate each protocol and generate the corresponding Erlang code.

    Inside the container shell, run:
    ```sh
    ./mMST.sh -run-all-gt-examples
    ```
    **Expected output**.
    The script will loop through all `.scr` files in `scribble-gt-demos/scribble/`, validating and projecting each of them. It should complete without errors.
   The generated Erlang code will be placed under `generated/<protocol_name>`

   To validate one example at a time: 
   **TODO:**

5. **Walkthrough: Compile and run a single example.**
    Let's run the `CircuitBreaker` example. The Erlang code is already generated in the image, so we just need to compile and run it.

    Inside the container, navigate to the example's directory:
    ```sh
    cd scribble-gt-demos/erlang/circuit_breaker/
    ```
    Now, launch an Erlang shell using `rebar3`:
    ```sh
    rebar3 shell
    ```
    Once the shell starts (you'll see a `===> Verifying dependencies...` message followed by an Erlang prompt `1>`), start the application:
    ```rebar3
    application:start(circuit_breaker).
    ```
    **Expected output**.
    You will see log messages from the different roles (API, Controller, Storage, User) as they interact according to the protocol.
    ```
    ok
    B: s1 Sending start_storage to Storage 
    B: s3 Sending start_controller to API 
    A: s1 Sending start_user to User 
    ...
    ```

6.  **Stop the example.**
    First, stop the application:
    ```rebar3
    application:stop(circuit_breaker).
    ```
    Then, you can exit the `rebar3` shell by pressing `Ctrl+C` twice, or by typing `q().` and pressing Enter.




---

## 2.2. Main directories/commands in the artifact image
<table border-collapse="collapse">
<tr>
<td width=25%><strong>Component</strong></td>
<td width=25%><strong>Description</strong></td>
<td width=50%><strong>Path</strong> inside Docker image</td>
</tr>
<tr>
<td>Toolchain</td>
<td>Base directory</td>
<td><code>/root/scribble-java</code></td>
</tr>
<tr>
<td></td>
<td>Toolchain script</td>
<td><code>/root/scribble-java/mMST.sh</code></td>
</tr>
<tr>
<td></td>
<td>Toolchain source</td>
<td><code>/root/scribble-java/scribble-*</code></td>
</tr>
<tr>
<td>Examples</td>
<td>Protocol specifications</td>
<td><code>/root/scribble-java/scribble-gt-demos/scribble/</code></td>
</tr>
<tr>
<td></td>
<td>Erlang implementations</td>
<td><code>/root/scribble-java/scribble-gt-demos/erlang/</code></td>
</tr>
</table>

**Notes** on compiling and running examples.

-   All Erlang examples are self-contained `rebar3` projects.
-   To run an example, first `cd` into its directory (e.g., `cd scribble-gt-demos/erlang/circuit_breaker/`).
-   Use `rebar3 shell` to compile the project and start an interactive Erlang shell.
-   Inside the shell, use `application:start(example_name).` to run the application.


---

---

# 4. <a name="STEPS"></a> Step-by-step instructions

> *In the Step by Step Instructions, explain how to reproduce any experiments or other activities that support the conclusions in your paper. Write this for readers who have a deep interest in your work and are studying it to improve it or compare against it. If your artifact runs for more than a few minutes, point this out, note how long it is expected to run (roughly) and explain how to run it on smaller inputs. Reviewers may choose to run on smaller inputs or larger inputs depending on available resources.*
>
> *Be sure to explain the **expected outputs** produced by the Step by Step Instructions. State where to find the outputs and how to interpret them relative to the paper. If there are any expected warnings or error messages, explain those as well. Ideally, artifacts should include sample outputs and logs for comparison.*

This section lists the steps for demonstrating the claims from the paper
regarding our Rust framework.
The claims are organized as follows:

- <a href="#TODO">4.1.</a> **Compiling and running the examples** referred to in the paper.
- <a href="#TODO">4.2.</a> **Additional features** supported by the framework for TODO if applicable
- <a href="#TODO">4.3.</a> Overview of ...code generation TODO



---

## 4.1. <a name="EXAMPLES"></a> Compiling and running the examples referred to in the paper

- <a href="#TODO">4.1.1.</a> **Examples using mixed-choice**
- <a href="#TODO">4.1.2.</a> **Rabbit MQ** use case.

---

See TODO for the command line instructions for running each example.

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

### 4.1.1. <a name="TABLE1"></a> Example application protocols from our target domains

- Claim from the paper
  
  > 1007: *Table 1 lists a series of application protocols covering various
  > features of our framework, including a small collection of reusable DSP
  > building blocks used to implement them.*

- ...do existing impls exist? different than generated?

The table summarises the examples from Tab. 1 in the paper:

- all mocks

<table border-collapse="collapse">
<tr>
<td width=3%></td>
<td width=37%><strong>Example</strong> (from Table 1 in the paper)</td>
<td width=14%><strong>Directory</strong></td>
<td width=18%><strong>.scr</strong></td>
<td width=28%><strong>Source code file?</strong></td>
</tr>
<tr>
<td style="border-bottom: 1px solid black"></td>
<td style="border-bottom: 1px solid black">with references where appropriate</td>
<td style="border-bottom: 1px solid black">...</td>
<td style="border-bottom: 1px solid black">...</td>
<td style="border-bottom: 1px solid black">in <code>...</code></td>
</tr>
<tr>
<td>(1)</td>
<td>Calculator <a href="https://www.TODO">[Hu and Yoshida, 2016]</a></td>
<td><code>scribble-gt-demos/scribble</code></td>
<td>Calculator.scr</td>
<td><code>scribble-gt-demos/erlang/calculator/src</code></td>
</tr>
<tr>
<td>(2)</td>
<td>CircuitBreaker<a href="https://arxiv.org/pdf/2204.13464"> [Lagaillardie, Neykova and
Yoshida 2022]</a></td>
<td><code>scribble-gt-demos/scribble</code></td>
<td>CircuitBreaker.scr</td>
<td><code>scribble-gt-demos/erlang/circuit_breaker/src</code></td>
</tr>
<tr>
<td>(3)</td>
<td>DistributedLogging <a href="https://arxiv.org/pdf/2204.13464"> [Lagaillardie, Neykova and
Yoshida 2022]</a></td>
<td><code>scribble-gt-demos/scribble<code></td>
<td>DistributedLogging.scr</td>
<td><code>scribble-gt-demos/erlang/distributed_logging/src<code></td>
</tr>
<tr>
<td>(4)</td>
<td>Fibonacci <a href="https://TODO">[Hu and Yoshida, 2016]</a></td>
<td><code>scribble-gt-demos/scribble<code></td>
<td>Fibonacci.scr</td>
<td><code>scribble-gt-demos/erlang/fibonacci/src</code></td>
</tr>
<tr>
<td>(5)</td>
<td>SMTP <a href="https://TODO">[Hu and Yoshida, 2016]</a></td>
<td><code>scribble-gt-demos/scribble<code></td>
<td>SMTP.scr</td>
<td><code>scribble-gt-demos/erlang/SMTP/src<code></td>
</tr>
<tr>
<td>(6)</td>
<td>TwoBuyer <a href="https://TODO">[Honda, Yoshida and Carbone 2008]</a></td>
<td><code>scribble-gt-demos/scribble<code></td>
<td>TwoBuyer.scr</td>
<td><code>scribble-gt-demos/erlang/two_buyer/src</code></td>
</tr>
<tr>
<td>(7)</td>
<td>TravelAgency <a href="https://TODO">[Hu, Yoshida and Honda, 2008]</a></td>
<td><code>scribble-gt-demos/scribble<code></td>
<td>TravelAgency.scr</td>
<td><code>scribble-gt-demos/erlang/travel_agency/src</code></td>
</tr>
<tr>
<td>(8)</td>
<td>OnlineWallet <a href="https://TODO">[Neykova, Yoshida and Hu, 2013]</a></td>
<td><code>scribble-gt-demos/scribble<code></td>
<td>OnlineWallet.scr</td>
<td><code>scribble-gt-demos/erlang/online_wallet/src</code></td>
</tr>
</table>

Table 1 in the paper has a few info columns that can be checked against the
examples in the artifact if the reader wishes.
We give some general pointers for doing so for each example as appropriate:

- **M** -- Multiparty, here informally meaning more than two roles, cf. binary (two-party) session types.  (Of course, formally a binary session type is also an MST.)
  - Scribble: more than two roles.
  - Erlang: separate `gen_statem` processes/modules for each role (e.g., `gen_<role>.erl`, `<role>.erl`).
- **B/S** -- Branch/Select: protocol can proceed one out of a set of paths.
  - Scribble: choice at some role.
  - Erlang: callback functions in the `gen_<role>.erl`, `<role>.erl` modules that implement each branch label mapping to different state transitions.
- **Rec** -- Recursion: protocol can repeat.
  - Scribble: rec X and continue X.
  - Erlang: state functions loop in `<role>.erl` via `gen_<role>.erl` callbacks invoking the same state upon `continue` events.
- **MC** -- Mixed-choice.
  - Scribble: mixed { ... } -- cf. formal notation.
  - Erlang: mixed-choice states encoded in the generic behaviour module with a mixed-choice counter; fresh messages (matching the counter) drive `gen_statem` transitions.
- **nMC** -- non-directed MC.
  - Scribble: MC where during execution ... TODO
  - Erlang:  same `gen_statem` encoding, but without directed commit triggers; the first valid message commits and stale ones are purged.
- **GC** -- garbage collection (stale message purging).
  - Scribble: N/A
  - Erlang:  generic behaviour module compares per-message counters against state data and discards stale messages before dispatch.




---

#### Examples from Table 1 in the paper.

The following briefly summarizes the expected output of each example in the
earlier table.  

- &#8203;(1) **Calculator**

  This example demonstrates a basic multiparty protocol with recursion.

    **Instructions:**
    1. Navigate to the example directory: `cd scribble-gt-demos/erlang/calculator/`
    2. Start the interactive shell: `rebar3 shell`
    3. Inside the shell, start the application: `application:start(calculator).`
    **Expected output**.
    TODO

    ```sh
    TODO
    ...
    ```

- &#8203;(2) **CircuitBreaker**
  This example demonstrates a **mixed-choice (MC)**. The `API` role observes the `Controller`. If the `Controller` sends `trip`, the circuit breaks. Otherwise, the `User` can continue making requests. This also demonstrates **garbage collection (GC)** of stale messages.

    **Expected output**.
    TODO

    ```sh
    TODO
    ...
    ```

  - TODO




- &#8203; (3) **DistributedLogging**
  `Source` processes send log entries to a central `Logger`, which forwards them to a `Collector`.

  1.  **Directory**: `cd /root/scribble-java/scribble-gt-demos/erlang/distributed_logging/`
  2.  **Shell**: `rebar3 shell`
  3.  **Run**: `application:start(distributed_logging).`
  4.  **Expected output**: A log message is sent, acknowledged, and collected.
      ```
      ok
      L: s1 Sending start_source to Source
      S: s1 Sending log("hello") to Logger
      L: s3 Sending ack() to Source
      S: s3 Received ack
      L: s3 Sending collect("hello") to Collector
      C: s1 Received log: "hello"
      ```
  5.  **Stop**: `application:stop(distributed_logging).` then `q().`.

- &#8203; (4) **Fibonacci**
  `Source` processes send log entries to a central `Logger`, which forwards them to a `Collector`.

  1.  **Directory**: `cd /root/scribble-java/scribble-gt-demos/erlang/fibonacci/`
  2.  **Shell**: `rebar3 shell`
  3.  **Run**: `application:start(fibonacci).`
  4.  **Expected output**: A log message is sent, acknowledged, and collected.
      ```
      ok
      L: s1 Sending start_source to Source
      S: s1 Sending log("hello") to Logger
      L: s3 Sending ack() to Source
      S: s3 Received ack
      L: s3 Sending collect("hello") to Collector
      C: s1 Received log: "hello"
      ```
  5.  **Stop**: `application:stop(fibonacci).` then `q().`.

- &#8203; (5) **OnlineWallet**
`Source` processes send log entries to a central `Logger`, which forwards them to a `Collector`.

  1.  **Directory**: `cd /root/scribble-java/scribble-gt-demos/erlang/online_wallet/`
  2.  **Shell**: `rebar3 shell`
  3.  **Run**: `application:start(online_wallet).`
  4.  **Expected output**: A log message is sent, acknowledged, and collected.
      ```
      ok
      L: s1 Sending start_source to Source
      S: s1 Sending log("hello") to Logger
      L: s3 Sending ack() to Source
      S: s3 Received ack
      L: s3 Sending collect("hello") to Collector
      C: s1 Received log: "hello"
      ```
  5.  **Stop**: `application:stop(online_wallet).` then `q().`.


- &#8203; (6) **SMTP**
  `Source` processes send log entries to a central `Logger`, which forwards them to a `Collector`.

  1.  **Directory**: `cd /root/scribble-java/scribble-gt-demos/erlang/smtp/`
  2.  **Shell**: `rebar3 shell`
  3.  **Run**: `application:start(smtp).`
  4.  **Expected output**: A log message is sent, acknowledged, and collected.
      ```
      ok
      L: s1 Sending start_source to Source
      S: s1 Sending log("hello") to Logger
      L: s3 Sending ack() to Source
      S: s3 Received ack
      L: s3 Sending collect("hello") to Collector
      C: s1 Received log: "hello"
      ```
  5.  **Stop**: `application:stop(smtp).` then `q().`.

- &#8203; (7) **TwoBuyer**
  `Source` processes send log entries to a central `Logger`, which forwards them to a `Collector`.

  1.  **Directory**: `cd /root/scribble-java/scribble-gt-demos/erlang/two_buyer/`
  2.  **Shell**: `rebar3 shell`
  3.  **Run**: `application:start(two_buyer).`
  4.  **Expected output**: A log message is sent, acknowledged, and collected.
      ```
      ok
      L: s1 Sending start_source to Source
      S: s1 Sending log("hello") to Logger
      L: s3 Sending ack() to Source
      S: s3 Received ack
      L: s3 Sending collect("hello") to Collector
      C: s1 Received log: "hello"
      ```
5.  **Stop**: `application:stop(two_buyer).` then `q().`.

- &#8203; (7) **TravelAgency**
  `Source` processes send log entries to a central `Logger`, which forwards them to a `Collector`.

  1.  **Directory**: `cd /root/scribble-java/scribble-gt-demos/erlang/travel_agency/`
  2.  **Shell**: `rebar3 shell`
  3.  **Run**: `application:start(travel_agency).`
  4.  **Expected output**: A log message is sent, acknowledged, and collected.
      ```
      ok
      L: s1 Sending start_source to Source
      S: s1 Sending log("hello") to Logger
      L: s3 Sending ack() to Source
      S: s3 Received ack
      L: s3 Sending collect("hello") to Collector
      C: s1 Received log: "hello"
      ```
5.  **Stop**: `application:stop(travel_agency).` then `q().`.


---

### 4.1.2. <a name="TODO"></a> Rabbit MQ Use Case

- Claim from the paper:
  
  > TODO

This section describes how to run the RabbitMQ case study mentioned in the paper. This example demonstrates the framework on a more complex, real-world protocol.











---

## 4.2. <a name="TODO"></a> Additional features supported by the framework for TODO if applicable

- Claim from the paper:
  
  > TODO

TODO


---

## 4.3. <a name="TODO"></a> Overview of ...code generation TODO

- Claims from paper
  
  > TODO

The purpose of this section is to give an overview of TODO

- <a href="#TODO">4.3.1.</a> TODO
- <a href="#TODO">4.3.2.</a> TODO


---

### 4.3.1. <a name="OUTLINE"></a> Outline of key aspects

TODO











---

---

# <a name="REUSABILITY"></a> 5. Resusability guide

In the Reusability Guide, explain which parts of your artifact constitute the core pieces which should be evaluated for reusability. Explain how to adapt the artifact to new inputs or new use cases. Provide instructions for how to find/generate/read documentation about the core artifact. Articulate any limitations to the artifact’s reusability.

- Claim from paper:
  
  > 851: *We present an implementation of our framework as a Rust crate.*

The source code for our framework is in `/root/rust_pst/src`. The `lib.rs` file in this directory defines the "public" interface of our framework to export as a crate (i.e., Rust library) for external users.

[A.2.](#YOURWAY) gives a tutorial on using our framework as a Rust crate to
implement a rate-based message passing system.







---

---

# <a name="APPENDIX"></a> Appendix

## <a name="EXTRA"></a> A.1. Additional examples

The artifact includes some additional examples to those referred to in the
paper.
The table below categorizes and summarizes them.

<table border-collapse="collapse">
<tr>
<td width=5%></td>
<td width=30%></td>
<td width=15%><strong>Tag</strong></td>
<td width=27%><strong>Source code file</strong></td>
</tr>
<tr>
<td style="border-bottom: 1px solid black"></td>
<td style="border-bottom: 1px solid black"><strong>Example</strong></td>
<td style="border-bottom: 1px solid black">for <code>cargo run</code></td>
<td style="border-bottom: 1px solid black">in <code>/root/rust_pst/examples</code></td>
</tr>
<tr>
<td colspan="4"><strong>a) “Hello World” examples of Rate-Compatibility (RC)</strong></td>
</tr>
<tr>
<td style="border-top: 1px solid black">(A1)</td>
<td style="border-top: 1px solid black">Period 1:1</td>
<td style="border-top: 1px solid black"><code>simple</code></td>
<td style="border-top: 1px solid black"><code>simple.rs</code></td>
</tr>
<tr>
<td>(A2)</td>
<td>Period 1:2 (Simple)</td>
<td><code>rate12-simple</code></td>
<td><code>rate12-simple.rs</code></td>
</tr>
<tr>
<td>(A3)</td>
<td>Period 1:2</td>
<td><code>rate12</code></td>
<td><code>rate12.rs</code></td>
</tr>
<tr>
<td>(A4)</td>
<td>Relative period ratio 1:2<sup>&dagger;</sup></td>
<td><code>ratio12</code></td>
<td><code>ratio12.rs</code></td>
</tr>
<tr>
<td style="border-bottom: 1px solid black">(A5)</td>
<td style="border-bottom: 1px solid black">Period 2:3</td>
<td style="border-bottom: 1px solid black"><code>rate23</code></td>
<td style="border-bottom: 1px solid black"><code>rate23.rs<code></td>
</tr>
<tr>
<td colspan="4"><strong>b) An example rate mismatch error</strong> (n.b.
does <strong>not</strong> compile: expected output is a typing error)</td>
</tr>
<tr>
<td style="border-top: 1px solid black; border-bottom: 1px solid black">(A6)</td>
<td style="border-top: 1px solid black; border-bottom: 1px solid black">Rate mismatch</td>
<td style="border-top: 1px solid black; border-bottom: 1px solid black"><code>mismatch</code></td>
<td style="border-top: 1px solid black; border-bottom: 1px solid black"><code>mismatch.rs</code></td>
</tr>
</table>

<sup>&dagger;</sup>This is the same example as in [3.2](#FEATURES); here we use it to demonstrate
a different aspect.

The following briefly summarises the expected output of each example from the above table.





---

#### a) “Hello World” examples of Rate-Compatibility (RC)


- (A1) **Simple 1:1** -- `simple`  
  **Expected output**.
  Compiles successfully. Prints out `42` repeatedly. Terminate with `Ctrl-C`.

  ```sh
  42
  42
  42
  42
  42
  42
  ...
  ```

- ...


























---

---

## A.2. <a name="YOURWAY"></a> Building a TODO

TODO





---

---

## A.3. <a name="VALIDATION"></a> Scribble validation of mixed-choice protocols

> l.866 *it syntactically checks the source protocol for (a) the basic condition regarding message labels (Definition 3.1), (b) awareness (Definition 3.6) and balance (Definition 3.9), and (c) projectability (Section 4.2) onto all roles. In our current implementation, checking (b) syntactically means inferring role occurrences and dependencies between roles from the source protocol as written without, e.g., semantically unfolding recursive types.*

The `mMST.sh` script (cf. **TODO** command line instructions) uses our Scribble
extension to conservatively validate protocols as described.  Our toolchain
aborts if the protocol that cannot be invalidated and it prints an error
message.  (**N.B.** the implementation is set to debugging mode, so it often
prints full stack traces along with the error message; this is intended.)

The validation includes basic syntactic checks, including those of base
Scribble.  We summarise the key checks related specifically to mixed-choice as
developed in this paper:

- **Balanced roles**, cf. Def. 3.9.  Every choice and MC must feature the
  same set of roles in every case.  E.g.,
  - OK: `mixed { 0() from A to B; 1() from B to A(); 2() from B to C(); } () or A->B () { 3() from B to A; 4() from B to C; }`
  - Not OK: `mixed { 0() from A to B; 1() from B to A(); 2() from B to C(); } () or A->B () { 3() from B to A; }`

- **Single-decision**, cf. Def 3.6(1).  In the RHS of every MC, every role
  must strictly depend on the observer.  E.g.,
  - OK: `mixed { 0() from A to B; 1() from B to A(); } () or A->B () { 2() from B to A; }`
  - Not OK: `mixed { 0() from A to B; 1() from B to A(); } () or A->B () { 3() from A to B; 2() from B to A; }`

- **Clear-termination**, cf. Def 3.6(2).  In the LHS of every MC, every role
  must eventually depend on the observer, unless it diverges in the LHS in
  which case at least one of the LHS or RHS must not have any free recursion
  variables.  E.g.,
  - OK: `rec X { mixed { 0() from A to B; 1() from B to A(); continue X; } } () or A->B () { 2() from B to A; }`  **CHECKME**
  - Not OK: `rec X { mixed { 0() from A to B; 1() from B to A(); continue X; } } () or A->B () { 2() from B to A; continue X; }`

- **Message labels**, cf. Sec. 3.1.  Labels of committing messages on the LHS and RHS of every
  MC must be disjoint.  E.g.,
  - OK: `mixed { 0() from A to B; 1() from B to A(); } () or A->B () { 2() from B to A; }`
  - Not OK: `mixed { 0() from A to B; 1() from B to A(); } () or A->B () { 1() from B to A; }`

All of the examples in this artifact satisfy the above conditions.


**FIXME** double check all line numbers using submission version

