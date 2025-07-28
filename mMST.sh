#!/usr/bin/env bash
#
# Copyright 2008 The Scribble Authors
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#


##
# Config:
#
# $SCRIBBLE_HOME
#   Set this to the `scribble-java` root directory.
#   i.e., the directory of the `parent` mvn module (that contains the
#   `scribble-ast`, `scribble-cli`, etc. mvn submodules),
#   or the directory that contains the `lib` directory containing the generated
#   distribution jars.
##

if [ -z "${SCRIBBLE_HOME}" ]; then 
    SCRIBHOME=$(dirname "$0")
else
    SCRIBHOME=${SCRIBBLE_HOME}
fi

# ANTLR 3 runtime jar location.
# Set this to the location of the ANTLR 3 runtime jar, or place the jar in:
# `$SCRIBHOME/lib`.  (This script looks for the ANTLR jar in those locations.)
ANTLR_RUNTIME_JAR=$SCRIBHOME'/scribble-parser/lib/antlr-3.5.2-complete.jar'
  # e.g., '~/.m2/repository/org/antlr/antlr-runtime/3.4/antlr-runtime-3.4.jar'
  #    or '/cygdrive/c/Users/[User]/.m2/repository/org/antlr/antlr-runtime/3.4/antlr-runtime-3.4.jar'
  #        (i.e., the Maven install location)


usage() {
  echo Usage:  'mMST.sh [option]... <PathToScribbleFile> [option]...'
  cat <<EOF

 <PathToScribbleFile>     Source Scribble module (.scr file) containing global protocol(s).

 mMST.sh uses mMST for Erlang code generation.
 By default, for each protocol and role found in <SCRFILE>:
   - Global-local correspondence checking is performed.
   - Erlang modules (.erl, .hrl for callbacks and main module) are generated.
   - Endpoint FSM diagrams (.dot) are generated.
   - Output is placed in './generated/<ProtocolName>/'
     (e.g., ./generated/MyProto/roleA.erl, ./generated/MyProto/roleA.dot).

 Options:
  -h, --help                 Show this info and exit
  -v                         Scribble debug info (verbose console output)
  --verbose                  Echo the java command before execution

  -run-scribble-examples     Validate and generate code for all examples under
                            scribble-gt-demos/scribble/
  -run-erlang-examples       Run compile, start, and stop on all Erlang OTP app examples under
                            scribble-gt-demos/erlang/
  -clean-erlang-examples     Clean only Erlang OTP app examples under
                            scribble-gt-demos/erlang/
  -copy-erlang-demos <ProtocolName>  Copy generated gen_<role>.erl files from generated/<ProtocolName> to scribble-gt-demos/erlang/<ProtocolName>/src
  -copy-all-erlang-demos       Copy gen_*.erl from every generated/<ProtocolName> into scribble-gt-demos/erlang/<protocolName>/src

  -gt-gen-erlang 
            Generate Erlang code.
            ./mMST.sh -gt-gen-erlang <ProtocolName> <PathToScribbleFile>
  -gt-gen-erlang-role
            Generate Erlang code.
            ./mMST.sh -gt-gen-erlang-role <ProtocolName> <Role> <PathToScribbleFile>
  -gt-gen-efsm
            Generate event-based FSMs.
            ./mMST.sh -gt-gen-efsm <ProtocolName> <Role> <PathToScribbleFile>


EOF
}


fixpath() {
    windows=0

    if [ "$(uname | grep -c CYGWIN)" -ne 0 ]; then
        windows=1
    fi

    cp="$1"
    if [ "$windows" = 1 ]; then
        cygpath -pw "$cp"
    else
        echo "$cp"
    fi
}

CLASSPATH="$SCRIBHOME/scribble-ast/target/classes"
CLASSPATH="$CLASSPATH:$SCRIBHOME/scribble-cli/target/classes"
CLASSPATH="$CLASSPATH:$SCRIBHOME/scribble-codegen/target/classes"
CLASSPATH="$CLASSPATH:$SCRIBHOME/scribble-core/target/classes"
CLASSPATH="$CLASSPATH:$SCRIBHOME/scribble-main/target/classes"
CLASSPATH="$CLASSPATH:$SCRIBHOME/scribble-parser/target/classes"
CLASSPATH="$CLASSPATH:$SCRIBHOME/scribble-gt/target/classes"
if test -f "$ANTLR_RUNTIME_JAR"; then
    CLASSPATH="$CLASSPATH:$ANTLR_RUNTIME_JAR"
fi
CLASSPATH="$CLASSPATH:$SCRIBHOME/lib/antlr.jar"
CLASSPATH="$CLASSPATH:$SCRIBHOME/lib/commons-io.jar"
CLASSPATH="$CLASSPATH:$SCRIBHOME/lib/scribble-ast.jar"
CLASSPATH="$CLASSPATH:$SCRIBHOME/lib/scribble-cli.jar"
CLASSPATH="$CLASSPATH:$SCRIBHOME/lib/scribble-core.jar"
CLASSPATH="$CLASSPATH:$SCRIBHOME/lib/scribble-main.jar"
CLASSPATH="$CLASSPATH:$SCRIBHOME/lib/scribble-parser.jar"
CLASSPATH="$CLASSPATH:$SCRIBHOME/lib/scribble-gt.jar"
CLASSPATH="$(fixpath "$CLASSPATH")" # Correctly assign the output of fixpath

usage=0
verbose=0
ARGS="" # Initialize ARGS to empty string
run_scribble_examples=0 # Flag for the new option (formerly run_all_gt_examples)
run_erlang_examples=0  # Flag for running and stopping Erlang OTP apps (formerly run_all_erlang_app_examples)
run_clean_all=0 # Flag for cleaning entire workspace
run_gt_gen_erlang=0 # Flag for Erlang code generation
run_gt_gen_erlang_role=0 # Flag for Erlang role generation
run_gt_gen_efsm=0 # Flag for EFSM generation + dot-to-png conversion
run_clean_erlang=0 # Flag for cleaning Erlang examples only
run_copy_erlang=0 # Flag for copying generated Erlang modules to demos
run_copy_all_erlang=0 # Flag for copying all protocols to Erlang demos

while true; do
    case "$1" in
        "")
            break
            ;;
        -h)
            usage=1
            break
            ;;
        --help)
            usage=1
            break
            ;;
        --verbose)
            verbose=1
            shift
            ;;
        -run-scribble-examples)
            run_scribble_examples=1
            shift # Consume the option
            ;;
        -run-erlang-examples)
            run_erlang_examples=1
            shift
            ;;
        -clean-erlang-examples)
            run_clean_erlang=1
            shift
            ;;
        -copy-erlang-demos)
            run_copy_erlang=1
            PROTOCOL_NAME="$2"
            shift 2
            ;;
        -copy-all-erlang-demos)
            run_copy_all_erlang=1
            shift
            ;;
        -gt-gen-erlang)
            run_gt_gen_erlang=1
            PROTOCOL_NAME="$2"
            SCRFILE="$3"
            shift 3
            ;; # consume Erlang code gen flag
        -gt-gen-erlang-role)
            run_gt_gen_erlang_role=1
            PROTOCOL_NAME="$2"
            ROLE_NAME="$3"
            SCRFILE="$4"
            shift 4
            ;; # consume Erlang role gen flag
        -gt-gen-efsm)
            run_gt_gen_efsm=1
            PROTOCOL_NAME="$2"
            ROLE_NAME="$3"
            SCRFILE="$4"
            shift 4
            ;;  # consume EFSM flag
        -clean-all)
            run_clean_all=1
            shift
            ;;
        *)
            ARGS="${ARGS}${ARGS:+ }$1" # Append argument with a preceding space if ARGS is not empty
            shift
            ;;
    esac
done


if [ "$usage" = 1 ]; then
    usage
    exit 0
elif [ "$run_clean_all" = 1 ]; then
    echo "Cleaning all Maven modules..."
    mvn clean -q
    echo "Removing generated/ directory..."
    rm -rf "$SCRIBHOME/generated"
    echo "Cleaning Erlang example builds..."
    for dir in "$SCRIBHOME/scribble-gt-demos/erlang"/*/; do
        [ -d "$dir" ] || continue
        echo "Rebar3 clean: $dir"
        (cd "$dir" && rebar3 clean)
    done
    exit 0
elif [ "$run_clean_erlang" = 1 ]; then
    ERL_DIR="$SCRIBHOME/scribble-gt-demos/erlang"
    echo "Cleaning Erlang example builds in: $ERL_DIR"
    for dir in "$ERL_DIR"/*/; do
        [ -d "$dir" ] || continue
        echo "Rebar3 clean: $(basename "$dir")"
        (cd "$dir" && rebar3 clean)
    done
    exit 0
elif [ "$run_copy_erlang" = 1 ]; then
    SRC_DIR="$SCRIBHOME/generated/$PROTOCOL_NAME"
    # Convert protocol name to lowercase (portable) to match demo folder
    # Convert CamelCase ProtocolName to snake_case demo folder
    DEMO_NAME=$(printf "%s" "$PROTOCOL_NAME" \
        | sed -E 's/([a-z0-9])([A-Z])/\1_\2/g' \
        | tr '[:upper:]' '[:lower:]')
    DEST_DIR="$SCRIBHOME/scribble-gt-demos/erlang/$DEMO_NAME/src"
    echo "Copying gen_*.erl from $SRC_DIR to $DEST_DIR"
    mkdir -p "$DEST_DIR"
    for file in "$SRC_DIR"/gen_*.erl; do
        echo "Copying $(basename "${file}") from $SRC_DIR to $DEST_DIR"
        cp "$file" "$DEST_DIR"/
    done
    exit 0
elif [ "$run_copy_all_erlang" = 1 ]; then
    for src in "$SCRIBHOME"/generated/*/; do
        proto=${src#"$SCRIBHOME"/generated/}; proto=${proto%/}
        demo=$(printf "%s" "$proto" \
            | sed -E 's/([a-z0-9])([A-Z])/\1_\2/g' \
            | tr '[:upper:]' '[:lower:]')
        DEST_DIR="$SCRIBHOME/scribble-gt-demos/erlang/$demo/src"
        mkdir -p "$DEST_DIR"
        for file in "$SCRIBHOME/generated/$proto"/gen_*.erl; do
            echo "Copying $(basename "${file}") from $SCRIBHOME/generated/$proto to $DEST_DIR"
            cp "$file" "$DEST_DIR"/
        done
    done
    exit 0
fi

CMD="java -cp $CLASSPATH org.scribble.ext.gt.cli.GTCommandLine2"

# Define the main GT invocation function
scribblec() {
    if [ "$verbose" = 1 ]; then
        echo "Executing: $CMD" "$@"
    fi
    eval "$CMD" "$@"
}

## When the batch flag is set, loop through all `.scr` files and process them
if [ "$run_scribble_examples" = 1 ]; then
    EXAMPLES_DIR="$SCRIBHOME/scribble-gt-demos/scribble"
    if [ ! -d "$EXAMPLES_DIR" ]; then
        echo "Error: Examples directory not found: $EXAMPLES_DIR" >&2
        exit 1
    fi
    echo "Running all examples from: $EXAMPLES_DIR"
    if [ -z "$(find "$EXAMPLES_DIR" -type f -name '*.scr' -print -quit)" ]; then
        echo "No .scr examples found in $EXAMPLES_DIR"
    else
        # find "$EXAMPLES_DIR" -type f -name '*.scr' -print0 | while IFS= read -r -d $'\0' scr_file; do
        find "$EXAMPLES_DIR" -type f -name '*.scr' | while IFS= read -r scr_file; do

            echo "Processing example: $scr_file"
            scribblec "$scr_file"
        done
    fi
    exit 0 # Exit after running all examples
elif [ "$run_erlang_examples" = 1 ]; then
    ERL_DIR="$SCRIBHOME/scribble-gt-demos/erlang"
    echo "Running, starting, and stopping OTP apps in: $ERL_DIR"
    for dir in "$ERL_DIR"/*/; do
        [ -d "$dir" ] || continue
        app=$(basename "$dir")
        echo "Building: $app"
        (cd "$dir" && rebar3 compile)
        echo "Starting and stopping: $app"
        # (cd "$dir" && \
        # erl -noshell -pa _build/default/lib/*/ebin \
        # -eval "application:ensure_all_started('{app}'), timer:sleep(2000), application:stop('{app}'), init:stop().")
        (cd "$dir" && erl -noshell \
            -pa _build/default/lib/*/ebin \
            -eval "application:ensure_all_started('${app}'), timer:sleep(2000), application:stop('${app}'), init:stop().")

    done
    exit 0
elif [ "$run_gt_gen_erlang" = 1 ]; then
    scribblec -gt-gen-erlang "$PROTOCOL_NAME" "$SCRFILE"
    exit 0
elif [ "$run_gt_gen_erlang_role" = 1 ]; then
    scribblec -gt-gen-erlang-role "$PROTOCOL_NAME" "$ROLE_NAME" "$SCRFILE"
    exit 0
elif [ "$run_gt_gen_efsm" = 1 ]; then
    # Generate EFSM dot file via GT command
    scribblec -gt-gen-efsm "$PROTOCOL_NAME" "$ROLE_NAME" "$SCRFILE"

    # Convert .dot to .png if present
    DOT_FILE="$SCRIBHOME/generated/$PROTOCOL_NAME/$ROLE_NAME.dot"
    if [ -f "$DOT_FILE" ]; then
        echo "Generating EFSM: $DOT_FILE for $ROLE_NAME in protocol $PROTOCOL_NAME"
        # Ensure Graphviz 'dot' is available
        if ! command -v dot >/dev/null 2>&1; then
            echo "Error: 'dot' command not found. Please install Graphviz (e.g. 'brew install graphviz')" >&2
            exit 1
        fi
        dot -Tpng "$DOT_FILE" -o "${DOT_FILE%.dot}.png"
    else
        echo "Warning: .dot file not found for $PROTOCOL_NAME/$ROLE_NAME"
    fi
    exit 0
else
    # Original execution path for single file or specific options
    scribblec "$ARGS"
fi
