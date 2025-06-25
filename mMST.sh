#!/bin/sh
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

  -run-all-gt-examples     Run default operations for all examples in
                           scribble-gt-demos/scribble/

  -gt-no-corr
            Skip global-local correspondence checking.
  -gt-gen-erlang 
            Generate Erlang code.
            ./mMST.sh -gt-gen-erlang <ProtocolName> <PathToScribbleFile>
  -gt-gen-erlang-role
            Generate Erlang code.
            ./mMST.sh -gt-gen-erlang-role <ProtocolName> <Role> <PathToScribbleFile>
  -gt-event-fsm
            Generate event-based FSMs.
            ./mMST.sh -gt-event-fsm <ProtocolName> <Role> <PathToScribbleFile>


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
# CLASSPATH="$CLASSPATH:$SCRIBHOME/lib/antlr-runtime.jar"
CLASSPATH="$CLASSPATH:$SCRIBHOME/lib/commons-io.jar"
CLASSPATH="$CLASSPATH:$SCRIBHOME/lib/scribble-ast.jar"
CLASSPATH="$CLASSPATH:$SCRIBHOME/lib/scribble-cli.jar"
# CLASSPATH="$CLASSPATH:$SCRIBHOME/lib/scribble-codegen.jar"
CLASSPATH="$CLASSPATH:$SCRIBHOME/lib/scribble-core.jar"
CLASSPATH="$CLASSPATH:$SCRIBHOME/lib/scribble-main.jar"
CLASSPATH="$CLASSPATH:$SCRIBHOME/lib/scribble-parser.jar"
CLASSPATH="$CLASSPATH:$SCRIBHOME/lib/scribble-gt.jar"
#CLASSPATH="$CLASSPATH:$SCRIBHOME/lib/freemarker-2.3.32.jar"
#CLASSPATH="$CLASSPATH:$SCRIBHOME/lib/stringtemplate.jar"
CLASSPATH="$(fixpath "$CLASSPATH")" # Correctly assign the output of fixpath

usage=0
verbose=0
ARGS="" # Initialize ARGS to empty string
run_all_gt_examples=0 # Flag for the new option

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
        -run-all-gt-examples)
            run_all_gt_examples=1
            shift # Consume the option
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
fi

CMD="java -cp $CLASSPATH org.scribble.ext.gt.cli.GTCommandLine2"

# Define the main GT invocation function
scribblec() {
    if [ "$verbose" = 1 ]; then
        echo "Executing: $CMD" "$@"
    fi
    eval "$CMD" "$@"
}

# When the batch flag is set, loop through all `.scr` files and process them
if [ "$run_all_gt_examples" = 1 ]; then
    EXAMPLES_DIR="$SCRIBHOME/scribble-gt-demos/scribble"
    if [ ! -d "$EXAMPLES_DIR" ]; then
        echo "Error: Examples directory not found: $EXAMPLES_DIR" >&2
        exit 1
    fi
    echo "Running all examples from: $EXAMPLES_DIR"
    if [ -z "$(find "$EXAMPLES_DIR" -type f -name '*.scr' -print -quit)" ]; then
        echo "No .scr examples found in $EXAMPLES_DIR"
    else
        find "$EXAMPLES_DIR" -type f -name '*.scr' -print0 | while IFS= read -r -d $'\0' scr_file; do
            echo "Processing example: $scr_file"
            scribblec "$scr_file"
        done
    fi
    exit 0 # Exit after running all examples
else
    # Original execution path for single file or specific options
    scribblec "$ARGS"
fi

