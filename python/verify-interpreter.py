# Runs fuzzing tests against the Churchroad and Gator interpreters, given a Verilog file.
import argparse
from collections import defaultdict
import subprocess
import random
import os

NUM_TEST_CASES = 100
NUM_CLOCK_CYCLES = 1


if __name__ == "__main__":
    parser = argparse.ArgumentParser()

    parser.add_argument("-churchroad_src", help="Path of Churchroad src")
    parser.add_argument("-egraph_json_path", help="Path of EGraph JSON")
    parser.add_argument("-dump_path", help="Place to dump")
    parser.add_argument(
        "-inputs",
        help="Semicolon-separated input tuples of (signal-name, bw), e.g. (INIT, 64);(I0, 1)",
    )
    parser.add_argument(
        "-outputs",
        help="Semicolon-separated output tuple of (signal-name, bw), e.g. (O, 64)",
    )

    args = parser.parse_args()

    if args.dump_path is None:
        print("Error: dump path not found")
        exit(1)

    if args.inputs is None:
        print("Error: inputs not found")
        exit(1)

    if args.outputs is None:
        print("Error: outputs not found")
        exit(1)

    # Check environment variables
    if "CHURCHROAD_DIR" not in os.environ:
        print("Error: Please set the CHURCHROAD_DIR environment variable")
        exit(1)

    if "GATOR_DIR" not in os.environ:
        print("Error: Please set the GATOR_DIR environment variable")
        exit(1)

    signal_to_bitwidth = {}

    # process signals
    # inputs are comma separated (name, bw)
    input_tuples = []
    for pair in args.inputs.split(";"):
        if not (len(pair) > 2 and pair[0] == "(" and pair[-1] == ")"):
            raise ValueError("Invalid input/output format")
        pair = pair[1:-1]
        [var_name, bitwidth] = pair.split(",")
        bitwidth = int(bitwidth)

        input_tuples.append((var_name, bitwidth))

        if var_name in signal_to_bitwidth:
            raise ValueError("Duplicate signal name found")

        signal_to_bitwidth[var_name] = bitwidth

    # Create input
    inputs = defaultdict(list)
    for signal, bitwidth in input_tuples:
        for _ in range(NUM_TEST_CASES):
            test_inputs = []
            for _ in range(NUM_CLOCK_CYCLES):
                test_inputs.append(random.randint(0, 2**bitwidth - 1))
            inputs[signal].append(test_inputs)

    input_str = ""
    churchroad_input_str = ""
    churchroad_input_str += f"{NUM_TEST_CASES}\n"
    churchroad_input_str += f"{NUM_CLOCK_CYCLES}\n"
    churchroad_input_str += f"{len(input_tuples)}\n"
    # Now, add inputs
    for test_case in range(NUM_TEST_CASES):
        for time in range(NUM_CLOCK_CYCLES):
            for signal, input_values in inputs.items():
                input_str += f"{signal}:{input_values[test_case][time]}\n"

    churchroad_input_str += input_str

    # run Churchroad
    churchroad_cmd = [
        "cargo",
        "run",
        "--manifest-path",
        os.path.join(os.environ["CHURCHROAD_DIR"], "Cargo.toml"),
        args.churchroad_src,
    ]

    gator_cmd = [
        "racket",
        "racket/main.rkt",
        "--json-filepath",
        args.egraph_json_path,
        "--num-test-cases",
        str(NUM_TEST_CASES),
        "--num-clock-cycles",
        str(NUM_CLOCK_CYCLES),
        "--output-signal-name",
        "O",
        # args.outputs.split(";")[0].split(",")[0][1:],
    ]

    churchroad_proc = subprocess.Popen(
        churchroad_cmd,
        cwd=os.environ["CHURCHROAD_DIR"],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )

    gator_proc = subprocess.Popen(
        gator_cmd,
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )

    (churchroad_output, churchroad_err) = churchroad_proc.communicate(
        # encode as bytes
        input=churchroad_input_str.encode()
    )

    (gator_output, gator_err) = gator_proc.communicate(
        # encode as bytes
        input=input_str.encode()
    )

    print(f"Churchroad output: {churchroad_output.decode('utf-8')}")
    print(f"Gator output: {gator_output.decode('utf-8')}")

    if churchroad_proc.returncode != 0:
        print(f"Churchroad failed with error: {churchroad_err.decode('utf-8')}")
        exit(1)

    if gator_proc.returncode != 0:
        print(f"Gator failed with error: {gator_err.decode('utf-8')}")
        exit(1)

    # Compare outputs
    churchroad_output = churchroad_output.decode().split("\n")
    # throw away first four lines of churchroad output
    churchroad_output = churchroad_output[4:]

    gator_output = gator_output.decode().split("\n")

    if len(churchroad_output) != len(gator_output):
        print("Outputs are not the same length")
        exit(1)

    for churchroad_line, gator_line in zip(churchroad_output, gator_output):
        if churchroad_line != gator_line:
            print(f"Churchroad output: {churchroad_line}")
            print(f"Gator output: {gator_line}")
            print("Outputs are not the same")
            exit(1)

    if churchroad_output == gator_output:
        print("complete success")
