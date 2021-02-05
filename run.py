import subprocess
import sys
import re
import matplotlib.pyplot as plt
from scipy.stats import shapiro, mannwhitneyu
from ast import literal_eval

FNAME = "data.txt"

def statistical_significance(argv):
    # call with two different fitness functions
    pop_size = 10
    generations = 10
    chromosome_size = 5
    chromosome_range = 3
    fitness_function_1 = argv[1]
    fitness_function_2 = argv[2]

    vals = {}
    for fitness_function in (fitness_function_1, fitness_function_2):
        args = [
            "./Gable",
            "--pop_size",
            str(pop_size),
            "--generations",
            str(generations),
            "--chromosome_size",
            str(chromosome_size),
            "--chromosome_range",
            str(chromosome_range),
            ]
        if fitness_function == "RandomSearch":
            args.append("-r")
        else:
            args.extend(["--fitness_function", fitness_function])

        proc = subprocess.run(args, capture_output=True, encoding="utf-8")
        summary_idx = proc.stdout.find("SUMMARY")
        assert(summary_idx >= 0)
        vals_idx = proc.stdout[:summary_idx].rfind("[")
        vals[fitness_function] = literal_eval(proc.stdout[vals_idx:summary_idx])
        print(f"{fitness_function}: {vals[fitness_function]}")

        output = re.split("[\n ]", proc.stdout[summary_idx:])
        mean = float(output[6])
        median = float(output[8])
        print(f"mean: {mean}")
        print(f"median: {median}")

    # Mann-Whitney U test
    U, p = mannwhitneyu(vals[fitness_function_1], vals[fitness_function_2])
    print(f"p = {p}")

def normality_test(argv):
    # call with fitness function as first arg
    pop_size = 10
    generations = 10
    chromosome_size = 5
    chromosome_range = 3
    fitness_function = argv[1]
    args = [
        "./Gable",
        "--pop_size",
        str(pop_size),
        "--generations",
        str(generations),
        "--chromosome_size",
        str(chromosome_size),
        "--chromosome_range",
        str(chromosome_range),
        ]
    if fitness_function == "RandomSearch":
        args.append("-r")
    else:
        args.extend(["--fitness_function", fitness_function])

    proc = subprocess.run(args, capture_output=True, encoding="utf-8")
    summary_idx = proc.stdout.find("SUMMARY")
    assert(summary_idx >= 0)
    vals_idx = proc.stdout[:summary_idx].rfind("[")
    vals = literal_eval(proc.stdout[vals_idx:summary_idx])
    print(vals)

    # Shapiro-Wilk test
    stat, p = shapiro(vals)
    print(f"p = {p}")

def read_floats(fname):
    data = []
    with open(fname, 'r') as f:
        s = f.readline()
        try:
            data.append(float(s))
        except:
            print("Not a number: %s" % s)
    return data

def plot_time():
    data = [
        read_floats("RefinementTypes_times.txt"),
        read_floats("IOExamples_times.txt")
        ]
    plt.boxplot(data)
    plt.xticks([1, 2], ["Refinement Types", "IO Examples"])
    plt.ylabel("Time per computation (secs)")
    plt.title("Fitness computation time for chromosome size = 3")
    plt.show()

def plot_gens():
    pop_size = 10
    generations = 10
    problem = "Bound"
    # data to plot
    search_space_size = []
    avgs = {"RefinementTypes": [], "RefinementTypesNew": [], "IOExamples": [], "RandomSearch": []}
    errs = {"RefinementTypes": [], "RefinementTypesNew": [], "IOExamples": [], "RandomSearch": []}
    for chromosome_size in (2, 3, 4,):
        for chromosome_range in (2, 3, 4,):
            for fitness_function in ("RefinementTypes", "RefinementTypesNew", "IOExamples", "RandomSearch"):
                print(f"Running {fitness_function} with chromosome size {chromosome_size} and range {chromosome_range}")
                if fitness_function == "RefinementTypes":
                    size = (chromosome_range + 1) ** chromosome_size
                    with open(FNAME, 'a+') as f:
                        f.write('\n' + str(size) + '\t')
                    # only add this once
                    # chromosome range is inclusive
                    # chromosome_size elements, each has (chromosome_range + 1) possibilities
                    search_space_size.append(size)
                args = [
                    "./Gable",
                    "--pop_size",
                    str(pop_size),
                    "--generations",
                    str(generations),
                    "--chromosome_size",
                    str(chromosome_size),
                    "--chromosome_range",
                    str(chromosome_range),
                    "--problem",
                    problem,
                    ]
                if fitness_function == "RandomSearch":
                    args.append("-r")
                else:
                    args.extend(["--fitness_function", fitness_function])
                proc = subprocess.run(args, capture_output=True, encoding="utf-8")
                summary_idx = proc.stdout.find("SUMMARY")
                assert(summary_idx >= 0)
                vals_idx = proc.stdout[:summary_idx].rfind("[")
                vals = literal_eval(proc.stdout[vals_idx:summary_idx])
                output = re.split("[\n ]", proc.stdout[summary_idx:])
                count = int(output[2])
                failed = int(output[4])
                mean = float(output[6])
                median = float(output[8])
                min_ = int(float(output[10]))
                max_ = int(float(output[12]))
                stddev = float(output[14])
                iqr = int(float(output[16]))
                avgs[fitness_function].append(mean)
                errs[fitness_function].append(stddev)
                with open(FNAME, 'a+') as f:
                    f.write(str(mean) + '\t')
                    f.write(str(stddev) + '\t')
                # print(vals)
                # print(f"Count = {count}")
                # print(f"failed = {failed}")
                # print(f"mean = {mean}")
                # print(f"median = {median}")
                # print(f"min = {min_}")
                # print(f"max = {max_}")
                # print(f"stddev = {stddev}")
                # print(f"iqr = {iqr}")

    plt.errorbar(search_space_size, avgs["RefinementTypes"], yerr=errs["RefinementTypes"], label="refinement types", capsize=5)
    plt.errorbar(search_space_size, avgs["RefinementTypesNew"], yerr=errs["RefinementTypesNew"], label="refinement types new", capsize=5)
    plt.errorbar(search_space_size, avgs["IOExamples"], yerr=errs["IOExamples"], label="i/o examples", capsize=5)
    plt.errorbar(search_space_size, avgs["RandomSearch"], yerr=errs["RandomSearch"], label="random search", capsize=5)
    plt.xlabel("Search Space Size")
    plt.ylabel("Number of generations")
    plt.title("Generations to optimal solution (pop size = 10)")
    plt.legend()
    plt.show()

if __name__ == "__main__":
    args = sys.argv
    plot_gens()
    # plot_time()
    # normality_test(args)
    # statistical_significance(args)
