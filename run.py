import subprocess
import sys
import re
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
from scipy.stats import shapiro, mannwhitneyu
from ast import literal_eval

FNAME = "data.txt"

def statistical_significance(argv):
    # call with two different fitness functions
    pop_size = 10
    generations = 3
    chromosome_size = 4
    chromosome_range = 4
    eval_ = "BestFitness"
    fitness_function = argv[1]

    vals = {}
    for random in (True, False):
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
            "--eval",
            eval_,
            "--fitness_function",
            fitness_function
            ]
        if random:
            args.append("-r")

        proc = subprocess.run(args, capture_output=True, encoding="utf-8")
        summary_idx = proc.stdout.find("SUMMARY")
        assert(summary_idx >= 0)
        vals_idx = proc.stdout[:summary_idx].rfind("[")
        vals[random] = literal_eval(proc.stdout[vals_idx:summary_idx])
        print(f"{random}: {vals[random]}")

        output = re.split("[\n ]", proc.stdout[summary_idx:])
        mean = float(output[6])
        median = float(output[8])
        print(f"mean: {mean}")
        print(f"median: {median}")

    # Mann-Whitney U test
    U, p = mannwhitneyu(vals[True], vals[False])
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

def compare_random(argv):
    pop_size = 10
    generations = 8
    problem = "MultiFilter2"
    eval_ = "BestFitness"
    fitness_function = argv[1]
    # data to plot
    search_space_size = []
    avgs = {fitness_function: [], "RandomSearch": []}
    errs = {fitness_function: [], "RandomSearch": []}
    vals = {fitness_function: [], "RandomSearch": []}
    for chromosome_size in (5,):
        for chromosome_range in (9,):
            for random in (False, True):
                print(f"Running {fitness_function} with chromosome size {chromosome_size} and range {chromosome_range}")
                if not random:
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
                    "--eval",
                    eval_,
                    "--fitness_function",
                    fitness_function
                    ]
                if random:
                    args.append("-r")
                proc = subprocess.run(args, capture_output=True, encoding="utf-8")
                summary_idx = proc.stdout.find("SUMMARY")
                assert(summary_idx >= 0)
                vals_idx = proc.stdout[:summary_idx].rfind("[")
                this_vals = literal_eval(proc.stdout[vals_idx:summary_idx])
                output = re.split("[\n ]", proc.stdout[summary_idx:])
                count = int(output[2])
                failed = int(output[4])
                mean = float(output[6])
                median = float(output[8])
                min_ = int(float(output[10]))
                max_ = int(float(output[12]))
                stddev = float(output[14])
                iqr = int(float(output[16]))

                if random:
                    avgs["RandomSearch"].append(mean)
                    errs["RandomSearch"].append(stddev)
                    vals["RandomSearch"].append(this_vals)
                else:
                    avgs[fitness_function].append(mean)
                    errs[fitness_function].append(stddev)
                    vals[fitness_function].append(this_vals)
                with open(FNAME, 'a+') as f:
                    f.write(str(mean) + '\t')
                    f.write(str(stddev) + '\t')
                print(this_vals)
                # print(f"Count = {count}")
                # print(f"failed = {failed}")
                # print(f"mean = {mean}")
                # print(f"median = {median}")
                # print(f"min = {min_}")
                # print(f"max = {max_}")
                # print(f"stddev = {stddev}")
                # print(f"iqr = {iqr}")

    for i in range(len(vals[fitness_function])):
        U, p = mannwhitneyu(vals[fitness_function][i], vals["RandomSearch"][i])
        print(f"Mann Whitney U test: p = {p} for search space size {search_space_size[i]}")

    plt.errorbar(search_space_size, avgs[fitness_function], yerr=errs[fitness_function], label=fitness_function, capsize=5)
    plt.errorbar(search_space_size, avgs["RandomSearch"], yerr=errs["RandomSearch"], label="random search", capsize=5)
    plt.xlabel("Search Space Size")
    plt.ylabel("Best Fitness Found")
    plt.title(f"Best fitness after {generations} generations (pop size = 10)")
    plt.legend()
    plt.show()

def fitness_hist(argv):
    fname = argv[1]
    fitnesses = []
    with open(fname, 'r') as f:
        for line in f.readlines():
            fitness_idx = line.find(": ") + 2
            fitness = float(line[fitness_idx:])
            fitnesses.append(fitness)
    
    plt.hist(fitnesses, bins=10)
    plt.xlabel("Fitness")
    plt.ylabel("Number of individuals")
    plt.title("Fitness values")
    plt.show()

def fitness_scatter(argv):
    pop_size = 10
    generations = 10
    chromosome_size = 4
    chromosome_range = 7
    problem = "MultiFilter"
    fitness_function = argv[1]
    # data to plot
    gens = []
    fitnesses = []
    random_fitnesses = []
    num_trials = 1
    for random in (False, True):
        print(f"Running {fitness_function} for {problem}, with random = {random}")
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
            "--fitness_function",
            fitness_function,
            "--num_trials",
            str(num_trials)
            ]
        if random:
            args.append("-r")
        proc = subprocess.run(args, capture_output=True, encoding="utf-8")
        
        trial = 0
        gen = 0
        count = 0
        for line in proc.stderr.split('\n'):
            fitness_start = line.find(":") + 1
            fitness_end = line.find(",")
            if fitness_start == -1 or fitness_end == -1:
                # empty line
                continue

            fitness = float(line[fitness_start : fitness_end])
            print(f"Gen {gen}, Fitness {fitness}")
            if random:
                # only add to gens once
                gens.append(gen)
            if random:
                random_fitnesses.append(fitness)
            else:
                fitnesses.append(fitness)

            count += 1
            if count == pop_size:
                count = 0
                gen += 1

            if gen == generations:
                gen = 0
                trial += 1
            
            if trial == num_trials:
                break

    plt.scatter(gens, fitnesses, alpha=0.1, color='b')
    plt.scatter(gens, random_fitnesses, alpha=0.1, color='r')
    plt.title(f"{problem}: pop size {pop_size}, {num_trials} trials, size {chromosome_size}, range {chromosome_range}")
    plt.xlabel("Generations")
    plt.ylabel("Fitness")
    plt.legend(handles=[
            mpatches.Patch(color='b', label=fitness_function),
            mpatches.Patch(color='r', label="random")
        ])
    plt.show()

if __name__ == "__main__":
    args = sys.argv
    # plot_gens()
    # plot_time()
    # normality_test(args)
    # statistical_significance(args)
    compare_random(args)
    # fitness_hist(args)
    # fitness_scatter(args)
