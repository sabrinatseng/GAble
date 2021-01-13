import subprocess
import re
import matplotlib.pyplot as plt

FNAME = "data.txt"

def main():
    pop_size = 10
    generations = 10
    # data to plot
    search_space_size = []
    avgs = {"RefinementTypes": [], "IOExamples": []}
    for chromosome_size in (2, 3, 4):
        for chromosome_range in (2, 3):
            for fitness_function in ("RefinementTypes", "IOExamples"):
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
                    "--fitness_function",
                    fitness_function,
                    "--chromosome_size",
                    str(chromosome_size),
                    "--chromosome_range",
                    str(chromosome_range),
                    ]
                proc = subprocess.run(args, capture_output=True, encoding="utf-8")
                summary_idx = proc.stdout.find("SUMMARY")
                assert(summary_idx >= 0)
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
                with open(FNAME, 'a+') as f:
                    f.write(str(mean) + '\t')

    plt.plot(search_space_size, avgs["RefinementTypes"], label="refinement types")
    plt.plot(search_space_size, avgs["IOExamples"], label="i/o examples")
    plt.xlabel("Search Space Size")
    plt.ylabel("Number of generations")
    plt.legend()
    plt.show()

if __name__ == "__main__":
    main()
