import pandas as pd
import matplotlib
matplotlib.use('TkAgg')
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages

# some definitions
type_operation = 'type_operation'
type_object = 'type_object'
type_range = 'type_range'
type_container = 'type_container'
x_axis = 'Iterations'

descriptions = []
descriptions.append(
    "This report summarizes the benchmark analysis of VectorTree \n" +
    "by comparing its performance with:\n" +
    "  1. std::vector\n" +
    "  2. a primitive persistent vector\n\n" +
    "VectorTree is created to obtain a persistent version of std::vector for multithreaded usage. " +
    "In other words, the target of VectorTree is to achieve the performance of std::vector " +
    "while keeping the persistency to deal with the concurrency issues. " +
    "Hence, the 1st container to compare with is std::vector. " +
    "The 2nd one is a primitive definition for a persistent vector. " +
    "The flowchart of any operation on the primitive persistent vector is as follows:\n" +
    "  1. create a copy of the original vector,\n" +
    "  2. apply the request on the copy,\n" +
    "  3. return the modified copy."
)
descriptions.append(
    "Four fundamental operations are inspected in this benchmark:\n" +
    "  1. emplace_back\n" +
    "  2. pop_back\n" +
    "  3. pop_front\n" +
    "  4. traversal\n\n" +
    "The 3rd operation is actually not a property of the vector data structure " +
    "such that std::vector interface does not include it. " +
    "It requires one-step move operation on all elements which is linear, O(N). " +
    "Its the same, even more problematic, in case of a tree of vectors. " +
    "VectorTree solves this problem using swap-and-pop idiom " +
    "while loosing the order of elements. " +
    "Hence, the pop_front graphs showing a better performance for VectorTree, " +
    "actually shows the performance support coming from the swap-and-pop idiom. " +
    "In other words, consider the graphs for the pop_front operation as the comparison between:\n" +
    "  - pop_front: move<T> with O(N)\n" +
    "  - swap-and-pop: front() = back(); pop_back(): copy<T> with O(2*BufferSize)"
)
descriptions.append(
    "Summary of the test parameters:\n" +
    "  1. Inspected operations: emplace_back, pop_back, pop_front and traversal\n" +
    "  2. Inspected objects: Two types are inspected: type_small and type_large\n" +
    "  3. Original container size:\n" +
    "       BUFFER_SIZE_1 (32)\n" +
    "       BUFFER_SIZE_2 (1024)\n" +
    "       BUFFER_SIZE_3 (32768)\n\n" +
    "The two object types are defined simply as follows:\n" +
    "  1. type_small: an object with one field of int\n" +
    "  2. type_large: an object with an array of 256 elements of int\n\n" +
    "The number of the combinations of the test parameters are:\n" +
    "  1. # of the inspected operations: 4\n" +
    "  2. # of the inspected objects: 2\n" +
    "  3. # of the inspected original container sizes: 3\n" +
    "Hence, the combination of the test parameters yield:\n" +
    "  Test count = 4 * 2 * 3 = 24\n\n" +
    "Note that the tests are performed with a VectorTree of buffer size of 32."
)
descriptions.append(
    "The tests follow a simple algorithm:\n" +
    "  1. Initialize a container with a predefined size, N\n" +
    "  2. Aplly the operation iteratively N times on this original container\n" +
    "  3. Measure timing\n\n"
    "A templated wrapper class is created to simulate a uniform interface " +
    "for the inspected four operations " +
    "which helps to simplify the google benchmark macros. " +
    "A base template (VectorTree) and two specializations (std::vector and persistent_vector) " +
    "simulate the three containers to be compared.\n\n" +
    "DEFINE_BENCHMARK macro defines a shortcut to the google benchmark macros."
)
descriptions.append(
    "The tests are quite simple and the corresponding graphs are self-explanatory. " +
    "Hence, i will not go through the results in detail.\n" +
    "In summary:\n" +
    "  - For containers of small size, although VectorTree is the worst one,\n" +
    "    it performs efficiently (~600ms):\n" +
    "      -> See graphs with BUFFER_SIZE_1\n" +
    "  - For containers of large size, VectorTree performs way better than\n" +
    "    the primitive persistent vector:\n" +
    "      -> See graphs with BUFFER_SIZE_3\n" +
    "  - VectorTree approaches to std::vector\n" +
    "    in case of the three fundamental operations:\n" +
    "      -> See graphs with BUFFER_SIZE_3 and especially with type_large\n" +
    "  - The swap-and-pop idiom provides\n" +
    "    an efficient solution to the pop_front operation:\n" +
    "      -> See graphs with pop_front"
)
descriptions.append(
    "  - The most important problem of VectorTree is the traversal performance.\n" +
    "    A traversal with a VectorTree of size=32768 takes around ~0.01 seconds\n" +
    "    which may be unacceptable in some cases:\n" +
    "      -> See Figures 21 and 24\n\n" +
    "    The iterator must step to the next leaf node\n" +
    "    when the end of the current leaf node buffer is reached\n" +
    "    which requires a DFS algorithm within the tree structure.\n" +
    "    The worst traversal performance is a result of the DFS algorithm\n" +
    "    abstracted/hidden by the path_to_leaf_node__current variable.\n" +
    "    Hence, the iterator of VectorTree must be improved.\n" +
    "    I will study two approaches to either remove or improve the DFS:\n" +
    "      1. Memoization (removes DFS):\n" +
    "         VectorTree stores a linear container of pointers\n" +
    "         to the leaf node buffers,\n" +
    "      2. Asynchronous approach (improves DFS):\n" +
    "         A thread determines the next leaf node buffer asynchronously.\n" +
    "    Both have pros and cons which must be studied in detail.\n" +
    "    For example the memoization adds more memory load:\n" +
    "      array of pointers of N/32 size where 32 is the buffer size.\n" +
    "    Asynhcronous approach, on the other hand,\n" +
    "    terminates the default copy capability of the iterator\n" +
    "    which reduces the copy construction performance.\n" +
    "    The increment operator (i.e. operator++(int)) and the derivatives\n" +
    "    becomes ineffective as a result."
)

# load csv
df = pd.read_csv("../build/bin/benchmark_results.csv")

# parse the 'name' column into its components and the iteration count
# Example name: "BM__emplace_back__small__BUFFER_SIZE_1__type_std/32" ->
#   emplace_back - small object - BUFFER_SIZE_1 (1024) - std::vector - Iteration count=32
name_split = df['name'].str.replace("BM__", "").str.split('/', expand=True)
df['full_name'] = name_split[0]
df[x_axis] = name_split[1].astype(int)

components = df['full_name'].str.split('__', expand=True)
components.columns = [type_operation, type_object, type_range, type_container]
df = pd.concat([df, components], axis=1)

# convert real_time from nanoseconds to microseconds
df['time_us'] = df['real_time'] * 1e-3

# create the pdf output
with PdfPages("benchmark_results.pdf") as pdf:
    # title and description pages
    for i_description, description in enumerate(descriptions):
        fig, ax = plt.subplots()
        if not i_description:
            fig.suptitle("Benchmark Results Report", fontsize=18, weight='bold')
        ax.axis('off')
        fig.text(0.03, 0.5, description, fontsize=11, va='center', wrap=True)
        pdf.savefig(fig)
        plt.close(fig)

    # one page/graph per: type_operation, type_object, type_range
    figure_id = 0
    for (type_operation__, type_object__, type_range__), group in df.groupby([type_operation, type_object, type_range]):
        figure_id += 1

        fig, ax = plt.subplots()
        for container_type, sub in group.groupby(type_container):
            ax.plot(sub[x_axis], sub['time_us'], marker='o', label=container_type)
        ax.set_title(f"Figure {figure_id}: {type_operation__} | {type_object__} | {type_range__}")
        ax.set_xlabel(x_axis)
        ax.set_ylabel("Time (μs)")
        ax.legend(title="Container")
        ax.grid(True)
        plt.tight_layout()
        pdf.savefig(fig)
        plt.close(fig)
