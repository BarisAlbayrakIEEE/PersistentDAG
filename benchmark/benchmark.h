/*!
 * Google benchmarks for PersistentDAG interface.
 *
 * @author    <baris.albayrak.ieee@gmail.com>
 * @version   0.0.1
 * @see       the README file of the github repository below for an overview of this project.
 * 
 * github:    <https://github.com/BarisAlbayrakIEEE/PersistentDAG.git>
 */

#ifndef _benchmark_HeaderFile
#define _benchmark_HeaderFile

#include <benchmark/benchmark.h>
#include <type_traits>
#include "../inc/PersistentDAG_1.h"
#include "../inc/PersistentDAG_2.h"

/*!
 * small object
 */
struct type_small{
    int _i{};

    public:
    type_small() = default;
    type_small(int i) : _i(i) {};
};

/*!
 * large object
 */
using _u_large = std::array<int, 256>;
struct type_large{
    _u_large _arr{{}};
    int _i{};

    public:
    type_large() = default;
    type_large(int i) : _i(i) { for (auto& val : _arr) val = i; };
};

#endif
