/*!
 * Gtest test cases for PersistentDAG interface.
 *
 * @author    <baris.albayrak.ieee@gmail.com>
 * @version   0.0.1
 * @see       the README file of the github repository below for an overview of this project.
 * 
 * github:    <https://github.com/BarisAlbayrakIEEE/PersistentDAG.git>
 */

#ifndef _test_HeaderFile
#define _test_HeaderFile

#include <gtest/gtest.h>
#include <numeric>
#include <random>
#include "../inc/PersistentDAG_1.h"
#include "../inc/PersistentDAG_2.h"

struct Foo{
    public:
    int _i{};
    Foo() = default;
    Foo(int i) : _i(i) {};
};

#endif
