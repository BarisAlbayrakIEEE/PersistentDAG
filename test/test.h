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

using namespace PersistentDAGNamespace_1;

struct Foo{
    public:
    int _i{};
    Foo() = default;
    Foo(int i) : _i(i) {};
};

using _pd_1 = PersistentDAGNamespace_1::PersistentDAG<Foo>;
using _pd_2 = PersistentDAGNamespace_2::PersistentDAG<Foo>;

/*!
 * helper function to create a VectorTree with the input size - use emplace_back
 */
_pd_1 get_pd_1_1(std::size_t N) {
    _pd_1 pd{};
    int counter{};
    for (auto i = 0; i < N; ++i) {
        if (counter == 0) {
            pd = pd.emplace(
                EDAG_node_types::non_updatable,
                EDAG_node_states::uptodate,
                i
            );
        }
        else if (counter == 1) {
            pd = pd.emplace(

                EDAG_node_types::self_updatable,
                EDAG_node_states::uptodate,
                i
            );
        }
        else if (counter == 2) {
            pd = pd.emplace(
                EDAG_node_types::ancestor_updatable,
                EDAG_node_states::uptodate,
                i
            );
        }
        else {
            counter = -1;
            pd = pd.emplace(
                EDAG_node_types::invariant_updatable,
                EDAG_node_states::uptodate,
                i
            );
        }
        ++counter;
    }
    return pd;
};

/*!
 * helper function to create a VectorTree with the input size - use VectorTree constructor with std::vector
 */
_pd_1 get_pd_1_2(std::size_t N) {
    std::vector<Foo> v{};
    for (std::size_t i = 0; i < N; ++i) v.emplace_back(i);
    return _pd_1(v);
};

static constexpr std::size_t LARGE_SIZE{ 1025 };
static constexpr std::size_t EDGE_SIZE{ 1024 };
_pd_1 pd_0{};
_pd_1 pd_LARGE = get_pd_1_2(LARGE_SIZE);
_pd_1 pd_EDGE = get_pd_1_2(EDGE_SIZE);
std::random_device RD; // obtain a random number from hardware
std::mt19937 GEN(RD()); // seed the generator

/*!
 * test VectorTree for the ctor-default
 */
TEST(test_VectorTree, ctor_default) {
    ASSERT_EQ(pd_0.size(), 0);
    ASSERT_TRUE(pd_0.empty());
    for (auto it = pd_0.cbegin(); it != pd_0.cend(); ++it) {
        ASSERT_EQ(0, 1);
    }
}

#endif
