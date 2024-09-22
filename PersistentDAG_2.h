/*!
 * A persistent DAG interface
 * 
 * this interface is the next version for PersistentDAG_2.
 * the following issues are covered in this version:
 *     1. The updatability of a node is a part of the static type definition
 *        which solves the problem in PersistentDAG_2.h
 *        about the runtime updatability definition
 *     2. nodes are classified based on the number of ancestors
 *        in order to have a better memory allocation
 *     3. DAG_node class is defined to address the 1st 2 issues
 *     4. the background operation removing the unused deleted nodes is removed
 *        to ensure that the background thread works only on the state data
 * 
 * 
 * 
 * DAG_node is a templated class with two template parameters:
 *     updatability and ancestor node count
 * the acceptable updatability types are the same as the ones in the 1st version:
 *     non_updatable, self_updatable, ancestor_updatable, invariant_updatable
 * 
 * the ancestor node count is mostly a part of the definition.
 * for example, in a geometry application,
 * a line is made by two points
 * which means by definition it has two ancestors.
 * on the other hand, for some types,
 * the ancestor count may not be fixed by definition.
 * for example, a surface can be defined by one or more edges.
 * the static type definition by ancestor node count
 * is limitted by a static constant (ANCESTOR_COUNT__MAX)
 * which is initialized with 8.
 * hence, types with ancestor node count less than or equal to ANCESTOR_COUNT__MAX
 * BY DEFINITION can be stored by the nodes
 * with statically defined ancestors (std::array)
 * while the others (having ancestors more than ANCESTOR_COUNT__MAX or ancestors count is dynamic)
 * can be stored by the nodes with dynamic (std::vector) ancestor nodes.
 * the same approach is not applied to the descendant nodes
 * as the descendant node count is rarely a part of the type definition.
 * defining the ancestor nodes in static containers (std::array)
 * allows locating all the ancestor node allocations of the same type
 * (i.e. nodes with 1 ancestor, nodes with two ancestors, etc.)
 * into one big contiguous memory:
 *     std::vector<std::array<node_location, 1>>
 *     std::vector<std::array<node_location, 2>>
 *     std::vector<std::array<node_location, 3>>
 *     std::vector<std::array<node_location, 4>>
 *     std::vector<std::array<node_location, 5>>
 *     std::vector<std::array<node_location, 6>>
 *     std::vector<std::array<node_location, 7>>
 *     std::vector<std::array<node_location, 8>>
 *     std::vector<std::vector<node_location>>
 *     where node_location is the location
 *     (i.e. the container and the index) of the ancestor node
 *     and is defined as std::pair<unsigned char, std::size_t>
 * the above ancestor node definition increases
 * the effectivity of the copy constructor of the DAG of
 * the 1st version which defines the ancestor nodes as
 * std::vector<std::vector<std::size_t>>>.
 * 
 * having a templated node class, on the other hand,
 * introduces some complexity as various node types
 * must be managed with a uniform interface
 * where std::variant comes to rescue.
 * std::variant provides type safe manipulation on various types.
 * however, it adds some extra memory to address the current type.
 * additionally, it requires uniform memory allocation for the enclosed types
 * which ends up allocating the memory for the type with the largest size.
 * in order to cancel this excessive memory allocation,
 * a couple of variant types are defined to ensure that
 * each stores the nodes with the same size
 * (e.g. non-updatable-node and invariant-updatable-node-without-ancestor).
 * 
 * std::variant together with ancestor node count type
 * provides accessing the node data statically very well.
 * however, some situations are runtime dependent.
 * remember that the ancestor node definition:
 *     std::pair<unsigned char, std::size_t>.
 * the first data in the pair (which is a runtime value) locates the container
 * and the second is the index.
 * as the first data locating the node container is a runtime value,
 * it requires a switch-case conditional.
 * this would cause a sensible increase in the traversal time.
 * in order to solve this issue,
 * an array holding the pointers to the node containers is initilaized
 * during the constructors of the DAG.
 * this solution replaces the conditional inspection with a pointer indirection.
 * 
 * the 4th item makes the background thread
 * to manipulate the node state data only.
 * in the 1st version, the background thread modifies
 * both the node states and the node relations.
 * in this 2nd version, the node relations data
 * is not a shared data between the threads.
 * hence, only the node state data must be secured
 * which can easily be handled by a mutex lock.
 *
 * 
 * 
 * @see the main documentation of Persistent_DAG_1.h for the details
 *
 * 
 *
 * @author    <baris.albayrak.ieee@gmail.com>
 * @version   0.0.1
 * github:    <https://github.com/BarisAlbayrakIEEE/cpp.git>
 */

#ifndef _PersistentDAG_2_HeaderFile
#define _PersistentDAG_2_HeaderFile

#include <array>
#include <vector>
#include <stack>
#include <queue>
#include <tuple>
#include <unordered_set>
#include <unordered_map>
#include <variant>
#include <type_traits>
#include <concepts>
#include <limits.h>
#include <mutex>
#include <future>
#include "read_only_queue_wrapper.h"
#include "VectorTree.h"

namespace DAGNamespace {
    /*!
     * types for template parameters
     */
    struct DAG_node_type__non_updatable;
    struct DAG_node_type__ancestor_updatable;
    struct DAG_node_type__invariant_updatable;
    struct traversal_type_BFS;
    struct traversal_type_DFS;
    struct direction_type_ancestor;
    struct direction_type_descendant;

    /*!
     * @brief
     * erases the element with the input index
     * by swapping with the last element
     * followed by a pop_back
     * only the index (i.e. iterator) of the last element is invalidated
     * and must be corrected
     */
    template <typename T>
    inline void erase__by_swap_and_pop__std_vector__nonconst(
        std::vector<T>& vec,
        std::size_t index_erase)
    {
        auto& element_erase{ vec[index_erase] };
        auto& element_last{ vec.back() };
        std::swap(element_erase, element_last);
        vec.pop_back();
    };

    /*!
     * @brief
     * erases the element with the input index
     * by swapping with the last element
     * followed by a pop_back
     * only the index (i.e. iterator) of the last element is invalidated
     * and must be corrected
    */
    template <typename T>
    [[nodiscard]] inline auto erase__by_swap_and_pop__std_vector__const(
        const std::vector<T>& vec1,
        std::size_t index_erase)
        -> std::vector<T>
    {
        std::vector<T> vec2{ vec1 };
        auto& element_erase{ vec2[index_erase] };
        auto& element_last{ vec2.back() };
        std::swap(element_erase, element_last);
        vec2.pop_back();
        return vec2;
    };

    /*!
     * DAG_node state enumeration
     */
    enum class EDAG_node_states : unsigned char {
        uptodate,
        ancestor_fail,
        invalid,
        cycled,
        deleted};

    /*!
     * @brief limit for static allocation
     */
    static const unsigned char ANCESTOR_COUNT__MAX = 8;
    static constexpr unsigned char ANCESTOR_COUNT__EXCEED = ANCESTOR_COUNT__MAX + 1;
    static constexpr unsigned char NODE_CONTAINER_COUNT = ANCESTOR_COUNT__EXCEED + 1;

    /// aliases for the ancestor count
    template <unsigned char ancestor_count>
    using _a_integral = std::integral_constant<unsigned char, ancestor_count>;
    using _a_node_location = std::pair<unsigned char, std::size_t>;
    template <unsigned char ancestor_count>
	using _a_node_locations__cond = std::conditional_t<
		(ancestor_count < ANCESTOR_COUNT__EXCEED),
		std::array<_a_node_location, ancestor_count>,
		std::vector<_a_node_location>>;
	using _a_node_locations__var_raw = std::variant<
		std::array<_a_node_location, 1>*,
		std::array<_a_node_location, 2>*,
		std::array<_a_node_location, 3>*,
		std::array<_a_node_location, 4>*,
		std::array<_a_node_location, 5>*,
		std::array<_a_node_location, 6>*,
		std::array<_a_node_location, 7>*,
		std::array<_a_node_location, 8>*,
		std::vector<_a_node_location>*>;

	/*!
	 * @brief base template metafunction to validate the ancestor count < ANCESTOR_COUNT__EXCEED
	 */
	template <unsigned char ancestor_count>
	struct fix_ancestor_count {
		static constexpr unsigned char value = ancestor_count > ANCESTOR_COUNT__MAX ? ANCESTOR_COUNT__EXCEED : ancestor_count;
	};
	template <unsigned char ancestor_count>
	static const auto fix_ancestor_count_v = fix_ancestor_count<ancestor_count>::value;

    /// concepts
    template <typename DAG_node_type, typename ancestor_count_type>
    concept CDAG_node__non_updatable = (
        std::is_same_v<DAG_node_type, DAG_node_type__non_updatable> &&
        ancestor_count_type::value == 0);
    template <typename DAG_node_type, typename ancestor_count_type>
    concept CDAG_node__ancestor_updatable = (
        std::is_same_v<DAG_node_type, DAG_node_type__ancestor_updatable> &&
        ancestor_count_type::value > 0);
    template <typename DAG_node_type, typename ancestor_count_type>
    concept CDAG_node__invariant_updatable = (
        std::is_same_v<DAG_node_type, DAG_node_type__invariant_updatable> &&
        ancestor_count_type::value >= 0);
    template <typename DAG_node_type, typename ancestor_count_type>
    concept CDAG_node__0 = (
        CDAG_node__non_updatable<DAG_node_type, ancestor_count_type> ||
        (
			CDAG_node__invariant_updatable<DAG_node_type, ancestor_count_type> &&
            ancestor_count_type::value == 0));
    template <typename DAG_node_type, typename ancestor_count_type>
    concept CDAG_node__n = (
        CDAG_node__ancestor_updatable<DAG_node_type, ancestor_count_type> ||
        (
			CDAG_node__invariant_updatable<DAG_node_type, ancestor_count_type> &&
			ancestor_count_type::value > 0));
    template <typename DAG_node_type, typename ancestor_count_type>
    concept CDAG_node = (
		CDAG_node__0<DAG_node_type, ancestor_count_type> ||
		CDAG_node__n<DAG_node_type, ancestor_count_type>);






	/*!
	 * @brief Hash function object for
	 * std::unordered_map<std::pair<unsigned char, std::size_t>, Value>
	 */
	using _a_pair = std::pair<unsigned char, std::size_t>;
	struct pair_hasher {
		std::size_t operator()(_a_pair p) const noexcept {
			return std::size_t(p.first) << 32 | p.second;
		}
	};
	/*!
	 * @brief KeyEq function object for
	 * std::unordered_map<std::pair<unsigned char, std::size_t>, Value>
	 */
	struct pair_keyeq {
		bool operator()(const _a_pair& lhs, const _a_pair& rhs) const {
			return lhs.first == rhs.first && lhs.second == rhs.second;
		};
	};






	/*!
	 * @brief PersistentDAG_2 class
	 * @see the main documentation of this header file for the details
	 */
	template <typename T>
    class PersistentDAG_2 {
        /// forward declarations
		template <typename DAG_type, typename traversal_type, typename direction_type>
        friend struct DAG_iterator_base;
        template <typename DAG_type, typename traversal_type, typename direction_type>
        friend class DAG_const_iterator;
        template <typename DAG_type, typename traversal_type, typename direction_type>
        friend class DAG_iterator;





	public:

		/*!
		 * @brief DAG_node class
		 * base template
		 */
		template <typename DAG_node_type, typename ancestor_count_type>
			requires (CDAG_node<DAG_node_type, ancestor_count_type>)
		class DAG_node {};

		/*!
		 * @brief DAG_node class
		 * specialization for the nodes without ancestors:
		 *     (DAG_node_type == DAG_node_type__non_updatable || DAG_node_type == DAG_node_type__invariant_updatable) &&
		 *     ancestor_count_type == _a_integral<0>
		 */
		template <typename DAG_node_type, typename ancestor_count_type>
			requires (CDAG_node__0<DAG_node_type, ancestor_count_type>)
		class DAG_node<DAG_node_type, ancestor_count_type> {
			friend class PersistentDAG_2<T>;

			using _u_ancestor_nodes = std::vector<_a_node_location>;

			DAG_node() = default;

			[[nodiscard]] auto get_ancestor_count() const -> unsigned char {
				return 0;
			};
			[[nodiscard]] inline auto get_node_locations__ancestor__cond() const -> std::optional<_u_ancestor_nodes> {
				return {};
			};
			[[nodiscard]] inline auto get_node_locations__ancestor__var_raw() const -> std::optional<_a_node_locations__var_raw> {
				return {};
			};

			template <typename DAG_node_type2 = DAG_node_type>
				requires (std::is_same_v<DAG_node_type, DAG_node_type__non_updatable>)
			[[nodiscard]] auto get_final_state__default(
				PersistentDAG_2<T> const*,
				T const*,
				EDAG_node_states) const
				-> EDAG_node_states
			{
				return EDAG_node_states::uptodate;
			};

			template <typename DAG_node_type2 = DAG_node_type>
				requires (std::is_same_v<DAG_node_type, DAG_node_type__invariant_updatable>)
			[[nodiscard]] auto get_final_state__default(
				PersistentDAG_2<T> const*,
				T const* data,
				EDAG_node_states state__initial) const
				-> EDAG_node_states
			{
				EDAG_node_states state__final{ state__initial };
				if (data->inspect_invariant()) {
					state__final = EDAG_node_states::uptodate;
				}
				else {
					state__final = EDAG_node_states::invalid;
				}
				return state__final;
			};

			[[nodiscard]] auto get_final_state__propogate(
				PersistentDAG_2<T> const*,
				T const*,
				EDAG_node_states state__initial,
				EDAG_node_states) const
				-> EDAG_node_states
			{
				return state__initial;
			};
		};

		/*!
		 * @brief DAG_node class
		 * specialization for the nodes with ancestors
		 *     DAG_node_type == DAG_node_type__ancestor_updatable &&
		 *     ancestor_count_type != _a_integral<0>
		 */
		template <typename ancestor_count_type>
			requires (CDAG_node__n<DAG_node_type__ancestor_updatable, ancestor_count_type>)
		class DAG_node<DAG_node_type__ancestor_updatable, ancestor_count_type> {
			friend class PersistentDAG_2<T>;

			using _u_ancestor_nodes = _a_node_locations__cond<ancestor_count_type::value>;
			_u_ancestor_nodes _ancestor_nodes;

			DAG_node() = default;
			explicit DAG_node(const _u_ancestor_nodes& node_locations__ancestor__cond)
				: _ancestor_nodes(node_locations__ancestor__cond) {};

			[[nodiscard]] inline auto get_ancestor_count() const -> unsigned char {
				if (ancestor_count_type::value > ANCESTOR_COUNT__EXCEED) {
					return ANCESTOR_COUNT__EXCEED;
				}
				return ancestor_count_type::value;
			};
			[[nodiscard]] inline auto get_node_locations__ancestor__cond() const -> std::optional<_u_ancestor_nodes> {
				return _ancestor_nodes;
			};
			[[nodiscard]] inline auto get_node_locations__ancestor__var_raw() -> std::optional<_a_node_locations__var_raw> {
				return _a_node_locations__var_raw(&_ancestor_nodes);
			};

			[[nodiscard]] auto get_final_state__default__helper(
				PersistentDAG_2<T> const* DAG_,
				T const*,
				EDAG_node_states state__initial)
				-> EDAG_node_states
			{
				if (state__initial == EDAG_node_states::deleted || state__initial == EDAG_node_states::cycled) {
					return state__initial;
				}

				EDAG_node_states state__final{ state__initial };
				bool check_all_uptodate{ true };
				for (auto& node_location__ancestor : _ancestor_nodes) {
					auto state_ancestor{ DAG_->get_state(node_location__ancestor) };
					if (state_ancestor != EDAG_node_states::uptodate) {
						check_all_uptodate = false;
						if (
							static_cast<unsigned char>(EDAG_node_states::ancestor_fail) >
							static_cast<unsigned char>(state_ancestor))
						{
							state__final = EDAG_node_states::ancestor_fail;
						}
					}
				}
				if (check_all_uptodate) {
					state__final = EDAG_node_states::uptodate;
				}
				return state__final;
			};

			[[nodiscard]] auto get_final_state__default(
				PersistentDAG_2<T> const* DAG_,
				T const* data,
				EDAG_node_states state__initial)
				-> EDAG_node_states
			{
				return get_final_state__default__helper(DAG_, data, state__initial);
			};

			[[nodiscard]] auto get_final_state__propogate(
				PersistentDAG_2<T> const* DAG_,
				T const*,
				EDAG_node_states state__initial,
				EDAG_node_states state__propogate)
				-> EDAG_node_states
			{
				EDAG_node_states state__final{ state__initial };
				if (state__propogate == EDAG_node_states::uptodate) {
					bool all_uptodate{ true };
					for (auto& node_location__ancestor : _ancestor_nodes) {
						auto state_ancestor{ DAG_->get_state(node_location__ancestor) };
						if (static_cast<unsigned char>(state_ancestor) >
							static_cast<unsigned char>(state__initial))
						{
							all_uptodate = false;
							break;
						}
					}
					if (all_uptodate) {
						state__final = EDAG_node_states::uptodate;
					}
				}
				else if (
					static_cast<unsigned char>(state__propogate) >
					static_cast<unsigned char>(state__initial))
				{
					state__final = state__propogate;
				}
				return state__final;
			};
		};

		/*!
		 * @brief DAG_node class
		 * specialization for the nodes with ancestors
		 *     DAG_node_type == DAG_node_type__invariant_updatable &&
		 *     ancestor_count_type != _a_integral<0>
		 */
		template <typename ancestor_count_type>
			requires (CDAG_node__n<DAG_node_type__invariant_updatable, ancestor_count_type>)
		class DAG_node<DAG_node_type__invariant_updatable, ancestor_count_type> {
			friend class PersistentDAG_2<T>;

			using _u_ancestor_nodes = _a_node_locations__cond<ancestor_count_type::value>;
			_u_ancestor_nodes _ancestor_nodes;

			DAG_node() = default;
			explicit DAG_node(const _u_ancestor_nodes& node_locations__ancestor__cond)
				: _ancestor_nodes(node_locations__ancestor__cond) {};

			[[nodiscard]] inline auto get_ancestor_count() const -> unsigned char {
				if (ancestor_count_type::value > ANCESTOR_COUNT__EXCEED) {
					return ANCESTOR_COUNT__EXCEED;
				}
				return ancestor_count_type::value;
			};
			[[nodiscard]] inline auto get_node_locations__ancestor__cond() const -> std::optional<_u_ancestor_nodes> {
				return _ancestor_nodes;
			};
			[[nodiscard]] inline auto get_node_locations__ancestor__var_raw() -> std::optional<_a_node_locations__var_raw> {
				return _a_node_locations__var_raw(&_ancestor_nodes);
			};

			[[nodiscard]] auto get_final_state__default__helper(
				PersistentDAG_2<T> const* DAG_,
				T const*,
				EDAG_node_states state__initial)
				-> EDAG_node_states
			{
				if (state__initial == EDAG_node_states::deleted || state__initial == EDAG_node_states::cycled) {
					return state__initial;
				}

				EDAG_node_states state__final{ state__initial };
				bool check_all_uptodate{ true };
				for (auto& node_location__ancestor : _ancestor_nodes) {
					auto state_ancestor{ DAG_->get_state(node_location__ancestor) };
					if (state_ancestor != EDAG_node_states::uptodate) {
						check_all_uptodate = false;
						if (
							static_cast<unsigned char>(EDAG_node_states::ancestor_fail) >
							static_cast<unsigned char>(state_ancestor))
						{
							state__final = EDAG_node_states::ancestor_fail;
						}
					}
				}
				if (check_all_uptodate) {
					state__final = EDAG_node_states::uptodate;
				}
				return state__final;
			};

			[[nodiscard]] auto get_final_state__default(
				PersistentDAG_2<T> const* DAG_,
				T const* data,
				EDAG_node_states state__initial)
				-> EDAG_node_states
			{
				auto state__final{ get_final_state__default__helper(DAG_, data, state__initial) };
				if (state__final == EDAG_node_states::uptodate && !data->inspect_invariant()) {
					state__final = EDAG_node_states::invalid;
				}
				return state__final;
			};

			[[nodiscard]] auto get_final_state__propogate(
				PersistentDAG_2<T> const* DAG_,
				T const* data,
				EDAG_node_states state__initial,
				EDAG_node_states state__propogate)
				-> EDAG_node_states
			{
				EDAG_node_states state__final{ state__initial };
				if (state__propogate == EDAG_node_states::uptodate) {
					bool all_uptodate{ true };
					for (auto& node_location__ancestor : _ancestor_nodes) {
						auto state_ancestor{ DAG_->get_state(node_location__ancestor) };
						if (static_cast<unsigned char>(state_ancestor) >
							static_cast<unsigned char>(state__initial))
						{
							all_uptodate = false;
							break;
						}
					}
					if (all_uptodate) {
						if (!data->inspect_invariant()) {
							state__final = EDAG_node_states::invalid;
						}
						else {
							state__final = EDAG_node_states::uptodate;
						}
					}
				}
				else if (
					static_cast<unsigned char>(state__propogate) >
					static_cast<unsigned char>(state__initial))
				{
					state__final = state__propogate;
				}
				return state__final;
			};
		};

		/// aliases for DAG_node class
		using _a_node_non_updatable = DAG_node<DAG_node_type__non_updatable, _a_integral<0>>;
		template <typename DAG_node_type>
		using _a_node_0 = DAG_node<DAG_node_type, _a_integral<0>>;
		template <typename DAG_node_type>
		using _a_node_n = DAG_node<DAG_node_type, _a_integral<ANCESTOR_COUNT__EXCEED>>;
		using _a_node_0_var_val = std::variant<
			_a_node_0<DAG_node_type__non_updatable>,
			_a_node_0<DAG_node_type__invariant_updatable>>;
		template <unsigned char ancestor_count>
		using _a_node_n_var_val = std::variant<
			DAG_node<DAG_node_type__ancestor_updatable, _a_integral<ancestor_count>>,
			DAG_node<DAG_node_type__invariant_updatable, _a_integral<ancestor_count>>>;
		using _a_node_var_val = std::variant<
			DAG_node<DAG_node_type__non_updatable, _a_integral<0>>,
			DAG_node<DAG_node_type__ancestor_updatable, _a_integral<1>>,
			DAG_node<DAG_node_type__ancestor_updatable, _a_integral<2>>,
			DAG_node<DAG_node_type__ancestor_updatable, _a_integral<3>>,
			DAG_node<DAG_node_type__ancestor_updatable, _a_integral<4>>,
			DAG_node<DAG_node_type__ancestor_updatable, _a_integral<5>>,
			DAG_node<DAG_node_type__ancestor_updatable, _a_integral<6>>,
			DAG_node<DAG_node_type__ancestor_updatable, _a_integral<7>>,
			DAG_node<DAG_node_type__ancestor_updatable, _a_integral<8>>,
			DAG_node<DAG_node_type__ancestor_updatable, _a_integral<ANCESTOR_COUNT__EXCEED>>,
			DAG_node<DAG_node_type__invariant_updatable, _a_integral<0>>,
			DAG_node<DAG_node_type__invariant_updatable, _a_integral<1>>,
			DAG_node<DAG_node_type__invariant_updatable, _a_integral<2>>,
			DAG_node<DAG_node_type__invariant_updatable, _a_integral<3>>,
			DAG_node<DAG_node_type__invariant_updatable, _a_integral<4>>,
			DAG_node<DAG_node_type__invariant_updatable, _a_integral<5>>,
			DAG_node<DAG_node_type__invariant_updatable, _a_integral<6>>,
			DAG_node<DAG_node_type__invariant_updatable, _a_integral<7>>,
			DAG_node<DAG_node_type__invariant_updatable, _a_integral<8>>,
			DAG_node<DAG_node_type__invariant_updatable, _a_integral<ANCESTOR_COUNT__EXCEED>>>;
		using _a_node_var_raw = std::variant<
			DAG_node<DAG_node_type__non_updatable, _a_integral<0>>*,
			DAG_node<DAG_node_type__ancestor_updatable, _a_integral<1>>*,
			DAG_node<DAG_node_type__ancestor_updatable, _a_integral<2>>*,
			DAG_node<DAG_node_type__ancestor_updatable, _a_integral<3>>*,
			DAG_node<DAG_node_type__ancestor_updatable, _a_integral<4>>*,
			DAG_node<DAG_node_type__ancestor_updatable, _a_integral<5>>*,
			DAG_node<DAG_node_type__ancestor_updatable, _a_integral<6>>*,
			DAG_node<DAG_node_type__ancestor_updatable, _a_integral<7>>*,
			DAG_node<DAG_node_type__ancestor_updatable, _a_integral<8>>*,
			DAG_node<DAG_node_type__ancestor_updatable, _a_integral<ANCESTOR_COUNT__EXCEED>>*,
			DAG_node<DAG_node_type__invariant_updatable, _a_integral<0>>*,
			DAG_node<DAG_node_type__invariant_updatable, _a_integral<1>>*,
			DAG_node<DAG_node_type__invariant_updatable, _a_integral<2>>*,
			DAG_node<DAG_node_type__invariant_updatable, _a_integral<3>>*,
			DAG_node<DAG_node_type__invariant_updatable, _a_integral<4>>*,
			DAG_node<DAG_node_type__invariant_updatable, _a_integral<5>>*,
			DAG_node<DAG_node_type__invariant_updatable, _a_integral<6>>*,
			DAG_node<DAG_node_type__invariant_updatable, _a_integral<7>>*,
			DAG_node<DAG_node_type__invariant_updatable, _a_integral<8>>*,
			DAG_node<DAG_node_type__invariant_updatable, _a_integral<ANCESTOR_COUNT__EXCEED>>*>;
		template <unsigned char ancestor_count>
		using _a_nodes_var_raw = std::conditional_t<
			ancestor_count < ANCESTOR_COUNT__EXCEED,
			std::array<_a_node_var_raw, ancestor_count>,
			std::vector<_a_node_var_raw>>;
		using _a_nodes_0 = std::vector<_a_node_0_var_val>;
		using _a_nodes_1 = std::vector<_a_node_n_var_val<1>>;
		using _a_nodes_2 = std::vector<_a_node_n_var_val<2>>;
		using _a_nodes_3 = std::vector<_a_node_n_var_val<3>>;
		using _a_nodes_4 = std::vector<_a_node_n_var_val<4>>;
		using _a_nodes_5 = std::vector<_a_node_n_var_val<5>>;
		using _a_nodes_6 = std::vector<_a_node_n_var_val<6>>;
		using _a_nodes_7 = std::vector<_a_node_n_var_val<7>>;
		using _a_nodes_8 = std::vector<_a_node_n_var_val<8>>;
		using _a_nodes_n = std::vector<_a_node_n_var_val<ANCESTOR_COUNT__EXCEED>>;
		using _a_node_container_var_raw = std::variant<
			_a_nodes_0*, _a_nodes_1*, _a_nodes_2*, _a_nodes_3*, _a_nodes_4*,
			_a_nodes_5*, _a_nodes_6*, _a_nodes_7*, _a_nodes_8*, _a_nodes_n*>;
		template <unsigned char ancestor_count>
		using _a_que_wrap_a = read_only_queue_wrapper<std::array<_a_node_location, ancestor_count>>;
		using _a_que_wrap_n = read_only_queue_wrapper<std::vector<_a_node_location, std::allocator<_a_node_location>>>;
		using _a_que_wrap_var_val = std::variant<
			_a_que_wrap_a<1>, _a_que_wrap_a<2>, _a_que_wrap_a<3>, _a_que_wrap_a<4>,
			_a_que_wrap_a<5>, _a_que_wrap_a<6>, _a_que_wrap_a<7>, _a_que_wrap_a<8>,
			_a_que_wrap_n>;

	private:

		/// head and tail nodes
		static inline const auto _node_location__head{ _a_node_location(0, SIZE_MAX - 1) };
		static inline const auto _node_location__tail{ _a_node_location(ANCESTOR_COUNT__EXCEED, SIZE_MAX) };

        /// members
		mutable std::mutex _mutex;
		std::atomic<bool> _valid_state{ true };
		_a_nodes_0 _nodes_0{};
        _a_nodes_1 _nodes_1{};
        _a_nodes_2 _nodes_2{};
        _a_nodes_3 _nodes_3{};
        _a_nodes_4 _nodes_4{};
        _a_nodes_5 _nodes_5{};
        _a_nodes_6 _nodes_6{};
        _a_nodes_7 _nodes_7{};
        _a_nodes_8 _nodes_8{};
        _a_nodes_n _nodes_n{};
		std::array<std::vector<EDAG_node_states>, NODE_CONTAINER_COUNT> _DAG_node_states{ {} };
		std::array<std::vector<std::vector<_a_node_location>>, NODE_CONTAINER_COUNT> _node_locations__descendant{ {} };
		std::vector<_a_node_location> _node_locations__head_descendant{};
		std::vector<_a_node_location> _node_locations__tail_ancestor{};
		std::unordered_set<_a_node_location, pair_hasher, pair_keyeq> _node_locations__deleted;
		std::unordered_map<
			_a_node_location,
			std::vector<_a_node_location>,
			pair_hasher,
			pair_keyeq> _node_locations__cycled;
		std::array<_a_node_container_var_raw, NODE_CONTAINER_COUNT> _node_containers{ {} };
		std::array<VectorTree<T>, NODE_CONTAINER_COUNT> _datas{ {} };





		/*!
		 * base class for STL style iterators
		 *   - supports both BFS and DFS
		 *   - STL style iterator classes are bidirectional iterators
		 *     provided by the direction type template parameter (ancestor or descendant)
		 *   - performs looping instead of recursion
		 *     as the DAG is a large structure which may result stack overflow
		 *   - _nodes__visited member deals with the problem
		 *     caused by the many-to-one relationship
		 *   - as i work with indices instead of the pointers
		 *     _nodes__visited can be defined by a std::vector instead of a std::map
		 *     which provides a constant time access to the cycled indices.
		 *     on the other hand, a std::map would suffer from rehashing
		 *     or hash collisions when the DAG gets large (e.g. with a million nodes).
		 * 
		 * as specified in the main documentation of this header file,
		 * there are two main issues to be solved for a persistant DAG:
		 *     1. copy constructor performance
		 *     2. BFS/DFS traversal performance
		 * the approach in this DAG implementation is as follows:
		 *     1. define and store all data in DAG class
		 *         * node relations: ancestor/descendant
		 *         * node states
		 *         * etc.
		 *     2. do not define a node class but relate each data via indices:
		 *         ancestor nodes of node i: ancestor_nodes[i]
		 *         descendant nodes of node i: descendant_nodes[i]
		 *         state of node i: states[i]
		 * this approach increases the effectivity of the copy constructor
		 * but creates and additional indirection while traversing the DAG.
		 * however, as specified above,
		 * working with indices allows using
		 * std::vector instead of std::map for the cycled indices
		 * which ensures constant time access.
		 * see above for the details.
		 * 
		 * see the main documentation of this header file for the details
		 * 
		 * CAUTION:
		 *     the constructor, update_node_pool and set_next_node may throw.
		 *     hence, the increment and decrement operators of the iterators may throw.
		 *     the strong exception safety must be considered while using the iterators
		 */
		template <typename DAG_type, typename traversal_type, typename direction_type>
		struct DAG_iterator_base
		{
			friend class PersistentDAG_2<T>;
			friend class DAG_const_iterator<DAG_type, traversal_type, direction_type>;
			friend class DAG_iterator<DAG_type, traversal_type, direction_type>;

			/// local alises
			using _u_DAG_raw = DAG_type const*;
			using _u_pool_type = std::conditional_t<
				std::is_same_v<traversal_type, traversal_type_BFS>,
				std::queue<_a_que_wrap_var_val>,
				std::stack<_a_que_wrap_var_val>>;

			/// members
			std::array<std::vector<bool>, NODE_CONTAINER_COUNT> _nodes__visited;
			_u_DAG_raw _DAG_raw{};
			_a_node_location _node_location;
			_u_pool_type _node_pool;
			/*!
			 * this member provides skiping the children
			 * (i.e. descendant nodes if the iteration is in descendant direction).
			 * the iteration neglects/skips the path stating from the current node (i.e. _node_location).
			 * do not reset the value to false
			 * after skipping a path when required.
			 * otherwise the iteration will skip all paths
			 * 
			 * see the documentation of DAG::propogate_DAG_node_state
			 * for an example usage
			 */
			bool _skip_remaining{};

			void allocate_nodes__visited(_u_DAG_raw DAG_raw) {
				_nodes__visited[0] = std::vector<bool>(DAG_raw->_nodes_0.size());
				_nodes__visited[1] = std::vector<bool>(DAG_raw->_nodes_1.size());
				_nodes__visited[2] = std::vector<bool>(DAG_raw->_nodes_2.size());
				_nodes__visited[3] = std::vector<bool>(DAG_raw->_nodes_3.size());
				_nodes__visited[4] = std::vector<bool>(DAG_raw->_nodes_4.size());
				_nodes__visited[5] = std::vector<bool>(DAG_raw->_nodes_5.size());
				_nodes__visited[6] = std::vector<bool>(DAG_raw->_nodes_6.size());
				_nodes__visited[7] = std::vector<bool>(DAG_raw->_nodes_7.size());
				_nodes__visited[8] = std::vector<bool>(DAG_raw->_nodes_8.size());
				_nodes__visited[ANCESTOR_COUNT__EXCEED] = std::vector<bool>(DAG_raw->_nodes_n.size());
			};

			DAG_iterator_base() noexcept = default;
			template <typename direction_type2 = direction_type>
				requires (std::is_same_v<direction_type2, direction_type_ancestor>)
			explicit DAG_iterator_base(_u_DAG_raw DAG_raw) : _DAG_raw(DAG_raw)
			{
				allocate_nodes__visited(DAG_raw);
				auto& node_locations__tail_ancestor{ DAG_raw->_node_locations__tail_ancestor };
				if (node_locations__tail_ancestor.empty()) {
					_node_location = _DAG_raw->get_end_node<direction_type2>();
				}
				else {
					_a_que_wrap_n que_wrap{ &node_locations__tail_ancestor };
					_node_pool.emplace(que_wrap);
					_node_location = node_locations__tail_ancestor[0];
					_nodes__visited[_node_location.first][_node_location.second] = true;
				}
			};
			template <typename direction_type2 = direction_type>
				requires (std::is_same_v<direction_type2, direction_type_descendant>)
			explicit DAG_iterator_base(_u_DAG_raw DAG_raw) : _DAG_raw(DAG_raw)
			{
				allocate_nodes__visited(DAG_raw);
				auto& node_locations__head_descendant{ DAG_raw->_node_locations__head_descendant };
				if (node_locations__head_descendant.empty()) {
					_node_location = _DAG_raw->get_end_node<direction_type2>();
				}
				else {
					_a_que_wrap_n que_wrap{ &node_locations__head_descendant };
					_node_pool.emplace(que_wrap);
					_node_location = node_locations__head_descendant[0];
					_nodes__visited[_node_location.first][_node_location.second] = true;
				}
			};
			DAG_iterator_base(_u_DAG_raw DAG_raw, _a_node_location node_location) noexcept
				:
				_DAG_raw(DAG_raw),
				_node_location{ node_location }
			{
				allocate_nodes__visited(DAG_raw);
				_nodes__visited[node_location.first][node_location.second] = true;
			};
			DAG_iterator_base(_u_DAG_raw DAG_raw, _a_node_location node_location, bool) noexcept
				:
				_DAG_raw(DAG_raw),
				_node_location{ node_location } {};

			/*!
			 * @brief updates the iterator pool
			 */
			template <typename direction_type2 = direction_type>
			inline void update_node_pool() {
				auto& node_locations__ancestor__var_raw = _DAG_raw->get_node_locations__ancestor__var_raw(_node_location);
				if (node_locations__ancestor__var_raw) {
					auto que_wrap = std::visit(
						[](auto arg) { return _a_que_wrap_var_val(arg); },
						node_locations__ancestor__var_raw.value());
					_node_pool.push(que_wrap);
				}
			};
			template <>
			inline void update_node_pool<direction_type_descendant>() {
				const auto& node_locations__descendant = _DAG_raw->get_descendant_nodes__const(_node_location);
				if (!node_locations__descendant.empty()) {
					_a_que_wrap_n que_wrap{ &node_locations__descendant };
					_node_pool.emplace(que_wrap);
				}
			};

			[[nodiscard]] inline _a_que_wrap_var_val& get_next_from_pool(
				std::stack<_a_que_wrap_var_val>& container)
			{
				return container.top();
			};
			[[nodiscard]] inline _a_que_wrap_var_val& get_next_from_pool(
				std::queue<_a_que_wrap_var_val>& container)
			{
				return container.front();
			};

			/*!
			 * @brief this method sets the next node
			 * no recursion in order to reduce the stack usage
			 */
			void set_next_node()
			{
				if (
					_DAG_raw->get_state(_node_location) !=
					EDAG_node_states::cycled && !_skip_remaining)
				{
					this->update_node_pool<direction_type>();
				}
				while (!_node_pool.empty()) {
					while (!_node_pool.empty()) {
						const auto& current_que_wrap_var_val{ get_next_from_pool(_node_pool) };
						auto empty_ = std::visit(
							[](const auto& arg) { return arg.empty(); },
							current_que_wrap_var_val);
						if (!empty_) { break; }
						_node_pool.pop();
					}
					if (_node_pool.empty()) {
						_node_location = _DAG_raw->get_end_node<direction_type>();
						return;
					}

					auto& current_que_wrap_var_val{ get_next_from_pool(_node_pool) };
					_node_location = std::visit(
						[](auto& arg) { return *arg.front(); },
						current_que_wrap_var_val);
					if (!_nodes__visited[_node_location.first][_node_location.second]) {
						_nodes__visited[_node_location.first][_node_location.second] = true;
						return;
					}
				}
				_node_location = _DAG_raw->get_end_node<direction_type>();
			};
		};

	public:

		/*!
		 * The STL style const iterator class
		 * 
		 * template parameters:
		 *     traversal_type == traversal_type_BFS || traversal_type == traversal_type_DFS
		 *     direction_type == direction_type_ancestor || direction_type == direction_type_descendant
		 * 
		 * see DAG_iterator_base for the details
		 * 
		 * Base template for:
		 *     direction_type == direction_type_ancestor
		 */
		template <typename DAG_type, typename traversal_type, typename direction_type>
		class DAG_const_iterator
			: public DAG_iterator_base<DAG_type, traversal_type, direction_type>
		{
			friend class PersistentDAG_2<T>;

			/// local alises
			using _u_base = DAG_iterator_base<DAG_type, traversal_type, direction_type>;

		public:

			using _u_base::_u_base;

			// STL aliases
			using iterator_category = std::bidirectional_iterator_tag;
			using value_type = typename DAG_type::value_type;
			using difference_type = typename DAG_type::difference_type;
			using pointer = typename DAG_type::const_pointer;
			using reference = const value_type&;

			[[nodiscard]] inline reference operator*() const noexcept {
				return this->_DAG_raw->get_data(this->_node_location);
			};

			[[nodiscard]] inline pointer operator->() const noexcept {
				return &this->_DAG_raw->get_data(this->_node_location);
			};

			inline DAG_const_iterator& operator++() noexcept {
				_u_base::set_next_node();
				return *this;
			};

			[[nodiscard]] inline DAG_const_iterator operator++(int) noexcept {
				DAG_const_iterator _temp{ *this };
				++*this;
				return _temp;
			};

			[[nodiscard]] inline DAG_const_iterator operator+(const difference_type offset) noexcept {
				DAG_const_iterator _temp{ *this };
				_temp += offset;
				return _temp;
			};

			DAG_const_iterator& operator+=(const difference_type offset) noexcept {
				auto node_location__end{ this->_DAG_raw->get_end_node<direction_type>() };
				for (std::ptrdiff_t i = 0; i < offset; ++i) {
					if (this->_node_location == node_location__end) { return *this; }
					operator++();
				}
				return *this;
			};

			[[nodiscard]] inline bool operator==(const DAG_const_iterator& rhs) const noexcept {
				return this->_DAG_raw == rhs._DAG_raw && this->_node_location == rhs._node_location;
			};

			[[nodiscard]] inline bool operator!=(const DAG_const_iterator& rhs) const noexcept {
				return !(*this == rhs);
			};
		};






	private:

		/*!
		 * The STL style iterator class
		 * Follows std::vector::iterator approach which bases std::vector::const_iterator
		 * 
		 * See the documentation of DAG_const_iterator for the details
		 * 
		 * Notice that the non-const iterator is private
		 * to keep the persistency of DAG.
		 */
		template <typename DAG_type, typename traversal_type, typename direction_type>
		class DAG_iterator
			: public DAG_const_iterator<DAG_type, traversal_type, direction_type>
		{
			friend class PersistentDAG_2<T>;

		public:

			// STL aliases
			using iterator_category = std::bidirectional_iterator_tag;
			using value_type = typename DAG_type::value_type;
			using difference_type = typename DAG_type::difference_type;
			using pointer = typename DAG_type::pointer;
			using reference = value_type&;

			/// local alises
			using _u_base = DAG_const_iterator<DAG_type, traversal_type, direction_type>;

			// Constructors
			using _u_base::_u_base;

			[[nodiscard]] inline reference operator*() const noexcept {
				return const_cast<reference>(_u_base::operator*());
			};

			[[nodiscard]] inline pointer operator->() const noexcept {
				return const_cast<pointer>(_u_base::operator->());
			};

			inline DAG_iterator& operator++() noexcept {
				_u_base::operator++();
				return *this;
			};

			[[nodiscard]] inline DAG_iterator operator++(int) noexcept {
				DAG_iterator _temp{ *this };
				_u_base::operator++();
				return _temp;
			};

			[[nodiscard]] inline DAG_iterator operator+(const difference_type offset) const noexcept {
				DAG_iterator _temp{ *this };
				_u_base::operator+=(offset);
				return _temp;
			};

			inline DAG_iterator& operator+=(const difference_type offset) noexcept {
				_u_base::operator+=(offset);
				return *this;
			};

			[[nodiscard]] inline bool operator==(const DAG_iterator& rhs) const noexcept {
				return this->_DAG_raw == rhs._DAG_raw && this->_node_location == rhs._node_location;
			};

			[[nodiscard]] inline bool operator!=(const DAG_iterator& rhs) const noexcept {
				return !(*this == rhs);
			};
		};





	public:

		// STL aliases
		using value_type = T;
		using allocator_type = std::allocator<T>;
		using pointer = value_type*;
		using const_pointer = const value_type*;
		using iterator = DAG_iterator<PersistentDAG_2<T>, traversal_type_DFS, direction_type_ancestor>;
		using const_iterator = DAG_const_iterator<PersistentDAG_2<T>, traversal_type_DFS, direction_type_ancestor>;
		using reference = T&;
		using const_reference = const T&;
		using size_type = std::size_t;
		using difference_type = std::ptrdiff_t;





	private:

		/// local alises
		template <typename traversal_type, typename direction_type>
		using iterator_type = DAG_iterator<PersistentDAG_2<T>, traversal_type, direction_type>;
		template <typename traversal_type, typename direction_type>
		using const_iterator_type = DAG_const_iterator<PersistentDAG_2<T>, traversal_type, direction_type>;
		using iterator_BFS_ancestor = iterator_type<traversal_type_BFS, direction_type_ancestor>;
		using iterator_BFS_descendant = iterator_type<traversal_type_BFS, direction_type_descendant>;
		using iterator_DFS_ancestor = iterator_type<traversal_type_DFS, direction_type_ancestor>;
		using iterator_DFS_descendant = iterator_type<traversal_type_DFS, direction_type_descendant>;
		using const_iterator_BFS_ancestor = const_iterator_type<traversal_type_BFS, direction_type_ancestor>;
		using const_iterator_BFS_descendant = const_iterator_type<traversal_type_BFS, direction_type_descendant>;
		using const_iterator_DFS_ancestor = const_iterator_type<traversal_type_DFS, direction_type_ancestor>;
		using const_iterator_DFS_descendant = const_iterator_type<traversal_type_DFS, direction_type_descendant>;

		/*!
		 * non-const iterators
		 * internal usage only
		 * not noexcept as the iterator constructor may throw bad_alloc
		 */
		template <typename traversal_type = traversal_type_BFS>
		[[nodiscard]] inline iterator_type<traversal_type, direction_type_descendant> begin() {
			return iterator_type<traversal_type, direction_type_descendant>(this);
		};
		template <typename traversal_type = traversal_type_BFS>
		[[nodiscard]] inline iterator_type<traversal_type, direction_type_descendant> end() {
			return iterator_type<traversal_type, direction_type_descendant>(this, _node_location__tail, true);
		};
		template <typename traversal_type = traversal_type_BFS>
		[[nodiscard]] inline iterator_type<traversal_type, direction_type_ancestor> rbegin() {
			return iterator_type<traversal_type, direction_type_ancestor>(this);
		};
		template <typename traversal_type = traversal_type_BFS>
		[[nodiscard]] inline iterator_type<traversal_type, direction_type_ancestor> rend() {
			return iterator_type<traversal_type, direction_type_ancestor>(this, _node_location__head, true);
		};





		/*!
		 * @brief sets the array containing the raw pointers to the node containers
		 */
		void set_node_container_pointer_array()
		{
			_node_containers[0] = &_nodes_0;
			_node_containers[1] = &_nodes_1;
			_node_containers[2] = &_nodes_2;
			_node_containers[3] = &_nodes_3;
			_node_containers[4] = &_nodes_4;
			_node_containers[5] = &_nodes_5;
			_node_containers[6] = &_nodes_6;
			_node_containers[7] = &_nodes_7;
			_node_containers[8] = &_nodes_8;
			_node_containers[ANCESTOR_COUNT__EXCEED] = &_nodes_n;
		};

		/*!
		 * @brief gets raw pointer to the node container
		 */
		[[nodiscard]] inline auto get_node_container(
			unsigned char node_location__first) const
			-> _a_node_container_var_raw
		{
			return _node_containers[node_location__first];
		};

		/*!
		 * @brief gets raw pointer to the node
		 */
		[[nodiscard]] inline auto get_node(
			_a_node_location node_location) const
			-> _a_node_var_raw
		{
			auto node_container_var_raw = get_node_container(node_location.first);
			return std::visit(
				[&node_location](auto arg1) {
					auto& node_var_val{ arg1->operator[](node_location.second) };
					return std::visit(
						[](auto& arg2) { return _a_node_var_raw(&arg2); },
						node_var_val);
				},
				node_container_var_raw);
		};

		/*!
		 * @brief gets raw pointer to the node
		 */
		[[nodiscard]] inline auto get_node__last(
			unsigned char node_location__first)
			-> _a_node_var_raw
		{
			auto node_container_var_raw = get_node_container(node_location__first);
			auto node_location__second__last = std::visit(
				[](auto arg) { return arg->size() - 1; },
				node_container_var_raw);
			return std::visit(
				[&node_location__second__last](auto arg1) {
					auto& node_var_val{ arg1->operator[](node_location__second__last) };
					return std::visit(
						[](auto& arg2) { return _a_node_var_raw(&arg2); },
						node_var_val);
				},
				node_container_var_raw);
		};

		/*!
		 * @brief gets location of the node in the containers
		 */
		[[nodiscard]] inline auto get_node_location(
			_a_node_var_raw node_var_raw) const
			-> _a_node_location
		{
			auto ancestor_count = std::visit(
				[](auto arg) { return arg->get_ancestor_count(); },
				node_var_raw);
			auto node_container_var_raw = get_node_container(ancestor_count);
			auto node_location__second = std::visit(
				[&node_container_var_raw](auto arg1) {
					return std::visit(
						[arg1](auto arg2) { return std::distance(arg2->data(), arg1); },
						node_container_var_raw); },
				node_var_raw);
			return _a_node_location(ancestor_count, node_location__second);
		};

		/*!
		 * @brief gets location of the nodes
		 */
		template <typename ancestor_count_type>
		[[nodiscard]] inline auto convert_nodes_into_node_locations(
			const _a_nodes_var_raw<ancestor_count_type::value>& nodes_var_raw) const
			-> _a_node_locations__cond<ancestor_count_type::value>
		{
			_a_node_locations__cond<ancestor_count_type::value> node_locations;
			for (unsigned char i = 0; i < nodes_var_raw.size(); i++) {
				node_locations[i] = get_node_location(nodes_var_raw[i]);
			}
			return node_locations;
		};

		/*!
		 * @brief gets location of the nodes
		 */
		template <>
		[[nodiscard]] inline auto convert_nodes_into_node_locations<_a_integral<ANCESTOR_COUNT__EXCEED>>(
			const _a_nodes_var_raw<ANCESTOR_COUNT__EXCEED>& nodes_var_raw) const
			-> _a_node_locations__cond<ANCESTOR_COUNT__EXCEED>
		{
			_a_node_locations__cond<ANCESTOR_COUNT__EXCEED> node_locations{};
			for (auto node_var_raw : nodes_var_raw) {
				node_locations.push_back(get_node_location(node_var_raw));
			}
			return node_locations;
		};
		
		/*!
		 * @brief gets the state of the node
		 */
		[[nodiscard]] inline auto get_state(
			_a_node_var_raw node_var_raw) const
			-> EDAG_node_states
		{
			return get_state(get_node_location(node_var_raw));
		};

		/*!
		 * @brief gets the state of the node
		 */
		[[nodiscard]] inline auto get_state(
			const _a_node_location& node_location) const
			-> EDAG_node_states
		{
			return _DAG_node_states[node_location.first][node_location.second];
		};

		/*!
		 * @brief gets the ancestors of the node
		 */
		[[nodiscard]] inline auto get_node_locations__ancestor__var_raw(
			_a_node_location node_location)
			-> std::optional<_a_node_locations__var_raw>
		{
			auto node_var_raw{ get_node(node_location) };
			return std::visit(
				[](auto arg) { return arg->get_node_locations__ancestor__var_raw(); },
				node_var_raw);
		};

		/*!
		 * @brief gets the descendants of the node given by a pair defining its location
		 */
		[[nodiscard]] inline auto get_node_locations__descendant(
			_a_node_location node_location)
			-> std::vector<_a_node_location>&
		{
			return _node_locations__descendant[node_location.first][node_location.second];
		};

		/*!
		 * @brief gets the descendants of the node given by a pair defining its location
		 */
		[[nodiscard]] inline auto get_descendant_nodes__const(
			_a_node_location node_location) const
			-> std::vector<_a_node_location> const&
		{
			return _node_locations__descendant[node_location.first][node_location.second];
		};

		/*!
		 * @brief gets the data stored in the node given by a pair defining its location
		 */
		[[nodiscard]] inline auto get_data(_a_node_location node_location) const -> T const& {
			return _datas[node_location.first][node_location.second];
		};

		/*!
		 * @brief append the node into the descendants of the ancestors
		 */
		template <unsigned char ancestor_count>
		void append_to_descendants_of_ancestors(
			_a_node_location node_location,
			const _a_node_locations__cond<ancestor_count>& node_locations__ancestor__cond)
		{
			for (unsigned char i = 0; i < ancestor_count; ++i) {
				auto& node_locations__descendant{ get_node_locations__descendant(node_locations__ancestor__cond[i]) };
				node_locations__descendant.push_back(node_location);
			}
		};

		/*!
		 * @brief erase the node from the descendants of the ancestors - 1
		 */
		inline void erase_from_descendants_of_ancestors_1(
			_a_node_location node_location,
			_a_node_locations__var_raw node_locations__ancestor__var_raw)
		{
			std::visit(
				[&node_location, this](auto arg) {
					for (unsigned char i = 0; i < arg->size(); ++i) {
						auto& node_locations__descendant{ this->get_node_locations__descendant(arg->operator[](i)) };
						auto it = std::find(
							node_locations__descendant.begin(),
							node_locations__descendant.end(),
							node_location);
						node_locations__descendant.erase(it);
					}
				},
			node_locations__ancestor__var_raw);
		};
		inline void erase_from_descendants_of_ancestors_2(
			_a_node_location node_location)
		{
			auto node_locations__ancestor__var_raw{ get_node_locations__ancestor__var_raw(node_location) };
			if (!node_locations__ancestor__var_raw) {
				throw std::logic_error("node type without ancestors.");
			}
			erase_from_descendants_of_ancestors_1(
				node_location,
				node_locations__ancestor__var_raw.value());
		};

		/*!
		 * @brief gets the end node of the iteration
		 * Base template for
		 *     direction_type == direction_type_ancestor
		 */
		template <typename direction_type>
		[[nodiscard]] inline _a_node_location get_end_node() const noexcept
		{
			return _node_location__head;
		};

		/*!
		 * @brief gets the end node of the iteration
		 * Specialization for
		 *     direction_type == direction_type_descendant
		 */
		template <>
		[[nodiscard]] inline _a_node_location get_end_node<direction_type_descendant>() const noexcept {
			return _node_location__tail;
		};




		/*!
		 * @brief a helper function to copy all members accept for the datas into the new DAG
		 * may throw
		 */
		void copy_members_accept_for_datas(PersistentDAG_2<T>& new_DAG, unsigned char container_index) const
		{
			new_DAG._nodes_0 = _nodes_0;
			new_DAG._nodes_1 = _nodes_1;
			new_DAG._nodes_2 = _nodes_2;
			new_DAG._nodes_3 = _nodes_3;
			new_DAG._nodes_4 = _nodes_4;
			new_DAG._nodes_5 = _nodes_5;
			new_DAG._nodes_6 = _nodes_6;
			new_DAG._nodes_7 = _nodes_7;
			new_DAG._nodes_8 = _nodes_8;
			new_DAG._nodes_n = _nodes_n;
			new_DAG._node_locations__descendant = _node_locations__descendant;
			new_DAG._node_locations__head_descendant = _node_locations__head_descendant;
			new_DAG._node_locations__tail_ancestor = _node_locations__tail_ancestor;
			new_DAG._node_locations__deleted = _node_locations__deleted;
			new_DAG._node_locations__cycled = _node_locations__cycled;
			for (unsigned char i = 0; i < NODE_CONTAINER_COUNT; ++i) {
				if (i != container_index) {
					new_DAG._datas[i] = _datas[i];
				}
			}
			new_DAG._node_containers[0] = &new_DAG._nodes_0;
			new_DAG._node_containers[1] = &new_DAG._nodes_1;
			new_DAG._node_containers[2] = &new_DAG._nodes_2;
			new_DAG._node_containers[3] = &new_DAG._nodes_3;
			new_DAG._node_containers[4] = &new_DAG._nodes_4;
			new_DAG._node_containers[5] = &new_DAG._nodes_5;
			new_DAG._node_containers[6] = &new_DAG._nodes_6;
			new_DAG._node_containers[7] = &new_DAG._nodes_7;
			new_DAG._node_containers[8] = &new_DAG._nodes_8;
			new_DAG._node_containers[ANCESTOR_COUNT__EXCEED] = &new_DAG._nodes_n;

			std::scoped_lock l(_mutex);
			new_DAG._DAG_node_states = _DAG_node_states;

			auto valid_state{ _valid_state.load(std::memory_order_acquire) };
			new_DAG._valid_state.store(valid_state, std::memory_order_relaxed);
		};





		/*!
		 * DAG topological comparison - less than
		 * notice that two nodes do not have to be related.
		 * in other words, Not(a < b) does not imply a > b.
		 * 
		 * by definition a < b if a can be reached
		 * by a traversal in ancestor direction starting from b.
		 * 
		 * use DFS traversal as the memory usage is less for DFS comparingly
		 * and the runtime performance is same as the DAG is unweighted.
		 * 
		 * use a traversal in descendant direction
		 * as the user would mostly work close to the tail
		 * 
		 * the iterator may throw
		 */
		[[nodiscard]] bool inspect_topologically_less(
			_a_node_location node_location__ancestor,
			_a_node_location node_location__descendant) const
		{
			auto it{ const_iterator_DFS_descendant(this, node_location__ancestor) };
			auto ite{ cend<traversal_type_DFS>() };
			auto itf{ const_iterator_DFS_descendant(this, node_location__descendant) };
			for (; it != ite; ++it) {
				if (it == itf) { return true; }
			}
			return false;
		};

		/*!
		 * DAG topological comparison - greater than
		 * see the documentation of inspect_topologically_less for the details
		 * 
		 * use DFS traversal as the memory usage is less for DFS comparingly
		 * and the runtime performance is same as the DAG is unweighted.
		 * 
		 * use a traversal in descendant direction
		 * as the user would mostly work close to the tail
		 * 
		 * the iterator may throw
		 */
		[[nodiscard]] bool inspect_topologically_greater(
			_a_node_location node_location__descendant,
			_a_node_location node_location__ancestor) const
		{
			auto it{ const_iterator_DFS_descendant(this, node_location__ancestor) };
			auto ite{ cend<traversal_type_DFS>() };
			auto itf{ const_iterator_DFS_descendant(this, node_location__descendant) };
			for (; it != ite; ++it) {
				if (it == itf) { return true; }
			}
			return false;
		};

		/*!
		 * inspect if a directed cycle is formed by the input nodes.
		 * 
		 * start the iteration from the input descendant
		 * and perform the iteration in the descendant direction
		 * in order to reduce the iteration length.
		 * because, if there is no cycle,
		 * the iteration will terminate at the end (i.e. tail for descendant traversal)
		 * and mostly the user works close to the tail node.
		 * 
		 * the iterator may throw
		 */
		[[nodiscard]] bool inspect_directed_cycle(
			_a_node_location node_location__ancestor,
			_a_node_location node_location__descendant) const
		{
			return inspect_topologically_less(node_location__descendant, node_location__ancestor);
		};





		/*!
		 * update the state of a node
		 * when the ancestor nodes has changed.
		 * the state change is propogated through the descendant paths.
		 * 
		 * may throw as propogate_DAG_node_state may throw
		 */
		void inline update_DAG_node_state(_a_node_location node_location)
		{
			auto node_var_raw{ get_node(node_location) };
			const auto& data{ get_data(node_location) };
			auto state__initial{ _DAG_node_states[node_location.first][node_location.second] };
			auto state__final = std::visit(
				[this, &data, state__initial](auto arg) {
					return arg->get_final_state__default(
						this,
						&data,
						state__initial); },
				node_var_raw);
			if (state__final != EDAG_node_states::uptodate) { _valid_state = false; }
			if (state__final != state__initial) { propogate_DAG_node_state(node_location); }
		};

		/*!
		 * propogates the change in the state of a node through the descendant nodes
		 * 
		 * uses BFS traversal as it propogates faster
		 * 
		 * uses _skip_remaining member of the iterator
		 * to skip a path in the graph if required:
		 *     consider the state of a node A has changed
		 *     and requires an iteration to propogate the state change.
		 *     consider node A has two descendant nodes: nodes B and C.
		 *     consider node B has two descendant nodes: nodes D and E.
		 *     assume the state of node B does not change
		 *     when the iteration reaches node B.
		 *     there is no need to continue the iteration from node B
		 *     to inspect nodes D and E
		 *     as the state of node B remains the same.
		 *     so the iteration will skip the descendant nodes of the node B
		 *     and continue with node C.
		 * 
		 * the iterator may throw bad_alloc
		 */
		void inline propogate_DAG_node_state(_a_node_location node_location__propogate)
		{
			auto it{ iterator_BFS_descendant(this, node_location__propogate) };
			auto ite{ end<traversal_type_BFS>() };
			auto state__propogate{ get_state(node_location__propogate) };
			for (; it != ite; ++it) {
				auto& node_location{ it._node_location };
				const auto& data{ get_data(node_location) };
				auto node_var_raw{ get_node(node_location) };
				auto state__initial{_DAG_node_states[node_location.first][node_location.second]};
				auto state__final = std::visit(
					[this, &data, state__initial, state__propogate](auto arg) {
						return arg->get_final_state__propogate(
							this,
							&data,
							state__initial,
							state__propogate); },
					node_var_raw);
				if (state__final != EDAG_node_states::uptodate) { _valid_state = false; }
				if (state__final == state__initial) { it._skip_remaining = true; }
				else { it._skip_remaining = false; }
			}
		};







		/*!
		 * helper class for insert and emplace operations
		 * notice that ancestor_count_type cannot be used to
		 * create a statically defined function family
		 * as its a runtime issue for insert and emplace functions.
		 * Hence, ancestor_count_type is defined by the function overloading.
		 */
		template <typename DAG_node_type>
		auto inline append_node_helper__dynamic()
			-> std::size_t
		{
			_nodes_0.emplace_back(DAG_node<DAG_node_type, _a_integral<0>>());
			return _nodes_0.size() - 1;
		};
		template <typename DAG_node_type>
		auto inline append_node_helper__dynamic(
			const std::array<_a_node_location, 1>& ancestor_nodes)
			-> std::size_t
		{
			_nodes_1.emplace_back(DAG_node<DAG_node_type, _a_integral<1>>(ancestor_nodes));
			return _nodes_1.size() - 1;
		};
		template <typename DAG_node_type>
		auto inline append_node_helper__dynamic(
			const std::array<_a_node_location, 2>& ancestor_nodes)
			-> std::size_t
		{
			_nodes_2.emplace_back(DAG_node<DAG_node_type, _a_integral<2>>(ancestor_nodes));
			return _nodes_2.size() - 1;
		};
		template <typename DAG_node_type>
		auto inline append_node_helper__dynamic(
			const std::array<_a_node_location, 3>& ancestor_nodes)
			-> std::size_t
		{
			_nodes_3.emplace_back(DAG_node<DAG_node_type, _a_integral<3>>(ancestor_nodes));
			return _nodes_3.size() - 1;
		};
		template <typename DAG_node_type>
		auto inline append_node_helper__dynamic(
			const std::array<_a_node_location, 4>& ancestor_nodes)
			-> std::size_t
		{
			_nodes_4.emplace_back(DAG_node<DAG_node_type, _a_integral<4>>(ancestor_nodes));
			return _nodes_4.size() - 1;
		};
		template <typename DAG_node_type>
		auto inline append_node_helper__dynamic(
			const std::array<_a_node_location, 5>& ancestor_nodes)
			-> std::size_t
		{
			_nodes_5.emplace_back(DAG_node<DAG_node_type, _a_integral<5>>(ancestor_nodes));
			return _nodes_5.size() - 1;
		};
		template <typename DAG_node_type>
		auto inline append_node_helper__dynamic(
			const std::array<_a_node_location, 6>& ancestor_nodes)
			-> std::size_t
		{
			_nodes_6.emplace_back(DAG_node<DAG_node_type, _a_integral<6>>(ancestor_nodes));
			return _nodes_6.size() - 1;
		};
		template <typename DAG_node_type>
		auto inline append_node_helper__dynamic(
			const std::array<_a_node_location, 7>& ancestor_nodes)
			-> std::size_t
		{
			_nodes_7.emplace_back(DAG_node<DAG_node_type, _a_integral<7>>(ancestor_nodes));
			return _nodes_7.size() - 1;
		};
		template <typename DAG_node_type>
		auto inline append_node_helper__dynamic(
			const std::array<_a_node_location, 8>& ancestor_nodes)
			-> std::size_t
		{
			_nodes_8.emplace_back(DAG_node<DAG_node_type, _a_integral<8>>(ancestor_nodes));
			return _nodes_8.size() - 1;
		};
		template <typename DAG_node_type>
		auto inline append_node_helper__dynamic(
			const std::vector<_a_node_location>& ancestor_nodes)
			-> std::size_t
		{
			_nodes_n.emplace_back(DAG_node<DAG_node_type, _a_integral<ANCESTOR_COUNT__EXCEED>>(ancestor_nodes));
			return _nodes_n.size() - 1;
		};

		/*!
		* helper function to erase an element
		* see the documentation of erase for the details
		*
		* may throw
		*/
		template <unsigned char ancestor_count>
		void erase_mutate(
			_a_node_location node_location__erase,
			const VectorTree<T>& datas__erase)
		{
			// erase the node from the descendants/ancestors of head/tail
			if (
				auto it = std::find(
					_node_locations__head_descendant.begin(),
					_node_locations__head_descendant.end(),
					node_location__erase);
				it != _node_locations__head_descendant.end())
			{
				_node_locations__head_descendant.erase(it);
			}
			if (
				auto it = std::find(
					_node_locations__tail_ancestor.begin(),
					_node_locations__tail_ancestor.end(),
					node_location__erase);
				it != _node_locations__tail_ancestor.end())
			{
				_node_locations__tail_ancestor.erase(it);
			}

			// remove the erased node from deleted and cycled nodes
			_node_locations__deleted.erase(node_location__erase);
			_node_locations__cycled.erase(node_location__erase);

			// remove the erased node from the descendants of the ancestors of the erased node
			erase_from_descendants_of_ancestors_2(node_location__erase);

			// cannot have descendant nodes to be erased
			;

			// use erase__by_swap_and_pop if the node to be erased
			// is not the last node in the container storing the input node
			// this function invalidates only the pointer to the last element
			// which can be corrected easily in contant time
			auto node_container_var_raw{ get_node_container(node_location__erase.first) };
			auto node_location__second__last = std::visit(
				[](auto arg) { return arg->size() - 1; },
				node_container_var_raw);

			auto node_var_raw__last = get_node__last(node_location__erase.first);
			auto ancestor_count__last = std::visit(
				[](auto arg) { return arg->get_ancestor_count(); },
				node_var_raw__last);
			auto node_location__last = _a_node_location(ancestor_count__last, node_location__second__last);
			auto& datas__erase__container{ _datas[node_location__erase.first] };
			if (node_location__second__last != node_location__erase.second) {
				// swap and pop - node container
				std::visit(
					[&node_location__erase](auto arg) {
						erase__by_swap_and_pop__std_vector__nonconst(
							*arg,
							node_location__erase.second); },
					node_container_var_raw);

				// swap and pop - data
				datas__erase__container = datas__erase.erase__by_swap_and_pop(
					node_location__erase.second);

				// swap and pop - states
				erase__by_swap_and_pop__std_vector__nonconst(
					_DAG_node_states[node_location__erase.first],
					node_location__erase.second);

				// swap and pop - descendants
				erase__by_swap_and_pop__std_vector__nonconst(
					_node_locations__descendant[node_location__erase.first],
					node_location__erase.second);

				// invalidated last node - descendants of the ancestors
				if (ancestor_count__last > 0) {
					erase_from_descendants_of_ancestors_2(node_location__last);
				}

				// invalidated last node - ancestors of the descendants
				for (
					const auto& descendant_nodes__last{ get_descendant_nodes__const(node_location__last) };
					const auto& node_location__descendant : descendant_nodes__last)
				{
					auto node_locations__ancestor__var_raw{ get_node_locations__ancestor__var_raw(node_location__descendant) };
					if (!node_locations__ancestor__var_raw) {
						throw std::logic_error("node type without ancestors.");
					}
					std::visit(
						[&node_location__erase, &node_location__last](auto arg) {
							auto it = std::find(
								arg->begin(),
								arg->end(),
								node_location__last);
							if (it != arg->end()) { *it = node_location__erase; }
						},
						node_locations__ancestor__var_raw.value());
				}

				// invalidated last node - cycled nodes
				if (_node_locations__cycled.contains(node_location__last)) {
					auto pair_ = _node_locations__cycled.extract(node_location__last);
					pair_.key() = node_location__erase;
					_node_locations__cycled.insert(std::move(pair_));
				}

				// invalidated last node - the DAG node pointer of the contained type
				auto node_var_raw__erase{ get_node(node_location__erase) };
				//datas__erase__container[node_location__erase.second].set_DAG_node(node_var_raw__erase);
			}
			else {
				// pop - node container
				std::visit(
					[](auto arg) { arg->pop_back(); },
					node_container_var_raw);

				// pop - data
				datas__erase__container = datas__erase.pop_back();

				// pop - states
				_DAG_node_states[node_location__erase.first].pop_back();

				// pop - descendants
				_node_locations__descendant[node_location__erase.first].pop_back();
			}
		};

		/*!
		 * helper function to replace the ancestor nodes of a node
		 * @exceptsafe strong exception safety is a direct result of the persistency.
		 */
		template <unsigned char ancestor_count>
		[[nodiscard]] auto replace_ancestors_helper(
			_a_node_location node_location,
			const T& data_new,
			const _a_node_locations__cond<ancestor_count>& node_locations__ancestor__cond__new) const
			-> PersistentDAG_2<T>
		{
			// create a new DAG
			PersistentDAG_2<T> new_DAG{};

			// set the data
			new_DAG._datas[node_location.first] = _datas[node_location.first].set_data(node_location, data_new);

			// copy other members
			copy_members_accept_for_datas(new_DAG, node_location.first);

			// modify the descendants of the current ancestor nodes
			auto node_locations__ancestor__var_raw__old{ new_DAG.node_locations__ancestor__var_raw(node_location) };
			if (!node_locations__ancestor__var_raw__old) {
				throw std::logic_error("node type without ancestors.");
			}
			std::visit(
				[&node_location](auto arg) {
					for (unsigned char i = 0; i < arg->size(); ++i) {
						auto& node_location__ancestor{ arg->operator[](i) };
						auto& node_locations__descendant{ new_DAG.get_node_locations__descendant(node_location__ancestor) };
						auto it = std::find(
							node_locations__descendant.begin(),
							node_locations__descendant.end(),
							node_location);
						if (it != node_locations__descendant.end()) { node_locations__descendant.erase(it); }
						if (node_locations__descendant.empty() &&
							new_DAG._node_locations__deleted.contains(node_location__ancestor))
						{
							new_DAG.erase_helper(node_location__ancestor);
						}
					}
				},
			node_locations__ancestor__var_raw__old.value());

			// modify the descendants of the new ancestor nodes
			// and also inspect the tail node
			for (auto node_location__ancestor : node_locations__ancestor__cond__new) {
				auto& node_locations__descendant{ new_DAG.get_node_locations__descendant(node_location__ancestor) };
				if (node_locations__descendant.empty()) {
					auto it = std::find(
						new_DAG._node_locations__tail_ancestor.begin(),
						new_DAG._node_locations__tail_ancestor.end(),
						node_location__ancestor);
					new_DAG._node_locations__tail_ancestor.erase(it);
				}
				node_locations__descendant.push_back(node_location);
			}

			// replace the ancestor nodes of the input node
			auto node_var_raw{ new_DAG.get_node(node_location) };
			std::visit(
				[&node_locations__ancestor__cond__new](auto arg) { arg->_ancestor_nodes = node_locations__ancestor__cond__new; },
				node_var_raw);

			// inspect tail nodes
			std::visit(
				[&node_location](auto arg) {
					for (unsigned char i = 0; i < arg->size(); ++i) {
						auto& node_location__ancestor{ arg->operator[](i) };
						auto& node_locations__descendant{ new_DAG.get_node_locations__descendant(node_location__ancestor) };
						if (node_locations__descendant.empty()) {
							new_DAG._node_locations__tail_ancestor.push_back(node_location__ancestor);
						}
					}
				},
			node_locations__ancestor__var_raw__old.value());

			// update the DAG_node_state
			new_DAG.update_DAG_node_state(node_location);

			// execute the background thread
			auto valid_state{ new_DAG._valid_state.load(std::memory_order_acquire) };
			if (!valid_state) {
				auto fut = std::async(&PersistentDAG_2::update_DAG_state, &new_DAG); // allow deferred execution
			}
			return new_DAG; // ensure NRVO due to single return
		};





    public:



		/*!
		 * default constructor
		 */
		PersistentDAG_2() {
			set_node_container_pointer_array();
		};

		/*!
		 * @brief copy constructor
		 * 
		 * @exceptsafe strong exception safety is satisfied by using temporaries.
		 * another method would be reserving the vectors
		 * before copying with std::copy.
		 * but that would require the contained type T
		 * to have a non-throwing copy constructor.
		 * 
		 * @see see the main documentation of this header file
		 * for a discussion on the performance of the copy constructor
		 */
		PersistentDAG_2(const PersistentDAG_2& rhs)
			:
			_datas(rhs._datas),
			_nodes_0(rhs._nodes_0),
			_nodes_1(rhs._nodes_1),
			_nodes_2(rhs._nodes_2),
			_nodes_3(rhs._nodes_3),
			_nodes_4(rhs._nodes_4),
			_nodes_5(rhs._nodes_5),
			_nodes_6(rhs._nodes_6),
			_nodes_7(rhs._nodes_7),
			_nodes_8(rhs._nodes_8),
			_nodes_n(rhs._nodes_n),
			_node_locations__descendant(rhs._node_locations__descendant),
			_node_locations__head_descendant(rhs._node_locations__head_descendant),
			_node_locations__tail_ancestor(rhs._node_locations__tail_ancestor),
			_node_locations__deleted(rhs._node_locations__deleted),
			_node_locations__cycled(rhs._node_locations__cycled)
		{
			set_node_container_pointer_array();

			// copy _DAG_node_states if the background thread has joined
			std::scoped_lock l(rhs._mutex);
			_DAG_node_states = rhs._DAG_node_states;

			auto valid_state{ rhs._valid_state.load(std::memory_order_acquire) };
			_valid_state.store(valid_state, std::memory_order_relaxed);
		};

		/*!
		 * @brief copy assignment
		 * @exceptsafe strong exception safety by copy-and-swap idiom
		 */
		PersistentDAG_2& operator=(PersistentDAG_2 rhs) {
			swap(*this, rhs);
			set_node_container_pointer_array();
			return *this;
		};

		/*!
		 * @brief Friend swap function for the copy-and-swap idiom
		 */
		friend inline void swap(PersistentDAG_2& lhs, PersistentDAG_2& rhs) noexcept {
			using std::swap;
			swap(lhs._node_locations__cycled, rhs._node_locations__cycled);
			swap(lhs._node_locations__deleted, rhs._node_locations__deleted);
			swap(lhs._valid_state, rhs._valid_state);
			swap(lhs._nodes_0, rhs._nodes_0);
			swap(lhs._nodes_1, rhs._nodes_1);
			swap(lhs._nodes_2, rhs._nodes_2);
			swap(lhs._nodes_3, rhs._nodes_3);
			swap(lhs._nodes_4, rhs._nodes_4);
			swap(lhs._nodes_5, rhs._nodes_5);
			swap(lhs._nodes_6, rhs._nodes_6);
			swap(lhs._nodes_7, rhs._nodes_7);
			swap(lhs._nodes_8, rhs._nodes_8);
			swap(lhs._nodes_n, rhs._nodes_n);
			swap(lhs._DAG_node_states, rhs._DAG_node_states);
			swap(lhs._node_locations__descendant, rhs._node_locations__descendant);
			swap(lhs._node_locations__head_descendant, rhs._node_locations__head_descendant);
			swap(lhs._node_locations__tail_ancestor, rhs._node_locations__tail_ancestor);
			swap(lhs._datas, rhs._datas);
		};

		/*!
		 * @brief move constructor
		 * 
		 * CAUTION:
		 * not default:
		 *     std::unordered_map does not have noexcept move constructor.
		 *     but it has a noexcept move assignment
		 */
		PersistentDAG_2(PersistentDAG_2&& rhs) noexcept {
			move_helper(std::move(rhs));
		};

		/*!
		 * @brief move assignment
		 */
		PersistentDAG_2& operator=(PersistentDAG_2&& rhs) noexcept {
			move_helper(std::move(rhs));
			return *this;
		};

		/*!
		 * @brief move helper
		 * all members including std::unordered_map has noexcept move assignment
		 */
		void move_helper(PersistentDAG_2&& rhs) noexcept {
			_node_locations__cycled = std::move(rhs._node_locations__cycled);
			_node_locations__deleted = std::move(rhs._node_locations__deleted);
			_nodes_0 = std::move(rhs._nodes_0);
			_nodes_1 = std::move(rhs._nodes_1);
			_nodes_2 = std::move(rhs._nodes_2);
			_nodes_3 = std::move(rhs._nodes_3);
			_nodes_4 = std::move(rhs._nodes_4);
			_nodes_5 = std::move(rhs._nodes_5);
			_nodes_6 = std::move(rhs._nodes_6);
			_nodes_7 = std::move(rhs._nodes_7);
			_nodes_8 = std::move(rhs._nodes_8);
			_nodes_n = std::move(rhs._nodes_n);
			_datas = std::move(rhs._datas);
			_node_locations__descendant = std::move(rhs._node_locations__descendant);
			_node_locations__head_descendant = std::move(rhs._node_locations__head_descendant);
			_node_locations__tail_ancestor = std::move(rhs._node_locations__tail_ancestor);
			set_node_container_pointer_array();

			// move _DAG_node_states if the background thread has joined
			std::scoped_lock l(rhs._mutex);
			_DAG_node_states = std::move(rhs._DAG_node_states);

			auto valid_state{ rhs._valid_state.load(std::memory_order_acquire) };
			_valid_state.store(valid_state, std::memory_order_relaxed);
		};

		~PersistentDAG_2() = default;

		[[nodiscard]] inline bool operator==(const PersistentDAG_2& rhs) const noexcept {
			return _datas == rhs._datas;
		};

		/*!
		 * @brief const iterators - cbegin
		 * @exceptsafe not noexcept as the iterator constructor may throw bad_alloc
		 */
		template <typename traversal_type = traversal_type_BFS>
		[[nodiscard]] inline const_iterator_type<traversal_type, direction_type_descendant> cbegin() const {
			return const_iterator_type<traversal_type, direction_type_descendant>(this);
		};
		/*!
		 * @brief const iterators - cend
		 * @exceptsafe not noexcept as the iterator constructor may throw bad_alloc
		 */
		template <typename traversal_type = traversal_type_BFS>
		[[nodiscard]] inline const_iterator_type<traversal_type, direction_type_descendant> cend() const {
			return const_iterator_type<traversal_type, direction_type_descendant>(this, _node_location__tail, true);
		};
		/*!
		 * @brief const iterators - crbegin
		 * @exceptsafe not noexcept as the iterator constructor may throw bad_alloc
		 */
		template <typename traversal_type = traversal_type_BFS>
		[[nodiscard]] inline const_iterator_type<traversal_type, direction_type_ancestor> crbegin() const {
			return const_iterator_type<traversal_type, direction_type_ancestor>(this);
		};
		/*!
		 * @brief const iterators - crend
		 * @exceptsafe not noexcept as the iterator constructor may throw bad_alloc
		 */
		template <typename traversal_type = traversal_type_BFS>
		[[nodiscard]] inline const_iterator_type<traversal_type, direction_type_ancestor> crend() const {
			return const_iterator_type<traversal_type, direction_type_ancestor>(this, _node_location__head, true);
		};





		/*!
		 * @brief insert an element into the DAG persistently
		 * specialization: without ancestors
		 * 
		 * @exceptsafe strong exception safety is a direct result of the persistency.
		 */
		template <typename DAG_node_type, typename U = T>
            requires (CDAG_node__0<DAG_node_type, _a_integral<0>>)
        [[nodiscard]] auto insert(U&& data) const -> PersistentDAG_2<T> {
			// create a new DAG
			PersistentDAG_2<T> new_DAG{};

			// copy and push the data
			new_DAG._datas[0] = _datas[0].push_back(std::forward<U>(data));
			auto& data_new{ new_DAG._datas[0].back() };

			// copy other members
			copy_members_accept_for_datas(new_DAG, 0);

			// create new node
			auto node_location__second{ new_DAG.append_node_helper__dynamic<DAG_node_type>() };
			auto node_location__new = _a_node_location(0, node_location__second);

			// set the DAG node pointer of the contained type T
			data_new.set_DAG_node(new_DAG.get_node(node_location__new));

			// state for the new node
			new_DAG._DAG_node_states[0].emplace_back(EDAG_node_states::uptodate);

			// descendant nodes for the new node
			new_DAG._node_locations__descendant[0].emplace_back();

			// add new node into the descendant/ancestor nodes of head/tail nodes
			new_DAG._node_locations__head_descendant.push_back(node_location__new);
			new_DAG._node_locations__tail_ancestor.push_back(node_location__new);

			// execute the background thread
			auto valid_state{ new_DAG._valid_state.load(std::memory_order_acquire) };
			if (!valid_state) {
				auto fut = std::async(&PersistentDAG_2::update_DAG_state, &new_DAG); // allow deferred execution
			}
			return new_DAG; // ensure NRVO due to single return
		};

		/*!
		 * @brief insert an element into the DAG persistently
		 * specialization: with ancestors as node locations
		 * 
		 * @exceptsafe strong exception safety is a direct result of the persistency.
		 */
		template <typename DAG_node_type, unsigned char ancestor_count, typename U = T>
            requires (CDAG_node__n<DAG_node_type, _a_integral<ancestor_count>>)
        [[nodiscard]] auto insert(
            const _a_node_locations__cond<ancestor_count>& node_locations__ancestor__cond,
            U&& data)
			-> PersistentDAG_2<T>
        {
			static constexpr auto ancestor_count_v_{ fix_ancestor_count_v<ancestor_count> };

			// create a new DAG
			PersistentDAG_2<T> new_DAG{};

			// copy and push the data
			new_DAG._datas[ancestor_count_v_] = _datas[ancestor_count_v_].push_back(std::forward<U>(data));
			auto& data_new{ new_DAG._datas[ancestor_count_v_].back() };

			// copy other members
			copy_members_accept_for_datas(new_DAG, ancestor_count_v_);

			// create new node
			auto node_location__second{ new_DAG.append_node_helper__dynamic<DAG_node_type>(node_locations__ancestor__cond) };
			auto node_location__new = _a_node_location(ancestor_count_v_, node_location__second);

			// set the DAG node pointer of the contained type T
			data_new.set_DAG_node(new_DAG.get_node(node_location__new));

			// state for the new node
			new_DAG._DAG_node_states[ancestor_count_v_].emplace_back(EDAG_node_states::uptodate);

			// descendant nodes for the new node
			new_DAG._node_locations__descendant[ancestor_count_v_].emplace_back();

			// add new node into the descendant nodes of the input ancestors
			new_DAG.append_to_descendants_of_ancestors<ancestor_count_v_>(
				node_location__new,
				node_locations__ancestor__cond);

			// add new node into the ancestor nodes of tail node
			new_DAG._node_locations__tail_ancestor.push_back(node_location__new);

			// execute the background thread
			auto valid_state{ new_DAG._valid_state.load(std::memory_order_acquire) };
			if (!valid_state) {
				auto fut = std::async(&PersistentDAG_2::update_DAG_state, &new_DAG); // allow deferred execution
			}
			return new_DAG; // ensure NRVO due to single return
        };

		/*!
		 * @brief insert an element into the DAG persistently
		 * specialization: with ancestors as pointers to nodes
		 * 
		 * @exceptsafe strong exception safety is a direct result of the persistency.
		 */
		template <typename DAG_node_type, unsigned char ancestor_count, typename U = T>
			requires (CDAG_node__n<DAG_node_type, _a_integral<ancestor_count>>)
		[[nodiscard]] auto insert(
			const _a_nodes_var_raw<ancestor_count>& nodes__ancestor__var_raw,
			U&& data)
			-> PersistentDAG_2<T>
		{
			auto node_locations__ancestor__cond{
				convert_nodes_into_node_locations<_a_integral<ancestor_count>>(
					nodes__ancestor__var_raw) };
			return emplace<DAG_node_type, ancestor_count, U>(
				node_locations__ancestor__cond,
				std::forward<U>(data));
		};






		/*!
		 * @brief emplace an element into the DAG persistently
		 * specialization: without ancestors
		 * 
		 * @exceptsafe strong exception safety is a direct result of the persistency.
		 */
		template <typename DAG_node_type, typename... Ts>
            requires (CDAG_node__0<DAG_node_type, _a_integral<0>>)
        [[nodiscard]] auto emplace(Ts&&... args) -> PersistentDAG_2<T>
		{
			// create a new DAG
			PersistentDAG_2<T> new_DAG{};

			// copy and push the data
			new_DAG._datas[0] = _datas[0].emplace_back(std::forward<Ts>(args)...);
			auto& data_new{ new_DAG._datas[0].back() };

			// copy other members
			copy_members_accept_for_datas(new_DAG, 0);

			// create new node
			auto node_location__second{ new_DAG.append_node_helper__dynamic<DAG_node_type>() };
			auto node_location__new = _a_node_location(0, node_location__second);

			// set the DAG node pointer of the contained type T
			data_new.set_DAG_node(new_DAG.get_node(node_location__new));

			// state for the new node
			new_DAG._DAG_node_states[0].emplace_back(EDAG_node_states::uptodate);

			// descendant nodes for the new node
			new_DAG._node_locations__descendant[0].emplace_back();

			// add new node into the descendant/ancestor nodes of head/tail nodes
			new_DAG._node_locations__head_descendant.push_back(node_location__new);
			new_DAG._node_locations__tail_ancestor.push_back(node_location__new);

			// execute the background thread
			auto valid_state{ new_DAG._valid_state.load(std::memory_order_acquire) };
			if (!valid_state) {
				auto fut = std::async(&PersistentDAG_2::update_DAG_state, &new_DAG); // allow deferred execution
			}
			return new_DAG; // ensure NRVO due to single return
		};

		/*!
		 * @brief emplace an element into the DAG persistently
		 * specialization: with ancestors as node locations
		 * 
		 * @exceptsafe strong exception safety is a direct result of the persistency.
		 */
		template <typename DAG_node_type, unsigned char ancestor_count, typename... Ts>
			requires (CDAG_node__n<DAG_node_type, _a_integral<ancestor_count>>)
		[[nodiscard]] auto emplace(
			const _a_node_locations__cond<ancestor_count>& node_locations__ancestor__cond,
			Ts&&... args)
			-> PersistentDAG_2<T>
		{
			static constexpr auto ancestor_count_v_{ fix_ancestor_count_v<ancestor_count> };

			// create a new DAG
			PersistentDAG_2<T> new_DAG{};

			// copy and push the data
			new_DAG._datas[ancestor_count_v_] = _datas[ancestor_count_v_].emplace_back(std::forward<Ts>(args)...);
			auto& data_new{ new_DAG._datas[ancestor_count_v_].back() };

			// copy other members
			copy_members_accept_for_datas(new_DAG, ancestor_count_v_);

			// create new node
			auto node_location__second{ new_DAG.append_node_helper__dynamic<DAG_node_type>(node_locations__ancestor__cond) };
			auto node_location__new = _a_node_location(ancestor_count_v_, node_location__second);

			// set the DAG node pointer of the contained type T
			data_new.set_DAG_node(new_DAG.get_node(node_location__new));

			// state for the new node
			new_DAG._DAG_node_states[ancestor_count_v_].emplace_back(EDAG_node_states::uptodate);

			// descendant nodes for the new node
			new_DAG._node_locations__descendant[ancestor_count_v_].emplace_back();

			// add new node into the descendant nodes of the input ancestors
			new_DAG.append_to_descendants_of_ancestors<ancestor_count_v_>(
				node_location__new,
				node_locations__ancestor__cond);

			// add new node into the ancestor nodes of tail node
			new_DAG._node_locations__tail_ancestor.push_back(node_location__new);

			// execute the background thread
			auto valid_state{ new_DAG._valid_state.load(std::memory_order_acquire) };
			if (!valid_state) {
				auto fut = std::async(&PersistentDAG_2::update_DAG_state, &new_DAG); // allow deferred execution
			}
			return new_DAG; // ensure NRVO due to single return
		};

		/*!
		 * @brief emplace an element into the DAG persistently
		 * specialization: with ancestors as pointers to nodes
		 * 
		 * @exceptsafe strong exception safety is a direct result of the persistency.
		 */
		template <typename DAG_node_type, unsigned char ancestor_count, typename... Ts>
			requires (CDAG_node__n<DAG_node_type, _a_integral<ancestor_count>>)
		[[nodiscard]] auto emplace(
			const _a_nodes_var_raw<ancestor_count>& nodes__ancestor__var_raw,
			Ts&&... args)
			-> PersistentDAG_2<T>
		{
			auto node_locations__ancestor__cond{
				convert_nodes_into_node_locations<_a_integral<ancestor_count>>(
					nodes__ancestor__var_raw) };
			return emplace<DAG_node_type, ancestor_count, Ts...>(
				node_locations__ancestor__cond,
				std::forward<Ts>(args)...);
		};






		/*!
		 * @brief erases the input element from the DAG persistently
		 * terminates if the input has descendants
		 * inform the user in such a case and call erase_with_descendants
		 * if the user insists on erasing the element
		 * 
		 * @see the main documentation of this header file for the details.
		 * 
		 * @exceptsafe strong exception safety is a direct result of the persistency.
		 */
		template <unsigned char ancestor_count>
		[[nodiscard]] auto erase(const _a_node_location& node_location__erase) const
			-> std::optional<PersistentDAG_2<T>>
		{
			if (const auto& node_locations__descendant{ get_descendant_nodes__const(node_location__erase) };
				!node_locations__descendant.empty())
			{
				return {};
			}

			// copy the DAG
			PersistentDAG_2<T> new_DAG{};
			copy_members_accept_for_datas(new_DAG, node_location__erase.first);

			// erase
			new_DAG.erase_mutate<ancestor_count>(
				node_location__erase,
				_datas[node_location__erase.first]);

			// execute the background thread
			auto valid_state{ new_DAG._valid_state.load(std::memory_order_acquire) };
			if (!valid_state) {
				auto fut = std::async(&PersistentDAG_2::update_DAG_state, &new_DAG); // allow deferred execution
			}
			return new_DAG; // ensure NRVO due to single return
		};

		/*!
		 * @brief erases the input element from the DAG persistently
		 * terminates if the input has descendants
		 * inform the user in such a case and call erase_with_descendants
		 * if the user insists on erasing the element
		 * 
		 * @see the main documentation of this header file for the details.
		 * 
		 * @exceptsafe strong exception safety is a direct result of the persistency.
		 */
		template <unsigned char ancestor_count>
		[[nodiscard]] inline auto erase(_a_node_var_raw node_var_raw__erase) const
			-> std::optional<PersistentDAG_2<T>>
		{
			auto node_location__erase{ get_node_location(node_var_raw__erase) };
			return erase<ancestor_count>(node_location__erase);
		};

		/*!
		 * @brief updates the state of the input element as deleted.
		 * execute when the user insists on erasing an element having ancestor nodes.
		 * the states of the descendant nodes are updated with ancestor_fail.
		 * later the user may recover the states.
		 * 
		 * @see the main documentation of this header file for the details.
		 * 
		 * @exceptsafe strong exception safety is a direct result of the persistency.
		 */
		[[nodiscard]] auto erase_with_descendants(
			const _a_node_location& node_location__erase) const
			-> PersistentDAG_2<T>
		{
			// create a new DAG
			PersistentDAG_2<T> new_DAG{ *this };

			// set the state of the node as deleted
			new_DAG._DAG_node_states[node_location__erase.first][node_location__erase.second] =
				EDAG_node_states::deleted;

			// propogate the state change
			new_DAG.propogate_DAG_node_state(node_location__erase);

			// execute the background thread
			auto valid_state{ new_DAG._valid_state.load(std::memory_order_acquire) };
			if (!valid_state) {
				auto fut = std::async(&PersistentDAG_2::update_DAG_state, &new_DAG); // allow deferred execution
			}
			return new_DAG;
		};

		/*!
		 * @brief updates the state of the input element as deleted.
		 * execute when the user insists on erasing an element having ancestor nodes.
		 * the states of the descendant nodes are updated with ancestor_fail.
		 * later the user may recover the states.
		 * 
		 * @see the main documentation of this header file for the details.
		 * 
		 * @exceptsafe strong exception safety is a direct result of the persistency.
		 */
		[[nodiscard]] auto erase_with_descendants(
			_a_node_var_raw node_var_raw__erase) const
			-> PersistentDAG_2<T>
		{
			auto node_location__erase{ get_node_location(node_var_raw__erase) };
			return erase_with_descendants(node_location__erase);
		};









		/*!
		 * @brief replace the ancestors of a node.
		 * terminates if the result is a directed cycle.
		 * inform the user in such a case
		 * and call replace_ancestors_forced
		 * if the user insists on replacing the ancestors
		 * 
		 * @see the main documentation of this header file for the details.
		 * 
		 * @exceptsafe may throw std::logic_error if a directed cycle is detected
		 * @exceptsafe strong exception safety is a direct result of the persistency.
		 */
		template <unsigned char ancestor_count>
		[[nodiscard]] auto replace_ancestors(
			const _a_node_location& node_location,
			const T& data_new,
			const _a_node_locations__cond<ancestor_count>& node_locations__ancestor__new) const
			-> std::optional<PersistentDAG_2<T>>
		{
			// inspect directed cycle
			for (auto node_location__ancestor : node_locations__ancestor__new) {
				if (inspect_directed_cycle(node_location__ancestor, node_location)) {
					return {};
				}
			}
			return std::make_optional<PersistentDAG_2<T>>( // RVO is guaranteed by the standard
				replace_ancestors_helper<ancestor_count>(
					node_location,
					data_new,
					node_locations__ancestor__new));
		};

		/*!
		 * @brief replace the ancestors of a node.
		 * terminates if the result is a directed cycle.
		 * inform the user in such a case
		 * and call replace_ancestors_forced
		 * if the user insists on replacing the ancestors
		 * 
		 * @see the main documentation of this header file for the details.
		 * 
		 * @exceptsafe may throw std::logic_error if a directed cycle is detected
		 * @exceptsafe strong exception safety is a direct result of the persistency.
		 */
		template <unsigned char ancestor_count>
		[[nodiscard]] auto replace_ancestors(
			_a_node_var_raw node_var_raw,
			const T& data_new,
			const _a_nodes_var_raw<ancestor_count>& nodes_var_raw__ancestor__new) const
			-> PersistentDAG_2<T>
		{
			auto node_location{ get_node_location(node_var_raw) };
			auto node_locations__ancestor__new{
				convert_nodes_into_node_locations<_a_integral<ancestor_count>>(
					nodes_var_raw__ancestor__new) };
			return replace_ancestors<ancestor_count>(
				node_location,
				data_new,
				node_locations__ancestor__new);
		};

		/*!
		 * @brief replace the ancestors of a node
		 * although the repllacement causes a directed cycle.
		 * 
		 * @see the main documentation of this header file for the details.
		 * 
		 * @exceptsafe strong exception safety is a direct result of the persistency.
		 */
		template <unsigned char ancestor_count>
		[[nodiscard]] auto replace_ancestors_forced(
			const _a_node_location& node_location,
			const T& data_new,
			const _a_node_locations__cond<ancestor_count>& node_locations__ancestor__new) const
			-> PersistentDAG_2<T>
		{
			std::vector<_a_node_location> node_locations__cycled;
			for (auto node_location__ancestor : node_locations__ancestor__new) {
				if (inspect_directed_cycle(node_location__ancestor, node_location)) {
					node_locations__cycled.push_back(node_location__ancestor);
				}
			}
			auto new_DAG{
				replace_ancestors_helper<ancestor_count>(
					node_location,
					data_new,
					node_locations__ancestor__new) };
			new_DAG._node_locations__cycled[node_location] = std::move(node_locations__cycled);

			return new_DAG; // ensure NRVO due to single return
		};





		/*!
		 * @brief inspects and updates the state of the DAG
		 * executed by the background thread
		 *
		 * use BFS traversal:
		 *     BFS is a level-by-level traversal
		 *     which ensures the state of the ancestors are examined
		 *     when performing in the descendant direction.
		 *     an additional descendant iteration is required for each node
		 *     if a descendant DFS traversal is used
		 *     which makes the linear runtime quadratic.
		 *
		 * notice that the function terminates
		 * if DAG is in a valid state (i.e. all nodes are uptodate)
		 *
		 * one of the only two mutating functions together with erase_deleted_nodes
		 * which are prepared for the background threads.
		 * this function traverses through the whole DAG.
		 * hence, the runtime complexity is O(kN).
		 * however, the function holds only the lock on _mutex__state
		 * which allows copy of the members accept for _DAG_node_states
		 * in the copy constructor.
		 * hence, the main thread can do essential work
		 * while the background thread executes this function.
		 * the other background process, erase_deleted_nodes,
		 * executes faster if the deleted node count is small.
		 *
		 * @exceptsafe may throw due to the copy constructor of the DAG and the iterator
		 * but satisfies strong exception safety
		 * as the initial data is backed up
		 */
		void update_DAG_state()
		{
			// backup the state data as the iterator may throw
			std::array<std::vector<EDAG_node_states>, NODE_CONTAINER_COUNT> DAG_node_states;
			std::unordered_map<
				_a_node_location,
				std::vector<_a_node_location>,
				pair_hasher,
				pair_keyeq> node_locations__cycled;
			{
				std::scoped_lock l(_mutex);
				DAG_node_states = _DAG_node_states;
				node_locations__cycled = _node_locations__cycled;
			}

			// update the states
			auto it{ begin() };
			auto ite{ end() };
			bool valid_state{ true };
			for (; it != ite; ++it) {
				update_DAG_node_state(it._node_location);
				if (
					DAG_node_states[it._node_location.first][it._node_location.second] !=
					EDAG_node_states::uptodate)
				{
					valid_state = false;
				}
			}
			{
				std::scoped_lock l(_mutex);
				_DAG_node_states = DAG_node_states;
				_node_locations__cycled = node_locations__cycled;

				_valid_state.store(valid_state, std::memory_order_relaxed);
			}
		};
	};
};

#endif
